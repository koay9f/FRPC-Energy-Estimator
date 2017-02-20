library(tidyverse)

# ADDITIONAL BACKGROUND FUNCTIONS
# make lists for additional materials----
othermatfxn <- function(typeother, matrix, additive, filler){
  if (typeother == "Not Used" ) {
    "Not Used"
  } else {
    if (typeother == "Matrix"){
      matrix
    } else{
      if (typeother == "Additive") {
        additive
      } else{
        filler
        }
      }
    }
      }
# cure list dependent on molding----
curelistfxn <- function(moldtype, all, only, wlup, autoclave){
 if (moldtype == "Wet (Hand) Lay up") {
   wlup
 } else{
   if (moldtype == "Vacuum Bag (Autoclave)"){
     autoclave
     
   }else{
     if (moldtype == "Automatic Fiber Placement"){
       all
     } else{
       if (moldtype == "Automatic Tape Placement"){
         all
       }else{
         if (moldtype == "Compression Molding"){
           all
         }else{
           if (moldtype == "Resin Transfer Molding"){
             all
             
           }  else{
       only
           }   } }  }}}}

# int list dependent on molding----
intlistfxn <- function(moldtype) {
  if (moldtype == "Wet (Hand) Lay up") {
    c("Prepregs, Hand (TS)", "Prepregs, Hand (TP)", "Dry Weave", "Dry Braid", "Dry Knit", "Not Used")
  } else {
    if (moldtype == "Vacuum Bag (Autoclave)"){
      c("Prepregs, Hand (TS)", "Prepregs, Hand (TP)")
    }else {
      if (moldtype == "Automatic Fiber Placement"){
        c("Prepregs, Auto, Fiber (TS)", "Prepregs, Auto, Fiber (TP)")
      } else{
        if (moldtype == "Automatic Tape Placement"){
          c("Prepregs, Auto, Tape (TS)", "Prepregs, Auto, Tape (TP)")
        }else{
          if (moldtype == "Spray Up"){
            c("Chopped")
          }else{
            if (moldtype == "Pultrusion"){
              c("Dry Weave", "Not Used")
            }else{
              if (moldtype == "Filament Winding"){
                c("Prepregs, Auto, Fiber (TS)", "Prepregs, Auto, Fiber (TP)", "Dry Braid", "Not Used")
              }else{
                if (moldtype == "Sheet Molding Compound"){
                  c("SMC")
                }else{
                  if (moldtype == "Structural Reaction Injection Molding"){
                    c("Chopped", "Dry Weave", "Dry Knit", "Powdered P4")
              
            }  else{
              c("Prepregs, Hand (TS)", "Prepregs, Auto, Tape (TS)", "Prepregs, Auto, Fiber (TS)", "Prepregs, Hand (TP)", "Prepregs, Auto, Tape (TP)", "Prepregs, Auto, Fiber (TP)", "Powdered P4", "Dry Weave", "Dry Braid", "Dry Knit", "Not Used")
            }}}}}}}}}}


# Additional Fxns ----
yfs <- function(scrap){(1-scrap)}

newmassfrac_fxn <- function(oldfrac, weight, inserta, insertb){
  newfrac <- oldfrac*(weight - inserta - insertb)/weight
  newfrac
}

Data_mass_fxn <- function(mass, r.f.f, r.f.pm, r.f.ma, r.f.mb, r.f.mc, r.m.ia, r.m.ib){
  massfrac.df <- data_frame(
    vari.name = c("fiber","pm", "ma", "mb", "mc"),
    raw.value = c(r.f.f, r.f.pm, r.f.ma, r.f.mb, r.f.mc)
  )
  massfrac.df <- massfrac.df %>%
    rowwise() %>%
    mutate(mass.frac = newmassfrac_fxn(raw.value, mass, r.m.ia, r.m.ib))
  
  
  
  insertmass.df <- data_frame(
    vari.name = c("ia","ib"),
    raw.value = c(r.m.ia, r.m.ib)
    )  
  insertmass.df <- insertmass.df %>%
    rowwise() %>%
  mutate(mass.frac = (raw.value/mass))

 
 calc.mass.frac.df <- bind_rows(massfrac.df, insertmass.df)
  calc.mass.frac.df
  
  }

check <- function(check){
  if (check == 1 ) {
  ""
  } else {"Error: Mass fractions for technology set 1 do not equal 1"}
  }

# Define which yields are being used ----
# Layup - convert scrap --> yield; deside if default or user, reduce by amt of recycling
yield_layup <- function(int_scrap_user_val, int_scrap_recycle_val) {
    yield_val <- yfs(int_scrap_user_val/100) + (int_scrap_recycle_val/100) * (int_scrap_user_val/100)
    yield_val
}



# Mold - deside if default or user, reduce by amt of recycling
yield_mold <- function( mold_yield_user_val, mold_yield_recycle_val) {
      yield_val <- mold_yield_user_val/100 + (mold_yield_recycle_val/100) * (yfs(mold_yield_user_val/100))
    yield_val
}


# Finish - convert scrap --> yield; reduce by amt of recycling
yield_finish <- function(fin_scrap_user_val, fin_scrap_recycle_val)  {
  yield_val <- yfs(fin_scrap_user_val/100) + (fin_scrap_recycle_val/100) * (fin_scrap_user_val/100)
  yield_val
}

# BIGFUNCTION1 ----
BIGFUNCTION1 <- function(
  finish_yield, mold_yield, layup_yield,
  f_f, f_pm, f_ma, f_mb, f_mc, f_ia, f_ib,
  int_prepreg_YN, finalmass){
  
  # Define all mass fractions ----
  
  
  
  massfracs_fxn <- function(ff, fm_pri, foa, fob, foc, fia, fib) {
    
    fm <- sum(fm_pri, foa, fob, foc)
    fi <- sum(fia, fib)
    massfracs_list <- c(ff,fm,fi)
    massfracs_list
  }
  massfracs <- massfracs_fxn(f_f, f_pm, f_ma, f_mb, f_mc, f_ia, f_ib)  

  # prepreg yn ----
  prepregYN <- function(prepreg){
    pYN <- if(prepreg == "TRUE") {
      1
    } else 0
    pYN}
  YN <- prepregYN(int_prepreg_YN)

  
  # BUILD DATAFRAME YIELD ----
  
  Data_yield <- data_frame(
    material = c(rep("fiber",3), rep("matrix", 3), rep("insert", 3)),
    
    stage = c(rep(c("finish", "mold", "layup"),3)),
    
    massfrac = c(rep(massfracs[1], 3),rep(massfracs[2],3), rep(massfracs[3],3)),
    
    stageyield = c(rep(c(finish_yield, mold_yield, layup_yield), 3)),
    
    applyyield = c(1,1,1, 1,1,YN, 0, 1, 0)
  )
  # DATAFRAME FUNCTIONS ----
  # m.f.mat_fxn <- function(mat,stg, ts) {
  #   finalmass <- if (ts == "ts1") {
  #     final_part_mass1
  #   } else {final_part_mass2}
  

    
    massfrac_fxn <- function(mat,stg){
      massfraction.df <- dplyr::filter(Data_yield, material == mat, stage == stg) %>%
        select(3)
      frac <- unname(unlist(massfraction.df))
      frac
    
    
    final_mass_material <- finalmass*massfrac_fxn(mat,stg)
    
    return(final_mass_material)
  }
  
  yield_overall_fxn <- function(mat,stg, ts){
    yield <- function(mat,stg, ts){
      syield.df <- dplyr::filter(Data_yield, material == mat, stage == stg) %>%
        select(stageyield)
      yield <- unname(unlist(syield.df))
      yield
    }
    
    applyyield_fxn <- function(mat,stg, ts){
      ayield.df <- dplyr::filter(Data_yield, material == mat, stage == stg) %>%
        select(5)
      apply_yield <- unname(unlist(ayield.df))
      apply_yield
    }
    
    yield_overall <- switch(stg,
                            finish =  yield(mat, "finish") ^ applyyield_fxn(mat, "finish"),
                            mold   = (yield(mat, "finish") ^ applyyield_fxn(mat, "finish")) * (yield(mat, "mold")  ^ applyyield_fxn(mat, "mold")),
                            layup  = (yield(mat, "finish") ^ applyyield_fxn(mat, "finish")) * (yield(mat, "mold")  ^ applyyield_fxn(mat, "mold")) * (yield(mat,"layup") ^ applyyield_fxn(mat, "layup"))
    )
    yield_overall
  }
  
  
  mass_initial_gen <- function(finalpartmass, yield){finalpartmass/yield}
  
  # APPEND DATAFRAME ----
  
  # create col with "actual yield
  Data_yield <- Data_yield %>%
    rowwise() %>%
    mutate(yield_actual = yield_overall_fxn(material, stage))
  
  # creates col: the final part mass of each material
  Data_yield <- Data_yield %>%
    rowwise() %>%
    mutate(material_final_mass = finalmass)
  
  # creates col: divides the final mass by the  yield     
  Data_yield <- Data_yield %>%   
    rowwise() %>%
    mutate(mass_initial = mass_initial_gen(material_final_mass, yield_actual)) 
  
  #END FUNCTION - IT WORKS, BUT IT DOES NOT ADD A NEW TABLE TO THE GLOBAL ENVIRONMENT
  Data_yield
}


# BIGFUNCTION2 ----

BIGFUNCTION2 <- function(Data_yield,
                         f_pm, f_ma, f_mb, f_mc, f_ia, f_ib,
                         E_1, E_2, E_3, E_4,E_5, E_6, E_7, E_8, E_9, E_10, E_11){
  
  # BUILD DATA FRAME

 
  mass_fxn <- function(mat,stg){
    layupmass.df <- dplyr::filter(Data_yield, material == mat, stage == stg) %>%
      select(mass_initial)
    mass_i <- unname(unlist(layupmass.df))
    mass_i
  }
  
  massfrac_fxn <- function(mat,stg){
    massfraction.df <- dplyr::filter(Data_yield, material == mat, stage == stg) %>%
      select(4)
    frac <- unname(unlist(massfraction.df))
    frac
  }
  
  
  fib.mass.i <- int.fib.mass.i <- mass_fxn("fiber", "layup")

  matrix.mass.i <- c(f_pm, f_ma, f_mb, f_mc) * mass_fxn("matrix", "layup")/massfrac_fxn("matrix", "layup")

  insert.mass.i <- c(f_ia, f_ib)* mass_fxn("insert", "layup")/massfrac_fxn("insert", "layup")

  mold.mass <- sum(mass_fxn("fiber", "mold"), mass_fxn("matrix", "mold"), mass_fxn("insert", "mold"))

  finish.mass <- cure.mass <- sum(mass_fxn("fiber", "finish"), mass_fxn("matrix", "finish"), mass_fxn("insert", "finish"))

  # Build Data Frame
  Data_energy <- data_frame(
    techset = c(rep("ts1", 11)),
    process_step = c("Fiber", "Fiber Intermediate", "Primary Matrix", "Additional Matrix A", "Additional Matrix B", " Additional Matrix C", "Insert A", "Insert B", "Molding", "Curing", "Finishing"),
    mass_materials = c(fib.mass.i , int.fib.mass.i, matrix.mass.i, insert.mass.i, mold.mass, cure.mass, finish.mass), 
    energy_materials = c(E_1, E_2, E_3, E_4,E_5, E_6, E_7, E_8, E_9, E_10, E_11)
    
  )
  
  # Calc total energy per part
  Data_energy <- Data_energy %>%
    rowwise() %>%
    mutate(finalenergy = mass_materials*energy_materials)
  Data_energy[is.na(Data_energy)]<- 0
  
  Data_energy
}

  


