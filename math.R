library(tidyverse)
Data_Cite <- read.csv("data/Data_Citations.csv")
cite_name = Data_Cite$Name
cite_source = Data_Cite$Source
cite_full1 = Data_Cite$Full_Citation1
cite_full2 = Data_Cite$Full_Citation2
cite_full3 = Data_Cite$Full_Citation3
cite_full4 = Data_Cite$Full_Citation4
cite_full5 = Data_Cite$Full_Citation5
#  FUNCTIONS TO MAKE SERVER.R WORK
# for molding process dataframe ----
checkboxprops <- function(YN, allx, some){
  if (YN){
    allx
  } else {
    some
  }
}
# for citations
citefxn <- function(type){
  cite.list <- dplyr::filter(Data_Cite, Type == type) %>%
    select(Name)
  unname(unlist(cite.list))
 }
# determine if need to use appended dataframes ----
whichone <- function(go, new, old){
  if (go > 0) {
    new
  }  else {
    old
  }
}

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

# What df should be used to build the Data_MatrixM_new dataframe
BuildnewMatrix.df <- function(gom, goa, gof, newmat, oldmat, newadd, oldadd, newfil, oldfil, other){
  mat<- if (gom > 0) {
      newmat
      } else {
        oldmat
      }
  additive<-   if (goa > 0) {
      newadd
    } else {
      oldadd
    }
     
 filler <-   if (gof > 0) {
      newfil
    } else {
      oldfil
    }

  df <- rbind(mat, additive, filler, other)
  df
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



calcenergy <- function(mass, power.m, rate.m, time.m, 
                       power.p, rate.p, time.p,
                       power.c, rate.c, time.c,
                       power.h, rate.h, time.h,
                       power.o, rate.o, time.o){
  motor <- power.m* rate.m * time.m
  pump <- power.p* rate.p * time.p
  compress <- power.c* rate.c * time.c
  heat <- power.h* rate.h * time.h
  other <- power.o* rate.o * time.o
  #Convert from kW to MW, convert min to s, convert % to frac
  specificenergy <- sum(motor, pump, compress, heat, other)*(60/(1000*100))/mass
  specificenergy
}

whichenergy <-  function(addYN, calced, user){
  if (addYN) {
    user
  } else {
    calced
  }
}



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
              c("Dry Braid", "Not Used")
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


# FUNCTIONS USED FOR FINAL CALCULATIONS ----
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

# Calculate true yields (scrap --> yield & include recycling) ----
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
BIGFUNCTION1 <- function(partname,
  finish_yield, mold_yield, layup_yield,
  f_f, f_pm, f_ma, f_mb, f_mc, f_ia, f_ib,
  int_prepreg_YN, finalmass){
  
  # Build a mass frac list for use in dataframe
  massfracs_fxn <- function(ff, fm_pri, foa, fob, foc, fia, fib) {
    
    fm <- sum(fm_pri, foa, fob, foc)
    fi <- sum(fia, fib)
    massfracs_list <- c(ff,fm,fi)
    massfracs_list
  }
  massfracs <- massfracs_fxn(f_f, f_pm, f_ma, f_mb, f_mc, f_ia, f_ib)  
  
  # Is the intermediate a prepreg (does matrix mass figure into intermediate scrap?)
  prepregYN <- function(prepreg){
    pYN <- if(prepreg == "TRUE") {
      1
    } else 0
    pYN}
  
  YN <- prepregYN(int_prepreg_YN)
  
  # BUILD DATAFRAME YIELD 
  Data_yield <- data_frame(
    part = c(rep(partname, 9)),
    material = c(rep("fiber",3), rep("matrix", 3), rep("insert", 3)),
    stage = c(rep(c("finish", "mold", "layup"),3)),
    massfrac = c(rep(massfracs[1], 3),rep(massfracs[2],3), rep(massfracs[3],3)),
    stageyield = c(rep(c(finish_yield, mold_yield, layup_yield), 3)),
    applyyield = c(1,1,1, 1,1,YN, 0, 1, 0)
  )

  # Match given material and stage to yield and generate cumulative yield to given point

    yield <- function(mat,stg){
      syield.df <- dplyr::filter(Data_yield, material == mat, stage == stg) %>%
        select(stageyield)
      yield <- unname(unlist(syield.df))
      yield
    }
    
    # Determines if should apply the stage yield for a given material 
    applyyield_fxn <- function(mat,stg){
      ayield.df <- dplyr::filter(Data_yield, material == mat, stage == stg) %>%
        select(6)
      apply_yield <- unname(unlist(ayield.df))
      apply_yield
    }
    
    #Builds column for stage yield
    stage_yield_fxn <- function(mat, stg){
      sy <- if (applyyield_fxn(mat,stg) == 1) {
        yield(mat,stg)
      } else 1
      sy}
    
    #Builds new column for cumulative yield at given stage for a given material
    yield_overall_fxn <- function(mat,stg){
    yield_overall <- switch(stg,
                            finish =  stage_yield_fxn(mat, "finish"),
                            mold   = stage_yield_fxn(mat, "finish") * stage_yield_fxn(mat, "mold"),
                            layup  = stage_yield_fxn(mat, "finish") * stage_yield_fxn(mat, "mold") * stage_yield_fxn(mat,"layup")
    )
    yield_overall
  }
  
  #Back calculate mass at begining of stage given cumulative yield and final part mass
  mass_initial_gen <- function(finalpartmass, yield){finalpartmass/yield}
  
  # APPEND DATAFRAME ----
  #create col: stage yield
  Data_yield <- Data_yield %>%
    rowwise() %>%
    mutate(yield_stage = stage_yield_fxn(material, stage))
  
   # create col: cumulative yield for a given material & stage
 
   Data_yield <- Data_yield %>%
    rowwise() %>%
    mutate(yield_actual = yield_overall_fxn(material, stage))
  
  # creates col: the final part mass (same for each material & stage)
  Data_yield <- Data_yield %>%
    rowwise() %>%
    mutate(final_mass = finalmass)
  # creates col: the final part mass (for each material)
  Data_yield <- Data_yield %>%
    rowwise() %>%
    mutate(material_final_mass = final_mass*massfrac)
  
  # creates col: divides the final mass by the  cumulative yield     
  Data_yield <- Data_yield %>%   
    rowwise() %>%
    mutate(mass_initial = mass_initial_gen(material_final_mass, yield_actual)) 
  
  #END FUNCTION - builds dataframe for cumulative yield and initial mass for each stage and material
  Data_yield
}


# BIGFUNCTION2 ----

BIGFUNCTION2 <- function(Data_yield, partname,
                         f_pm, f_ma, f_mb, f_mc, f_ia, f_ib,
                         E_1, E_2, E_3, E_4,E_5, E_6, E_7, E_8, E_9, E_10, E_11){
  
  # Calculate material mass at each stage
  
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
    part = c(rep(partname, 11)),
    process_step = c("Fiber", "Fiber Intermediate", "Primary Matrix", "Additional Matrix A", "Additional Matrix B", " Additional Matrix C", "Insert A", "Insert B", "Molding", "Curing", "Finishing"),
    mass_materials = c(fib.mass.i , int.fib.mass.i, matrix.mass.i, insert.mass.i, mold.mass, cure.mass, finish.mass), 
    energy_kg = c(E_1, E_2, E_3, E_4,E_5, E_6, E_7, E_8, E_9, E_10, E_11)
    )
  
  # Calculate total energy per process segment
  Data_energy <- Data_energy %>%
    rowwise() %>%
    mutate(finalenergy = mass_materials*energy_kg)
  Data_energy[is.na(Data_energy)]<- 0
  
  Data_energy
}

# Final data frame ----
finaldf <- function(name1, name2, AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, AA2, BB2, CC2, DD2, EE2, FF2, GG2, HH2, II2, JJ2, KK2){
  pd <- function (n1, n2){
    pd <- abs(n1-n2)*100/n1
    #pd <- paste(pd,"%")
  }
  
  tempdf <- data_frame(
    Part1 = c(AA, BB, sum(CC, DD,EE, FF, GG), HH, II, JJ, KK, sum(AA, BB, CC, DD,EE, FF, GG, HH, II, JJ, KK)),
    Part2 = c(AA2, BB2, sum(CC2, DD2, EE2, FF2, GG2), HH2, II2, JJ2, KK2, sum(AA2, BB2, CC2, DD2, EE2, FF2, GG2, HH2, II2, JJ2, KK2))
  )
  
  
  tempdf <- tempdf %>%
    rowwise() %>%
    mutate(Percent_change = pd(Part1, Part2))
  
  displaydf <- t(tempdf)
  suppressWarnings( rownames(displaydf) <- c(name1,name2, "Percent Change (%)"))
  suppressWarnings( colnames(displaydf) <- c("Fiber", "Primary Matrix Material", "Other Materials", "Intermediate", "Molding", "Curing", "Finishing", "Total"))
  
  displaydf
}
