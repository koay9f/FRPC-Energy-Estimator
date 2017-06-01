library(tidyverse)


# FUNCTIONS TO MAKE SERVER.R WORK 
# Citations ----
Data_Cite = read.csv("data/Data_Citations.csv")
cite_name = Data_Cite$Name
cite_source = Data_Cite$Source
cite_full1 = Data_Cite$Full_Citation1
cite_full2 = Data_Cite$Full_Citation2
cite_full3 = Data_Cite$Full_Citation3
cite_full4 = Data_Cite$Full_Citation4
cite_full5 = Data_Cite$Full_Citation5

citefxn <- function(type){
  cite.list <- dplyr::filter(Data_Cite, Type == type) %>%
    select(Name)
    unname(unlist(cite.list))
}

# Properties  ----
checkboxprops <- function(YN, allx, some){
if (YN){
  allx
} else {
  some
  }
}

# Custom Data ====
# Calculate custom molding or curing energy
calcenergy <- function(mass, pm, rm, tm, 
                       pp, rp, tp,  pc, rc, tc,
                       heat,
                       po, uo, ro, to){
  Errc <- function (val){if (is.na(val)) {0}else{val}}
  power.m <- Errc(pm)
  rate.m <- Errc(rm)
  time.m <- Errc(tm)
  power.p <- Errc(pp)
  rate.p <- Errc(rp)
  time.p <- Errc(tp)
  power.c <- Errc(pc)
  rate.c <- Errc(rc)
  time.c <- Errc(tc)
  power.o <- Errc(po)
  unit.o <- Errc(uo)
  rate.o <- Errc(ro)
  time.o <- Errc(to)

  motor <- power.m* rate.m * time.m * 3.1 # convert to embodied
  pump <- power.p* rate.p * time.p * 3.1
  compress <- power.c* rate.c * time.c * 3.1
  
  u.o <- if (unit.o == "kW (electricity)") {3.1} else {(1/3412.142)} 
  other <- power.o* rate.o * time.o * u.o
    #Convert from kW to MW, convert min to s, convert % to frac, / mass
  specificenergy <- sum(motor, pump, compress, heat, other)*(60/(1000*100))/mass
  specificenergy
}

# calc process heating energy
calc.ph <- function(p1, u1, r1, t1,  p2, u2, r2, t2,  p3, u3, r3, t3,  p4, u4, r4, t4,  p5, u5, r5, t5,
                    p6, u6, r6, t6,  p7, u7, r7, t7,  p8, u8, r8, t8,  p9, u9, r9, t9,  p0, u0, r0, t0){
  Errc <- function (val){if (is.na(val)) {0}else{val}}

  
  u.h <- function (unit) {if (unit == "kW (electricity)") {3.1} else {(1/3412.142)} }
    # either *3 for embodied electricity or divide by 3412 for BTU/h --> kW & emboided =1 
  heat <- function (px, rx, tx, ux) {px * rx * tx * ux}
 
  h1 <- heat(Errc(p1), Errc(r1), Errc(t1), u.h(u1))
  h2 <- heat(Errc(p2), Errc(r2), Errc(t2), u.h(u2)) 
  h3 <- heat(Errc(p3), Errc(r3), Errc(t3), u.h(u3))
  h4 <- heat(Errc(p4), Errc(r4), Errc(t4), u.h(u4))
  h5 <- heat(Errc(p5), Errc(r5), Errc(t5), u.h(u5))
  h6 <- heat(Errc(p6), Errc(r6), Errc(t6), u.h(u6))
  h7 <- heat(Errc(p7), Errc(r7), Errc(t7), u.h(u7))
  h8 <- heat(Errc(p8), Errc(r8), Errc(t8), u.h(u8))
  h9 <- heat(Errc(p9), Errc(r9), Errc(t9), u.h(u9))
  h0 <- heat(Errc(p0), Errc(r0), Errc(t0), u.h(u0))
  energy <- sum(h1, h2, h3, h4, h5, h6, h7, h8, h9, h0)
  energy
  #this value is not "real" it still needs to be converted from % (/ 100), convert from min --> s, convert kW to MW and / mass
}


# How to tell if should use calculated molding/curing energy or energy entered as MJ/kg
whichenergy <-  function(addYN, calced, user){
  if (addYN) {
    user
  } else  {
    calced
  }
}

# Build DF with input values for download ----
inputsdf <- function(inputtable, values){
  in.df <- inputtable
  in.df[["User"]] <- values
  
  in.df}

# Upload ----
#Match selections to example
exselect <- function(df, vari) {
  rowcall <- dplyr::filter (df, Variable_Name == vari) %>% select(4)
  unname(unlist(rowcall))
}

#Use Default selection or upload ? - names & numbers
whichselect <- function(rer, def, vari){
  whichdf <- if (is.null(rer)) {
    def
  } else {
    rer}
  rowcall <- dplyr::filter (whichdf, Variable_Name == vari) %>% select(4)
  unname(unlist(rowcall))
}

#Use Default selection or upload or example? - names & numbers
whichselect2 <- function(rer, def, go, ex, vari){
  whichdf <- if (is.null(rer)) {
      if (go > 0) {
        ex
        } else {
        def
      }
    } else {
      rer}
  
  rowcall <- dplyr::filter (whichdf, Variable_Name == vari) %>% select(4)
  unname(unlist(rowcall))
}

#Use Default selection or a reload - YNs
YNcheck <- function(rer,vari){
  rowcall <- dplyr::filter (rer, Variable_Name == vari) %>% select(4)
  YN <- if (rowcall == "TRUE") {
    1
  } else if (rowcall == "FALSE") {
    0
  }
  YN
}

# Specific Additional materials given type ----
othermatfxn <- function(typeother, matrix, additive, filler){
  if (typeother == "Not Used" ) {
    "Not Used"
  } else if (typeother == "Matrix"){
      matrix
    } else if (typeother == "Additive") {
        additive
      } else {
        filler
      }      }

# Curing Tech dependent on molding type ----
curelistfxn <- function(moldtype, all, only, wlup, autoclave){
  if (moldtype == "Wet (Hand) Lay up") {
    wlup
  } else if (moldtype == "Vacuum Bag (Autoclave)"){
      autoclave
  }else if (moldtype == "Automatic Fiber Placement"){
        all
  } else if (moldtype == "Automatic Tape Placement"){
        all
  }else if (moldtype == "Compression Molding"){
        all
  }else if (moldtype == "Resin Transfer Molding"){
        all
  }  else {
        only
            }   }

# Intermediate Tech dependent on molding type ----
intlistfxn <- function(moldtype) {
  if (moldtype == "Wet (Hand) Lay up") {
    c("Prepregs, Manual, Fabric (TS)", "Prepregs, Manual, Fabric (TP)", "Dry Weave", "Dry Braid", "Dry Knit", "Not Used")
  } else if (moldtype == "Vacuum Bag (Autoclave)"){
      c("Prepregs, Manual, Fabric (TS)", "Prepregs, Manual, Fabric (TP)")
  }else if (moldtype == "Automatic Fiber Placement"){
      c("Prepregs, Auto, Fiber (TS)", "Prepregs, Auto, Fiber (TP)")
  } else if (moldtype == "Automatic Tape Placement"){
      c("Prepregs, Auto, Tape (TS)", "Prepregs, Auto, Tape (TP)")
  }else if (moldtype == "Spray Up"){
      c("Chopped")
  }else if (moldtype == "Pultrusion"){
      c("Dry Braid", "Not Used")
  }else if (moldtype == "Filament Winding"){
      c("Prepregs, Auto, Fiber (TS)", "Prepregs, Auto, Fiber (TP)", "Dry Braid", "Not Used")
  }else if (moldtype == "Sheet Molding Compound"){
      c("SMC")
  }else if (moldtype == "Injection Molding"){
    c("SMC")
  }else if (moldtype == "Structural Reaction Injection Molding"){
      c("Chopped", "Dry Weave", "Dry Knit", "Powdered P4")
  }  else {
      c("Prepregs, Manual, Fabric (TS)", "Prepregs, Auto, Tape (TS)", "Prepregs, Auto, Fiber (TS)", "Prepregs, Manual, Fabric (TP)", "Prepregs, Auto, Tape (TP)", "Prepregs, Auto, Fiber (TP)", "Powdered P4", "Dry Weave", "Dry Braid", "Dry Knit", "Not Used")
  }}

#If all int  should be yes or no
checkallYN <- function(moldin, reload, go, custom, vari){
  x <- 
    if (!is.null(reload)) { #check reload file
    as.logical(dplyr::filter (reload, Variable_Name == vari) %>% select(4) )
  } else   if ((go + !is.null(custom)) > 0 ){ #check if custom int/cure has been added
    1    
    } else    if (!is.null(moldin)) { #check if using custom mold
    as.numeric(moldin)
      } else {0}
    }

# FUNCTIONS USED FOR FINAL CALCULATIONS ----
# Calculate yield from scrap
yfs <- function(scrap){(1-scrap)}

# Calculate true mass fraction that includes inserts
newmassfrac_fxn <- function(oldfrac, weight, inserta, insertb){
  newfrac <- oldfrac*(weight - inserta - insertb)/weight
  newfrac
}

# Builds dataframe with all materials and their true mass fraction
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

# Is the intermediate a prepreg (does matrix mass figure into intermediate scrap? - note prepreg energy is specifically MJ / kg fiber)
prepregYN <- function(prepreg){
  pYN <- if(prepreg == "TRUE") {
    1
  } else 0
  pYN}

# Calculate actual yield ( change scrap to yield & include recycling) ----
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
  # More functions specific to BIGFUNCTION1----
  # Build a mass frac list (used to build dataframe) ----
  massfracs_fxn <- function(ff, fm_pri, foa, fob, foc, fia, fib) {
    fm <- sum(fm_pri, foa, fob, foc)
    fi <- sum(fia, fib)
    massfracs_list <- c(ff,fm,fi)
    massfracs_list
  }
  massfracs <- massfracs_fxn(f_f, f_pm, f_ma, f_mb, f_mc, f_ia, f_ib)  
  
  # Match given material and stage to yield and generate cumulative yield to given point ----
  yield <- function(mat,stg){
      syield.df <- dplyr::filter(Data_yield, Material == mat, Stage == stg) %>%
        select(stage_yield)
      yield <- unname(unlist(syield.df))
      yield
    }
    
  # Determines if should apply the stage yield for a given material 
  applyyield_fxn <- function(mat,stg){
    ayield.df <- dplyr::filter(Data_yield, Material == mat, Stage == stg) %>%
      select(apply_yield)
    applyyield <- unname(unlist(ayield.df))
    applyyield
  }
    
  #Builds column for stage yield (if should apply yield then fills with yield value, if not then 1)
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
                          layup  = stage_yield_fxn(mat, "finish") * stage_yield_fxn(mat, "mold") * stage_yield_fxn(mat,"layup") )
  yield_overall
  }
  
  #Back calculate mass at begining of stage from cumulative yield and final part mass
  mass_initial_gen <- function(finalpartmass, yield){finalpartmass/yield}
  
  # Decide if intermediate is a prepreg ----
  YN <- prepregYN(int_prepreg_YN)
  
  # BUILD DATAFRAME  ----
  Data_yield <- data_frame(
    Part = c(rep(partname, 9)),
    Material = c(rep("fiber",3), rep("matrix", 3), rep("insert", 3)),
    Stage = c(rep(c("finish", "mold", "layup"),3)),
    Mass_Fraction = c(rep(massfracs[1], 3),rep(massfracs[2],3), rep(massfracs[3],3)),
    stage_yield = c(rep(c(finish_yield, mold_yield, layup_yield), 3)),
    apply_yield = c(1,1,1, 1,1,YN, 0, 1, 0)
  )
  
  # APPEND DATAFRAME ----
  #create column: stage yield
  Data_yield <- Data_yield %>%
    rowwise() %>%
    mutate(yield_stage = stage_yield_fxn(Material, Stage))
  
   # create column: cumulative yield for a given material & stage
 Data_yield <- Data_yield %>%
    rowwise() %>%
    mutate(yield_cumulative = yield_overall_fxn(Material, Stage))
  
  # creates column: the final part mass (same for each material & stage)
  Data_yield <- Data_yield %>%
    rowwise() %>%
    mutate(part_final_mass = finalmass)
  
  # creates column: the final part mass (for each material)
  Data_yield <- Data_yield %>%
    rowwise() %>%
    mutate(material_final_mass = part_final_mass*Mass_Fraction)
  
  # creates column: divides the final mass by the  cumulative yield     
  Data_yield <- Data_yield %>%   
    rowwise() %>%
    mutate(mass_initial = mass_initial_gen(material_final_mass, yield_cumulative)) 
  
  #END FUNCTION - builds dataframe for cumulative yield and initial mass for each Stage and material
  Data_yield
}


# BIGFUNCTION2 ----

BIGFUNCTION2 <- function(Data_yield, partname,
                         f_pm, f_ma, f_mb, f_mc, f_ia, f_ib,
                         E_1, E_2, E_3, E_4,E_5, E_6, E_7, E_8, E_9, E_10, E_11){
  
  # Functions specific to BIGFUNCTION2
  #Need mass of individual matrix materials (they were grouped for BIGFUNCTION1)
  mass_fxn <- function(mat,stg){
    layupmass.df <- dplyr::filter(Data_yield, Material == mat, Stage == stg) %>%
      select(mass_initial)
    mass_i <- unname(unlist(layupmass.df))
    mass_i
  }
  
  massfrac_fxn <- function(mat,stg){
    massfraction.df <- dplyr::filter(Data_yield, Material == mat, Stage == stg) %>%
      select(4)
    frac <- unname(unlist(massfraction.df))
    frac
  }
  #Defines the mass for each stage (Prepreg specific energy is defined as MJ/kg fiber)
  fib.mass.i <- int.fib.mass.i <- mass_fxn("fiber", "layup")
  matrix.mass.i <- c(f_pm, f_ma, f_mb, f_mc) * mass_fxn("matrix", "layup")/massfrac_fxn("matrix", "layup")
  insert.mass.i <- c(f_ia, f_ib)* mass_fxn("insert", "layup")/massfrac_fxn("insert", "layup")
  mold.mass <- cure.mass <- sum(mass_fxn("fiber", "mold"), mass_fxn("matrix", "mold"), mass_fxn("insert", "mold"))
  finish.mass <- sum(mass_fxn("fiber", "finish"), mass_fxn("matrix", "finish"), mass_fxn("insert", "finish"))
  
  # Build Data Frame
  Data_energy <- data_frame(
    Part = c(rep(partname, 11)),
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

# Build Results DataTable ----
finaldf <- function(name1, name2, w1, w2, AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, YF1, YM1, YT1,
                                  AA2, BB2, CC2, DD2, EE2, FF2, GG2, HH2, II2, JJ2, KK2, YF2, YM2, YT2){
  pd <- function (n1, n2){
    pd <- abs(n1-n2)*100/n1
    }
  
  tempdf <- data_frame(
    Part1 = c(w1, AA, BB, sum(CC, DD,EE, FF, GG), HH, II, JJ, KK, sum(AA, BB, CC, DD,EE, FF, GG, HH, II, JJ, KK), YF1*100, YM1*100, YT1*100),
    Part2 = c(w2, AA2, BB2, sum(CC2, DD2, EE2, FF2, GG2), HH2, II2, JJ2, KK2, sum(AA2, BB2, CC2, DD2, EE2, FF2, GG2, HH2, II2, JJ2, KK2), YF2*100, YM2*100, YT2*100)
        )
  
  tempdf <- tempdf %>%
    rowwise() %>%
    mutate(Percent_change = pd(Part1, Part2))
  
  displaydf <- t(tempdf)
  suppressWarnings( rownames(displaydf) <- c(name1, name2, "Percent Change (%)"))
  suppressWarnings( colnames(displaydf) <- c("Part Weight (kg)", "Fiber (MJ)", "Primary Matrix Material (MJ)", "Other Materials (MJ)", "Intermediate (MJ)",
                                             "Molding (MJ)", "Curing (MJ)", "Finishing (MJ)", "Total (MJ)",
                                             "Fiber Yield (%)", "Matrix Yield (%)", "Process Yield (%)"))
  
  
  displaydf
}

