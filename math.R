library(tidyverse)

# ADDITIONAL BACKGROUND FUNCTIONS




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
    yield_val <- yfs(int_scrap_user_val) + int_scrap_recycle_val * int_scrap_user_val
    yield_val
}



# Mold - deside if default or user, reduce by amt of recycling
yield_mold <- function( mold_yield_user_val, mold_yield_recycle_val) {
      yield_val <- mold_yield_user_val + mold_yield_recycle_val * yfs(mold_yield_user_val)
    yield_val
}


# Finish - convert scrap --> yield; reduce by amt of recycling
yield_finish <- function(fin_scrap_user_val, fin_scrap_recycle_val)  {
  yield_val <- yfs(fin_scrap_user_val) + fin_scrap_recycle_val * fin_scrap_user_val
  yield_val
}

# BIGFUNCTION1 ----
BIGFUNCTION1 <- function(
  finish_yield1, mold_yield1, layup_yield1,
  finish_yield2, mold_yield2, layup_yield2,
  f_f1, f_pm1, f_ma1, f_mb1, f_mc1, f_ia1, f_ib1,
  f_f2, f_pm2, f_ma2, f_mb2, f_mc2, f_ia2, f_ib2,
  int_prepreg_YN1, int_prepreg_YN2, 
  final_part_mass1, final_part_mass2){
  
  # Define all mass fractions ----
  
  
  
  massfracs <- function(ff, fm_pri, foa, fob, foc, fia, fib) {
    
    fm <- sum(fm_pri, foa, fob, foc)
    fi <- sum(fia, fib)
    massfracs_list <- c(ff,fm,fi)
    massfracs_list
  }
  
  massfracs1 <- massfracs(f_f1, f_pm1, f_ma1, f_mb1, f_mc1, f_ia1, f_ib1)  
  massfracs2 <- massfracs(f_f2, f_pm2, f_ma2, f_mb2, f_mc2, f_ia2, f_ib2)
  
  
  # prepreg yn ----
  prepregYN <- function(prepreg){
    YN <- if(prepreg == "TRUE") {
      1
    } else 0
    YN}
  YN1 <- prepregYN(int_prepreg_YN1)
  YN2 <- prepregYN(int_prepreg_YN2)
  
  
  # BUILD DATAFRAME YIELD ----
  
  Data_yield <- data_frame(
    techset = c(rep("ts1", 9), rep("ts2", 9)),
    
    material = c(rep(c(rep("fiber",3), rep("matrix", 3), rep("insert", 3)), 2)),
    
    stage = c(rep(c("finish", "mold", "layup"),6)),
    
    massfrac = c(rep(massfracs1[1], 3),rep(massfracs1[2],3), rep(massfracs1[3],3), rep(massfracs2[1], 3), rep(massfracs2[2],3), rep(massfracs2[3],3)),
    
    stageyield = c(rep(c(finish_yield1, mold_yield1, layup_yield1), 3), rep(c(finish_yield2, mold_yield2, layup_yield2),3)),
    
    applyyield = c(1,1,1, 1,1,YN1, 0, 1, 0, 1,1,1, 1,1,YN2, 0, 1, 0)
  )
  # DATAFRAME FUNCTIONS ----
  m.f.mat_fxn <- function(mat,stg, ts) {
    finalmass <- if (ts == "ts1") {
      final_part_mass1
    } else {final_part_mass2}
    
    massfrac_fxn <- function(mat,stg, ts){
      massfraction.df <- dplyr::filter(Data_yield, material == mat, stage == stg, techset == ts) %>%
        select(4)
      frac <- unname(unlist(massfraction.df))
      frac
    }
    
    final_mass_material <- finalmass*massfrac_fxn(mat,stg, ts)
    
    return(final_mass_material)
  }
  
  yield_overall_fxn <- function(mat,stg, ts){
    yield <- function(mat,stg, ts){
      syield.df <- dplyr::filter(Data_yield, material == mat, stage == stg, techset == ts) %>%
        select(stageyield)
      yield <- unname(unlist(syield.df))
      yield
    }
    
    applyyield_fxn <- function(mat,stg, ts){
      ayield.df <- dplyr::filter(Data_yield, material == mat, stage == stg, techset == ts) %>%
        select(6)
      apply_yield <- unname(unlist(ayield.df))
      apply_yield
    }
    
    yield_overall <- switch(stg,
                            finish =  yield(mat, "finish", ts) ^ applyyield_fxn(mat, "finish", ts),
                            mold   = (yield(mat, "finish", ts) ^ applyyield_fxn(mat, "finish", ts)) * (yield(mat, "mold", ts)  ^ applyyield_fxn(mat, "mold", ts)),
                            layup  = (yield(mat, "finish", ts) ^ applyyield_fxn(mat, "finish", ts)) * (yield(mat, "mold", ts)  ^ applyyield_fxn(mat, "mold", ts)) * (yield(mat,"layup", ts) ^ applyyield_fxn(mat, "layup", ts))
    )
    yield_overall
  }
  
  
  mass_initial_gen <- function(finalpartmass, yield){finalpartmass/yield}
  
  # APPEND DATAFRAME ----
  
  # create col with "actual yield
  Data_yield <- Data_yield %>%
    rowwise() %>%
    mutate(yield_actual = yield_overall_fxn(material, stage, techset))
  
  # creates col: the final part mass of each material
  Data_yield <- Data_yield %>%
    rowwise() %>%
    mutate(material_final_mass = m.f.mat_fxn(material,stage, techset))
  
  # creates col: divides the final mass by the  yield     
  Data_yield <- Data_yield %>%   
    rowwise() %>%
    mutate(mass_initial = mass_initial_gen(material_final_mass, yield_actual)) 
  
  #END FUNCTION - IT WORKS, BUT IT DOES NOT ADD A NEW TABLE TO THE GLOBAL ENVIRONMENT
  Data_yield
}


# # BEGINING OF BIGFUNCTION2 ----

BIGFUNCTION2 <- function(Data_yield,
                         f_pm1, f_ma1, f_mb1, f_mc1, f_ia1, f_ib1,
                         f_pm2, f_ma2, f_mb2, f_mc2, f_ia2, f_ib2,
                         E_1, E_2, E_3, E_4,E_5, E_6, E_7, E_8, E_9, E_10, E_11, E_12, E_13, E_14, E_15, E_16, E_17, E_18, E_19, E_20, E_21, E_22){
  
  # BUILD DATA FRAME

 
  mass_fxn <- function(mat,stg, ts){
    layupmass.df <- dplyr::filter(Data_yield, material == mat, stage == stg, techset == ts) %>%
      select(mass_initial)
    mass_i <- unname(unlist(layupmass.df))
    mass_i
  }
  
  massfrac_fxn <- function(mat,stg, ts){
    massfraction.df <- dplyr::filter(Data_yield, material == mat, stage == stg, techset == ts) %>%
      select(4)
    frac <- unname(unlist(massfraction.df))
    frac
  }
  
  
  fib.mass.i1 <- int.fib.mass.i1 <- mass_fxn("fiber", "layup", "ts1")
  fib.mass.i2 <- int.fib.mass.i2 <- mass_fxn("fiber", "layup", "ts1")
  
  matrix.mass.i1 <- c(f_pm1, f_ma1, f_mb1, f_mc1) * mass_fxn("matrix", "layup", "ts1")/massfrac_fxn("matrix", "layup", "ts1")
  matrix.mass.i2 <- c(f_pm2, f_ma2, f_mb2, f_mc2) * mass_fxn("matrix", "layup", "ts2")/massfrac_fxn("matrix", "layup", "ts2")
  
  insert.mass.i1 <- c(f_ia1, f_ib1)* mass_fxn("insert", "layup", "ts1")/massfrac_fxn("insert", "layup", "ts1")
  insert.mass.i2 <- c(f_ia2, f_ib2)* mass_fxn("insert", "layup", "ts2")/massfrac_fxn("insert", "layup", "ts2")
  
  mold.mass1 <- sum(mass_fxn("fiber", "mold", "ts1"), mass_fxn("matrix", "mold", "ts1"), mass_fxn("insert", "mold", "ts1"))
  mold.mass2 <- sum(mass_fxn("fiber", "mold", "ts2"), mass_fxn("matrix", "mold", "ts2"), mass_fxn("insert", "mold", "ts2"))
  
  finish.mass1 <- cure.mass1 <- sum(mass_fxn("fiber", "finish", "ts1"), mass_fxn("matrix", "finish", "ts1"), mass_fxn("insert", "finish", "ts1"))
  finish.mass2 <- cure.mass2 <- sum(mass_fxn("fiber", "finish", "ts2"), mass_fxn("matrix", "finish", "ts2"), mass_fxn("insert", "finish", "ts2"))
  
  # Build Data Frame
  Data_energy <- data_frame(
    techset = c(rep("ts1", 11), rep("ts2", 11)),
    process_step = c(rep(c("Fiber", "Fiber Intermediate", "Primary Matrix", "Additional Matrix A", "Additional Matrix B", " Additional Matrix C", "Insert A", "Insert B", "Molding", "Curing", "Finishing"), 2)),
    mass_materials = c(fib.mass.i1 , int.fib.mass.i1, matrix.mass.i1, insert.mass.i1, mold.mass1, cure.mass1, finish.mass1, 
                       fib.mass.i2, int.fib.mass.i2, matrix.mass.i2, insert.mass.i2, mold.mass2, cure.mass2, finish.mass2),
    energy_materials = c(E_1, E_2, E_3, E_4,E_5, E_6, E_7, E_8, E_9, E_10, E_11, E_12, E_13, E_14, E_15, E_16, E_17, E_18, E_19, E_20, E_21, E_22)
    
  )
  
  # Calc total energy per part
  Data_energy <- Data_energy %>%
    rowwise() %>%
    mutate(finalenergy = mass_materials*energy_materials)
  
  
  Data_energy
}

