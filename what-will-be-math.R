# The math for mass change calculations
# mass.in = mass.out / yield

library(tidyverse)


fiberfetch <- function(USERYN, DEFAULT, USER){
  ff <- if(USERYN) {
    DEFAULT
  }else USER
  ff}

yfs <- function(scrap){(1-scrap)}

# Define which yields are being used ----
# Layup - convert scrap --> yield; deside if default or user, reduce by amt of recycling
yield_layup <- function(int_scrap_use_default, int_scrap_default_val, int_scrap_user_val, int_scrap_recycle_val) {
  if (int_scrap_use_default){
    yield_val <- yfs(int_scrap_default_val) + int_scrap_recycle_val * int_scrap_default_val
  } else {
    yield_val <- yfs(int_scrap_user_val) + int_scrap_recycle_val * int_scrap_user_val
  }
  yield_val
}

layupyield1 <- yield_layup(output$intscrapUSERYN1, intscrapNum1z, output$intscrapUSERNum1,output$intscraprecycle1)
layupyield2 <- yield_layup(output$intscrapUSERYN2, intscrapNum2z, output$intscrapUSERNum2,output$intscraprecycle2)

# Mold - deside if default or user, reduce by amt of recycling
yield_mold <- function(mold_yield_use_default, mold_yield_default_val, mold_yield_user_val, mold_yield_recycle_val) {
  if (mold_yield_use_default){
    yield_val <- mold_yield_default_val + mold_yield_recycle_val * yfs(mold_yield_default_val)
  } else {
    yield_val <- mold_yield_user_val + mold_yield_recycle_val * yfs(mold_yield_user_val)
  }
  yield_val
}

moldyield1 <- yield_mold(output$moldyieldUSERYN1, moldyieldNum1z, output$moldyieldUSERNum1,output$moldrecycle1)
moldyield2 <- yield_mold(output$moldyieldUSERYN2, moldyieldNum2z, output$moldyieldUSERNum2,output$moldrecycle2)

# Finish - convert scrap --> yield; reduce by amt of recycling
yield_finish <- function(fin_scrap_user_val, fin_scrap_recycle_val)  {
  yield_val <- yfs(fin_scrap_user_val) + fin_scrap_recycle_val * fin_scrap_user_val
  yield_val
}

finishyield1 <-  yield_finish(output$finishscrap1, output$finishscraprecycle1)
finishyield2 <-  yield_finish(output$finishscrap2, output$finishscraprecycle2)


# Define all mass fractions ----
f.pm1 <- output$primatrixfrac1 
f.ma1 <- output$othermatrixAfrac1 
f.mb1 <- output$othermatrixBfrac1 
f.mc1 <- output$othermatrixCfrac1
f.ia1 <- output$insertsAfrac1
f.ib1 <- output$insertsBfrac1
f.pm2 <- output$primatrixfrac2 
f.ma2 <- output$othermatrixAfrac2 
f.mb2 <- output$othermatrixBfrac2 
f.mc2 <- output$othermatrixCfrac2
f.ia2 <- output$insertsAfrac2
f.ib2 <- output$insertsBfrac2


massfracs <- function(ff_use_default, ff_default, ff_user, fm_pri, foa, fob, foc, fia, fib) {
  ff <- if(ff_use_default) {
    ff_default
  }else ff_user
  
  fm <- sum(fm_pri, foa, fob, foc)
  fi <- sum(fia, fib)
  massfracs_list <- c(ff,fm,fi)
  massfracs_list
    }
  
massfracs1 <- massfracs(output$moldfracUSERYN1, moldfracNum1z,output$moldfracUSERNum1, f.pm1, f.ma1, f.mb1, f.mc1, f.ia1, f.ib1)  
massfracs2 <- massfracs(output$moldfracUSERYN2, moldfracNum2z,output$moldfracUSERNum2, f.pm2, f.ma2, f.mb2, f.mc2, f.ia2, f.ib2)


# prepreg yn
prepregYN <- function(prepreg){
 YN <- if(prepreg) {
   1
 } else 0
 YN}
YN1 <- prepregYN(intprepregYN1z)
YN2 <- prepregYN(intprepregYN2z)



# BUILD DATAFRAME YIELD ----

Data_yield <- data_frame(
  techset = c(rep("ts1", 9), rep("ts2", 9)),
  
  material = c(rep(c(rep("fiber",3), rep("matrix", 3), rep("insert", 3)), 2)),
  
  stage = c(rep(c("finish", "mold", "layup"),6)),
  
  massfrac = c(rep(massfracs1[1], 3),rep(massfracs1[2],3), rep(massfracs1[3],3), rep(massfracs2[1], 3), rep(massfracs2[2],3), rep(massfracs2[3],3)),
  
  stageyield = c(rep(c(finishyield1, moldyield1, layupyield1), 3), rep(c(finishyield2, moldyield2, layupyield2),3)),
  
  applyyield = c(1,1,1, 1,1,YN1, 0, 1, 0, 1,1,1, 1,1,YN2, 0, 1, 0)
)
# DATAFRAME FUNCTIONS ----
    m.f.mat_fxn <- function(mat,stg, ts) {
        finalmass <- if (ts == "ts1") {
        finalpartmass1Z
      } else {finalpartmass2z}
  
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
   print(yield_overall)
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

# CALC ENERGY FOR EACH MATERIAL & PROCESS ----
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

matrix.mass.i1 <- c(f.pm1, f.ma1, f.mb1, f.mc1) * mass_fxn("fiber", "layup", "ts1")/massfrac_fxn("fiber", "layup", "ts1")
matrix.mass.i2 <- c(f.pm2, f.ma2, f.mb2, f.mc2) * mass_fxn("fiber", "layup", "ts2")/massfrac_fxn("fiber", "layup", "ts2")

insert.mass.i1 <- c(f.ia1, f.ib1)* mass_fxn("insert", "layup", "ts1")/massfrac_fxn("insert", "layup", "ts1")
insert.mass.i2 <- c(f.ia2, f.ib2)* mass_fxn("insert", "layup", "ts2")/massfrac_fxn("insert", "layup", "ts2")

mold.mass1 <- sum(mass_fxn("fiber", "mold", "ts1"), mass_fxn("matrix", "mold", "ts1"), mass_fxn("insert", "mold", "ts1"))
mold.mass2 <- sum(mass_fxn("fiber", "mold", "ts2"), mass_fxn("matrix", "mold", "ts2"), mass_fxn("insert", "mold", "ts2"))

finish.mass1 <- cure.mass1 <- sum(mass_fxn("fiber", "finish", "ts1"), mass_fxn("matrix", "finish", "ts1"), mass_fxn("insert", "finish", "ts1"))
finish.mass2 <- cure.mass2 <- sum(mass_fxn("fiber", "finish", "ts2"), mass_fxn("matrix", "finish", "ts2"), mass_fxn("insert", "finish", "ts2"))

# Build Data Frame
Data_energy <- data_frame(
  techset = c(rep("ts1", 11), rep("ts2", 11)),
  process_step = c(rep(c("Fiber", "Intermediate", "PriMatrix", "Matrix.a", "Matrix.b", "Matrix.c", "Insert.a", "Insert.b", "Mold", "Cure", "Finish"), 2)),
  mass_materials = c(fib.mass.i1 , int.fib.mass.i1, matrix.mass.i1, insert.mass.i1, mold.mass1, cure.mass1, finish.mass1, 
                     fib.mass.i2, int.fib.mass.i2, matrix.mass.i2, insert.mass.i2, mold.mass2, cure.mass2, finish.mass2),
  energy_materials = c(fiberEnergyNum1z, intEnergyNum1z, primatrixEnergyNum1z, othermatrixAEnergyNum1z, othermatrixBEnergyNum1z, othermatrixCEnergyNum1z, 
                       insertsAEnergyNum1z, insertsBEnergyNum1z, EnergyNum1z, cureEnergyNum1z, finishEnergyNum1z,
                     fiberEnergyNum2z, intEnergyNum2z, primatrixEnergyNum2z, othermatrixAEnergyNum2z, othermatrixBEnergyNum2z, othermatrixCEnergyNum2z, 
                     insertsAEnergyNum2z, insertsBEnergyNum2z, EnergyNum2z, cureEnergyNum2z, finishEnergyNum2z)
  
  )

# Calc total energy per part
Data_energy <- Data_energy %>%
  rowwise() %>%
  mutate(finalenergy = mass_materials*energy_materials)


# Calc energy for each segment
