library(tidyverse)



ff_use_default1 <- output$moldfracUSERYN1
ff_default1 <-moldfracNum1z
ff_user1 <- output$moldfracUSERNum1
ff_use_default2 <- output$moldfracUSERYN2
ff_default2 <-moldfracNum2z
ff_user2 <- output$moldfracUSERNum2

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

int.YN1 <-output$intscrapUSERYN1
int.s.def1 <- intscrapNum1z
int.s.user1 <- output$intscrapUSERNum1
int.s.rec1 <- output$intscraprecycle1

# BEGINING OF BIGFUNCTION1
BIGFUNCTION1 <- function(
  int.YN1, int.s.def1, int.s.user1, int.s.rec1,
  int.YN2, int.s.def2, int.s.user2, int.s.rec2,
  mold.YN1, mold.y.def1, mold.y.user1, mold.y.rec1,
  mold.YN2, mold.y.def2, mold.y.user2, mold.y.rec2,
  fin.s.1, fin.s.rec1, fin.s.2, fin.s.rec2,
  ff.use.default1, ff.default1, ff.user1, f.pm1, f.ma1, f.mb1, f.mc1, f.ia1, f.ib1,
  ff.use.default2, ff.default2, ff.user2, f.pm2, f.ma2, f.mb2, f.mc2, f.ia2, f.ib2,
  int.prepreg.YN1, int.prepreg.YN2, 
  final.part.mass1, final.part.mass2){

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

layupyield1 <- yield_layup(int.YN1, int.s.def1, int.s.user1, int.s.rec1)
layupyield2 <- yield_layup(int.YN2, int.s.def2, int.s.user2, int.s.rec2)

# Mold - deside if default or user, reduce by amt of recycling
yield_mold <- function(mold_yield_use_default, mold_yield_default_val, mold_yield_user_val, mold_yield_recycle_val) {
  if (mold_yield_use_default){
    yield_val <- mold_yield_default_val + mold_yield_recycle_val * yfs(mold_yield_default_val)
  } else {
    yield_val <- mold_yield_user_val + mold_yield_recycle_val * yfs(mold_yield_user_val)
  }
  yield_val
}

moldyield1 <- yield_mold(mold.YN1, mold.y.def1, mold.y.user1, mold.y.rec1)
moldyield2 <- yield_mold(mold.YN2, mold.y.def2, mold.y.user2, mold.y.rec2)

# Finish - convert scrap --> yield; reduce by amt of recycling
yield_finish <- function(fin_scrap_user_val, fin_scrap_recycle_val)  {
  yield_val <- yfs(fin_scrap_user_val) + fin_scrap_recycle_val * fin_scrap_user_val
  yield_val
}

finishyield1 <-  yield_finish(fin.s.1, fin.s.rec1)
finishyield2 <-  yield_finish(fin.s.2, fin.s.rec2)


# Define all mass fractions ----



massfracs <- function(ff_use_default, ff_default, ff_user, fm_pri, foa, fob, foc, fia, fib) {
  ff <- if(ff_use_default) {
    ff_default
  }else ff_user
  
  fm <- sum(fm_pri, foa, fob, foc)
  fi <- sum(fia, fib)
  massfracs_list <- c(ff,fm,fi)
  massfracs_list
}

massfracs1 <- massfracs(ff.use.default1, ff.default1, ff.user1, f.pm1, f.ma1, f.mb1, f.mc1, f.ia1, f.ib1)  
massfracs2 <- massfracs(ff.use.default2, ff.default2, ff.user2, f.pm2, f.ma2, f.mb2, f.mc2, f.ia2, f.ib2)


# prepreg yn
prepregYN <- function(prepreg){
  YN <- if(prepreg) {
    1
  } else 0
  YN}
YN1 <- prepregYN(int.prepreg.YN1)
YN2 <- prepregYN(int.prepreg.YN2)


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
    final.part.mass1
  } else {final.part.mass2}
  
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

#END FUNCTION - IT WORKS, BUT IT DOES NOT ADD A NEW TABLE TO THE GLOBAL ENVIRONMENT
Data_yield
}


# # BEGINING OF BIGFUNCTION2 ----
E.fib1 <- fiberEnergyNum1z
E.int1 <- intEnergyNum1z 
E.pmat1 <- primatrixEnergyNum1z
E.mata1 <- othermatrixAEnergyNum1z
E.matb1 <- othermatrixBEnergyNum1z
E.matc1 <- othermatrixCEnergyNum1z
E.insa1 <- insertsAEnergyNum1z
E.insb1 <- insertsBEnergyNum1z
E.mold1 <- EnergyNum1z 
E.cure1 <- cureEnergyNum1z
E.fin1 <- finishEnergyNum1z
E.fib2<- fiberEnergyNum2z
E.int2 <-intEnergyNum2z
E.pmat2 <- primatrixEnergyNum2z
E.mata2 <- othermatrixAEnergyNum2z
E.matb2 <-othermatrixBEnergyNum2z
E.matc2 <- othermatrixCEnergyNum2z
E.insa2 <- insertsAEnergyNum2z
E.insb2 <- insertsBEnergyNum2z
E.mold2 <-EnergyNum2z
E.cure2 <-cureEnergyNum2z
E.fin2 <-finishEnergyNum2z



# make sure can call a df

BIGFUNCTION2 <- function(Data_yield,
                         f.pm1, f.ma1, f.mb1, f.mc1, f.ia1, f.ib1,
                         f.pm2, f.ma2, f.mb2, f.mc2, f.ia2, f.ib2,
                         E.fib1, E.int1, E.pmat1, E.mata1, E.matb1, E.matc1,
                         E.insa1, E.insb1, E.mold1, E.cure1, E.fin1,
                         E.fib2, E.int2, E.pmat2, E.mata2, E.matb2, E.matc2,
                         E.insa2, E.insb2, E.mold2, E.cure2, E.fin2
                         ){

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
    energy_materials = c(E.fib1, E.int1, E.pmat1, E.mata1, E.matb1, E.matc1,
                       E.insa1, E.insb1, E.mold1, E.cure1, E.fin1,
                       E.fib2, E.int2, E.pmat2, E.mata2, E.matb2, E.matc2,
                       E.insa2, E.insb2, E.mold2, E.cure2, E.fin2)
  
)

# Calc total energy per part
Data_energy <- Data_energy %>%
  rowwise() %>%
  mutate(finalenergy = mass_materials*energy_materials)


Data_energy
}

# Calc energy for each segment
