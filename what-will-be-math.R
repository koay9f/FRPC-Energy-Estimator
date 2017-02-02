# The math for mass change calculations
# mass.in = mass.out / yield

library(tidyverse)

yfs <- function(scrap){(1-scrap)}

#Define which yields are being used ----
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
massfracs <- function(ff_use_default, ff_default, ff_user, fm_pri, foa, fob, foc, fia, fib) {
  ff <- if(ff_use_default) {
    ff_default
  }else ff_user
  
  fm <- sum(fm_pri, foa, fob, foc)
  fi <- sum(fia, fib)
  massfracs_list <- c(ff,fm,fi)
  massfracs_list
    }
  
massfracs1 <- massfracs(output$moldfracUSERYN1, moldfracNum1z,output$moldfracUSERNum1, fm1, foa1, fob1, foc1, fia1, fib1)  
massfracs2 <- massfracs(output$moldfracUSERYN2, moldfracNum2z,output$moldfracUSERNum2, fm2, foa2, fob2, foc2, fia2, fib2)


# prepreg yn
prepregYN <- function(prepreg){
 YN <- if(prepreg) {
   1
 } else 0
 YN}
YN1 <- prepregYN(intprepregYN1z)
YN2 <- prepregYN(intprepregYN2z)



#BUILD DATAFRAME YIELD ----
techset <- c(rep("ts1", 9), rep("ts2", 9))
stage <- c(rep(c("finish", "mold", "layup"),6))
material <- c(rep(c(rep("fiber",3), rep("matrix", 3), rep("insert", 3)), 2))
massfrac <- c (rep(massfrac1[1], 3),rep(massfrac1[2],3), rep(massfrac1[3],3), rep(massfrac2[1], 3), rep(massfrac2[2],3), rep(massfrac2[3],3))
stageyield <-  c(rep(c(finishyield1, moldyield1, layupyield1), 6), rep(c(finishyield2, moldyield2, layupyield2),6))
applyyield <- c(1,1,1, 1,1,YN1, 0, 1, 0, 1,1,1, 1,1,YN2, 0, 1, 0)

Data_yield <- data_frame(
  techset = techset,
  material = material,
  stage = stage,
  massfrac = massfrac,
  stageyield = stageyield,
  applyyield = applyyield
)
  
yield <- function(material,stage, techset){
  yield.df <- Data_yield %>% filter(material == material & stage == stage & techset == techset)
  yield <- yield.df["stageyield"]
  yield}

applyyield_fxn <- function(material,stage, techset){
  yield.df <- Data_yield %>% filter (material == material & stage == stage & techset == techset)
  applyyield <- yield.df[applyyield]
  applyyield}
  
finalmass_material <- function(material,stage, techset) {
  finalmass <- if (techset == "ts1") {
    finalpartmass1Z
  } else finalpartmass2z
       massfrac <- function(material, stage, techset){
       massfrac <- Data_yield %>% filter (material == material & stage == stage & techset == techset)
       massfrac}
      finall_mass_material <- function(finalmass,massfrac){
       final_mass_material <- finalmass*massfrac}
  final_mass_material
}

yield_actual_gen <- function(material,stage, techset,){
  switch(stage,
         finish = (yield(material,"finish", techset)) ^ (applyyield_fxn(material,"finish", techset)),
         mold = (yield(material,"finish", techset) * yield(material,"mold", techset)) ^ (applyyield_fxn(material,"mold", techset)),
         layup = (yield(material,"finish", techset) * yield(material,"mold", techset) * yield(material,"layup", techset)) ^ (applyyield_fxn(material,"layup", techset)))
  yield_actual_gen }


mass_initial_gen <- function(finalpartmass, yield){finalpartmass/yield}



Data_yield <- Data_yield %>%
  mutate(yield_actual = yield_actual_gen(material == material,stage == stage, techset == techset, applyyield == applyyield)) %>%
  mutate(material_mass = finalmass_material(material == material,techset == techset, massfrac == massfrac)) %>%
  mutate(mass_initial = mass_initial_gen(material_mass == finalpartmass, yield_actual == yield)) 