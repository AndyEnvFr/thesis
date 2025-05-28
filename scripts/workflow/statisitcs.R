library(mgcv)

res_glm <- lapply(res, function(x){
  return(x %>% mutate(census = as.numeric(census)))
})

results <- lapply(res, function(x)){
  fm1 <- glm(formula = "mort ~ con + census", family = binomial(), data = x)
  coeflist <- 
}


fm1 <- glm(formula = "mort ~ con * census", family = binomial(), data = res_glm[["ddF1_disp1_sr2_eF_fbmr1_dc4"]])
fm2 <- glm(formula = "mort ~ con * census", family = binomial(), data = res_glm[["ddT1_disp1_sr2_eF_fbmr10_dc4"]])
summary(fm1)
summary(fm2)

head(res_glm[[3]])

fg1 <- gam(formula = "mort ~ te(con, census)", family = binomial(), data = res_glm[["ddT1_disp1_sr2_eF_fbmr10_dc4"]])

