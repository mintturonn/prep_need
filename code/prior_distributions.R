
library(here)
library(rriskDistributions)
library(readxl)

betavars <- function(mu1, var1){
  alpha1 = ((1 - mu1) / var1 - 1 / mu1) * mu1 ^ 2;
  beta1 = alpha1 * (1 / mu1 - 1);
  
  return(list(alpha=alpha1, beta=beta1))
}

## data

read_excel(here("data/params.xlsx"), sheet = "nat_hist") -> pars

###############################################################################

p <- c(0.025, 0.50, 0.975) 

shapes <- matrix(NA, length(pars$params), 3)
rownames(shapes) <- pars$params

pars_bact <- c("msm.bact", "wsm.bact", "msw.bact", 
               "wsm.cond1", "wsm.cond2", "wsm.cond3", "wsm.cond4", "wsm.cond5", "msm.cond_eff",
               "msm.cond1", "msm.cond2", "msm.cond3", "msm.cond4", "msm.cond5", "wsm.cond_eff",
               "msw.cond1", "msw.cond2", "msw.cond3", "msw.cond4", "msw.cond5", "msw.cond_eff")

for (i in 1:length(pars_bact)){
 
 q <- cbind(pars$base_min[pars$params==pars_bact[i]], pars$base_case[pars$params==pars_bact[i]],  pars$base_max[pars$params==pars_bact[i]])  

 fit.bact <- round(get.beta.par(p, as.vector(q), show.output = FALSE, tol = 0.001),3)
 
 txt <- round(quantile(rbeta(10^5, fit.bact[1], fit.bact[2]) , probs = c(0.5, 0.025, 0.975)),3)
 shapes[pars_bact[i], ] <- c(fit.bact[1], fit.bact[2], paste0(txt[1], " (", txt[2], "-", txt[3], ")") )

}

pars_nums <- c("msm.nact", "msm.contact1","msm.contact2", "msm.contact3","msm.contact4","msm.contact5",  
               "wsm.nact","wsm.contact1","wsm.contact2","wsm.contact3","wsm.contact4","wsm.contact5",
               "msw.nact","msw.contact1","msw.contact2","msw.contact3","msw.contact4","msw.contact5" )

for (i in 1:length(pars_nums)){
  
  q <- cbind(pars$base_min[pars$params==pars_nums[i]], pars$base_case[pars$params==pars_nums[i]],  pars$base_max[pars$params==pars_nums[i]])  
  
  fit.lnorm <- round(get.lnorm.par(p, as.vector(q), show.output = FALSE, tol = 0.0001),3)
  
  txt <- round(quantile(rlnorm(10^5, fit.lnorm[1], fit.lnorm[2]) , probs = c(0.5, 0.025, 0.975)),3)
  shapes[pars_nums[i], ] <- c(fit.lnorm[1], fit.lnorm[2], paste0(txt[1], " (", txt[2], "-", txt[3], ")") )
  
}


