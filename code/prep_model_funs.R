


gen.params <- function(dat){
  ###################################
  # initialize the model

  params <- list(
    msm.bact =  rbeta(1, dat$shape1[dat$params=="msm.bact"], dat$shape2[dat$params=="msm.bact"]),
    msm.nact =  rlnorm(1, dat$shape1[dat$params=="msm.nact"], dat$shape2[dat$params=="msm.nact"]),
    msm.contact0 =  rlnorm(1, dat$shape1[dat$params=="msm.contact1"], dat$shape2[dat$params=="msm.contact1"]),
    msm.contact1 =  rlnorm(1, dat$shape1[dat$params=="msm.contact1"], dat$shape2[dat$params=="msm.contact1"]),
    msm.contact2 =  rlnorm(1, dat$shape1[dat$params=="msm.contact2"], dat$shape2[dat$params=="msm.contact2"]),
    msm.contact3 =  rlnorm(1, dat$shape1[dat$params=="msm.contact3"], dat$shape2[dat$params=="msm.contact3"]),
    msm.contact4 =  rlnorm(1, dat$shape1[dat$params=="msm.contact4"], dat$shape2[dat$params=="msm.contact4"]),
    msm.contact5 =  rlnorm(1, dat$shape1[dat$params=="msm.contact5"], dat$shape2[dat$params=="msm.contact5"]),
    msm.cond0 =  rbeta(1, dat$shape1[dat$params=="msm.cond1"], dat$shape2[dat$params=="msm.cond1"]),
    msm.cond1 =  rbeta(1, dat$shape1[dat$params=="msm.cond1"], dat$shape2[dat$params=="msm.cond1"]),
    msm.cond2 =  rbeta(1, dat$shape1[dat$params=="msm.cond2"], dat$shape2[dat$params=="msm.cond2"]),
    msm.cond3 =  rbeta(1, dat$shape1[dat$params=="msm.cond3"], dat$shape2[dat$params=="msm.cond3"]),
    msm.cond4 =  rbeta(1, dat$shape1[dat$params=="msm.cond4"], dat$shape2[dat$params=="msm.cond4"]),
    msm.cond5 =  rbeta(1, dat$shape1[dat$params=="msm.cond5"], dat$shape2[dat$params=="msm.cond5"]),
    msm.cond_eff =  rbeta(1, dat$shape1[dat$params=="msm.cond_eff"], dat$shape2[dat$params=="msm.cond_eff"]),
    wsm.bact =  rbeta(1, dat$shape1[dat$params=="wsm.bact"], dat$shape2[dat$params=="wsm.bact"]),
    wsm.nact   =  rlnorm(1, dat$shape1[dat$params=="wsm.nact"], dat$shape2[dat$params=="wsm.nact"]),
    wsm.contact0  =  rlnorm(1, dat$shape1[dat$params=="wsm.contact1"], dat$shape2[dat$params=="wsm.contact1"]),
    wsm.contact1  =  rlnorm(1, dat$shape1[dat$params=="wsm.contact1"], dat$shape2[dat$params=="wsm.contact1"]),
    wsm.contact2  =  rlnorm(1, dat$shape1[dat$params=="wsm.contact2"], dat$shape2[dat$params=="wsm.contact2"]),
    wsm.contact3  =  rlnorm(1, dat$shape1[dat$params=="wsm.contact3"], dat$shape2[dat$params=="wsm.contact3"]),
    wsm.contact4  =  rlnorm(1, dat$shape1[dat$params=="wsm.contact4"], dat$shape2[dat$params=="wsm.contact4"]),
    wsm.contact5  =  rlnorm(1, dat$shape1[dat$params=="wsm.contact5"], dat$shape2[dat$params=="wsm.contact5"]),
    wsm.cond0 =  rbeta(1, dat$shape1[dat$params=="wsm.cond1"], dat$shape2[dat$params=="wsm.cond1"]),
    wsm.cond1 =  rbeta(1, dat$shape1[dat$params=="wsm.cond1"], dat$shape2[dat$params=="wsm.cond1"]),
    wsm.cond2 =  rbeta(1, dat$shape1[dat$params=="wsm.cond2"], dat$shape2[dat$params=="wsm.cond2"]),
    wsm.cond3 =  rbeta(1, dat$shape1[dat$params=="wsm.cond3"], dat$shape2[dat$params=="wsm.cond3"]),
    wsm.cond4 =  rbeta(1, dat$shape1[dat$params=="wsm.cond4"], dat$shape2[dat$params=="wsm.cond4"]),
    wsm.cond5 =  rbeta(1, dat$shape1[dat$params=="wsm.cond5"], dat$shape2[dat$params=="wsm.cond5"]),
    wsm.cond_eff =  rbeta(1, dat$shape1[dat$params=="wsm.cond_eff"], dat$shape2[dat$params=="wsm.cond_eff"]),
    msw.bact =  rbeta(1, dat$shape1[dat$params=="msw.bact"], dat$shape2[dat$params=="msw.bact"]),
    msw.nact   =  rlnorm(1, dat$shape1[dat$params=="msw.nact"], dat$shape2[dat$params=="msw.nact"]),
    msw.contact0  =  rlnorm(1, dat$shape1[dat$params=="msw.contact1"], dat$shape2[dat$params=="msw.contact1"]),
    msw.contact1  =  rlnorm(1, dat$shape1[dat$params=="msw.contact1"], dat$shape2[dat$params=="msw.contact1"]),
    msw.contact2  =  rlnorm(1, dat$shape1[dat$params=="msw.contact2"], dat$shape2[dat$params=="msw.contact2"]),
    msw.contact3  =  rlnorm(1, dat$shape1[dat$params=="msw.contact3"], dat$shape2[dat$params=="msw.contact3"]),
    msw.contact4  =  rlnorm(1, dat$shape1[dat$params=="msw.contact4"], dat$shape2[dat$params=="msw.contact4"]),
    msw.contact5  =  rlnorm(1, dat$shape1[dat$params=="msw.contact5"], dat$shape2[dat$params=="msw.contact5"]),
    msw.cond0  =  rbeta(1, dat$shape1[dat$params=="msw.cond1"], dat$shape2[dat$params=="msw.cond1"]), 
    msw.cond1  =  rbeta(1, dat$shape1[dat$params=="msw.cond1"], dat$shape2[dat$params=="msw.cond1"]),       
    msw.cond2  =  rbeta(1, dat$shape1[dat$params=="msw.cond2"], dat$shape2[dat$params=="msw.cond2"]),
    msw.cond3  =  rbeta(1, dat$shape1[dat$params=="msw.cond3"], dat$shape2[dat$params=="msw.cond3"]),
    msw.cond4  =  rbeta(1, dat$shape1[dat$params=="msw.cond4"], dat$shape2[dat$params=="msw.cond4"]),
    msw.cond5  =  rbeta(1, dat$shape1[dat$params=="msw.cond5"], dat$shape2[dat$params=="msw.cond5"]),
    msw.cond_eff  =  rbeta(1, dat$shape1[dat$params=="msw.cond_eff"], dat$shape2[dat$params=="msw.cond_eff"]) 
  )
}


statefun <- function(dat, rr_re) {
  
  incid <- matrix(NA, length(dat$prev), length(rr_re))
  
  for (i in 1:length(rr_re)){
  incid[,i] <- 1- ( (1-rr_re[i]*dat$prev) + rr_re[i]*dat$prev*(1-dat$bact*(1-dat$vsupp_perc))^(dat$nact*(1-dat$cond))*(1-dat$bact*(1-dat$vsupp_perc)*(1-dat$cond_eff))^(dat$nact*dat$cond) )^dat$contact
  }
  
  colnames(incid) <- c("AI/AN", "Asian", "Black", "Hispanic", "NH/PI", "White")
  incid <- as.data.frame(incid)
  return(incid)
}

statefun_min <- function(dat, rr_re) {
  
  incid <- matrix(NA, length(dat$prev), length(rr_re))
  
  for (i in 1:length(rr_re)){
    incid[,i] <- 1- ( (1-rr_re[i]*dat$prev_min) + rr_re[i]*dat$prev_min*(1-dat$bact_min*(1-dat$vsupp_perc))^(dat$nact_min*(1-dat$cond_min))*(1-dat$bact_min*(1-dat$vsupp_perc)*(1-dat$cond_eff_min))^(dat$nact_min*dat$cond_min) )^dat$contact_min
  }
  
  colnames(incid) <- c("AI/AN_min", "Asian_min", "Black_min", "Hispanic_min", "NH/PI_min", "White_min")
  incid <- as.data.frame(incid)
  return(incid)
}

statefun_max <- function(dat, rr_re) {
  
  incid <- matrix(NA, length(dat$prev), length(rr_re))
  
  for (i in 1:length(rr_re)){
    incid[,i] <- 1- ( (1-rr_re[i]*dat$prev_max) + rr_re[i]*dat$prev_max*(1-dat$bact_max*(1-dat$vsupp_perc))^(dat$nact_max*(1-dat$cond_max))*(1-dat$bact_max*(1-dat$vsupp_perc)*(1-dat$cond_eff_max))^(dat$nact_max*dat$cond_max) )^dat$contact_max
  }
  
  colnames(incid) <- c("AI/AN_max", "Asian_max", "Black_max", "Hispanic_max", "NH/PI_max", "White_max")
  incid <- as.data.frame(incid)
  return(incid)
}

prepstatefun <- function(dat, rr_re) {
  
  incid <- matrix(NA, length(dat$prev), length(rr_re))
  
  for (i in 1:length(rr_re)){
  incid[,i]  <- 1- ( (1-rr_re[i]*dat$prev) + rr_re[i]*dat$prev*(1-dat$bact*(1-dat$vsupp_perc)*(1-prep.eff))^(dat$nact*(1-dat$cond))*(1-dat$bact*(1-dat$vsupp_perc)*(1-dat$cond_eff)*(1-prep.eff))^(dat$nact*dat$cond) )^dat$contact
  }
  
  colnames(incid) <- c("AI/AN_pr", "Asian_pr", "Black_pr", "Hispanic_pr", "NH/PI_pr", "White_pr")
  incid <- as.data.frame(incid)
  return(incid)
}

statefun_sdpairs <- function(dat) {
  
  incid <- 1- ((1-dat$bact_pair)^(dat$nact*(1-dat$cond_pair))*(1-dat$bact_pair*(1-dat$cond_eff))^(dat$nact*dat$cond_pair) )
  
  return(incid)
}

statefun_old <- function(dat) {
  
  incid <- 1- ( (1-dat$prev2) + dat$prev2*(1-dat$bact)^(dat$nact*(1-dat$cond))*(1-dat$bact*(1-dat$cond_eff))^(dat$nact*dat$cond) )^dat$contact
  
  return(incid)
}

prepstatefun_old <- function(dat) {
  
  incid <- 1- ( (1-dat$prev2) + dat$prev2*(1-dat$bact*(1-prep.eff))^(dat$nact*(1-dat$cond))*(1-dat$bact*(1-dat$cond_eff)*(1-prep.eff))^(dat$nact*dat$cond) )^dat$contact
  
  return(incid)
}

prepstatefun_sdpairs <- function(dat) {
  
  incid <- 1- ((1-dat$bact_pair*(1-prep.eff))^(dat$nact*(1-dat$cond_pair))*(1-dat$bact_pair*(1-prep.eff)*(1-dat$cond_eff))^(dat$nact*dat$cond_pair) )
  
  return(incid)
}

icerfun <- function(numseq){
  
  numseq2 <- numseq * 0.95
  icer <- (par$base_case[par$params=="prep_discounted"] - numseq2 * par$base_case[par$params=="hiv_discounted"] ) / (numseq2 * par$base_case[par$params=="qalys_lost_hiv"])
  
  tab <- tibble(risk = numseq, `risk reduction` = numseq2, icer = icer)
  return(tab)
}

berfun <- function(prev2, beta0, nact, safe, safe_eff, contact, rr) {
  
  incid <- 1-( (1-rr*prev2) + rr*prev2*(1-beta0)^(nact*(1-safe))*(1-beta0*(1-safe_eff))^(nact*safe) )^contact
  return(incid)
}