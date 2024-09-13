
#####################################

betapars <- function(mu, var) {
  # This function calculates the beta and alpha (shape) parameters for beta likelhood, taking the model output (mean) and variance from data (sd^2)
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  
  return(params = list(alpha = alpha, beta = beta))
}

#####################################
## SIR version
pwsex_sir <- function(pnat, pr.msm, pr.wsm, pr.msw, vs.msm, vs.wsm, vs.msw) {
  
  msm.inc <-  wsm.inc <-  msw.inc <- array(NA, dim= c(length(pr.wsm), 7, 5))
  
  dimnames(msm.inc) <-   dimnames(wsm.inc) <-   dimnames(msw.inc) <- list(sub("_.*", "", names(pr.msm)), 
                                                                          c("Black", "Hispanic", "AIAN", "NHOPI",  "Multiracial", "Asian",  "White"), 
                                                                          c("13-24","25-34", "35-44", "45-54", "55+"))
 
  re.rr <- pnat[grep("RR.", names(pnat))]
  re.msm <- c(re.rr[grep("msm", names(re.rr))], rr.white = 1)
  re.wsm <- c(re.rr[grep("wsm", names(re.rr))], rr.white = 1)
  re.msw <- c(re.rr[grep("msw", names(re.rr))], rr.white = 1)
  
  msm.cond <- pnat[grep("msm.cond", names(pnat))]
  wsm.cond <- pnat[grep("wsm.cond", names(pnat))]
  msw.cond <- pnat[grep("msw.cond", names(pnat))]
  
  msm.pcr <- pnat[grep("msm.contact", names(pnat))]
  wsm.pcr <- pnat[grep("wsm.contact", names(pnat))]
  msw.pcr <- pnat[grep("msw.contact", names(pnat))]
  
  
  
  for (i in 1:length(re.msm)){
    for (a in 1:5){
      msm.inc[,i,a] <- 1- ( (1-re.msm[i]*pr.msm) + re.msm[i]*pr.msm*(1-pnat["msm.bact"]*(1-vs.msm))^(pnat["msm.nact"]*(1- msm.cond[a]))*(1-pnat["msm.bact"]*(1-vs.msm)*(1-pnat["msm.cond_eff"]))^(pnat["msm.nact"]*msm.cond[a]) )^msm.pcr[a]
    }
  }
  
  for (i in 1:length(re.wsm)){
    for (a in 1:5){
      wsm.inc[,i,a] <- 1- ( (1-re.wsm[i]*pr.wsm) + re.wsm[i]*pr.wsm*(1-pnat["wsm.bact"]*(1-vs.wsm))^(pnat["wsm.nact"]*(1- wsm.cond[a]))*(1-pnat["wsm.bact"]*(1-vs.wsm)*(1-pnat["wsm.cond_eff"]))^(pnat["wsm.nact"]*wsm.cond[a]) )^wsm.pcr[a]
    }
  } 
  
  for (i in 1:length(re.msw)){
    for (a in 1:5){
      msw.inc[,i,a] <- 1- ( (1-re.msw[i]*pr.msw) + re.msw[i]*pr.msw*(1-pnat["msw.bact"]*(1-vs.msw))^(pnat["msw.nact"]*(1- msw.cond[a]))*(1-pnat["msw.bact"]*(1-vs.msw)*(1-pnat["msw.cond_eff"]))^(pnat["msw.nact"]*msw.cond[a]) )^msw.pcr[a]
    }
  }
  
  
  return(list(msm.inc=msm.inc, wsm.inc=wsm.inc, msw.inc=msw.inc))
}

############### PWID model




###############IMIS version
pwsex_imis <- function(pnat, pr.msm, pr.wsm, pr.msw, vs.msm, vs.wsm, vs.msw) {
  
  msm.inc <-  wsm.inc <-  msw.inc <- array(NA, dim= c(length(pr.wsm), length(rr_re_msm), 5))
  
  dimnames(msm.inc) <-   dimnames(wsm.inc) <-   dimnames(msw.inc) <- list(sub("_.*", "", names(pr.msm)), 
                                                                          c("AIAN", "Asian", "Black", "Hispanic", "NHOPI", "White"), 
                                                                          c("13-24","25-34", "35-44", "45-54", "55+"))
  
  msm.cond <- pnat[grep("msm.cond", names(pnat))]
  wsm.cond <- pnat[grep("wsm.cond", names(pnat))]
  msw.cond <- pnat[grep("msw.cond", names(pnat))]
  
  msm.pcr <- pnat[grep("msm.contact", names(pnat))]
  wsm.pcr <- pnat[grep("wsm.contact", names(pnat))]
  msw.pcr <- pnat[grep("msw.contact", names(pnat))]
  
  
  
  for (i in 1:length(rr_re_msm)){
    for (a in 1:5){
    msm.inc[,i,a] <- 1- ( (1-rr_re_msm[i]*pr.msm) + rr_re_msm[i]*pr.msm*(1-pnat["msm.bact"]*(1-vs.msm))^(pnat["msm.nact"]*(1- msm.cond[a]))*(1-pnat["msm.bact"]*(1-vs.msm)*(1-pnat["msm.cond_eff"]))^(pnat["msm.nact"]*msm.cond[a]) )^msm.pcr[a]
    }
  }
  
  for (i in 1:length(rr_re_wsm)){
    for (a in 1:5){
      wsm.inc[,i,a] <- 1- ( (1-rr_re_wsm[i]*pr.wsm) + rr_re_wsm[i]*pr.wsm*(1-pnat["wsm.bact"]*(1-vs.wsm))^(pnat["wsm.nact"]*(1- wsm.cond[a]))*(1-pnat["wsm.bact"]*(1-vs.wsm)*(1-pnat["wsm.cond_eff"]))^(pnat["wsm.nact"]*wsm.cond[a]) )^wsm.pcr[a]
    }
  } 
  
  for (i in 1:length(rr_re_msw)){
    for (a in 1:5){
      msw.inc[,i,a] <- 1- ( (1-rr_re_msw[i]*pr.msw) + rr_re_msw[i]*pr.msw*(1-pnat["msw.bact"]*(1-vs.msw))^(pnat["msw.nact"]*(1- msw.cond[a]))*(1-pnat["msw.bact"]*(1-vs.msw)*(1-pnat["msw.cond_eff"]))^(pnat["msw.nact"]*msw.cond[a]) )^msw.pcr[a]
    }
  }
  

  return(list(msm.inc=msm.inc, wsm.inc=wsm.inc, msw.inc=msw.inc))
}

pwid_model <- function(n, r, c, b, p, t) {
  # n: average number of injections annually
  # r: proportion of shared-injections
  # c: average number of sharing partners annually
  # b: probability of HCV transmission per shared-injection
  # p: HIV prevalence among PWID adjusted for knowledge of status
  # t: proportion of viral suppression among HIV-positive PWID
  I <- 1 - ((1 - p) + p * (1 - (1 - t) * b)^((n / c) * r))^c
  return(I)
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



