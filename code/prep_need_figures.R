
## ALLOCATION STRATEGY
df <- data.frame (risk = seq(0, 0.1, by=0.000005),
                  prep_100 = rep(1, length(seq(0, 0.1, by=0.000005))))

df$allocation_1 <- ifelse(df$risk<12/10^5, 0.3, 1)
df$allocation_2 <- ifelse(df$risk<36/10^5, 0.1, 1)

df %>%
  ggplot() + 
  geom_line(aes(x=risk, y=prep_100), colour="pink", size=2) +
  geom_line(aes(x=risk, y=allocation_1), colour="orange", size=2) +
  geom_line(aes(x=risk, y=allocation_2), colour="darkorange", size=2) +
  scale_x_continuous(trans='log10') +
  theme_bw() + xlab("HIV risk") + ylab("PrEP allocation") + ylim(c(0, 1.2)) +
  theme(legend.position = "bottom") 

df <- data.frame (quantiles = seq(0, 1, by=0.05),
                  wsm = quantile(pop.wsm.prepnd$incid, probs = seq(0, 1, by=0.05), na.rm=TRUE),
                  msw = quantile(pop.msw.prepnd$incid, probs = seq(0, 1, by=0.05), na.rm=TRUE),
                  msm = quantile(pop.msm.prepnd$incid, probs = seq(0, 1, by=0.05), na.rm=TRUE))


df %>%
  pivot_longer(!quantiles, names_to = "population", values_to = "risk") %>%
  ggplot(aes(y=risk, x=quantiles, color=population)) +
  geom_point() +
  geom_line() +
  #scale_y_continuous(trans='log2', breaks = c(1e-08, 1e-07, 1e-06, 1e-05, 1e-04, 1e-03, 1e-02, 1e-01, 1) ) + 
  ylab("Annual HIV risk") +
  xlab("Quantiles") +
  theme_minimal() + theme(legend.position = "bottom")


df2 <- data.frame (risk = quantile(pop.wsm.prepnd$incid, probs = seq(0, 1, by=0.05), na.rm=TRUE),
                   `prep_100` = rep(1, 21),
                   prep_grad = c(seq(0.20, 1, by=0.05), rep(1, 4)),
                   step2 = c(rep(0.1, 10), rep(0.5, 9), rep(1, 2)),
                   step1 = c(rep(0.3, 17), rep(1, 4)))

df2 %>%
  pivot_longer(!risk, names_to = "allocation", values_to = "proportion") %>%
  filter(allocation == "prep_100") %>%
  ggplot(aes(y=proportion, x=risk, color=allocation)) +
  geom_line(size=1) +
  scale_color_manual(values = c( "#E69F00", "grey20", "pink", "maroon")) +
  scale_x_continuous(trans='log2', breaks = c(1e-08, 1e-07, 1e-06, 1e-05, 1e-04, 1e-03, 1e-02, 1e-01, 1) ) + 
  ylim(c(0,1)) +
  ylab("Proportion") +
  xlab("Risk") +
  theme_minimal() + theme(legend.position = "bottom")



#################
# MSM compared to CDC
msm_state_age %>%
  filter(Age.Group!="13-17" | Age.Group!="13-24") %>%
  group_by(state) %>%
  summarize(prep_msm_cdc=sum(prep_msm_cdc),
            prep_100_min=sum(prep_100_min),
            prep_100_max=sum(prep_100_max)) %>%
  ungroup() %>%
  ggplot()+
  geom_point(aes(y=reorder(state, prep_msm_cdc), x=prep_msm_cdc), colour="#0072B2", size = 3, alpha = 4/10) +
  geom_linerange(aes(y=reorder(state, prep_msm_cdc), xmin=prep_100_min, xmax=prep_100_max), colour="maroon", size = 3, alpha = 2/10) +
  # scale_color_manual(values = c("#0072B2", "#E69F00", "grey20", "pink", "maroon")) +
  #facet_wrap(~Age.Group, nrow=1) +
  scale_x_continuous(trans='log10', limits =c(10,  10^7)) + #breaks = c(1, 10, 100, 1000, 10^4, 10^5, 10^6)) +
 # facet_wrap(~Age.Group) +
  theme_bw() + xlab("Number needing PrEP (log scale)") + ylab("") +
  theme(legend.position = "bottom") 

msm_state_age %>%
  filter(Age.Group!="13-24") %>%
  ggplot()+
  geom_point(aes(y=reorder(state, prep_msm_cdc), x=prep_msm_cdc), colour="#0072B2", size = 3, alpha = 4/10) +
  geom_linerange(aes(y=reorder(state, prep_msm_cdc), xmin=prep_100_min, xmax=prep_100_max), colour="maroon", size = 3, alpha = 2/10) +
  # scale_color_manual(values = c("#0072B2", "#E69F00", "grey20", "pink", "maroon")) +
  #facet_wrap(~Age.Group, nrow=1) +
  scale_x_continuous(trans='log10', limits =c(1,  10^7)) + #breaks = c(1, 10, 100, 1000, 10^4, 10^5, 10^6)) +
  facet_wrap(~Age.Group, ncol=6) +
  theme_bw() + xlab("Number needing PrEP (log scale)") + ylab("") +
  theme(legend.position = "bottom") 

msm_state_age %>%
  ggplot()+
  geom_point(aes(y=reorder(state, prep_msm_cdc), x=prep_msm_cdc), colour="#0072B2", size = 3, alpha = 4/10) +
  geom_linerange(aes(y=reorder(state, prep_msm_cdc), xmin=prep_100_min, xmax=prep_100_max), colour="maroon", size = 3, alpha = 2/10) +
  # scale_color_manual(values = c("#0072B2", "#E69F00", "grey20", "pink", "maroon")) +
  #facet_wrap(~Age.Group, nrow=1) +
  scale_x_continuous(trans='log10', limits =c(10,  10^7)) + #breaks = c(1, 10, 100, 1000, 10^4, 10^5, 10^6)) +
  facet_wrap(~Age.Group) +
  theme_bw() + xlab("Number needing PrEP (log scale)") + ylab("") +
  theme(legend.position = "bottom") 



#################

msm_state_age %>%
  #filter(!is.na(prep_100_min)) %>%
  filter(Age.Group!="13-24") %>%
  ggplot()+
  #geom_point(aes(y=reorder(state, prep_msm_cdc), x=prep_msm_cdc, color=Age.Group),  size = 3, alpha = 4/10) +
  geom_linerange(aes(y=reorder(state, prep_100_min), xmin=prep_100_min, xmax=prep_100_max, color=Age.Group), size = 3, alpha = 2/10) +
  # scale_color_manual(values = c("#0072B2", "#E69F00", "grey20", "pink", "maroon")) +
  #facet_wrap(~Age.Group, nrow=1) +
  scale_x_continuous(trans='log10', limits =c(1,  10^7)) + #breaks = c(1, 10, 100, 1000, 10^4, 10^5, 10^6)) +
  facet_wrap(~Age.Group, ncol=6) +
  theme_bw() + xlab("Number needing PrEP (log scale)") + ylab("") +
  theme(legend.position = "bottom") + theme(legend.position = "none")

msm_state_age %>%
  filter(Age.Group=="13-17" | Age.Group=="18-24") %>%
  ggplot()+
  #geom_point(aes(y=reorder(state, prep_msm_cdc), x=prep_msm_cdc, color=Age.Group),  size = 3, alpha = 4/10) +
  geom_linerange(aes(y=reorder(state, prep_msm_cdc), xmin=prep_100_min, xmax=prep_100_max, color=Age.Group), size = 3, alpha = 2/10) +
  # scale_color_manual(values = c("#0072B2", "#E69F00", "grey20", "pink", "maroon")) +
  #facet_wrap(~Age.Group, nrow=1) +
  scale_x_continuous(trans='log10', limits =c(10,  10^7)) + #breaks = c(1, 10, 100, 1000, 10^4, 10^5, 10^6)) +
  #facet_wrap(~Age.Group, ncol=6) +
  theme_bw() + xlab("Number needing PrEP (log scale)") + ylab("") +
  theme(legend.position = "bottom") #+ theme(legend.position = "none")

################

results_all %>%
  filter(state !="Northern Mariana Islands") %>%
  filter(state !="American Samoa") %>%
  filter(state !="Guam") %>%
  filter(state !="US Virgin Islands") %>%
  filter(state !="Vermont") %>%
  filter(state !="North Dakota") %>%
  filter(state !="Utah") %>%
  filter(state !="Montana") %>%
  filter(state !="Missouri") %>%
  filter(state !="Idaho") %>%
  filter(state !="Delaware") %>%
  filter(state !="Massachusetts") %>%
  filter(state !="New Hampshire") %>%
  # filter(Sex == "Female") %>%
  # filter(Sex == "Male") %>%
  group_by(state) %>%
  summarize(cdc_estimate=sum(cdc_estimate),
            prep_100_min=sum(prep_100_min),
            prep_100_max=sum(prep_100_max),
            prep_step1_min=sum(prep_step1_min),
            prep_step1_max=sum(prep_step1_max))  %>% #-> table_res
   ungroup() %>%
  #select(-c("prep_incid", "prep_100", "prep_incid_min", "prep_incid_max", "prep_step1") ) %>%
 # pivot_longer( cols = c("cdc_estiamte", "prep_100_min",  "prep_100_max", "prep_step1_min", "prep_step1_max"), names_to = "type", values_to = "estimate") %>%
  ggplot()+
    geom_point(aes(y=reorder(state, cdc_estimate), x=cdc_estimate), colour="#0072B2", size = 3, alpha = 4/10) +
    geom_linerange(aes(y=reorder(state, cdc_estimate), xmin=prep_100_min, xmax=prep_100_max), colour="maroon", size = 3, alpha = 2/10) +
    geom_linerange(aes(y=reorder(state, cdc_estimate), xmin=prep_step1_min, xmax=prep_step1_max), colour ="#E69F00",  size = 3, alpha = 4/10) +
    # scale_color_manual(values = c("#0072B2", "#E69F00", "grey20", "pink", "maroon")) +
    #facet_wrap(~Age.Group, nrow=1) +
    scale_x_continuous(trans='log10', limits =c(100, 10^7)) + #breaks = c(1, 10, 100, 1000, 10^4, 10^5, 10^6)) +
    theme_bw() + xlab("Number needing PrEP (log scale)") + ylab("") +
    theme(legend.position = "bottom") 



incid_comp %>%
  filter(state !="Northern Mariana Islands") %>%
  filter(state !="American Samoa") %>%
  filter(state !="Guam") %>%
  filter(state !="US Virgin Islands") %>%
  ggplot()+
  geom_point(aes(y=state, x=risk_est),color="blue", size = 4, alpha = 4/10) +
  geom_pointrange(aes(y=state, x=cases, xmin = cases_ll, xmax = cases_ul), alpha = 4/10) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("annual HIV incidence estimates")


incid_comp %>%
  filter(state != "District of Columbia") %>%
  ggplot(aes(y=state, x=risk_rate))+
  geom_pointrange(aes(y=state, x=risk_rate, xmin = risk_rate_min, xmax = risk_rate_max),color="blue", size = 0.7, alpha = 5/10) +
  geom_pointrange(aes(y=state, x=rate, xmin = rate_ll, xmax = rate_ul), size = 0.7,  alpha = 5/10) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  #geom_text(aes(label=round(rate_ratio,2), y=state, x=45)) +
  xlab("annual HIV incidence estimates")

incid_comp %>%
  filter(state == "District of Columbia") %>%
  ggplot(aes(y=state, x=risk_rate))+
  geom_pointrange(aes(y=state, x=risk_rate, xmin = risk_rate_min, xmax = risk_rate_max),color="blue", size = 0.7, alpha = 4/10) +
  geom_pointrange(aes(y=state, x=rate, xmin = rate_ll, xmax = rate_ul), size = 0.7,  alpha = 3/10) +
  theme_bw() +
  theme(legend.position = "bottom") +
#  geom_text(aes(label=round(rate_ratio,2), y=state, x=180)) +
  xlim(c(0, 200)) +
  xlab("annual HIV incidence estimates")


## penalty 
incid_comp %>%
  ggplot(aes(y=ratio, x=rate_ratio))+
  geom_point(alpha = 3/10) +
  geom_smooth(method=lm, se=TRUE) +
  stat_cor(aes(label = ..r.label..), method = "pearson", size = 4, col = "red", label.x = 5, label.y = 5) +
  theme_bw() +
  ylim(c(0,NA)) +
  theme(legend.position = "none") +
  geom_vline(xintercept=1, color = "red") +
  xlab("Rate ratio: Bernoull.model / HIV incidence") + ylab("Rate ratio: HIV prevalence to HIV diagnoses")

incid_comp %>%
  ggplot(aes(y=`rate_HIV diagnoses`, x=rate_ratio)) +
  geom_point( alpha = 3/10) +
  geom_smooth(method=lm, se=TRUE) +
  stat_cor(aes(label = ..r.label..), method = "pearson", size = 4, col = "red", label.x = 5, label.y = 5) +
  ylim(c(0,NA)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept=1, color = "red") +
  xlab("Rate ratio: Bernoull.model / HIV incidence") + ylab("HIV diagnoses")


incid_comp %>%
  ggplot(aes(y=rate, x=rate_ratio))+
  geom_point(alpha = 3/10) +
  geom_smooth(method=lm, se=TRUE) +
  stat_cor(aes(label = ..r.label..), method = "pearson", size = 4, col = "red", label.x = 5, label.y = 5) +
  ylim(c(0,NA)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept=1, color = "red") +
  xlab("Rate ratio: Bernoull.model / HIV incidence") + ylab("HIV incidence")

