t_prior <- student_t(df = 7, location = 0, scale = 2.5, autoscale=TRUE)
t_prior_int <- student_t(df = 7, location = 0, scale = 2.5, autoscale=TRUE)

uni_cessation24 <- stan_glm(cessation_24 ~ treatment, data = trial_dta,
                            family = binomial(link = "logit"), 
                            prior = t_prior,
                            prior_intercept = t_prior_int, 
                            seed = SEED, refresh=0, chains = 10)

summary(uni_cessation24)
#shinystan::launch_shinystan(uni_cessation24, ppd = TRUE)

#posterior_vs_prior(uni_cessation24)
#ggsave(file="posterior_prior.pdf", width=6, height=4, unit='in', dpi=500)

prior_summary(uni_cessation24)

p_direction(uni_cessation24)

round(exp(coef(uni_cessation24)), 2)


round(exp(posterior_interval(uni_cessation24, prob = 0.95)), 2)


pplot<-plot(uni_cessation24, "intervals", prob = 0.95, prob_outer = 0.95, trans="exp",
            pars = c("treatment1"))

plot_dat <- pplot$data
plot_dat$parameter <- as.character(plot_dat$parameter)
plot_dat[1,1] <- "Cytisine treatment (univariable)"

week_24_dta <- pplot$data
week_24_dta$parameter <- as.character(week_24_dta$parameter)
week_24_dta[1,1] <- "Cytisine treatment"

multi_cessation24 <- stan_glm(cessation_24 ~ treatment + dependence + age
                              + female + krs , data = trial_dta, 
                              family = binomial(link = "logit"), 
                              prior = t_prior, prior_intercept = t_prior,
                              seed = SEED, refresh=0, chains=10)

summary(multi_cessation24)
#shinystan::launch_shinystan(multi_cessation24, ppd = TRUE)

p_direction(multi_cessation24)

prior_summary(multi_cessation24)

round(exp(coef(multi_cessation24)), 2) 


round(exp(posterior_interval(multi_cessation24, prob = 0.95)), 2) 


prior_summary(multi_cessation24)


pplot<-plot(multi_cessation24, "intervals", prob = 0.95, prob_outer = 1, trans="exp",
            pars = c("treatment1", "dependence", "age", "female", "krs"))

plot_dat2 <- pplot$data
plot_dat2$parameter <- as.character(plot_dat2$parameter)
plot_dat2[1,1] <- "Cytisine treatment (multivariable)"
plot_dat2[2,1] <- "Fagerström test category"
plot_dat2[3,1] <- "Age (unit: 10 years)"
plot_dat2[4,1] <- "Sex: female"
plot_dat2[5,1] <- "Educational attainment level"
plot_dat <- plot_dat[1,]
plot_dat <- rbind(plot_dat, plot_dat2)



plot_dat$fill <- c("black", "white", "white", "white", "white", "white")

ckbplotr::forest_plot(panels=list(plot_dat),
                      col.key = "parameter",
                      col.estimate = "m",
                      col.lci = "l",
                      col.uci = "h",
                      nullval = 1,
                      xlim=c(0.3, 1.8),
                      exponentiate = FALSE,
                      col.right.heading = "OR (95% CI)",
                      xlab = "Odds ratio and 95% credible interval",
                      pointsize=3, 
                      shape=22,
                      fill="fill",
                      stroke=0.7,
                      logscale = TRUE,
                      ciunder=TRUE,
                      xticks = c(0.3, 0.6, 0.8, 1, 1.2, 1.5, 1.8),
                      showcode = TRUE,
                      base_size = 11,
                      col.heading.space = -0.5,
                      scalepoints = FALSE) + theme(family="serif") 

ggsave(file="cessation_forest.pdf", width=6, height=4, unit='in', dpi=500)
ggsave(file="cessation_forest.png", width=6, height=4, unit='in', dpi=500)

#ggsave('cessation_forest.pdf', width=6, height=4, unit='in', dpi=500)

#ADHERENCE


t_prior <- student_t(df = 7, location = 0, scale = 2.5, autoscale=TRUE)
t_prior_int <- student_t(df = 7, location = 0, scale = 2.5, autoscale=TRUE)

uni_adherence <- stan_glm(adherence ~ treatment, data = trial_dta,
                          family = binomial(link = "logit"), 
                          prior = t_prior,
                          prior_intercept = t_prior_int, 
                          seed = SEED, refresh=0, chains = 10)

summary(uni_adherence)
prior_summary(uni_adherence)

p_direction(uni_adherence)

round(exp(coef(uni_adherence)), 2)


round(exp(posterior_interval(uni_adherence, prob = 0.95)), 2)

pplot<-plot(uni_adherence, "intervals", prob = 0.95, prob_outer = 0.95, trans="exp",
            pars = c("treatment1"))

plot_dat <- pplot$data
plot_dat$parameter <- as.character(plot_dat$parameter)
plot_dat[1,1] <- "Cytisine treatment (univariable)"

multi_adherence <- stan_glm(adherence ~ treatment + dependence + age
                            + female + krs , data = trial_dta, 
                            family = binomial(link = "logit"), 
                            prior = t_prior, prior_intercept = t_prior,
                            seed = SEED, refresh=0, chains=10)

summary(multi_adherence)
p_direction(multi_adherence)

prior_summary(multi_adherence)

round(exp(coef(multi_adherence)), 2) 

round(exp(posterior_interval(multi_adherence, prob = 0.95)), 2) 


prior_summary(multi_adherence)

pplot<-plot(multi_adherence, "intervals", prob = 0.95, prob_outer = 1, trans="exp",
            pars = c("treatment1", "dependence", "age", "female", "krs"))

plot_dat2 <- pplot$data
plot_dat2$parameter <- as.character(plot_dat2$parameter)
plot_dat2[1,1] <- "Cytisine treatment (multivariable)"
plot_dat2[2,1] <- "Fagerström test category"
plot_dat2[3,1] <- "Age (unit: 10 years)"
plot_dat2[4,1] <- "Sex: female"
plot_dat2[5,1] <- "Educational attainment level"
plot_dat <- plot_dat[1,]
plot_dat <- rbind(plot_dat, plot_dat2)

plot_dat$fill <- c("black", "white", "white", "white", "white", "white")

ckbplotr::forest_plot(panels=list(plot_dat),
                      col.key = "parameter",
                      col.estimate = "m",
                      col.lci = "l",
                      col.uci = "h",
                      nullval = 1,
                      xlim=c(0.6, 3),
                      exponentiate = FALSE,
                      col.right.heading = "OR (95% CI)",
                      xlab = "Odds ratio and 95% credible interval",
                      pointsize=3, 
                      shape=22,
                      fill="fill",
                      stroke=0.7,
                      logscale = TRUE,
                      ciunder=TRUE,
                      xticks = c(0.6, 0.8, 1, 1.5, 2, 2.5, 3),
                      showcode = TRUE,
                      base_size = 11,
                      col.heading.space = -0.5,
                      scalepoints = FALSE) + theme(family="serif") 

ggsave(file="adherence_forest.pdf", width=6, height=4, unit='in', dpi=500)
ggsave(file="adherence_forest.png", width=6, height=4, unit='in', dpi=500)

##

t_prior <- student_t(df = 7, location = 0, scale = 2.5, autoscale=TRUE)
t_prior_int <- student_t(df = 7, location = 0, scale = 2.5, autoscale=TRUE)


uni_cessation24_hier <- stan_glmer(cessation_24 ~ treatment + (1 | therapist), data = trial_dta,
                                   family = binomial(link = "logit"), 
                                   prior = t_prior,
                                   prior_intercept = t_prior_int, 
                                   seed = SEED, refresh=0, chains = 10)

summary(uni_cessation24_hier)
#shinystan::launch_shinystan(uni_cessation24_hier, ppd = TRUE)

#posterior_vs_prior(uni_cessation24_hier)
#ggsave(file="posterior_prior.pdf", width=6, height=4, unit='in', dpi=500)

prior_summary(uni_cessation24_hier)

p_direction(uni_cessation24_hier)

sum <- summary(uni_cessation24_hier, 
               pars = c("Intercept", "treatment1"),
               probs = c(0.025, 0.975),
               digits = 2)

round(exp(sum[1]), 2)


round(exp(posterior_interval(uni_cessation24_hier, prob = 0.95)), 3)

pplot<-plot(uni_cessation24_hier, "intervals", prob = 0.95, prob_outer = 0.95, trans="exp",
            pars = c("treatment1"))

plot_dat <- pplot$data
plot_dat$parameter <- as.character(plot_dat$parameter)
plot_dat[1,1] <- "Cytisine treatment (univariable)"


sims <- as.matrix(uni_cessation24_hier)

mu_a_sims <- as.matrix(uni_cessation24_hier, 
                       pars = "(Intercept)")

u_sims <- as.matrix(uni_cessation24_hier, 
                    regex_pars = "b\\[\\(Intercept\\) therapist\\:")

a_sims <- as.numeric(mu_a_sims) + u_sims



s__alpha_sims <- as.matrix(uni_cessation24_hier, 
                           pars = "Sigma[therapist:(Intercept),(Intercept)]")

a_mean <- apply(X = a_sims,    
                MARGIN = 2,
                FUN = mean)
a_sd <- apply(X = a_sims,     
              MARGIN = 2,
              FUN = sd)


a_quant <- apply(X = a_sims, 
                 MARGIN = 2, 
                 FUN = quantile, 
                 probs = c(0.025, 0.50, 0.975))
a_quant <- data.frame(t(a_quant))
names(a_quant) <- c("Q2.5", "Q50", "Q97.5")

a_df <- data.frame(a_mean, a_sd, a_quant)
round(head(a_df), 2)


a_df <- a_df[order(a_df$a_mean), ]
a_df$a_rank <- c(1 : dim(a_df)[1])  




ggplot(data = a_df, 
       aes(x = a_rank, 
           y = a_mean)) +
  geom_pointrange(aes(ymin = Q2.5, 
                      ymax = Q97.5),
                  position = position_jitter(width = 0.1, 
                                             height = 0)) + 
  geom_hline(yintercept = mean(a_df$a_mean), 
             size = 0.5, 
             col = "red") + 
  scale_x_continuous("Rank", 
                     breaks = seq(from = 0, 
                                  to = 80, 
                                  by = 5)) + 
  scale_y_continuous(expression(paste("varying intercept, ", alpha[j]))) + 
  theme_bw( base_family = "serif")

ggsave(file="cessation_intercepts_hier.pdf", width=6, height=4, unit='in', dpi=500)

multi_cessation24_hier <- stan_glmer(cessation_24 ~ treatment + dependence + age
                                     + female + krs + (1 | therapist), data = trial_dta, 
                                     family = binomial(link = "logit"), 
                                     prior = t_prior, prior_intercept = t_prior,
                                     seed = SEED, refresh=0, chains=10)


sims <- as.matrix(multi_cessation24_hier)

mu_a_sims <- as.matrix(multi_cessation24_hier, 
                       pars = "(Intercept)")

u_sims <- as.matrix(multi_cessation24_hier, 
                    regex_pars = "b\\[\\(Intercept\\) therapist\\:")

a_sims <- as.numeric(mu_a_sims) + u_sims


s__alpha_sims <- as.matrix(multi_cessation24_hier, 
                           pars = "Sigma[therapist:(Intercept),(Intercept)]")


a_mean <- apply(X = a_sims,     
                MARGIN = 2,
                FUN = mean)
a_sd <- apply(X = a_sims,       
              MARGIN = 2,
              FUN = sd)

a_quant <- apply(X = a_sims, 
                 MARGIN = 2, 
                 FUN = quantile, 
                 probs = c(0.025, 0.50, 0.975))
a_quant <- data.frame(t(a_quant))
names(a_quant) <- c("Q2.5", "Q50", "Q97.5")


a_df <- data.frame(a_mean, a_sd, a_quant)
round(head(a_df), 2)


a_df <- a_df[order(a_df$a_mean), ]
a_df$a_rank <- c(1 : dim(a_df)[1])   


ggplot(data = a_df, 
       aes(x = a_rank, 
           y = a_mean)) +
  geom_pointrange(aes(ymin = Q2.5, 
                      ymax = Q97.5),
                  position = position_jitter(width = 0.1, 
                                             height = 0)) + 
  geom_hline(yintercept = mean(a_df$a_mean), 
             size = 0.5, 
             col = "red") + 
  scale_x_continuous("Rank", 
                     breaks = seq(from = 0, 
                                  to = 80, 
                                  by = 5)) + 
  scale_y_continuous(expression(paste("varying intercept, ", alpha[j]))) + 
  theme_bw( base_family = "serif")

ggsave(file="cessation_intercepts_hier_multivariable.pdf", width=6, height=4, unit='in', dpi=500)

#shinystan::launch_shinystan(multi_cessation24_hier, ppd = TRUE)

p_direction(multi_cessation24_hier)


prior_summary(multi_cessation24_hier)

sum <- summary(multi_cessation24_hier, 
               pars = c("treatment1"),
               probs = c(0.025, 0.975),
               digits = 2)

round(exp(sum[1]), 2)

round(exp(posterior_interval(multi_cessation24_hier, prob = 0.95)), 3)

prior_summary(multi_cessation24_hier)

pplot<-plot(multi_cessation24_hier, "intervals", prob = 0.95, prob_outer = 1, trans="exp",
            pars = c("treatment1", "dependence", "age", "female", "krs"))

plot_dat2 <- pplot$data
plot_dat2$parameter <- as.character(plot_dat2$parameter)
plot_dat2[1,1] <- "Cytisine treatment (multivariable)"
plot_dat2[2,1] <- "Fagerström test category"
plot_dat2[3,1] <- "Age (unit: 10 years)"
plot_dat2[4,1] <- "Sex: female"
plot_dat2[5,1] <- "Educational attainment level"
plot_dat <- plot_dat[1,]
plot_dat <- rbind(plot_dat, plot_dat2)


plot_dat$fill <- c("black", "white", "white", "white", "white", "white")

ckbplotr::forest_plot(panels=list(plot_dat),
                      col.key = "parameter",
                      col.estimate = "m",
                      col.lci = "l",
                      col.uci = "h",
                      nullval = 1,
                      xlim=c(0.3, 1.8),
                      exponentiate = FALSE,
                      col.right.heading = "OR (95% CI)",
                      xlab = "Odds ratio and 95% credible interval",
                      pointsize=3, 
                      shape=22,
                      fill="fill",
                      stroke=0.7,
                      logscale = TRUE,
                      ciunder=TRUE,
                      xticks = c(0.3, 0.6, 0.8, 1, 1.2, 1.5, 1.8),
                      showcode = TRUE,
                      base_size = 11,
                      col.heading.space = -0.5,
                      scalepoints = FALSE) + theme(family="serif") 

ggsave(file="cessation_forest_hier.pdf", width=6, height=4, unit='in', dpi=500)
ggsave(file="cessation_forest_hier.png", width=6, height=4, unit='in', dpi=500)
