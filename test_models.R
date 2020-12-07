library(lme4)
library(rstanarm)

temp <- bind_rows(eb75.2, eb81.3, eb68.2, eb88.1)

temp <- merge(temp, reg_preds_constituencies, by = "Constituency", all.x = T)

temp <- temp %>% group_by(age_cat2, gender_male, ms, Constituency, country, gdp_cap2, 
                          higher_ed2,hhd_income2, unemp_pct2) %>%
  summarise(pro = sum(eu.leg == 1), against = n() - pro) %>% 
  na.omit()


eu.leg.lme4 <- glmer(cbind(pro, against) ~ (1|country) + (1|Constituency) + (1 | age_cat2)  + (1|ms) + gdp_cap2  +  gender_male, data = temp , family = binomial(link = "logit"))

eu.leg.stan <-  stan_glmer(cbind(pro, against) ~ (1|country) + (1|Constituency) + 
                             (1 | age_cat2)  + (1|ms) + gdp_cap2  + unemp_pct2 + 
                             gender_male, data = temp , family = binomial(link = "logit"), 
                           adapt_delta = 0.99,
                           chains = 4, 
                           cores = 4)

est <- poststrat_survey(list(eu.leg.stan))[[1]]

invlogit <- plogis

individual.model <- eu.leg.lme4

poststrat <- census2011_ms
N <- length(unique(poststrat$Constituency))

state.ranefs <- array(NA,c(N+1,1))
dimnames(state.ranefs) <- list(c(unique(poststrat$Constituency),"effect"))

for (i in unique(poststrat$Constituency)) {
  state.ranefs[i,1] <- ranef(individual.model)$Constituency[i,1] 
  
}

state.ranefs[,1][is.na(state.ranefs[,1])] <- 0

cellpred <- invlogit(fixef(individual.model)["(Intercept)"]
                     + ranef(individual.model)$country[poststrat$country,1]
                     + ranef(individual.model)$age_cat2[poststrat$age_cat2,1]
                     + ranef(individual.model)$ms[poststrat$ms,1]
                     + state.ranefs[poststrat$Constituency,1]
                     + (fixef(individual.model)["gdp_cap2"] * poststrat$gdp_cap2) 
                     + (fixef(individual.model)["gender_male"] * poststrat$gender_male) 
                       )

cellpredweighted <- cellpred * (poststrat$freq / poststrat$total_n) 

statepred <- 100* as.vector(tapply(cellpredweighted, poststrat$Constituency,sum, na.rm = T)

statepred <- as.data.frame(statepred)
statepred$Constituency <- unique(poststrat$Constituency)
statepred$statepred <- statepred$statepred/100

merge(statepred, est, by = 'Constituency')
