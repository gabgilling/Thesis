
eb88.1 <- readRDS("/Users/gabgilling/Documents/Documents - Gabriel’s MacBook Pro/GitHub/Thesis/Data/eb881.rds")
fit.88.1 <- readRDS("/Users/gabgilling/Documents/Documents - Gabriel’s MacBook Pro/GitHub/Thesis/Models/fit.881.rds")

p1 <- plot(fit.88.1[[8]])

p1 + ggtitle("Hierarchical Model Parameter Estimates") + xlab("Coefficient")

m1 <- fit.88.1[[8]]

hist(m1$fitted.values)

eb88.1 %>% group_by(age_cat2, gender_male, ms, Constituency, country) %>%
  summarise(pro = sum(eu.moreprojects == 1), against = n() - pro) %>% 
  na.omit()

poststrat <- census2011_ms
poststrat_region <- poststrat[poststrat$Constituency == "Ile de France", ] %>% na.omit()

tail(poststrat_region %>% group_by(Constituency, age_cat2, gender_male, ms) %>% summarise(N = n()))

posterior_prob_region <- posterior_epred(
  fit.88.1[[8]],
  # transform = TRUE,
  draws = 1000,
  newdata = as.data.frame(poststrat_region)
)

par(pin = c(2,2), cex.main = 0.75)
hist(posterior_prob_region[,1], main = "Probability Distribution for Supporting Question 20D\nCell Type 1", 
     xlab= "Probability")

par(pin = c(2,2), cex.main = 0.75)
hist(poststrat_prob_region, main = "Probability Distribution for Supporting Question 20D\nIle De France Constituency",
     xlab = "Probability")

prefs.881 <- readRDS("/Users/gabgilling/Documents/Documents - Gabriel’s MacBook Pro/GitHub/Thesis/Data/prefs.881.rds")

sup.moreeu <- prefs.881[[2]][[8]]
sup.moreeu$pref <- sup.moreeu$pref*100
sup.moreeu$se <- sup.moreeu$se*100

ggplot(sup.moreeu,
       aes(x= reorder(Constituency, pref), y = pref, color = country)) + 
  geom_point() + 
  geom_errorbar(aes(ymin= pref - se, ymax= pref + se), width=0) +
  theme_bw() +
  ggtitle("Average Level of Support for EB88.1 QD20") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Constituency", y = "Mean Preference %") +
  coord_flip() +
  scale_color_discrete(name="Country", labels = c("France", "Italy", "United Kingdom")) +
  theme(plot.title = element_text(hjust = 0.5))
