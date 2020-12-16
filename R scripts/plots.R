total.prefs <- readRDS("/Users/gabgilling/Documents/Documents - Gabriel’s MacBook Pro/GitHub/Thesis/Data/total.prefs.rds")

prefs.plot.total <- ggplot(total.prefs %>% 
                             group_by(Constituency, country) %>% 
                             summarise(mean_pref = mean(mean_pref), mean_se = mean(mean_se)),
                           aes(x= reorder(Constituency, mean_pref), y = mean_pref, color = country)) + 
  geom_point() + 
  geom_errorbar(aes(ymin= mean_pref - mean_se, ymax= mean_pref + mean_se), width=0) +
  theme_bw() +
  ggtitle("Preferences For Environmental Regulation\nEuropean Parliament Constituencies\n2004-2019") +
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Constituency", y = "Mean Preference") +
  coord_flip() + 
  scale_color_discrete(name="Country", labels = c("France", "Italy", "United Kingdom"))

ggsave("/Users/gabgilling/Documents/Documents - Gabriel’s MacBook Pro/GitHub/Thesis/prefs_plot_total.jpg") 

prefs.plot.total.yearly <- ggplot(total.prefs %>% 
                                    group_by(year) %>% 
                                    summarise(mean_pref = mean(mean_pref), mean_se = mean(mean_se)),
                                  aes(x= year, y = mean_pref)) + 
  geom_point() + 
  geom_errorbar(aes(ymin= mean_pref - mean_se, ymax= mean_pref + mean_se), width=0) +
  theme_classic() +
  ggtitle("") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Year", y = "Mean Preference") 

ggsave("/Users/gabgilling/Documents/Documents - Gabriel’s MacBook Pro/GitHub/Thesis/prefs_plot_total_yearly.jpg")

means <- total.prefs %>% 
  group_by(Constituency, parliament, country) %>% 
  summarise(mean_pref = mean(mean_pref), mean_se = mean(mean_se)) %>% 
  group_by(parliament) %>% summarise(m = mean(mean_pref))



ggplot(total.prefs %>% 
         group_by(Constituency, parliament, country) %>% 
         summarise(mean_pref = mean(mean_pref), mean_se = mean(mean_se)),
       aes(x= reorder(Constituency, mean_pref), y = mean_pref, color = country))+ 
  geom_point() +
  geom_hline(data = means, aes(yintercept = m), linetype = 'dashed')+
  geom_errorbar(aes(ymin= mean_pref - mean_se, ymax= mean_pref + mean_se), width=0) +
  theme_bw() +
  facet_grid(~parliament) +
  ggtitle("Preferences For Environmental Regulation\nEuropean Parliament Constituencies\nby Term") +
  theme(axis.text.x = element_text(angle = 90, size = 6), plot.title = element_text(hjust = 0.5),
        legend.position = c(.77,.865), legend.key.size = unit(0.125, "cm"),
        legend.background=element_blank()) +
  labs(x = "Constituency", y = "Mean Preference") +
  scale_color_discrete(name="Country", labels = c("France", "Italy", "United\nKingdom")) + 
  coord_flip() 



ggsave("/Users/gabgilling/Documents/Documents - Gabriel’s MacBook Pro/GitHub/Thesis/Plots/prefs_total_term.jpg")

### MEPs

mep.ids$Parliament <- ifelse(mep.ids$Parliament == 6, "6th Term (2004-2009)", 
                             ifelse(mep.ids$Parliament == 7, "7th Term (2009-2014)", "8th Term (2014-2019)"))

ggplot(mep.ids %>% 
         group_by(constituency, country) %>% 
         mutate(mean_pref = mean(mean_pref),
                mean_sd = mean(mean_sd)),
       aes(x = reorder(constituency, -mean_pref), y = mean_pref, color = country)) + 
  geom_point() + 
  geom_errorbar(aes(ymin= mean_pref - mean_sd, ymax= mean_pref + mean_sd), width=0) +
  ggtitle("Preferences For MEPs Environmental Regulation\nEuropean Parliament Constituencies\n2004-2019") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) +
  coord_flip() +
  scale_color_discrete(name="Country", labels = c("France", "Italy", "United Kingdom")) +
  labs(x = "Constituency", y = "Mean Preference")

# ggsave("/Users/gabgilling/Documents/Documents - Gabriel’s MacBook Pro/GitHub/Thesis/Plots/meps_total.jpg")


means <- mep.ids %>% 
  group_by(constituency, Parliament, country) %>% 
  summarise(mean_pref = mean(mean_pref), mean_sd = mean(mean_sd)) %>% 
  group_by(Parliament) %>% summarise(m = mean(mean_pref))

ggplot(mep.ids %>% 
         group_by(constituency, Parliament, country) %>% 
         mutate(mean_pref = mean(mean_pref),
                mean_sd = mean(mean_sd)),
       aes(x = reorder(constituency, mean_pref), y = mean_pref, color = country)) + 
  geom_point() + 
  geom_errorbar(aes(ymin= mean_pref - mean_sd, ymax= mean_pref + mean_sd), width=0) +
  facet_wrap(~Parliament) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 6), plot.title = element_text(hjust = 0.5),
        legend.position = c(.084,.865), legend.key.size = unit(0.125, "cm"),
        legend.background=element_blank()) + 
  ggtitle("MEP Preferences For Environmental Regulation\nEuropean Parliament Constituencies\nby Term") +
  coord_flip() +
  scale_color_discrete(name="Country", labels = c("France", "Italy", "United\nKingdom")) +
  labs(x = "Constituency", y = "Mean Preference") +
  geom_hline(data = means, aes(yintercept = m), linetype = 'dashed')


means <- mep.ids %>% 
  group_by(constituency, ideo6) %>% 
  summarise(mean_pref = mean(mean_pref), mean_sd = mean(mean_sd)) %>% 
  group_by(ideo6) %>% summarise(m = mean(mean_pref))


ggplot(mep.ids %>% 
         group_by(constituency, ideo6) %>% 
         summarise(mean_pref = mean(mean_pref),
                mean_sd = mean(mean_sd)),
       aes(x = reorder(constituency, mean_pref), y = mean_pref)) + 
  geom_point() + 
  geom_errorbar(aes(ymin= mean_pref - mean_sd, ymax= mean_pref + mean_sd), width=0) +
  facet_wrap(~ideo6) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 6), plot.title = element_text(hjust = 0.5),
        legend.position = c(.084,.865), legend.key.size = unit(0.125, "cm"),
        legend.background=element_blank()) + 
  ggtitle("MEP Preferences For Environmental Regulation\nEuropean Parliament Constituencies\nby Term") +
  coord_flip() +
  scale_color_discrete(name="Country", labels = c("France", "Italy", "United\nKingdom")) +
  labs(x = "Constituency", y = "Mean Preference") +
  geom_hline(data = means, aes(yintercept = m), linetype = 'dashed')


