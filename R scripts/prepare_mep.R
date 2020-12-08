mep.ids <- read.csv("https://raw.githubusercontent.com/gabgilling/Thesis/master/Data/mep_data_with_IDs.csv")

mep.ids$date_begin <- as.Date(mep.ids$date_begin, format = "%m/%d/%y")
mep.ids$date_end <- as.Date(mep.ids$date_end, format = "%m/%d/%y")
mep.ids$constituency <- with(mep.ids, ifelse(constituency %in% 
                                               c("Massif-central–Central", "Massif Central-Centre",  "Massif central-Centre","Massif central – Centre"),
                                             "Massif-central-Centre", constituency))

mep.ids$constituency <- with(mep.ids, ifelse(constituency %in% c("Île-de-France", "Ile-de-France"), "Ile de France" , constituency))

mep.ids$Parliament <- with(mep.ids, ifelse(date_begin >= "2004-07-20" & date_end <= "2009-06-07", 6, 
                                           ifelse(date_begin > "2009-06-07" & date_end <= "2014-07-14", 7,
                                                  ifelse(date_begin >= "2014-07-15" & date_end <= "2020-01-01", 8, Parliament))))

mep.ids$ideo6 <- ""
mep.ids$ideo6 <- with(mep.ids, ifelse(Group %in% c("Party of European Socialists ", "Party of European Socialists"), "Left", ideo6))

mep.ids$ideo6 <- with(mep.ids, ifelse(Group %in% c("The Greens–European Free Alliance", "European Green Party"), "Greens", ideo6))

mep.ids$ideo6 <- with(mep.ids, ifelse(Group %in% c("European United Left–Nordic Green Left", "Parti de la gauche européenne"), "Far-Left", ideo6))

mep.ids$ideo6 <- with(mep.ids, ifelse(Group %in% c("Europe of Freedom and Democracy", "Independence/Democracy","Europe of Nations and Freedom", "Identity, Tradition, Sovereignty", "Europe of Freedom and Direct Democracy","EFD", "Alliance of European National Movements"), "Far-Right", ideo6))

mep.ids$ideo6 <- with(mep.ids, ifelse(Group %in% c("European Free Alliance", "NI" ,"Non-Inscrits" ), "Other", ideo6))

mep.ids$ideo6 <- with(mep.ids, ifelse(Group %in% c("Alliance of Conservatives and Reformists in Europe", "European People's Party", "European Conservatives and Reformists Party", "Union for Europe of the Nations"), "Right", ideo6))

mep.ids$ideo6 <- with(mep.ids, ifelse(Group %in% c("Alliance of Liberals and Democrats for Europe", "Union for Europe of the Nations", "Parti démocrate européen", "Alliance of Liberals and Democrats for Europe Party"), "Center", ideo6))

#write.csv(mep.ids, "/Users/gabgilling/Documents/GitHub/Thesis/Data/mep_data_with_IDs.csv")
