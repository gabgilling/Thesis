library(haven)
library(dplyr)

## eb81.3
eb81.3 <- read_dta("/Users/gabgilling/Downloads/ZA5914_v3-0-0.dta")

nuts_eb_dict <- read.csv("/Users/gabgilling/Documents/Documents - Gabriel’s MacBook Pro/GitHub/Thesis/Data/nuts_eb_dictionary.csv")

eb81.3$gender_male <- ifelse(eb81.3$d10 == 1, 1, 0)

# for education census
# eb81.3$age_cat1 <- with(eb81.3, ifelse(d11 >= 18 & d11 < 25, "Y18-24",
#                                ifelse(d11 >= 25 & d11 < 30, "Y25-29",
#                                ifelse( d11 >= 30 & d11 < 50, "Y30-49",
#                                       ifelse(d11 >= 50 & d11 < 65, "Y50-64",
#                                       ifelse(d11 >= 65 & d11 < 85, "Y65-84",
#                                       ifelse(d11 >= 85, "Y_GE85",
#                                       NA)))))))

# for marital status census
eb81.3$age_cat2 <- with(eb81.3, ifelse(d11 >= 15 & d11 < 30, "Y15-29",
                                       ifelse(d11 >= 30 & d11 < 50, "Y30-49",
                                              ifelse(d11 >= 50 & d11 < 65, "Y50-64",
                                                      ifelse(d11 >= 65 & d11 < 85, "Y65-84",
                                                             ifelse(d11 >= 85, "Y_GE85",
                                                                    NA))))))

eb81.3$ms <- with(eb81.3, ifelse(d7 %in% c(1,2,3,4), "MAR",
                                 ifelse(d7 %in% c(5,6,7,8), "REP",
                                        ifelse(d7 %in% c(9,10), "SIN",
                                               ifelse(d7 %in% c(11,12), "DIV",
                                                      ifelse(d7 %in% c(13,14), "WID",
                                                             NA))))))


#eb81.3$edu <- eb81.3$qb1

eb81.3$NUTS_eb_new <- with(eb81.3, ifelse(isocntry == "FR", paste("FR", eb81.3$p7fr, sep = ""), 
                                      ifelse(isocntry %in% c("GB-GBN", "GB-NIR"), paste("UK", eb81.3$p7gb, sep = ""),
                                             ifelse(isocntry == "IT", paste("IT", eb81.3$p7it_r1, sep = ""), NA))))


eb81.3 <- eb81.3 %>% filter (isocntry %in% c('FR', 'GB-GBN', "GB-NIR", 'IT'))

eb81.3 <- merge(eb81.3, nuts_eb_dict %>% select("Constituency", "Region.Name", "NUTS_eb_new"), by = "NUTS_eb_new", all.x = T)


eb81.3$year <- 2014
eb81.3$parliament <- 7

eb81.3$eu.leg <- ifelse(eb81.3$qa18_1 %in% c(1,2), 1, ifelse(eb81.3$qa18_1 %in% c(3,4), 0, NA))
eb81.3$eu.enf <- ifelse(eb81.3$qa18_2 %in% c(1,2), 1, ifelse(eb81.3$qa18_2 %in% c(3,4), 0, NA))
eb81.3$eu.asst <- ifelse(eb81.3$qa18_3 %in% c(1,2), 1, ifelse(eb81.3$qa18_3 %in% c(3,4), 0, NA))
eb81.3$eu.fund <- ifelse(eb81.3$qa18_4 %in% c(1,2), 1, ifelse(eb81.3$qa18_4 %in% c(3,4), 0, NA))
eb81.3$cooperation <- ifelse(eb81.3$qa15 == 2, 1, ifelse(eb81.3$qa15 == 1, 0, NA))
eb81.3$envorecon <- ifelse(eb81.3$qa14 == 1, 1, ifelse(eb81.3$qa15 == 2, 0, NA))
eb81.3$env.important <- ifelse(eb81.3$qa1 %in% c(1,2), 1, ifelse(eb81.3$qa1 %in% c(3,4), 0, NA))

eb81.3$country <- ifelse(eb81.3$isocntry %in% c("GB-GBN", "GB-NIR"), "UK", eb81.3$isocntry)


eb81.3 <- eb81.3 %>% select(gender_male, age_cat2, NUTS_eb_new, ms, country, year, parliament, Constituency, Region.Name,
                  eu.leg, eu.enf, eu.asst, eu.fund, cooperation, envorecon, env.important)



## eb75.2
eb75.2 <- read_dta("/Users/gabgilling/Downloads/ZA5480_v4-0-1.dta")

eb75.2 <- eb75.2 %>% filter(v7 %in% c("GB-GBN", "GB-NIR", "FR", "IT"))

eb75.2$NUTS_eb_old <- with(eb75.2, ifelse(v7 == "FR", paste("FR", eb75.2$v660, sep = ""), 
                                      ifelse(v7 %in% c("GB-GBN", "GB-NIR"), paste("UK", eb75.2$v673, sep = ""),
                                             ifelse(v7 == "IT", paste("IT", eb75.2$v668, sep = ""), NA))))

eb75.2 <- merge(eb75.2, nuts_eb_dict %>% select("Constituency", "Region.Name", "NUTS_eb_old"), by = "NUTS_eb_old", all.x = T)

#unique(eb75.2$NUTS_eb)


eb75.2$gender_male <- ifelse(eb75.2$v601 == 1, 1, 0)

eb75.2$age_cat2 <- with(eb75.2, ifelse(v602 >= 15 & v602 < 30, "Y15-29",
                                       ifelse(v602>= 30 & v602 < 50, "Y30-49",
                                              ifelse(v602 >= 50 & v602 < 65, "Y50-64",
                                                     ifelse(v602 >= 65 & v602 < 85, "Y65-84",
                                                            ifelse(v602 >= 85, "Y_GE85",
                                                                   NA))))))

eb75.2$ms <- with(eb75.2, ifelse(v596 %in% c(1,2,3,4), "MAR",
                                 ifelse(v596 %in% c(5,6,7,8), "REP",
                                        ifelse(v596 %in% c(9,10), "SIN",
                                               ifelse(v596 %in% c(11,12), "DIV",
                                                      ifelse(v596 %in% c(13,14), "WID",
                                                             NA))))))

eb75.2$isocntry <- eb75.2$v7
eb75.2$year <- 2011
eb75.2$parliament <- 7

eb75.2$env.important<- ifelse(eb75.2$v183 %in% c(1,2), 1, ifelse(eb75.2$v183 %in% c(3,4), 0, NA))
eb75.2$envorecon <- ifelse(eb75.2$v337 == 1 & eb75.2$v338 == 2, 1, ifelse(eb75.2$v337 == 3 | eb75.2$v338 == 3, 0, NA))
eb75.2$cooperation <- ifelse(eb75.2$v339 == 2, 1, ifelse(eb75.2$v339== 1, 0, NA))
eb75.2$eu.leg <- ifelse(eb75.2$v354 %in% c(1,2), 1, ifelse(eb75.2$v354 %in% c(3,4), 0, NA))
eb75.2$eu.asst <- ifelse(eb75.2$v355 %in% c(1,2), 1, ifelse(eb75.2$v355 %in% c(3,4), 0, NA))
eb75.2$eu.fund <- ifelse(eb75.2$v356 %in% c(1,2), 1, ifelse(eb75.2$v356 %in% c(3,4), 0, NA))

eb75.2$country <- ifelse(eb75.2$isocntry %in% c("GB-GBN", "GB-NIR"), "UK", eb75.2$isocntry)

eb75.2 <- eb75.2 %>% select(gender_male, age_cat2, NUTS_eb_old, year, ms, parliament, country, Constituency, Region.Name,
                          env.important, envorecon, cooperation, eu.leg,
                          eu.asst, eu.fund)

## 83.4
eb83.4 <- read_dta("/Users/gabgilling/Downloads/ZA6595_v3-0-0.dta")
eb83.4$gender_male <- ifelse(eb83.4$d10 == 1, 1, 0)

eb83.4$age_cat2 <- with(eb83.4, ifelse(d11 >= 15 & d11 < 30, "Y15-29",
                                       ifelse(d11 >= 30 & d11 < 50, "Y30-49",
                                              ifelse( d11 >= 50 & d11 < 65, "Y50-64",
                                                      ifelse(d11 >= 65 & d11 < 85, "Y65-84",
                                                             ifelse(d11 >= 85, "Y_GE85",
                                                                ifelse(d11 == 99, NA, NA)))))))

eb83.4$ms <- with(eb83.4, ifelse(d7 %in% c(1,2,3,4), "MAR",
                                 ifelse(d7 %in% c(5,6,7,8), "REP",
                                        ifelse(d7 %in% c(9,10), "SIN",
                                               ifelse(d7 %in% c(11,12), "DIV",
                                                      ifelse(d7 %in% c(13,14), "WID",
                                                             NA))))))

eb83.4$NUTS_eb_new <- with(eb83.4, ifelse(isocntry == "FR", paste("FR", p7fr, sep = ""), 
                                      ifelse(isocntry %in% c("GB-GBN", "GB-NIR"), paste("UK", p7gb, sep = ""),
                                             ifelse(isocntry == "IT", paste("IT", p7it_r1, sep = ""), NA))))


eb83.4 <- eb83.4 %>% filter (isocntry %in% c('FR', 'GB-GBN', "GB-NIR", 'IT'))

eb83.4 <- merge(eb83.4, nuts_eb_dict %>% select("Constituency", "Region.Name", "NUTS_eb_new"), by = "NUTS_eb_new", all.x = T)


eb83.4$year <- 2015
eb83.4$parliament <- 8


eb83.4$cc.serious <- with(eb83.4, ifelse(qa2r  >= 6, 1, 0))
eb83.4$cc.boosteconjobs <- with(eb83.4, ifelse(qa4_2 %in% c(1,2), 1, 0))

eb83.4$renewtargets <- with(eb83.4, ifelse(qa7 %in% c(1,2), 1, 
                                           ifelse(qa7 %in% c(3,4), 0, NA)))
eb83.4$supenergyeff <- with(eb83.4, ifelse(qa8 %in% c(1,2), 1, 
                                           ifelse(qa8 %in% c(3,4), 0, NA)))

eb83.4$country <- ifelse(eb83.4$isocntry %in% c("GB-GBN", "GB-NIR"), "UK", eb83.4$isocntry)


# qb are about biodiversity
eb83.4 <- eb83.4 %>% select(gender_male, age_cat2, NUTS_eb_new, ms, year, parliament, country, Constituency, Region.Name,
                            cc.serious, cc.boosteconjobs, renewtargets, supenergyeff)

## 88.1
eb88.1 <- read_dta("/Users/gabgilling/Downloads/ZA6925_v1-0-0.dta")
eb88.1$gender_male <- ifelse(eb88.1$d10 == 1, 1, 0)

eb88.1$age_cat2 <- with(eb88.1, ifelse(d11 >= 15 & d11 < 30, "Y15-29",
                                       ifelse(d11 >= 30 & d11 < 50, "Y30-49",
                                              ifelse( d11 >= 50 & d11 < 65, "Y50-64",
                                                      ifelse(d11 >= 65 & d11 < 85, "Y65-84",
                                                             ifelse(d11 >= 85, "Y_GE85",
                                                                    ifelse(d11 == 99, NA, NA)))))))

eb88.1$ms <- with(eb88.1, ifelse(d7 %in% c(1,2,3,4), "MAR",
                                 ifelse(d7 %in% c(5,6,7,8), "REP",
                                        ifelse(d7 %in% c(9,10), "SIN",
                                               ifelse(d7 %in% c(11,12), "DIV",
                                                      ifelse(d7 %in% c(13,14), "WID",
                                                             NA))))))

eb88.1$NUTS_eb_new <- with(eb88.1, ifelse(isocntry == "FR", paste("FR", p7fr, sep = ""), 
                                      ifelse(isocntry %in% c("GB-GBN", "GB-NIR"), paste("UK", p7gb, sep = ""),
                                             ifelse(isocntry == "IT", paste("IT", p7it_r1, sep = ""), NA))))


eb88.1 <- eb88.1 %>% filter (isocntry %in% c('FR', 'GB-GBN', "GB-NIR", 'IT'))

eb88.1 <- merge(eb88.1, nuts_eb_dict %>% select("Constituency", "Region.Name", "NUTS_eb_new"), by = "NUTS_eb_new", all.x = T)


eb88.1$year <- 2017
eb88.1$parliament <- 8

eb88.1$env.important <- ifelse(eb88.1$qd1 %in% c(1,2), 1, 0)
eb88.1$polluters.resp <- ifelse(eb88.1$qd5_2 %in% c(1,2), 1, 0)
eb88.1$cooperation <- ifelse(eb88.1$qd6 == 2, 1, 0)

eb88.1$qd7_1 <- ifelse(eb88.1$qd7_1 == 4, NA, eb88.1$qd7_1)
eb88.1$qd7_2 <- ifelse(eb88.1$qd7_2 == 4, NA, eb88.1$qd7_2)
eb88.1$qd7_3 <- ifelse(eb88.1$qd7_3 == 4, NA, eb88.1$qd7_3)
eb88.1$qd7_4 <- ifelse(eb88.1$qd7_4 == 4, NA, eb88.1$qd7_4)
eb88.1$qd7_5 <- ifelse(eb88.1$qd7_5 == 4, NA, eb88.1$qd7_5)
eb88.1$qd7_6 <- ifelse(eb88.1$qd7_6 == 4, NA, eb88.1$qd7_6)

eb88.1$env.enough <- with(eb88.1, 
                          ifelse(qd7_1+ qd7_2+ qd7_3 + qd7_4 + qd7_5 + qd7_6>= 12, 1, 0))

# eb88.1$stricterreg <- with(eb88.1, ifelse(qd8_4== 1, 1, 0))
# eb88.1$enfreg <- with(eb88.1, ifelse(qd8_2 == 1, 1, 0))
# eb88.1$highertax <- with(eb88.1, ifelse(qd8_6 == 1, 1, 0))
# eb88.1$heavierfines <- with(eb88.1, ifelse(qd8_3 == 1, 1, 0))
# eb88.1$financialincentives <- with(eb88.1, ifelse(qd8_5 == 1, 1, 0))
# eb88.1$investtech <- with(eb88.1, ifelse(qd8_7 == 1, 1, 0))
# eb88.1$training <- with(eb88.1, ifelse(qd8_8 == 1, 1, 0))

eb88.1$eu.leg <- ifelse(eb88.1$qd9_1 %in% c(1,2), 1, ifelse(eb88.1$qa18_1 %in% c(3,4), 0, NA))
eb88.1$eu.enf <- ifelse(eb88.1$qd9_2 %in% c(1,2), 1, ifelse(eb88.1$qa18_2 %in% c(3,4), 0, NA))
eb88.1$eu.asst <- ifelse(eb88.1$qd9_3 %in% c(1,2), 1, ifelse(eb88.1$qa18_3 %in% c(3,4), 0, NA))

eb88.1$eu.moreprojects <- ifelse(eb88.1$qd20 %in% c(1,2), 1, 0)

eb88.1$country <- ifelse(eb88.1$isocntry %in% c("GB-GBN", "GB-NIR"), "UK", eb88.1$isocntry)


eb88.1 <- eb88.1 %>% select(gender_male, age_cat2, ms, NUTS_eb_new, year, parliament, 
                            country, Constituency, Region.Name,
                            env.important, polluters.resp, cooperation, env.enough, 
                            eu.leg, eu.enf, eu.asst, eu.moreprojects)

## eb 68.2
eb68.2<- read_dta("/Users/gabgilling/Downloads/ZA4742_v4-0-1.dta")

eb68.2$gender_male <- ifelse(eb68.2$v618 == 1, 1, 0)

eb68.2$age_cat2 <- with(eb68.2, ifelse(v619 >= 15 & v619 < 30, "Y15-29",
                                       ifelse(v619>= 30 & v619 < 50, "Y30-49",
                                              ifelse(v619 >= 50 & v619 < 65, "Y50-64",
                                                     ifelse(v619 >= 65 & v619 < 85, "Y65-84",
                                                            ifelse(v619 >= 85, "Y_GE85",
                                                                   NA))))))

eb68.2$ms <- with(eb68.2, ifelse(v615 %in% c(1,2,3,4), "MAR",
                                 ifelse(v615 %in% c(5,6,7,8), "REP",
                                        ifelse(v615 %in% c(9,10), "SIN",
                                               ifelse(v615 %in% c(11,12), "DIV",
                                                      ifelse(v615 %in% c(13,14), "WID",
                                                             NA))))))

eb68.2 <- eb68.2 %>% filter(v7 %in% c("GB-GBN", "GB-NIR", "FR", "IT"))

eb68.2$NUTS_eb_old <- with(eb68.2, ifelse(v7 == "FR", paste("FR", v685, sep = ""), 
                                      ifelse(v7 %in% c("GB-GBN", "GB-NIR"), paste("UK", v691, sep = ""),
                                             ifelse(v7 == "IT", paste("IT", v714, sep = ""), NA))))

eb68.2 <- merge(eb68.2, nuts_eb_dict %>% select("Constituency", "Region.Name", "NUTS_eb_old"), by = "NUTS_eb_old", all.x = T)


eb68.2$year <- 2007
eb68.2$parliament <- 6

eb68.2$env.important <- ifelse(eb68.2$v474 %in% c(1,2), 1, 0)
eb68.2$envoverecon1 <- ifelse(eb68.2$v548 == 1, 1, 
                             ifelse(eb68.2$v548 == 2, 0, NA))

eb68.2$envoverecon2 <- ifelse(eb68.2$v595 == 1, 1,
                            ifelse(eb68.2$v595 == 2, 0, NA))

eb68.2$cooperation <- ifelse(eb68.2$v596 == 2, 1, 0)

eb68.2$eu.leg <- ifelse(eb68.2$v607 %in% c(1,2), 1, 0)
eb68.2$eu.asst <- ifelse(eb68.2$v608 %in% c(1,2), 1, 0)
eb68.2$eu.fund <- ifelse(eb68.2$v609 %in% c(1,2), 1, 0)

# eb68.2$stricterreg <- with(eb68.2, ifelse(v600== 1, 1, 0))
# eb68.2$enfreg <- with(eb68.2, ifelse(v598 == 1, 1, 0))
# eb68.2$highertax <- with(eb68.2, ifelse(v602 == 1, 1, 0))
# eb68.2$heavierfines <- with(eb68.2, ifelse(v599 == 1, 1, 0))
# eb68.2$financialincentives <- with(eb68.2, ifelse(v601 == 1, 1, 0))

eb68.2$isocntry <- eb68.2$v7

eb68.2$country <- ifelse(eb68.2$isocntry %in% c("GB-GBN", "GB-NIR"), "UK", eb68.2$isocntry)


eb68.2 <- eb68.2 %>% select(gender_male, age_cat2, country, ms, NUTS_eb_old, year, parliament, 
                            Constituency, Region.Name,
                            env.important,envoverecon1, envoverecon2, cooperation,
                            eu.leg, eu.asst, eu.fund)



## eb72.1
eb72.1<- read_dta("/Users/gabgilling/Downloads/ZA4975_v3-0-0.dta")
eb72.1 <- eb72.1 %>% filter(v7 %in% c("GB-GBN", "GB-NIR", "FR", "IT"))

eb72.1$NUTS_eb_old <- with(eb72.1, ifelse(v7 == "FR", paste("FR", v504, sep = ""), 
                                      ifelse(v7 %in% c("GB-GBN", "GB-NIR"), paste("UK", v510, sep = ""),
                                             ifelse(v7 == "IT", paste("IT", v533, sep = ""), NA))))

eb72.1 <- merge(eb72.1, nuts_eb_dict %>% select("Constituency", "Region.Name", "NUTS_eb_old"), by = "NUTS_eb_old", all.x = T)


eb72.1$ms <- with(eb72.1, ifelse(v437 %in% c(1,2,3,4), "MAR",
                                 ifelse(v437 %in% c(5,6,7,8), "REP",
                                        ifelse(v437 %in% c(9,10), "SIN",
                                               ifelse(v437 %in% c(11,12), "DIV",
                                                      ifelse(v437 %in% c(13,14), "WID",
                                                             NA))))))

eb72.1$gender_male <- ifelse(eb72.1$v441 == 1, 1, 0)

eb72.1$age_cat2 <- with(eb72.1, ifelse(v442>= 15 & v442 < 30, "Y15-29",
                                       ifelse(v442>= 30 & v442 < 50, "Y30-49",
                                              ifelse(v442 >= 50 & v442 < 65, "Y50-64",
                                                     ifelse(v442 >= 65 & v442 < 85, "Y65-84",
                                                          ifelse(v442 >= 85, "Y_GE85",
                                                              ifelse(v442 == 99, NA, NA)))))))

eb72.1$year <- 2009
eb72.1$parliament <- 7

eb72.1$cc.serious <- with(eb72.1, ifelse(v385  >= 6, 1, 0))

eb72.1$cc.enough <- with(eb72.1, ifelse(v387+v388+v389+v390 >= 9, 1, 0))

eb72.1$cc.stopable <- with(eb72.1, ifelse(v392 %in% c(1,2), 1, 0))
eb72.1$cc.notexaggerated <- with(eb72.1, ifelse(v393 %in% c(3,4), 1, 0))
eb72.1$cc.co2impact <- with(eb72.1, ifelse(v394 %in% c(3,4) | v395 %in% c(3,4), 1, 0))
eb72.1$cc.positiveecon <- with(eb72.1, ifelse(v396 %in% c(1,2), 1, 0))
eb72.1$alternativefuels <- with(eb72.1, ifelse(v397 %in% c(1,2) | v398 %in% c(1,2), 1, 0))

eb72.1$env.positiveecon <- with(eb72.1, ifelse(v413 %in% c(1,2), 1, 0))
eb72.1$env.nonegativeecon <- with(eb72.1, ifelse(v414 %in% c(3,4), 1, 0))

eb72.1$isocntry <- eb72.1$v7

eb72.1$country <- ifelse(eb72.1$isocntry %in% c("GB-GBN", "GB-NIR"), "UK", eb72.1$isocntry)


# merge 397 and 398 into 1
eb72.1 <- eb72.1 %>% select(gender_male, age_cat2, country, ms, NUTS_eb_old, year, parliament, Constituency, Region.Name,
                  cc.serious, cc.enough, cc.stopable, cc.notexaggerated, cc.co2impact,
                  cc.positiveecon, alternativefuels, env.positiveecon, env.nonegativeecon)



## eb71.1

eb71.1<- read_dta("/Users/gabgilling/Downloads/ZA4971_v4-0-0.dta")
eb71.1 <- eb71.1 %>% filter(v7 %in% c("GB-GBN", "FR", "IT"))

eb71.1$NUTS_eb_old <- with(eb71.1, ifelse(v7 == "FR", paste("FR", v711, sep = ""), 
                                      ifelse(v7 %in% c("GB-GBN"), paste("UK", v717, sep = ""),
                                             ifelse(v7 == "IT", paste("IT", v744, sep = ""), NA))))

eb71.1 <- merge(eb71.1, nuts_eb_dict %>% select("Constituency", "Region.Name", "NUTS_eb_old"), by = "NUTS_eb_old", all.x = T)


eb71.1$ms <- with(eb71.1, ifelse(v641 %in% c(1,2,3,4), "MAR",
                                 ifelse(v641 %in% c(5,6,7,8), "REP",
                                        ifelse(v641 %in% c(9,10), "SIN",
                                               ifelse(v641 %in% c(11,12), "DIV",
                                                      ifelse(v641 %in% c(13,14), "WID",
                                                             NA))))))

eb71.1$gender_male <- ifelse(eb71.1$v644 == 1, 1, 0)

eb71.1$age_cat2 <- with(eb71.1, ifelse(v645>= 15 & v645 < 30, "Y15-29",
                                       ifelse(v645 >= 30 & v645 < 50, "Y30-49",
                                              ifelse(v645 >= 50 & v645< 65, "Y50-64",
                                                     ifelse(v645 >= 65 & v645 < 85, "Y65-84",
                                                            ifelse(v645 >= 85, "Y_GE85",
                                                                   ifelse(v645 == 99, NA, NA)))))))

eb71.1$year <- 2009
eb71.1$parliament <- 6

eb71.1$cc.serious <- with(eb71.1, ifelse(v522  >= 6, 1, 0))

eb71.1$cc.stopable <- with(eb71.1, ifelse(v526 %in% c(1,2), 1, 0))
eb71.1$cc.notexaggerated <- with(eb71.1, ifelse(v527 %in% c(3,4), 1, 0))
eb71.1$cc.co2impact <- with(eb71.1, ifelse(v528 %in% c(3,4), 1, 0))
eb71.1$cc.positiveecon <- with(eb71.1, ifelse(v529 %in% c(1,2), 1, 0))
eb71.1$alternativefuels <- with(eb71.1, ifelse(v530 %in% c(1,2), 1, 0))

eb71.1$isocntry <- eb71.1$v7

eb71.1$country <- ifelse(eb71.1$isocntry %in% c("GB-GBN", "GB-NIR"), "UK", eb71.1$isocntry)


eb71.1 <- eb71.1 %>% select(gender_male, age_cat2, country, ms, NUTS_eb_old, year, parliament, Constituency, Region.Name,
                            cc.serious, cc.stopable, cc.notexaggerated, cc.co2impact,
                            cc.positiveecon, alternativefuels)

##eb69.2
eb69.2<- read_dta("/Users/gabgilling/Downloads/ZA4744_v5-0-0.dta")
eb69.2 <- eb69.2 %>% filter(v7 %in% c("GB-GBN", "GB-NIR", "FR", "IT"))

eb69.2$NUTS_eb_old <- with(eb69.2, ifelse(v7 == "FR", paste("FR", v834, sep = ""), 
                                      ifelse(v7 %in% c("GB-GBN", "GB-NIR"), paste("UK", v840, sep = ""),
                                             ifelse(v7 == "IT", paste("IT", v867, sep = ""), NA))))

eb69.2<- merge(eb69.2, nuts_eb_dict %>% select("Constituency", "Region.Name", "NUTS_eb_old"), by = "NUTS_eb_old", all.x = T)


eb69.2$ms <- with(eb69.2, ifelse(v764 %in% c(1,2,3,4), "MAR",
                                 ifelse(v764 %in% c(5,6,7,8), "REP",
                                        ifelse(v764 %in% c(9,10), "SIN",
                                               ifelse(v764 %in% c(11,12), "DIV",
                                                      ifelse(v764 %in% c(13,14), "WID",
                                                             NA))))))

eb69.2$gender_male <- ifelse(eb69.2$v767 == 1, 1, 0)

eb69.2$age_cat2 <- with(eb69.2, ifelse(v768>= 15 & v768 < 30, "Y15-29",
                                       ifelse(v768 >= 30 & v768 < 50, "Y30-49",
                                              ifelse(v768 >= 50 & v768< 65, "Y50-64",
                                                     ifelse(v768 >= 65 & v768 < 85, "Y65-84",
                                                            ifelse(v768 >= 85, "Y_GE85",
                                                                   ifelse(v768 == 99, NA, NA)))))))

eb69.2$year <- 2008
eb69.2$parliament <- 6

eb69.2$cc.serious <-with(eb69.2, ifelse(v705  >= 6, 1, 0))

eb69.2$cc.enough <- with(eb69.2, ifelse(v712 == 3 & v713 == 3 & v714 == 3, 1, 0))

eb69.2$cc.stopable <- with(eb69.2, ifelse(v720 %in% c(1,2), 1, 0))
eb69.2$cc.notexaggerated <- with(eb69.2, ifelse(v721 %in% c(3,4), 1, 0))
eb69.2$cc.co2impact <- with(eb69.2, ifelse(v722 %in% c(3,4), 1, 0))
eb69.2$cc.positiveecon <- with(eb69.2, ifelse(v723 %in% c(1,2), 1, 0))
eb69.2$alternativefuels <- with(eb69.2, ifelse(v724 %in% c(1,2), 1, 0))

eb69.2$ghgtarget <- with(eb69.2, ifelse(v758 %in% c(3,2) & v759 %in% c(3,2), 1, 0))

eb69.2$renewtargets <- with(eb69.2, ifelse(v760 %in% c(2,3), 1, 0))

eb69.2$isocntry <- eb69.2$v7

eb69.2$country <- ifelse(eb69.2$isocntry %in% c("GB-GBN", "GB-NIR"), "UK", eb69.2$isocntry)

eb69.2 <- eb69.2 %>% select(gender_male, age_cat2, country, ms, NUTS_eb_old, year, parliament, Constituency, Region.Name,
                            cc.serious, cc.enough, cc.stopable, cc.notexaggerated , 
                            cc.co2impact, cc.positiveecon, alternativefuels, ghgtarget,
                            renewtargets)



##eb90.2
eb90.2 <- read_dta("/Users/gabgilling/Downloads/ZA7488_v1-0-0.dta")

eb90.2 <- eb90.2 %>% filter(isocntry %in% c("GB", "FR", "IT"))

eb90.2$NUTS_eb_new <- with(eb90.2, ifelse(isocntry == "FR", paste("FR", p7fr, sep = ""), 
                                      ifelse(isocntry %in% c("GB-GBN", "GB-NIR"), paste("UK", p7gb, sep = ""),
                                             ifelse(isocntry == "IT", paste("IT", p7it_r1, sep = ""), NA))))

eb90.2<- merge(eb90.2, nuts_eb_dict %>% select("Constituency", "Region.Name", "NUTS_eb_new"), by = "NUTS_eb_new", all.x = T)


eb90.2$gender_male <- ifelse(eb90.2$d10 == 1, 1, 0)

eb90.2$age_cat2 <- with(eb90.2, ifelse(d11 >= 15 & d11 < 30, "Y15-29",
                                       ifelse(d11 >= 30 & d11 < 50, "Y30-49",
                                              ifelse(d11 >= 50 & d11 < 65, "Y50-64",
                                              ifelse(d11 >= 65 & d11 < 85, "Y65-84",
                                                     ifelse(d11 >= 85, "Y_GE85",
                                                            NA))))))

eb90.2$ms <- with(eb90.2, ifelse(d7 %in% c(1,2,3,4), "MAR",
                                 ifelse(d7 %in% c(5,6,7,8), "REP",
                                        ifelse(d7 %in% c(9,10), "SIN",
                                               ifelse(d7 %in% c(11,12), "DIV",
                                                      ifelse(d7 %in% c(13,14), "WID",
                                                             NA))))))
eb90.2$year <- 2018
eb90.2$parliament <- 8

eb90.2$cc.humancaused <- with(eb90.2, ifelse(qb1 %in% c(1,2), 1, 
                                      ifelse(qb1 %in% c(3,4), 2, NA))) 

eb90.2$morerecycling <- with(eb90.2, ifelse(qb4_1 %in% c(1,2), 1, 
                                             ifelse(qb4_1 %in% c(3,4), 2, NA))) 

eb90.2$supenergyeff <- with(eb90.2, ifelse(qb4_2 %in% c(1,2), 1, 
                                             ifelse(qb4_2 %in% c(3,4), 2, NA))) 

eb90.2$supcleaneconomy <- with(eb90.2, ifelse(qb4_3 %in% c(1,2), 1, 
                                           ifelse(qb4_3 %in% c(3,4), 2, NA))) 

eb90.2$cc.boosteconjobs <- with(eb90.2, ifelse(qb5_1 %in% c(1,2), 1, 
                                             ifelse(qb5_1 %in% c(3,4), 2, NA))) 

eb90.2$supcleanenergy <- with(eb90.2, ifelse(qb5_5 %in% c(1,2), 1, 
                                            ifelse(qb5_5 %in% c(3,4), 2, NA)))

eb90.2$country <- ifelse(eb90.2$isocntry == "GB", "UK", eb90.2$isocntry)


eb90.2 <- eb90.2 %>% select(gender_male, age_cat2, ms, NUTS_eb_new, year, parliament, country, Constituency, Region.Name,
                            cc.humancaused, morerecycling, supenergyeff, supcleaneconomy,cc.boosteconjobs, 
                            supcleanenergy)
                  
##eb62.1
eb62.1<- read_dta("/Users/gabgilling/Downloads/ZA4230_v1-1-0.dta")
eb62.1 <- eb62.1 %>% filter(v7 %in% c("GB-GBN", "GB-NIR", "FR", "IT"))

eb62.1$NUTS_eb_old <- with(eb62.1, ifelse(v7 == "FR", paste("FR", v627, sep = ""), 
                                      ifelse(v7 %in% c("GB-GBN", "GB-NIR"), paste("UK", v633, sep = ""),
                                             ifelse(v7 == "IT", paste("IT", v654, sep = ""), NA))))

eb62.1<- merge(eb62.1, nuts_eb_dict %>% select("Constituency", "Region.Name", "NUTS_eb_old"), by = "NUTS_eb_old", all.x = T)

eb62.1$ms <- with(eb62.1, ifelse(v582 %in% c(1,2,3,4), "MAR",
                                 ifelse(v582 %in% c(5,6,7,8), "REP",
                                        ifelse(v582 %in% c(9,10), "SIN",
                                               ifelse(v582 %in% c(11,12), "DIV",
                                                      ifelse(v582 %in% c(13,14), "WID",
                                                             NA))))))

eb62.1$gender_male <- ifelse(eb62.1$v585 == 1, 1, 0)

eb62.1$age_cat2 <- with(eb62.1, ifelse(v58 >= 15 & v58 < 30, "Y15-29",
                                       ifelse(v58 >= 30 & v58 < 50, "Y30-49",
                                              ifelse(v58 >= 50 & v58< 65, "Y50-64",
                                                     ifelse(v58 >= 65 & v58 < 85, "Y65-84",
                                                            ifelse(v58 >= 85, "Y_GE85",
                                                                   ifelse(v58 == 99, NA, NA)))))))

eb62.1$year <- 2018
eb62.1$parliament <- 8

eb62.1$envimportant1 <- with(eb62.1, ifelse(v310 == 1, 1, 0))

eb62.1$envimportant2 <- with(eb62.1, ifelse(v311 %in% c(1,2), 1, 
                                              ifelse(v311 %in% c(3,4), 0, NA)))

# eb62.1$stricterreg <- with(eb62.1, ifelse(v352 == 1, 1, 0))
# eb62.1$enfreg <- with(eb62.1, ifelse(v353 == 1, 1, 0))
# eb62.1$highertax <- with(eb62.1, ifelse(v354 == 1, 1, 0))
# eb62.1$supNGOs <- with(eb62.1, ifelse(v356 == 1, 1, 0))
# eb62.1$financialincentives <- with(eb62.1, ifelse(v357 == 1, 1, 0))

eb62.1$isocntry <- eb62.1$v7

eb62.1$country <- ifelse(eb62.1$isocntry %in% c("GB-NIR", "GB-GBN"), "UK", eb62.1$isocntry)


eb62.1 <- eb62.1 %>% select(gender_male, age_cat2, country, ms, NUTS_eb_old, year, parliament,
                            Constituency, Region.Name,envimportant1, envimportant2)
                            # envimportant1, envimportant2, stricterreg, enfreg, highertax,
                            # supNGOs, financialincentives)



##eb87.1
eb87.1 <- read_dta("/Users/gabgilling/Downloads/ZA6861_v1-2-0.dta")

eb87.1 <- eb87.1 %>% filter(isocntry %in% c("GB-GBN", "FR", "IT"))

eb87.1$NUTS_eb_new <- with(eb87.1, ifelse(isocntry == "FR", paste("FR", p7fr, sep = ""), 
                                      ifelse(isocntry %in% c("GB-GBN", "GB-NIR"), paste("UK", p7gb, sep = ""),
                                             ifelse(isocntry == "IT", paste("IT", p7it_r1, sep = ""), NA))))


eb87.1<- merge(eb87.1, nuts_eb_dict %>% select("Constituency", "Region.Name", "NUTS_eb_new"), by = "NUTS_eb_new", all.x = T)



eb87.1$gender_male <- ifelse(eb87.1$d10 == 1, 1, 0)

eb87.1$age_cat2 <- with(eb87.1, ifelse(d11 >= 15 & d11 < 30, "Y15-29",
                                       ifelse(d11 >= 30 & d11 < 50, "Y30-49",
                                              ifelse(d11 >= 50 & d11 < 65, "Y50-64",
                                              ifelse(d11 >= 65 & d11 < 85, "Y65-84",
                                                     ifelse(d11 >= 85, "Y_GE85",
                                                            NA))))))

eb87.1$ms <- with(eb87.1, ifelse(d7 %in% c(1,2,3,4), "MAR",
                                 ifelse(d7 %in% c(5,6,7,8), "REP",
                                        ifelse(d7 %in% c(9,10), "SIN",
                                               ifelse(d7 %in% c(11,12), "DIV",
                                                      ifelse(d7 %in% c(13,14), "WID",
                                                             NA))))))

eb87.1$year <- 2017
eb87.1$parliament <- 8

eb87.1$cc.serious <-with(eb87.1, ifelse(qc2 >= 6, 1, 0))
eb87.1$cc.boosteconjobs <- with(eb87.1, ifelse(qc4_1 %in% c(1,2), 1, 
                                        ifelse(qc4_1 %in% c(3,4), 0, NA)))

eb87.1$supcleanenergy <- with(eb87.1, ifelse(qc4_5 %in% c(1,2), 1, 
                                             ifelse(qc4_5 %in% c(3,4), 0, NA)))

eb87.1$renewtargets <- with(eb87.1, ifelse(qc7 %in% c(1,2), 1, 
                                           ifelse(qc7 %in% c(3,4), 0, NA)))

eb87.1$supenergyeff <- with(eb87.1, ifelse(qc8 %in% c(1,2), 1, 
                                           ifelse(qc8 %in% c(3,4), 0, NA)))

eb87.1$country <- ifelse(eb87.1$isocntry %in% c("GB-NIR", "GB-GBN"), "UK", eb87.1$isocntry)


eb87.1 <- eb87.1 %>% select(gender_male, age_cat2, ms, NUTS_eb_new, year, parliament, country, Constituency, Region.Name,
                           cc.serious, cc.boosteconjobs, supcleanenergy, renewtargets, supenergyeff)

##eb80.2
eb80.2 <- read_dta("/Users/gabgilling/Downloads/ZA5877_v2-0-0.dta")

eb80.2 <- eb80.2 %>% filter(isocntry %in% c("GB-GBN", "GB-NIR", "FR", "IT"))

eb80.2$NUTS_eb_new <- with(eb80.2, ifelse(isocntry == "FR", paste("FR", p7fr, sep = ""), 
                                      ifelse(isocntry %in% c("GB-GBN", "GB-NIR"), paste("UK", p7gb, sep = ""),
                                             ifelse(isocntry == "IT", paste("IT", p7it_r1, sep = ""), NA))))

eb80.2<- merge(eb80.2, nuts_eb_dict %>% select("Constituency", "Region.Name", "NUTS_eb_new"), by = "NUTS_eb_new", all.x = T)


eb80.2$gender_male <- ifelse(eb80.2$d10 == 1, 1, 0)

eb80.2$age_cat2 <- with(eb80.2, ifelse(d11 >= 15 & d11 < 30, "Y15-29",
                                       ifelse(d11 >= 30 & d11 < 50, "Y30-49",
                                              ifelse(d11 >= 50 & d11 < 65, "Y50-64",
                                              ifelse(d11 >= 65 & d11 < 85, "Y65-84",
                                                     ifelse(d11 >= 85, "Y_GE85",
                                                            NA))))))

eb80.2$ms <- with(eb80.2, ifelse(d7 %in% c(1,2,3,4), "MAR",
                                 ifelse(d7 %in% c(5,6,7,8), "REP",
                                        ifelse(d7 %in% c(9,10), "SIN",
                                               ifelse(d7 %in% c(11,12), "DIV",
                                                      ifelse(d7 %in% c(13,14), "WID",
                                                             NA))))))

eb80.2$year <- 2013
eb80.2$parliament <- 7

eb80.2$cc.serious <- with(eb80.2, ifelse(qa2 >= 6, 1, 0))
eb80.2$cc.boosteconjobs <- with(eb80.2, ifelse(qa4_1 %in% c(1,2), 1, 
                                    ifelse(qa4_1 %in% c(3,4), 0, NA)))

eb80.2$renewtargets <- with(eb80.2, ifelse(qa7 %in% c(1,2), 1, 
                                           ifelse(qa7 %in% c(3,4), 0, NA)))
eb80.2$supenergyeff <- with(eb80.2, ifelse(qa8 %in% c(1,2), 1, 
                                           ifelse(qa8 %in% c(3,4), 0, NA)))

eb80.2$country <- ifelse(eb80.2$isocntry %in% c("GB-NIR", "GB-GBN"), "UK", eb80.2$isocntry)

  
eb80.2 <- eb80.2 %>% select(gender_male, age_cat2, NUTS_eb_new, ms, year, parliament, country, Constituency, Region.Name,
                            cc.serious, cc.boosteconjobs, renewtargets, supenergyeff)
