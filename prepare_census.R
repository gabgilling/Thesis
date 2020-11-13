library(data.table)

# nuts_dictionary <- read.csv("https://raw.githubusercontent.com/gabgilling/Thesis/master/Data/nuts_dictionary.csv")
nuts_fr <- read.csv("https://raw.githubusercontent.com/gabgilling/Thesis/master/Data/NUTS_FR.csv")

census2011_education <- read.csv("https://raw.githubusercontent.com/gabgilling/Thesis/master/Data/census_education.csv")
census2011_ms <- read.csv("https://raw.githubusercontent.com/gabgilling/Thesis/master/Data/census_marital_status.csv")

### Nomenclature
# DISREP   Persons whose registered partnership was legally dissolved
# -> recode as DIV
# DTHREP   Persons whose registered partnership ended with the death of the partner
# -> recode as WID
# REP   Persons in registered partnership
# SIN   Single persons (never in legal unions)

census2011_ms$ms <- with(census2011_ms, ifelse(LMS == "DISREP", "DIV",
                                        ifelse(LMS == "DTHREP", "WID",
                                        ifelse(LMS == "UNK", NA,
                                               LMS))))

census2011_education$NUTS2_old <- census2011_education$GEO
census2011_ms$NUTS2_old <- census2011_ms$GEO

reg_preds <- read.csv("https://raw.githubusercontent.com/gabgilling/Thesis/master/Data/regional_predictors.csv")

reg_preds_std <- reg_preds %>% select(gdp_cap,density, area, higher_ed, hhd_income, unemp_pct) %>% robustHD::standardize()

reg_preds_std <- cbind(reg_preds_std, reg_preds %>% select(NUTS, NUTS2_old, NUTS_eb, Region.Name))

census2011_education <- merge(census2011_education, reg_preds_std, by = "NUTS2_old")
census2011_ms <- merge(census2011_ms, reg_preds_std, by = "NUTS2_old")

census2011_education <- merge(census2011_education, nuts_fr %>% select(Constituency, Region.Name), by = "Region.Name", all.x = TRUE)
census2011_ms <- merge(census2011_ms, nuts_fr %>% select(Constituency, Region.Name), by = "Region.Name", all.x = TRUE)

census2011_education$Constituency <- ifelse(is.na(census2011_education$Constituency), census2011_education$Region.Name, 
                                            census2011_education$Constituency)
census2011_ms$Constituency <- ifelse(is.na(census2011_ms$Constituency), census2011_ms$Region.Name, 
                                            census2011_ms$Constituency)
# census2011_education <- merge(census2011_education, nuts_fr, by = "NUTS2", all.x = TRUE)

census2011_education$CAS <- NULL
census2011_education$TIME <- NULL
census2011_education$SIE <- NULL
census2011_education$LOC <- NULL
census2011_education$FLAGS <- NULL
census2011_education$FOOTNOTES <- NULL

census2011_ms$CAS <- NULL
census2011_ms$TIME <- NULL
census2011_ms$SIE <- NULL
census2011_ms$LOC <- NULL
census2011_ms$FLAGS <- NULL
census2011_ms$FOOTNOTES <- NULL


census2011_education$age_cat <- ifelse(census2011_education$AGE %in% c('Y18', 'Y19', 'Y20-24'), 'Y18-24', 
                                       census2011_education$AGE)

census2011_ms$age_cat <- census2011_ms$AGE

census2011_education$gender_male <- ifelse(census2011_education$SEX == 'M', 1,0 )
census2011_ms$gender_male <- ifelse(census2011_ms$SEX == 'M', 1,0 )

census2011_education$edu <- with(census2011_education, ifelse(EDU == 'ED1', 1, ifelse(EDU == 'ED2', 2, ifelse(EDU == 'ED3', 3,
                                                      ifelse(EDU == 'ED4', 4, ifelse(EDU == 'ED5', 5, ifelse(EDU == 'ED6', 6, NA)))))))

census2011_education$freq <- census2011_education$VALUE
census2011_ms$freq <- census2011_ms$VALUE

census2011_education$country <- substr(census2011_education$NUTS, 1, 2)
census2011_ms$country <- substr(census2011_ms$NUTS, 1, 2)

census2011_education$GEO <- NULL
census2011_education$SEX <- NULL
census2011_education$AGE <- NULL
census2011_education$EDU <- NULL
census2011_education$VALUE <- NULL

census2011_ms$GEO <- NULL
census2011_ms$SEX <- NULL
census2011_ms$AGE <- NULL
census2011_ms$EDU <- NULL
census2011_ms$VALUE <- NULL
census2011_ms$LMS <- NULL
census2011_ms$COC <- NULL

## rename columns

census2011_education$Constituency <- with(census2011_education, 
                                          ifelse(Constituency == "Nord-Ovest", "North-West Italy", Constituency))
census2011_education$Constituency <- with(census2011_education, 
                                          ifelse(Constituency == "Centro (IT)", "Central Italy", Constituency))
census2011_education$Constituency <- with(census2011_education, 
                                          ifelse(Constituency == "Sud", "South Italy", Constituency))

census2011_education$Constituency <- with(census2011_education, 
                                          ifelse(Constituency == "Isole", "Islands Italy", Constituency))

census2011_education$Constituency <- with(census2011_education, 
                                          ifelse(Constituency == "Nord-Est", "North-East Italy", Constituency))


census2011_education$Constituency <- with(census2011_education, 
                                          ifelse(Constituency == "Yorkshire and The Humber", "Yorkshire and the Humber", 
                                                 Constituency))
census2011_education$Constituency <- with(census2011_education, 
                                          ifelse(Constituency == "East of England",
                                                 "East England", Constituency))
census2011_education$Constituency <- with(census2011_education, 
                                          ifelse(Constituency == "South West (UK)", 
                                                 "South-West England", Constituency))

census2011_education$Constituency <- with(census2011_education, 
                                          ifelse(Constituency == "North East (UK)", 
                                                 "North East England", Constituency))

census2011_education$Constituency <- with(census2011_education, 
                                          ifelse(Constituency == "Northern Ireland (UK)", 
                                                 "Northern Ireland", Constituency))

census2011_education$Constituency <- with(census2011_education, 
                                          ifelse(Constituency == "North West (UK)" , "North West England", 
                                                 Constituency))
census2011_education$Constituency <- with(census2011_education, 
                                          ifelse(Constituency == "East Midlands (UK)" , "East Midlands", Constituency))
census2011_education$Constituency <- with(census2011_education, 
                                          ifelse(Constituency == "South East (UK)" , "South-East England", 
                                                 Constituency))
census2011_education$Constituency <- with(census2011_education, 
                                          ifelse(Constituency == "West Midlands (UK)"  , "West Midlands", Constituency))

census2011_ms$Constituency <- with(census2011_ms, 
                                          ifelse(Constituency == "Nord-Ovest", "North-West Italy", Constituency))
census2011_ms$Constituency <- with(census2011_ms, 
                                          ifelse(Constituency == "Centro (IT)", "Central Italy", Constituency))
census2011_ms$Constituency <- with(census2011_ms, 
                                          ifelse(Constituency == "Sud", "South Italy", Constituency))

census2011_ms$Constituency <- with(census2011_ms, 
                                          ifelse(Constituency == "Isole", "Islands Italy", Constituency))

census2011_ms$Constituency <- with(census2011_ms, 
                                          ifelse(Constituency == "Nord-Est", "North-East Italy", Constituency))


census2011_ms$Constituency <- with(census2011_ms, 
                                          ifelse(Constituency == "Yorkshire and The Humber", "Yorkshire and the Humber", 
                                                 Constituency))
census2011_ms$Constituency <- with(census2011_ms, 
                                          ifelse(Constituency == "East of England",
                                                 "East England", Constituency))
census2011_ms$Constituency <- with(census2011_ms, 
                                          ifelse(Constituency == "South West (UK)", 
                                                 "South-West England", Constituency))

census2011_ms$Constituency <- with(census2011_ms, 
                                          ifelse(Constituency == "North East (UK)", 
                                                 "North East England", Constituency))

census2011_ms$Constituency <- with(census2011_ms, 
                                          ifelse(Constituency == "Northern Ireland (UK)", 
                                                 "Northern Ireland", Constituency))

census2011_ms$Constituency <- with(census2011_ms, 
                                          ifelse(Constituency == "North West (UK)" , "North West England", 
                                                 Constituency))
census2011_ms$Constituency <- with(census2011_ms, 
                                          ifelse(Constituency == "East Midlands (UK)" , "East Midlands", Constituency))
census2011_ms$Constituency <- with(census2011_ms, 
                                          ifelse(Constituency == "South East (UK)" , "South-East England", 
                                                 Constituency))
census2011_ms$Constituency <- with(census2011_ms, 
                                          ifelse(Constituency == "West Midlands (UK)"  , "West Midlands", Constituency))

# write.csv(census2011_education, "/Users/gabgilling/Documents/GitHub/Thesis/Data/census_edu_predictors.csv")
# write.csv(census2011_ms, "/Users/gabgilling/Documents/GitHub/Thesis/Data/census_ms_predictors.csv")
