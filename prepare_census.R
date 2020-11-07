library(data.table)

# nuts_dictionary <- read.csv("https://raw.githubusercontent.com/gabgilling/Thesis/master/Data/nuts_dictionary.csv")
nuts_fr <- read.csv("https://raw.githubusercontent.com/gabgilling/Thesis/master/Data/NUTS_FR.csv")

census2011_education <- read.csv("https://raw.githubusercontent.com/gabgilling/Thesis/master/Data/census_education.csv")

census2011_education$NUTS2_old <- census2011_education$GEO

reg_preds <- read.csv("https://raw.githubusercontent.com/gabgilling/Thesis/master/Data/regional_predictors.csv")

reg_preds_std <- reg_preds %>% select(gdp_cap,density, area, higher_ed, hhd_income, unemp_pct) %>% robustHD::standardize()

reg_preds_std <- cbind(reg_preds_std, reg_preds %>% select(NUTS, NUTS2_old, NUTS_eb, Region.Name))

census2011_education <- merge(census2011_education, reg_preds_std, by = "NUTS2_old")

census2011_education <- merge(census2011_education, nuts_fr %>% select(Constituency, Region.Name), by = "Region.Name", all.x = TRUE)

census2011_education$Constituency <- ifelse(is.na(census2011_education$Constituency), census2011_education$Region.Name, census2011_education$Constituency)
# census2011_education <- merge(census2011_education, nuts_fr, by = "NUTS2", all.x = TRUE)

census2011_education$CAS <- NULL
census2011_education$TIME <- NULL
census2011_education$SIE <- NULL
census2011_education$LOC <- NULL
census2011_education$FLAGS <- NULL
census2011_education$FOOTNOTES <- NULL


census2011_education$age_cat <- ifelse(census2011_education$AGE %in% c('Y18', 'Y19', 'Y20-24'), 'Y18-24', 
                                       census2011_education$AGE)

census2011_education$gender_male <- ifelse(census2011_education$SEX == 'M', 1,0 )
census2011_education$edu <- with(census2011_education, ifelse(EDU == 'ED1', 1, ifelse(EDU == 'ED2', 2, ifelse(EDU == 'ED3', 3,
                                                                                                              ifelse(EDU == 'ED4', 4, ifelse(EDU == 'ED5', 5, ifelse(EDU == 'ED6', 6, NA)))))))

census2011_education$freq <- census2011_education$VALUE

census2011_education$country <- substr(census2011_education$NUTS, 1, 2)

census2011_education$GEO <- NULL
census2011_education$SEX <- NULL
census2011_education$AGE <- NULL
census2011_education$EDU <- NULL
census2011_education$VALUE <- NULL