# Thesis documentation for _Mr. P Visits Brussels: An Investigation of the Democratic Deficit in the European Union_

The goal of this thesis is to investigate the democratic deficit in the European Union by using Multi-level Regression and Poststratification (MRP) to quantify regional level preferences on a set of salient issues surrounding environmental regulations. I have chosen that topic because it has both large amounts of 1) polling conducted on the matter 2) legislation voted on in the E.U. Parliament. I will potentially look into more topics (LGTBQ+ rights, immigration, trade) if time allows.

Once regional preferences are calculated, they are correlated with the regions' - called constituencies -  Members of Parliament (MEPs) roll call votes on legislation corresponding to the aforementioned issues. 

There are only 3 countries that have a sub-national constituencies at the E.U. Parliament: the United Kingdom (until 2020), France (until 2019) and Italy. I will analyze surveys and roll call votes for the 6th, 7th and 8th legislatures - from 2004 to 2019. 

This GitHub repo documents all of the steps I took to extract and wrangle the data I needed, as well as preliminary results as I progress throughout the semester. 

## Step 1: MRP

MRP is divided into 2 steps. 

First, a set of multilevel models are fit on survey responses in order to estimate public opinion on a particular issue. For EU data, individual level predictors (which must have corresponding categories in census data) are _age_, _education_ and _gender_. I will also try running analyses using _marital status_ instead of education if time allows. Regional level predictors include GDP per capita, population density, tertiary education shares and median household income.

Second, post stratification is conducted using census data, using true demographic weights in the population to adjust the estimates (i.e. the true proportion of 18-24 year old males with college degrees or 50-55 year old females with high school degrees etc. in the Ile de France region of France, the Central region in Italy and so forth).

See the literature reviews and project proposals in the repo for more detailed explanations.


### 1. Getting survey data and fitting the multi-level models.

All survey data comes from Eurobarometer.
A database containing all of the raw survey data and associated codebooks can be found on the [GESIS website](https://www.gesis.org/en/eurobarometer-data-service/search-data-access/topics).

I filter the raw data and recode the dataframe (see Analysis.rmd).

I then fit a multi-level model on a set of individual questions. For instance, Eurobarometer 81.3 asks respondents whether the environment should be protected by regulations.

First, I tally responses by demographic variables and regions.

`protect.env <- eb81.3_filt %>% group_by(age_cat, gender_male, edu, NUTS_eb) %>%
  summarise(pro = sum(protect.env == 1), against = n() - pro) %>% na.omit()`

After some extra wrangling I can finally run the model:

`fit.protect.env <- stan_glmer(cbind(pro, against) ~ (1|country) + (1|NUTS_eb) + (1|gender_male)  + (1 | age_cat)  + (1|edu) + gdp_cap + higher_ed + hhd_income + unemp_pct, data = protect.env, family = binomial(link = "logit"))`

In this thesis I will conducting my analyses using a Bayesian framework (see project proposal). stan_glmer performs the same way as lme4's glm function call, with random effects for variable _x_ specified with the (1| _x_) notation.

### 2. Poststratification

The census data is sourced directly from [Eurostat](https://ec.europa.eu/eurostat/web/population-and-housing-census/census-data/2011-census).

Code to wrangle and set up the census datafile can be found in the [prepare_census.R] (https://github.com/gabgilling/Thesis/blob/master/prepare_census.R) file.

The following function uses rstanarm's posterior_epred() function to estimate opinions: we are simulating 1000 draws from the posterior distribution of our estimated coefficients and then weighting them with the true distribution of each demographic combination in the regional populations.


`generate_region_estimates <- function(poststrat, fitted_model){
  
  ## generate state_df
  N <- length(unique(poststrat$Region.Name))
  
  region_preferences <- data.frame(
                          Region.Name = unique(poststrat$Region.Name),
                          constituency = rep("", N),
                          country = rep("", N),
                          pref = rep(-1, N),
                          se = rep(-1, N)
)

  for (i in unique(poststrat$Region.Name)) {
    print(i)
    poststrat_region <- poststrat[poststrat$Region.Name == i, ]
    
    posterior_prob_region <- posterior_epred(
      fitted_model,
      # transform = TRUE,
      draws = 1000,
      newdata = as.data.frame(poststrat_region)
    )
    
    poststrat_prob_region <- posterior_prob_region %*% poststrat_region$freq / sum(poststrat_region$freq)

    #This is the estimate for popn in state:
    region_preferences[region_preferences$Region.Name == i,]$pref <- round(mean(poststrat_prob_region), 4)
    region_preferences[region_preferences$Region.Name == i,]$se <- round(sd(poststrat_prob_region), 4)
    region_preferences[region_preferences$Region.Name == i,]$country <- unique(poststrat_region$country)
    region_preferences[region_preferences$Region.Name == i,]$constituency <- unique(poststrat_region$Constituency)
  }
  return(region_preferences)
}
` 

The results are plotted below:
![env_prefs](/Plots/env_prefs_plot.jpg)
 




 