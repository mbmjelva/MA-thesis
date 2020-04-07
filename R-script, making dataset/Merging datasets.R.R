
## Constructing dataset - R ##

library(tidyverse)
library(countrycode)
library(questionr)
library(tidymodels)
library(stargazer)

# Loading datasets --------------------------------------------------------

geoepr <- read_rds("./Egne datasett/geoepr.rds")
wb <- read_rds("./Egne datasett/wbi.rds")
shdi <- read_rds("./Egne datasett/shdi.rds")
kof <- read_rds("./Egne datasett/kof.rds")
vdem <- read_rds("./Egne datasett/vdem.rds")
pop <- read_rds("./Egne datasett/population.rds")
cru <- read_rds("./Egne datasett/cru.rds")
prio_ucdp <- read_rds("./Egne datasett/prio_ucdp_merged.rds")

# Fix cru-variables ---------------------------------

# Remove infinite numbers
cru$spei3 <- if_else(is.infinite(cru$spei3), 0, cru$spei3)

# Remove all gridcells that cover ocean
lons <- unique(prio_ucdp$xcoord)
lats <- unique(prio_ucdp$ycoord)

cru <- filter(cru, (lon %in% lons & lat %in% lats))

# Divide SPEI3 into two variables based on positive and negative values

cru <- cru %>% mutate(spei3_pos = ifelse(spei3 > 0, spei3, 0),
                      spei3_neg = ifelse(spei3 < 0, spei3, 0))

# Check if there is a time trend in SPEI
mod <- lm(spei3_neg ~ year, cru)
mod2 <- lm(spei3_pos ~ year, cru)

stargazer(mod, mod2, 
          covariate.labels = c("Year", "Constant"),
          dep.var.labels = c("SPEI3 negative", "SPEI3 positive"))

# Year has a significant effect on the spei values. Subtract the effect from the spei values to detrend SPEI.
cru$spei3_neg <- cru$spei3_neg - mod$coefficients["year"]
cru$spei3_pos <- cru$spei3_pos - mod2$coefficients["year"]


# Changing country to gwno for all variables to be matched by country ------------------------------

# Fra priogrid er det kun de landene som er independent på det gitte tidspunktet som tas med. De som ikke er independent skrives om til missing. 
# Altså er det kun de som blir missing under og som er småstater som må endres på manuelt, hvis de korresponderer med priogrid.


## vdem
#' Kommentar: Palestina og Hong Kong må settes til NA fordi de ikke er selvstendige stater
#' Mikrostatene er inkludert i priogrid, men får ikek tl å konvertere dem (muligens ikke inkludert i countrycodes). Endrer manuelt etterpå. Yemen måtte bare bytte navn.
#' 

vdem <- dplyr::mutate(vdem, gwno = countrycode(country, "country.name", "gwn"))

# Manually change values. Somaliland has to be set to NA as not independent (if left as it is, it is given the same code as Somalia, thus wrongly dobling the amount of observations for that country code)
vdem$gwno <- ifelse(vdem$country == "Seychelles", 591,
                             ifelse(vdem$country == "Vanuatu", 935,
                                    ifelse(vdem$country == "Sao Tome and Principe", 403,
                                           ifelse(vdem$country == "Yemen", 678,
                                                  ifelse(vdem$country == "Somaliland", NA, vdem$gwno)))))


##' WB
#' Her er det mange verdier som mangler, men de er for det meste små stater som ikke er med i countrycodes datasettet. Hva skal man gjøre med dem? Kode manuelt? Se på priogrid og hvordan det er kodet der

wb <- mutate(wb, gwno = countrycode(country, "country.name", "gwn"))

# Må endre manuelt på mikrostater (og Yemen) som er selvstendige. Ikke-selvstendige stater blir satt til missing på gwno.
wb$gwno <- ifelse(wb$country == "Dominica", 54,
                  ifelse(wb$country == "Grenada", 55,
                         ifelse(wb$country == "Kiribati", 970,
                                ifelse(wb$country == "Liechtenstein", 223,
                                       ifelse(wb$country == "Marshall Islands", 983,
                                              ifelse(wb$country == "Micronesia, Fed. Sts.", 987,
                                                     ifelse(wb$country == "Monaco", 221,
                                                            ifelse(wb$country == "Nauru", 971,
                                                                   ifelse(wb$country == "Palau", 986,
                                                                          ifelse(wb$country == "Samoa", 990,
                                                                                 ifelse(wb$country == "San Marino", 331,
                                                                                        ifelse(wb$country == "Sao Tome and Principe", 403,
                                                                                               ifelse(wb$country == "Seychelles", 591,
                                                                                                      ifelse(wb$country == "Tonga", 972, 
                                                                                                             ifelse(wb$country == "Tuvalu", 973,
                                                                                                                    ifelse(wb$country == "Vanuatu", 935,
                                                                                                                           ifelse(wb$country == "Yemen", 678, wb$gwno)))))))))))))))))

# Kof
# Samme problem her som for wb, pluss at landmed upper middle income etc. er inkludert. Er vel ikke så farlig, de blir uansett omgjort til na ved merging (?)

kof <- kof %>% mutate(gwno = countrycode(country, "country.name", "gwn"))

# Manually give values to some of the gwno. North Korea was given the wrong gwno value in the countrycode transformation
kof$gwno <- ifelse(kof$country == "Andorra", 231, 
                   ifelse(kof$country == "Dominica", 54,
                          ifelse(kof$country == "Kiribati", 970,
                                 ifelse(kof$country == "Liechtenstein", 223,
                                        ifelse(kof$country == "Marshall Islands", 983,
                                               ifelse(kof$country == "Micronesia, Fed. Sts.", 987,
                                                      ifelse(kof$country == "Monaco", 221,
                                                             ifelse(kof$country == "Palau", 986,
                                                                    ifelse(kof$country == "Samao", 990,
                                                                           ifelse(kof$country == "San Marino", 331,
                                                                                  ifelse(kof$country == "Sao Tome and Principe", 403,
                                                                                         ifelse(kof$country == "Seychelles", 591,
                                                                                                ifelse(kof$country == "Tonga", 972, 
                                                                                                       ifelse(kof$country == "Vanuatu", 935,
                                                                                                              ifelse(kof$country == "Yemen, Rep.", 678,
                                                                                                                     ifelse(kof$country == "Korea, Dem. Rep.", 731, kof$gwno))))))))))))))))


# Merging datasets --------------------------------------------------------

# Year must be numeric in all datasets as it is part of the joining key
wb$year <- as.numeric(wb$year)
prio_ucdp$year <- as.numeric(prio_ucdp$year)


# Rename the variable names so that they are not all called "value" (pop, shdi, excluded)
pop <- questionr::rename.variable(pop, "value", "pop")
shdi <- questionr::rename.variable(shdi, "value", "shdi")
geoepr <- questionr::rename.variable(geoepr, "value", "excluded")

# Remove the 15 observations that have missing on the gwno-variable as they create duplicates when joining with the other datasets (created 10 000 extra observations)
prio_ucdp <- prio_ucdp %>% filter(!is.na(gwno))

# Join the datasets
new_data <- prio_ucdp %>% 
  left_join(shdi, by = c("xcoord" = "x", "ycoord" = "y", "year" = "mydate")) %>%
  left_join(cru, by = c("xcoord" = "lon", "ycoord" = "lat", "year" = "year")) %>%
  left_join(pop, by = c("xcoord" = "x", "ycoord" = "y", "year" = "mydate")) %>% 
  left_join(geoepr, by = c("xcoord" = "x", "ycoord" = "y", "year" = "mydate")) %>% 
  left_join(wb, by = c("gwno", "year")) %>% 
  left_join(vdem, by = c("gwno", "year")) %>% 
  left_join(kof, by = c("gwno", "year"))


# Log-transformation of relevant variables --------------------------------
# conflict skal egentlig være faktor med two levels (ikke numerisk, da kan de bli feil senere ved rf)

log_vars <- c("gdp", "bdist3", "capdist", "ttime_mean")

final <- new_data %>%
  dplyr::select(gid, year, gwno, lon = xcoord, lat = ycoord, conflict, events, best, spei3, spei3_pos, spei3_neg, temp, 
                agri_ih, irrig_sum, bdist3, capdist, ttime_mean, pop, empl_agr, unempl_tot, excluded, shdi, 
                libdem, global_ind, gdp) %>%
  recipe(.) %>%
  update_role(!!log_vars, new_role = "log_transform") %>%
  step_log(has_role("log_transform"), offset = 1) %>%
  prep() %>%
  juice() %>%
  mutate(conflict = as.factor(conflict))


# Lag the variables -------------------------------------------------------

final <- final %>%
  group_by(gid) %>%
  arrange(year) %>%
  recipe(conflict ~ .) %>% # Identifies the kind of model we have
  remove_role(gid, year, old_role = "predictor") %>% # Remove gid and year from being part of the predictors
  step_lag(all_predictors(), lag = 1) %>% # Lag all the predictors one year
  prep() %>% # Estimate the required parameters from a training set that can later be applied to the whole data set
  juice() # Returns variables from the processed training set (all the lagged variables to the dataset)


final <- final %>%
  group_by(gid) %>%
  arrange(year) %>%
  mutate(lag_conflict = lag(conflict))

saveRDS(final, "./Egne datasett/final_dataset.rds")



