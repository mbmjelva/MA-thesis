
## Constructing dataset - R ##

library(tidyverse)
library(countrycode)
library(questionr)
# setwd("/Users/mathildemjelva/Library/Mobile Documents/com~apple~CloudDocs/Mastergrad/Masteroppgave/R-master/")

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

# Remove all gridcells that cover ocean(water?)
lons <- unique(prio_ucdp$xcoord)
lats <- unique(prio_ucdp$ycoord)

cru <- filter(cru, (lon %in% lons & lat %in% lats))

# Divide SPEI3 into two variables based on positive and negative values

cru <- cru %>% mutate(spei3_pos = ifelse(spei3 > 0, spei3, 0),
                      spei3_neg = ifelse(spei3 < 0, spei3, 0))

# Changing country to gwno for all variables to be matched by country ------------------------------

# Fra priogrid er det kun de landene som er independent på det gitte tidspunktet som tas med. De som ikke er independent skrives om til missing. 
# Altså er det kun de som blir missing under og som er småstater som må endres på manuelt, hvis de korresponderer med priogrid.


## vdem
#' Kommentar: Palestina og Hong Kong må settes til NA fordi de ikke er selvstendige stater
#' Mikrostatene er inkludert i priogrid, men får ikek tl å konvertere dem (muligens ikke inkludert i countrycodes). Endrer manuelt etterpå. Yemen måtte bare bytte navn.
#' 

vdem <- dplyr::mutate(vdem, gwno = countrycode(country, "country.name", "gwn"))

vdem$gwno[vdem$country == "Seychelles"] <- 591
vdem$gwno[vdem$country == "Vanuatu"] <- 935
vdem$gwno[vdem$country == "Sao Tome and Principe"] <- 403
vdem$gwno[vdem$country == "Yemen"] <- 678

##' WB
#' Her er det mange verdier som mangler, men de er for det meste små stater som ikke er med i countrycodes datasettet. Hva skal man gjøre med dem? Kode manuelt? Se på priogrid og hvordan det er kodet der

wb <- mutate(wb, gwno = countrycode(country, "country.name", "gwn"))

# Må endre manuelt på mikrostater (og Yemen) som er selvstendige. Ikke-selvstendige stater blir satt til missing på gwno.
wb$gwno[wb$country == "Dominica"] <- 54
wb$gwno[wb$country == "Grenada"] <- 55
wb$gwno[wb$country == "Kiribati"] <- 970
wb$gwno[wb$country == "Liechtenstein"] <- 223
wb$gwno[wb$country == "Marshall Islands"] <- 983
wb$gwno[wb$country == "Micronesia, Fed. Sts."] <- 987
wb$gwno[wb$country == "Monaco"] <- 221
wb$gwno[wb$country == "Nauru"] <- 971
wb$gwno[wb$country == "Palau"] <- 986
wb$gwno[wb$country == "Samoa"] <- 990
wb$gwno[wb$country == "San Marino"] <- 331
wb$gwno[wb$country == "Sao Tome and Principe"] <- 403
wb$gwno[wb$country == "Seychelles"] <- 591
wb$gwno[wb$country == "Tonga"] <- 972
wb$gwno[wb$country == "Tuvalu"] <- 973
wb$gwno[wb$country == "Vanuatu"] <- 935
wb$gwno[wb$country == "Yemen"] <- 678

# Kof
# Samme problem her som for wb, pluss at landmed upper middle income etc. er inkludert. Er vel ikke så farlig, de blir uansett omgjort til na ved merging (?)

kof <- kof %>% mutate(gwno = countrycode(country, "country.name", "gwn"))

kof$gwno[vdem$country == "Andorra"] <- 231
kof$gwno[vdem$country == "Dominica"] <- 54
kof$gwno[vdem$country == "Kiribati"] <- 970
kof$gwno[vdem$country == "Liechtenstein"] <- 223
kof$gwno[vdem$country == "Marshall Islands"] <- 983
kof$gwno[vdem$country == "Micronesia, Fed. Sts."] <- 987
kof$gwno[kof$country == "Monaco"] <- 221
kof$gwno[kof$country == "Palau"] <- 986
kof$gwno[kof$country == "Samoa"] <- 990
kof$gwno[kof$country == "San Marino"] <- 331
kof$gwno[kof$country == "Sao Tome and Principe"] <- 403
kof$gwno[kof$country == "Seychelles"] <- 591
kof$gwno[kof$country == "Tonga"] <- 972
kof$gwno[kof$country == "Vanuatu"] <- 935
kof$gwno[kof$country == "Yemen, Rep."] <- 678


# Merging datasets --------------------------------------------------------
wb$year <- as.numeric(wb$year)
prio_ucdp$year <- as.numeric(prio_ucdp$year)


# Gjør om navnene slik at det blir enklere å lese, ikke er to som heter "value" (shdi, pop)
pop <- questionr::rename.variable(pop, "value", "pop")
shdi <- questionr::rename.variable(shdi, "value", "shdi")
geoepr <- questionr::rename.variable(geoepr, "value", "excluded")


new_data <- prio_ucdp %>% 
  left_join(shdi, by = c("xcoord" = "x", "ycoord" = "y", "year" = "mydate")) %>%
  left_join(cru, by = c("xcoord" = "lon", "ycoord" = "lat", "year" = "year")) %>%
  left_join(pop, by = c("xcoord" = "x", "ycoord" = "y", "year" = "mydate")) %>% 
  left_join(geoepr, by = c("xcoord" = "x", "ycoord" = "y", "year" = "mydate")) %>% 
  left_join(wb, by = c("gwno", "year")) %>% 
  left_join(vdem, by = c("gwno", "year")) %>% 
  left_join(kof, by = c("gwno", "year"))


# conflict skal egentlig være faktor med two levels (ikke numerisk, da kan de bli feil senere ved rf)
final <- new_data  %>% dplyr::select(gid, year, gwno, lon = xcoord, lat = ycoord, conflict, events, best, spei3, spei3_pos, spei3_neg, temp, 
                                     agri_ih, irrig_sum, bdist3, capdist, ttime_mean, pop, empl_agr, unempl_tot, excluded, shdi, 
                                     libdem, global_ind, gdp) %>% 
  mutate(conflict = as.factor(conflict))


saveRDS(final, "./Egne datasett/final_dataset.rds")



