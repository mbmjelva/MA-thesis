
############################
### Population - tidying ###
############################

library(tidyverse)
library(countrycode)

# setwd("/Users/mathildemjelva/Library/Mobile Documents/com~apple~CloudDocs/Mastergrad/Masteroppgave/R-master/")
pop <- readRDS("./Data/Population density/gpw4_population_density_interpolated.rds")


res <- tibble()
for(i in 1:length(names(pop))){
  pop_year <- raster::subset(pop, i)
  
  df <- raster::rasterToPoints(pop_year)
  df <- dplyr::as_tibble(df)
  df <- tidyr::gather(df, key = "mydate", value = "value", -x, -y)
  df$mydate <- sub("Year.", "", df$mydate)
  df$mydate <- as.numeric(df$mydate)
  res <- bind_rows(res, df)
}


saveRDS(res, "./Egne datasett/population.rds")



