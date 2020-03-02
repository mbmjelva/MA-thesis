#### MIRCA2000 fra rasterbrick til tibble #####

library(tidyverse)

mirca_growseas <- read_rds("./Egne datasett/mirca_growseas.rds")
mirca_startend <- read_rds("./Egne datasett/mirca_startend.rds")

# Growingseason
res <- tibble()
for(i in 1:length(names(mirca_growseas))){
  pop_year <- raster::subset(mirca_growseas, i)
  
  df <- raster::rasterToPoints(pop_year)
  df <- dplyr::as_tibble(df)
  df <- tidyr::gather(df, key = "names", value = "value", -x, -y)
  # df$names <- sub("Year.", "", df$names) Finnes ikke på årsnivå, dropper denne
  # df$names <- as.factor(df$names) # Er også i character form
  res <- bind_rows(res, df)
}

res <- rename(res, "gs_weight" = "value")


saveRDS(res, "./Egne datasett/mirca_growseas_tibble.rds")



# Start-end

res_startend <- tibble()
for(i in 1:length(names(mirca_startend))){
  pop_year <- raster::subset(mirca_startend, i)
  
  df <- raster::rasterToPoints(pop_year)
  df <- dplyr::as_tibble(df)
  df <- tidyr::gather(df, key = "names", value = "value", -x, -y)
  # df$names <- sub("Year.", "", df$names) Finnes ikke på årsnivå, dropper denne
  # df$names <- as.factor(df$names) # Er også i character form
  res_startend <- bind_rows(res_startend, df)
}

mirca_startend

