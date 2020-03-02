##### SHDI til grid #####


#setwd("/Users/mathildemjelva/Library/Mobile Documents/com~apple~CloudDocs/Mastergrad/Masteroppgave/R-master/")
library(tidyverse)
library(sf)


# Funksjoner for å kjøre de andre funksjonene -----------------------------

# Funksjoner for å kjøre det under 
prio_crs <- function(){"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"}
prio_resolution <- function(){0.5}
prio_extent <- function(){raster::extent(-180, 180, -90, 90)}
prio_nrow <- function(){360}
prio_ncol <- function(){720}


create_pg_indices <- function(ncol, nrow){
  # To create PRIO-GRID, swap ncol and nrow, load index in reverse order, and
  # rotate 90 degrees once.
  rotate <- function(x) t(apply(x, 2, rev))
  pg <- rotate(matrix(rev(1:(ncol*nrow)), nrow=ncol, ncol=nrow))
  
  return(pg)
}

# Funksjon for å lage prio_blank_grid

prio_blank_grid <- function(ncol = FALSE, nrow = FALSE, crs = FALSE, extent = FALSE){
  if(!ncol){
    ncol <- prio_ncol()
  }
  if(!nrow){
    nrow <- prio_nrow()
  }
  if(!crs){
    crs <- prio_crs()
  }
  if(!extent){
    extent <- prio_extent()
  }
  
  pg <- create_pg_indices(ncol,nrow)
  pg <- raster::raster(pg, crs = crs)
  raster::extent(pg) <- extent
  pg
}




# Funksjon for å lage yearly_brick

yearly_brick <- function(data, variable, raster.fun){
  years <- unique(data$year)
  emp <- list()
  
  for(i in 1:length(years)) {
    y <- years[i]
    if(sum(data$year == y) > 0)
      emp[[i]] <- raster::rasterize(data[which(data$year == y),],
                                    prio_blank_grid(),
                                    field = variable,
                                    fun = raster.fun)
  }
  brick <- raster::brick(emp)
  names(brick) <- paste0(variable,"_", years)
  return(brick)
}


# Lager SHDI --------------------------------------------------------------


gen_shdi <- function(path) {
  shdi <- read.csv(file.path("./Data/SHDI/SHDI Complete 3.0.csv"))
  geom <- sf::read_sf(file.path("./Data/SHDI/GDL-SHDI-SHP-2/GDL-SHDI-SHP-2.shp"))
  
  shdi <- shdi %>%
    dplyr::filter(level == "Subnat") %>%
    dplyr::select(year, iso_code, GDLCODE, shdi, esch, lgnic, lifexp, msch) # Har addet alle variablene her, men får ikke til å ha med alle til slutt.
  
  geom <- geom %>%
    dplyr::select(GDLCode, iso_code, geometry)
  
  
  shdi.full <- merge(geom, shdi, by.x = "GDLCode", by.y = "GDLCODE")
  
  shdiyr <- yearly_brick(shdi.full, variable = 'shdi', raster.fun = 'mean') # Her.
  return(shdiyr)
}

shdi <- gen_shdi(".")


# Gjør om fra rasterbrick til dataframe -----------------------------------


rasterbrick_to_df <- function(rasterbrick, prefix) {
  f <- tibble()
  for(i in 1:length(names(rasterbrick))){
    
    pop_year <- raster::subset(rasterbrick, i)
    
    df <- raster::rasterToPoints(pop_year)
    df <- dplyr::as_tibble(df)
    df <- tidyr::gather(df, key = "mydate", value = "value", -x, -y)
    df$mydate <- sub(prefix, "", df$mydate)
    df$mydate <- as.numeric(df$mydate)
    f <- bind_rows(f, df)
  }
  return(f)
}

shdi <- rasterbrick_to_df(shdi, "shdi_")


saveRDS(shdi, "./Egne datasett/shdi_rasterbrick.rds")
saveRDS(res, "./Egne datasett/shdi.rds")



