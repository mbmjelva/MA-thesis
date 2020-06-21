#######################
### EPR - tidying   ###
#######################

library(tidyverse)


# Funksjoner som må til for å lage geoepr ---------------------------------


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



# Lager geoepr ------------------------------------------------------------


gen_geoepr <- function(path){
  files <- file.path(path, c("./Data/Ethnic Power Relations dataset/GeoEPR-2019 2/GeoEPR.shp","./Data/Ethnic Power Relations dataset/EPR-2019.csv"))
  
  geoepr <- sf::st_read(files[1], stringsAsFactors = FALSE, quiet = TRUE)
  geoepr <- sf::st_transform(geoepr, crs = prio_crs())

  geoepr <- geoepr %>%
    dplyr::select(gwno = gwid, from, to, group, grptype = type, groupid, geometry) %>%
    dplyr::mutate(from = as.numeric(from), to = as.numeric(to),
                  year = prio_year(from, to)) %>%
    tidyr::unnest(year)

  # Join with EPR to obtain group status
  epr <- read.csv(files[2])
  
  
  epr <- epr %>%
    dplyr::select(gwno = gwid, groupid, status, from, to) %>%
    dplyr::mutate(excluded = ifelse(status == "POWERLESS" | status == "DISCRIMINATED",
                                    yes = 1, no = 0),
                  year = prio_year(from, to)) %>%
    tidyr::unnest(year) %>%
    dplyr::select(gwno, groupid, year, excluded)
  
  
  geoepr_epr <- base::merge(geoepr, epr,
                            by.x = c("gwno", "groupid", "year"),
                            by.y = c("gwno", "groupid", "year"),
                            all.x = TRUE)
  
  geoepr_epr <- geoepr_epr[!sf::st_is_empty(geoepr_epr$geometry),]
  
  brick <- yearly_brick(geoepr_epr, 
                                  variable = "excluded", 
                                  raster.fun = "count")
  
  return(brick)
}


geoepr <- gen_geoepr(".")


# From raster to dataframe ------------------------------------------------


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

geoepr <- rasterbrick_to_df(geoepr, "excluded_")

geoepr$value <- geoepr$value -1 # Give cells with no excluded groups value 0 instead of 1

# Export ------------------------------------------------------------------

saveRDS(geoepr, "./Egne datasett/geoepr.rds")
