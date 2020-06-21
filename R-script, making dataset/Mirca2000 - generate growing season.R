### Generate growing season to use as weight for SPEI  ###

library(tidyverse)

## NB: To be able to run this script, must run the "priogrid-functions" - script first.

# Functions from priogrid -------------------------------------------------

#' Generate monthly growseas variable

#' Function to generate the area proportion of each cell

#' that is being harvested for each month. 

#' 


# Load Cropping periods list data, v. 1.1. Resolution 30 arc-minute

mirca <- read.table(gzfile("./Data/MIRCA/growing_periods_listed/CELL_SPECIFIC_CROPPING_CALENDARS_30MN.TXT.gz"), header = T, sep = "\t")


# Make raster and save -----------------------------------------------
# Must run all the functions below to get the raster

mirca_growseas <- gen_growseas(mirca)
saveRDS(mirca_growseas, "./Egne datasett/mirca_growseas.rds")

mirca_startend <- gen_grow_start_end(mirca)
saveRDS(mirca_startend, "./Egne datasett/mirca_startend.rds")

# Function ----------------------------------------------------------------




gen_growseas <- function(mirca){
  
  m_small <- prep_growseas(mirca)
  
  
  
  m_raster <- raster::rasterFromXYZ(m_small[c("long", "lat", "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug",
                                              
                                              "sep", "oct", "nov", "dec")], crs = prio_crs(), digits = 1)
  
  
  
  agg <- prio_aggregate_raster(m_raster, fun = mean)
  
  raster::extent(agg) <- prio_extent()
  
  
  
  names(agg) <- tolower(month.abb)
  
  
  
  agg
  
}






#' Generate grow_start and grow_end variables

#' 

#' Function to generate grow_start and grow_end 

#' based on MIRCA2000 data.

#' 

#' @param mirca MIRCA2000 data (.TXT).

#' @param no_months Width of rolling window (number of months) used to compute maximum sum of harvested area. Defaults to 5.

#' 

#' @return PRIO-GRID-consistend RasterBrick with one layer for each variable.


# Function ----------------------------------------------------------------



gen_grow_start_end <- function(mirca, no_months = 5){
  
  mirca_prep <- prep_growseas(mirca) # NOTE: Does not work - need to figure out data.table dependencies
  
  
  
  mirca_prep <- mirca_prep %>%
    
    dplyr::mutate(jan2 = jan, feb2 = feb, mar2 = mar, apr2 = apr, may2 = may) %>%
    
    tidyr::gather(key = "month", value = "area", -long, -lat) %>%
    
    dplyr::mutate(nummonth = match(tolower(month), tolower(month.abb))) %>%
    
    dplyr::mutate(nummonth = ifelse(month == "jan2", 13,
                                    
                                    ifelse(month == "feb2", 14,
                                           
                                           ifelse(month == "mar2", 15,
                                                  
                                                  ifelse(month == "apr2", 16,
                                                         
                                                         ifelse(month == "may2", 17, nummonth)
                                                         
                                                  ))))) %>%
    
    dplyr::arrange(long, lat, nummonth) %>%
    
    dplyr::mutate(id = dplyr::group_indices())
  
  
  
  
  
  data.table::setDT(mirca_prep)
  
  
  
  mirca_prep[,consec_mo := zoo::rollapply(area, width = no_months, sum, align = "right", fill = NA, partial = F), by = id]
  
  
  
  recode_months <- function(var){
    
    ifelse(var == 13, 1,
           
           ifelse(var == 14, 2,
                  
                  ifelse(var == 15, 3,
                         
                         ifelse(var == 16, 4,
                                
                                ifelse(var == 17, 5,
                                       
                                       var)))))
    
  }
  
  
  
  
  
  mirca_final <- mirca_prep %>%
    
    dplyr::mutate(grow_start = ifelse(!is.na(consec_mo), nummonth - (no_months - 1), NA),
                  
                  grow_end = ifelse(!is.na(consec_mo), nummonth, NA)) %>%
    
    dplyr::mutate(grow_start = recode_months(grow_start),
                  
                  grow_end = recode_months(grow_end)) %>%
    
    dplyr::group_by(lat, long) %>%
    
    dplyr::slice(which.max(consec_mo)) ### NOTE: This approach does NOT consider ties - it chooses the FIRST max for each point by default
  
  
  
  
  
  growseas <- raster::rasterFromXYZ(mirca_final[c("long", "lat", "grow_start", "grow_end")], crs = prio_crs(), digits = 1)
  
  grow_start <- prio_aggregate_raster(growseas[[1]], fun = first)
  
  grow_end <- prio_aggregate_raster(growseas[[2]], fun = last)
  
  growseas <- raster::brick(grow_start, grow_end)
  
  names(growseas) <- c("grow_start", "grow_end")
  
  
  
  growseas
  
  
  
}






# Function ----------------------------------------------------------------



#' Data preparation function



prep_growseas <- function(mirca){
  
  # mirca <- read.table(gzfile(mirca), header = T, sep = "\t")
  
  
  
  m_small <- mirca %>%
    
    dplyr::group_by(lat, lon, start, end) %>%
    
    dplyr::summarize(area = sum(area))
  
  
  
  data.table::setDT(m_small)
  
  m_small <- m_small[, list(lat = lat, long = lon, area = area, start = start, end = end, month = select_months(start, end)),
                     
                     list(1:nrow(m_small))][, nrow := NULL][]
  
  
  
  m_small <- m_small[, list(area = sum(area, na.rm = T)), by = c("lat", "long", "month")]
  
  
  
  
  
  m_small <- m_small %>%
    
    dplyr::group_by(lat, long) %>%
    
    dplyr::mutate(season_weight = area / sum(area)) %>%
    
    dplyr::select(lat, long, month, season_weight) %>%
    
    tidyr::spread(month, season_weight) %>%
    
    magrittr::set_names( c("lat", "long", tolower(month.abb)) )
  
  
  
  m_small
  
}




# Function ----------------------------------------------------------------



select_months <- function(start, end, drop_perennials=FALSE){
  
  if(is.na(start) | is.na(end)){
    
    return(rep(NA, 12))
    
  } else if(drop_perennials & start==1 & end==12){ # perennials have growing seasons 1:12
    
    return(rep(NA, 12))
    
  } else {
    
    if(start <= end){
      
      return(start:end)
      
    } else {
      
      return(c(start:12, 1:end))
      
    }
    
  }
  
}



#' Roll over Columns

#'

#' Calculates the sum of a rolling window over a range of columns.

#' Only considers columns not specified in "exclude"

#'

#' Assumes that the columns are sorted

#'

#' @param data A set of data

#' @param exclude Columns to exclude 

#' @param winsize Size of the window 

#' @param shift Shift the window. Ex. size 3 / shift 1 gives sum FROM col instead of around


# Function ----------------------------------------------------------------


rollovercols <- function(data, fun = sum, winsize = 3, shift = 1, 
                         
                         exclude = c('lat','long'), debug = FALSE){
  
  if(winsize %% 2 == 0){
    
    stop(glue::glue('Window size must be odd, not {winsize}!'))
    
  } 
  
  dnames <- names(data)
  
  months <- dnames[!dnames %in% exclude]
  
  
  
  winmid <- ceiling(winsize / 2)
  
  winmargin <- (winsize - 1) / 2
  
  mids <- seq(winmid,length(months) - winmid,1)
  
  
  
  rowindices <- 1:length(months) 
  
  winindices <- c(tail(rowindices,winmargin + abs(shift)),
                  
                  rowindices,
                  
                  head(rowindices,winmargin + abs(shift)))
  
  
  
  sumdata <- apply(data,1,function(row){
    
    row <- row[months]
    
    
    
    sapply(1:length(row),function(colindex){
      
      mnames <- names(row)
      
      wincenter  <- colindex + winmargin
      
      w <- seq(colindex + shift + abs(shift),
               
               ((winsize + colindex) - 1) + shift + abs(shift))
      
      window <- winindices[w]
      
      if(debug){
        
        print(glue::glue('Names: {glue::glue_collapse(mnames[window],sep = \',\')}'))
        
        print(glue::glue('Values: {glue::glue_collapse(row[window],sep = \',\')}'))
        
        
        
        print(glue::glue('Index: {colindex}'))
        
        print(glue::glue('Sum: {sum(row[window])}'))
        
      }
      
      fun(row[window])
      
    })
    
  })
  
  res <- as.data.frame(t(sumdata))
  
  names(res) <- months
  
  for(val in exclude){
    
    res[val] <- data[val]
    
  }
  
  res
  
}



