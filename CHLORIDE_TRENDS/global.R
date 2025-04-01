#setwd("G:\\Nitrate_Trends\\WI_Nitrate_Shiny_App")
#setwd("G:/usr/Nitrate_Trends/WI_Nitrate_Trend_Shiny_App")
#setwd("G:\\usr\\Nitrate_Trends\\WI_Nitrate_Shiny_App")

##As of February 28th, 2023, it runs! 

list.of.packages<-c("ggplot2","lubridate", "plyr", "dplyr", "gridExtra",  "car", "grid", "openxlsx", "Hmisc", "zoo",
                    "DescTools", "birk", "data.table", "stringr", "spatstat", "maptools", "smacpod", "RColorBrewer",
                    "cartography", "sf", "ggmap", "rgdal", "raster", "SpatialPosition", "plotly", "mapproj",
                    "leaflet", "sp", "zyp", "shinythemes", "scales")
#new.packages<-list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)

library(shiny)
#library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(gridExtra)
library(car)
library(grid)
library(openxlsx)
library(Hmisc)
library(zoo)
library(DescTools)
library(birk)
library(data.table)
library(stringr)
library(spatstat)
#library(maptools)
library(smacpod)
library(RColorBrewer)
library(cartography)
library(sf)
library(ggmap)
#library(rgdal)
library(raster)
library(SpatialPosition)
library(plotly)
library(mapproj)
library(leaflet)
library(sp)
library(zyp)
library(shinythemes)
library(scales)

library(shiny)
library(leaflet)
library(sp)
library(sf)
library(shinythemes)
library(plyr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(gridExtra)
library(car)
library(grid)
library(openxlsx)
library(RColorBrewer)
library(rgdal)
library(tidyr)
library(plotly)
#library(ggpubr)
library(rhandsontable)
library(vtable)

####CALL IN FILES & OTHER DATA PROCESSING STEPS#####

#Change date to current date when modifying with new data
modified <- "March 17, 2025"

#Call in files, skip first line of headers, header=FALSE means that 1st row is treated as data rather than header

#Update Nitrate_Trend_Test for current data
dfclean <- readRDS("dfclean_reg.rds")
well.trends <- readRDS("wells.unique.rds")
well.trends2 <- readRDS("well.trends2.rds")
dfwisco <- readRDS("dfwisco.rds")
dfwisco2 <- readRDS("dfwisco2.rds")
dfcounty2 <- readRDS("dfcounty2.rds")
dfcleanRecent <- readRDS("dfcleanRecent.rds")
counties <- readRDS("WIcounties.rds")
wells.unique.bp <- readRDS("wells.unique.bp.rds")
#dfwisco_sig_count <- readRDS("dfwisco_sig_count.rds")

wells.unique.bp0 <- wells.unique.bp[!(wells.unique.bp$k < 4),]

wells.unique.bp2 <- wells.unique.bp0 %>% 
  dplyr::group_by(YEAR, County.name) %>% 
  dplyr::summarise(average = mean(Sample.Analytical.Result.Amount), stdev = sd(Sample.Analytical.Result.Amount), med = median(Sample.Analytical.Result.Amount), q_range = IQR(Sample.Analytical.Result.Amount), cnt = n())
#goes from 2920 obs. to 802 obs.
county.mean <- well.trends %>% 
  dplyr::group_by(County) %>%
  dplyr::summarise(Chloride.mean = mean(Chloride.mean, na.rm = T),
                   wells.n = n())
#is now only 72 obs.
county.mean$Chloride.trend <- dfcounty2$Chloride.trend

county.mean$COUNTY_NAM <- toupper(as.character(county.mean$County))

#colnames(county.mean)[colnames(county.mean) == "County"] <- "COUNTY_NAM"
county.mean$COUNTY_NAM <- as.character(county.mean$COUNTY_NAM)
counties@data$COUNTY_NAM <- toupper(as.character(counties@data$COUNTY_NAM))
counties@data <- left_join(counties@data, county.mean, by = "COUNTY_NAM")

#The next few lines will order the inputs in a proper way
dfclean <- dfclean[order(dfclean[["YEAR"]]),]
dfclean <- dfclean[order(dfclean[["County.name"]]),]
vector_WUWN <- as.vector(sort(dfclean$WI.Unique.Well..))

dfcleansing <- dfclean[!duplicated(dfclean[["WI.Unique.Well.."]]),]
#everything up through here runs at least 