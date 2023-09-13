#setwd("G:\\Nitrate_Trends\\WI_Nitrate_Shiny_App")
#setwd("G:/usr/Nitrate_Trends/WI_Nitrate_Trend_Shiny_App")
#setwd("G:\\usr\\Nitrate_Trends\\WI_Nitrate_Shiny_App")
# setwd("C:/Users/kmasarik/OneDrive - UWSP/SHINY_APPS/WI_NITRATE_TEST/no3_trends-20200518_km")


list.of.packages<-c("ggplot2","lubridate", "plyr", "dplyr", "gridExtra",  "car", "grid", "openxlsx", "Hmisc", "zoo",
                    "DescTools", "birk", "data.table", "stringr", "spatstat", "maptools", "smacpod", "RColorBrewer",
                    "cartography", "sf", "ggmap", "rgdal", "raster", "SpatialPosition", "plotly", "mapproj",
                    "leaflet", "sp", "zyp", "shinythemes", "scales")
new.packages<-list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(ggplot2)
library(cowplot)
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
library(maptools)
library(smacpod)
library(RColorBrewer)
library(cartography)
library(sf)
library(ggmap)
library(rgdal)
library(raster)
library(SpatialPosition)
library(plotly)
library(mapproj)
library(leaflet)
library(sp)
library(zyp)
library(shinythemes)
library(scales)



####CALL IN FILES & OTHER DATA PROCESSING STEPS#####

#Change date to current date when modifying with new data
modified <<- "February 28, 2020"

# Change these values as needed
# currentYear is the most recent year with data. Not necessarily the actual current year
currentYear = 2022

#Call in files, skip first line of headers, header=FALSE means that 1st row is treated as data rather than header

#call in logos for branding app
uwsp <- tags$a(href='https://www.google.com',
               tags$img(src='uwsp.png', height = '75', width = '325', align = 'center'))
uwex <- tags$a(href='https://www.google.com',
               tags$img(src='uwex.png', height = '75', width = '275', align = 'center'))
dnr <- tags$a(href='https://www.google.com',
              tags$img(src='dnr.png', height = '75', width = '125', align = 'center'))


#Update Nitrate_Trend_Test for current data
dfclean <- readRDS("dfclean.rds")
well.trends2 <- readRDS("well.trends2.rds")
dfwisco <- readRDS("dfwisco.rds")
dfwisco2 <- readRDS("dfwisco2.rds")
dfcounty2 <- readRDS("dfcounty2.rds")
dfcleanRecent <- readRDS("dfcleanRecent.rds")
counties <- readRDS("WIcounties2.rds")
wells.unique.bp <- readRDS("wells.unique.bp.rds")
WI_stats <- readRDS("WI_stats.rds")
summary_stats <- readRDS("summary_stats.rds")
summary_stats2 <- readRDS("summary_stats2.rds")




wells.unique.bp0 <- wells.unique.bp[!(wells.unique.bp$k < 20),]

wells.unique.bp2 <- wells.unique.bp0 %>% 
  dplyr::group_by(YEAR , County.name) %>% 
  dplyr::summarise(average = mean(Result.amount), 
                   stdev = sd(Result.amount), 
                   med = median(Result.amount), 
                   q_range = IQR(Result.amount), 
                   cnt = n())

county.mean <- well.trends2 %>% 
  dplyr::group_by(County) %>%
  dplyr::summarise(Nitrate.mean = mean(Nitrate.mean, na.rm = T),
                   wells.n = n())


county.mean$Nitrate.trend <- dfcounty2$Nitrate.trend

county.mean$COUNTY_NAM <- toupper(as.character(county.mean$County))

county.mean$COUNTY_NAM <- as.character(county.mean$COUNTY_NAM)
counties@data$COUNTY_NAM <- toupper(as.character(counties@data$COUNTY_NAM))
counties@data <- left_join(counties@data, county.mean, by = "COUNTY_NAM")

counties@data$Positive.trend <- ((counties@data$np1+counties@data$np2)/counties@data$wells.n)
counties@data$Negative.trend <- ((counties@data$nn1+counties@data$nn2)/counties@data$wells.n)
counties@data$No.trend <- ((counties@data$n0)/counties@data$wells.n)

# well.trends2$Positive.trend <- well.trends2$Nitrate.trend
# well.trends2$Negative.trend <- well.trends2$Nitrate.trend

#Create new df for statewide pie chart
dfwisco$cnt <- 1

#The next few lines will order the inputs in a proper way
dfclean <- dfclean[order(dfclean[["YEAR"]]),]
dfclean <- dfclean[order(dfclean[["County.name"]]),]
vector_WUWN <- as.vector(sort(dfclean$WI.unique.well..))

dfcleansig <- dfclean[!duplicated(dfclean[["WI.unique.well.."]]),]

####START SHINY APP#####



shinyApp(
  source('UI.R', local = TRUE),
  source('Server.R', local = TRUE)
)

### PARENTHESIS NEEDED TO CLOSE SHINYAPP ###

# Run the application 
shinyApp(ui = ui, server = server)