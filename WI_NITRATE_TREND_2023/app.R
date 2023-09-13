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
library(tidyverse)
library(dplyr)
library(plyr)
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
library(magrittr)
# library(knitr)


####CALL IN FILES & OTHER DATA PROCESSING STEPS#####
#Change date to current date when modifying with new data
modified <- "February 28, 2020"


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
dfcounty2 <- readRDS("dfcounty2.rds")
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





# Variable values that get used in the text throughout the application. These will naturally change
totalTrendWells <- as.numeric(nrow(well.trends2))
noSigTrend <- sprintf("%0.1f%%", subset(WI_stats, Trend == "No Significant Trend")$Percent * 100)
posSigTrend <- sprintf("%0.1f%%", (subset(WI_stats, Trend == "Significant Decrease (>2.5 mg/L per decade change)")$Percent * 100)+(subset(WI_stats, Trend == "Slight Decrease (1.0-2.5 mg/L per decade change)")$Percent * 100))
negSigTrend <- sprintf("%0.1f%%", (subset(WI_stats, Trend == "Significant Increase (>2.5 mg/L per decade change)")$Percent * 100)+(subset(WI_stats, Trend == "Slight Increase (1.0-2.5 mg/L per decade change)")$Percent * 100))

####START SHINY APP#####



shinyApp(
  ui <- fluidPage(
    
    # App title ----
    #headerPanel(title=uwsp,dnr),
    titlePanel("Nitrate in Wisconsin's Public Water Systems"), 
    
    #shinythemes::themeSelector("darkly"),
    
    ###START NAVIGATION BAR###
    
    
    ###CONTROLS BACKGROUND THEME###
    theme = shinythemes::shinytheme('flatly'), # <--- To not use a theme, comment this
    "",
    sidebarLayout(
      mainPanel(width = 6,
                fluidRow(
                  column(4,
                         selectInput("type", "Map Type",
                                     c("County Summary" = "county",
                                       "Individual Wells" = "point"))
                  ),
                  column(4,
                         selectInput("var" , "Variable:",
                                     c("Nitrate Concentration" = "Nitrate.mean",
                                       "Nitrate Trend" = "Nitrate.trend",
                                       "Increasing Wells" = "Positive.trend",
                                       "Decreasing Wells" = "Negative.trend"))
                  )
                ),
                hr(),
                fluidRow(
                  column(12, offset = 0,
                         leaflet::leafletOutput("map", height = 620)
                  )
                ),
                br(),
                div(strong("Created by: Grant Moser, Jennifer Dierauer, and Kevin Masarik")),
                #div("Grant Moser, Jennifer Dierauer, and Kevin Masarik"),
                div("Center for Watershed Science and Education in partnership with the Wisconsin Department of Natural Resources"),
                #div("in partnership with the Wisconsin Department of Natural Resources"),
                div("Last modified:", modified,'.', a(href="mailto:kmasarik@uwsp.edu","Contact Kevin Masarik for questions")),
                #div(a(href="mailto:kmasarik@uwsp.edu","Contact Kevin Masarik for questions")),
                #br(),
                div(uwsp,uwex,dnr),
      ),
      
      sidebarPanel(width = 6, 
                   tabsetPanel(
                     tabPanel("About",
                              h4(strong("Overview")),
                              p("Public water systems are required to submit annual nitrate samples
                              to the Wisconsin Department of Natural Resources. These data create an opportunity to learn about current quality 
                              in addition to exploring changes in nitrate concentrations over time in these wells. For more information on the data and methods used to determine trends click on the 'Data' Section"),
                              br(),
                              h4(strong("Individual Well Data")),
                              p(strong("Nitrate Concentration")),
                              p("Represents the most recent nitrate-nitrogen concentration of those public wells that have submitted a nitrate
                              sample within the past 6 years. Additional information can be obtained by clicking on the individual well location on the map."),
                              #br(),
                              p(strong("Nitrate Trend")),
                              p("Linear regression was used to determine nitrate trends for each individual public water supply system. 
                              Size of point represents magnitude of the rate of change. Determination of a trend does not mean that water quality will continue to increase 
                              or decrease indefinitely, ultimately changes to the surrounding land use could result in changes to nitrate concentrations in these wells. To see 
                              the annual data or to learn more about how the individual nitrate trend was determined go to 'Individual Well'"),
                              p(strong("Decreasing Wells")),
                              p("Individual wells with a decreasing trend."),
                              p(strong("Increasing Wells")),
                              p("Individual wells with an increasing trend."),
                              br(),
                              h4(strong("County Summary")),
                              p(strong("Nitrate Concentration")),
                              p("Mean annual nitrate-nitrogen concentration of public water systems for 2018."),
                              p(strong("Nitrate Trends")),
                              p("Because public water supply wells may not be statistically representative of land use and geology of the county
                                as a whole, trend data is currently not summarized at the county level.  We hope to develop ways to investigate this 
                                question at a county level soon."),
                              p(strong("Increasing Wells")),
                              p("Percent of public water supply wells by county with a statistically significant increasing trend."),
                              p(strong("Decreasing Wells")),
                              p("Percent of public water supply wells by county with a statistically significant decreasing trend."),
                              # p(strong("Nitrate Trends")),
                              # p("A subset of the public wells with more than 20 years of nitrate results were used to determine if there is evidence of 
                              # a county-wide nitrate trend. Overall county trends while statistically significant, often represent a very small change in county-wide annual 
                              # mean nitrate concentrations.  Particularly in counties with a small number of public water systems, the county trend may be influenced by one or two wells that have 
                              # shown increases over time. To see the annual data or learn more about how the county trend was determined go to `County Summary`."),
                              # br(),
                              # div(uwsp,dnr),
                              # br(),
                              # div(strong("Created by: Grant Moser, Jennifer Dierauer, and Kevin Masarik")),
                              # #div("Grant Moser, Jennifer Dierauer, and Kevin Masarik"),
                              # div("Center for Watershed Science and Education in partnership with the Wisconsin Department of Natural Resources"),
                              # #div("in partnership with the Wisconsin Department of Natural Resources"),
                              # div("Last modified:", modified),
                              # div(a(href="mailto:kmasarik@uwsp.edu","Contact Kevin Masarik for questions")),
                              
                     ),
                     tabPanel("Individual Wells",  
                              h4(strong("Nitrate-nitrogen Concentrations by Wisconsin Unique Well Number")),
                              p("Nitrate-nitrogen concentration by year for each public water system with more than 6 years of data. Use the drop down menu
                        below to select any public water system using its Wisconsin Unique Well Number."),
                              p("A linear model was used to fit a regression line to the annual maximum nitrate concentrations. The gray 'bands' around the regression line in the plot below represent the range in which the true regression line lies at a certain 
                      level of confidence (95% in the plot)."),
                              p("The regression line or trend is considered significant if the p-value < 0.05 and the slope 
                      is greater than 0.10 (or results in a change of greater than 1 mg/L for a 10 year period)"),
                              br(),
                              selectizeInput("r",
                                             "Select or type a WI Unique Well Number:",
                                             choices = unique(wells.unique.bp$WI.unique.well..),
                                             selected = "",
                                             width = 300
                              ),
                              h4(strong("Trend Line Equation, R-squared, and p-value:")),
                              htmlOutput("WUWN_info"),
                              br(),
                              htmlOutput("WUWN_trend"),
                              br(),
                              plotlyOutput("line_WUWN", height = 400)
                     ),
                     tabPanel("County Summary",
                              h4(strong("Median and Mean Annual Nitrate Concentration by County")),
                              p("Annual county-wide median and mean nitrate-nitrogen concentration for 
                                all public water system with more than 20 years of data. 
                                Wells with less than 20 years of nitrate data were excluded to reduce bias 
                                when contaminated wells are decommissioned or new wells come online."),
                              p("Box plots represent the distribution of annual data.  Annual mean concentration represented 
                                by the blue diamond.  Number of samples listed below box plot for 
                                each year."),
                              p("Use the drop down menu below to view data for any county."), 
                              #p("A linear model was used to fit a regression line to annual county mean nitrate concentration. The gray 'bands' around the regression line in the plot below represent the range 
                              #in which the true regression line lies at a certain level of confidence (95% in the plot)."),
                              #p("The regression line or trend is considered significant if the p-value < 0.05 and if the slope is greater than 0.01 (or results in a change of greater than 0.1 mg/L for a 10 year period)"),
                              br(),
                              selectizeInput("l",
                                             "Choose a County:",
                                             choices = wells.unique.bp2$County.name,
                                             selected = "",
                                             width = 300
                              ),
                              #h4(strong("Trend Line Equation, R-squared, and p-value:")),
                              #htmlOutput("county_info"),
                              br(),
                              htmlOutput("county_trend"),
                              br(),
                              plotlyOutput("select_county", height = 400)
                              # br(),
                              # plotlyOutput("county_histo")
                     ),
                     tabPanel("Statewide Summary",
                              h4(strong("Statewide Overview")),
                              br(),
                              p(strong("Trends in Public Water Supply Systems")),
                              p("Trends in the", totalTrendWells, "public water supply systems for which more than 6 years of data exist suggest 
                              no significant trend in",noSigTrend , "of wells, an increasing trend in", posSigTrend, "of wells, and a decreasing trend 
                              in", negSigTrend, "of wells. While most wells show no significant trend over time, these data suggest
                              of those that demonstrate a significant trend, slightly more are increasing than are decreasing."),
                              p("For the purposes of figure below a slight increasing/decreasing trend is considered to be any well with a p-value 
                                less than 0.05 and a rate of change greater than 1 mg/L over a 10 year period; while a significant increasing/
                                decreasing trend is one in which the p-value is less than 0.05 and the rate of change is greater than 2.5 mg/L over a 
                                10 year period."),
                              br(),
                              plotlyOutput("trend_pie"),
                              br(),
                              p(strong("Annual Statewide Nitrate-Nitrogen Summary")),
                              p("Summary statistics of nitrate-nitrogen concentrations by year for all public water system with more than 20 years of data. Wells with less than 20 years of data were excluded 
                                to reduce bias when contaminated wells are decommissioned or new wells come online."), 
                              plotlyOutput("wi_summary"),
                              p("Plot A: Annual summary statistics with wells grouped by the trend classification (Significant Increase, Slight Increase, etc.). 
                                Annual mean concentration is represented by circles. Hover over circles for more information, e.g., number of samples, maximum value, etc."),
                              br(),
                              plotlyOutput("wi_summary2"),
                              p("Plot B: Annual summary statistics. Annual mean concentration is represented by circles; interquartile range is represented by black vertical lines. 
                                Hover over circles for more information, e.g., number of samples, maximum value, etc.")
                     ),
                     tabPanel("Data",
                              h4(strong("Data Overview")),
                              p("Samples for nitrate are generally submitted annually from public water systems and are required to be reported to the Department of Natural Resources.
                      Data is publicly available from the Wisconsin Department of Natural Resources'",a(href="https://dnr.wi.gov/topic/Groundwater/grn.html","Groundwater Retrieval Network"),"and has 
                      been aggregated here to better visualize nitrate levels and long-term nitrate trends in Wisconsin's groundwater."),
                              p("While these data provide a long-term dataset, it is important to consider that public water wells 
                    are often not as representative of groundwater quality in more rural parts of counties.  In addition, municipal wells may be drilled deeper and often are not reflective
                    of nitrate concentrations in the shallow groundwater."),
                              p("These data provide valuable information about groundwater quality, however there are many areas that are underrepresented by this dataset.
                      Counties may need additional strategies to better understand water quality trends countywide, particularly in the more rural areas of the county where private wells predomoninantly rely on groundwater."),
                              br(),
                              p(strong("Municipal Community (MC)")," - water systems with 15 or more service connections, or serve a community of at least 25 residents 
                      for at least 6 months of the year. MC systems are owned by a city, town, village, or other 
                      government entity."),
                              br(),
                              p(strong("Other-than-municipal community (OTM)")," - water systems have 15 or more service connections, or serve a community of at least 25 residents 
                      for at least 6 months of the year, but are not owned by municipalities. OTM systems include mobile 
                      home parks, subdivisions, apartment buildings and condominium complexes."),
                              br(),
                              p(strong("Trasient Non-Community Wells (TN)")," - systems serve at least 25 people, but not necessarily the same people, for 60 days a year or more. 
                      TN systems include motels, restaurants, taverns, campgrounds, parks and gas stations."),
                              br(),
                              p(strong("Non-transient non-community (NN)")," - water systems serve at least 25 of the same people for at least 6 months of the year. NN systems 
                      include schools, day care centers, factories, or businesses with 25 or more employees."),
                              br(),
                              h4(strong("Data Clean-up")),
                              p(strong("Well Selection")),
                              p("The Groundwater Retrieval Network represents data from all known wells, including wells that may no longer be in use.
                      Wells without a sample collected in the past 6 years were 
                      excluded from the analysis to reduce the amount of data from public water supply wells that may no longer be in use."),
                              br(),
                              p(strong("Annual Nitrate Value")),
                              p("Some public water supply systems are sampled more than once per year.  Others may also have treated samples represented in the original 
                      dataset.  To account for these issues, only the maximum nitrate value for each calendar year was selected for use in the trend analysis of individual wells and the statewide/county summaries."),
                              br()   
                     )
                   )
      )
    )
  ),
  server = function(input, output, session) {
    
    
    #create base map with no layers
    output$map <- renderLeaflet({
      leaflet() %>% 
        addProviderTiles("Stamen.TonerLite", group = "Toner Lite", 
                         # set the maximum zoom level of the map - should meet the requirment of not showing exact location
                         options = providerTileOptions(minZoom = 2, maxZoom = 10)) %>% 
        # set the default zoom level by fitting bounds to the spatial extent of the state
        fitBounds(lng1 = -91.9, lat1 = 43.45, lng2 = -87.7, lat2 = 46.1) 
    })
    
    
    # add layers based on user selections
    observeEvent({ # observer - updates map when either of the input selections are changed
      input$var
      input$type
    }, {
      
      # create a vector of the values that will be used to color the polygons or circles
      if (input$type == "county") {
        fill.vals <- counties@data[, which(colnames(counties@data) == input$var)] # select the column
        
      } else if (input$type == "point"){
        fill.vals <- well.trends2[, which(colnames(well.trends2) == input$var)] # select the column
        fill.vals <- as.vector(as.data.frame(fill.vals)[,1])
      }
      
      
      # set color palette based on selection
      max.val <- max(abs(fill.vals), na.rm = T)
      
      if (input$var == "Nitrate.mean") { 
        
        if (input$type == "point"){
          mbreaks <- c(0, 1, 2.0, 5, 10, max.val)
          
        }else { #input$var == 'county'
          mbreaks <- c(0, 1, 2.0, 5, max.val)
        }
        
        clrs <- brewer.pal(length(mbreaks) - 1, "Reds")
        pal <- colorBin(palette = clrs, domain = fill.vals, bins = mbreaks)
        
      } else if (input$var =="Nitrate.trend") {
        
        
        if (input$type == "point"){
          mbreaks <- c(0, 1, 2.0, 5, 10, max.val)
          
          pal <- colorFactor(palette = c("#08519c", "#6baed6", "grey", "pink", "#fb8072"),
                             domain = fill.vals,
                             levels = c("-2", "-1", "0", "1", "2"))
          
        }else { #input$var == 'county'
          mbreaks <- c(0, 1, 2.0, 5, max.val)
          
          pal <- colorFactor(palette = c("white", "white", "white", "white", "white"),
                             domain = fill.vals,
                             levels = c("-2", "-1", "0", "1", "2"))
        }
      } else if (input$var == "Positive.trend") {
        
        if (input$type == "point"){
          mbreaks <- c(0, 1, 2.0, 5, 10, max.val)
          
          pal <- colorFactor(palette = c("white", "white", "white", "pink", "#fb8072"),
                             domain = fill.vals,
                             levels = c("-2", "-1", "0", "1", "2"))
          
        }else {
          #mbreaks <- c(0, .02, .05, .1, .2, .3, .4, .5, max.val)  
          mbreaks <- c(0, .02, .05, .1, .2, max.val) 
          
          clrs <- brewer.pal(length(mbreaks) - 1, "Oranges")
          pal <- colorBin(palette = clrs, domain = fill.vals, bins = mbreaks)
        }
        
        
        
      } else {  #Negative.trend
        
        if (input$type == "point"){
          mbreaks <- c(0, 1, 2.0, 5, 10, max.val)
          
          pal <- colorFactor(palette = c("#08519c", "#6baed6", "white", "white", "white"),
                             domain = fill.vals,
                             levels = c("-2", "-1", "0", "1", "2"))
          
        }else {  #Negative.trend
          #mbreaks <- c(0, .02, .05, .1, .2, .3, .4, .5, max.val)     
          mbreaks <- c(0, .02, .05, .1, .2, .3) 
          
          clrs <- brewer.pal(length(mbreaks) - 1, "Blues")
          pal <- colorBin(palette = clrs, domain = fill.vals, bins = mbreaks)
        }
        
        
        
      }
      
      # add the layer to the base map using leafletProxy
      
      if (input$type == "point") { # for the point data - use addCircleMarkers
        
        if(input$var == "Nitrate.mean"){
          
          proxy <- leafletProxy("map") # set which map is the proxy, here there is only 1 but there could be several
          proxy %>%
            clearShapes() %>% # clear previous layer
            clearMarkers() %>% # clear previous markers
            clearControls() %>% # clear previous legend
            # add circles for lat, long point data - using USGS gauging stns here, but would be similar for wells
            addPolygons(data = counties,
                        layerId = as.vector(counties@data$COUNTY_FIP),
                        stroke = T, color = "black",
                        smoothFactor = 1, weight = 1, fill = FALSE) %>%
            addCircleMarkers(data = well.trends2, lng = well.trends2$Longitude, lat = well.trends2$Latitude,
                             layerId = as.vector(well.trends2$WI.unique.well..),
                             popup = paste0(well.trends2$WI.unique.well..,
                                            "<br>Most Recent Nitrate-Nitrogen Level (mg/L): ", round(well.trends2$Nitrate.mean, 1),
                                            "<br>Most Recent Sample Date: ", well.trends2$Recent,
                                            "<br>Maximum Nitrate-Nitrogen Level (mg/L): ", round(well.trends2$m, 1),
                                            "<br>Minimum Nitrate-Nitrogen Level (mg/L): ", round(well.trends2$min, 1),
                                            "<br>Number of samples: ", well.trends2$n),
                             stroke = T, color = "black", weight = 0.75, radius = 3.5,
                             fillOpacity = 0.9, fillColor = ~pal(fill.vals)) %>% 
            addLegend("bottomleft", colors = c("#fee5d9", "#fcae91 ", "#fb6a4a", "#de2d26", "#a50f15"), #pal = pal, values = fill.vals,  
                      labels = c("Less than 1.0",  "1.0 - 2.0",  "2.0 - 5.0", "5.0 - 10.0", "Greater than 10.0"),
                      title = paste("Nitrate-Nitrogen (mg/L)"),
                      opacity = 1)
          
          
        }else if (input$var == "Nitrate.trend") {
          
          proxy <- leafletProxy("map") # set which map is the proxy, here there is only 1 but there could be several
          proxy %>%
            clearShapes() %>% # clear previous layer
            clearMarkers() %>% # clear previous markers
            clearControls() %>% # clear previous legend
            # add circles for lat, long point data - using USGS gauging stns here, but would be similar for wells
            addPolygons(data = counties,
                        # layerId = as.vector(counties@data$COUNTY_FIP),
                        stroke = T, color = "black",
                        smoothFactor = 1, weight = 1, fill = FALSE) %>%
            addCircleMarkers(data = well.trends2, lng = well.trends2$Longitude, lat = well.trends2$Latitude,
                             # layerId = as.vector(well.trends2$WI.unique.well..),
                             popup = paste0(well.trends2$WI.unique.well..,
                                            "<br>Most Recent Nitrate-Nitrogen Level (mg/L): ", round(well.trends2$Nitrate.mean, 1),
                                            "<br>Most Recent Sample Date: ", well.trends2$Recent,
                                            "<br>Maximum Nitrate-Nitrogen Level (mg/L): ", round(well.trends2$m, 1),
                                            "<br>Minimum Nitrate-Nitrogen Level (mg/L): ", round(well.trends2$min, 1),
                                            "<br>Number of samples: ", well.trends2$n,
                                            "<br>Rate of Change (mg/L per decade): ", ifelse(well.trends2$sig_change == "no sig change", "No trend", round(well.trends2$change, 2))),
                             stroke = T, color = "black", weight = 0.75, 
                             radius = ifelse(well.trends2$sig_change == "no sig change", 3.0, (4.0 + abs(well.trends2$change))),
                             fillOpacity = 5.0, fillColor = ~pal(fill.vals)) %>%
            addLegend("bottomleft", colors = c("#08519c", "#6baed6", "grey", "pink", "#fb8072"),
                      labels = c("Significant Decrease",  "Slight Decrease",  "No Significant Change",
                                 "Slight Increase", "Significant Increase"),
                      title = paste("Nitrate-Nitrogen Change"),
                      opacity = 2)
          
          
        }else if (input$var == "Positive.trend") {
          
          proxy <- leafletProxy("map") # set which map is the proxy, here there is only 1 but there could be several
          proxy %>%
            clearShapes() %>% # clear previous layer
            clearMarkers() %>% # clear previous markers
            clearControls() %>% # clear previous legend
            # add circles for lat, long point data - using USGS gauging stns here, but would be similar for wells
            addPolygons(data = counties,
                        layerId = as.vector(counties@data$COUNTY_FIP),
                        stroke = T, color = "black",
                        smoothFactor = 1, weight = 1, fill = FALSE) %>%
            addCircleMarkers(data = well.trends2, lng = well.trends2$Longitude, lat = well.trends2$Latitude,
                             layerId = as.vector(well.trends2$WI.unique.well..),
                             popup = paste0(well.trends2$WI.unique.well..,
                                            "<br>Most Recent Nitrate-Nitrogen Level (mg/L): ", round(well.trends2$Nitrate.mean, 1),
                                            "<br>Most Recent Sample Date: ", well.trends2$Recent,
                                            "<br>Number of samples: ", well.trends2$n,
                                            "<br>Rate of Change (mg/L per decade): ", ifelse(well.trends2$sig_change == "no sig change", "No trend", round(well.trends2$change, 2))),
                             stroke = T, 
                             color = "black", 
                             weight = 0.75,
                             radius = ifelse(well.trends2$sig_change == "no sig change", 3.0, 
                                      ifelse(well.trends2$sig_change == "small neg", 3.0,
                                      ifelse(well.trends2$sig_change == "big neg", 3.0,
                                            (4.0 + abs(well.trends2$change))))),
                             fillOpacity = 1.0, fillColor = ~pal(fill.vals)) %>% 
            addLegend("bottomleft", colors = c("pink", "#fb8072"),
                      labels = c("Slight Increase", "Significant Increase"),
                      title = paste("Nitrate-Nitrogen Change"),
                      opacity = 1)
          
          
        }else {   #(input$var == "Negative.trend") {
          
          proxy <- leafletProxy("map") # set which map is the proxy, here there is only 1 but there could be several
          proxy %>%
            clearShapes() %>% # clear previous layer
            clearMarkers() %>% # clear previous markers
            clearControls() %>% # clear previous legend
            # add circles for lat, long point data - using USGS gauging stns here, but would be similar for wells
            addPolygons(data = counties,
                        layerId = as.vector(counties@data$COUNTY_FIP),
                        stroke = T, color = "black",
                        smoothFactor = 1, weight = 1, fill = FALSE) %>%
            addCircleMarkers(data = well.trends2, lng = well.trends2$Longitude, lat = well.trends2$Latitude,
                             layerId = as.vector(well.trends2$WI.unique.well..),
                             popup = paste0(well.trends2$WI.unique.well..,
                                            "<br>Most Recent Nitrate-Nitrogen Level (mg/L): ", round(well.trends2$Nitrate.mean, 1),
                                            "<br>Most Recent Sample Date: ", well.trends2$Recent,
                                            "<br>Number of samples: ", well.trends2$n,
                                            "<br>Rate of Change (mg/L per decade): ", ifelse(well.trends2$sig_change == "no sig change", "No trend", round(well.trends2$change, 2))),
                             stroke = T, 
                             color = "black", 
                             weight = 0.75,
                             radius = ifelse(well.trends2$sig_change == "no sig change", 3.0, 
                                      ifelse(well.trends2$sig_change == "small pos", 3.0,
                                      ifelse(well.trends2$sig_change == "big pos", 3.0,
                                            (4.0 + abs(well.trends2$change))))),
                             fillOpacity = 1.0, fillColor = ~pal(fill.vals)) %>% 
            addLegend("bottomleft", colors = c("#08519c", "#6baed6"),
                      labels = c("Significant Decrease","Slight Decrease"),
                      title = paste("Nitrate-Nitrogen Change"),
                      opacity = 1)
          
        }
        
      } else { # for the other option (counties) - use addPolygons #County
        
        if(input$var == "Nitrate.mean"){
          
          proxy <- leafletProxy("map")
          proxy %>%
            clearShapes() %>% # clear previous polygons
            clearMarkers() %>% # clear previous markers
            clearControls() %>% # clear previous legend
            addPolygons(data = counties,
                        layerId = as.vector(counties@data$COUNTY_FIP),
                        stroke = T, color = "black",
                        smoothFactor = 1, weight = 1,
                        fillColor = ~pal(fill.vals), fillOpacity = 0.9,
                        popup = paste0(counties@data$COUNTY_NAM, " County",
                                       "<br>Mean Nitrate-Nitrogen Level(mg/L): ", round(counties@data$Nitrate.mean, 1),
                                       "<br>Number of Wells: ", counties@data$wells.n),
                        highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>% 
            addLegend("bottomleft", title = paste("Nitrate-Nitrogen Mean (mg/L)"), 
                      colors = c("#fee5d9", "#fcae91 ", "#fb6a4a", "#a50f15", "#969696"),  
                      labels = c("Less than 1.0",  "1.0 - 2.0",  "2.0 - 5.0", "Greater than 5", "Not Available"),
                      opacity = 1)
          
          
          
        }else if (input$var == "Nitrate.trend") {
          
          proxy <- leafletProxy("map")
          proxy %>%
            clearShapes() %>% # clear previous polygons
            clearMarkers() %>% # clear previous markers
            clearControls() %>% # clear previous legend
            addPolygons(data = counties,
                        layerId = as.vector(counties@data$COUNTY_FIP),
                        stroke = T, color = "black",
                        smoothFactor = 1, weight = 1,
                        fillColor = ~pal(fill.vals), fillOpacity = 0.9,
                        popup = paste0(counties@data$COUNTY_NAM, " County",
                                       "<br>Mean Nitrate Level (mg/L): ", round(counties@data$Nitrate.mean, 1),
                                       "<br>Number of Wells: ", counties@data$wells.n,
                                       "<br>Significant Decrease: ", counties@data$nn2,
                                       "<br>Slight Decrease: ", counties@data$nn1,
                                       "<br>No Significant Change: ", counties@data$n0,
                                       "<br>Slight Increase: ", counties@data$np1,
                                       "<br>Significant Increase: ", counties@data$np2,
                                       "<br>Wells Increasing (%): ", round(counties@data$Positive.trend*100, 1),
                                       "<br>Wells Decreasing (%): ", round(counties@data$Negative.trend*100, 1),
                                       "<br>Wells No Trend (%): ", round(counties@data$No.trend*100, 1)),
                        highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>% 
            addLegend("bottomleft", title = "Coming Soon",
                      colors = c("white"),
                      labels = c("Working on County Trend Representation"),
                      opacity = 1) 
          # addLegend("bottomleft", colors = c("#08519c", "#6baed6", "grey", "pink", "#fb8072"),
          #           labels = c("Likely Decrease",  "Possible Decrease",  "No Significant Change",
          #                      "Possible Increase", "Likely Increase"),
          #           opacity = 1) 
          
        }else if (input$var == "Positive.trend"){
          
          proxy <- leafletProxy("map")
          proxy %>%
            clearShapes() %>% # clear previous polygons
            clearMarkers() %>% # clear previous markers
            clearControls() %>% # clear previous legend
            addPolygons(data = counties,
                        layerId = as.vector(counties@data$COUNTY_FIP),
                        stroke = T, color = "black",
                        smoothFactor = 1, weight = 1,
                        fillColor = ~pal(fill.vals), fillOpacity = 0.9,
                        popup = paste0(counties@data$COUNTY_NAM, " County",
                                       "<br>Mean Nitrate Level (mg/L): ", round(counties@data$Nitrate.mean, 1),
                                       "<br>Number of Wells: ", counties@data$wells.n,
                                       "<br>Significant Decrease: ", counties@data$nn2,
                                       "<br>Slight Decrease: ", counties@data$nn1,
                                       "<br>No Significant Change: ", counties@data$n0,
                                       "<br>Slight Increase: ", counties@data$np1,
                                       "<br>Significant Increase: ", counties@data$np2,
                                       "<br>Wells Increasing (%): ", round(counties@data$Positive.trend*100, 1),
                                       "<br>Wells Decreasing (%): ", round(counties@data$Negative.trend*100, 1),
                                       "<br>Wells No Trend (%): ", round(counties@data$No.trend*100, 1)),
                        highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>%
            addLegend("bottomleft", title = "Percent of Wells Increasing",
                      colors = brewer.pal(5, "Oranges"), #c("#08519c", "#6baed6", "grey", "pink", "#fb8072"),
                      labels = c("Less than 2%",  "2-5%",  "5-10%",
                                 "10-20%", "Greater than 20%"),
                      opacity = 1)
          
        }else { #(input$var == "Negative.Trend")
          
          proxy <- leafletProxy("map")
          proxy %>%
            clearShapes() %>% # clear previous polygons
            clearMarkers() %>% # clear previous markers
            clearControls() %>% # clear previous legend
            addPolygons(data = counties,
                        layerId = as.vector(counties@data$COUNTY_FIP),
                        stroke = T, color = "black",
                        smoothFactor = 1, weight = 1,
                        fillColor = ~pal(fill.vals), fillOpacity = 0.9,
                        popup = paste0(counties@data$COUNTY_NAM, " County",
                                       "<br>Mean Nitrate Level (mg/L): ", round(counties@data$Nitrate.mean, 1),
                                       "<br>Number of Wells: ", counties@data$wells.n,
                                       "<br>Significant Decrease: ", counties@data$nn2,
                                       "<br>Slight Decrease: ", counties@data$nn1,
                                       "<br>No Significant Change: ", counties@data$n0,
                                       "<br>Slight Increase: ", counties@data$np1,
                                       "<br>Significant Increase: ", counties@data$np2,
                                       "<br>Wells Increasing (%): ", round(counties@data$Positive.trend*100, 1),
                                       "<br>Wells Decreasing (%): ", round(counties@data$Negative.trend*100, 1),
                                       "<br>Wells No Trend (%): ", round(counties@data$No.trend*100, 1)),
                        highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>%
            addLegend("bottomleft",  title = paste("Percent of Wells Decreasing"),
                      colors = brewer.pal(4, "Blues"), #c("#08519c", "#6baed6", "grey", "pink", "#fb8072"),
                      labels = c("Less than 2%",  "2-5%",  "5-10%",
                                 "Greater than 10%"),
                      opacity = 1)
        }
      }
    }
    
    )
    
    
    
    
    output$line_WUWN <- renderPlotly({
      
      p4 <- {
        
        #Subsetting
        dfclean5 <- subset(wells.unique.bp, WI.unique.well..==input$r)
        
        #Regression
        lm_eqn = function(dfclean5){
          m = lm(Result.amount ~ YEAR, dfclean5);
          eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2*","~~italic(p-value)~"="~pvalue,
                           list(a = format(coef(m)[1], digits = 2),
                                b = format(coef(m)[2], digits = 2),
                                r2 = format(summary(m)$r.squared, digits = 3),
                                fstat = format(summary(m)$fstatistic[1], digits = 3),
                                pval <- pf(summary(m)$fstatistic[1], summary(m)$fstatistic[2], summary(m)$fstatistic[3],
                                           lower.tail = FALSE),
                                pvalue = format(pval, digits = 2)))
          as.character(as.expression(eq));
        }
        
        eq <- ddply(dfclean5, .(WI.unique.well..), lm_eqn)
        
        
        #plot
        ggplot(dfclean5, aes(x = YEAR, y = Result.amount)) +
          geom_point(size = 3, color = "black") +
          geom_smooth(method = "lm", formula = y~x) +
          #ylim(0,15) +
          geom_hline(yintercept = 10 , linetype = "dashed" , color = "red") +
          xlab("Year") +
          scale_x_continuous(breaks = seq(min(dfclean5$YEAR), max(dfclean5$YEAR), by = 1)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Nitrate-Nitrogen Concentration (mg/L)") +
          labs(title="")
      }
      ggplotly(p4)
    })
    
    
    
    
    output$select_county <- renderPlotly({
      
      
      pdf <- {
        
        #Subsetting
        #dfclean4 <- subset(wells.unique.bp2 , County.name==input$l)
        dfclean4 <- subset(wells.unique.bp0 , County.name==input$l)
        dfclean4$`Average Concentration(mg/L)` <- dfclean4$Result.amount
        colnames(dfclean4)[which(names(dfclean4) == "YEAR")] <- "Year"
        
        #Box plot of annual nitrate concentrations by county
        ggplot(dfclean4, aes(x = Year, y = `Average Concentration(mg/L)`, group = Year)) +
          geom_boxplot()+
          stat_summary(fun.y=mean, colour="blue", geom="point",
                       shape=18, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = "")) +
          geom_hline(yintercept = 10 , linetype = "dashed" , color = "red") +
          xlab("Year") +
          scale_x_continuous(limits = c((min(dfclean4$Year)), (max(dfclean4$Year))), breaks = seq((min(dfclean4$Year)), (max(dfclean4$Year)), by = 1)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Nitrate-Nitrogen Concentration (mg/L)")
      }
      
      ggplotly(pdf, tooltip = c("x", "y")) %>%
        style(hoverinfo = "none", traces = c(3))
    })
    
    cols2 = c("Significant Decrease" = "#08519c",
              "Significant Increase" = "#fb8072",
              "No Significant Change" = "grey",
              "Slight Decrease" = "#6baed6",
              "Slight Increase" = "pink"
    )
    
    output$wi_summary <- renderPlotly({
      
      text <- paste("Nitrate Trend:", summary_stats$`Nitrate Trend`, 
                    "\nYear:", summary_stats$Year,
                    "\nMean Concentration (mg/L):", round(summary_stats$`Average Concentration (mg/L)`, 2),
                    "\nMaximum:", summary_stats$`Maximum Concentration (mg/L)`,
                    "\n75th Percentile:", summary_stats$`75th Percentile (mg/L)`,
                    "\n50th Percentile:", summary_stats$`50th Percentile (mg/L)`,
                    "\n25th Percentile:", summary_stats$`25th Percentile (mg/L)`,
                    "\nMinimum:", summary_stats$`Minimum Concentration (mg/L)`,
                    "\nNumber Of Samples:", summary_stats$`Number of Samples`)
      
      
      # Generates annual nitrate concentrations for all wells statewide with more than 20 years worth of data.
      wi_plot <- ggplot(summary_stats, aes(x = Year, y = `Average Concentration (mg/L)`, fill = `Nitrate Trend`, text=text), height = 500) + 
        ggtitle("(A)") + 
        geom_point(aes(), size = 3.0) + 
        geom_line(aes()) + 
        theme(legend.position="none") +
        scale_fill_manual(values = cols2) +
        scale_x_continuous(breaks = seq(1995, currentYear, 1)) +
        scale_y_continuous(breaks = seq(2.5, 15, 2.5)) +
        theme(plot.title = element_text(color="black", size=14, face="bold.italic"),
              panel.background = element_rect(fill='white'),
              axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
              axis.title.x = element_text(face = "bold"),
              axis.title.y = element_text(face = "bold"))
            
      
      ggplotly(wi_plot, tooltip = c("text"))
      
    })
    
    
    
    #Start of code to generate pie chart of statewide trends.
    cols = c("Significant Decrease (>2.5 mg/L per decade change)" = "#08519c",
             "Significant Increase (>2.5 mg/L per decade change)" = "#fb8072",
             "No Significant Trend" = "grey",
             "Slight Decrease (1.0-2.5 mg/L per decade change)" = "#6baed6",
             "Slight Increase (1.0-2.5 mg/L per decade change)" = "pink")
    
    WI_stats$Trend = factor(WI_stats$Trend, levels = c("Significant Decrease (>2.5 mg/L per decade change)", 
                                                       "Slight Decrease (1.0-2.5 mg/L per decade change)", 
                                                       "No Significant Trend",
                                                       "Slight Increase (1.0-2.5 mg/L per decade change)",
                                                       "Significant Increase (>2.5 mg/L per decade change)"))
    
    output$trend_pie <- renderPlotly({
      
      text3 <- paste("")
      
      p <- ggplot(WI_stats, aes(x=Trend, y=Percent, fill=Trend)) +
        geom_bar(stat="identity") +
        theme_bw() +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
        geom_text(aes(label = paste0(Percent*100, "%"),
                      y = Percent + .03),
                  vjust = 0, size = 6) +
        scale_fill_manual(values = cols) +
        theme(legend.position="none") +
        xlab("") + ylab("") +
        theme(
              panel.background = element_rect(fill='white'),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              # axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank() 
        ) #+
      # theme_nothing()
      
      ggplotly(p, tooltip = c("text3")) 
      
    })
    
    
    output$wi_summary2 <- renderPlotly({
      
      text <- paste("Nitrate Trend:", summary_stats2$`Nitrate Trend`, 
                    "\nYear:", summary_stats2$Year,
                    "\nMean Concentration (mg/L):", round(summary_stats2$`Average Concentration (mg/L)`, 2),
                    "\nMaximum:", summary_stats2$`Maximum Concentration (mg/L)`,
                    "\n75th Percentile:", summary_stats2$`75th Percentile (mg/L)`,
                    "\n50th Percentile:", summary_stats2$`50th Percentile (mg/L)`,
                    "\n25th Percentile:", summary_stats2$`25th Percentile (mg/L)`,
                    "\nMinimum:", summary_stats2$`Minimum Concentration (mg/L)`,
                    "\nNumber Of Samples:", summary_stats2$`Number of Samples`)
      
      p <- ggplot(summary_stats2, aes(x = Year)) +
        ggtitle("(B)") +
        scale_y_continuous(breaks = seq(0, 3, 1)) +
        geom_pointrange(aes(y = `Average Concentration (mg/L)`, ymin = `25th Percentile (mg/L)`, ymax = `75th Percentile (mg/L)`,
                            text = paste("Minimum:", `Minimum Concentration (mg/L)`,
                                         "\nMaximum:", `Maximum Concentration (mg/L)`,
                                         "\nNumber of Samples:", `Number of Samples`)),
                        fill='white', color='black', shape=21, fatten = 15, size = 3) +
        scale_x_continuous(breaks = seq(1995, currentYear, 1)) + 
        theme(plot.title = element_text(color="black", size=14, face="bold.italic"),
              panel.background = element_rect(fill='white'),
              axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
              axis.title.x = element_text(face = "bold"),
              axis.title.y = element_text(face = "bold")) 
      
      ggplotly(p)
      
    })
    
    output$WUWN_info <- renderText({
      
      #Subsetting
      dfclean51 <- subset(dfwisco , WI.unique.well..==input$r)
      
      dfclean51$V1 <- str_trim(dfclean51$V1 , side = c("both"))
      dfclean51$V1 <- gsub("italic" , "" , dfclean51$V1)
      dfclean51$V1 <- gsub("\\(", "", dfclean51$V1)
      dfclean51$V1 <- gsub("\\*", "", dfclean51$V1)
      dfclean51$V1 <- gsub("\\)", "", dfclean51$V1)
      dfclean51$V1 <- gsub("\"", "", dfclean51$V1)
      dfclean51$V1 <- gsub("cYEAR =", "", dfclean51$V1)
      dfclean51$V1 <- gsub("%.%", "", dfclean51$V1)
      dfclean51$V1 <- gsub("~", "", dfclean51$V1)
      dfclean51$V1 <- gsub("== c`Intercept` ", "", dfclean51$V1)
      dfclean51$V1 <- gsub("cvalue = ", "", dfclean51$V1)
      dfclean51$V1 <- gsub("^", "", dfclean51$V1, fixed=TRUE)
      dfclean51$V1 <- gsub("r2", "R-Squared", dfclean51$V1, fixed=TRUE)
      dfclean51$V1 <- gsub("p - value ", "P-Value", dfclean51$V1)
      dfclean51$V1 <- gsub(" , ", ", ", dfclean51$V1, fixed=TRUE) 
      
      paste(dfclean51$V1)
      
      
    })
    
    #Not used by going to keep for now. If we ever add a county regression line we will want this
    output$county_info <- renderText({
      
      #Subsetting
      dfclean41 <- subset(dfcounty2 , County.name==input$l)
      
      dfclean41$V1 <- str_trim(dfclean41$V1 , side = c("both"))
      dfclean41$V1 <- gsub("italic" , "" , dfclean41$V1)
      dfclean41$V1 <- gsub("\\(", "", dfclean41$V1)
      dfclean41$V1 <- gsub("\\*", "", dfclean41$V1)
      dfclean41$V1 <- gsub("\\)", "", dfclean41$V1)
      dfclean41$V1 <- gsub("\"", "", dfclean41$V1)
      dfclean41$V1 <- gsub("cYEAR =", "", dfclean41$V1)
      dfclean41$V1 <- gsub("%.%", "", dfclean41$V1)
      dfclean41$V1 <- gsub("~", "", dfclean41$V1)
      dfclean41$V1 <- gsub("== c`Intercept` ", "", dfclean41$V1)
      dfclean41$V1 <- gsub("cvalue = ", "", dfclean41$V1)
      dfclean41$V1 <- gsub("^", "", dfclean41$V1, fixed=TRUE)
      dfclean41$V1 <- gsub("r2", "R-Squared", dfclean41$V1, fixed=TRUE)
      dfclean41$V1 <- gsub("p - value ", "P-Value", dfclean41$V1)
      dfclean41$V1 <- gsub(" , ", ", ", dfclean41$V1, fixed=TRUE) 
      
      paste(dfclean41$V1)
      
      
    })
    
    
    
  }
)

### PARENTHESIS NEEDED TO CLOSE SHINYAPP ###

# Run the application 
# shinyApp(ui , server)