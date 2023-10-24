

library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(plotly)
library(leaflet)
library(shinythemes)
library(sp)
library(stringr)



# create a data-processing file that is run whenever the data is updated
# within the file, write the data to a file

# Change date to current date when modifying with new data
modified <- "July 6, 2020"

#call in logos for branding app
uwsp <- tags$a(href="https://www.uwsp.edu/cnr-ap/watershed/Pages/default.aspx", 
               tags$img(src='uwsp.png', height = '75', width = '325', align = 'center'))
uwex <- tags$a(href="https://extension.wisc.edu/", 
               tags$img(src='uwex.png', height = '75', width = '275', align = 'center'))


# I've removed most of the pre-processing steps and saved the final files that get used
well.trends2 <- readRDS("well.trends2.rds")
dfwisco <- readRDS("dfwisco.rds")
dfcounty2 <- readRDS("dfcounty2.rds")
counties <- readRDS("WIcounties.rds")
wells.unique.bp <- readRDS("wells.unique.bp.rds")
wi_stats <- readRDS("wi_stats.rds")

wells.unique.bp <- wells.unique.bp[order(wells.unique.bp$WI.unique.well..),]

wi_stats$pct <- round(wi_stats$pct, 1)
colnames(wi_stats) <- c("Trend", "Count", "Percent")

# subset to the wells with more than 20 years of data
wells.unique.bp0 <- wells.unique.bp[!(wells.unique.bp$k < 20),]

# summarize by year and county
wells.unique.bp2 <- wells.unique.bp0 %>% 
  dplyr::group_by(YEAR , County.name) %>% 
  dplyr::summarise(average = mean(Result.amount), 
                   stdev = sd(Result.amount), 
                   med = median(Result.amount), 
                   q_range = IQR(Result.amount), 
                   cnt = n())


ui <- fluidPage(
  
  # App title ----
  titlePanel(strong("Nitrate in Wisconsin's Public Water Systems")), 
  
  ###START NAVIGATION BAR###
  
  ###CONTROLS BACKGROUND THEME###
  theme = shinytheme('flatly'), # <--- To not use a theme, comment this
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
                       leafletOutput("map", height = 620)
                )
              ),
              br(),
              div(strong("Created by: Grant Moser, Jennifer Dierauer, and Kevin Masarik")),
              div("Center for Watershed Science and Education"), #in partnership with the Wisconsin Department of Natural Resources"),
              div("Last modified:", modified,'.', a(href="mailto:kmasarik@uwsp.edu","Contact Kevin Masarik for questions")),
              div(uwsp,uwex)
    ),
    sidebarPanel(width = 6)
  )
)





server <- function(input, output, session) {
  
  
  #create base map with no layers
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("Stamen.TonerLite", group = "Toner Lite", 
                       # set the maximum zoom level of the map - should meet the requirment of not showing exact location
                       options = providerTileOptions(minZoom = 2, maxZoom = 10)) %>% 
      # set the default zoom level by fitting bounds to the spatial extent of the state
      fitBounds(lng1 = -92.9, lat1 = 42.45, lng2 = -86.7, lat2 = 47.1) 
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
        
      } else {  #Negative.trend
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
                      #layerId = as.vector(counties@data$COUNTY_FIP),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = well.trends2, lng = well.trends2$Longitude, lat = well.trends2$Latitude,
                           #layerId = as.vector(well.trends2$WI.unique.well..),
                           popup = paste0(well.trends2$WI.unique.well..,
                                          "<br>Most Recent Nitrate-Nitrogen Level (mg/L): ", round(well.trends2$Nitrate.mean, 1),
                                          "<br>Most Recent Sample Date: ", well.trends2$Recent,
                                          "<br>Maximum Nitrate-Nitrogen Level (mg/L): ", round(well.trends2$m, 1),
                                          "<br>Minimum Nitrate-Nitrogen Level (mg/L): ", round(well.trends2$min, 1),
                                          "<br>Number of samples: ", well.trends2$n,
                                          "<br>Rate of Change (mg/L per decade): ", ifelse(well.trends2$sig_change == "no sig change", "No trend", round(well.trends2$change, 2))),
                           stroke = T, color = "black", weight = 0.75, 
                           radius = ifelse(well.trends2$sig_change == "no sig change", 1.5, (1.5 + abs(well.trends2$change))),
                           fillOpacity = 1.0, fillColor = ~pal(fill.vals)) %>% 
          addLegend("bottomleft", colors = c("#08519c", "#6baed6", "grey", "pink", "#fb8072"),
                    labels = c("Significant Decrease",  "Slight Decrease",  "No Significant Change",
                               "Slight Increase", "Significant Increase"),
                    title = paste("Nitrate-Nitrogen Change"),
                    opacity = 1)
        
        
      }else if (input$var == "Positive.trend") {
        
        proxy <- leafletProxy("map") # set which map is the proxy, here there is only 1 but there could be several
        proxy %>%
          clearShapes() %>% # clear previous layer
          clearMarkers() %>% # clear previous markers
          clearControls() %>% # clear previous legend
          # add circles for lat, long point data - using USGS gauging stns here, but would be similar for wells
          addPolygons(data = counties,
                      #layerId = as.vector(counties@data$COUNTY_FIP),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = well.trends2, lng = well.trends2$Longitude, lat = well.trends2$Latitude,
                           #layerId = as.vector(well.trends2$WI.unique.well..),
                           popup = paste0(well.trends2$WI.unique.well..,
                                          "<br>Most Recent Nitrate-Nitrogen Level (mg/L): ", round(well.trends2$Nitrate.mean, 1),
                                          "<br>Most Recent Sample Date: ", well.trends2$Recent,
                                          "<br>Number of samples: ", well.trends2$n,
                                          "<br>Rate of Change (mg/L per decade): ", ifelse(well.trends2$sig_change == "no sig change", "No trend", round(well.trends2$change, 2))),
                           stroke = T, 
                           color = "black", 
                           weight = 0.75, 
                           radius = ifelse(well.trends2$sig_change == "no sig change", 1, 3.5),
                           fillOpacity = 1.0, fillColor = ~pal(fill.vals)) %>% 
          addLegend("bottomleft", colors = c("pink", "#fb8072"),
                    labels = c("Slight Increase", "Significant Increase"),
                    title = paste("Nitrate-Nitrogen Change"),
                    opacity = 1)
        
        
      } else {   #(input$var == "Negative.trend") {
        
        proxy <- leafletProxy("map") # set which map is the proxy, here there is only 1 but there could be several
        proxy %>%
          clearShapes() %>% # clear previous layer
          clearMarkers() %>% # clear previous markers
          clearControls() %>% # clear previous legend
          # add circles for lat, long point data - using USGS gauging stns here, but would be similar for wells
          addPolygons(data = counties,
                      #layerId = as.vector(counties@data$COUNTY_FIP),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = well.trends2, lng = well.trends2$Longitude, lat = well.trends2$Latitude,
                           layerId = as.vector(well.trends2$WI.unique.well..),
                           popup = paste0(well.trends2$WI.unique.well..,
                                          "<br>Most Recent Nitrate-Nitrogen Level (mg/L): ", round(well.trends2$Nitrate.mean, 1),
                                          "<br>Most Recent Sample Date: ", well.trends2$Recent,
                                          "<br>Number of samples: ", well.trends2$n,
                                          "<br>Rate of Change (mg/L per decade): ", ifelse(well.trends2$sig_change == "no sig change", "No trend", round(well.trends2$change, 2))),
                           stroke = T, color = "black", 
                           weight = 0.75, radius = ifelse(well.trends2$sig_change == "no sig change", 1, 3.5),
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
                      #layerId = as.vector(counties@data$COUNTY_FIP),
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
        
        
        
      } else if (input$var == "Nitrate.trend") {
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>% # clear previous polygons
          clearMarkers() %>% # clear previous markers
          clearControls() %>% # clear previous legend
          addPolygons(data = counties,
                      #layerId = as.vector(counties@data$COUNTY_FIP),
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
                      #layerId = as.vector(counties@data$COUNTY_FIP),
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
                      #layerId = as.vector(counties@data$COUNTY_FIP),
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
  
}


shinyApp(ui, server)