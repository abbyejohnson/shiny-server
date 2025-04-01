####SERVER FUNCTIONS####

server = function(input, output, session) {
  
  
  #create base map with no layers
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles(urlTemplate = 'https://tiles.stadiamaps.com/tiles/{variant}/{z}/{x}/{y}{r}.png?api_key={apikey}',
               attribution = paste('&copy; <a href="https://www.stadiamaps.com/" target="_blank">Stadia Maps</a> ' ,
                                   '&copy; <a href="https://www.stamen.com/" target="_blank">Stamen Design</a> ' ,
                                   '&copy; <a href="https://openmaptiles.org/" target="_blank">OpenMapTiles</a> ' ,
                                   '&copy; <a href="https://www.openstreetmap.org/about" target="_blank">OpenStreetMap</a> contributors'),
                       # set the maximum zoom level of the map - should meet the requirment of not showing exact location
                       options = tileOptions(minZoom = 2, maxZoom = 10, variant='stamen_toner_lite', apikey = '21849f37-b13a-4111-8858-95d6d8902aeb')) %>% 
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
    
    if (input$var == "Chloride.mean") { #Chloride Concentration
      
      if (input$type == "point"){
        mbreaks <- c(0, 10, 25, 100, 250, max.val)
        
      }else { #input$var == 'county'
        mbreaks <- c(0, 10, 25, 50, 100, max.val)
      }
      
      clrs <- brewer.pal(length(mbreaks) - 1, "PuOr")
      pal <- colorBin(palette = clrs, domain = fill.vals, bins = mbreaks, reverse = TRUE)
      #yay it's fixed :)        
      
    } else {  # input$var = Chloride.trend
      
      pal <- colorFactor(palette = c("#08519C", "#6baed6", "grey", "pink", "#fb8072"),
                         domain = fill.vals,
                         levels = c("-2", "-1", "0", "1", "2"))
    }
    
    # add the layer to the base map using leafletProxy
    
    if (input$type == "point") { # for the point data - use addCircleMarkers
      
      if(input$var == "Chloride.mean"){
        
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
                           layerId = as.vector(well.trends2$WI.Unique.Well..),
                           popup = paste0(well.trends2$WI.Unique.Well..,
                                          "<br>Most Recent Chloride Level (mg/L): ", round(well.trends2$Chloride.mean, 2),
                                          "<br>Most Recent Sample Date: ", well.trends2$Recent,
                                          "<br>Number of samples: ", well.trends2$n),
                           stroke = T, color = "black", weight = 0.75, radius = 3.5,
                           fillOpacity = 0.9, fillColor = ~pal(fill.vals)) %>% 
          addLegend("bottomleft", colors = c("#5e3c99", "#b2abd2", "#f7f7f7", "#fdb863", "#e66101"), #pal = pal, values = fill.vals,  
                    labels = c("Less than 10.0",  "10.0 - 25.0",  "25.0 - 100.0", "100.0 - 250.0", "Greater than 250.0"),
                    title = paste("Chloride (mg/L)"),
                    opacity = 1)
        
      } else{    #input$var == "Chloride.trend"
        
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
                           layerId = as.vector(well.trends2$WI.Unique.Well..),
                           popup = paste0(well.trends2$WI.Unique.Well..,
                                          "<br>Most Recent Chloride Level (mg/L): ", round(well.trends2$Chloride.mean, 2),
                                          "<br>Most Recent Sample Date: ", well.trends2$Recent,
                                          "<br>Number of samples: ", well.trends2$n,
                                          "<br>Rate of Change (mg/L per decade): ", ifelse(well.trends2$sig_change == "no sig change", "No trend", round(well.trends2$change, 2))),
                           stroke = T, color = "black", weight = 0.75, radius = ifelse(well.trends2$sig_change == "no sig change", 3.5, (3.5 + abs(well.trends2$change/10))),
                           fillOpacity = 1.0, fillColor = ~pal(fill.vals)) %>% 
          addLegend("bottomleft", colors = c("#08519c", "#6baed6", "grey", "pink", "#fb8072"),
                    labels = c("Significant Decrease",  "Slight Decrease",  "No Significant Change",
                               "Slight Increase", "Significant Increase"),
                    title = paste("Chloride Change (mg/L per year)"),
                    opacity = 1)
      }
      
      
    } else { # for the other option (counties) - use addPolygons #County
      
      if(input$var == "Chloride.mean"){
        
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
                                     "<br>Mean Chloride Level(mg/L): ", round(counties@data$Chloride.mean, 2),
                                     "<br>Number of Wells: ", counties@data$wells.n),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>% 
          addLegend("bottomleft", title = paste("Chloride Mean (mg/L)"), 
                    colors = c("#5e3c99", "#b2abd2", "#f7f7f7", "#fdb863", "#e66101"),  
                    labels = c("Less than 10.0",  "10.0 - 25.0",  "25.0 - 100.0", "100.0 - 250.0", "Greater than 250.0"),
                    opacity = 1)
      }
      
      else{    #input$var == "Chloride.trend"
        
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
                                     "<br>Mean Chloride Level (mg/L): ", round(counties@data$Chloride.mean, 2),
                                     "<br>Number of Wells: ", counties@data$wells.n,
                                     "<br>Significant Decrease: ", counties@data$nn2,
                                     "<br>Slight Decrease: ", counties@data$nn1,
                                     "<br>No Significant Change: ", counties@data$n0,
                                     "<br>Slight Increase: ", counties@data$np1,
                                     "<br>Significant Increase: ", counties@data$np2),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>% 
          addLegend("bottomleft", colors = c("#08519c", "#6baed6", "grey", "pink", "#fb8072"),
                    labels = c("Likely Decrease",  "Possible Decrease",  "No Significant Change",
                               "Possible Increase", "Likely Increase"),
                    opacity = 1) 
      }
    }
  }
  
  )
  
  output$line_WUWN <- renderPlotly({
    #error; object 'output' not found 
    p4 <- { 
      
      #Subsetting
      dfclean5 <- subset(wells.unique.bp , WI.Unique.Well..==input$r)
      
      #Regression
      lm_eqn = function(dfclean5){
        m = lm(Sample.Analytical.Result.Amount ~ YEAR, dfclean5);
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
      
      eq <- ddply(dfclean5, .(WI.Unique.Well..), lm_eqn)
      
      
      #plot
      ggplot(dfclean5, aes(x = YEAR, y = Sample.Analytical.Result.Amount)) +
        geom_point(size = 3, color = "black") +
        geom_smooth(method = "lm", formula = y~x) +
        #ylim(0,15) +
        geom_hline(yintercept = 10 , linetype = "dashed" , color = "red") +
        xlab("Year") +
        scale_x_continuous(breaks = seq(min(dfclean5$YEAR), max(dfclean5$YEAR), by = 1)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
        ylab("Chloride Concentration (mg/L)") +
        labs(title="")
    }
    ggplotly(p4)
  })
  
  
  
  
  output$select_county <- renderPlotly({
    
    
    pdf <- {
      
      
      #Subsetting
      dfclean4 <- subset(wells.unique.bp2 , County.name==input$l)
      
      #Regression line
      lm_eqn = function(dfclean4){
        m = lm(average ~ YEAR, dfclean4);
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
      
      eq <- ddply(dfclean4, .(County.name), lm_eqn)
      
      #Plot
      ggplot(dfclean4, aes(x = YEAR, y = average)) +
        #geom_errorbar(aes(ymin=average-stdev,ymax=average+stdev)) +
        geom_text(label = dfclean4$cnt, y = -0.5) +
        geom_point(size=3, color="black") +
        geom_smooth(method = "lm", formula = y~x) +
        #ylim(-0.5,10) +
        xlab("Year") +
        scale_x_continuous(breaks = seq(min(dfclean4$YEAR), max(dfclean4$YEAR), by = 1)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
        ylab("Chloride Concentration (mg/L)") +
        labs(title="")
      
      
    }
    
    ggplotly(pdf)
  })
  
  
  
  output$WUWN_info <- renderText({
    
    #Subsetting
    dfclean51 <- subset(dfwisco , WI.Unique.Well..==input$r)
    
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
  
  output$WUWN_trend <- renderText({
    
    #Subsetting
    dfclean52 <- subset(dfwisco , WI.Unique.Well..==input$r)
    
    paste(dfclean52$shiny_trend)
  })
  
  output$county_trend <- renderText({
    
    #Subsetting
    dfclean42 <- subset(dfcounty2 , County.name==input$l)
    
    paste(dfclean42$shiny_trend)
  })
  
  
}

### PARENTHESIS NEEDED TO CLOSE SHINYAPP ###

# Run the application 
# shinyApp(ui , server)