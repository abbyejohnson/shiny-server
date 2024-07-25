
####SERVER FUNCTIONS####----------------------------------------------------------------------------------------------------

server = function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addTiles(urlTemplate = 'https://tiles.stadiamaps.com/tiles/{variant}/{z}/{x}/{y}{r}.png?api_key={apikey}',
               attribution = paste('&copy; <a href="https://www.stadiamaps.com/" target="_blank">Stadia Maps</a> ' ,
                                   '&copy; <a href="https://www.stamen.com/" target="_blank">Stamen Design</a> ' ,
                                   '&copy; <a href="https://openmaptiles.org/" target="_blank">OpenMapTiles</a> ' ,
                                   '&copy; <a href="https://www.openstreetmap.org/about" target="_blank">OpenStreetMap</a> contributors'),
                       options = tileOptions(minZoom = 5, maxZoom = 11, variant='stamen_toner_lite', apikey = '21849f37-b13a-4111-8858-95d6d8902aeb')) %>% 
      fitBounds(lng1 = min(df$LONGITUDE, na.rm = TRUE), lat1 = min(df$LATITUDE, na.rm = TRUE), lng2 = max(df$LONGITUDE, na.rm = TRUE), lat2 = max(df$LATITUDE, na.rm = TRUE))
      #fitBounds(lng1 = -92.155801, lat1 = 42.210618, lng2 = -91.543313, lat2 = 45.642400)
  })
  
  observeEvent({
    input$year
    input$var
    input$type
  }, 
  
  {
    if(input$year == "2023") {
      df1 <- df[df$YEAR == 2023,]
      munidf <- munidf[munidf$YEAR == 2023,]
    # }else if(input$year == "2023") {
    #   df1 <- df[df$YEAR == 2023,]
    #   munidf <- munidf[munidf$YEAR == 2023,]
    }else{
      df1 <- df[df$YEAR == 2023,]
      munidf <- munidf[munidf$YEAR == 2023,]
      # df1 <- df[df$YEAR == c(2022,2023),]
      # munidf <- munidf[munidf$YEAR == c(2022,2023),]
    }
    
    
    
    
    
    if (input$type == "Individual Wells") {
      fill.vals <- df1[,which(colnames(df1) == input$var)]
      fill.vals <- as.vector(as.data.frame(fill.vals)[,1])
      
    }else { #(input$type == "Municipality") {
      shape@data <- left_join(shape@data, munidf, by = "Municipality")
      fill.vals <- shape@data[, which(colnames(shape@data) == input$var)]
    }
    
    #max.val <- max(abs(fill.vals), na.rm = T)
    max.val <- ifelse(is.numeric(fill.vals),max(abs(fill.vals), na.rm = T),0)

    
    
    if(input$var == "Alkalinity"){
      mbreaks <- c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550)
      
      clrs <- brewer.pal(length(mbreaks), "PuOr")
      pal <- colorBin(palette = clrs, domain = fill.vals, bins = mbreaks)
      
      
    }else if(input$var == "Chloride"){
      mbreaks1 <- c(0, 10, 20, 50, 100, max.val)
      mbreaks2 <- seq(0, (max.val+6), by = 6)
      
      clrs1 <- brewer.pal(length(mbreaks1), "Oranges")
      clrs2 <- brewer.pal(length(mbreaks2), "Oranges")
      pal1 <- colorBin(palette = clrs1, domain = fill.vals, bins = mbreaks1)
      pal2 <- colorBin(palette = clrs2, domain = fill.vals, bins = mbreaks2)
      
    }else if(input$var == "Conductivity"){
      mbreaks1 <- c(0, 200, 300, 400, 500, 600, 700, 800, 900, 1000, max.val)
      mbreaks2 <- seq(0, (max.val+100), by = 100)
      
      clrs1 <- rev(brewer.pal(length(mbreaks1), "Spectral"))
      clrs2 <- rev(brewer.pal(length(mbreaks2), "Spectral"))
      pal1<- colorBin(palette = clrs1, domain = fill.vals, bins = mbreaks1)
      pal2 <- colorBin(palette = clrs2, domain = fill.vals, bins = mbreaks2)
      
      
    }else if(input$var == "Nitrate"){
      mbreaks <- c(0, 1, 2, 5, 10, 50)
      
      clrs <- rev(brewer.pal(length(mbreaks), "RdYlBu"))
      pal <- colorBin(palette = clrs, domain = fill.vals, bins = mbreaks)
      
    # }else if(input$var == "nitrate_direction"){
    #   mbreaks <- c(-10, -5.0, -2.50, -1.0, 1.0, 2.5, 5, 10)
    #   
    #   clrs <- rev(brewer.pal(length(mbreaks), "RdBu"))
    #   pal <- colorBin(palette = clrs, domain = fill.vals, bins = mbreaks)
    #   
      
      }else if(input$var == "Nitrate_sd"){
        mbreaks <- c(0, 0.1, 0.5, 1, 2, 3, 4, 5)

        clrs <- rev(brewer.pal(length(mbreaks), "RdYlBu"))
        pal <- colorBin(palette = clrs, domain = fill.vals, bins = mbreaks)

      
    }else if(input$var == "pH"){
      mbreaks <- c(0, 6.5, 7.0, 7.5, 8.0, 8.5, 9.0)
      clrs <- brewer.pal(length(mbreaks), "PRGn")
      pal <- colorBin(palette = clrs, domain = fill.vals, bins = mbreaks)
      
    }else if(input$var == "Arsenic"){
      #mbreaks1 <- c(0, 0.005, 0.006, 0.007, 0.008, 0.009)
      mbreaks1 <- c(0, 0.005, 0.007, 0.010, 0.015, 0.025, 0.05, 0.10)
      mbreaks2 <- c(0, 0.005, 0.006, 0.007, 0.008, 0.009, 0.010, 0.011)
      
      clrs1 <- rev(brewer.pal(length(mbreaks1), "RdYlGn"))
      clrs2 <- rev(brewer.pal(length(mbreaks2), "RdYlGn"))
      pal1 <- colorBin(palette = clrs1, domain = fill.vals, bins = mbreaks1)
      pal2 <- colorBin(palette = clrs2, domain = fill.vals, bins = mbreaks2)
      
    }else if(input$var == "Calcium"){
      #mbreaks1 <- c(0, 20, 40, 60, 80)
      mbreaks1 <- c(0, 10, 25, 50, 75, 100, 300)
      mbreaks2 <- c(0, 20, 40, 60, 80, 100)
      
      clrs1 <- rev(brewer.pal(length(mbreaks1), "RdGy"))
      clrs2 <- rev(brewer.pal(length(mbreaks2), "RdGy"))
      pal1 <- colorBin(palette = clrs1, domain = fill.vals, bins = mbreaks1)
      pal2 <- colorBin(palette = clrs2, domain = fill.vals, bins = mbreaks2)
      
    }else if(input$var == "Iron"){
      mbreaks1 <- c(0, 0.050, 0.100, 0.300, 1.0, 5.0, 18.0)
      mbreaks2 <- c(0, 0.50, 1.0, 1.50, 2.0)
      
      clrs1 <- rev(brewer.pal(length(mbreaks1), "RdBu"))
      clrs2 <- rev(brewer.pal(length(mbreaks1), "RdBu"))
      pal1 <- colorBin(palette = clrs1, domain = fill.vals, bins = mbreaks1)
      pal2 <- colorBin(palette = clrs2, domain = fill.vals, bins = mbreaks2)
      
    }else if(input$var == "Manganese"){
      mbreaks1 <- c(0, 0.025, 0.050, 0.100, 0.300, 10.0)
      mbreaks2 <- c(0, 0.05, 0.10, 0.15, 0.20, 0.25)
      
      clrs1 <- rev(brewer.pal(length(mbreaks1), "Spectral"))
      clrs2 <- rev(brewer.pal(length(mbreaks2), "Spectral"))
      pal1 <- colorBin(palette = clrs1, domain = fill.vals, bins = mbreaks1)
      pal2 <- colorBin(palette = clrs2, domain = fill.vals, bins = mbreaks2)
      
    }else if(input$var == "P"){
      mbreaks1 <- c(0, 0.010, 0.025, 0.050, 0.10, 0.20)
      mbreaks2 <- c(0, 0.005, 0.010, 0.025, 0.050, 0.10)
      
      clrs1 <- rev(brewer.pal(length(mbreaks1), "YlGn"))
      clrs2 <- rev(brewer.pal(length(mbreaks2), "YlGn"))
      pal1 <- colorBin(palette = clrs1, domain = fill.vals, bins = mbreaks1)
      pal2 <- colorBin(palette = clrs2, domain = fill.vals, bins = mbreaks2)
      
    }else if(input$var == "Sulfate"){
      mbreaks1 <- c(0, 10, 25, 50, 100, 250)
      mbreaks2 <- c(0, 10, 25, 50, 75, 100)
      
      clrs1 <- rev(brewer.pal(length(mbreaks1), "RdYlBu"))
      clrs2 <- rev(brewer.pal(length(mbreaks2), "RdYlBu"))
      pal1 <- colorBin(palette = clrs1, domain = fill.vals, bins = mbreaks1)
      pal2 <- colorBin(palette = clrs2, domain = fill.vals, bins = mbreaks2)
      
    }else if(input$var == "Potassium"){
      mbreaks1 <- c(0, 1, 5, 7.5, 10, 25, 30, 35, 40, 45)
      mbreaks2 <- c(0, 0.5, 1, 1.5, 2, 2.5)
      
      clrs1 <- rev(brewer.pal(length(mbreaks1), "RdYlGn"))
      clrs2 <- rev(brewer.pal(length(mbreaks2), "RdYlGn"))
      pal1 <- colorBin(palette = clrs1, domain = fill.vals, bins = mbreaks1)
      pal2 <- colorBin(palette = clrs2, domain = fill.vals, bins = mbreaks2)
      
    }else if(input$var == "Magnesium"){
      mbreaks1 <- c(0, 5, 10, 25, 50, 100, 150, 175)
      mbreaks2 <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
      
      clrs1 <- rev(brewer.pal(length(mbreaks1), "PuOr"))
      clrs2 <- rev(brewer.pal(length(mbreaks2), "PuOr"))
      pal1 <- colorBin(palette = clrs1, domain = fill.vals, bins = mbreaks1)
      pal2 <- colorBin(palette = clrs2, domain = fill.vals, bins = mbreaks2)
      
    }else if(input$var == "Sodium"){
      mbreaks1 <- c(0, 5, 10, 15, 20, 50, 100, 250, 400)
      mbreaks2 <- c(0, 10, 20, 30, 40, 50, 60)
      
      clrs1 <- rev(brewer.pal(length(mbreaks1), "BrBG"))
      clrs2 <- rev(brewer.pal(length(mbreaks2), "BrBG"))
      pal1 <- colorBin(palette = clrs1, domain = fill.vals, bins = mbreaks1)
      pal2 <- colorBin(palette = clrs2, domain = fill.vals, bins = mbreaks2)
      
    }else if(input$var == "DB_WT"){
      mbreaks <- c(-200, 0, 25, 50, 75, 100, max.val)
      mbreaks2 <- seq(-50, (max.val+25), by = 25)
      clrs <- brewer.pal(length(mbreaks), "PuOr")
      clrs2 <- brewer.pal(length(mbreaks2), "PuOr")
      pal <- colorBin(palette = clrs, domain = fill.vals, bins = mbreaks)
      pal2 <- colorBin(palette = clrs2, domain = fill.vals, bins = mbreaks2)
      
    }else if(input$var == "ROWCROP"){
      mbreaks <- c(0, 0.25, 0.50, 0.75, max.val)
      mbreaks2 <- seq(0, (max.val+.10), by = .10)
      clrs <- rev(brewer.pal(length(mbreaks), "RdYlGn"))
      clrs2 <- rev(brewer.pal(length(mbreaks2), "RdYlGn"))
      pal <- colorBin(palette = clrs, domain = fill.vals, bins = mbreaks)
      pal2 <- colorBin(palette = clrs2, domain = fill.vals, bins = mbreaks2) 
      
    }else if(input$var == "DAIRY_PERC"){
      mbreaks <- c(0, 0.25, 0.50, 0.75, max.val)
      mbreaks2 <- seq(0, (max.val+.10), by = .10)
      clrs <- rev(brewer.pal(length(mbreaks), "RdYlGn"))
      clrs2 <- rev(brewer.pal(length(mbreaks2), "RdYlGn"))
      pal <- colorBin(palette = clrs, domain = fill.vals, bins = mbreaks) 
      pal2 <- colorBin(palette = clrs2, domain = fill.vals, bins = mbreaks2)
      
    }else if(input$var == "AG_HAY_PAST"){
      mbreaks <- c(0, 0.25, 0.50, 0.75, max.val)
      mbreaks2 <- seq(0, (max.val+.10), by = .10)
      clrs <- rev(brewer.pal(length(mbreaks), "RdYlGn"))
      clrs2 <- rev(brewer.pal(length(mbreaks2), "RdYlGn"))
      pal <- colorBin(palette = clrs, domain = fill.vals, bins = mbreaks)   
      pal2 <- colorBin(palette = clrs2, domain = fill.vals, bins = mbreaks2)
      
    }else if(input$var == "HAY_PAST"){
      mbreaks <- c(0, 0.25, 0.50, 0.75, max.val)
      mbreaks2 <- seq(0, (max.val+.10), by = .10)
      clrs <- rev(brewer.pal(length(mbreaks), "RdYlGn"))
      clrs2 <- rev(brewer.pal(length(mbreaks2), "RdYlGn"))
      pal <- colorBin(palette = clrs, domain = fill.vals, bins = mbreaks)   
      pal2 <- colorBin(palette = clrs2, domain = fill.vals, bins = mbreaks2)
      
    }else if(input$var == "weighted.rank"){
      mbreaks <- c(1, 2, 3, 4, 5, 6, 7)
      clrs <- rev(brewer.pal(length(mbreaks), "RdYlGn"))
      pal <- colorBin(palette = clrs, domain = fill.vals, bins = mbreaks)    
      
    }else if(input$var == "MapUnit"){
      factpal <- colorFactor(rainbow(6), df$MapUnit)
      

    }else { # input$var == "Hardness"
      mbreaks1 <- c(0, 100, 150, 200, 250, 300, 350, 400, 450, 500, max.val)
      mbreaks2 <- seq(0, (max.val+50), by = 50)
      
      clrs1 <- brewer.pal(length(mbreaks1), "BrBG")
      clrs2 <- brewer.pal(length(mbreaks2), "BrBG")
      pal1 <- colorBin(palette = clrs1, domain = fill.vals, bins = mbreaks1)
      pal2 <- colorBin(palette = clrs2, domain = fill.vals, bins = mbreaks2)
    }                                
    
    
    
    if (input$type == "Individual Wells")  {
      
      if (input$var == "Nitrate"){
        
        proxy <- leafletProxy("map")  
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = df1, lng = df1$LONGITUDE, lat = df1$LATITUDE,
                           layerId = as.vector(df1$SAMPLE_ID),
                           popup = paste0("Well ID: ", df1$PROJECT_ID,
                                          "<br>Year: ", df1$YEAR,
                                          "<br>Sample Date: ", df1$SAMP_DATE,
                                          "<br><strong>Nitrate-Nitrogen Concentration (mg/L): </strong>", ifelse(df1$Nitrate == 0.05,"< 0.1",df1$Nitrate)),
                           stroke = T, color = "black", weight = 0.75, radius = 5,
                           fillOpacity = 0.9, fillColor = ~pal(fill.vals)) %>%
          addLegend("bottomleft", pal = pal, values = fill.vals, title = "Nitrate-Nitrogen <br/>(mg/L)", opacity = 1)
        
      }else if (input$var == "Nitrate_sd"){
        
        proxy <- leafletProxy("map")  
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = df1, lng = df1$LONGITUDE, lat = df1$LATITUDE,
                           layerId = as.vector(df1$SAMPLE_ID),
                           popup = paste0("Well ID: ", df1$PROJECT_ID,
                                          "<br>Year: ", df1$YEAR,
                                          "<br>Sample Date: ", df1$SAMP_DATE,
                                          "<br><strong>Nitrate-Nitrogen Difference (mg/L): </strong>", round(df1$Nitrate_sd,1)),
                           stroke = T, color = "black", weight = 0.75, radius = 5, #(df1$Nitrate_sd*2),
                           fillOpacity = 0.9, fillColor = ~pal(fill.vals)) %>%
          addLegend("bottomleft", pal = pal, values = fill.vals, title = "Nitrate-Nitrogen <br/>(mg/L)", opacity = 1)
        
        
        
      }else if (input$var == "Alkalinity"){
        
        proxy <- leafletProxy("map")  
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = df1, lng = df1$LONGITUDE, lat = df1$LATITUDE,
                           layerId = as.vector(df1$SAMPLE_ID),
                           popup = paste0("Well ID: ", df1$PROJECT_ID,
                                          "<br>YEAR: ", df1$YEAR,
                                          "<br>Alkalinity (mg/L as CaCO3): ", df1$Alkalinity),
                           stroke = T, color = "black", weight = 0.75, radius = 5,
                           fillOpacity = 0.9, fillColor = ~pal(fill.vals)) %>%
          addLegend("bottomleft", pal = pal, values = fill.vals, title = "Alkalinity </br>(mg/L as CaCO3)", opacity = 1)
        
        
        
      }else if (input$var == "Chloride"){
        
        proxy <- leafletProxy("map")  
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = df1, lng = df1$LONGITUDE, lat = df1$LATITUDE,
                           layerId = as.vector(df1$SAMPLE_ID),
                           popup = paste0("Well ID: ", df1$PROJECT_ID,
                                          "<br>YEAR: ", df1$YEAR,
                                          "<br>Chloride (mg/L): ", df1$Chloride),
                           stroke = T, color = "black", weight = 0.75, radius = 5,
                           fillOpacity = 0.9, fillColor = ~pal1(fill.vals)) %>%
          addLegend("bottomleft", pal = pal1, values = fill.vals, title = "Chloride </br>(mg/L)", opacity = 1)
        
        
        
      }else if (input$var == "Conductivity"){
        
        proxy <- leafletProxy("map")  
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = df1, lng = df1$LONGITUDE, lat = df1$LATITUDE,
                           layerId = as.vector(df1$SAMPLE_ID),
                           popup = paste0("Well ID: ", df1$PROJECT_ID,
                                          "<br>YEAR: ", df1$YEAR,
                                          "<br>Conductivity (umhos/cm): ", df1$Conductivity),
                           stroke = T, color = "black", weight = 0.75, radius = 5,
                           fillOpacity = 0.9, fillColor = ~pal1(fill.vals)) %>%
          addLegend("bottomleft", pal = pal1, values = fill.vals, title = "Conductivity </br>(umhos/cm)", opacity = 1)
        
        
      }else if (input$var == "pH"){
        
        proxy <- leafletProxy("map")  
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = df1, lng = df1$LONGITUDE, lat = df1$LATITUDE,
                           layerId = as.vector(df1$SAMPLE_ID),
                           popup = paste0("Well ID: ", df1$PROJECT_ID,
                                          "<br>YEAR: ", df1$YEAR,
                                          "<br>pH: ", df1$pH),
                           stroke = T, color = "black", weight = 0.75, radius = 5,
                           fillOpacity = 0.9, fillColor = ~pal(fill.vals)) %>%
          addLegend("bottomleft", pal = pal, values = fill.vals, title = "ph </br>(standard units)", opacity = 1)
        
      }else if (input$var == "Arsenic"){
        
        proxy <- leafletProxy("map")  
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = df1, lng = df1$LONGITUDE, lat = df1$LATITUDE,
                           layerId = as.vector(df1$SAMPLE_ID),
                           popup = paste0("Well ID: ", df1$PROJECT_ID,
                                          "<br>Year: ", df1$YEAR,
                                          "<br>Sample Date: ", df1$SAMP_DATE,
                                          "<br><strong>Arsenic (mg/L): </strong>", df1$Arsenic),
                           stroke = T, color = "black", weight = 0.75, radius = 5, 
                           fillOpacity = 0.9, fillColor = ~pal1(fill.vals)) %>%
          addLegend("bottomleft", pal = pal1, values = fill.vals, title = "Arsenic </br>(mg/L)", opacity = 1)
        
      }else if (input$var == "Calcium"){
        
        proxy <- leafletProxy("map")  
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = df1, lng = df1$LONGITUDE, lat = df1$LATITUDE,
                           layerId = as.vector(df1$SAMPLE_ID),
                           popup = paste0("Well ID: ", df1$PROJECT_ID,
                                          "<br>Year: ", df1$YEAR,
                                          "<br>Sample Date: ", df1$SAMP_DATE,
                                          "<br><strong>Calcium (mg/L): </strong>", df1$Calcium),
                           stroke = T, color = "black", weight = 0.75, radius = 5, 
                           fillOpacity = 0.9, fillColor = ~pal1(fill.vals)) %>%
          addLegend("bottomleft", pal = pal1, values = fill.vals, title = "Calcium <\br>(mg/L)", opacity = 1)
        
      }else if (input$var == "Iron"){
        
        proxy <- leafletProxy("map")  
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = df1, lng = df1$LONGITUDE, lat = df1$LATITUDE,
                           layerId = as.vector(df1$SAMPLE_ID),
                           popup = paste0("Well ID: ", df1$PROJECT_ID,
                                          "<br>Year: ", df1$YEAR,
                                          "<br>Sample Date: ", df1$SAMP_DATE,
                                          "<br><strong>Iron (mg/L): </strong>", df1$Iron),
                           stroke = T, color = "black", weight = 0.75, radius = 5, 
                           fillOpacity = 0.9, fillColor = ~pal1(fill.vals)) %>%
          addLegend("bottomleft", pal = pal1, values = fill.vals, title = "Iron </br>(mg/L)", opacity = 1)
        
      }else if (input$var == "Manganese"){
        
        proxy <- leafletProxy("map")  
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = df1, lng = df1$LONGITUDE, lat = df1$LATITUDE,
                           layerId = as.vector(df1$SAMPLE_ID),
                           popup = paste0("Well ID: ", df1$PROJECT_ID,
                                          "<br>Year: ", df1$YEAR,
                                          "<br>Sample Date: ", df1$SAMP_DATE,
                                          "<br><strong>Manganese (mg/L): </strong>", df1$Manganese),
                           stroke = T, color = "black", weight = 0.75, radius = 5, 
                           fillOpacity = 0.9, fillColor = ~pal1(fill.vals)) %>%
          addLegend("bottomleft", pal = pal1, values = fill.vals, title = "Manganese </br>(mg/L)", opacity = 1)
        
      }else if (input$var == "P"){
        
        proxy <- leafletProxy("map")  
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = df1, lng = df1$LONGITUDE, lat = df1$LATITUDE,
                           layerId = as.vector(df1$SAMPLE_ID),
                           popup = paste0("Well ID: ", df1$PROJECT_ID,
                                          "<br>Year: ", df1$YEAR,
                                          "<br>Sample Date: ", df1$SAMP_DATE,
                                          "<br><strong>Phosphorous (mg/L): </strong>", df1$P),
                           stroke = T, color = "black", weight = 0.75, radius = 5, 
                           fillOpacity = 0.9, fillColor = ~pal1(fill.vals)) %>%
          addLegend("bottomleft", pal = pal1, values = fill.vals, title = "Phosphorus </br>(mg/L)", opacity = 1)
        
      }else if (input$var == "Sulfate"){
        
        proxy <- leafletProxy("map")  
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = df1, lng = df1$LONGITUDE, lat = df1$LATITUDE,
                           layerId = as.vector(df1$SAMPLE_ID),
                           popup = paste0("Well ID: ", df1$PROJECT_ID,
                                          "<br>Year: ", df1$YEAR,
                                          "<br>Sample Date: ", df1$SAMP_DATE,
                                          "<br><strong>Sulfate (mg/L): </strong>", df1$Sulfate),
                           stroke = T, color = "black", weight = 0.75, radius = 5, 
                           fillOpacity = 0.9, fillColor = ~pal1(fill.vals)) %>%
          addLegend("bottomleft", pal = pal1, values = fill.vals, title = "Sulfate </br>(mg/L)", opacity = 1)
        
      }else if (input$var == "Potassium"){
        
        proxy <- leafletProxy("map")  
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = df1, lng = df1$LONGITUDE, lat = df1$LATITUDE,
                           layerId = as.vector(df1$SAMPLE_ID),
                           popup = paste0("Well ID: ", df1$PROJECT_ID,
                                          "<br>Year: ", df1$YEAR,
                                          "<br>Sample Date: ", df1$SAMP_DATE,
                                          "<br><strong>Potassium (mg/L): </strong>", df1$Potassium),
                           stroke = T, color = "black", weight = 0.75, radius = 5, 
                           fillOpacity = 0.9, fillColor = ~pal1(fill.vals)) %>%
          addLegend("bottomleft", pal = pal1, values = fill.vals, title = "Potassium </br>(mg/L)", opacity = 1)
        
      }else if (input$var == "Magnesium"){
        
        proxy <- leafletProxy("map")  
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = df1, lng = df1$LONGITUDE, lat = df1$LATITUDE,
                           layerId = as.vector(df1$SAMPLE_ID),
                           popup = paste0("Well ID: ", df1$PROJECT_ID,
                                          "<br>Year: ", df1$YEAR,
                                          "<br>Sample Date: ", df1$SAMP_DATE,
                                          "<br><strong>Magnesium (mg/L): </strong>", df1$Magnesium),
                           stroke = T, color = "black", weight = 0.75, radius = 5, 
                           fillOpacity = 0.9, fillColor = ~pal1(fill.vals)) %>%
          addLegend("bottomleft", pal = pal1, values = fill.vals, title = "Magnesium </br>(mg/L)", opacity = 1)
        
      }else if (input$var == "Sodium"){
        
        proxy <- leafletProxy("map")  
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = df1, lng = df1$LONGITUDE, lat = df1$LATITUDE,
                           layerId = as.vector(df1$SAMPLE_ID),
                           popup = paste0("Well ID: ", df1$PROJECT_ID,
                                          "<br>Year: ", df1$YEAR,
                                          "<br>Sample Date: ", df1$SAMP_DATE,
                                          "<br><strong>Sodium (mg/L): </strong>", df1$Sodium),
                           stroke = T, color = "black", weight = 0.75, radius = 5, 
                           fillOpacity = 0.9, fillColor = ~pal1(fill.vals)) %>%
          addLegend("bottomleft", pal = pal1, values = fill.vals, title = "Sodium </br>(mg/L)", opacity = 1)
        
        
      }else if (input$var == "DB_WT"){
        
        proxy <- leafletProxy("map")  
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = df1, lng = df1$LONGITUDE, lat = df1$LATITUDE,
                           layerId = as.vector(df1$SAMPLE_ID),
                           popup = paste0("Well ID: ", df1$PROJECT_ID,
                                          "<br>YEAR: ", df1$YEAR,
                                          "<br>Casing Below Water Table (ft): ", ifelse(is.na(df1$DB_WT),"Not Available",ifelse(df1$DB_WT < 0, "Above Water Table", df1$DB_WT))),
                           stroke = T, color = "black", weight = 0.75, radius = 5,
                           fillOpacity = 0.9, fillColor = ~pal(fill.vals)) %>%
          addLegend("bottomleft", pal = pal, values = fill.vals, title = "Depth Below Water Table (ft)", opacity = 1)
      
      }else if (input$var == "ROWCROP"){
        
        proxy <- leafletProxy("map")  
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = df1, lng = df1$LONGITUDE, lat = df1$LATITUDE,
                           layerId = as.vector(df1$SAMPLE_ID),
                           popup = paste0("Well ID: ", df1$PROJECT_ID,
                                          "<br>YEAR: ", df1$YEAR,
                                          "<br>Percent Row Crops (500 m buffer): ", ifelse(is.na(df1$ROWCROP),"Not Available", round((df1$ROWCROP*100),1))),
                           stroke = T, color = "black", weight = 0.75, radius = 5,
                           fillOpacity = 0.9, fillColor = ~pal(fill.vals)) %>%
          addLegend("bottomleft", pal = pal, values = fill.vals, title = "Percent Row Crops (500 m buffer)", opacity = 1)
      
      }else if (input$var == "DAIRY_PERC"){
        
        proxy <- leafletProxy("map")  
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = df1, lng = df1$LONGITUDE, lat = df1$LATITUDE,
                           layerId = as.vector(df1$SAMPLE_ID),
                           popup = paste0("Well ID: ", df1$PROJECT_ID,
                                          "<br>YEAR: ", df1$YEAR,
                                          "<br>Percent Dairy Rotation (500 m buffer): ", ifelse(is.na(df1$DAIRY_PERC),"Not Available", round((df1$DAIRY_PERC*100),1))),
                           stroke = T, color = "black", weight = 0.75, radius = 5,
                           fillOpacity = 0.9, fillColor = ~pal(fill.vals)) %>%
          addLegend("bottomleft", pal = pal, values = fill.vals, title = "Percent Dairy Rotation (500 m buffer)", opacity = 1)
        
        
      }else if (input$var == "AG_HAY_PAST"){
        
        proxy <- leafletProxy("map")  
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = df1, lng = df1$LONGITUDE, lat = df1$LATITUDE,
                           layerId = as.vector(df1$SAMPLE_ID),
                           popup = paste0("Well ID: ", df1$PROJECT_ID,
                                          "<br>YEAR: ", df1$YEAR,
                                          "<br>Percent All Agriculture (500 m buffer): ", ifelse(is.na(df1$AG_HAY_PAST),"Not Available", round((df1$AG_HAY_PAST*100),1))),
                           stroke = T, color = "black", weight = 0.75, radius = 5,
                           fillOpacity = 0.9, fillColor = ~pal(fill.vals)) %>%
          addLegend("bottomleft", pal = pal, values = fill.vals, title = "Percent All Agriculture (500 m buffer)", opacity = 1)
      
      }else if (input$var == "HAY_PAST"){
        
        proxy <- leafletProxy("map")  
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = df1, lng = df1$LONGITUDE, lat = df1$LATITUDE,
                           layerId = as.vector(df1$SAMPLE_ID),
                           popup = paste0("Well ID: ", df1$PROJECT_ID,
                                          "<br>YEAR: ", df1$YEAR,
                                          "<br>Percent Hay/Pasture (500 m buffer): ", ifelse(is.na(df1$HAY_PAST),"Not Available", round((df1$HAY_PAST*100),1))),
                           stroke = T, color = "black", weight = 0.75, radius = 5,
                           fillOpacity = 0.9, fillColor = ~pal(fill.vals)) %>%
          addLegend("bottomleft", pal = pal, values = fill.vals, title = "Percent Hay/Pasture (500 m buffer)", opacity = 1)
        
        
      }else if (input$var == "weighted.rank"){
        
        proxy <- leafletProxy("map")  
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = df1, lng = df1$LONGITUDE, lat = df1$LATITUDE,
                           layerId = as.vector(df1$SAMPLE_ID),
                           popup = paste0("Well ID: ", df1$PROJECT_ID,
                                          "<br>YEAR: ", df1$YEAR,
                                          "<br>Soil Drainage Rank (500 m buffer): ", ifelse(df$weighted.rank <= 1.5, "Very Poorly Drained",
                                                                                                      ifelse(df$weighted.rank > 1.5 & df$weighted.rank <= 2.5, "Poorly Drained", 
                                                                                                             ifelse(df$weighted.rank > 2.5 & df$weighted.rank <= 3.5, "Somewhat Poorly Drained", 
                                                                                                                    ifelse(df$weighted.rank > 3.5 & df$weighted.rank <= 4.5, "Moderately Well Drained",
                                                                                                                           ifelse(df$weighted.rank > 4.5 & df$weighted.rank <= 5.5, "Well Drained",
                                                                                                                                  ifelse(df$weighted.rank > 5.5 & df$weighted.rank <= 6.5, "Somewhat Excessively Drained",
                                                                                                                                         ifelse(df$weighted.rank > 6.5, "Excessively Drained", 
                                                                                                                                                ifelse(is.na(df$weighted.rank),"Not Available",9999))))))))),
                           stroke = T, color = "black", weight = 0.75, radius = 5,
                           fillOpacity = 0.9, fillColor = ~pal(fill.vals)) %>%
          addLegend("bottomleft", pal = pal, values = fill.vals, title = "Soil Drainage Rank (500 m buffer)", opacity = 1)
        
      }else if (input$var == "MapUnit"){

        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%

          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = df1, lng = df1$LONGITUDE, lat = df1$LATITUDE,
                           layerId = as.vector(df1$SAMPLE_ID),
                           popup = paste0("Well ID: ", df1$PROJECT_ID,
                                          "<br>YEAR: ", df1$YEAR,
                                          "<br>Quaternary Sediment Type: ", as.factor(df1$MapUnit),
                                          "<br>Map Unit Symbol: ", as.factor(df1$Symbol)),
                           stroke = T, color = "black", weight = 0.75, radius = 5,
                           fillOpacity = 0.9, fillColor = ~factpal(MapUnit)) #%>%
          #addLegend("bottomleft", pal = factpal, title = "Quaternary Geology Type", opacity = 1)
        
        
        
      }else {  #(input$var == "Hardness")
        
        proxy <- leafletProxy("map")  
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1, fill = FALSE) %>%
          addCircleMarkers(data = df1, lng = df1$LONGITUDE, lat = df1$LATITUDE,
                           layerId = as.vector(df1$SAMPLE_ID),
                           popup = paste0("Well ID: ", df1$PROJECT_ID,
                                          "<br>YEAR: ", df1$YEAR,
                                          "<br>Total Hardness (mg/L as CaCO3): ", ifelse(is.na(df1$Total.Hardness),"Softened",df1$Total.Hardness)),
                           stroke = T, color = "black", weight = 0.75, radius = 5,
                           fillOpacity = 0.9, fillColor = ~pal1(fill.vals)) %>%
          addLegend("bottomleft", pal = pal1, values = fill.vals, title = "Total Hardness </br>(mg/L as CaCO3)", opacity = 1)
      } 
        
     
      
      
      #Muni Maps below
      
    } else {  #(input$type == "Municipality")
      
      if (input$var == "Nitrate"){
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1,
                      fillColor = ~pal(fill.vals), fillOpacity = 0.9,
                      popup = paste0(shape@data$Municipality,
                                     "<br><strong>Nitrate-Nitrogen Concentration (mg/L)</strong>",
                                     "<br>Average: ", round(shape@data$Nitrate, 1),
                                     "<br>Minimum: ", shape@data$Nitrate_min,
                                     "<br>Maximum: ", shape@data$Nitrate_max,
                                     "<br>Median: ", shape@data$Nitrate_median,
                                     "<br>Number of Samples: ", shape@data$cnt_n),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>%
          addLegend("bottomleft", pal = pal, values = fill.vals, title = "Nitrate-Nitogen </br>(mg/L)", opacity = 1)
        
        
      }else if (input$var == "Alkalinity"){
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1,
                      fillColor = ~pal(fill.vals), fillOpacity = 0.9,
                      popup = paste0(shape@data$Municipality,
                                     "<br><strong>Alkalinity Concentration (mg/L as CaCO3)</strong>",
                                     "<br>Average: ", round(shape@data$Alkalinity, 1),
                                     "<br>Minimum: ", shape@data$Alkalinity_min,
                                     "<br>Maximum: ", shape@data$Alkalinity_max,
                                     "<br>Median: ", shape@data$Alkalinity_med,
                                     "<br>Number of Samples: ", shape@data$cnt_n),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>%
          addLegend("bottomleft", pal = pal, values = fill.vals, title = "Alkalinity </br>(mg/L as CaCO3)", opacity = 1)
        
        
      }else if (input$var == "Chloride"){
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1,
                      fillColor = ~pal2(fill.vals), fillOpacity = 0.9,
                      popup = paste0(shape@data$Municipality,
                                     "<br><strong>Chloride Concentration (mg/L)</strong>",
                                     "<br>Average: ", round(shape@data$Chloride,1),
                                     "<br>Minimum: ", shape@data$Chloride_min,
                                     "<br>Maximum: ", shape@data$Chloride_max,
                                     "<br>Median: ", shape@data$Chloride_med,
                                     "<br>Number of Samples: ", shape@data$cnt_n),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>%
          addLegend("bottomleft", pal = pal2, values = fill.vals, title = "Chloride </br>(mg/L)", opacity = 1)
        
        
      }else if (input$var == "Conductivity"){
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1,
                      fillColor = ~pal2(fill.vals), fillOpacity = 0.9,
                      popup = paste0(shape@data$Municipality,
                                     "<br><strong>Conductivity Concentration (umhos/cm)</strong>",
                                     "<br>Average: ", round(shape@data$Conductivity,1),
                                     "<br>Minimum: ", shape@data$Conductivity_min,
                                     "<br>Maximum: ", shape@data$Conductivity_max,
                                     "<br>Median: ", shape@data$Conductivity_med,
                                     "<br>Number of Samples: ", shape@data$cnt_n),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>%
          addLegend("bottomleft", pal = pal2, values = fill.vals, title = "Conductivity </br>(umhos/cm)", opacity = 1)
        
        
      }else if (input$var == "pH"){
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1,
                      fillColor = ~pal(fill.vals), fillOpacity = 0.9,
                      popup = paste0(shape@data$Municipality,
                                     "<br><strong>pH (standard units)</strong>",
                                     "<br>Average: ", round(shape@data$pH,1),
                                     "<br>Minimum: ", shape@data$pH_min,
                                     "<br>Maximum: ", shape@data$pH_max,
                                     "<br>Median: ", shape@data$pH_med,
                                     "<br>Number of Samples: ", shape@data$cnt_n),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>%
          addLegend("bottomleft", pal = pal, values = fill.vals, title = "pH </br>(standard units)", opacity = 1)
        
      }else if (input$var == "Arsenic"){
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1,
                      fillColor = ~pal2(fill.vals), fillOpacity = 0.9,
                      popup = paste0(shape@data$Municipality,
                                     "<br><strong>Arsenic (mg/L)</strong>",
                                     "<br>Average: ", round(shape@data$Arsenic,3),
                                     "<br>Minimum: ", shape@data$Arsenic_min,
                                     "<br>Maximum: ", shape@data$Arsenic_max,
                                     "<br>Median: ", shape@data$Arsenic_med,
                                     "<br>Number of Samples: ", shape@data$cnt_n),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>%
          addLegend("bottomleft", pal = pal2, values = fill.vals, title = "Arsenic </br>(mg/L)", opacity = 1)
        
      }else if (input$var == "Calcium"){
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1,
                      fillColor = ~pal2(fill.vals), fillOpacity = 0.9,
                      popup = paste0(shape@data$Municipality,
                                     "<br><strong>Calcium (mg/L)</strong>",
                                     "<br>Average: ", round(shape@data$Calcium,1),
                                     "<br>Minimum: ", shape@data$Calcium_min,
                                     "<br>Maximum: ", shape@data$Calcium_max,
                                     "<br>Median: ", shape@data$Calcium_med,
                                     "<br>Number of Samples: ", shape@data$cnt_n),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>%
          addLegend("bottomleft", pal = pal2, values = fill.vals, title = "Calcium </br>(mg/L)", opacity = 1)
        
      }else if (input$var == "Iron"){
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1,
                      fillColor = ~pal2(fill.vals), fillOpacity = 0.9,
                      popup = paste0(shape@data$Municipality,
                                     "<br><strong>Iron (mg/L)</strong>",
                                     "<br>Average: ", round(shape@data$Iron,1),
                                     "<br>Minimum: ", shape@data$Iron_min,
                                     "<br>Maximum: ", shape@data$Iron_max,
                                     "<br>Median: ", shape@data$Iron_med,
                                     "<br>Number of Samples: ", shape@data$cnt_n),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>%
          addLegend("bottomleft", pal = pal2, values = fill.vals, title = "Iron </br>(mg/L)", opacity = 1)
        
      }else if (input$var == "Manganese"){
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1,
                      fillColor = ~pal2(fill.vals), fillOpacity = 0.9,
                      popup = paste0(shape@data$Municipality,
                                     "<br><strong>Manganese (mg/L)</strong>",
                                     "<br>Average: ", round(shape@data$Manganese,3),
                                     "<br>Minimum: ", shape@data$Manganese_min,
                                     "<br>Maximum: ", shape@data$Manganese_max,
                                     "<br>Median: ", shape@data$Manganese_med,
                                     "<br>Number of Samples: ", shape@data$cnt_n),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>%
          addLegend("bottomleft", pal = pal2, values = fill.vals, title = "Manganese </br>(mg/L)", opacity = 1)
        
      }else if (input$var == "P"){
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1,
                      fillColor = ~pal2(fill.vals), fillOpacity = 0.9,
                      popup = paste0(shape@data$Municipality,
                                     "<br><strong>Phosphorous (mg/L)</strong>",
                                     "<br>Average: ", round(shape@data$P,3),
                                     "<br>Minimum: ", shape@data$P_min,
                                     "<br>Maximum: ", shape@data$P_max,
                                     "<br>Median: ", shape@data$P_med,
                                     "<br>Number of Samples: ", shape@data$cnt_n),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>%
          addLegend("bottomleft", pal = pal2, values = fill.vals, title = "Phosphorus </br>(mg/L)", opacity = 1)
        
      }else if (input$var == "Sulfate"){
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1,
                      fillColor = ~pal2(fill.vals), fillOpacity = 0.9,
                      popup = paste0(shape@data$Municipality,
                                     "<br><strong>Sulfate (mg/L)</strong>",
                                     "<br>Average: ", round(shape@data$Sulfate,1),
                                     "<br>Minimum: ", shape@data$Sulfate_min,
                                     "<br>Maximum: ", shape@data$Sulfate_max,
                                     "<br>Median: ", shape@data$Sulfate_med,
                                     "<br>Number of Samples: ", shape@data$cnt_n),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>%
          addLegend("bottomleft", pal = pal2, values = fill.vals, title = "Sulfate </br>(mg/L)", opacity = 1)
        
      }else if (input$var == "Potassium"){
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1,
                      fillColor = ~pal2(fill.vals), fillOpacity = 0.9,
                      popup = paste0(shape@data$Municipality,
                                     "<br><strong>Potassium (mg/L)</strong>",
                                     "<br>Average: ", round(shape@data$Potassium,1),
                                     "<br>Minimum: ", shape@data$Potassium_min,
                                     "<br>Maximum: ", shape@data$Potassium_max,
                                     "<br>Median: ", shape@data$Potassium_med,
                                     "<br>Number of Samples: ", shape@data$cnt_n),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>%
          addLegend("bottomleft", pal = pal2, values = fill.vals, title = "Potassium </br>(mg/L)", opacity = 1)
        
      }else if (input$var == "Magnesium"){
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1,
                      fillColor = ~pal2(fill.vals), fillOpacity = 0.9,
                      popup = paste0(shape@data$Municipality,
                                     "<br><strong>Magnesium (mg/L)</strong>",
                                     "<br>Average: ", round(shape@data$Magnesium,1),
                                     "<br>Minimum: ", shape@data$Magnesium_min,
                                     "<br>Maximum: ", shape@data$Magnesium_max,
                                     "<br>Median: ", shape@data$Magnesium_med,
                                     "<br>Number of Samples: ", shape@data$cnt_n),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>%
          addLegend("bottomleft", pal = pal2, values = fill.vals, title = "Magnesium </br>(mg/L)", opacity = 1)
        
      }else if (input$var == "Sodium"){
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1,
                      fillColor = ~pal2(fill.vals), fillOpacity = 0.9,
                      popup = paste0(shape@data$Municipality,
                                     "<br><strong>Sodium (mg/L)</strong>",
                                     "<br>Average: ", round(shape@data$Sodium,1),
                                     "<br>Minimum: ", shape@data$Sodium_min,
                                     "<br>Maximum: ", shape@data$Sodium_max,
                                     "<br>Median: ", shape@data$Sodium_med,
                                     "<br>Number of Samples: ", shape@data$cnt_n),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>%
          addLegend("bottomleft", pal = pal2, values = fill.vals, title = "Sodium </br>(mg/L)", opacity = 1)
      
      }else if (input$var == "DB_WT"){
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1,
                      fillColor = ~pal2(fill.vals), fillOpacity = 0.9,
                      popup = paste0(shape@data$Municipality,
                                     "<br><strong>Casing depth below the water tabled (ft)</strong>",
                                     "<br>Average: ", round(shape@data$DB_WT,1),
                                     "<br>Minimum: ", shape@data$DB_WT_min,
                                     "<br>Maximum: ", shape@data$DB_WT_max,
                                     "<br>Median: ", shape@data$DB_WT_med,
                                     "<br>Number of Samples: ", shape@data$cnt_n),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>%
          addLegend("bottomleft", pal = pal2, values = fill.vals, title = "Depth Below Water Table (ft)", opacity = 1)    
        
      }else if (input$var == "ROWCROP"){
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1,
                      fillColor = ~pal2(fill.vals), fillOpacity = 0.9,
                      popup = paste0(shape@data$Municipality,
                                     "<br>Precent Row Crop Agriculture (500 m buffer)</strong>",
                                     "<br>Average: ", round(shape@data$ROWCROP,2),
                                     "<br>Minimum: ", round(shape@data$ROWCROP_min,2),
                                     "<br>Maximum: ", round(shape@data$ROWCROP_max,2),
                                     "<br>Median: ", round(shape@data$ROWCROP_med,2),
                                     "<br>Number of Samples: ", shape@data$cnt_n),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>%
          addLegend("bottomleft", pal = pal2, values = fill.vals, title = "Percent Row Crops (500 m buffer)", opacity = 1)    
      
      }else if (input$var == "DAIRY_PERC"){
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1,
                      fillColor = ~pal2(fill.vals), fillOpacity = 0.9,
                      popup = paste0(shape@data$Municipality,
                                     "<br>Precent Dairy Rotation (500 m buffer)</strong>",
                                     "<br>Average: ", round(shape@data$DAIRY_PERC,2),
                                     "<br>Minimum: ", round(shape@data$DAIRY_PERC_min,2),
                                     "<br>Maximum: ", round(shape@data$DAIRY_PERC_max,2),
                                     "<br>Median: ", round(shape@data$DAIRY_PERC_med,2),
                                     "<br>Number of Samples: ", shape@data$cnt_n),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>%
          addLegend("bottomleft", pal = pal2, values = fill.vals, title = "Percent Dairy Rotation (500 m buffer)", opacity = 1)    
        
        
      }else if (input$var == "HAY_PAST"){
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1,
                      fillColor = ~pal2(fill.vals), fillOpacity = 0.9,
                      popup = paste0(shape@data$Municipality,
                                     "<br>Precent Hay/Pasture (500 m buffer)</strong>",
                                     "<br>Average: ", round(shape@data$HAY_PAST,2),
                                     "<br>Minimum: ", round(shape@data$HAY_PAST_min,2),
                                     "<br>Maximum: ", round(shape@data$HAY_PAST_max,2),
                                     "<br>Median: ", round(shape@data$HAY_PAST_med,2),
                                     "<br>Number of Samples: ", shape@data$cnt_n),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>%
          addLegend("bottomleft", pal = pal2, values = fill.vals, title = "Percent Hay/Pasture (500 m buffer)", opacity = 1)    
        
      }else if (input$var == "AG_HAY_PAST"){
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1,
                      fillColor = ~pal2(fill.vals), fillOpacity = 0.9,
                      popup = paste0(shape@data$Municipality,
                                     "<br>Precent All Agriculture (500 m buffer)</strong>",
                                     "<br>Average: ", round(shape@data$AG_HAY_PAST,2),
                                     "<br>Minimum: ", round(shape@data$AG_HAY_PAST_min,2),
                                     "<br>Maximum: ", round(shape@data$AG_HAY_PAST_max,2),
                                     "<br>Median: ", round(shape@data$AG_HAY_PAST_med,2),
                                     "<br>Number of Samples: ", shape@data$cnt_n),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>%
          addLegend("bottomleft", pal = pal2, values = fill.vals, title = "Percent All Agriculture (500 m buffer)", opacity = 1)    
        
         
      }else if (input$var == "weighted.rank"){
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1,
                      fillColor = ~pal(fill.vals), fillOpacity = 0.9,
                      popup = paste0(shape@data$Municipality,
                                     "<br>Soil Drainage Classification",
                                     "<br>Average: ", round(shape@data$weighted.rank,1),
                                     "<br>Minimum: ", round(shape@data$weighted.rank_min,1),
                                     "<br>Maximum: ", round(shape@data$weighted.rank_max,1),
                                     "<br>Median: ", round(shape@data$weighted.rank_med,1),
                                     "<br>Number of Samples: ", shape@data$cnt_n,
                                    "<br>", ifelse(df$weighted.rank <= 1.5, "Very Poorly Drained",
                                                   ifelse(df$weighted.rank > 1.5 & df$weighted.rank <= 2.5, "Poorly Drained", 
                                                          ifelse(df$weighted.rank > 2.5 & df$weighted.rank <= 3.5, "Somewhat Poorly Drained", 
                                                                 ifelse(df$weighted.rank > 3.5 & df$weighted.rank <= 4.5, "Moderately Well Drained",
                                                                        ifelse(df$weighted.rank > 4.5 & df$weighted.rank <= 5.5, "Well Drained",
                                                                               ifelse(df$weighted.rank > 5.5 & df$weighted.rank <= 6.5, "Somewhat Excessively Drained",
                                                                                      ifelse(df$weighted.rank > 6.5, "Excessively Drained", 
                                                                                             ifelse(is.na(df$weighted.rank),"Not Available",9999))))))))),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>%
          addLegend("bottomleft", pal = pal, values = fill.vals, title = "Soil Drainage Rank (500 m buffer)", opacity = 1)    
      
      }else if (input$var == "MapUnit"){
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1,
                      #fillColor = ~pal(fill.vals), fillOpacity = 0.9,
                      popup = paste0(shape@data$Municipality,
                                     "<br>Uppermost Bedrock Type not applicable for municipality display</strong>"),
                                highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T))   
        
      }else if (input$var == "Nitrate_sd"){
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1,
                      #fillColor = ~pal(fill.vals), fillOpacity = 0.9,
                      popup = paste0(shape@data$Municipality,
                                     "<br>Nitrate Variability not applicable for municipality display</strong>"),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T))      
        
      }else { #(input$var == "Total Hardness")
        
        proxy <- leafletProxy("map")
        proxy %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          addPolygons(data = shape,
                      layerId = as.vector(shape@data$OBJECTID),
                      stroke = T, color = "black",
                      smoothFactor = 1, weight = 1,
                      fillColor = ~pal2(fill.vals), fillOpacity = 0.9,
                      popup = paste0(shape@data$Municipality,
                                     "<br><strong>Total Hardness Concentration (mg/L as CaCO3)</strong>",
                                     "<br>Average: ", round(shape@data$Total.Hardness,1),
                                     "<br>Minimum: ", shape@data$Total.Hardness_min,
                                     "<br>Maximum: ", shape@data$Total.Hardness_max,
                                     "<br>Median: ", shape@data$Total.Hardness_med,
                                     "<br>Number of Samples: ", (shape@data$cnt_n - shape@data$cnt_Thard_n)),
                      highlight = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = T)) %>%
          addLegend("bottomleft", pal = pal2, values = fill.vals, title = paste(input$var), opacity = 1)
      }
    }
}
  
  )
  
  #Start of box plots by town 
  output$select_analyte <- renderPlotly({
    
    #Subsetting
    df3 <- subset(df, YEAR==input$year_muni)
    
    
    p1 <- {
      
      
      
      if(input$analyte == "Nitrate"){
        ggplot(df3, aes(x = TOWNSHIP, y = Nitrate)) +
          geom_boxplot() +
          stat_summary(fun = mean, colour ="blue", geom="point",
                       shape=5, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          geom_hline(yintercept = 10 , linetype = "dashed" , color = "red") +
          scale_fill_manual(values=c("black","blue")) +
          xlab("Town") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Nitrate-Nitrogen (mg/L)") +
          labs(title="Summary of Nitrate-Nitrogen Concentration by Town")
        
        
        
      }else if (input$analyte == "Alkalinity") {
        ggplot(df3, aes(x = TOWNSHIP, y = Alkalinity)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point", 
                       shape=5, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          xlab("Town") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Alkalinity (mg/L as CaCO3)") +
          labs(title="Summary of Alkalinity Concentration by Town")
        
      }else if (input$analyte == "Chloride") {
        ggplot(df3, aes(x = TOWNSHIP, y = Chloride)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point", 
                       shape=5, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0+1, label = ..count..)) +
          xlab("Town") +
          ylim(0,250) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Chloride (mg/L)") +
          labs(title="Summary of Chloride Concentration by Town")
        
      }else if (input$analyte == "Total Hardness") {
        ggplot(df3, aes(x = TOWNSHIP, y = Total.Hardness)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point", 
                       shape=5, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          geom_hline(yintercept = 150 , linetype = "dashed" , color = "green") +
          geom_hline(yintercept = 200 , linetype = "dashed" , color = "purple") +
          xlab("Town") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Total Hardness (mg/L as CaCO3)") +
          labs(title="Summary of TOtal Hardness Concentration by Town")
        
      }else if (input$analyte == "Conductivity") {
        ggplot(df3, aes(x = TOWNSHIP, y = Conductivity)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point", 
                       shape=5, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          xlab("Town") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Conductivity (umhos/cm)") +
          labs(title="Summary of Conductivity Concentration by Town")
        
      }else if (input$analyte == "Arsenic") {
        ggplot(df3, aes(x = TOWNSHIP, y = Arsenic)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point", 
                       shape=5, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          geom_hline(yintercept = 0.01 , linetype = "dashed" , color = "green") +
          xlab("Town") +
          ylim(0,0.10) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Arsenic (mg/L)") +
          labs(title="Summary of Arsenic Concentration by Town")
        
      }else if (input$analyte == "Calcium") {
        ggplot(df3, aes(x = TOWNSHIP, y = Calcium)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point", 
                       shape=5, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          #geom_hline(yintercept = 0.01 , linetype = "dashed" , color = "green") +
          xlab("Town") +
          #ylim(0,0.10) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Calcium (mg/L)") +
          labs(title="Summary of Calcium Concentration by Town")
        
      }else if (input$analyte == "Iron") {
        ggplot(df3, aes(x = TOWNSHIP, y = Iron)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point", 
                       shape=5, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          #geom_hline(yintercept = 0.01 , linetype = "dashed" , color = "green") +
          xlab("Town") +
          ylim(0,10) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Iron (mg/L)") +
          labs(title="Summary of Iron Concentration by Town")
        
      }else if (input$analyte == "Manganese") {
        ggplot(df3, aes(x = TOWNSHIP, y = Manganese)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point", 
                       shape=5, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          #geom_hline(yintercept = 0.01 , linetype = "dashed" , color = "green") +
          xlab("Town") +
          ylim(0,1) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Manganese (mg/L)") +
          labs(title="Summary of Manganese Concentration by Town")
        
      }else if (input$analyte == "Phosphorus") {
        ggplot(df3, aes(x = TOWNSHIP, y = P)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point", 
                       shape=5, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          #geom_hline(yintercept = 0.01 , linetype = "dashed" , color = "green") +
          xlab("Town") +
          ylim(0,1) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Phosphorus (mg/L)") +
          labs(title="Summary of Phosphorus Concentration by Town")
        
      }else if (input$analyte == "Sulfate") {
        ggplot(df3, aes(x = TOWNSHIP, y = Sulfate)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point", 
                       shape=5, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          #geom_hline(yintercept = 0.01 , linetype = "dashed" , color = "green") +
          xlab("Town") +
          #ylim(0,0.10) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Sulfate (mg/L)") +
          labs(title="Summary of Sulfate Concentration by Town")
        
      }else if (input$analyte == "Potassium") {
        ggplot(df3, aes(x = TOWNSHIP, y = Potassium)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point", 
                       shape=5, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          #geom_hline(yintercept = 0.01 , linetype = "dashed" , color = "green") +
          xlab("Town") +
          #ylim(0,0.10) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Potassium (mg/L)") +
          labs(title="Summary of Potassium Concentration by Town")
        
      }else if (input$analyte == "Magnesium") {
        ggplot(df3, aes(x = TOWNSHIP, y = Magnesium)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point", 
                       shape=5, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          #geom_hline(yintercept = 0.01 , linetype = "dashed" , color = "green") +
          xlab("Town") +
          #ylim(0,0.10) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Magnesium (mg/L)") +
          labs(title="Summary of Magnesium Concentration by Town")
        
      }else if (input$analyte == "Sodium") {
        ggplot(df3, aes(x = TOWNSHIP, y = Sodium)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point", 
                       shape=5, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          #geom_hline(yintercept = 0.01 , linetype = "dashed" , color = "green") +
          xlab("Town") +
          #ylim(0,0.10) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Sodium (mg/L)") +
          labs(title="Summary of Sodium Concentration by Town")
        
      }else { (input$analyte == "pH") 
        ggplot(df3, aes(x = TOWNSHIP, y = pH)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point", 
                       shape=5, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          geom_hline(yintercept = 7 , linetype = "dashed" , color = "grey") +
          xlab("Town") +
          ylim(6,9) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("pH (Standard Units)") +
          labs(title="Summary of pH Concentration by Town")
        
        
      }
    }
    
    ggplotly(p1)
  })
  
  output$county_analyte <- renderPlotly({


    p2 <- {

      if(input$cty_analyte == "Nitrate"){
        ggplot(df, aes(x = factor(YEAR), y = Nitrate)) +
          geom_boxplot(notch = TRUE) +
          stat_summary(fun=mean, colour="blue", geom="point",
                       shape=18, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          geom_hline(yintercept = 10 , linetype = "dashed" , color = "red") +
          xlab("Year") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Nitrate-Nitrogen (mg/L)") +
          labs(title="Summary of Countywide Nitrate-Nitrogen Concentrations")


      }else if (input$cty_analyte == "Alkalinity") {
        ggplot(df, aes(x = factor(YEAR), y = Alkalinity)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point",
                       shape=18, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          xlab("Year") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Alkalinity (mg/L as CaCO3)") +
          labs(title="Summary of Countywide Alkalinity Concentrations")

      }else if (input$cty_analyte == "Chloride") {
        ggplot(df, aes(x = factor(YEAR), y = Chloride)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point",
                       shape=18, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          xlab("Year") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Chloride (mg/L)") +
          labs(title="Summary of Countywide Chloride Concentrations")

      }else if (input$cty_analyte == "Total Hardness") {
        ggplot(df, aes(x = factor(YEAR), y = Total.Hardness)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point",
                       shape=18, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          geom_hline(yintercept = 150 , linetype = "dashed" , color = "green") +
          geom_hline(yintercept = 200 , linetype = "dashed" , color = "purple") +
          xlab("Year") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Total Hardness (mg/L as CaCO3)") +
          labs(title="Summary of Countywide Total Hardness Concentrations")

      }else if (input$cty_analyte == "Conductivity") {
        ggplot(df, aes(x = factor(YEAR), y = Conductivity)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point",
                       shape=18, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          xlab("Year") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Conductivity (umhos/cm)") +
          labs(title="Summary of Countywide Conductivity Concentrations")

      }else if (input$cty_analyte == "Arsenic") {
        ggplot(df, aes(x = factor(YEAR), y = Arsenic)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point",
                       shape=18, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          geom_hline(yintercept = 0.01 , linetype = "dashed" , color = "grey") +
          xlab("Year") +
          ylim(0,0.10) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Arsenic (mg/L)") +
          labs(title="Summary of Countywide Arsenic Concentrations")

      }else if (input$cty_analyte == "Calcium") {
        ggplot(df, aes(x = factor(YEAR), y = Calcium)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point",
                       shape=18, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          #geom_hline(yintercept = 0.01 , linetype = "dashed" , color = "grey") +
          xlab("Year") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Calcium (mg/L)") +
          labs(title="Summary of Countywide Calcium Concentrations")
        
      }else if (input$cty_analyte == "Iron") {
        ggplot(df, aes(x = factor(YEAR), y = Iron)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point",
                       shape=18, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          #geom_hline(yintercept = 0.01 , linetype = "dashed" , color = "grey") +
          xlab("Year") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Iron (mg/L)") +
          labs(title="Summary of Countywide Iron Concentrations")
        
      }else if (input$cty_analyte == "Manganese") {
        ggplot(df, aes(x = factor(YEAR), y = Manganese)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point",
                       shape=18, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          #geom_hline(yintercept = 0.01 , linetype = "dashed" , color = "grey") +
          xlab("Year") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Manganese (mg/L)") +
          labs(title="Summary of Countywide Manganese Concentrations")
        
      }else if (input$cty_analyte == "Phosphorus") {
        ggplot(df, aes(x = factor(YEAR), y = P)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point",
                       shape=18, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          #geom_hline(yintercept = 0.01 , linetype = "dashed" , color = "grey") +
          xlab("Year") +
          ylim(-0.5,0.5) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Phosphorus (mg/L)") +
          labs(title="Summary of Countywide Phosphorus Concentrations")
        
      }else if (input$cty_analyte == "Sulfate") {
        ggplot(df, aes(x = factor(YEAR), y = Sulfate)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point",
                       shape=18, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          #geom_hline(yintercept = 0.01 , linetype = "dashed" , color = "grey") +
          xlab("Year") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Sulfate (mg/L)") +
          labs(title="Summary of Countywide Sulfate Concentrations")
        
      }else if (input$cty_analyte == "Potassium") {
        ggplot(df, aes(x = factor(YEAR), y = Potassium)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point",
                       shape=18, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          #geom_hline(yintercept = 0.01 , linetype = "dashed" , color = "grey") +
          xlab("Year") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Potassium (mg/L)") +
          labs(title="Summary of Countywide Potassium Concentrations")
        
      }else if (input$cty_analyte == "Magnesium") {
        ggplot(df, aes(x = factor(YEAR), y = Magnesium)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point",
                       shape=18, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          #geom_hline(yintercept = 0.01 , linetype = "dashed" , color = "grey") +
          xlab("Year") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Magnesium (mg/L)") +
          labs(title="Summary of Countywide Magnesium Concentrations")
        
      }else if (input$cty_analyte == "Sodium") {
        ggplot(df, aes(x = factor(YEAR), y = Sodium)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point",
                       shape=18, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          #geom_hline(yintercept = 0.01 , linetype = "dashed" , color = "grey") +
          xlab("Year") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Sodium (mg/L)") +
          labs(title="Summary of Countywide Sodium Concentrations")

      }else { #(input$analyte == "pH") {
        ggplot(df, aes(x = factor(YEAR), y = pH)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point",
                       shape=18, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          geom_hline(yintercept = 7 , linetype = "dashed" , color = "grey") +
          xlab("Year") +
          ylim(6,9) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("pH (Standard Units)") +
          labs(title="Summary of Countywide pH Concentrations")


      }
    }

    ggplotly(p2)
  })
  
  #Start of graphs for individual wells. Output for each analyte
  #nitrate
  output$well_nitrate <- renderPlotly({
    
    #Subsetting
    df2 <- subset(df , PROJECT_ID==input$r)
    
    w1 <- { 
      
      ggplot(df2, aes(x = YEAR, y = Nitrate)) +
        geom_point(size = 3, color = "black") +
        geom_line(color = "blue") +
        ylim(0,max(df2$Nitrate)+10) +
        geom_hline(yintercept = 10 , linetype = "dashed" , color = "red") +
        theme(axis.title.x = element_blank()) +
        scale_x_continuous(breaks = seq(min(df2$YEAR), max(df2$YEAR), by = 1)) +
        ylab("mg/L") +
        labs(title="Nitrate-Nitrogen")
      
    }
    ggplotly(w1)
  })
  
  #chloride
  #Start of graphs for individual wells. Output for each analyte
  output$well_chloride <- renderPlotly({
    
    #Subsetting
    df2 <- subset(df , PROJECT_ID==input$r)
    
    w2 <- { 
      
      ggplot(df2, aes(x = YEAR, y = Chloride)) +
        geom_point(size = 3, color = "black") +
        geom_line(color = "blue") +
        ylim(0.0,max(df2$Chloride)+10) +
        #annotation_logticks(sides = "l") +
        theme(axis.title.x = element_blank()) +
        scale_x_continuous(breaks = seq(min(df2$YEAR), max(df2$YEAR), by = 1)) +
        ylab("mg/L") +
        labs(title="Chloride")
      
    }
    ggplotly(w2)
  })
  
  #Alkalinity
  #Start of graphs for individual wells. Output for each analyte
  output$well_alkalinity <- renderPlotly({
    
    #Subsetting
    df2 <- subset(df , PROJECT_ID==input$r)
    
    w3 <- { 
      
      ggplot(df2, aes(x = YEAR, y = Alkalinity)) +
        geom_point(size = 3, color = "black") +
        geom_line(color = "blue") +
        ylim(0,550) +
        xlab("Year") +
        theme(axis.title.x = element_blank()) +
        scale_x_continuous(breaks = seq(min(df2$YEAR), max(df2$YEAR), by = 1)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
        ylab("mg/L as CaCO3") +
        labs(title="Alkalinity")
      
    }
    ggplotly(w3)
  })
  
  #Conductivity
  #Start of graphs for individual wells. Output for each analyte
  output$well_conductivity <- renderPlotly({
    
    #Subsetting
    df2 <- subset(df , PROJECT_ID==input$r)
    
    w4 <- { 
      
      ggplot(df2, aes(x = YEAR, y = Conductivity)) +
        geom_point(size = 3, color = "black") +
        geom_line(color = "blue") +
        ylim(min(df2$Conductivity)-50,max(df2$Conductivity)+50) +
        theme(axis.title.x = element_blank()) +
        scale_x_continuous(breaks = seq(min(df2$YEAR), max(df2$YEAR), by = 1)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
        ylab("umhos/cm") +
        labs(title="Conductivity")
      
    }
    ggplotly(w4)
  })
  
  #Total Hardness
  #Start of graphs for individual wells. Output for each analyte
  output$well_hardness <- renderPlotly({
    
    #Subsetting
    df2 <- subset(df , PROJECT_ID==input$r)
    
    w5 <- { 
      
      ggplot(df2, aes(x = YEAR, y = Total.Hardness)) +
        geom_point(size = 3, color = "black") +
        geom_line(color = "blue") +
        ylim(min(df2$Total.Hardness)-50,max(df2$Total.Hardness)+50) +
        theme(axis.title.x = element_blank()) +
        scale_x_continuous(breaks = seq(min(df2$YEAR), max(df2$YEAR), by = 1)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
        ylab("mg/L as CaCO3") +
        labs(title="Total Hardness")
      
    }
    ggplotly(w5)
  })
  
  #ph
  #Start of graphs for individual wells. Output for each analyte
  output$well_ph <- renderPlotly({
    
    #Subsetting
    df2 <- subset(df , PROJECT_ID==input$r)
    
    w6 <- { 
      
      ggplot(df2, aes(x = YEAR, y = pH)) +
        geom_point(size = 3, color = "black") +
        geom_line(color = "blue") +
        ylim(6,9) +
        xlab("Year") +
        scale_x_continuous(breaks = seq(min(df2$YEAR), max(df2$YEAR), by = 1)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
        ylab("Standard Units") +
        labs(title="pH")
      
    }
    ggplotly(w6)
  })
  
  #Arsenic
  #Start of graphs for individual wells. Output for each analyte
  output$well_Arsenic <- renderPlotly({
    
    #Subsetting
    df2 <- subset(df , PROJECT_ID==input$r)
    
    w7 <- { 
      
      ggplot(df2, aes(x = YEAR, y = Arsenic)) +
        geom_point(size = 3, color = "black") +
        geom_line(color = "blue") +
        #ylim(6,9) +
        xlab("Year") +
        scale_x_continuous(breaks = seq(min(df2$YEAR), max(df2$YEAR), by = 1)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
        ylab("mg/L") +
        labs(title="Arsenic")
      
    }
    ggplotly(w7)
  })
  
  #Calcium
  #Start of graphs for individual wells. Output for each analyte
  output$well_Calcium <- renderPlotly({
    
    #Subsetting
    df2 <- subset(df , PROJECT_ID==input$r)
    
    w8 <- { 
      
      ggplot(df2, aes(x = YEAR, y = Calcium)) +
        geom_point(size = 3, color = "black") +
        geom_line(color = "blue") +
        #ylim(6,9) +
        xlab("Year") +
        scale_x_continuous(breaks = seq(min(df2$YEAR), max(df2$YEAR), by = 1)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
        ylab("mg/L") +
        labs(title="Calcium")
      
    }
    ggplotly(w8)
  })
  
  #Iron
  #Start of graphs for individual wells. Output for each analyte
  output$well_Iron <- renderPlotly({
    
    #Subsetting
    df2 <- subset(df , PROJECT_ID==input$r)
    
    w9 <- { 
      
      ggplot(df2, aes(x = YEAR, y = Iron)) +
        geom_point(size = 3, color = "black") +
        geom_line(color = "blue") +
        #ylim(6,9) +
        xlab("Year") +
        scale_x_continuous(breaks = seq(min(df2$YEAR), max(df2$YEAR), by = 1)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
        ylab("mg/L") +
        labs(title="Iron")
      
    }
    ggplotly(w9)
  })
  
  #Manganese
  #Start of graphs for individual wells. Output for each analyte
  output$well_Manganese <- renderPlotly({
    
    #Subsetting
    df2 <- subset(df , PROJECT_ID==input$r)
    
    w10 <- { 
      
      ggplot(df2, aes(x = YEAR, y = Manganese)) +
        geom_point(size = 3, color = "black") +
        geom_line(color = "blue") +
        #ylim(6,9) +
        xlab("Year") +
        scale_x_continuous(breaks = seq(min(df2$YEAR), max(df2$YEAR), by = 1)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
        ylab("mg/L") +
        labs(title="Manganese")
      
    }
    ggplotly(w10)
  })
  
  #Phosphorus
  #Start of graphs for individual wells. Output for each analyte
  output$well_Phosphorus <- renderPlotly({
    
    #Subsetting
    df2 <- subset(df , PROJECT_ID==input$r)
    
    w11 <- { 
      
      ggplot(df2, aes(x = YEAR, y = P)) +
        geom_point(size = 3, color = "black") +
        geom_line(color = "blue") +
        #ylim(6,9) +
        xlab("Year") +
        scale_x_continuous(breaks = seq(min(df2$YEAR), max(df2$YEAR), by = 1)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
        ylab("mg/L") +
        labs(title="Phosphorus")
      
    }
    ggplotly(w11)
  })
  
  #Sulfate
  #Start of graphs for individual wells. Output for each analyte
  output$well_Sulfate <- renderPlotly({
    
    #Subsetting
    df2 <- subset(df , PROJECT_ID==input$r)
    
    w12 <- { 
      
      ggplot(df2, aes(x = YEAR, y = Sulfate)) +
        geom_point(size = 3, color = "black") +
        geom_line(color = "blue") +
        #ylim(6,9) +
        xlab("Year") +
        scale_x_continuous(breaks = seq(min(df2$YEAR), max(df2$YEAR), by = 1)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
        ylab("mg/L") +
        labs(title="Sulfate")
      
    }
    ggplotly(w12)
  })
  
  #Potassium
  #Start of graphs for individual wells. Output for each analyte
  output$well_Potassium <- renderPlotly({
    
    #Subsetting
    df2 <- subset(df , PROJECT_ID==input$r)
    
    w13 <- { 
      
      ggplot(df2, aes(x = YEAR, y = Potassium)) +
        geom_point(size = 3, color = "black") +
        geom_line(color = "blue") +
        #ylim(6,9) +
        xlab("Year") +
        scale_x_continuous(breaks = seq(min(df2$YEAR), max(df2$YEAR), by = 1)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
        ylab("mg/L") +
        labs(title="Potassium")
      
    }
    ggplotly(w13)
  })
  
  #Magnesium
  #Start of graphs for individual wells. Output for each analyte
  output$well_Magnesium <- renderPlotly({
    
    #Subsetting
    df2 <- subset(df , PROJECT_ID==input$r)
    
    w14 <- { 
      
      ggplot(df2, aes(x = YEAR, y = Magnesium)) +
        geom_point(size = 3, color = "black") +
        geom_line(color = "blue") +
        #ylim(6,9) +
        xlab("Year") +
        scale_x_continuous(breaks = seq(min(df2$YEAR), max(df2$YEAR), by = 1)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
        ylab("mg/L") +
        labs(title="Magnesium")
      
    }
    ggplotly(w14)
  })
  
  #Sodium
  #Start of graphs for individual wells. Output for each analyte
  output$well_Sodium <- renderPlotly({
    
    #Subsetting
    df2 <- subset(df , PROJECT_ID==input$r)
    
    w15 <- { 
      
      ggplot(df2, aes(x = YEAR, y = Sodium)) +
        geom_point(size = 3, color = "black") +
        geom_line(color = "blue") +
        #ylim(6,9) +
        xlab("Year") +
        scale_x_continuous(breaks = seq(min(df2$YEAR), max(df2$YEAR), by = 1)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
        ylab("mg/L") +
        labs(title="Sodium")
      
    }
    ggplotly(w15)
  })
  
  #Start of graphs for trends (all wells, all years). Output for each analyte
  #Nitrate
  output$trend_fig <- renderPlotly({
    
    df4 <- subset(df, cnt_sum==3)

    t1 <- {
      
      
      
      if(input$trend_analyte == "Nitrate"){
        #raster image for nitrate sorted by 2016 nitrate concentrations with same well located vertically below
        ggplot(df4, aes(x = reorder(PROJECT_ID, -Nitrate_mean), y = YEAR)) +
          geom_raster(aes(fill = Nitrate, 
                          text = paste('Project ID:', PROJECT_ID,
                                       '</br>Nitrate-N Mean (mg/L):', Nitrate_mean))) +
          scale_fill_gradient2(name="Nitrate-N \n(mg/L)", low="blue", high = "red", mid = "yellow", midpoint = 10) +
          xlab("Project ID") + 
          theme(axis.text.x = element_blank()) +
          ylab("Year") +
          scale_y_continuous(breaks = seq(2020,2022,1)) +
          labs(title="Nitrate-Nitrogen Concentration by Well & Year")
        
        
        
      }else if (input$trend_analyte == "Alkalinity") {
        ggplot(df4, aes(x = reorder(PROJECT_ID, -Alkalinity_mean), y = YEAR)) +
          geom_raster(aes(fill = Alkalinity, 
                          text = paste('Project ID:', PROJECT_ID,
                                       '</br>Alkalinity Mean (mg/L as CaCO3):', Alkalinity_mean))) +
          scale_fill_gradient2(name="Alkalinity \n(mg/L as CaCO3)", low="blue", high = "red", mid = "yellow", midpoint = 250) +
          xlab("Project ID") + 
          theme(axis.text.x = element_blank()) +
          ylab("Year") +
          scale_y_continuous(breaks = seq(2020,2022,1)) +
          labs(title="Alkalinity Concentration by Well & Year")
        
      }else if (input$trend_analyte == "Chloride") {
          ggplot(df4, aes(x = reorder(PROJECT_ID, -Chloride_mean), y = YEAR)) +
          geom_raster(aes(fill = Chloride, 
                          text = paste('Project ID:', PROJECT_ID,
                                       '</br>Chloride Mean (mg/L):', Chloride_mean))) +
          scale_fill_gradient2(name="Chloride \n(mg/L)", low="blue", high = "red", mid = "yellow", midpoint = 75) +
          xlab("Project ID") + 
          theme(axis.text.x = element_blank()) +
          ylab("Year") +
          scale_y_continuous(breaks = seq(2020,2022,1)) +
          labs(title="Chloride Concentration by Well & Year")
        
      }else if (input$trend_analyte == "Total Hardness") {
        ggplot(df4, aes(x = reorder(PROJECT_ID, -THard_mean), y = YEAR)) +
          geom_raster(aes(fill = Total.Hardness, 
                          text = paste('Project ID:', PROJECT_ID,
                                       '</br>Total Hardness Mean (mg/L as CaCO3):', THard_mean))) +
          scale_fill_gradient2(name="Total Hardness \n(mg/L as CaCO3)", low="blue", high = "red", mid = "yellow", midpoint = 150) +
          xlab("Project ID") + 
          theme(axis.text.x = element_blank()) +
          ylab("Year") +
          scale_y_continuous(breaks = seq(2020,2022,1)) +
          labs(title="Total Hardness Concentration by Well & Year")
        
      }else if (input$trend_analyte == "Conductivity") {
        ggplot(df4, aes(x = reorder(PROJECT_ID, -Conductivity_mean), y = YEAR)) +
          geom_raster(aes(fill = Conductivity, 
                          text = paste('Project ID:', PROJECT_ID,
                                       '</br>Conductivity Mean (umhos/cm):', Conductivity_mean))) +
          scale_fill_gradient2(name="Conductivity \n(umhos/cm)", low="blue", high = "red", mid = "yellow", midpoint = 750) +
          xlab("Project ID") + 
          theme(axis.text.x = element_blank()) +
          ylab("Year") +
          scale_y_continuous(breaks = seq(2020,2022,1)) +
          labs(title="Conductivity Concentration by Well & Year")
        
      }else { #(input$analyte == "pH") {
        ggplot(df4, aes(x = reorder(PROJECT_ID, -pH_mean), y = YEAR)) +
          geom_raster(aes(fill = pH, 
                          text = paste('Project ID:', PROJECT_ID,
                                       '</br>pH Mean (standard units):', pH_mean))) +
          scale_fill_gradient2(name="pH \n(standard units)", low="blue", high = "red", mid = "yellow", midpoint = 7.5) +
          xlab("Project ID") + 
          theme(axis.text.x = element_blank()) +
          ylab("Year") +
          scale_y_continuous(breaks = seq(2020,2022,1)) +
          labs(title="pH by Well & Year")
        
        
      }
    }
    
    ggplotly(t1)
  })
  
##Start of county summary table
  output$cty_summary_table <- renderRHandsontable({
    
    df4 <- subset(df, YEAR==input$cty_year)
 

    
    labs <- c("Nitrate","Alkalinity","pH","Conductivity","Total Hardness","Chloride","Arsenic","Calcium",
                       "Iron","Manganese","Phosphorus","Sulfate","Potassium","Magnesium","Sodium")
    table1 <- sumtable(df4, vars = c("Nitrate","Alkalinity","pH","Conductivity","THard","Chloride","Arsenic","Calcium",
                                           "Iron","Manganese","P","Sulfate","Potassium","Magnesium","Sodium"), 
                       summ=c('min(x)', 'mean(x)', 'median(x)', 'max(x)','notNA(x)'), 
                       summ.names = list(c('Minimum','Mean','Median','Maximum','# of Samples')),out = 'return', labels = labs)
    # df4_stats <- summary.data.frame(df4_table)
    # table1 <- as.data.frame.matrix(df4_stats)

    #Paste values into table
    # table1 <- matrix(c(paste0(t1_r1_c1), paste0(t1_r2_c1), paste0(t1_r3_c1), paste0(t1_r4_c1), paste0(t1_r5_c1),
    #                    paste0(t1_r1_c2), paste0(t1_r2_c2), paste0(t1_r3_c2), paste0(t1_r4_c2), paste0(t1_r5_c2),
    #                    paste0(t1_r1_c3), paste0(t1_r2_c3), paste0(t1_r3_c3), paste0(t1_r4_c3), paste0(t1_r5_c3),
    #                    paste0(t1_r1_c4), paste0(t1_r2_c4), paste0(t1_r3_c4), paste0(t1_r4_c4), paste0(t1_r5_c4),
    #                    paste0(t1_r1_c5), paste0(t1_r2_c5), paste0(t1_r3_c5), paste0(t1_r4_c5), paste0(t1_r5_c5), 
    #                    paste0(t1_r1_c6), paste0(t1_r2_c6), paste0(t1_r3_c6), paste0(t1_r4_c6), paste0(t1_r5_c6)),
    #                  nrow = 5, ncol = 6, dimnames = list(c("Minimum", "Mean", "Median","Maximum","# of Samples"),c("Total Hardness (mg/L as CaCO3)", "Alkalinity (mg/L as CaCO3)", "Conductivity (umhos/cm)","pH","Nitrate-Nitrogen (mg/L)","Chloride (mg/L)")))
    # 
    # # Turning data frame into RHandsontable
     rhandsontable(table1, rowHeaderWidth = 100, readOnly = TRUE) %>% hot_cols(colWidths = 100)
  })

  #Start of box plots by town 
  output$index_fig <- renderPlotly({
    
    #Subsetting
    
    
    df5 <- subset(df, cnt_sum==1)
    
    t2 <- {
      
      if(input$trend_analyte == "Nitrate"){
        ggplot(df5, aes(x = factor(YEAR), y = Nitrate)) +
          geom_boxplot(notch = TRUE) +
          stat_summary(fun=mean, colour="blue", geom="point", 
                       shape=18, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          geom_hline(yintercept = 10 , linetype = "dashed" , color = "red") +
          xlab("Year") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Nitrate-Nitrogen (mg/L)") +
          labs(title="Summary of Countywide Nitrate-Nitrogen Concentrations")
        
        
        
      }else if (input$trend_analyte == "Alkalinity") {
        ggplot(df5, aes(x = factor(YEAR), y = Alkalinity)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point", 
                       shape=18, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          xlab("Year") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Alkalinity (mg/L as CaCO3)") +
          labs(title="Summary of Countywide Alkalinity Concentrations")
        
      }else if (input$trend_analyte == "Chloride") {
        ggplot(df5, aes(x = factor(YEAR), y = Chloride)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point", 
                       shape=18, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          xlab("Year") +
          #scale_y_log10(limits = c(0.05,500))+
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Chloride (mg/L)") +
          labs(title="Summary of Countywide Chloride Concentrations")
        
      }else if (input$trend_analyte == "Total Hardness") {
        ggplot(df5, aes(x = factor(YEAR), y = Total.Hardness)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point", 
                       shape=18, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          geom_hline(yintercept = 150 , linetype = "dashed" , color = "green") +
          geom_hline(yintercept = 200 , linetype = "dashed" , color = "purple") +
          xlab("Year") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Total Hardness (mg/L as CaCO3)") +
          labs(title="Summary of Countywide Total Hardness Concentrations")
        
      }else if (input$trend_analyte == "Conductivity") {
        ggplot(df5, aes(x = factor(YEAR), y = Conductivity)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point", 
                       shape=18, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          xlab("Year") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("Conductivity (umhos/cm)") +
          labs(title="Summary of Countywide Conductivity Concentrations")
        
        
      }else { #(input$analyte == "pH") {
        ggplot(df5, aes(x = factor(YEAR), y = pH)) +
          geom_boxplot() +
          stat_summary(fun=mean, colour="blue", geom="point", 
                       shape=18, size=3) +
          #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
          geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
          geom_hline(yintercept = 7 , linetype = "dashed" , color = "grey") +
          xlab("Year") +
          ylim(6,9) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
          ylab("pH (Standard Units)") +
          labs(title="Summary of Countywide pH Concentrations")
        
        
      }
    }
    
    ggplotly(t2)
  })  
  
  #Start of graphs for well and land use categories
  #Agricultural land category
    output$wl_AG_CAT <- renderPlotly({
      
      #Subsetting
      df6 <- subset(df, cnt_sum==1)
    
      if(input$wl_analyte == "Nitrate"){
        WL1 <- { 
          
                ggplot(df6, aes(x = factor(YEAR), y = Nitrate, fill = factor(YEAR))) +
                  geom_boxplot() +
                  facet_grid(.~factor(ROWCROP_CAT)) +
                  stat_summary(fun=mean, colour="blue", geom="point",
                                shape=18, size=3) +
                  geom_hline(yintercept = 10 , linetype = "dashed" , color = "red") +
                  #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
                  geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
                  xlab("") +
                  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
                  ylab("") +
                  labs(title="Nitrate-Nitrogen (mg/L) by Percent Row Crops (500 m buffer)")
    
          
        }
        ggplotly(WL1)
        
      }else if (input$wl_analyte == "Chloride"){
        WL1 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Chloride, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(ROWCROP_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Chloride (mg/L) by Percent Row Crops (500 m buffer)")
          
          
        }
        ggplotly(WL1)
        
      }else if (input$wl_analyte == "Total Hardness"){
        WL1 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Total.Hardness, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(ROWCROP_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Total Hardness (mg/L as CaCO3) by Percent Row Crops  (500 m buffer)")
          }
        ggplotly(WL1)  
      
      }else if (input$wl_analyte == "Alkalinity"){
        WL1 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Alkalinity, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(ROWCROP_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Alkalinity (mg/L as CaCO3) by Percent Row Crops  (500 m buffer)")
        }
        ggplotly(WL1) 
        
      }else if (input$wl_analyte == "Conductivity"){
        WL1 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Conductivity, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(ROWCROP_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Conductivity (umhos/cm) by Percent Row Crops  (500 m buffer)")
        }
        ggplotly(WL1) 
        
      }else if (input$wl_analyte == "Arsenic"){
        WL1 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Arsenic, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(ROWCROP_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Arsenic (mg/L) by Percent Row Crops (500 m buffer)")
          
          
        }
        ggplotly(WL1)
        
      }else if (input$wl_analyte == "Calcium"){
        WL1 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Calcium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(ROWCROP_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Calcium (mg/L) by Percent Row Crops (500 m buffer)")
          
          
        }
        ggplotly(WL1)
        
      }else if (input$wl_analyte == "Iron"){
        WL1 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Iron, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(ROWCROP_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            ylim(0,10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Iron (mg/L) by Percent Row Crops (500 m buffer)")
          
          
        }
        ggplotly(WL1)
        
      }else if (input$wl_analyte == "Manganese"){
        WL1 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Manganese, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(ROWCROP_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            ylim(0,1) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Manganese (mg/L) by Percent Row Crops (500 m buffer)")
          
          
        }
        ggplotly(WL1)
        
      }else if (input$wl_analyte == "Phosphorus"){
        WL1 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = P, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(ROWCROP_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Phosphorus (mg/L) by Percent Row Crops (500 m buffer)")
          
          
        }
        ggplotly(WL1)
        
      }else if (input$wl_analyte == "Sulfate"){
        WL1 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Sulfate, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(ROWCROP_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Sulfate (mg/L) by Percent Row Crops (500 m buffer)")
          
          
        }
        ggplotly(WL1)
        
      }else if (input$wl_analyte == "Potassium"){
        WL1 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Potassium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(ROWCROP_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Potassium (mg/L) by Percent Row Crops (500 m buffer)")
          
          
        }
        ggplotly(WL1)
        
      }else if (input$wl_analyte == "Magnesium"){
        WL1 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Magnesium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(ROWCROP_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Magnesium (mg/L) by Percent Row Crops (500 m buffer)")
          
          
        }
        ggplotly(WL1)
        
      }else if (input$wl_analyte == "Sodium"){
        WL1 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Sodium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(ROWCROP_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Sodium (mg/L) by Percent Row Crops (500 m buffer)")
          
          
        }
        ggplotly(WL1)
      
      }else{ #(input$wl_analyte == "pH){
        WL1 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = pH, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(ROWCROP_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0+6, label = ..count..)) +
            xlab("") +
            ylim(6,9) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="pH (standard units) by Percent Row Crops  (500 m buffer)")
              }
        ggplotly(WL1)
      }
    })
  
    
    
    
    #Soil Drainage Classification
    output$wl_DRAIN_CAT <- renderPlotly({
      
      #Subsetting
      df6 <- subset(df, cnt_sum==1)
      
      if(input$wl_analyte == "Nitrate"){
        WL2 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Nitrate, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DRAIN_CATEGORY)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            geom_hline(yintercept = 10 , linetype = "dashed" , color = "red") +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Nitrate-Nitrogen (mg/L) by Soil Drainage Classification")
          
          
        }
        ggplotly(WL2)
        
      }else if (input$wl_analyte == "Chloride"){
        WL2 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Chloride, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DRAIN_CATEGORY)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Chloride (mg/L) by Soil Drainage Classification")
          
          
        }
        ggplotly(WL2)
        
      }else if (input$wl_analyte == "Total Hardness"){
        WL2 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Total.Hardness, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DRAIN_CATEGORY)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Total Hardness (mg/L as CaCO3) by Soil Drainage Classification")
        }
        ggplotly(WL2)  
        
      }else if (input$wl_analyte == "Alkalinity"){
        WL2 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Alkalinity, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DRAIN_CATEGORY)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Alkalinity (mg/L as CaCO3) by Soil Drainage Classification")
        }
        ggplotly(WL2) 
        
      }else if (input$wl_analyte == "Conductivity"){
        WL2 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Conductivity, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DRAIN_CATEGORY)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Conductivity (umhos/cm) by Soil Drainage Classification")
        }
        ggplotly(WL2)  
        
      }else if (input$wl_analyte == "Arsenic"){
        WL2 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Arsenic, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DRAIN_CATEGORY)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Arsenic (mg/L) by Soil Drainage Classification")
          
          
        }
        ggplotly(WL2)
        
      }else if (input$wl_analyte == "Calcium"){
        WL2 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Calcium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DRAIN_CATEGORY)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Calcium (mg/L) by Soil Drainage Classification")
          
          
        }
        ggplotly(WL2)
        
      }else if (input$wl_analyte == "Manganese"){
        WL2 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Manganese, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DRAIN_CATEGORY)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            ylim(0,1) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Manganese (mg/L) by Soil Drainage Classification")
          
          
        }
        ggplotly(WL2)
        
      }else if (input$wl_analyte == "Iron"){
        WL2 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Iron, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DRAIN_CATEGORY)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            ylim(0,10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Iron (mg/L) by Soil Drainage Classification")
          
          
        }
        ggplotly(WL2)
        
      }else if (input$wl_analyte == "Phosphorus"){
        WL2 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = P, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DRAIN_CATEGORY)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Phosphorus (mg/L) by Soil Drainage Classification")
          
          
        }
        ggplotly(WL2)
        
      }else if (input$wl_analyte == "Sulfate"){
        WL2 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Sulfate, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DRAIN_CATEGORY)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Sulfate (mg/L) by Soil Drainage Classification")
          
          
        }
        ggplotly(WL2)
        
      }else if (input$wl_analyte == "Potassium"){
        WL2 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Potassium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DRAIN_CATEGORY)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Potassium (mg/L) by Soil Drainage Classification")
          
          
        }
        ggplotly(WL2)
        
      }else if (input$wl_analyte == "Magnesium"){
        WL2 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Magnesium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DRAIN_CATEGORY)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Magnesium (mg/L) by Soil Drainage Classification")
          
          
        }
        ggplotly(WL2)
        
      }else if (input$wl_analyte == "Sodium"){
        WL2 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Sodium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DRAIN_CATEGORY)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Sodium (mg/L) by Soil Drainage Classification")
          
          
        }
        ggplotly(WL2)
        
      }else{ #(input$wl_analyte == "pH){
        WL2 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = pH, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DRAIN_CATEGORY)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0+6, label = ..count..)) +
            xlab("") +
            ylim(6,9) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            #theme(strip.text.x = str_wrap(element_text())) +
            ylab("") +
            labs(title="pH (standard units) by Soil Drainage Classification")
        }
        ggplotly(WL2)
      }
    })
    
  #Well Casing Category
    #Soil Drainage Classification
    output$wl_DB_WT_CAT <- renderPlotly({
      
      #Subsetting
      df6 <- subset(df, cnt_sum==1)
      
      if(input$wl_analyte == "Nitrate"){
        WL3 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Nitrate, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(WELL_DEPTH_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            geom_hline(yintercept = 10 , linetype = "dashed" , color = "red") +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Nitrate-Nitrogen (mg/L) by Casing Depth Below the Water Table (ft)")
          
          
        }
        ggplotly(WL3)
        
      }else if (input$wl_analyte == "Chloride"){
        WL3 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Chloride, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(WELL_DEPTH_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Chloride (mg/L) by Casing Depth Below the Water Table (ft)")
          
          
        }
        ggplotly(WL3)
        
      }else if (input$wl_analyte == "Total Hardness"){
        WL3 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Total.Hardness, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(WELL_DEPTH_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Total Hardness (mg/L as CaCO3) by Casing Depth Below the Water Table (ft)")
        }
        ggplotly(WL3)  
        
      }else if (input$wl_analyte == "Alkalinity"){
        WL3 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Alkalinity, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(WELL_DEPTH_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Alkalinity (mg/L as CaCO3) by Casing Depth Below the Water Table (ft)")
        }
        ggplotly(WL3) 
        
      }else if (input$wl_analyte == "Conductivity"){
        WL3 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Conductivity, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(WELL_DEPTH_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Conductivity (umhos/cm) by Casing Depth Below the Water Table (ft)")
        }
        ggplotly(WL3)    
        
      }else if (input$wl_analyte == "Arsenic"){
        WL3 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Arsenic, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(WELL_DEPTH_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Arsenic (mg/L) by Casing Depth Below the Water Table (ft)")
          
          
        }
        ggplotly(WL3)
        
      }else if (input$wl_analyte == "Calcium"){
        WL3 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Calcium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(WELL_DEPTH_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Calcium (mg/L) by Casing Depth Below the Water Table (ft)")
          
          
        }
        ggplotly(WL3)
        
      }else if (input$wl_analyte == "Iron"){
        WL3 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Iron, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(WELL_DEPTH_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            ylim(0,10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Iron (mg/L) by Casing Depth Below the Water Table (ft)")
          
          
        }
        ggplotly(WL3)
        
      }else if (input$wl_analyte == "Manganese"){
        WL3 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Manganese, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(WELL_DEPTH_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            ylim(0,1) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Manganese (mg/L) by Casing Depth Below the Water Table (ft)")
          
          
        }
        ggplotly(WL3)
        
      }else if (input$wl_analyte == "Phosphorus"){
        WL3 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = P, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(WELL_DEPTH_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Phosphorus (mg/L) by Casing Depth Below the Water Table (ft)")
          
          
        }
        ggplotly(WL3)
        
      }else if (input$wl_analyte == "Sulfate"){
        WL3 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Sulfate, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(WELL_DEPTH_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Sulfate (mg/L) by Casing Depth Below the Water Table (ft)")
          
          
        }
        ggplotly(WL3)
        
      }else if (input$wl_analyte == "Potassium"){
        WL3 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Potassium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(WELL_DEPTH_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Potassium (mg/L) by Casing Depth Below the Water Table (ft)")
          
          
        }
        ggplotly(WL3)
        
      }else if (input$wl_analyte == "Magnesium"){
        WL3 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Magnesium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(WELL_DEPTH_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Magnesium (mg/L) by Casing Depth Below the Water Table (ft)")
          
          
        }
        ggplotly(WL3)
        
      }else if (input$wl_analyte == "Sodium"){
        WL3 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Sodium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(WELL_DEPTH_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="sodium (mg/L) by Casing Depth Below the Water Table (ft)")
          
          
        }
        ggplotly(WL3)
        
      }else{ #(input$wl_analyte == "pH){
        WL3 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = pH, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(WELL_DEPTH_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0+6, label = ..count..)) +
            xlab("") +
            ylim(6,9) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="pH (standard units) by Casing Depth Below the Water Table (ft)")
        }
        ggplotly(WL3)
      }
    }) 
    
    #Well Casing Category
    output$wl_QUATERNARY <- renderPlotly({
      
      #Subsetting
      df6 <- subset(df, cnt_sum==1)
      
      if(input$wl_analyte == "Nitrate"){
        WL4 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Nitrate, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(MapUnit)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            geom_hline(yintercept = 10 , linetype = "dashed" , color = "red") +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Nitrate-Nitrogen (mg/L) by Quaternary Type")
          
          
        }
        ggplotly(WL4)
        
      }else if (input$wl_analyte == "Chloride"){
        WL4 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Chloride, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(MapUnit)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Chloride (mg/L) by Quaternary Type")
          
          
        }
        ggplotly(WL4)
        
      }else if (input$wl_analyte == "Total Hardness"){
        WL4 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Total.Hardness, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(MapUnit)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Total Hardness (mg/L as CaCO3) by Quaternary Type")
        }
        ggplotly(WL4)  
        
      }else if (input$wl_analyte == "Alkalinity"){
        WL4 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Alkalinity, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(MapUnit)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Alkalinity (mg/L as CaCO3) by Quaternary Type")
        }
        ggplotly(WL4) 
        
      }else if (input$wl_analyte == "Conductivity"){
        WL4 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Conductivity, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(MapUnit)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Conductivity (umhos/cm) by Quaternary Type")
        }
        ggplotly(WL4)   
        
      }else if (input$wl_analyte == "Arsenic"){
        WL4 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Arsenic, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(MapUnit)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Arsenic (mg/L) by Quaternary Type")
          
          
        }
        ggplotly(WL4)
        
      }else if (input$wl_analyte == "Calcium"){
        WL4 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Calcium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(MapUnit)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Calcium (mg/L) by Quaternary Type")
          
          
        }
        ggplotly(WL4)
        
      }else if (input$wl_analyte == "Iron"){
        WL4 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Iron, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(MapUnit)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            ylim(0,10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Iron (mg/L) by Quaternary Type")
          
          
        }
        ggplotly(WL4)
        
      }else if (input$wl_analyte == "Manganese"){
        WL4 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Manganese, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(MapUnit)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            ylim(0,1) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Manganese (mg/L) by Quaternary Type")
          
          
        }
        ggplotly(WL4)
        
      }else if (input$wl_analyte == "Phosphorus"){
        WL4 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = P, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(MapUnit)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Phosphorus (mg/L) by Quaternary Type")
          
          
        }
        ggplotly(WL4)
        
      }else if (input$wl_analyte == "Sulfate"){
        WL4 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Sulfate, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(MapUnit)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Sulfate (mg/L) by Quaternary Type")
          
          
        }
        ggplotly(WL4)
        
      }else if (input$wl_analyte == "Potassium"){
        WL4 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Potassium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(MapUnit)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Potassium (mg/L) by Quaternary Type")
          
          
        }
        ggplotly(WL4)
        
      }else if (input$wl_analyte == "Magnesium"){
        WL4 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Magnesium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(MapUnit)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Magnesium (mg/L) by Quaternary Type")
          
          
        }
        ggplotly(WL4)
        
      }else if (input$wl_analyte == "Sodium"){
        WL4 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Sodium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(MapUnit)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Sodium (mg/L) by Quaternary Type")
          
          
        }
        ggplotly(WL4)
        
      }else{ #(input$wl_analyte == "pH){
        WL4 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = pH, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(MapUnit)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0+6, label = ..count..)) +
            xlab("") +
            ylim(6,9) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="pH (standard units) by Quaternary Type")
        }
        ggplotly(WL4)
      }
    })   

    #All Agricultural land category
    output$wl_AG_HAY_PAST_CAT <- renderPlotly({
      
      #Subsetting
      df6 <- subset(df, cnt_sum==1)
      
      if(input$wl_analyte == "Nitrate"){
        WL5 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Nitrate, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(AG_HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            geom_hline(yintercept = 10 , linetype = "dashed" , color = "red") +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Nitrate-Nitrogen (mg/L) by Percent All Agriculture (500 m buffer)")
          
          
        }
        ggplotly(WL5)
        
      }else if (input$wl_analyte == "Chloride"){
        WL5 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Chloride, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(AG_HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Chloride (mg/L) by Percent All Agriculture (500 m buffer)")
          
          
        }
        ggplotly(WL5)
        
      }else if (input$wl_analyte == "Total Hardness"){
        WL5 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Total.Hardness, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(AG_HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Total Hardness (mg/L as CaCO3) by Percent All Agriculture (500 m buffer)")
        }
        ggplotly(WL5)  
        
      }else if (input$wl_analyte == "Alkalinity"){
        WL5 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Alkalinity, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(AG_HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Alkalinity (mg/L as CaCO3) by Percent All Agriculture (500 m buffer)")
        }
        ggplotly(WL5) 
        
      }else if (input$wl_analyte == "Conductivity"){
        WL5 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Conductivity, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(AG_HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Conductivity (umhos/cm) by Percent All Agriculture (500 m buffer)")
        }
        ggplotly(WL5)  
        
      }else if (input$wl_analyte == "Arsenic"){
        WL5 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Arsenic, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(AG_HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Arsenic (mg/L) by Percent All Agriculture (500 m buffer)")
          
          
        }
        ggplotly(WL5)
        
      }else if (input$wl_analyte == "Calcium"){
        WL5 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Calcium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(AG_HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Calcium (mg/L) by Percent All Agriculture (500 m buffer)")
          
          
        }
        ggplotly(WL5)
        
      }else if (input$wl_analyte == "Iron"){
        WL5 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Iron, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(AG_HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            ylim(0,10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Iron (mg/L) by Percent All Agriculture (500 m buffer)")
          
          
        }
        ggplotly(WL5)
        
      }else if (input$wl_analyte == "Manganese"){
        WL5 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Manganese, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(AG_HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            ylim(0,1) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Manganese (mg/L) by Percent All Agriculture (500 m buffer)")
          
          
        }
        ggplotly(WL5)
        
      }else if (input$wl_analyte == "Phosphorus"){
        WL5 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = P, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(AG_HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Phosphorus (mg/L) by Percent All Agriculture (500 m buffer)")
          
          
        }
        ggplotly(WL5)
        
      }else if (input$wl_analyte == "Sulfate"){
        WL5 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Sulfate, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(AG_HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Sulfate (mg/L) by Percent All Agriculture (500 m buffer)")
          
          
        }
        ggplotly(WL5)
        
      }else if (input$wl_analyte == "Potassium"){
        WL5 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Potassium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(AG_HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Potassium (mg/L) by Percent All Agriculture (500 m buffer)")
          
          
        }
        ggplotly(WL5)
        
      }else if (input$wl_analyte == "Magnesium"){
        WL5 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Magnesium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(AG_HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Magnesium (mg/L) by Percent All Agriculture (500 m buffer)")
          
          
        }
        ggplotly(WL5)
        
      }else if (input$wl_analyte == "Sodium"){
        WL5 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Sodium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(AG_HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Sodium (mg/L) by Percent All Agriculture (500 m buffer)")
          
          
        }
        ggplotly(WL5)
        
      }else{ #(input$wl_analyte == "pH){
        WL5 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = pH, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(AG_HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0+6, label = ..count..)) +
            xlab("") +
            ylim(6,9) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="pH (standard units) by Percent All Agriculture (500 m buffer)")
        }
        ggplotly(WL5)
      }
    })
    #All Agricultural land category
    output$wl_HAY_PAST_CAT <- renderPlotly({
      
      #Subsetting
      df6 <- subset(df, cnt_sum==1)
      
      if(input$wl_analyte == "Nitrate"){
        WL6 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Nitrate, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            geom_hline(yintercept = 10 , linetype = "dashed" , color = "red") +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Nitrate-Nitrogen (mg/L) by Percent Hay/Pasture (500 m buffer)")
          
          
        }
        ggplotly(WL6)
        
      }else if (input$wl_analyte == "Chloride"){
        WL6 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Chloride, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Chloride (mg/L) by Percent Hay/Pasture (500 m buffer)")
          
          
        }
        ggplotly(WL6)
        
      }else if (input$wl_analyte == "Total Hardness"){
        WL6 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Total.Hardness, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Total Hardness (mg/L as CaCO3) by Percent Hay/Pasture (500 m buffer)")
        }
        ggplotly(WL6)  
        
      }else if (input$wl_analyte == "Alkalinity"){
        WL6 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Alkalinity, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Alkalinity (mg/L as CaCO3) by Percent Hay/Pasture (500 m buffer)")
        }
        ggplotly(WL6) 
        
      }else if (input$wl_analyte == "Conductivity"){
        WL6 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Conductivity, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Conductivity (umhos/cm) by Percent Hay/Pasture (500 m buffer)")
        }
        ggplotly(WL6)    
        
      }else if (input$wl_analyte == "Arsenic"){
        WL6 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Arsenic, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Arsenic (mg/L) by Percent Hay/Pasture (500 m buffer)")
          
          
        }
        ggplotly(WL6)
        
      }else if (input$wl_analyte == "Calcium"){
        WL6 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Calcium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Calcium (mg/L) by Percent Hay/Pasture (500 m buffer)")
          
          
        }
        ggplotly(WL6)
        
      }else if (input$wl_analyte == "Iron"){
        WL6 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Iron, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            ylim(0,10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Iron (mg/L) by Percent Hay/Pasture (500 m buffer)")
          
          
        }
        ggplotly(WL6)
        
      }else if (input$wl_analyte == "Manganese"){
        WL6 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Manganese, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            ylim(0,1) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Manganese (mg/L) by Percent Hay/Pasture (500 m buffer)")
          
          
        }
        ggplotly(WL6)
        
      }else if (input$wl_analyte == "Phosphorus"){
        WL6 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = P, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Phosphorus (mg/L) by Percent Hay/Pasture (500 m buffer)")
          
          
        }
        ggplotly(WL6)
        
      }else if (input$wl_analyte == "Sulfate"){
        WL6 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Sulfate, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Sulfate (mg/L) by Percent Hay/Pasture (500 m buffer)")
          
          
        }
        ggplotly(WL6)
        
      }else if (input$wl_analyte == "Potassium"){
        WL6 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Potassium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Potassium (mg/L) by Percent Hay/Pasture (500 m buffer)")
          
          
        }
        ggplotly(WL6)
        
      }else if (input$wl_analyte == "Magnesium"){
        WL6 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Magnesium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Magnesium (mg/L) by Percent Hay/Pasture (500 m buffer)")
          
          
        }
        ggplotly(WL6)
        
      }else if (input$wl_analyte == "Sodium"){
        WL6 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Sodium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Sodium (mg/L) by Percent Hay/Pasture (500 m buffer)")
          
          
        }
        ggplotly(WL6)
        
      }else{ #(input$wl_analyte == "pH){
        WL6 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = pH, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(HAY_PAST_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0+6, label = ..count..)) +
            xlab("") +
            ylim(6,9) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="pH (standard units) by Percent Hay/Pasture (500 m buffer)")
        }
        ggplotly(WL6)
      }
    })
    
    #Dairy Rotation land category
    output$wl_DAIRY_CAT <- renderPlotly({
      
      #Subsetting
      df6 <- subset(df, cnt_sum==1)
      
      if(input$wl_analyte == "Nitrate"){
        WL7 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Nitrate, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DAIRY_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            geom_hline(yintercept = 10 , linetype = "dashed" , color = "red") +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Nitrate-Nitrogen (mg/L) by Percent Dairy Rotation  (500 m buffer)")
          
          
        }
        ggplotly(WL7)
        
      }else if (input$wl_analyte == "Chloride"){
        WL7 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Chloride, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DAIRY_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Chloride (mg/L) by Percent Dairy Rotation  (500 m buffer)")
          
          
        }
        ggplotly(WL7)
        
      }else if (input$wl_analyte == "Total Hardness"){
        WL7 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Total.Hardness, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DAIRY_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Total Hardness (mg/L as CaCO3) by Percent Dairy Rotation  (500 m buffer)")
        }
        ggplotly(WL7)  
        
      }else if (input$wl_analyte == "Alkalinity"){
        WL7 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Alkalinity, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DAIRY_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Alkalinity (mg/L as CaCO3) by Percent Dairy Rotation  (500 m buffer)")
        }
        ggplotly(WL7) 
        
      }else if (input$wl_analyte == "Conductivity"){
        WL7 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Conductivity, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DAIRY_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Conductivity (umhos/cm) by Percent Dairy Rotation  (500 m buffer)")
        }
        ggplotly(WL7)    
        
      }else if (input$wl_analyte == "Arsenic"){
        WL7 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Arsenic, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DAIRY_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Arsenic (mg/L) by Percent Dairy Rotation  (500 m buffer)")
          
          
        }
        ggplotly(WL7)
        
      }else if (input$wl_analyte == "Calcium"){
        WL7 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Calcium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DAIRY_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Calcium (mg/L) by Percent Dairy Rotation  (500 m buffer)")
          
          
        }
        ggplotly(WL7)
        
      }else if (input$wl_analyte == "Iron"){
        WL7 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Iron, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DAIRY_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            ylim(0,1) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Iron (mg/L) by Percent Dairy Rotation  (500 m buffer)")
          
          
        }
        ggplotly(WL7)
        
      }else if (input$wl_analyte == "Manganese"){
        WL7 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Manganese, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DAIRY_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            ylim(0,1) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Manganese (mg/L) by Percent Dairy Rotation  (500 m buffer)")
          
          
        }
        ggplotly(WL7)
        
      }else if (input$wl_analyte == "Phosphorus"){
        WL7 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = P, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DAIRY_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Phosphorus (mg/L) by Percent Dairy Rotation  (500 m buffer)")
          
          
        }
        ggplotly(WL7)
        
      }else if (input$wl_analyte == "Sulfate"){
        WL7 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Sulfate, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DAIRY_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Sulfate (mg/L) by Percent Dairy Rotation  (500 m buffer)")
          
          
        }
        ggplotly(WL7)
        
      }else if (input$wl_analyte == "Potassium"){
        WL7 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Potassium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DAIRY_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Potassium (mg/L) by Percent Dairy Rotation  (500 m buffer)")
          
          
        }
        ggplotly(WL7)
        
      }else if (input$wl_analyte == "Magnesium"){
        WL7 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Magnesium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DAIRY_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Magnesium (mg/L) by Percent Dairy Rotation  (500 m buffer)")
          
          
        }
        ggplotly(WL7)
        
      }else if (input$wl_analyte == "Sodium"){
        WL7 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Sodium, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DAIRY_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            #ylim(0,0.10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Sodium (mg/L) by Percent Dairy Rotation  (500 m buffer)")
          
          
        }
        ggplotly(WL7)
        
      }else{ #(input$wl_analyte == "pH){
        WL7 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = pH, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DAIRY_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0+6, label = ..count..)) +
            xlab("") +
            ylim(6,9) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="pH (standard units) by Percent Dairy Rotation (500 m buffer)")
        }
        ggplotly(WL7)
      }
    })
  
}
