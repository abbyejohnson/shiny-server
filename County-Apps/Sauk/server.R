
####SERVER FUNCTIONS####----------------------------------------------------------------------------------------------------

server = function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles("Stamen.TonerLite", group = "Toner Lite", 
                       options = providerTileOptions(minZoom = 5, maxZoom = 11)) %>% 
      fitBounds(lng1 = min(df$LONGITUDE, na.rm = TRUE), lat1 = min(df$LATITUDE, na.rm = TRUE), lng2 = max(df$LONGITUDE, na.rm = TRUE), lat2 = max(df$LATITUDE, na.rm = TRUE))
  })
  
  observeEvent({
    input$year
    input$var
    input$type
  }, 
  
  {
    if(input$year == "2019") {
      df1 <- df[df$YEAR == 2019,]
      munidf <- munidf[munidf$YEAR == 2019,]
    }else if(input$year == "2020") {
      df1 <- df[df$YEAR == 2020,]
      munidf <- munidf[munidf$YEAR == 2020,]
    }else if(input$year == "2021") {
      df1 <- df[df$YEAR == 2021,]
      munidf <- munidf[munidf$YEAR == 2021,]
    }else{
      df1 <- df[df$YEAR == 2022,]
      munidf <- munidf[munidf$YEAR == 2022,]
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
      mbreaks <- c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500)
      
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
      
      }else if(input$var == "rate"){
        mbreaks <- c(-7.5, -0.25, -0.1, 0, 0.1, 0.25, 0.50, (max.val+1))

        clrs <- rev(brewer.pal(length(mbreaks), "RdYlBu"))
        pal <- colorBin(palette = clrs, domain = fill.vals, bins = mbreaks)

      
    }else if(input$var == "pH"){
      mbreaks <- c(0, 6.5, 7.0, 7.5, 8.0, 8.5)
      clrs <- brewer.pal(length(mbreaks), "PRGn")
      pal <- colorBin(palette = clrs, domain = fill.vals, bins = mbreaks)
      
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
      
    }else if(input$var == "Top.Bedrock"){
      factpal <- colorFactor(rainbow(4), df$Top.Bedrock)


    }else { # input$var == "Hardness"
      mbreaks1 <- c(0, 100, 150, 200, 250, 300, 350, 400, max.val)
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
          addLegend("bottomleft", pal = pal, values = fill.vals, title = paste(input$var), opacity = 1)
        
      }else if (input$var == "rate"){
        
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
                                          "<br><strong>Rate of Change (mg/L per year): </strong>", round((df1$rate),1)),
                           stroke = T, color = "black", weight = 0.75, radius = ifelse(df1$significant2 == "no change", 3.5, (3.5 + abs(df1$rate*5))),
                           fillOpacity = 0.9, fillColor = ~pal(fill.vals)) %>%
          #addLegend("bottomleft", pal = pal, values = fill.vals, title = "Rate of Change (mg/L per year)", opacity = 1)
          addLegend("bottomleft", pal = pal, values = c(-2, -0.25, -0.1, 0, 0.1, 0.25, 0.50, 2), title = "Rate of Change (mg/L per year)", opacity = 1)
        
        
        
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
          addLegend("bottomleft", pal = pal1, values = fill.vals, title = paste(input$var), opacity = 1)
        
        
        
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
          addLegend("bottomleft", pal = pal1, values = fill.vals, title = paste(input$var), opacity = 1)
        
        
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
          addLegend("bottomleft", pal = pal, values = fill.vals, title = paste(input$var), opacity = 1)
        
        
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
        
      }else if (input$var == "Top.Bedrock"){

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
                                          "<br>Bedrock type: ", as.factor(df1$Top.Bedrock)),
                           stroke = T, color = "black", weight = 0.75, radius = 5,
                           fillOpacity = 0.9, fillColor = ~factpal(Top.Bedrock))  #%>%
          #addLegend("bottomleft", factpal = factpal, color = Top.Bedrock, title = "Uppermost Bedrock Type", opacity = 1)
        
        
        
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
          addLegend("bottomleft", pal = pal1, values = fill.vals, title = paste(input$var), opacity = 1)
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
          addLegend("bottomleft", pal = pal, values = fill.vals, title = paste(input$var), opacity = 1)
        
        
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
          addLegend("bottomleft", pal = pal, values = fill.vals, title = paste(input$var), opacity = 1)
        
        
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
          addLegend("bottomleft", pal = pal2, values = fill.vals, title = paste(input$var), opacity = 1)
        
        
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
          addLegend("bottomleft", pal = pal2, values = fill.vals, title = paste(input$var), opacity = 1)
        
        
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
          addLegend("bottomleft", pal = pal, values = fill.vals, title = paste(input$var), opacity = 1)
      
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
                                     "<br><strong>Casing depth below the water table (ft)</strong>",
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
      
      }else if (input$var == "Top.Bedrock"){
        
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
        
      }else if (input$var == "rate"){
        
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
        
      }else { #(input$analyte == "pH") {
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
        ylim(4,10) +
        xlab("Year") +
        scale_x_continuous(breaks = seq(min(df2$YEAR), max(df2$YEAR), by = 1)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
        ylab("Standard Units") +
        labs(title="pH")
      
    }
    ggplotly(w6)
  })
  
  #Start of graphs for trends (all wells, all years). Output for each analyte
  #Nitrate
  output$trend_fig <- renderPlotly({
    
    df4 <- subset(df, cnt_sum==4)

    t1 <- {
      
      
      
      if(input$trend_analyte == "Nitrate"){
        #raster image for nitrate sorted by 2016 nitrate concentrations with same well located vertically below
        ggplot(df4, aes(x = reorder(PROJECT_ID, -Nitrate_mean), y = YEAR)) +
          geom_raster(aes(fill = Nitrate, 
                          text = paste('Project ID:', PROJECT_ID,
                                       '</br>Nitrate-N Mean (mg/L):', Nitrate_mean))) +
          scale_fill_gradient2(name="Nitrate-N \n(mg/L)", low="blue", high = "red", mid = "yellow", midpoint = 20) +
          xlab("Project ID") + 
          theme(axis.text.x = element_blank()) +
          ylab("Year") +
          scale_y_continuous(breaks = seq(2019,2022,1)) +
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
          scale_y_continuous(breaks = seq(2019,2022,1)) +
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
          scale_y_continuous(breaks = seq(2019,2022,1)) +
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
          scale_y_continuous(breaks = seq(2019,2022,1)) +
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
          scale_y_continuous(breaks = seq(2019,2022,1)) +
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
          scale_y_continuous(breaks = seq(2019,2022,1)) +
          labs(title="pH by Well & Year")
        
        
      }
    }
    
    ggplotly(t1)
  })
  
##Start of county summary table
  output$cty_summary_table <- renderRHandsontable({
    
    df4 <- subset(df, YEAR==input$cty_year)
 
#Generate values to populate an annual summmary Table with 5 rows x 6 columns
     t1_r1_c1 <- min(df4$Total.Hardness, na.rm = TRUE)
     t1_r1_c2 <- min(df4$Alkalinity, na.rm = TRUE)
     t1_r1_c3 <- min(df4$Conductivity, na.rm = TRUE)
     t1_r1_c4 <- min(df4$pH, na.rm = TRUE)
     t1_r1_c5 <- ifelse(min(df4$Nitrate, na.rm = TRUE), "< 0.1", min(df4$Nitrate, na.rm = TRUE))
     t1_r1_c6 <- min(df4$Chloride, na.rm = TRUE)
     t1_r2_c1 <- round(mean(df4$Total.Hardness, na.rm = TRUE), digits = 1)
     t1_r2_c2 <- round(mean(df4$Alkalinity, na.rm = TRUE), digits = 1)
     t1_r2_c3 <- round(mean(df4$Conductivity, na.rm = TRUE), digits = 1)
     t1_r2_c4 <- round(mean(df4$pH, na.rm = TRUE), digits = 1)
     t1_r2_c5 <- ifelse(mean(df4$Nitrate, na.rm = TRUE) < 0.1, "< 0.1", round(mean(df4$Nitrate, na.rm = TRUE), digits = 1))
     #t1_r2_c5 <- round(mean(df4$Nitrate, na.rm = TRUE), digits = 1)
     t1_r2_c6 <- round(mean(df4$Chloride, na.rm = TRUE), digits = 1)
     t1_r3_c1 <- median(df4$Total.Hardness, na.rm = TRUE)
     t1_r3_c2 <- median(df4$Alkalinity, na.rm = TRUE)
     t1_r3_c3 <- median(df4$Conductivity, na.rm = TRUE)
     t1_r3_c4 <- median(df4$pH, na.rm = TRUE)
     t1_r3_c5 <- median(df4$Nitrate, na.rm = TRUE)
     t1_r3_c6 <- median(df4$Chloride, na.rm = TRUE)
     t1_r4_c1 <- max(df4$Total.Hardness, na.rm = TRUE)
     t1_r4_c2 <- max(df4$Alkalinity, na.rm = TRUE)
     t1_r4_c3 <- max(df4$Conductivity, na.rm = TRUE)
     t1_r4_c4 <- max(df4$pH, na.rm = TRUE)
     t1_r4_c5 <- max(df4$Nitrate, na.rm = TRUE)
     t1_r4_c6 <- max(df4$Chloride, na.rm = TRUE)
     t1_r5_c1 <- (sum(df4$cnt) - sum(df4$cnt_Thard))
     t1_r5_c2 <- sum(df4$cnt)
     t1_r5_c3 <- sum(df4$cnt)
     t1_r5_c4 <- sum(df4$cnt)
     t1_r5_c5 <- sum(df4$cnt)
     t1_r5_c6 <- sum(df4$cnt)

    #Paste values into table
    table1 <- matrix(c(paste0(t1_r1_c1), paste0(t1_r2_c1), paste0(t1_r3_c1), paste0(t1_r4_c1), paste0(t1_r5_c1),
                       paste0(t1_r1_c2), paste0(t1_r2_c2), paste0(t1_r3_c2), paste0(t1_r4_c2), paste0(t1_r5_c2),
                       paste0(t1_r1_c3), paste0(t1_r2_c3), paste0(t1_r3_c3), paste0(t1_r4_c3), paste0(t1_r5_c3),
                       paste0(t1_r1_c4), paste0(t1_r2_c4), paste0(t1_r3_c4), paste0(t1_r4_c4), paste0(t1_r5_c4),
                       paste0(t1_r1_c5), paste0(t1_r2_c5), paste0(t1_r3_c5), paste0(t1_r4_c5), paste0(t1_r5_c5), 
                       paste0(t1_r1_c6), paste0(t1_r2_c6), paste0(t1_r3_c6), paste0(t1_r4_c6), paste0(t1_r5_c6)),
                     nrow = 5, ncol = 6, dimnames = list(c("Minimum", "Mean", "Median","Maximum","# of Samples"),c("Total Hardness (mg/L as CaCO3)", "Alkalinity (mg/L as CaCO3)", "Conductivity (umhos/cm)","pH","Nitrate-Nitrogen (mg/L)","Chloride (mg/L)")))
    
    # Turning data frame into RHandsontable
    rhandsontable(table1, rowHeaderWidth = 100, readOnly = TRUE) %>% hot_cols(colWidths = 100)
  })

  #Start of box plots by town 
  output$index_fig <- renderPlotly({
    
    #Subsetting
    
    
    df5 <- subset(df, cnt_sum==4)
    
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
      df6 <- subset(df, cnt_sum==4)
    
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
      df6 <- subset(df, cnt_sum==4)
      
      if(input$wl_analyte == "Nitrate"){
        WL2 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Nitrate, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DRAIN_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            geom_hline(yintercept = 10 , linetype = "dashed" , color = "red") +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Nitrate-Nitrogen (mg/L) by Soil Drainage Classification (500 m buffer)")
          
          
        }
        ggplotly(WL2)
        
      }else if (input$wl_analyte == "Chloride"){
        WL2 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Chloride, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DRAIN_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Chloride (mg/L) by Soil Drainage Classification (500 m buffer)")
          
          
        }
        ggplotly(WL2)
        
      }else if (input$wl_analyte == "Total Hardness"){
        WL2 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Total.Hardness, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DRAIN_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Total Hardness (mg/L as CaCO3) by Soil Drainage Classification (500 m buffer)")
        }
        ggplotly(WL2)  
        
      }else if (input$wl_analyte == "Alkalinity"){
        WL2 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Alkalinity, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DRAIN_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Alkalinity (mg/L as CaCO3) by Soil Drainage Classification (500 m buffer)")
        }
        ggplotly(WL2) 
        
      }else if (input$wl_analyte == "Conductivity"){
        WL2 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Conductivity, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DRAIN_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Conductivity (umhos/cm) by Soil Drainage Classification(500 m buffer)")
        }
        ggplotly(WL2)    
        
      }else{ #(input$wl_analyte == "pH){
        WL2 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = pH, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(DRAIN_CAT)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0+6, label = ..count..)) +
            xlab("") +
            ylim(6,9) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="pH (standard units) by Soil Drainage Classification (500 m buffer)")
        }
        ggplotly(WL2)
      }
    })
    
  #Well Casing Category
    #Soil Drainage Classification
    output$wl_DB_WT_CAT <- renderPlotly({
      
      #Subsetting
      df6 <- subset(df, cnt_sum==4)
      
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
    output$wl_BEDROCK.TOP <- renderPlotly({
      
      #Subsetting
      df6 <- subset(df, cnt_sum==4)
      
      if(input$wl_analyte == "Nitrate"){
        WL4 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Nitrate, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(Top.Bedrock)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            geom_hline(yintercept = 10 , linetype = "dashed" , color = "red") +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Nitrate-Nitrogen (mg/L) by Uppermost Bedrock Type")
          
          
        }
        ggplotly(WL4)
        
      }else if (input$wl_analyte == "Chloride"){
        WL4 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Chloride, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(Top.Bedrock)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            #scale_y_log10(limits = c(0.05,500)) +
            #annotation_logticks(sides = "l") +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Chloride (mg/L) by Bedrock Type")
          
          
        }
        ggplotly(WL4)
        
      }else if (input$wl_analyte == "Total Hardness"){
        WL4 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Total.Hardness, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(Top.Bedrock)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Total Hardness (mg/L as CaCO3) by Bedrock Type")
        }
        ggplotly(WL4)  
        
      }else if (input$wl_analyte == "Alkalinity"){
        WL4 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Alkalinity, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(Top.Bedrock)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Alkalinity (mg/L as CaCO3) by Bedrock Type")
        }
        ggplotly(WL4) 
        
      }else if (input$wl_analyte == "Conductivity"){
        WL4 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = Conductivity, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(Top.Bedrock)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0-1, label = ..count..)) +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 0.09), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="Conductivity (umhos/cm) by Bedrock Type")
        }
        ggplotly(WL4)    
        
      }else{ #(input$wl_analyte == "pH){
        WL4 <- { 
          
          ggplot(df6, aes(x = factor(YEAR), y = pH, fill = factor(YEAR))) +
            geom_boxplot() +
            facet_grid(.~factor(Top.Bedrock)) +
            stat_summary(fun=mean, colour="blue", geom="point",
                         shape=18, size=3) +
            #the following line is necessary to label the boxplots with the sample number. count*0-1 tricks geom_text to plotting below the xaxis.
            geom_text(stat = "count", aes(y = (..count..)*0+6, label = ..count..)) +
            xlab("") +
            ylim(6,9) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 11.5, face="bold")) +
            ylab("") +
            labs(title="pH (standard units) by Bedrock Type")
        }
        ggplotly(WL4)
      }
    })   

    #All Agricultural land category
    output$wl_AG_HAY_PAST_CAT <- renderPlotly({
      
      #Subsetting
      df6 <- subset(df, cnt_sum==4)
      
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
      df6 <- subset(df, cnt_sum==4)
      
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
      df6 <- subset(df, cnt_sum==4)
      
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
