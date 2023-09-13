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