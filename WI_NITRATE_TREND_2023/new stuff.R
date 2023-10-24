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



ggplot(WI_stats, aes(x=Trend, y=Percent, fill=Trend)) +
  geom_bar(stat="identity")

ggplot() +
  labs(title = "Chlorophyll Over Time") +
  theme(legend.title = element_blank()) +
  ylab("Chlorophyll a ug/L") + 
  geom_point(data = well.trends2,
             aes(x = ate,
                 y = YEAR,
                 colour = factor(shint_trend)),
             colour = "purple2",
             cex = 3, 
             na.rm = TRUE)




qplot(well.trends, Rc, data=rc.data, geom="boxplot") + 
  geom_point(aes(color=factor(MAP))) +
  stat_summary(fun.y=mean, geom="line", aes(colour=factor(MAP), group=factor(MAP))) +
  stat_summary(fun.y=mean, geom="point", aes(colour=factor(MAP), pch=3, size=3))
               
               
               


ggplot(well.trends2, aes(x=YEAR, y=Nitrate.mean, fill=shiny_trend))+
 geom_point(data = well.trends2, size = 4.0, colour="black", shape=21, show.legend = F)+
 scale_fill_manual(values = c("#08519c", "#6baed6", "grey", "pink", "#fb8072"))



  
  
  
  
ggplot(dfclean, aes(x=YEAR, y=Result.amount, fill=sig_change))+
  geom_point(aes(color=factor(sig_change))) +
  stat_summary(fun.y=mean, geom="line", aes(colour=factor(sig_change), group=factor(sig_change))) +
  stat_summary(fun.y=mean, geom="point", aes(colour=factor(sig_change)))



#Start of code to generate pie chart of statewide trends.
cols = c("Significant Decrease (>2.5 mg/L per decade change)" = "#08519c",
         "Significant Increase (>2.5 mg/L per decade change)" = "#fb8072",
         "No Significant Trend" = "grey",
         "Slight Decrease (1.0-2.5 mg/L per decade change)" = "#6baed6",
         "Slight Increase (1.0-2.5 mg/L per decade change)" = "pink")



cols2 = c("Significant Decrease" = "#08519c",
         "Significant Increase" = "#fb8072",
         "No Significant Change" = "grey",
         "Slight Decrease" = "#6baed6",
         "Slight Increase" = "pink"
          )






summary_stats <- dfclean %>% inner_join(well.trends2[c("WI.unique.well..", "shiny_trend")], by = "WI.unique.well..") %>%
                             group_by(shiny_trend, YEAR) %>%
                             summarise(`Average Concentration (mg/L)` = mean(Result.amount), `Maximum Concentration (mg/L)` = max(Result.amount), 
                                       `75th Percentile (mg/L)` = quantile(Result.amount, probs = c(0.75)), `50th Percentile (mg/L)` = quantile(Result.amount, probs = c(0.50)),
                                       `25th Percentile (mg/L)` = quantile(Result.amount, probs = c(0.25)), `Minimum Concentration (mg/L)`= min(Result.amount), 
                                       `Number of Samples` = n()) %>%
                             rename("Nitrate Trend" = "shiny_trend", "Year" = "YEAR")


# summary_stats$`Nitrate Trend` <- as.factor(summary_stats$`Nitrate Trend`)

ggplotly(
  ggplot(summary_stats, aes(x = Year, y = `Average Concentration (mg/L)`, fill = `Nitrate Trend`)) + 
    geom_point(aes(), size = 3.0) + 
    geom_line(aes()) + 
    theme(legend.position="none",
          panel.background = element_rect(fill='white')) +
      scale_fill_manual(values = cols2)
)



  scale_fill_manual(values = c("#08519c", "#6baed6", "grey", "pink", "#fb8072"))

  
  
  
  
  
  
  summary_stats2 <- dfclean %>% group_by(YEAR) %>%
    summarise(`Average Concentration (mg/L)` = mean(Result.amount), `Maximum Concentration (mg/L)` = max(Result.amount), 
              `75th Percentile (mg/L)` = quantile(Result.amount, probs = c(0.75)), `50th Percentile (mg/L)` = quantile(Result.amount, probs = c(0.50)),
              `25th Percentile (mg/L)` = quantile(Result.amount, probs = c(0.25)), `Minimum Concentration (mg/L)`= min(Result.amount), 
              `Number of Samples` = n()) %>%
    rename("Year" = "YEAR")
  
  
  
  text2 <- paste("Nitrate Trend:", summary_stats2$`Nitrate Trend`, 
                "\nYear:", summary_stats2$Year,
                "\nMean Concentration (mg/L):", round(summary_stats2$`Average Concentration (mg/L)`, 2),
                "\nMaximum:", summary_stats2$`Maximum Concentration (mg/L)`,
                "\n75th Percentile:", summary_stats2$`75th Percentile (mg/L)`,
                "\n50th Percentile:", summary_stats2$`50th Percentile (mg/L)`,
                "\n25th Percentile:", summary_stats2$`25th Percentile (mg/L)`,
                "\nMinimum:", summary_stats2$`Minimum Concentration (mg/L)`,
                "\nNumber Of Samples:", summary_stats2$`Number of Samples`)

  ggplotly(
    ggplot(summary_stats2, aes(x = Year)) +
      scale_y_continuous(breaks = seq(0, 3, 1)) +
      geom_pointrange(aes(y = `Average Concentration (mg/L)`, ymin = `25th Percentile (mg/L)`, ymax = `75th Percentile (mg/L)`),
                      fill='white', color='black', shape=21, fatten = 15, size = 3) +
      scale_x_continuous(breaks = seq(1995, currentYear, 1)) + 
      theme(panel.background = element_rect(fill='white'),
            axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"))
  )
  
  
  
  leaflet() %>% 
    addProviderTiles("Stamen.TonerLite", group = "Toner Lite", 
                     # set the maximum zoom level of the map - should meet the requirment of not showing exact location
                     options = providerTileOptions(minZoom = 2, maxZoom = 10)) %>% 
    # set the default zoom level by fitting bounds to the spatial extent of the state
    fitBounds(lng1 = -91.9, lat1 = 43.45, lng2 = -87.7, lat2 = 46.1) 
  
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
                     stroke = T, color = "black", weight = 0.75, radius = 3.5,
                     fillOpacity = 1.0, fillColor = ~pal(fill.vals)) %>% 
    addLegend("bottomleft", colors = c("#08519c", "#6baed6"),
              labels = c("Significant Decrease","Slight Decrease"),
              title = paste("Nitrate-Nitrogen Change"),
              opacity = 1)
  
  