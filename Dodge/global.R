library(shiny)
library(leaflet)
library(sp)
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
library(ggpubr)
library(rhandsontable)

#Change to reflect date of data updates
modified <- "April 23, 2021"

df <- read.csv("ALL_COUNTIES_DATA_MERGE_20220329.csv", header = TRUE)

df <- df[df$COUNTY == "DODGE",]

df_stats <- df %>% group_by(PROJECT_ID) %>% summarise_at(c("Nitrate","Alkalinity","pH","Conductivity","THard","Chloride","cnt","DB_WT"), 
                                                         list(mean=mean, median=median, min=min,max=max, sum=sum, sd=sd), na.rm = TRUE)
df_stats$nitrate_diff <- df_stats$Nitrate_max - df_stats$Nitrate_min
df <- left_join(df,df_stats,by = "PROJECT_ID")
df$nitrate_direction <- ifelse(df$Nitrate_mean < df$Nitrate, df$nitrate_diff, -1*df$nitrate_diff)
df$Top.Bedrock <- ifelse(df$Top.Bedrock == "SHALE" | df$Top.Bedrock == "", "No Info", df$Top.Bedrock)
df$TOWNSHIP <- toupper(df$TOWNSHIP)

#!!!!!!!!!New code for well construction
df$WELL_DEPTH_CAT<- factor(df$WELL_DEPTH_CAT, levels=c("ABOVE","0-25 FT","25-50 FT","50-75 FT","75-100 FT","> 100 FT","Not Available"),
                           labels=c("ABOVE","0-25 FT","25-50 FT","50-75 FT","75-100 FT","> 100 FT","Not Available"))

#!!!!!!!!!New code for well construction
df$DRAIN_CAT<- factor(df$DRAIN_CAT, levels=c("Very Poorly Drained","Poorly Drained","Somewhat Poorly Drained","Moderately Well Drained","Well Drained","Somewhat Excessively Drained","Excessively Drained","Not Available"),
                           labels=c("Very Poorly Drained","Poorly Drained","Somewhat Poorly Drained","Moderately Well Drained","Well Drained","Somewhat Excessively Drained","Excessively Drained","Not Available"))

#!!!!!!!!!New code for ag_perc
df$AG_CAT<- factor(df$AG_CAT, levels=c("0-25%","25-50%","50-75%","75-100%","Not Available"),
                      labels=c("0-25%","25-50%","50-75%","75-100%","Not Available"))

#!!!!!!!!!New code for dairy
df$DAIRY_CAT<- factor(df$DAIRY_CAT, levels=c("0-25%","25-50%","50-75%","75-100%","Not Available"),
                   labels=c("0-25%","25-50%","50-75%","75-100%","Not Available"))

#!!!!!!!!!New code for crop
df$ROWCROP_CAT<- factor(df$ROWCROP_CAT, levels=c("0-25%","25-50%","50-75%","75-100%","Not Available"),
                      labels=c("0-25%","25-50%","50-75%","75-100%","Not Available"))

#!!!!!!!!!New code for hay/pasture
df$HAY_PAST_CAT<- factor(df$HAY_PAST_CAT, levels=c("0-25%","25-50%","50-75%","75-100%","Not Available"),
                            labels=c("0-25%","25-50%","50-75%","75-100%","Not Available"))

#!!!!!!!!!New code for all ag
df$AG_HAY_PAST_CAT<- factor(df$AG_HAY_PAST_CAT, levels=c("0-25%","25-50%","50-75%","75-100%","Not Available"),
                      labels=c("0-25%","25-50%","50-75%","75-100%","Not Available"))

#!!!!!!!!!New code for urban
df$URBAN_CAT<- factor(df$URBAN_CAT, levels=c("0-2.5%","2.5-5%","5-30%",">30%","Not Available"),
                   labels=c("0-2.5%","2.5-5%","5-30%",">30%","Not Available"))

#!!!!!!!!!New code for bedrock
df$Top.Bedrock<- factor(df$Top.Bedrock, levels=c("DOLOMITE","SANDSTONE","UNCONSOLIDATED","No Info"),
                   labels=c("DOLOMITE","SANDSTONE","UNCONSOLIDATED","No Info"))





munidf <- df %>% 
  group_by(TOWNSHIP,YEAR) %>% 
  summarise_at(c("Nitrate", "Alkalinity","Total.Hardness","Conductivity","pH","Chloride","AG_PERC","HAY_PAST","AG_HAY_PAST","ROWCROP","DAIRY_PERC","weighted.rank","DB_WT","cnt","cnt_Thard"), 
               list(mean=mean, median=median, min=min,max=max,n=sum), na.rm = TRUE) %>%
  rename_("Nitrate" = "Nitrate_mean",
         "Alkalinity" = "Alkalinity_mean",
         "Total.Hardness" = "Total.Hardness_mean",
         "Conductivity" = "Conductivity_mean",
         "pH" = "pH_mean",
         "Chloride" = "Chloride_mean",
         "weighted.rank" = "weighted.rank_mean",
         "DB_WT" = "DB_WT_mean",
         "AG_PERC" = "AG_PERC_mean",
         "HAY_PAST" = "HAY_PAST_mean",
         "DAIRY_PERC" = "DAIRY_PERC_mean",
         "AG_HAY_PAST" = "AG_HAY_PAST_mean",
         "ROWCROP" = "ROWCROP_mean")


munidf$Municipality <- munidf$TOWNSHIP

shape <- readOGR(dsn = "Dodge_Municipal_LayerSource.shp", stringsAsFactors = FALSE)
counties <- readRDS("WIcounties.rds")

colnames(df)


shape@data$Municipality <- as.character(shape@data$MCD_NAME)



logo <- tags$a(tags$img(src='logo.png', height = '75', width ='150', align = 'center'))
uwsp <- tags$a(tags$img(src='uwsp.png', height = '75', width = '325', style='float:right; margin-left: 5px; margin-right: 5px'))
uwex <- tags$a(tags$img(src='uwex.png', height = '75', width = '275', style='float:right; margin-left: 5px; margin-right: 5px'))
nitrate_health <- tags$a(tags$img(src='nitrate_health.png', height = '500', width = '750', align = 'center'))
nitrate_interp <- tags$a(tags$img(src='nitrate_interp.png', height = '250', width = '375', align = 'center'))
soils <- tags$a(tags$img(src='soils.png', height = '600', width = '750', align = 'center'))
wiscland <- tags$a(tags$img(src='wiscland.png', height = '600', width = '750', align = 'center'))
casing <- tags$a(tags$img(src='casing.png', height = '500', width = '750', align = 'center'))
leaching_potential <- tags$a(tags$img(src='leaching_potential.png', height = '500', width = '600', align = 'center'))
leaching_wsoils <- tags$a(tags$img(src='leaching_wsoils.png', height = '500', width = '600', align = 'center'))
crop_nrates <- tags$a(tags$img(src='crop_nrates.png', height = '500', width = '800', align = 'center'))
boxplot_interp <- tags$a(tags$img(src='boxplot_interp.png', height = '500', width = '500', align = 'center'))



########################
#shinyApp(ui,server)