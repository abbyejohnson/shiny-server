library(shiny)
library(leaflet)
library(sf)
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
#install.packages("rgdal", repos="http://R-Forge.R-project.org")
library(rgdal)
library(tidyr)
library(plotly)
#library(ggpubr)
library(rhandsontable)
library(vtable)

#Change to reflect date of data updates
modified <- "February 3, 2026"

df <- read.csv("PIERCE_2025_ALL.csv", header = TRUE)
#df <- read.csv("JEFFERSON_wWell_Construction_v3.csv", header = TRUE)

df <- df[df$COUNTY == "PIERCE",]
#df$Arsenic <- ifelse(df$As_Qual=="<",0.0025,df$Arsenic)

df_stats <- df %>% group_by(PROJECT_ID) %>% summarise_at(c("Nitrate","Alkalinity","pH","Conductivity","THard","Chloride","cnt","DB_WT"), 
                                                         list(mean=mean, median=median, min=min,max=max, sum=sum, sd=sd), na.rm = TRUE)

df_stats$nitrate_diff <- df_stats$Nitrate_max - df_stats$Nitrate_min
df <- left_join(df,df_stats,by = "PROJECT_ID")
df$nitrate_direction <- ifelse(df$Nitrate_mean < df$Nitrate, df$nitrate_diff, -1*df$nitrate_diff)
df$Top.Bedrock <- ifelse(df$Top.Bedrock == "SHALE" | df$Top.Bedrock == "", "No Info", df$Top.Bedrock)
df$TOWNSHIP <- toupper(df$TOWNSHIP)


munidf <- df %>% 
  group_by(TOWNSHIP) %>% 
  summarise_at(c("Nitrate", "Alkalinity","Total.Hardness","Conductivity","pH","Chloride",
                 "AG_PERC","HAY_PAST","AG_HAY_PAST","ROWCROP","DAIRY_PERC","DB_WT",
                 "weighted.rank","cnt","cnt_Thard"), 
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
#munidf$Municipality <- ifelse(munidf$TOWNSHIP=="MAINE","Maine",ifelse(munidf$TOWNSHIP=="STRATFORD","Stratford",ifelse(munidf$TOWNSHIP=="KRONENWETTER","Kronenwetter",ifelse(munidf$TOWNSHIP=="FENWOOD","Fenwood",munidf$TOWNSHIP))))
#munidf$Year <- "2023&2024"

shape <- readOGR(dsn = "WI_Municipalities_2017.shp", stringsAsFactors = FALSE)
counties <- readRDS("WIcounties.rds")

colnames(df)


shape@data$Municipality <- as.character(shape@data$MCD_NAME)



logo <- tags$a(tags$img(src='pierce_logo.png',
                        alt = 'Pierce County Public Health logo featuring a green plant graphic to the left and the words ‘Pierce County’ in bold blue text above ‘Public Health’ in black script.',
                        height = '60', width ='60', align = 'center'))
uwsp <- tags$a(tags$img(src='uwsp.png',
                        alt = 'Logo and wordmark for the University of Wisconsin–Stevens Point.
                        On the left is the circular UW–Stevens Point emblem featuring a white clock tower silhouette on a purple and gold background.
                        To the right, the text reads: ‘Center for Watershed Science and Education, College of Natural Resources, University of Wisconsin–Stevens Point’ in purple lettering.',
                        height = '50', width = '225', style='float:right; margin-left: 5px; margin-right: 5px'))
uwex <- tags$a(tags$img(src='uwex.png',
                        alt = 'Logo for the University of Wisconsin–Madison Division of Extension.
                        The graphic includes the red and white UW crest on the left, followed by the text ‘Extension’ in red and ‘University of Wisconsin–Madison’ in black.',
                        height = '50', width = '200', style='float:right; margin-left: 5px; margin-right: 5px'))
nitrate_health <- tags$a(tags$img(src='nitrate_health.png',
                                  alt = 'A diverse family group stands together with several informational callout boxes explaining health risks associated with nitrate exposure.
                                  The callouts describe that nitrate can cause blue baby syndrome in infants, may cause thyroid disease, may cause birth defects in pregnant women,
                                  and may increase the risk for certain cancers. A large label notes that nitrate-nitrogen levels over 10 mg/L can be harmful. Source: Wisconsin Department of Health Services.',
                                  height = '500', width = '750', align = 'center'))
nitrate_interp <- tags$a(tags$img(src='nitrate_interp.png',
                                  alt = 'A vertical bar graphic showing three nitrate‑nitrogen concentration ranges in groundwater. The bottom blue section labeled ‘< 1 mg/L’
                                  represents natural or background levels of nitrate. The middle tan section labeled ‘1–10 mg/L’ indicates evidence of land‑use impacts and is considered suitable for drinking.
                                  The top red section labeled ‘> 10 mg/L’ warns that infants and women who are pregnant or may become pregnant should not drink water with this level of nitrate, and that everyone should avoid long‑term consumption',
                                  height = '250', width = '375', align = 'center'))
soils <- tags$a(tags$img(src='Drainage.jpg',
                         alt = 'A color‑coded map of Pierce County, Wisconsin, showing soil drainage classifications for a well water sampling project.
                         The county is divided into a grid, and colors represent drainage categories: excessively drained (red), somewhat excessively drained (orange), well drained (yellow), moderately well drained (light green), somewhat poorly drained (green), poorly drained (blue), and water (light blue).
                         Most of the county is dominated by yellow and orange areas, indicating well‑drained to somewhat excessively drained soils, with concentrations of blue and light blue drainage classes along southern waterways.
                         A small inset map at the bottom right highlights the location of Pierce County within Wisconsin. A north arrow and scale bar (0–10 miles) appear below the main map.
                         The map credits the USDA Natural Resources Conservation Service SSURGO Database and notes creation on February 13, 2026.',
                         height = '463.6', width = '600', align = 'center'))
wiscland <- tags$a(tags$img(src='Agriculture.jpg',
                            alt = 'A map of Pierce County, Wisconsin, showing agricultural land cover classifications for the Well Water Sampling Project. 
                            The county is divided into a grid and filled with small, scattered color‑coded patches representing different agricultural land uses.
                            Dark green areas show potato and vegetable production, medium green shows pasture, and light green shows hay. 
                            Yellow areas indicate dairy rotation fields, orange shows continuous corn, and light orange represents cash grain. 
                            The map displays a dense mix of cropland types, with especially large concentrations of orange and yellow agricultural areas in the central and eastern parts of the county.
                            A north arrow and 0–10 mile scale bar are located beneath the map. An inset map of Wisconsin at the bottom right highlights the location of Pierce County. 
                            The data source is listed as Wiscland 2.0, and the map is credited to Jack Frame, created February 13, 2026.',
                            height = '463.6', width = '600', align = 'center'))
casing <- tags$a(tags$img(src='casing.png',
                          alt = 'Diagram showing several vertical well casings extending from the ground surface into the water table at different depths.
                          Labels indicate casing depths of 25 feet, 50 feet, 75 feet, 100 feet, and more than 100 feet. The water level is shown as a horizontal dashed line, with the lower portions of the casings submerged below it.
                          The section of casing above the static water level (SWL) is labeled on the left side of the diagram.',
                          height = '500', width = '750', align = 'center'))
leaching_potential <- tags$a(tags$img(src='leaching_potential.png',
                                      alt = 'Conceptual graph illustrating the relationship between economic optimal nitrogen application rates and nitrate leaching potential.
                                      The x‑axis represents increasing nitrogen rates across different land uses or crops, including forest or prairie, alfalfa, soybean, corn–soybean rotation, corn, and potato.
                                      The y‑axis represents increasing nitrate leaching potential. A diagonal arrow transitions from blue to red, showing that as nitrogen application rates rise, water quality and nitrate concentrations increase from lower to higher levels.
                                      A color bar illustrates the gradient from low nitrate concentration in blue to high concentration in red.',
                                      height = '500', width = '600', align = 'center'))
leaching_wsoils <- tags$a(tags$img(src='leaching_wsoils.png',
                                   alt = 'Three‑dimensional conceptual diagram illustrating how nitrate leaching potential increases with higher nitrogen application rates and better‑drained soils.
                                   The x‑axis shows economic optimal nitrogen rates across different land uses or crops, including forest or prairie, alfalfa, soybean, corn–soybean rotation, corn, and potato.
                                   The y‑axis represents nitrate leaching potential. A third axis represents soil drainage classification, ranging from poorly drained to excessively well drained.
                                   A colored surface slopes upward from blue (lower nitrate concentration) to red (higher concentration), demonstrating that both increasing nitrogen rates and better soil drainage contribute to greater nitrate leaching.
                                   A color bar shows the gradient from low to high nitrate concentrations.',
                                   height = '500', width = '600', align = 'center'))
crop_nrates <- tags$a(tags$img(src='crop_nrates.png',
                               alt = 'A bar chart comparing nitrogen requirements for various crops, showing low, average, and high nitrogen application rates in pounds per acre.
                               Crops are listed along the x‑axis from lowest to highest nitrogen needs, starting with soybean, psyllium, alfalfa seedling, clover, vetch, chickpea, dry field pea, buckwheat, annual canarygrass, flax, oats, rye, triticale, barley, wheat, canola, millet, miscanthus, switchgrass, sesame, sunflower, tobacco, forage sorghum, grain sorghum, sweet corn, safflower, potato, and corn.
                               Nitrogen requirements range from near zero for legumes to over 200 pounds per acre for potato and corn.
                               A note indicates that legumes have a symbiotic relationship with nitrogen‑fixing bacteria. References are listed below the chart.',
                               height = '500', width = '800', align = 'center'))
boxplot_interp <- tags$a(tags$img(src='boxplot_interp.png',
                                  alt = 'Annotated diagram of a box‑and‑whisker plot explaining each statistical component.
                                  The box represents the interquartile range, spanning from the lower quartile (Q1) to the upper quartile (Q3).
                                  A horizontal line inside the box marks the median, and a diamond shape represents the mean.
                                  The top whisker extends to the maximum value excluding outliers, defined as Q3 plus 1.5 times the interquartile range.
                                  The bottom whisker extends to the minimum value excluding outliers, defined as Q1 minus 1.5 times the interquartile range.
                                  Dots above and below the whiskers represent outliers. Labels describe each component, including the number of samples shown below the plot.',
                                  height = '500', width = '500', align = 'center'))



########################
#shinyApp(ui,server)