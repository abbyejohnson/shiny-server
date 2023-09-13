
###USER INTERFACE###

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
              div(strong("Created by: Grant Moser, Jennifer Dierauer, Abby Johnson, and Kevin Masarik")),
              div("Center for Watershed Science and Education"),
              div("Last modified:", modified,'.', a(href="mailto:kmasarik@uwsp.edu","Contact us for questions")),
              #div(a(href="mailto:kmasarik@uwsp.edu","Contact Kevin Masarik for questions")),
              #br(),
              div(uwsp,uwex),
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
                   tabPanel("Learn about Nitrate",
                            h4(strong("What is Nitrate")),
                            p("This test measures the amount of nitrate-nitrogen in your well. Nitrate is a form of 
                                            nitrogen, commonly found in agricultural and lawn fertilizer, 
                                            that easily dissolves in water. It is also formed when waste materials such as manure or 
                                            septic effluent decompose. The natural level of nitrate in Wisconsin's groundwater is 
                                            less than 1 mg/L. Levels greater than this suggest groundwater has been impacted by 
                                            various land-use practices."),
                            br(),
                            h4(strong("Why Test for Nitrate")),
                            p("Nitrate is an important test for determining the safety of well water for drinking. Nitrate is a test that 
                                         allows us to understand the influence of human activities on well water quality. Because it moves
                                          can come from a variety of sources and moves easily through soil, it serves as a useful indicator of certain land-use activities. 
                                          An annual nitrate test is useful for better understanding whether water quality is getting better, worse, or staying
                                          the same with respect to certain land-uses (see Sources)."),
                            br(),
                            h4(strong("Interpreting Nitrate-Nitrogen Concentrations")),
                            tags$div(nitrate_interp),
                            br(),
                            h4(strong("Health Effects of Nitrate in Drinking Water")),
                            tags$div(nitrate_health),
                            br(),
                            h4(strong("Ways to reduce nitrate in your drinking water")),
                            tags$div("One way to reduce nitrate is to install a water treatment device approved for removal of nitrate; 
                                          testing is the only way to make sure these devices are properly functioning"),
                            br(),
                            tags$em("Point-of-use devices treat enough water for drinking and cooking needs"),
                            tags$ul(
                              tags$li("Reverse Osmosis"),
                              tags$li("Distillation")),
                            tags$em("Point-of-entry systems treat all water distributed throughout the house"),
                            tags$ul(
                              tags$li("Anion Exchange")),
                            tags$div("Additionally, you may want to investigate the potential that drilling a new well or well reconstruction 
                                          may provide water with safe nitrate levels"),
                            br(),
                            h4(strong("Sources of Nitrate")),
                            tags$ul(
                              tags$li("Agricultural Fertilizers"),
                              tags$li("Manure and other biosolids"),
                              tags$li("Septic Systems"),
                              tags$li("Lawn Fertilizers")
                            ),
                            br(),
                            h4(strong("Strategies to reduce nitrate in groundwater")),
                            tags$ul(
                              tags$li("Applying fertilizer at the right rate, time, source, place will maximize 
                                            profitability and minimize excessive losses of nitrogen to groundwater; additional 
                                            practices may be needed to improve water quality in areas with susceptible soils and geology"),
                              tags$li("You may not need as much nitrogen fertilizer as you think, conduct your own on-farm rate trials
                                            to develop customized fertilizer response curves for your farm"),
                              tags$li("Utilize conservation incentive programs to take marginal land or underperforming 
                                            parts of fields out of production"),
                              tags$li("Diversify cropping systems to include less nitrogen intensive crops in the rotation"),
                              tags$li("Explore and experiment with the use of cover crops, perennial cropping systems, or 
                                            managed grazing to reduce nitrate losses to groundwater")
                            )
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
)