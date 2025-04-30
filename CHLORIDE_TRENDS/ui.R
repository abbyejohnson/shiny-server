###USER INTERFACE###

  ui <- fluidPage(
    
    # App title ----
    titlePanel("Chloride Levels of Wisconsin's Public Water Systems"), 
    
    #shinythemes::themeSelector("darkly"),
    
    ###START NAVIGATION BAR###
    
    
    ###CONTROLS BACKGROUND THEME###
    theme = shinytheme('flatly'), # <--- To not use a theme, comment this
    "",
    sidebarLayout(
      mainPanel(width = 6,
                fluidRow(
                  column(4,
                         selectInput("type", "Map Type",
                                     c("Individual Wells" = "point"))
                  ),
                  column(4,
                         selectInput("var" , "Variable:",
                                     c("Chloride Concentration" = "Chloride.mean",
                                       "Chloride Trend" = "Chloride.trend"))
                  )
                ),
                hr(),
                fluidRow(
                  column(12, offset = 0,
                         leafletOutput("map", height = 620)
                  )
                )
      ),
      #  navbarPage(
      sidebarPanel(width = 6, 
                   tabsetPanel(
                     tabPanel("Overview",
                              h4(strong("Drinking Water and Chloride")),
                              br(),
                              p(strong("1. GROUNDWATER is the primary source of DRINKING WATER for 75% of Wisconsinites.")),
                              p("The 25% of residents who drink surface water live along Lake Michigan (Kenosha-Milwaukee-Green Bay) or Lake Superior (Superior-Ashland-Bayfield) where their municipalities source drinking water from the neighboring Great Lake. The rest of Wisconsinite's generally get 
                                their drinking water from groundwater accessed by public or private wells."),
                              br(),
                              p(strong("2. Wisconsin groundwater is generally low in chloride.")),
                              p("Chloride levels are generally less than 10 mg/L in much of Wisconsin. A couple exceptions to that are in and around Green Bay and Superior. In those regions, there can be naturally-occurring chloride in the geologic deposits found in these regions and background chloride concentrations are variable."),
                              br(),
                              p(strong("3. Sources of chloride to groundwater include winter road and sidewalk salt, water softener salt, waste brine from cheese and other food production, and potassium chloride fertilizers.")),
                              br(),
                              p(strong("4. Some public water systems in Wisconsin are seeing their levels of chloride increasing over time.")),
                              p("For those wells with a long enough record of chloride testing, more wells are showing increases than decreases. Explore chloride levels and changes over time in the 'Chloride Trends' tab."),
                              br(),
                              p(strong("Click on an individual well in the interactive map on the left to view detailed information on the most recent chloride sample from that well. Remember, naturally-occurring salt (chloride) levels are between 0-10mg/L throughout most of the state.")),
                              br(),
                              div(strong("Created by:")),
                              div("Center for Watershed Science and Education"),
                              div("Data Source: Wisconsin Department of Natural Resources, Groundwater Retrieval Network"),
                              div("Last modified:", modified),
                              div(a(href="mailto:kmasarik@uwsp.edu"),"Contact for questions: gndwater@uwsp.edu")
                              
                     ),
                     tabPanel("Chloride Trends",  
                              h4(strong("Chloride Levels in Public Drinking Water Wells")),
                        p("Chloride concentrations by year can be viewed for each public water system by selecting from the drop down below."),
                        br(),
                        p("Trend analysis has been performed on all wells with more than 4 years of data. Select 'Chloride Trends', from the drop down menu above the map to see which wells have observed increasing trends (pink), decreasing trends (blue), and no trend (grey)."),
                              p("A linear model was used to fit a regression line to the annual maximum cholride concentrations. The gray 'bands' around the regression line in the plot below represent the range in which the true regression line lies at a certain 
                      level of confidence (95% in the plot)."),
                              p("The regression line or trend is considered significant if the p-value < 0.05 and the slope 
                      is greater than 0.10 (or results in a change of greater than 1 mg/L for a 10 year period)"),
                              br(),
                              selectInput("r",
                                          "Select or type a WI Unique Well Number:",
                                          choices = well.trends$WI.Unique.Well..,
                                          selected = "",
                                          width = 300
                              ),
                              h4(strong("Trend Line Equation, R-squared, and p-value:")),
                              htmlOutput("WUWN_info"),
                              br(),
                              htmlOutput("WUWN_trend"),
                              br(),
                              plotlyOutput("line_WUWN", height = 400)
                              #this is the only other "line_WUWN" within the code, has to be source of error for line 429
                     ),
                     tabPanel("Solutions",
                              h4(strong("Reducing chloride impacts to groundwater")),
                              p("Many winter maintenance professionals are taking action to reduce their salt use by improving mechanical removal, calibrating equipment, incorporating liquid brine into their winter maintenance toolbox. Likewise, individuals and facilities managers are reducing their salt use through water softener tuneups and upgrades."),
                              br(),
                              p("You can protect our waters by following these simple steps:"),
                              p("You can protect our waters by following these simple steps:"),
                              p(strong("Use Winter Salt Wisely:", a(href="https://www.wisaltwise.com/Take-Action/Smart-Salting", "Wisconsin Salt Wise"))),
                              p(strong("If you have a water softener, make sure it's set correctly:", a(href="https://www.wisaltwise.com/Take-Action/Home-Water-Softeners", "Water Softeners"))),
                              p(strong("Be a conscientious motorist:", a(href="https://www.wisaltwise.com/Take-Action/Safe-Winter-Driving", "Safe Winter Driving"))),
                              p(strong("Help spread the word:", a(href="https://www.wisaltwise.com/Partner-Resources", "Outreach Tools"))),
                              ),
                     tabPanel("Data",
                              h4(strong("Data Overview")),
                              p("All data was gathered from the Wisconsin Department of Natural Resources'",a(href="https://dnr.wi.gov/topic/Groundwater/grn.html", "Groundwater Retrieval Network")),
                              br(),
                              p("While this data provides a long-term dataset, it is important to consider that public water wells 
                    are often not as representative of groundwater quality in more rural parts of the state.  In addition, municipal wells may be drilled deeper and often are not reflective
                    of chloride concentrations in the shallow groundwater."),
                              br(),
                              h4(strong("Public Drinking Water System Types")),
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
                      To ensure that data is representative of current conditions, wells without a sample collected in the past 6 years were 
                      excluded from the analysis"),
                              br(),
                              p(strong("Annual Chloride Value")),
                              p("Some public water supply systems are sampled more than once per year.  Others may also have treated samples represented in the original 
                      data.  To account for these issues, only the maximum chloride value for each calendar year was selected for use in the trend analysis and county averages."),
                              br()   
                     )
                   )
      )
    )
  )
  
    
    
