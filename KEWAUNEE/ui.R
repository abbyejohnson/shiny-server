##### Kewaunee COUNTY ----------------------------------------------------------------------------------------------------------

ui = fluidPage(
    
    #headerPanel(div(uwex,uwsp)),
    titlePanel(strong('Kewaunee County Well Water Monitoring Project')),
    theme = shinytheme("flatly"),
    "",
    
    sidebarLayout(
      mainPanel(width = 6, 
                fluidRow(
                  column(4,
                         selectInput("type", "Map Type",
                                     c("Individual Wells" = "Individual Wells",
                                       "Municipality" = "Municipality"), selected = "Individual Wells")),
                  column(3,
                         # sliderInput("year", "Year",
                         #             min = 2016, max = 2019,
                         #             value = 2016, step = 1)),
                         selectInput("year", "Year",
                                     c("2024" = "2024",
                                       "2023" = "2023",
                                       "2022" = "2022",
                                       "2021" = "2021",
                                       "2020" = "2020",
                                       "2019" = "2019",
                                       "2018" = "2018",
                                       "2017" = "2017",
                                       "2016" = "2016",
                                       "2015" = "2015",
                                       "2014" = "2014",
                                       "2013" = "2013",
                                       "2012" = "2012",
                                       "2011" = "2011",
                                       "2010" = "2010",
                                       "2009" = "2009",
                                       "2008" = "2008",
                                       "2007" = "2007",
                                       "2006" = "2006",
                                       "2005" = "2005",
                                       "2004" = "2004"), selected = "2024")),
                  column(5,
                         selectInput("var", "Variable:",
                                     c("Nitrate-Nitrogen" = "Nitrate",
                                       "Alkalinity" = "Alkalinity",
                                       "Chloride" = "Chloride",
                                       "Conductivity" = "Conductivity",
                                       "pH" = "pH",
                                       "Total Hardness" = "Total.Hardness",
                                       "Nitrate Trend" = "rate",
                                       "Chloride Trend" = "cl_rate"), selected = "Nitrate"))),
                                       # "Soil Drainage Rank" = "weighted.rank",
                                       # "Uppermost Bedrock Type" = "Top.Bedrock",
                                       # "Casing Below Water Table" = "DB_WT"
                hr(),
                fluidRow(
                  column(12, offset = 0,
                         leafletOutput("map", height = 550)
                  )
                ),
                br(),
                div(logo,uwsp,uwex),
                br(),
                div(strong("Center for Watershed Science and Education in partnership with Kewaunee County")),
                div("Created by: Kevin Masarik, Abby Johnson, Grant Moser, and Jennifer Dierauer"),
                div("Last modified:", modified,'.', a(href="mailto:gndwater@uwsp.edu","Contact us for questions")),
      ),
      position = NULL,
      
      sidebarPanel(width = 6, 
                   tabsetPanel(
                     tabPanel("ABOUT the Project",
                              h4(strong("Overview")),
                              tags$p("Kewaunee County has sampled wells over time 
                              to gather information on well water quality. The water quality data collected is intended to understand whether  
                              groundwater quality is changing over time.  This project has established a network of private well owners that has performed 
                              annual testing for a period of at least four years."),
                              # tags$ul
                              #         (tags$li("2019 - 348 well owners participated in Year 1"),
                              #         tags$li("2020 - 323 well owners participated in Year 2"),
                              #         tags$li("2021 - 307 well owners participated in Year 3"),
                              #         tags$li("2022 - 294 well owners participated in Year 4"),
                              #         tags$li("2023 - 269 well owners participated in Year 5"),
                              #           ),
                              tags$p("The information collected through these efforts will be used to analyze 
                              where and what factors may be contributing to any changes in groundwater quality observed over time.  
                              The well network is intended to be representative of Kewaunee County (i.e. accounting for 
                              the wide variety of geology, soils, land-use, and well construction found throughout the area)."),
                              br(),
                              h4(strong("Using the Map")),
                              p(strong("Map Type")),
                              p("Individual Wells: When individual wells are selected, this map view allows you to see the water quality
                                  test results for each well that was sampled. The well points are approximate locations in order to protect the privacy of participants. 
                                  Clicking on the points will provide the water quality
                                  result for whichever test is selected."),
                              p("Municipality: When the municipality view is selected, the map displays the average concentration for each
                                  of the water quality tests conducted. Clicking on the municipality will provide summary 
                                  statistics by town."),
                              p(strong("Year")),
                              p("This drop down menu allows you to see results from different years. Additional data will be made available as the project progresses."),
                              p(strong("Variable")),
                              p("This drop down menu allows you to view the different analytes or various attributes associated with the wells"),
                              br(),
                              h4(strong("LEARN about tests")),
                              p("Samples are analyzed for nitrate-nitrogen, chloride, alkalinity, total hardness, pH, and conductivity. Nitrate and
                                chloride are useful for understanding the degree to which groundwater has been affected by human activities. 
                                Click on the 'LEARN about tests' tab to learn more about the specific tests and what they tell us about groundwater."),
                              # br(),
                              # h4(strong("LEARN about other variables")),
                              # p("Other attributes associated with the well construction or land use are can be useful for 
                              #   understanding what factors may be influencing well water quality. Other variables include: 
                              #   Percent Agriculture, Percent Dairy Rotation, Percent Row Crops, Percent Hay/Pasture, 
                              #   Uppermost Bedrock Type, and Casing Depth Below the Water Table. Click on the 'LEARN about other variables' tab To learn more about other variables and what they tell us about groundwater."),
                              br(),
                              
                              h4(strong("EXPLORE project data")),
                              p("Data can further be explored for trends over time in individual wells, municipal summaries, or county-wide.  
                                Click on the 'EXPLORE project data' tab to investigate data in more detail."),
                              br(),
                              h4(strong("Interpreting the data")),
                              p("Boxplots are commonly used to display the data collected from this project. Boxplots help to visualize aspects of the data in picture format.
                                They also allow us to compare datasets between different years or categories of data from the same year. The following diagram contains
                                information that is helpful for understanding how to interpret boxplots of data for this project."),
                              tags$div(boxplot_interp)
                     ),
                     tabPanel("LEARN about Tests",
                              tabsetPanel(
                                tabPanel("Nitrate-Nitrogen",
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
                                         allows us to understand the influence of human activities on well water quality. Because it 
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
                                tabPanel("Chloride",
                                         h4(strong("What is Chloride")),
                                         p("In most areas of Wisconsin, chloride concentrations are naturally low 
                                          (usually less than 15 mg/L). Higher concentrations may serve as an indication that 
                                          the groundwater supplied to your well has been impacted by various human activities."),
                                         br(),
                                         h4(strong("Why Test for Chloride")),
                                         p("Chloride is a test that allows us to understand the influence of human activities on well water quality.
                                         Measuring chloride concentrations in your well water will allow us to better understand whether well 
                                          water quality is getting better, worse, or staying the same with respect to certain land-uses (see Sources)."),
                                         br(),
                                         h4(strong("Interpreting Chloride Concentrations")),
                                         p("Chloride is not toxic at typical concentrations found in groundwater. Unusually high 
                                          concentrations of chloride (greater than 150 mg/L) are often associated with road salt and may be related to nearby
                                          parking lots or road culverts where meltwater from winter deicing activities often accumulates.
                                          Water with concentrations greater than 250 mg/L are likely to contain elevated sodium and are sometimes 
                                          associated with a salty taste; water is also more likely to be corrosive to certain metals."),
                                         br(),
                                         h4(strong("Sources of Chloride")),
                                         tags$ul(
                                           tags$li("Agricultural Fertilizers (chloride is a companion ion of potash fertilizers)"),
                                           tags$li("Manure and other biosolids"),
                                           tags$li("Septic Systems"),
                                           tags$li("Road Salt")
                                         ),
                                         #tags$div(nitrate_health),
                                         br(),
                                         #h4(strong("hhh"))
                                ),
                                tabPanel("Alkalinity",
                                         h4(strong("What is Alkalinity")),
                                         p("Alkalinity is a measure of water's ability to neutralize acids.  Alkalinity is associated with carbonate 
                                          minerals and is commonly found in areas where groundwater is stored or transported in 
                                          carbonate aquifers which occur in parts of Kewaunee County."),
                                         br(),
                                         h4(strong("Why Test for Alkalinity")),
                                         p("Because alkalinity is related to the rocks and soils that water flows through on its way to a 
                                          well, we would expect alkalinity concentrations to be fairly stable from year to year. Any changes observed in alkalinity
                                          concentrations may help us better understand the influence of climate variability
                                          on well water quality on an individual well, or make sense of broader water quality results from Kewaunee County. 
                                          Particularly in wells that are uninfluenced by human activity,
                                          Alkalinity concentrations may help us better understand which wells are accessing younger water that may be
                                          more vulnerable or prone to contamination."),
                                         br(),
                                         h4(strong("Interpreting Alkalinity Concentrations")),
                                         p("There are no health concerns associated with having alkalinity in water. 
                                          Alkalinity should be roughly 75-100% of the total hardness value in an unsoftened sample. 
                                          Water with low levels of alkalinity (less than 150 mg/L) is more likely to be corrosive. 
                                          High alkalinity water (greater than 200 mg/L), may contribute to scale formation.  If total
                                          hardness is half or less than the alkalinity result, it likely indicates that your water has passed 
                                          through a water softener. If alkalinity is significantly less than total hardness, it be related to 
                                          elevated levels of chloride or nitrate in your water sample."),
                                         br()
                                ),
                                tabPanel("Total Hardness",
                                         h4(strong("What is Total Hardness")),
                                         p("The hardness test measures the amount of calcium and mangnesium in water. Calcium and 
                                          magnesium are essential nutrients, which generally come from natural sources of these elements in rock and soils.
                                          The amount present in drinking water is generally not a significant source of these nutrients compared with a health diet."),
                                         br(),
                                         h4(strong("Why Test for Total Hardness")),
                                         p("Because total hardness is related to the rocks and soils that water flows through on its way to a 
                                         well, we would expect total hardness concentrations to be fairly stable from year to year. Any changes observed in total hardness
                                          concentrations may help us better understand the influence of climate variability
                                          on well water quality on an individual well. Because hardness concentrations have been shown to increase when nitrate and/or chloride 
                                          increase, the total hardness test is a good complement to other tests."),
                                         br(),
                                         h4(strong("Interpreting Total Hardness Concentrations")),
                                         p("There are no health concerns associated with having total hardness in your water, however too much or too little hardness can be associated 
                                          with various aesthetic issues that can impact plumbing and other functions."),
                                         br(),
                                         p(strong("Hard Water: ")),
                                         p("Water with a total hardness value greater than 200 mgL is considered hard water. 
                                          Hard water can cause lime buildup (scaling) in pipes and water heaters. Elements responsible for water hardness can also react with soap 
                                          decreasing its cleaning ability, can cause buildup of soap scum, and/or graying of white laundry over time.
                                          Some people that use hard water for showering may notice problems with dry skin."),
                                         p(em(strong("If you are experiencing problems with hard water: ")),"Consider softening water using a water softener. 
                                          Water softeners remove calcium and magnesium and replace those elements with a different cation (usually sodium).
                                          Many people choose not to soften the cold-water tap used for drinking/cooking and the outdoor faucet used for yard watering."),
                                         br(),  
                                         p(strong("Soft Water: ")),
                                         p("Water with a total hardness concentration less than 150 mg/L is considered soft.  Water with too little hardness
                                          is often associated with corrosive water, which can be problematic for households with copper plumbing or other metal components of a
                                          plumbing system. Please note: Total Hardness values less than 50 would be rare for Kewaunee County, if your water reported less than 50 mg/L of Total 
                                          Hardness it likely represents softened or partially softened water."),
                                         p(em(strong("If you are experiencing problems with soft water or corrosion of household plumbing: ")),"You may want to consider a water treatment device
                                          (called a neutralizer) designed to make water less corrosive. Newer homes with plastic plumbing generally don't need to be as concerned with corrosive water
                                          with respect to the plumbing."),
                                         br(),
                                         p(strong("Ideal: ")),
                                         p("Water with total hardness between 150-200 mg/L is generally an ideal range of water hardness because there are enough ions to 
                                          protect against corrosion, but not too many that they contribute to scale formation. While it is a personal preference, households 
                                          with hardness in this range generally don't require additional treatment."),
                                         br(),
                                         p(em("Note: the water softening industry measures hardness in grains per gallon. 1 grain per gallon = 17.1 mg/L as CaCO3")),
                                         br(),
                                         h4(strong("Sources of Total Hardness")),
                                         p("Primarily dissolved carbonate minerals which occur naturally in soil and certain rock materials. When carbonate minerals dissolve, they
                                          increase the amount of calcium and magnesium ions in water."),
                                         br()
                                ),
                                tabPanel("pH",
                                         h4(strong("What is pH")),
                                         p("The pH test measures the concentration of hydrogen ions in a solution.  The concentration of hydrogen
                                          determines if a solution is acidic or basic. The lower the pH, the more corrosive water will be."),
                                         br(),
                                         h4(strong("Why Test for pH")),
                                         p("The pH of groundwater in Wisconsin in typically between 6.5 and 8.5. Lower or higher values can occasionally 
                                          occur in parts of the state because of certain geologic characteristics. While there are not health concerns associated with
                                          pH levels typical of Wisconsin groundwater, corrosive water (pH less than 7) is more likely to contain elevated levels of
                                          copper and/or lead if those materials occur in the plumbing system."),
                                         br(),
                                         p("Low pH values are most often caused by a lack of carbonate minerals. Low total hardness and alkalinity are often correlated with 
                                          low or acidic pH values.")
                                ),
                                tabPanel("Conductivity",
                                         h4(strong("What is Conductivity")),
                                         p("Conductivity measures the amount of dissolved substances (or ions) in water; but does not
                                          give an indication of which minerals are present.  Changes in conductivity over time may indicate changes 
                                          in overall water quality."),
                                         h4(strong("Why test for Conductivity")),
                                         p("There is no health standard associated with conductivity. A normal conductivity value measured in umhos/cm is roughly
                                          twice the total hardness (measured as CaCO3) in unsoftened water samples. If conductivity is significantly greater than twice 
                                          the hardness, it may indicate the presence of other human-influenced or naturally occurring ions such as chloride, nitrate,
                                          or sulfate. Because conductivity is relatively easy and cost effective to measrue, understanding variations in conductivity 
                                          may help in designing cost effective monitoring strategies for homeowners 
                                          to monitor their well water continuously through sensors rather than an annual test."),
                                )
                              )),
                      tabPanel("LEARN about other Variables",
                               tabsetPanel(
                                tabPanel("Nitrate and Chloride Trends",
                                         h4(strong("Why are we interested in nitrate and chloride trends?")),
                                         p("Nitrate and chloride move very easily through soil. As a result it helps
                                            us understand the degree to which land use is impacting 
                                            groundwater quality. By testing for nitrate and chloride annually, we are able 
                                            to see how groundwater quality is changing with respect to certain
                                            land use practices."),
                                         p("While nitrate and chloride may go up, down, or stay the same;
                                            changes in a similar direction can sometimes indicate a trend.
                                            Mann-Kendall Trend Test was used to determine if there was a relationship of nitrate and chloride
                                            to time. Wells with a p-value less than 0.10 were determined to have a significant trend. Meanwhile the sen slope 
                                            was used to provide determination of rate and direction of trend"),
                                         p("Other wells may simply go up and down each year because of weather or other factors such as crop rotations. More years of data are required 
                                           in order to analyze for trends. As we obtain more years of data, we will be able to determine whether variability represents 
                                           actual trends or just wells that might be more susceptible to fluctuations."),
                                         p("A map of nitrate and chloride trends can be viewed by changing the variable to 'Nitrate Trends' or 'Chloride Trends'. Nitrate and chloride
                                            trend maps are only viewable for individual wells and are not summarized by municipality."),
                                         p("When viewing the map of trends for individual wells: Blue circles represent
                                            decreasing trends. Red circles respresent increasing trends. Yellow circles
                                            indicate wells where no trend was observed. The size of the
                                            circle provides an idea of the rate of change observed in that well."), 
                                         p("Data on individual wells over time can be viewed for each parameter by clicking on the 'EXPLORE Project Data > 'Individual Wells' tab."),
                                )
                              )),
                     tabPanel("EXPLORE project data",
                              tabsetPanel(
                                tabPanel("Individual Wells",  
                                         h4(strong("Investigate Water Quality by Individual Well")),
                                         p("One of the major goals of the project is to understand variability in well water quality over time.  
                                          By sampling the same wells annually, we can better understand whether water quaility is different from one year 
                                          to the next or relatively similar. If water quality is different, we can make assessments of whether those differences constitute an
                                          increasing or decreasing trend."),
                                         br(),
                                         p("Well Water Project IDs have been assigned specifically for this project. If you are 
                                          a participant and know your well's Project ID, you can select or enter from the drop-down menu. Otherwise if you are 
                                          interested in learning about a particular point on the map, simply click on the point and enter that Project ID into 
                                          the drop down menu. In order to maintain anonymity, Project IDs have
                                          only been shared with project participants for their individual well."),
                                         br(),
                                         selectInput("r",
                                                     "Select or enter a Project ID number:",
                                                     choices = df$PROJECT_ID,
                                                     selected = "",
                                                     width = 300
                                         ),
                                         plotlyOutput("well_nitrate", height = 250),
                                         plotlyOutput("well_chloride", height = 250),
                                         plotlyOutput("well_alkalinity", height = 250),
                                         plotlyOutput("well_hardness", height = 250),
                                         plotlyOutput("well_conductivity", height = 250),
                                         plotlyOutput("well_ph", height = 250)
                                ),
                                
                                tabPanel("Municipality",
                                         h4(strong("Investigate Water Quality by Municipality")),
                                         p("Well water quality can be aggregated and displayed by municipality. Select a parameter 
                                           from the drop down menu to display the summary statistics as box plots by town. Hover 
                                           over the box plot to see values for mean, median, minimum, maximum values."),
                                         br(),
                                         p("The drop down menu can also be used to view data for a particular year of the project."),
                                         selectInput("analyte",
                                                     "Select analyte:",
                                                     choices = c("Nitrate","Alkalinity","Chloride","Total Hardness","pH","Conductivity"),
                                                     selected = "Nitrate",
                                                     width = 300
                                         ),
                                         selectInput("year_muni",
                                                     "Select Year:",
                                                     choices = c("2024","2023","2022","2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004"),
                                                     selected = "2024",
                                                     width = 300
                                         ),
                                         br(),
                                         plotlyOutput("select_analyte", height = 400)
                                ),
                                
                                tabPanel("Annual Summary Statistics",
                                         tabPanel("Annual Summary",
                                         h4(strong("Investigate County-wide Summary Statistics")),
                                         p("Summary statistics for each year of the project can be generated for all wells tested as part 
                                         of the annual monitoring by selecting an individual year: "),
                                         selectInput("cty_year",
                                                     "Select Year:",
                                                     choices = c("2024","2023","2022","2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004"),
                                                     selected = "2024",
                                                     width = 300
                                         ),
                                         br(),
                                         rHandsontableOutput("cty_summary_table", height = 300)
                                         
                                ))
                     )
                                  
                              
                            ))
      ))
)
 
  