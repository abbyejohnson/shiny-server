##### SAUK COUNTY ----------------------------------------------------------------------------------------------------------

ui = fluidPage(
    
    #headerPanel(div(uwex,uwsp)),
    titlePanel(strong('Sauk County Well Water Monitoring Project')),
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
                                     c("2022 (Year 4)" = "2022",
                                       "2021 (Year 3)" = "2021",
                                       "2020 (Year 2)" = "2020",
                                       "2019 (Year 1)" = "2019"), selected = "2022")),
                  column(5,
                         selectInput("var", "Variable:",
                                     c("Nitrate-Nitrogen" = "Nitrate",
                                       "Alkalinity" = "Alkalinity",
                                       "Chloride" = "Chloride",
                                       "Conductivity" = "Conductivity",
                                       "pH" = "pH",
                                       "Total Hardness" = "Total.Hardness",
                                       "Nitrate Trend" = "rate",
                                       "Percent Row Crops" = "ROWCROP",
                                       "Percent Dairy Rotation" = "DAIRY_PERC",
                                       "Percent Hay/Pasture" = "HAY_PAST",
                                       "Percent All Agriculture" = "AG_HAY_PAST",
                                       "Soil Drainage Rank" = "weighted.rank",
                                       "Uppermost Bedrock Type" = "Top.Bedrock",
                                       "Casing Below Water Table" = "DB_WT"), selected = "Nitrate"))),
                hr(),
                fluidRow(
                  column(12, offset = 0,
                         leafletOutput("map", height = 550)
                  )
                ),
                br(),
                div(logo,uwsp,uwex),
                br(),
                div(strong("Center for Watershed Science and Education in partnership with Green County")),
                div("Created by: Kevin Masarik, Abby Johnson, Grant Moser, and Jennifer Dierauer"),
                div("Last modified:", modified,'.', a(href="mailto:gndwater@uwsp.edu","Contact us for questions")),
      ),
      position = NULL,
      
      sidebarPanel(width = 6, 
                   tabsetPanel(
                     tabPanel("ABOUT the Project",
                              h4(strong("Overview")),
                              tags$p("Sauk County is conducting a five-year project 
                              to gather information on well water quality. The water quality data collected is intended to understand whether  
                              groundwater quality is changing over time.  This project has established a network of private well owners to perform 
                              annual testing for a period of five years."),
                              tags$ul
                                      (tags$li("2019 - 406 well owners participated in Year 1"),
                                      tags$li("2020 - 372 well owners participated in Year 2"),
                                      tags$li("2021 - 351 well owners participated in Year 3"),
                                      tags$li("2022 - 327 well owners participated in Year 4")
                                        ),
                              tags$p("The information collected through these efforts will be used to analyze 
                              where and what factors may be contributing to any changes in groundwater quality observed over time.  
                              The well network is intended to be representative of Sauk County (i.e. accounting for 
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
                              p("This drop down menu switches allows you to view the different analytes or various attributes associated with the wells"),
                              br(),
                              h4(strong("LEARN about tests")),
                              p("Samples are analyzed for nitrate-nitrogen, chloride, alkalinity, total hardness, pH, and conductivity. Nitrate and
                                chloride are useful for understanding the degree to which groundwater has been affected by human activities. 
                                Click on the 'LEARN about tests' tab To learn more about the specific tests and what they tell us about groundwater."),
                              br(),
                              h4(strong("LEARN about other variables")),
                              p("Other attributes associated with the well construction or land use are can be useful for 
                                understanding what factors may be influencing well water quality. Other variables include: 
                                Percent Agriculture, Percent Dairy Rotation, Percent Row Crops, Percent Hay/Pasture, 
                                Uppermost Bedrock Type, and Casing Depth Below the Water Table. Click on the 'LEARN about other variables' tab To learn more about other variables and what they tell us about groundwater."),
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
                                          carbonate aquifers which occur in parts of Green County."),
                                         br(),
                                         h4(strong("Why Test for Alkalinity")),
                                         p("Because alkalinity is related to the rocks and soils that water flows through on its way to a 
                                          well, we would expect alkalinity concentrations to be fairly stable from year to year. Any changes observed in alkalinity
                                          concentrations may help us better understand the influence of climate variability
                                          on well water quality on an individual well, or make sense of broader water quality results from Green County. 
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
                                          magnesium are essential nutrients, which generally come from naturally sources of these elements in rock and soils.
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
                                          plumbing system. Please note: Total Hardness values less than 50 would be rare for Green County, if your water reported less than 50 mg/L of Total 
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
                                         p("There is no health standard associated conductivity. A normal conductivity value measured in umhos/cm is roughly
                                          twice the total hardness (measured as CaCO3) in unsoftened water samples. If conductivity is significantly greater than twice 
                                          the hardness, it may indicate the presence of other human-influenced or naturally occurring ions such as chloride, nitrate,
                                          or sulfate. Because conductivity is relatively easy and cost effective to measrue, understanding variations in conductivity 
                                          may help in designing cost effective monitoring strategies for homeowners 
                                          to monitor their well water continuously through sensors rather than annual test."),
                                )
                              )),
                     tabPanel("LEARN about other Variables",
                              tabsetPanel(
                                  tabPanel("Land Cover Types",
                                         h4(strong("What does land cover have to do with well water quality?")),
                                         p("Land use plays an important role in certain water quality tests. Nitrate
                                           and chloride in particular can be influenced by land cover type. The relationship
                                           between various land cover types will be investigated further in future years. 
                                           These relationships can be used to develop statistical models that can help 
                                           predict nitrate and or chloride concentrations."),
                                         br(),
                                         tags$div(wiscland),
                                         h4(strong("Land cover categories")),
                                         br(),
                                         p("For each well, the Wiscland 2.0 dataset was used to calculate the percentage of various agricultural land cover types
                                           within a 500 meter buffer around each well."),
                                         p("Click on the 'Variable' tab above the map to the left to display wells by:"),
                                         tags$ul(
                                           tags$li("All Agriculture - Row Crops + Dairy Rotation + Hay/Pasture"),
                                           tags$li("Row Crops (Continuous Corn or Cash Grain) - Corn grain or corn silage grown every year in a 6-year 
                                                   rotation, or corn grain and soybean plantings alternate each year."),
                                           tags$li("Dairy Rotation - Corn grain, corn silage, and alfalfa in a 6 year-rotation, with typically 2 years of corn, 4 years of
                                                   alfalfa, and a cover crop between corn and alfalfa years."),
                                           tags$li("Hay/Pasture - Lands covered by planted perennial herbaceous vegetation and harvested for use as livestock forage
                                                   or lands covered by herbaceous vegetation, primarily perennial grasses, used for grazing livestock.")
                                         ),
                                         br(),
                                         h4(strong("Nitrogen fertilizer recommendations by crop type")),
                                         p("Nitrogen fertilizer recommendations vary by crop type, as a result there is often a relationship between the 
                                           types of crops/rotations and well water quality. Not all of the nitrogen applied as fertilizer/manure is taken up by 
                                           the plants. Heavy rains during the growing season can push nitrate past the reach of plant roots; any nitrate left over
                                           after the growing season is likely to leach into groundwater with fall rains and/or spring snow melt."),
                                         br(),
                                         tags$div(crop_nrates),
                                         h4(strong("Nitrate leaching potential")),
                                         p("Diagram below illustrates the relationship between nitrate leaching and crop type when using economic optimal rates."),
                                         tags$div(leaching_potential),
                                         
                                ),
                                tabPanel("Bedrock Type",
                                         h4(strong("Why are we interested in bedrock type?")),
                                         p("Different rock types often have different chemical compositions that can influence tests such as
                                           total hardness, alkalinity, and pH. The type of bedrock also helps determine how quickly groundwater moves and can also influence how easily 
                                           groundwater can become contaminated. For instance groundwater moving through large cracks or fractures will
                                           generally move much quicker and may not be as well filtered as water moving through sandstone or sand."),
                                         tags$ul(
                                           tags$li("Dolomite (red) - A type of carbonate rock that is generally associated with higher concentrations 
                                                   of calcium and magnesium (i.e. total hardness). Groundwater occurs in the cracks and fractures of 
                                                   this bedrock type."),
                                           tags$li("Sandstone (green) - A type of sedimentary rock formed when sand from oceans or seas were cemented together.
                                                   Sandstone generally contains less calcium and magnesium and may provide better filtration from contaminants such as 
                                                   bacteria."),
                                           tags$li("Unconsolidated (blue) - Wells with this description mean that the well terminated above consolidated bedrock. Generally
                                                   these are wells that rely on the sand and gravel aquifer to provide water to the well."),
                                           tags$li("No Info (purple) - Some wells do not have reliable well construction information associated with them."),
                                         ),
                                ),
                                tabPanel("Soil Drainage Rank",
                                         h4(strong("What Does the soil drainage classification tell us?")),
                                         p("Soil drainage influences how quickly water and also certain contaminants can move through the soil.
                                           The faster water is able to move through, the more easily dissolved ions such as nitrate and chloride can move past the root zone
                                           of plants. In addition, soils that are able to hold onto water longer mean plants have a greater ability to utilize water 
                                           and nutrients stored in the soil profile, reducing the risk of losing nutrients to groundwater."),
                                         tags$div(soils),
                                         br(),
                                         h4(strong("Order and description of soil drainage rank")),
                                         p("Soil drainage classification of land within a 500 meter buffer of the well was calculated from the NRCS
                                           SSURGO database. A weighted rank was determined using the following rankings:"),
                                         tags$ul(
                                           tags$li("(1) Excessively drained - Water is removed very rapidly. Internal free water occurrence
                                            is very rare or very deep. The soils are often coarse-textured and have very high hydraulic 
                                            conductivity."),
                                           tags$li("(2) Somewhat excessively drained - Water is removed from the soil rapidly. Internal 
                                            free water occurrence is very rare or very deep. The soils are commonly coarse-textured 
                                            and have high saturated hydraulic conductivity."),
                                           tags$li("(3) Well drained - Water is removed from the soil readily but not rapidly. Internal free 
                                            water occurrence is deep or very deep; annual duration is not specified. Water is available 
                                            to plants throughout most of the growing season in humid regions. Wetness does not inhibit growth of 
                                            roots for significant periods during most growing seasons. The soils are mainly free of the deep to 
                                             redoximorphic features that are related to wetness."),
                                           tags$li("(4) Moderately well drained - Water is removed from the soil somewhat slowly during 
                                             some periods of the year. Internal free water occurrence is moderately deep and 
                                            transitory through permanent. The soils are wet for only a short time within the rooting 
                                            depth during the growing season, but long enough that most mesophytic crops are affected. 
                                            They commonly have a moderately low or lower saturated hydraulic conductivity in a layer
                                            within the upper 1 m, periodically receive high rainfall, or both."),
                                           tags$li("(5) Somewhat poorly drained - Water is removed slowly so that the soil is wet at 
                                            a shallow depth for significant periods during the growing season. The occurrence of 
                                            internal free water commonly is shallow to moderately deep and transitory to permanent. 
                                            Wetness markedly restricts the growth of mesophytic crops, unless artificial drainage
                                            is provided. The soils commonly have one or more of the following characteristics: 
                                            low or very low saturated hydraulic conductivity, a high water table, additional water from 
                                            seepage, or nearly continuous rainfall."),
                                           tags$li("(6) Poorly drained - Water is removed so slowly that the soil is wet at 
                                            shallow depths periodically during the growing season or remains wet for long 
                                            periods. The occurrence of internal free water is shallow or very shallow and common or 
                                            persistent. Free water is commonly at or near the surface long enough during the growing 
                                            season so that most mesophytic crops cannot be grown, unless the soil is artificially 
                                            drained. The soil, however, is not continuously wet directly below plow-depth. Free water 
                                            at shallow depth is usually present. This water table is commonly the result of low or 
                                            very low saturated hydraulic conductivity of nearly continuous rainfall, or of a combination of these."),
                                           tags$li("(7) Very poorly drained - Water is removed from the soil so slowly that free water 
                                            remains at or very near the ground surface during much of the growing season. The occurrence 
                                            of internal free water is very shallow and persistent or permanent. Unless the soil is 
                                            artificially drained, most mesophytic crops cannot be grown. The soils are commonly level or 
                                            depressed and frequently ponded. If rainfall is high or nearly continuous, slope gradients may be greater."),
                                         ),
                                         br(),
                                         h4(strong("How soil drainage classification influences nitrate leaching loss")),
                                         p("Diagram showing the relationship between crop type and soil drainage classification on water quality with respect to nitrate."),
                                         tags$div(leaching_wsoils),
                                ),
                                tabPanel("Casing Below Water Table",
                                         h4(strong("What is casing depth below the water table?")),
                                         p("Casing helps to determine how far below the ground your well accesses groundwater. Casing helps to prevent loose rock or soil 
                                         from getting into your well. Casing also determines where within the aquifer your well is receiving its water. Wells that are 
                                         cased into the water table generally provide water that is deeper and generally older. Wells cased deeper into the water table often times have lower levels of 
                                           contaminants such as nitrate and chloride. This may depend on a variety of other factors such as where you are located and what types of
                                           bedrock your well is drilled into."),
                                         br(),
                                         h4(strong("Casing depth below the water table (feet)")),
                                         p("Diagram below illustrates the role of casing depth in reference to the water table. Colors correspond to map legend for the variable
                                           'Casing depth below water table'."),
                                         tags$div(casing),
                                ),
                                tabPanel("Nitrate Trends",
                                         h4(strong("Why are we interested in nitrate trends?")),
                                         p("Nitrate moves very easily through soil. As a result it helps
                                            us understand the degree to which land use is impacting 
                                            groundwater quality. By testing for nitrate annually, we are able 
                                            to see how groundwater quality is changing with respect to certain
                                            land use practices."),
                                         p("While nitrate may go up, down, or stay the same;
                                            changes in a similar direction can sometimes indicate a trend.
                                            Linear regression was used to determine if there was a relationship of nitrate
                                            to time. Wells with a p-value less than 0.10 and an r-squared value greater than 0.6 were 
                                            considered to have a significant trend for the purpose of this map."),
                                         p("Other wells may simply go up and down each year because of weather or other factors such as crop rotations. More years of data are required 
                                           in order to analyze for trends. As we obtain more years of data, we will be able to determine whether variability represents 
                                           actual trends or just wells that might be more susceptible to fluctuations."),
                                         p("A map of nitrate trends can be viewed by changing the variable to 'Nitrate Trends'. A nitrate
                                            trend map is only viewable for individual wells and are not summarized by municipality."),
                                         p("When viewing the map of trends for individual wells: Blue circles represent
                                            decreasing trends. Red circles respresent increasing trends. Grey circles
                                            indicate wells where no trend was observed. The size of the
                                            circle provides an idea of the rate of change observed in that well."), 
                                         p("Data on individual wells over time can be viewed for each parameter by clicking on the 'EXPLORE Project Data > 'Individual Wells' tab."),
                                        )
                              )),
                     tabPanel("EXPLORE project data",
                              tabsetPanel(
                                tabPanel("County Trends",
                                         h4(strong("Investigate Trends")),
                                         p("Overall trends can be visualized by comparing data from wells tested over multiple years. 
                                            For the purposes of this analysis, only wells that participated in all sampling years are included. As a result
                                            from year to year, the data summaries (i.e. mean, median, min, max, etc) may be slightly different depending on how many
                                            and which wells drop out of the project."),
                                         p("Select an analyte to see annual comparisons for each analyte."),
                                        selectInput("trend_analyte",
                                                     "Select analyte:",
                                                     choices = c("Nitrate","Alkalinity","Chloride","Total Hardness","pH","Conductivity"),
                                                     selected = "Nitrate",
                                                     width = 300
                                         ),
                                         br(),
                                         plotlyOutput("index_fig", height = 300),
                                         br(),
                                         h4(strong("Visualizing Trends")),
                                         p("The following figure displays all wells on the horizontal axis. 
                                          Depending on which analyte is selected, the concentration for each year is displayed 
                                          vertically. Wells are arranged from the highest average concentration on the left to the lowest on the right. 
                                          Color changes vertically indicate wells with greater variability. Clicking on the figure 
                                          will allow you to zoom into a particular part of the figure for more detail."),
                                         plotlyOutput("trend_fig", height = 400)
                                ),
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
                                                     choices = c("2022","2021","2020","2019"),
                                                     selected = "2022",
                                                     width = 300
                                         ),
                                         br(),
                                         plotlyOutput("select_analyte", height = 400)
                                ),
                                tabPanel("Wells, Geology, & Land Cover",
                                         h4(strong("Investigate Water Quality by Well and other Land Use Factors")),
                                         p("The following figures allow you to investigate well water quality grouped by different
                                           variables and categories. Boxplots help to illustrate results by year within the individual 
                                           categories. Please note that not all variables impact well water quality the same.
                                           For instance, land cover categories might be important for nitrate while bedrock type might be
                                           important for things like hardness or alkalinity. More information on these variables can 
                                           be found be clicking on the 'LEARN about other variables' tab."),
                                         br(),
                                         p("The drop down menu can also be used to select a particular analyte of interest. All figures
                                           will update to display the selected analyte. Please be patient, it may take a few moments for
                                           figures to be updated."),
                                         selectInput("wl_analyte",
                                                     "Select Analyte:",
                                                     choices = c("Nitrate",
                                                                 "Chloride",
                                                                 "Alkalinity",
                                                                 "pH",
                                                                 "Conductivity",
                                                                 "Total Hardness"),
                                                     selected = "Nitrate",
                                                     width = 300
                                         ),
                                         br(),
                                         plotlyOutput("wl_AG_HAY_PAST_CAT", height = 400),
                                         br(),
                                         plotlyOutput("wl_AG_CAT", height = 400),
                                         br(),
                                         plotlyOutput("wl_DAIRY_CAT", height = 400),
                                         br(),
                                         plotlyOutput("wl_HAY_PAST_CAT", height = 400),
                                         br(),
                                         plotlyOutput("wl_DRAIN_CAT", height = 400),
                                         br(),
                                         plotlyOutput("wl_DB_WT_CAT", height = 400),
                                         br(),
                                         plotlyOutput("wl_BEDROCK.TOP", height = 400)
                                    ),
                                
                                tabPanel("Annual Summary Statistics",
                                         tabPanel("Annual Summary",
                                         h4(strong("Investigate County-wide Summary Statistics")),
                                         p("Summary statistics for each year of the project can be generated for all wells tested as part 
                                         of the annual monitoring by selecting an individual year: "),
                                         selectInput("cty_year",
                                                     "Select Year:",
                                                     choices = c("2022","2021","2020","2019"),
                                                     selected = "2022",
                                                     width = 300
                                         ),
                                         br(),
                                         rHandsontableOutput("cty_summary_table", height = 300),
                                         p("Select an analyte to see summary statistics for all wells tested by year. 
                                         The number of wells sampled is written below each box plot. Drop in number of wells 
                                         from year to year is due to participants that failed to send in the annual sample 
                                         or chose to not continue participating in the project. For better year-to-year comparisons, 
                                           click on the 'County Trends' tab.Click on an individual analyte to generate box plots for all well samples over the 
                                           course of the project."),
                                         selectInput("cty_analyte",
                                                     "Select analyte:",
                                                     choices = c("Nitrate","Alkalinity","Chloride","Total Hardness","pH","Conductivity"),
                                                     selected = "Nitrate",
                                                     width = 300
                                         ),
                                         plotlyOutput("county_analyte", height = 400))
                                ))
                     )
                                  
                              
                            ))
                   ))
  
  