# Read in CSV Files
crops <- read.csv("N_Content_expanded.csv")
fert <- read.csv("Ammonia_Lost.csv")
soil <- read.csv("denitrification.csv")

#read in pictures
#diagram1 <- tags$a(href='https://acsess.onlinelibrary.wiley.com/doi/pdfdirect/10.2136/1991.managingnitrogen.c5',
#                   tags$img(src='soil_profile.png', height = '700', width = '250', align = 'center'))
diagram1 <- tags$a(href='https://acsess.onlinelibrary.wiley.com/doi/pdfdirect/10.2136/1991.managingnitrogen.c5',
                   tags$img(src='nbudget.png', style = "height: 400px; width: 100%", align = 'center'))


uwex <- tags$a(tags$img(src='uwex.png', height = '75', width = '275', align = 'center'))
uwsp <- tags$a(tags$img(src='uwsp.png', height = '75', width = '325', align = 'center'))


#tabItem(tabName = "about")
tabItem(tabName = "about",
        fluidRow(
          box(title = "Overview",
              status = "info",
              solidHeader = T,
              width = 12,
              p(strong("The Nitrate Leaching Calculator uses the conservation of mass to estimate potential nitrate leaching simulations of various management scenarios.")),
              p("-The Field Management option allow for comparing impacts of two management scenarios within a field."),
              p("-The Rotation Management option allows for simulation of nitrate leaching losses from one field over the course of a four year rotation."),  
              br(),
              div(diagram1),
              br(),
              p("Use of the app requires knowledge of various inputs such as yield, fertilizer, soil type, etc.
                The Nitrate Leaching Calculator is based off of work by Meisenger and Randall (1991) and users would benefit from
                reading their work to better understand all of the underlying assumptions. It is going to be more accurate when actual yields 
                are used rather than yield goals. As a result it is more appropriate to think of this as a hindcasting model to understand 
                the potential leachable N at the end of the time period rather than a predictive tool. Many of the assumptions 
                are based on the calculator being used for typical Wisconsin agricultural systems and climate."),
              p(a(href="https://acsess.onlinelibrary.wiley.com/doi/10.2136/1991.managingnitrogen.c5","Meisinger, J.J., Randall, G.W., 1991. Estimating Nitrogen Budgets for Soil-Crop Systems. 
                Managing Nitrogen for Groundwater Quality and Farm Profitability, 
                pp. 85-124.")),
              br(),
              p(strong("What information do you need:")),
              p("1. Soil organic matter content"),
              p("2. Soil drainage classification"),
              p("3. Soil pH"),
              p("4. Nitrogen inputs (i.e. nitrogen fertilizer, manure, or other biosolid application rates)"),
              p("5. Information on fertilizer form and application method"),
              p("6. Crop type and yield"),
              p("7. If irrigated, irrigation nitrate concentration and amount applied"),
              p("8. If cover crop was grown, type of cover crop and biomass estimate"),
              br(),
               div(uwex,uwsp),
               div(strong("Created by: Kevin Masarik and Grant Moser")),
               div("Nitrate Leaching Calculator - BETA VERSION 1.3"),
               div("Center for Watershed Science and Education"),
               div("Last modified:", "July 5, 2023",'.', a(href="mailto:kmasarik@uwsp.edu","Contact us for questions", target="_blank")),
               ),
          # box(title = "",
          #     status = "info",
          #     solidHeader = F,
          #     width = 5,
          #     div(diagram1)),
        ),
        fluidRow(
          box(title = "Reference Tables",
              width = 12,
              solidHeader = T,
              status = "success",
              column(12,
                   box(title = "Denitrification",
                         solidHeader = T,
                         status = "info",
                         width = 12,
                         p("Values used for estimating denitrification come from Table 5-7 from Meisenger and Randall (1991). 
                         Percent of inorganic N (fertilizer, precipitation, irrigation) that is anticipated to denitrify
                         for various soil drainage types and organic matter contents."),
                         rHandsontableOutput("rhot_table_1")
                         ),
                     box(title = "Ammonia Loss",
                         solidHeader = T,
                         status = "info",
                         width = 12,
                         p("Values used for Ammonia Loss modified from Table 5-6.1 from Meisenger and Randall (1991). Used middle of range
                         for approximate ammonia loss as a percentage of fertilizer inputs based on form, application 
                         method, and soil pH. Assumed rain within 7 days of application."),
                         rHandsontableOutput("rhot_table_2")
                         ),
                    box(title = "Outputs",
                         solidHeader = T,
                         status = "info",
                         width = 12,
                        p("This table was adapted from Table 5-4 of Meisenger and Randall (1991) and includes the estimated N contents of the harvested portion of the crops and vegetables.
                          When multiplied by the yield in listed units and standardized for percent moisture the N removed via harvested
                          portion of the crop can be calculated. The middle of the general range was used for purposes of this table."),  
                          rhandsontable(crops, readOnly = TRUE)
                         ),
               
          )
        )
        )

      )