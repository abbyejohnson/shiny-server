# Read in CSV Files
crops <- read.csv("N_Content_expanded.csv")
fert <- read.csv("Ammonia_Lost.csv")
soil <- read.csv("denitrification.csv")

#read in pictures
diagram1 <- tags$a(href='https://acsess.onlinelibrary.wiley.com/doi/pdfdirect/10.2136/1991.managingnitrogen.c5',
                   tags$img(src='soil_profile.png', height = '700', width = '250', align = 'center'))
uwex <- tags$a(tags$img(src='uwex.png', height = '75', width = '275', align = 'center'))
uwsp <- tags$a(tags$img(src='uwsp.png', height = '75', width = '325', align = 'center'))


#tabItem(tabName = "about")
tabItem(tabName = "about",
        fluidRow(
          box(title = "Overview",
              status = "info",
              solidHeader = T,
              width = 7,
              p("The Nitrate Leaching Calculator uses the conservation of mass to estimate potential nitrate leaching simulations of various management scenarios. 
                The Field Management option allow for comparing impacts of two management scenarios within a field. The Rotation Management option allows for 
                simulation of nitrate leaching losses from one field over the course of a four year rotation."),  
              p("The tool relies on user knowledge of various inputs such as yield, fertilizer, soil type, etc.
                Other factors are assigned using tables and other assumptions outlined in Meisenger and Randall (1991)."),
              
              p(strong("What information do you need:")),
              p("1. Soil organic matter content"),
              p("2. Soil drainage classification"),
              p("3. Soil pH"),
              p("4. Nitrogen inputs (i.e. nitrogen fertilizer, manure, or other biosolid application rates"),
              p("5. Information on fertilizer form and application method"),
              p("6. Crop type and yield"),
              p("7. If irrigated, irrigation nitrate concentration and amount applied"),
              p("8. If cover crop was grown, type of cover crop and biomass estimate"),
              br(),
              br(),
              br(),
              div(uwex,uwsp),
              div(strong("Created by: Kevin Masarik and Grant Moser")),
              div("Center for Watershed Science and Education"),
              div("Last modified:", "November 19, 2022",'.', a(href="mailto:kmasarik@uwsp.edu","Contact us for questions")),
              ),
          box(title = "",
              status = "info",
              solidHeader = F,
              width = 5,
              div(diagram1)),
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
                         rHandsontableOutput("rhot_table_1")
                         #rhandsontable(soil)
                         ),
                     box(title = "Ammonia Loss",
                         solidHeader = T,
                         status = "info",
                         width = 12,
                         rHandsontableOutput("rhot_table_2")
                         ),
                    box(title = "Outputs",
                         solidHeader = T,
                         status = "info",
                         width = 12,
                         #rHandsontableOutput("rhot3_yr4")
                         rhandsontable(crops, readOnly = TRUE)
                         )
                     # box(title = "Change in N Storage",
                     #     solidHeader = T,
                     #     status = "info",
                     #     width = 12,
                     #     #rHandsontableOutput("rhot4_yr4")
                     #     ),
                     # box(title = "Cover Crop Residue",
                     #     solidHeader = T,
                     #     status = "info",
                     #     #width = 12,
                     #     #rHandsontableOutput("rhot5_yr4")
                     #     )
          )
        )
      ))