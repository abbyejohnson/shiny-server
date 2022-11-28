# Read in CSV Files
crops <- read.csv("N_Content_expanded.csv")
fert <- read.csv("Ammonia_Lost.csv")
soil <- read.csv("denitrification.csv")

#read in pictures
diagram1 <- tags$a(href='https://acsess.onlinelibrary.wiley.com/doi/pdfdirect/10.2136/1991.managingnitrogen.c5',
                   tags$img(src='field_management_fig.png', height = '390', width = '600', align = 'center'))


soil_drain <- soil %>% select(Soil.Drainage.Classification) %>% slice(1:5)


tabItem(tabName = "field",
        fluidRow(
          box(title = "Nitrogen Budget: Field Management",
              status = "info",
              solidHeader = T,
              width = 6,
              p("Field Management: When using the Nitrate Leaching Calculator to simulate field management, 
                the calculator allows for comparison of two management scenarios on the same field.
                By entering acreage it estimates a field based flow-weighted mean
                nitrate concentration relative to the proportion of each management practice and 
                overall water quality impact below the field."),
              p("The tool relies on user knowledge of various inputs such as yield, fertilizer, soil type, etc.
                Other factors are assigned using tables and other assumptions outlined in Meisenger and Randall (1991)."),
              p(strong("Using this tool:")),
              p("1. Enter information about soils and other environmental conditions."),
              p("2. Enter information about crop and nitrogen inputs for up to two management scenarios."),
              p("3. If field is irrigated and/or cover crop was grown, click yes to enter information."),
              p("4. If simulating the impact of altering a management practice on a portion of the field, enter the acreage for each scenario. 
                    The calculator will estimate a weighted leaching loss/nitrate concentration based on the acreage."),
              p("5. Scroll down to the bottom of the page to view your Nitrogen Budget."),
              p("For more information about various data inputs, hover over the 'i' next to the field name."),
          ),
          box(title = "",
              status = "info",
              solidHeader = F,
              width = 6,
              div(diagram1)),
        ),
        fluidRow(
          box(title = "Environmental Factors & Other Conditions", 
              width = 4,
              solidHeader = T,
              status = "primary",
              box(title = "Soil",
                  solidHeader = T,
                  status = "info",
                  width = 12,
                  column(6, 
                         strong("Soil Organic Matter Content"),
                         span(shiny::icon("info-circle"),
                              id = "info_soilOrgMatCont"),
                         selectInput("soilType",
                                     NULL,
                                     choices = soil$Organic.Matter.Content,
                                     selected = 1),
                         tippy::tippy_this(elementId = "info_soilOrgMatCont",
                                           tooltip = "<span style='font-size:16px;'>Select the percent soil organic matter<span>",
                                           placement = "right")
                  ),
                  column(6,
                         strong("Soil Drainage Classification"),
                         span(shiny::icon("info-circle"),
                              id = "info_drainClass"),
                         selectInput("drainClass",
                                     NULL,
                                     choices = soil_drain$Soil.Drainage.Classification,
                                     selected = 1),
                         tippy::tippy_this(elementId = "info_drainClass",
                                           tooltip = "<span style='font-size:16px;'>Select soil drainage classification<span>",
                                           placement = "right")
                  )
              ),
              box(title = "Change in N Storage",
                  solidHeader = T,
                  status = "info",
                  width = 12,
                  column(6,
                         strong("Change in Inorganic N"),
                         span(shiny::icon("info-circle"), 
                              id = "info_inoN"),
                         numericInput("inoN",
                                      NULL,
                                      value = 0,
                                      min = 0),
                         tippy::tippy_this(elementId = "info_inoN",
                                           tooltip = "<span style='font-size:16px;'>If you have know the change in NO3 and NH4 enter here, otherwise assume '0'<span>",
                                           placement = "right")
                  ),
                  column(6,
                         strong("Change in Organic N"),
                         span(shiny::icon("info-circle"), 
                              id = "info_orgN"),
                         # numericInput("orgN",
                         #              NULL,
                         #              value = 0,
                         #              min = 0),
                          uiOutput("orgN_storage"),
                         tippy::tippy_this(elementId = "info_orgN",
                                           tooltip = "<span style='font-size:16px;'>Soil organic matter content used to estimate change in organic N storage, if organic N measured can replace default value<span>",
                                           placement = "right")
                  )
              ),
              box(title = "Precipitation",
                  solidHeader = T,
                  status = "info",
                  width = 12,
                  column(6,
                         strong("Nitrate-N Concentration (mg/L)"),
                         span(shiny::icon("info-circle"), 
                              id = "info_precip"),
                         numericInput("precip",
                                      NULL,
                                      value = 0.5,
                                      min = 0),
                         tippy::tippy_this(elementId = "info_precip",
                                           tooltip = "<span style='font-size:16px;'>Enter the annual average concentration of nitrate-N in precipitation for your area if known. Default for Wisconsin is 0.5 mg/L nitrate-N.<span>",
                                           placement = "right")
                  ),
                  column(6,
                         strong("Annual Total (inches)"),
                         span(shiny::icon("info-circle"), 
                              id = "info_inchesPre"),
                         numericInput("InchPre",
                                      NULL,
                                      value = 32,
                                      min = 0),
                         tippy::tippy_this(elementId = "info_inchesPre",
                                           tooltip = "<span style='font-size:16px;'>Calculate this by entering irrigation water nitrate and inches applied<span>",
                                           placement = "right")
                  )
              ),
              box(title = "Irrigation",
                  solidHeader = T,
                  status = "info",
                  width = 12,
                  column(width = 6,
                         h3("Is this field irrigated?"),
                         span(shiny::icon("info-circle"),
                              id = "info_irr"),
                         radioButtons("irr",
                                      NULL,
                                      choices = list("No" = 0,
                                                     "Yes" = 1)
                         ),
                         tippy::tippy_this(elementId = "irr",
                                           tooltip = "<span style='font-size:16px;'>If this field was irrigated, click 'Yes'<span>",
                                           placement = "right")
                  ),
                  uiOutput("irrigation_Qs")
              )
          ),
          
          box(title = "Management Scenario 1",
              width = 4,
              solidHeader = T,
              status = "warning",
              box(title = "Crop Information",
                  solidHeader = T,
                  status = "info",
                  width = 12,
                  fluidRow(
                    column(6,
                           strong("Crop Type"),
                           span(shiny::icon("info-circle"), 
                                id = "info_crop1"),
                           selectInput("crop1",
                                       NULL,
                                       choices = crops$Crop.Type,
                                       selected = 1),
                           tippy::tippy_this(elementId = "info_crop1",
                                             tooltip = "<span style='font-size:16px;'>Select crop type for each field or portion of field<span>",
                                             placement = "right")
                    ),
                    column(6,
                           strong("Harvest Material / Units"),
                           span(shiny::icon("info-circle"), 
                                id = "info_units1"),
                           uiOutput("unit_selection1"),
                           tippy::tippy_this(elementId = "info_units1",
                                             tooltip = "<span style='font-size:16px;'>Type of harvested material and units of measure can be selected once a crop type is selected.<span>",
                                             placement = "right")
                    )
                  ),
                  fluidRow(
                    column(6,
                           strong("Yield"),
                           span(shiny::icon("info-circle"),
                                id = "info_yield1"),
                           numericInput("yield1",
                                        NULL,
                                        value = 0,
                                        min = 0),
                           tippy::tippy_this(elementId = "info_yield1",
                                             tooltip = "<span style='font-size:16px;'>Enter yield for each field or portion of field<span>",
                                             placement = "right")
                    ),
                    column(6,
                           strong("Acres"),
                           span(shiny::icon("info-circle"),
                                id = "info_acres1"),
                           numericInput("acres1",
                                        NULL,
                                        value = 0,
                                        min = 0),
                           tippy::tippy_this(elementId = "info_acres1",
                                             tooltip = "<span style='font-size:16px;'>Enter size of field/portion of field. Only used to calculate combined leachable N<span>",
                                             placement = "right")
                    )
                  )
              ),
              box(title = "Nitrogen Inputs",
                  solidHeader = T,
                  status = "info",
                  width = 12,
                  box(title = "Fertilizer",
                      solidHeader = T,
                      status = "info",
                      width = 12,
                      fluidRow(
                        column(12,
                               strong("Form"),
                               span(shiny::icon("info-circle"), 
                                    id = "info_fertform1"),
                               selectInput("fertform1",
                                           NULL,
                                           choices = fert$Fertilizer.N.Source,
                                           selected = 1),
                               tippy::tippy_this(elementId = "info_fertform1",
                                                 tooltip = "<span style='font-size:16px;'>Enter total amount of nitrogen fertilizer applied (lbs N/acre)<span>",
                                                 placement = "right")
                        )
                      ),
                      fluidRow(
                        column(6,
                               strong("Application Method"),
                               span(shiny::icon("info-circle"), 
                                    id = "info_fertApp1"),
                               uiOutput("fert_selection1"),
                               tippy::tippy_this(elementId = "info_fertApp1",
                                                 tooltip = "<span style='font-size:16px;'>Select the application method<span>",
                                                 placement = "right")
                        ),
                        column(6,
                               strong("Rate (lbs N/acre)"),
                               span(shiny::icon("info-circle"), 
                                    id = "info_fert1"),
                               numericInput("fert1",
                                            NULL,
                                            value = 0,
                                            min = 0),
                               tippy::tippy_this(elementId = "info_fert1",
                                                 tooltip = "<span style='font-size:16px;'>Enter total amount of nitrogen fertilizer applied (lbs N/acre)<span>",
                                                 placement = "right")
                        )
                      )
                  ),
                  box(title = "Manure and Previous Year Manure Credits",
                      solidHeader = T,
                      status = "info",
                      width = 12,
                      column(6,
                             strong("Manure"),
                             span(shiny::icon("info-circle"), 
                                  id = "info_man1"),
                             numericInput("man1",
                                          NULL,
                                          value = 0,
                                          min = 0),
                             tippy::tippy_this(elementId = "info_man1",
                                               tooltip = "<span style='font-size:16px;'>First year available N and any N credits from previous years<span>",
                                               placement = "right")
                      )
                      # column(6,
                      #        strong("Symbiotic N Fixation (Legumes)"),
                      #        span(shiny::icon("info-circle"), 
                      #             id = "info_leg1"),
                      #        numericInput("leg1",
                      #                     NULL,
                      #                     value = 0,
                      #                     min = 0),
                      #        tippy::tippy_this(elementId = "info_leg1",
                      #                          tooltip = "<span style='font-size:16px;'>If no legumes grown, this value should be 0.<br/>If legumes grown, enter pounds of nitrogen produced from growing legumes<span>",
                      #                          placement = "right")
                      # )
                  )
              ),
              box(title = "Cover Crop Residue (lbs N/acre)",
                  solidHeader = T,
                  status = "info",
                  width = 12,
                  column(width = 6,
                         h3("Did you plant a cover crop?"),
                         span(shiny::icon("info-circle"),
                              id = "info_cover1YN"),
                         radioButtons("cp1",
                                      NULL,
                                      choices = list("No" = 0,
                                                     "Yes" = 1)
                         ),
                         tippy::tippy_this(elementId = "info_cover1YN",
                                           tooltip = "<span style='font-size:20px;'>If cover crop was grown click yes to enter information, otherwise leave as 'no'<span>",
                                           placement = "right")
                  ),
                  uiOutput("plot1cover")
                  
              )
          ),
          box(title = "Management Scenario 2",
              width = 4,
              solidHeader = T,
              status = "danger",
              box(title = "Crop Information",
                  solidHeader = T,
                  status = "info",
                  width = 12,
                  fluidRow(
                    column(6,
                           strong("Crop Type"),
                           span(shiny::icon("info-circle"), 
                                id = "info_crop2"),
                           selectInput("crop2",
                                       NULL,
                                       choices = crops$Crop.Type,
                                       selected = 1),
                           tippy::tippy_this(elementId = "info_crop2",
                                             tooltip = "<span style='font-size:16px;'>Select crop type for each field or portion of field<span>",
                                             placement = "right")
                    ),
                    column(6,
                           strong("Harvest Material / Units"),
                           span(shiny::icon("info-circle"), 
                                id = "info_units2"),
                           uiOutput("unit_selection2"),
                           tippy::tippy_this(elementId = "info_units2",
                                             tooltip = "<span style='font-size:16px;'>Type of harvested material and units of measure can be selected once a crop type is selected.<span>",
                                             placement = "right")
                    )
                  ),
                  fluidRow(
                    column(6,
                           strong("Yield"),
                           span(shiny::icon("info-circle"),
                                id = "info_yield1"),
                           numericInput("yield2",
                                        NULL,
                                        value = 0,
                                        min = 0),
                           tippy::tippy_this(elementId = "info_yield2",
                                             tooltip = "<span style='font-size:16px;'>Enter Yield for each field or portion of field<span>",
                                             placement = "right")
                    ),
                    column(6,
                           strong("Acres"),
                           span(shiny::icon("info-circle"),
                                id = "info_acres2"),
                           numericInput("acres2",
                                        NULL,
                                        value = 0,
                                        min = 0),
                           tippy::tippy_this(elementId = "info_acres2",
                                             tooltip = "<span style='font-size:16px;'>Enter size of field/portion of field. Only used to calculate combined leachable N<span>",
                                             placement = "right")
                    )
                  )
              ),
              box(title = "Nitrogen Inputs",
                  solidHeader = T,
                  status = "info",
                  width = 12,
                  box(title = "Fertilizer",
                      solidHeader = T,
                      status = "info",
                      width = 12,
                      fluidRow(
                        column(12,
                               strong("Form"),
                               span(shiny::icon("info-circle"), 
                                    id = "info_fertform2"),
                               selectInput("fertform2",
                                           NULL,
                                           choices = fert$Fertilizer.N.Source,
                                           selected = 1),
                               tippy::tippy_this(elementId = "info_fertform2",
                                                 tooltip = "<span style='font-size:16px;'>Enter total amount of nitrogen fertilizer applied (lbs N/acre)<span>",
                                                 placement = "right")
                        )
                      ),
                      fluidRow(
                        column(6,
                               strong("Application Method"),
                               span(shiny::icon("info-circle"), 
                                    id = "info_fertApp2"),
                               uiOutput("fert_selection2"),
                               tippy::tippy_this(elementId = "info_fertApp1",
                                                 tooltip = "<span style='font-size:16px;'>Select the application method<span>",
                                                 placement = "right")
                        ),
                        column(6,
                               strong("Rate (lbs N/acre)"),
                               span(shiny::icon("info-circle"), 
                                    id = "info_fert2"),
                               numericInput("fert2",
                                            NULL,
                                            value = 0,
                                            min = 0),
                               tippy::tippy_this(elementId = "info_fert2",
                                                 tooltip = "<span style='font-size:16px;'>Enter total amount of nitrogen fertilizer applied (lbs N/acre)<span>",
                                                 placement = "right")
                        )
                      )
                  ),
                  box(title = "Manure and Previous Year Manure Credits",
                      solidHeader = T,
                      status = "info",
                      width = 12,
                      column(6,
                             strong("Manure"),
                             span(shiny::icon("info-circle"), 
                                  id = "info_man2"),
                             numericInput("man2",
                                          NULL,
                                          value = 0,
                                          min = 0),
                             tippy::tippy_this(elementId = "info_man2",
                                               tooltip = "<span style='font-size:16px;'>If manure applied, enter first year available N and any N credits from previous years<span>",
                                               placement = "right")
                      )
                      # column(6,
                      #        strong("Symbiotic N Fixation (Legumes)"),
                      #        span(shiny::icon("info-circle"), 
                      #             id = "info_leg2"),
                      #        numericInput("leg2",
                      #                     NULL,
                      #                     value = 0,
                      #                     min = 0),
                      #        tippy::tippy_this(elementId = "info_leg2",
                      #                          tooltip = "<span style='font-size:16px;'>If no legumes grown, this value should be 0.<br/>If legumes grown, enter pounds of nitrogen produced from growing legumes<span>",
                      #                          placement = "right")
                      # )
                  )
              ),
              box(title = "Cover Crop Residue (lbs N/acre)",
                  solidHeader = T,
                  status = "info",
                  width = 12,
                  column(width = 6,
                         h3("Did you plant a cover crop?"),
                         span(shiny::icon("info-circle"),
                              id = "info_cover2YN"),
                         radioButtons("cp2",
                                      NULL,
                                      choices = list("No" = 0,
                                                     "Yes" = 1)
                         ),
                         tippy::tippy_this(elementId = "info_cover2YN",
                                           tooltip = "<span style='font-size:16px;'>If cover crop was grown click yes to enter information, otherwise leave as 'no'<span>",
                                           placement = "right")
                  ),
                  uiOutput("plot2cover")
                  
              )
          )
        ),
        fluidRow(
          column(12,
                 box(title = "Nitrogen Balance",
                     solidHeader = T,
                     status = "success",
                     width = 12,
                     column(6,
                            fluidRow(
                              box(title = "Crops & Environmental Factors",
                                  solidHeader = T,
                                  status = "info",
                                  width = 12,
                                  rHandsontableOutput("rhot1"))
                            ),
                            fluidRow(
                              box(title = "Inputs (lbs N/acre)",
                                  solidHeader = T,
                                  status = "info",
                                  width = 12,
                                  rHandsontableOutput("rhot2"))
                            ),
                            fluidRow(
                              box(title = "Outputs (lbs N/acre)",
                                  solidHeader = T,
                                  status = "info",
                                  width = 12,
                                  rHandsontableOutput("rhot3"))
                            )
                            
                     ),
                     column(6,
                            fluidRow(
                              box(title = "Change in N Storage (lbs N/acre)",
                                  solidHeader = T,
                                  status = "info",
                                  width = 12,
                                  rHandsontableOutput("rhot4"))
                            ),
                            fluidRow(
                              box(title = "Cover Crop Residue (lbs N/acre)",
                                  solidHeader = T,
                                  status = "info",
                                  width = 12,
                                  rHandsontableOutput("rhot5"))
                            ),
                            box(title = "Potential Leachable Nitrogen",
                                solidHeader = T,
                                status = "success",
                                width = 12,
                                fluidRow(
                                  column(6,
                                         box(title = "Management Scenario 1",
                                             solidHeader = T,
                                             status = "warning",
                                             width = 12,
                                             column(9,
                                                    h4("Leachable N (lb/acre):"),
                                                    h4("Nitrate-N (mg/L):")),
                                             column(3,
                                                    h4(uiOutput("OP_value1")),
                                                    h4(uiOutput("OP_value3"))
                                             )
                                         )
                                  ),
                                  column(6,
                                         box(title = "Management Scenario 2",
                                             solidHeader = T,
                                             status = "danger",
                                             width = 12,
                                             column(9,
                                                    h4("Leachable N (lb/acre):"),
                                                    h4("Nitrate-N (mg/L):")),
                                             column(3,
                                                    h4(uiOutput("OP_value2")),
                                                    h4(uiOutput("OP_value4")))
                                         )
                                  )
                                ),
                                fluidRow(
                                  column(12,
                                         box(title = "Total for Field",
                                             solidHeader = T,
                                             status = "primary",
                                             width = 12,
                                             column(6,
                                                    h4("Total Leachable N (lb/acre)"),
                                                    h4("Field-Weighted Nitrate-N (mg/L)")),
                                             column(6,
                                                    h4(uiOutput("OP_value5")),
                                                    h4(uiOutput("OP_value6")))
                                             
                                             
                                             
                                             
                                         )
                                  )
                                )
                            )
                     )  
                 )   
          )
        )
)