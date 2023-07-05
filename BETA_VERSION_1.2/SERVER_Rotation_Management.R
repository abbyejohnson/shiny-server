output$year1CropUnits <- renderUI({
  unitsSelections_yr1 <- filter(crops, Crop.Type == input$crop_yr1)
  
  selectInput("unitsYr1",
              NULL,
              choices = unitsSelections_yr1$Category)
})

output$year2CropUnits <- renderUI({
  unitsSelections_yr2 <- filter(crops, Crop.Type == input$crop_yr2)
  
  selectInput("unitsYr2",
              NULL,
              choices = unitsSelections_yr2$Category)
})

output$year3CropUnits <- renderUI({
  unitsSelections_yr3 <- filter(crops, Crop.Type == input$crop_yr3)
  
  selectInput("unitsYr3",
              NULL,
              choices = unitsSelections_yr3$Category)
})

output$year4CropUnits <- renderUI({
  unitsSelections_yr4 <- filter(crops, Crop.Type == input$crop_yr4)
  
  selectInput("unitsYr4",
              NULL,
              choices = unitsSelections_yr4$Category)
})

output$irrigation_Qs_yr1 <- renderUI({
  if (input$irr_yr1 == 1){
    fluidRow(
      column(6,
             strong("Nitrate-N Concention (mg/L)"),
             span(shiny::icon("info-circle"),
                  id = "info_irri"),
             numericInput("irri_yr1",
                          NULL,
                          value = 18,
                          min = 0),
             
             tippy::tippy_this(elementId = "info_irri",
                               tooltip = "Enter the nitrate-N concentration of the 
                                  water supply (if known). </br>
                                  This can be determined by collecting a sample
                                  from the irrigation well </br>
                                  and having it analyzed for nitrate-nitrogen.</br></br>
                                  Nitrate concentations vary from well to well, 
                                  but are fairly consistent </br>
                                  during the growing season and from year to 
                                  year. </br></br>
                                  If concentration not known: </br>
                                  Use 18 mg/L which is an average concentration for 
                                  irrigation wells </br> 
                                  in Central Wisconsin.",
                               placement = "right")
      ),
      column(6,
             strong("Irrigation water applied (inches)"),
             span(shiny::icon("info-circle"),
                  id = "info_irrInch"),
             numericInput("irrInch_yr1",
                          NULL,
                          value = 0,
                          min = 0),
             
             tippy::tippy_this(elementId = "info_irrInch",
                               tooltip = "Enter total inches of irrigation water applied during the growing season",
                               placement = "right")
      )
    )
  }else{
    NULL
  }
})

output$plot_cover_yr1 <- renderUI({
  if (input$cp_yr1 == 1){
    column(6,
           strong("Cover Crop Type 1"),
           span(shiny::icon("info-circle"),
                id = "info_coverCropType1"),
           selectInput("cct_yr1",
                       NULL,
                       choices = crops$Crop.Type,
                       selected = 1),
           tippy::tippy_this(elementId = "info_coverCropType_yr1",
                             tooltip = "Cover Crops asdfasdfasdf",
                             placement = "right"
           ),
           strong("Cover Crop Units 1"),
           span(shiny::icon("info-circle"),
                id = "info_coverCropunit1"),
           uiOutput("cover_crop_selection_yr1"),
           
           
           strong("Cover Crop Yield 1"),
           span(shiny::icon("info-circle"),
                id = "info_coverCropYield1"),
           numericInput("ccy_yr1",
                        NULL,
                        value = 0,
                        min = 0),
           tippy::tippy_this(elementId = "info_coverCropYield_yr1",
                             tooltip = "Cover Crops asdfasdfasdf",
                             placement = "right")
    )
  }else{
    NULL
  }
  
})

output$cover_crop_selection_yr1 <- renderUI({
  cover_units_yr1 <- filter(crops, Crop.Type == input$cct_yr1)
  
  selectInput("c_units_yr1",
              NULL,
              choices = cover_units_yr1$Category)
})

output$fert_selection_yr1 <- renderUI({
  fert_sel <- filter(fert, Fertilizer.N.Source == input$fertform_yr1)
  
  selectInput("appMeth_yr1",
              NULL,
              choices = fert_sel$Application.Method)
})

output$irrigation_Qs_yr2 <- renderUI({
  if (input$irr_yr2 == 1){
    fluidRow(
      column(6,
             strong("Nitrate-N Concention (mg/L)"),
             span(shiny::icon("info-circle"),
                  id = "info_irri"),
             numericInput("irri_yr2",
                          NULL,
                          value = 18,
                          min = 0),
             
             tippy::tippy_this(elementId = "info_irri",
                               tooltip = "Enter the nitrate-N concentration of the 
                                  water supply (if known). </br>
                                  This can be determined by collecting a sample
                                  from the irrigation well </br>
                                  and having it analyzed for nitrate-nitrogen.</br></br>
                                  Nitrate concentations vary from well to well, 
                                  but are fairly consistent </br>
                                  during the growing season and from year to 
                                  year. </br></br>
                                  If concentration not known: </br>
                                  Use 18 mg/L which is an average concentration for 
                                  irrigation wells </br> 
                                  in Central Wisconsin.",
                               placement = "right")
      ),
      column(6,
             strong("Irrigation water applied (inches)"),
             span(shiny::icon("info-circle"),
                  id = "info_irrInch"),
             numericInput("irrInch_yr2",
                          NULL,
                          value = 0,
                          min = 0),
             
             tippy::tippy_this(elementId = "info_irrInch",
                               tooltip = "Enter total inches of irrigation water applied during the growing season",
                               placement = "right")
      )
    )
  }else{
    NULL
  }
})

output$plot_cover_yr2 <- renderUI({
  if (input$cp_yr2 == 1){
    column(6,
           strong("Cover Crop Type 1"),
           span(shiny::icon("info-circle"),
                id = "info_coverCropType1"),
           selectInput("cct_yr2",
                       NULL,
                       choices = crops$Crop.Type,
                       selected = 1),
           tippy::tippy_this(elementId = "info_coverCropType_yr2",
                             tooltip = "Cover Crops asdfasdfasdf",
                             placement = "right"
           ),
           strong("Cover Crop Units 1"),
           span(shiny::icon("info-circle"),
                id = "info_coverCropunit1"),
           uiOutput("cover_crop_selection_yr2"),
           
           
           strong("Cover Crop Yield 1"),
           span(shiny::icon("info-circle"),
                id = "info_coverCropYield1"),
           numericInput("ccy_yr2",
                        NULL,
                        value = 0,
                        min = 0),
           tippy::tippy_this(elementId = "info_coverCropYield_yr2",
                             tooltip = "Cover Crops asdfasdfasdf",
                             placement = "right")
    )
  }else{
    NULL
  }
  
})

output$cover_crop_selection_yr2 <- renderUI({
  cover_units_yr2 <- filter(crops, Crop.Type == input$cct_yr2)

  selectInput("c_units_yr2",
              NULL,
              choices = cover_units_yr2$Category)
})

output$fert_selection_yr2 <- renderUI({
  fert_sel <- filter(fert, Fertilizer.N.Source == input$fertform_yr2)

  selectInput("appMeth_yr2",
              NULL,
              choices = fert_sel$Application.Method)
})

output$irrigation_Qs_yr3 <- renderUI({
  if (input$irr_yr3 == 1){
    fluidRow(
      column(6,
             strong("Nitrate-N Concention (mg/L)"),
             span(shiny::icon("info-circle"),
                  id = "info_irri"),
             numericInput("irri_yr3",
                          NULL,
                          value = 18,
                          min = 0),
             
             tippy::tippy_this(elementId = "info_irri",
                               tooltip = "Enter the nitrate-N concentration of the 
                                  water supply (if known). </br>
                                  This can be determined by collecting a sample
                                  from the irrigation well </br>
                                  and having it analyzed for nitrate-nitrogen.</br></br>
                                  Nitrate concentations vary from well to well, 
                                  but are fairly consistent </br>
                                  during the growing season and from year to 
                                  year. </br></br>
                                  If concentration not known: </br>
                                  Use 18 mg/L which is an average concentration for 
                                  irrigation wells </br> 
                                  in Central Wisconsin.",
                               placement = "right")
      ),
      column(6,
             strong("Irrigation water applied (inches)"),
             span(shiny::icon("info-circle"),
                  id = "info_irrInch"),
             numericInput("irrInch_yr3",
                          NULL,
                          value = 0,
                          min = 0),
             
             tippy::tippy_this(elementId = "info_irrInch",
                               tooltip = "Enter total inches of irrigation water applied during the growing season",
                               placement = "right")
      )
    )
  }else{
    NULL
  }
})

output$plot_cover_yr3 <- renderUI({
  if (input$cp_yr3 == 1){
    column(6,
           strong("Cover Crop Type 1"),
           span(shiny::icon("info-circle"),
                id = "info_coverCropType1"),
           selectInput("cct_yr3",
                       NULL,
                       choices = crops$Crop.Type,
                       selected = 1),
           tippy::tippy_this(elementId = "info_coverCropType_yr3",
                             tooltip = "Cover Crops asdfasdfasdf",
                             placement = "right"
           ),
           strong("Cover Crop Units 1"),
           span(shiny::icon("info-circle"),
                id = "info_coverCropunit1"),
           uiOutput("cover_crop_selection_yr3"),
           
           
           strong("Cover Crop Yield 1"),
           span(shiny::icon("info-circle"),
                id = "info_coverCropYield1"),
           numericInput("ccy_yr3",
                        NULL,
                        value = 0,
                        min = 0),
           tippy::tippy_this(elementId = "info_coverCropYield_yr3",
                             tooltip = "Cover Crops asdfasdfasdf",
                             placement = "right")
    )
  }else{
    NULL
  }
  
})

output$cover_crop_selection_yr3 <- renderUI({
  cover_units_yr3 <- filter(crops, Crop.Type == input$cct_yr3)
  
  selectInput("c_units_yr3",
              NULL,
              choices = cover_units_yr3$Category)
})

output$fert_selection_yr3 <- renderUI({
  fert_sel <- filter(fert, Fertilizer.N.Source == input$fertform_yr3)
  
  selectInput("appMeth_yr3",
              NULL,
              choices = fert_sel$Application.Method)
})

output$irrigation_Qs_yr4 <- renderUI({
  if (input$irr_yr4 == 1){
    fluidRow(
      column(6,
             strong("Nitrate-N Concention (mg/L)"),
             span(shiny::icon("info-circle"),
                  id = "info_irri"),
             numericInput("irri_yr4",
                          NULL,
                          value = 18,
                          min = 0),
             
             tippy::tippy_this(elementId = "info_irri",
                               tooltip = "Enter the nitrate-N concentration of the 
                                  water supply (if known). </br>
                                  This can be determined by collecting a sample
                                  from the irrigation well </br>
                                  and having it analyzed for nitrate-nitrogen.</br></br>
                                  Nitrate concentations vary from well to well, 
                                  but are fairly consistent </br>
                                  during the growing season and from year to 
                                  year. </br></br>
                                  If concentration not known: </br>
                                  Use 18 mg/L which is an average concentration for 
                                  irrigation wells </br> 
                                  in Central Wisconsin.",
                               placement = "right")
      ),
      column(6,
             strong("Irrigation water applied (inches)"),
             span(shiny::icon("info-circle"),
                  id = "info_irrInch"),
             numericInput("irrInch_yr4",
                          NULL,
                          value = 0,
                          min = 0),
             
             tippy::tippy_this(elementId = "info_irrInch",
                               tooltip = "Enter total inches of irrigation water applied during the growing season",
                               placement = "right")
      )
    )
  }else{
    NULL
  }
})

output$plot_cover_yr4 <- renderUI({
  if (input$cp_yr4 == 1){
    column(6,
           strong("Cover Crop Type 1"),
           span(shiny::icon("info-circle"),
                id = "info_coverCropType1"),
           selectInput("cct_yr4",
                       NULL,
                       choices = crops$Crop.Type,
                       selected = 1),
           tippy::tippy_this(elementId = "info_coverCropType_yr4",
                             tooltip = "Cover Crops asdfasdfasdf",
                             placement = "right"
           ),
           strong("Cover Crop Units 1"),
           span(shiny::icon("info-circle"),
                id = "info_coverCropunit1"),
           uiOutput("cover_crop_selection_yr4"),


           strong("Cover Crop Yield 1"),
           span(shiny::icon("info-circle"),
                id = "info_coverCropYield1"),
           numericInput("ccy_yr4",
                        NULL,
                        value = 0,
                        min = 0),
           tippy::tippy_this(elementId = "info_coverCropYield_yr4",
                             tooltip = "Cover Crops asdfasdfasdf",
                             placement = "right")
    )
  }else{
    NULL
  }

})

output$cover_crop_selection_yr4 <- renderUI({
  cover_units_yr4 <- filter(crops, Crop.Type == input$cct_yr4)

  selectInput("c_units_yr4",
              NULL,
              choices = cover_units_yr4$Category)
})

output$fert_selection_yr4 <- renderUI({
  fert_sel <- filter(fert, Fertilizer.N.Source == input$fertform_yr4)

  selectInput("appMeth_yr4",
              NULL,
              choices = fert_sel$Application.Method)
})

output$rhot1_RM <- renderRHandsontable({
  
  df_N_Content_yr1 <- crops %>%
    filter(Crop.Type == input$crop_yr1 & Category == input$unitsYr1) %>%
    select(Avg.N.harvested..lb.N.unit.)
  
  N_Content_yr1 <- df_N_Content_yr1[1,1]
  
  df_recharge_yr1 <- soil %>%
    filter(Organic.Matter.Content == input$soilType_RM & Soil.Drainage.Classification == input$drainClass_RM) %>%
    select(Inches.Recharge)
  
  recharge_yr1 <- df_recharge_yr1[1,1]
  
  if (input$irr_yr1 == 1){
    irr_in_yr1 <- input$irrInch_yr1
  } else{
    irr_in_yr1 <- 0 
  }
  
  df_N_Content_yr2 <- crops %>%
    filter(Crop.Type == input$crop_yr2 & Category == input$unitsYr2) %>%
    select(Avg.N.harvested..lb.N.unit.)
  
  N_Content_yr2 <- df_N_Content_yr2[1,1]
  
  df_recharge_yr2 <- soil %>%
    filter(Organic.Matter.Content == input$soilType_RM & Soil.Drainage.Classification == input$drainClass_RM) %>%
    select(Inches.Recharge)
  
  recharge_yr2 <- df_recharge_yr2[1,1]
  
  if (input$irr_yr2 == 1){
    irr_in_yr2 <- input$irrInch_yr2
  } else{
    irr_in_yr2 <- 0 
  }
  
  df_N_Content_yr3 <- crops %>%
    filter(Crop.Type == input$crop_yr3 & Category == input$unitsYr3) %>%
    select(Avg.N.harvested..lb.N.unit.)
  
  N_Content_yr3 <- df_N_Content_yr3[1,1]
  
  df_recharge_yr3 <- soil %>%
    filter(Organic.Matter.Content == input$soilType_RM & Soil.Drainage.Classification == input$drainClass_RM) %>%
    select(Inches.Recharge)
  
  recharge_yr3 <- df_recharge_yr3[1,1]
  
  if (input$irr_yr3 == 1){
    irr_in_yr3 <- input$irrInch_yr3
  } else{
    irr_in_yr3 <- 0 
  }
  
  
  df_N_Content_yr4 <- crops %>%
    filter(Crop.Type == input$crop_yr4 & Category == input$unitsYr4) %>%
    select(Avg.N.harvested..lb.N.unit.)
  
  N_Content_yr4 <- df_N_Content_yr4[1,1]
  
  df_recharge_yr4 <- soil %>%
    filter(Organic.Matter.Content == input$soilType_RM & Soil.Drainage.Classification == input$drainClass_RM) %>%
    select(Inches.Recharge)
  
  recharge_yr4 <- df_recharge_yr4[1,1]
  
  if (input$irr_yr4 == 1){
    irr_in_yr4 <- input$irrInch_yr4
  } else{
    irr_in_yr4 <- 0 
  }
  
  
  cop_info <- matrix(c("Year 1", input$crop_yr1, N_Content_yr1, input$yield_yr1, input$acres_yr1, input$soilType_RM, input$drainClass_RM, recharge_yr1, irr_in_yr1,
                       "Year 2", input$crop_yr2, N_Content_yr2, input$yield_yr2, input$acres_yr2, input$soilType_RM, input$drainClass_RM, recharge_yr2, irr_in_yr2,
                       "Year 3", input$crop_yr3, N_Content_yr3, input$yield_yr3, input$acres_yr3, input$soilType_RM, input$drainClass_RM, recharge_yr3, irr_in_yr3,
                       "Year 4", input$crop_yr4, N_Content_yr4, input$yield_yr4, input$acres_yr4, input$soilType_RM, input$drainClass_RM, recharge_yr4, irr_in_yr4),
                     nrow = 9, ncol = 4, 
                     dimnames = list(c("","Crop Type", "N Content", "Yield", "Acres", "Soil Org Matter %", "Soil Drainage", "Recharge Estimate (in.)", "Irrigation Applied (in.)")))
  rhandsontable(cop_info, 
                rowHeaderWidth = 240, 
                readOnly = TRUE)
})

output$rhot2_RM <- renderRHandsontable({
  
  if (input$irr_yr1 == 1){
    irrigation_yr1 <- round((input$irri_yr1 * input$irrInch_yr1 * 0.226), digits = 1)
  } else{
    irrigation_yr1 <- 0 
  }
  
  precipitation_yr1 <- round((input$precip_RM_yr1 * input$InchPre_RM_yr1 * 0.226), digits = 1) 
  dryDecomp_yr1 <- precipitation_yr1
  
  ##################
  cropSeed_yr1 <- 0
  nonsymbioticFixation_yr1 <- 3
  ##################
  
  inputTotal_yr1 <- input$fert_yr1 + input$man_yr1 + input$leg_yr1 + irrigation_yr1 + precipitation_yr1 + dryDecomp_yr1 + cropSeed_yr1 + nonsymbioticFixation_yr1
  inputTotal_yr1 <- round(inputTotal_yr1, digits = 1)
  
  if (input$irr_yr2 == 1){
    irrigation_yr2 <- round((input$irri_yr2 * input$irrInch_yr2 * 0.226), digits = 1)
  } else{
    irrigation_yr2 <- 0 
  }
  
  precipitation_yr2 <- round((input$precip_RM_yr2 * input$InchPre_RM_yr2 * 0.226), digits = 1) 
  dryDecomp_yr2 <- precipitation_yr2
  
  ##################
  cropSeed_yr2 <- 0
  nonsymbioticFixation_yr2 <- 3
  ##################
  
  inputTotal_yr2 <- input$fert_yr2 + input$man_yr2 + input$leg_yr2 + irrigation_yr2 + precipitation_yr2 + dryDecomp_yr2 + cropSeed_yr2 + nonsymbioticFixation_yr2
  inputTotal_yr2 <- round(inputTotal_yr2, digits = 1)
  
  if (input$irr_yr3 == 1){
    irrigation_yr3 <- round((input$irri_yr3 * input$irrInch_yr3 * 0.226), digits = 1)
  } else{
    irrigation_yr3 <- 0 
  }
  
  precipitation_yr3 <- round((input$precip_RM_yr3 * input$InchPre_RM_yr3 * 0.226), digits = 1) 
  dryDecomp_yr3 <- precipitation_yr3
  
  ##################
  cropSeed_yr3 <- 0
  nonsymbioticFixation_yr3 <- 3
  ##################
  
  inputTotal_yr3 <- input$fert_yr3 + input$man_yr3 + input$leg_yr3 + irrigation_yr3 + precipitation_yr3 + dryDecomp_yr3 + cropSeed_yr3 + nonsymbioticFixation_yr3
  inputTotal_yr3 <- round(inputTotal_yr3, digits = 1)
  
  if (input$irr_yr4 == 1){
    irrigation_yr4 <- round((input$irr_yr4 * input$irrInch_yr4 * 0.226), digits = 1)
  } else{
    irrigation_yr4 <- 0 
  }
  
  precipitation_yr4 <- round((input$precip_RM_yr4 * input$InchPre_RM_yr4 * 0.226), digits = 1) 
  dryDecomp_yr4 <- precipitation_yr4
  
  ##################
  cropSeed_yr4 <- 0
  nonsymbioticFixation_yr4 <- 3
  ##################
  
  inputTotal_yr4 <- input$fert_yr4 + input$man_yr4 + input$leg_yr4 + irrigation_yr4 + precipitation_yr4 + dryDecomp_yr4 + cropSeed_yr4 + nonsymbioticFixation_yr4
  inputTotal_yr4 <- round(inputTotal_yr4, digits = 1)
  
  cop_info <- matrix(c("Year 1", 0, input$fert_yr1, input$man_yr1, input$leg_yr1, irrigation_yr1, precipitation_yr1, dryDecomp_yr1, cropSeed_yr1, nonsymbioticFixation_yr1, inputTotal_yr1,
                       "Year 2", 0, input$fert_yr2, input$man_yr2, input$leg_yr2, irrigation_yr2, precipitation_yr2, dryDecomp_yr2, cropSeed_yr2, nonsymbioticFixation_yr2, inputTotal_yr2,
                       "Year 3", 0, input$fert_yr3, input$man_yr3, input$leg_yr3, irrigation_yr3, precipitation_yr3, dryDecomp_yr3, cropSeed_yr3, nonsymbioticFixation_yr3, inputTotal_yr3,
                       "Year 4", 0, input$fert_yr4, input$man_yr4, input$leg_yr4, irrigation_yr4, precipitation_yr4, dryDecomp_yr4, cropSeed_yr4, nonsymbioticFixation_yr4, inputTotal_yr4),
    
                       # "Year 1", 0, input$fert_yr1, input$man_yr1, input$leg_yr1, irrigation_yr1, precipitation_yr1, dryDecomp_yr1, cropSeed_yr1, nonsymbioticFixation_yr1, inputTotal_yr1,
                       # "Year 2", 0, input$fert_yr2, input$man_yr2, input$leg_yr2, irrigation_yr2, precipitation_yr2, dryDecomp_yr2, cropSeed_yr2, nonsymbioticFixation_yr2, inputTotal_yr2,
                       # "Year 3", 0, input$fert_yr3, input$man_yr3, input$leg_yr3, irrigation_yr3, precipitation_yr3, dryDecomp_yr3, cropSeed_yr3, nonsymbioticFixation_yr3, inputTotal_yr3,
                       # "Year 4", 0, input$fert_yr4, input$man_yr4, input$leg_yr4, irrigation_yr4, precipitation_yr4, dryDecomp_yr4, cropSeed_yr4, nonsymbioticFixation_yr4, inputTotal_yr4), 
                     nrow = 11, ncol = 4, 
                     dimnames = list(c("", "Carry Over", "Fertilizer", "Manure", "Symbiotic N fixation (legumes)", "Irrigation", "Precipitation", "Dry Deposition", "Crop Seed", "Nonsymbiotic Fixation", "Total N Input Per Acre")))
  rhandsontable(cop_info, 
                rowHeaderWidth = 240, 
                readOnly = TRUE)
})

output$rhot3_RM <- renderRHandsontable({
  
  #####
   # `irr` variables represent a yes or no question.  0 - No Irrigation.  1 - Yes Irrigation.
   # `irri` variables represent how much irrigation
  
  
  df_N_unit1 <- crops %>%
    filter(Crop.Type == input$crop_yr1 & Category == input$unitsYr1) %>%
    select(N.Per.Unit)
  
  N_unit_yr1 <- df_N_unit1[1,1]
  N_unit_yr1 <- round(N_unit_yr1, digits = 1)
  
  harvMat_yr1 <- N_unit_yr1 * input$yield_yr1

  df_perLoss_yr1 <- fert %>%
    filter(Fertilizer.N.Source == input$fertform_yr1 & Application.Method == input$appMeth_yr1) %>%
    select(Percent.Lost)

  perLoss_yr1 <- df_perLoss_yr1[1,1]

  perLoss_yr1 <- round(perLoss_yr1, digits = 1)




  ammLoss_yr1 <- input$fert_yr1 * (perLoss_yr1/100)
  ammLoss_yr1 <- round(ammLoss_yr1, digits = 1)

  if (input$irr_yr1 == 1){
    irrigation_yr1 <- input$irri_yr1 * input$irrInch_yr1 * 0.226
  } else{
    irrigation_yr1 <- 0
  }

  cropSeed_yr1 <- 0

  nonsymbioticFixation_yr1 <- 3

  precipitation_yr1 <- input$precip_RM_yr1 * input$InchPre_RM_yr1 * 0.226

  inputTotal_yr1 <- input$fert_yr1 + input$man_yr1 + input$leg_yr1 + irrigation_yr1 + precipitation_yr1 + precipitation_yr1 + cropSeed_yr1 + nonsymbioticFixation_yr1
  inputTotal_yr1 <- round(inputTotal_yr1, digits = 1)

  df_perInoDent_yr1 <- soil %>%
    filter(Organic.Matter.Content == input$soilType_RM & Soil.Drainage.Classification == input$drainClass_RM) %>%
    select(Percent.of.inorganic.denitrified)

  perInoDent_yr1 <- df_perInoDent_yr1[1,1]

  dent_yr1 <- (input$fert_yr1 + precipitation_yr1 + precipitation_yr1 + irrigation_yr1) * (perInoDent_yr1 / 100)
  dent_yr1 <- round(dent_yr1, digits = 1)

  erosion_yr1 <- 0

  runoff_yr1 <- 0

  misgas_yr1 <- inputTotal_yr1 * 0.01
  misgas_yr1 <- round(misgas_yr1, digits = 1)

  ammAtSen_yr1 <- 0

  totalOutputs_yr1 <- harvMat_yr1 + ammLoss_yr1 + dent_yr1 + erosion_yr1 + runoff_yr1 + misgas_yr1 + ammAtSen_yr1
  totalOutputs_yr1 <- round(totalOutputs_yr1, digits = 1)

  df_N_unit2 <- crops %>%
    filter(Crop.Type == input$crop_yr2 & Category == input$unitsYr2) %>%
    select(N.Per.Unit)

  N_unit_yr2 <- df_N_unit2[1,1]
  N_unit_yr2 <- round(N_unit_yr2, digits = 1)

  harvMat_yr2 <- N_unit_yr2 * input$yield_yr2

  df_perLoss_yr2 <- fert %>%
    filter(Fertilizer.N.Source == input$fertform_yr2 & Application.Method == input$appMeth_yr2) %>%
    select(Percent.Lost)

  perLoss_yr2 <- df_perLoss_yr2[1,1]

  perLoss_yr2 <- round(perLoss_yr2, digits = 1)



  ammLoss_yr2 <- input$fert_yr2 * (perLoss_yr2/100)
  ammLoss_yr2 <- round(ammLoss_yr2, digits = 1)

  if (input$irr_yr2 == 1){
    irrigation_yr2 <- input$irri_yr2 * input$irrInch_yr2 * 0.226
  } else{
    irrigation_yr2 <- 0
  }

  cropSeed_yr2 <- 0

  nonsymbioticFixation_yr2 <- 3

  precipitation_yr2 <- input$precip_RM_yr2 * input$InchPre_RM_yr2 * 0.226

  inputTotal_yr2 <- input$fert_yr2 + input$man_yr2 + input$leg_yr2 + irrigation_yr2 + precipitation_yr2 + precipitation_yr2 + cropSeed_yr2 + nonsymbioticFixation_yr2
  inputTotal_yr2 <- round(inputTotal_yr2, digits = 1)

  df_perInoDent_yr2 <- soil %>%
    filter(Organic.Matter.Content == input$soilType_RM & Soil.Drainage.Classification == input$drainClass_RM) %>%
    select(Percent.of.inorganic.denitrified)

  perInoDent_yr2 <- df_perInoDent_yr2[1,1]

  dent_yr2 <- (input$fert_yr2 + precipitation_yr2 + precipitation_yr2 + irrigation_yr2) * (perInoDent_yr2 / 100)
  dent_yr2 <- round(dent_yr2, digits = 1)

  erosion_yr2 <- 0

  runoff_yr2 <- 0

  misgas_yr2 <- inputTotal_yr2 * 0.01
  misgas_yr2 <- round(misgas_yr2, digits = 1)

  ammAtSen_yr2 <- 0

  totalOutputs_yr2 <- harvMat_yr2 + ammLoss_yr2 + dent_yr2 + erosion_yr2 + runoff_yr2 + misgas_yr2 + ammAtSen_yr2
  totalOutputs_yr2 <- round(totalOutputs_yr2, digits = 1)

  
  df_N_unit3 <- crops %>%
    filter(Crop.Type == input$crop_yr3 & Category == input$unitsYr3) %>%
    select(N.Per.Unit)
  
  N_unit_yr3 <- df_N_unit3[1,1]
  N_unit_yr3 <- round(N_unit_yr3, digits = 1)

  harvMat_yr3 <- N_unit_yr3 * input$yield_yr3

  df_perLoss_yr3 <- fert %>%
    filter(Fertilizer.N.Source == input$fertform_yr3 & Application.Method == input$appMeth_yr3) %>%
    select(Percent.Lost)

  perLoss_yr3 <- df_perLoss_yr3[1,1]

  perLoss_yr3 <- round(perLoss_yr3, digits = 1)


  ammLoss_yr3 <- input$fert_yr3 * (perLoss_yr3/100)
  ammLoss_yr3 <- round(ammLoss_yr3, digits = 1)

  if (input$irr_yr3 == 1){
    irrigation_yr3 <- input$irri_yr3 * input$irrInch_yr3 * 0.226
  } else{
    irrigation_yr3 <- 0
  }

  cropSeed_yr3 <- 0

  nonsymbioticFixation_yr3 <- 3

  precipitation_yr3 <- input$precip_RM_yr3 * input$InchPre_RM_yr3 * 0.226

  inputTotal_yr3 <- input$fert_yr3 + input$man_yr3 + input$leg_yr3 + irrigation_yr3 + precipitation_yr3 + precipitation_yr3 + cropSeed_yr3 + nonsymbioticFixation_yr3
  inputTotal_yr3 <- round(inputTotal_yr3, digits = 1)

  df_perInoDent_yr3 <- soil %>%
    filter(Organic.Matter.Content == input$soilType_RM & Soil.Drainage.Classification == input$drainClass_RM) %>%
    select(Percent.of.inorganic.denitrified)

  perInoDent_yr3 <- df_perInoDent_yr3[1,1]

  dent_yr3 <- (input$fert_yr3 + precipitation_yr3 + precipitation_yr3 + irrigation_yr3) * (perInoDent_yr3 / 100)
  dent_yr3 <- round(dent_yr3, digits = 1)

  erosion_yr3 <- 0

  runoff_yr3 <- 0

  misgas_yr3 <- inputTotal_yr3 * 0.01
  misgas_yr3 <- round(misgas_yr3, digits = 1)

  ammAtSen_yr3 <- 0

  totalOutputs_yr3 <- harvMat_yr3 + ammLoss_yr3 + dent_yr3 + erosion_yr3 + runoff_yr3 + misgas_yr3 + ammAtSen_yr3
  totalOutputs_yr3 <- round(totalOutputs_yr3, digits = 1)
  

  df_N_unit4 <- crops %>%
    filter(Crop.Type == input$crop_yr4 & Category == input$unitsYr4) %>%
    select(N.Per.Unit)

  N_unit_yr4 <- df_N_unit4[1,1]
  N_unit_yr4 <- round(N_unit_yr4, digits = 1)

  harvMat_yr4 <- N_unit_yr4 * input$yield_yr4

  df_perLoss_yr4 <- fert %>%
    filter(Fertilizer.N.Source == input$fertform_yr4 & Application.Method == input$appMeth_yr4) %>%
    select(Percent.Lost)

  perLoss_yr4 <- df_perLoss_yr4[1,1]
  perLoss_yr4 <- round(perLoss_yr4, digits = 1)


  ammLoss_yr4 <- input$fert_yr4 * (perLoss_yr4/100)
  ammLoss_yr4 <- round(ammLoss_yr4, digits = 1)

  if (input$irr_yr4 == 1){
    irrigation_yr4 <- input$irri_yr4 * input$irrInch_yr4 * 0.226
  } else{
    irrigation_yr4 <- 0
  }

  cropSeed_yr4 <- 0

  nonsymbioticFixation_yr4 <- 3

  precipitation_yr4 <- input$precip_RM_yr4 * input$InchPre_RM_yr4 * 0.226

  inputTotal_yr4 <- input$fert_yr4 + input$man_yr4 + input$leg_yr4 + irrigation_yr4 + precipitation_yr4 + precipitation_yr4 + cropSeed_yr4 + nonsymbioticFixation_yr4
  inputTotal_yr4 <- round(inputTotal_yr4, digits = 1)

  df_perInoDent_yr4 <- soil %>%
    filter(Organic.Matter.Content == input$soilType_RM & Soil.Drainage.Classification == input$drainClass_RM) %>%
    select(Percent.of.inorganic.denitrified)

  perInoDent_yr4 <- df_perInoDent_yr4[1,1]

  dent_yr4 <- (input$fert_yr4 + precipitation_yr4 + precipitation_yr4 + irrigation_yr4) * (perInoDent_yr4 / 100)
  dent_yr4 <- round(dent_yr4, digits = 1)

  erosion_yr4 <- 0

  runoff_yr4 <- 0

  misgas_yr4 <- inputTotal_yr4 * 0.01
  misgas_yr4 <- round(misgas_yr4, digits = 1)

  ammAtSen_yr4 <- 0

  totalOutputs_yr4 <- harvMat_yr4 + ammLoss_yr4 + dent_yr4 + erosion_yr4 + runoff_yr4 + misgas_yr4 + ammAtSen_yr4
  totalOutputs_yr4 <- round(totalOutputs_yr4, digits = 1)
  
  # cop_info <- matrix(c("Year 1", 1, 2, 3, 4, 5, 6, 7, 8,
  #                      "Year 2", 1, 2, 3, 4, 5, 6, 7, 8,
  #                      "Year 3", 1, 2, 3, 4, 5, 6, 7, 8,
  #                      "Year 4", 1, 2, 3, 4, 5, 6, 7, 8),
  #                    nrow = 9, ncol = 4, 
  #                    dimnames = list(c("","Harvested material (Main Crop)", "Ammonia Loss", "Denitrification", "Erosion", "Runoff", "Miscellaneous Gaseous", "Ammonia at Senescence", "Total N Output Per Acre")))
  # rhandsontable(cop_info, 
  #               rowHeaderWidth = 240, 
  #               readOnly = TRUE)
  
  cop_info <- matrix(c("Year 1", harvMat_yr1, ammLoss_yr1, dent_yr1, erosion_yr1, runoff_yr1, misgas_yr1, ammAtSen_yr1, totalOutputs_yr1,
                       "Year 2", harvMat_yr2, ammLoss_yr2, dent_yr2, erosion_yr2, runoff_yr2, misgas_yr2, ammAtSen_yr2, totalOutputs_yr2,
                       "Year 3", harvMat_yr3, ammLoss_yr3, dent_yr3, erosion_yr3, runoff_yr3, misgas_yr3, ammAtSen_yr3, totalOutputs_yr3,
                       "Year 4", harvMat_yr4, ammLoss_yr4, dent_yr4, erosion_yr4, runoff_yr4, misgas_yr4, ammAtSen_yr4, totalOutputs_yr4),
                     nrow = 9, ncol = 4,
                     dimnames = list(c("","Harvested material (Main Crop)", "Ammonia Loss", "Denitrification", "Erosion", "Runoff", "Miscellaneous Gaseous", "Ammonia at Senescence", "Total N Output Per Acre")))
  rhandsontable(cop_info,
                rowHeaderWidth = 240,
                readOnly = TRUE)
})

output$rhot4_RM <- renderRHandsontable({
  
  total_change_N_yr1 <- input$inoN_RM_yr1 + input$orgN_RM_yr1
  total_change_N_yr2 <- input$inoN_RM_yr2 + input$orgN_RM_yr2
  total_change_N_yr3 <- input$inoN_RM_yr3 + input$orgN_RM_yr3
  total_change_N_yr4 <- input$inoN_RM_yr4 + input$orgN_RM_yr4
  
  cop_info <- matrix(c("Year 1", input$inoN_RM_yr1, input$orgN_RM_yr1, total_change_N_yr1,
                       "Year 2", input$inoN_RM_yr2, input$orgN_RM_yr2, total_change_N_yr2,
                       "Year 3", input$inoN_RM_yr3, input$orgN_RM_yr3, total_change_N_yr3,
                       "Year 4", input$inoN_RM_yr4, input$orgN_RM_yr4, total_change_N_yr4), 
                     nrow = 4, ncol = 4, 
                     dimnames = list(c("", "Change in Inorganic N", "Change in Organic N", "Total Storage Change")))
  rhandsontable(cop_info, 
                rowHeaderWidth = 240, 
                readOnly = TRUE)
})

output$rhot5_RM <- renderRHandsontable({
  if(input$cp_yr1 == 0){
    cct_yr1 <- 0
    c_units_yr1 <- 0
    ccy_yr1 <- 0
  } else{
    cct_yr1 <- input$cct_yr1
    df_c_units_yr1 <- crops %>%
      filter(Crop.Type == input$cct_yr1 & Category == input$c_units_yr1) %>%
      select(Avg.N.harvested..lb.N.unit.)
    
    c_units_yr1 <- df_c_units_yr1[1,1]
    
    ccy_yr1 <- input$ccy_yr1
  }
  
  
  if(input$cp_yr1 == 0){
    n_per_unit_yr1 <- 0
  } else{
    df_n_per_unit_yr1 <- crops %>%
      filter(Crop.Type == input$cct_yr1 & Category == input$c_units_yr1) %>%
      select(N.Per.Unit)
    
    n_per_unit_yr1 <- df_n_per_unit_yr1[1,1]
  }
  
  totCovCropRes_yr1 <- ccy_yr1 * n_per_unit_yr1
  
  if(input$cp_yr2 == 0){
    cct_yr2 <- 0
    c_units_yr2 <- 0
    ccy_yr2 <- 0
  } else{
    cct_yr2 <- input$cct_yr2
    df_c_units_yr2 <- crops %>%
      filter(Crop.Type == input$cct_yr2 & Category == input$c_units_yr2) %>%
      select(Avg.N.harvested..lb.N.unit.)
    
    c_units_yr2 <- df_c_units_yr2[1,1]
    
    ccy_yr2 <- input$ccy_yr2
  }
  
  
  if(input$cp_yr2 == 0){
    n_per_unit_yr2 <- 0
  } else{
    df_n_per_unit_yr2 <- crops %>%
      filter(Crop.Type == input$cct_yr2 & Category == input$c_units_yr2) %>%
      select(N.Per.Unit)
    
    n_per_unit_yr2 <- df_n_per_unit_yr2[1,1]
  }
  
  totCovCropRes_yr2 <- ccy_yr2 * n_per_unit_yr2
  
  if(input$cp_yr3 == 0){
    cct_yr3 <- 0
    c_units_yr3 <- 0
    ccy_yr3 <- 0
  } else{
    cct_yr3 <- input$cct_yr3
    df_c_units_yr3 <- crops %>%
      filter(Crop.Type == input$cct_yr3 & Category == input$c_units_yr3) %>%
      select(Avg.N.harvested..lb.N.unit.)
    
    c_units_yr3 <- df_c_units_yr3[1,1]
    
    ccy_yr3 <- input$ccy_yr3
  }
  
  
  if(input$cp_yr3 == 0){
    n_per_unit_yr3 <- 0
  } else{
    df_n_per_unit_yr3 <- crops %>%
      filter(Crop.Type == input$cct_yr3 & Category == input$c_units_yr3) %>%
      select(N.Per.Unit)
    
    n_per_unit_yr3 <- df_n_per_unit_yr3[1,1]
  }
  
  totCovCropRes_yr3 <- ccy_yr3 * n_per_unit_yr3
  
  if(input$cp_yr4 == 0){
    cct_yr4 <- 0
    c_units_yr4 <- 0
    ccy_yr4 <- 0
  } else{
    cct_yr4 <- input$cct_yr4
    df_c_units_yr4 <- crops %>%
      filter(Crop.Type == input$cct_yr4 & Category == input$c_units_yr4) %>%
      select(Avg.N.harvested..lb.N.unit.)
    
    c_units_yr4 <- df_c_units_yr4[1,1]
    
    ccy_yr4 <- input$ccy_yr4
  }
  
  
  if(input$cp_yr4 == 0){
    n_per_unit_yr4 <- 0
  } else{
    df_n_per_unit_yr4 <- crops %>%
      filter(Crop.Type == input$cct_yr4 & Category == input$c_units_yr4) %>%
      select(N.Per.Unit)
    
    n_per_unit_yr4 <- df_n_per_unit_yr4[1,1]
  }
  
  totCovCropRes_yr4 <- ccy_yr4 * n_per_unit_yr4
  
  cop_info <- matrix(c("Year 1", cct_yr1, c_units_yr1, ccy_yr1, totCovCropRes_yr1,
                       "Year 2", cct_yr2, c_units_yr2, ccy_yr2, totCovCropRes_yr2,
                       "Year 3", cct_yr3, c_units_yr3, ccy_yr3, totCovCropRes_yr3,
                       "Year 4", cct_yr4, c_units_yr4, ccy_yr4, totCovCropRes_yr4), 
                     nrow = 5, ncol = 4, 
                     dimnames = list(c("","Crop Type", "N Content", "Yield", "Total Cover Crop Residue")))
  rhandsontable(cop_info, 
                rowHeaderWidth = 240, 
                readOnly = TRUE)
})

output$Yr1_OP_TLN <- renderUI({
  # Getting Input Value for final calculation
  if (input$irr_yr1 == 1){
    irrigation_yr1 <- input$irri_yr1 * input$irrInch_yr1 * 0.226
  } else{
    irrigation_yr1 <- 0
  }
  precipitation_yr1 <- input$precip_RM_yr1 * input$InchPre_RM_yr1 * 0.226
  cropSeed_yr1 <- 0
  nonsymbioticFixation_yr1 <- 3
  
  # Inputs
  inputTotal_yr1 <- input$fert_yr1 + input$man_yr1 + input$leg_yr1 + irrigation_yr1 + precipitation_yr1 + precipitation_yr1 + cropSeed_yr1  + nonsymbioticFixation_yr1
  
  df_N_unit_yr1 <- crops %>%
    filter(Crop.Type == input$crop_yr1 & Category == input$unitsYr1) %>%
    select(N.Per.Unit)
  
  N_unit_yr1 <- df_N_unit_yr1[1,1]
  
  harvMat_yr1 <- N_unit_yr1 * input$yield_yr1
  
  perLoss_yr1 <- fert %>%
    filter(Fertilizer.N.Source == input$fertform_yr1 & Application.Method == input$appMeth_yr1) %>%
    select(Percent.Lost)
  
  ammLoss_yr1 <- input$fert_yr1 * (perLoss_yr1/100)
  
  if (input$irr_yr1 == 1){
    irrigation_yr1 <- input$irri_yr1 * input$irrInch_yr1 * 0.226
  } else{
    irrigation_yr1 <- 0
  }
  df_perInoDent_yr1 <- soil %>%
    filter(Organic.Matter.Content == input$soilType_RM & Soil.Drainage.Classification == input$drainClass_RM) %>%
    select(Percent.of.inorganic.denitrified)
  
  perInoDent_yr1 <- df_perInoDent_yr1[1,1]
  
  dent_yr1 <- (input$fert_yr1 + precipitation_yr1 + irrigation_yr1) * (perInoDent_yr1 / 100)
  erosion_yr1 <- 0
  runoff_yr1 <- 0
  misgas_yr1 <- inputTotal_yr1 * 0.01
  ammAtSen_yr1 <- 0
  
  # Outputs
  totalOutputs_yr1 <- harvMat_yr1 + ammLoss_yr1 + dent_yr1 + erosion_yr1 + runoff_yr1 + misgas_yr1 + ammAtSen_yr1
  
  
  
  # Total Storage Change
  total_change_N_yr1 <- input$inoN_RM_yr1 + input$orgN_RM_yr1

  # Cover Crop Residue
  if(input$cp_yr1 == 0){
    cct_yr1 <- 0
    c_units_yr1 <- 0
    ccy_yr1 <- 0
  } else{
    cct_yr1 <- input$cct_yr1
    c_units_yr1 <- crops %>%
      filter(Crop.Type == input$cct_yr1 & Category == input$c_units_yr1) %>%
      select(Avg.N.harvested..lb.N.unit.)
    ccy_yr1 <- input$ccy_yr1
  }

  if(input$cp_yr1 == 0){
    n_per_unit_yr1 <- 0
  } else{
    df_n_per_unit_yr1 <- crops %>%
      filter(Crop.Type == input$cct_yr1 & Category == input$c_units_yr1) %>%
      select(N.Per.Unit)

    n_per_unit_yr1 <- df_n_per_unit_yr1[1,1]
  }
  totCovCropRes_yr1 <- ccy_yr1 * n_per_unit_yr1


  # totalInputs - TotalOutputs - Total Storage Change - Total Cover Crop Residue
  OP1_yr1 <- inputTotal_yr1 - totalOutputs_yr1 - total_change_N_yr1 - totCovCropRes_yr1
  OP1_yr1 <- round(OP1_yr1, digits = 1)
  OP1_yr1 <- as.character(OP1_yr1)
  paste(OP1_yr1)
})

output$Yr1_OP_FWN <- renderUI({
    # Getting Input Value for final calulation
    if (input$irr_yr1 == 1){
      irrigation_yr1 <- input$irri_yr1 * input$irrInch_yr1 * 0.226
    } else{
      irrigation_yr1 <- 0 
    }
  
    precipitation_yr1 <- input$precip_RM_yr1 * input$InchPre_RM_yr1 * 0.226
    
    cropSeed_yr1 <- 0
    nonsymbioticFixation_yr1 <- 3
    # Inputs
    inputTotal_yr1 <- input$fert_yr1 + input$man_yr1 + input$leg_yr1 + irrigation_yr1 + precipitation_yr1 + precipitation_yr1 + cropSeed_yr1 + nonsymbioticFixation_yr1
    

  #   
    df_N_unit_yr1 <- crops %>%
      filter(Crop.Type == input$crop_yr1 & Category == input$unitsYr1) %>%
      select(N.Per.Unit)

    N_unit_yr1 <- df_N_unit_yr1[1,1]

    harvMat_yr1 <- N_unit_yr1 * input$yield_yr1

  
    df_perLoss_yr1 <- fert %>%
      filter(Fertilizer.N.Source == input$fertform_yr1 & Application.Method == input$appMeth_yr1) %>%
      select(Percent.Lost)

    perLoss_yr1 <- df_perLoss_yr1[1,1]

    ammLoss_yr1 <- input$fert_yr1 * (perLoss_yr1/100)


    df_perInoDent_yr1 <- soil %>%
      filter(Organic.Matter.Content == input$soilType & Soil.Drainage.Classification == input$drainClass) %>%
      select(Percent.of.inorganic.denitrified)

    perInoDent_yr1 <- df_perInoDent_yr1[1,1]

    dent_yr1 <- (input$fert_yr1 + precipitation_yr1 + irrigation_yr1) * (perInoDent_yr1 / 100)

    erosion_yr1 <- 0
    runoff_yr1 <- 0
    misgas_yr1 <- inputTotal_yr1 * 0.01
    ammAtSen_yr1 <- 0
    # Outputs
    totalOutputs_yr1 <- harvMat_yr1 + ammLoss_yr1 + dent_yr1 + erosion_yr1 + runoff_yr1 + misgas_yr1 + ammAtSen_yr1

    
    # Total Storage Change
    total_change_N_yr1 <- input$inoN_RM_yr1 + input$orgN_RM_yr1

    # Cover Crop Residue
    if(input$cp_yr1 == 0){
      cct_yr1 <- 0
      ccy_yr1 <- 0
    } else{
      cct_yr1 <- input$cct_yr1
      df_c_units_yr1 <- crops %>%
        filter(Crop.Type == input$cct_yr1 & Category == input$c_units_yr1) %>%
        select(Avg.N.harvested..lb.N.unit.)

      c_units_yr1 <- df_c_units_yr1[1,1]

      ccy_yr1 <- input$ccy_yr1
    }

    if(input$cp_yr1 == 0){
      n_per_unit_yr1 <- 0
    } else{
      df_n_per_unit_yr1 <- crops %>%
        filter(Crop.Type == input$cct_yr1 & Category == input$c_units_yr1) %>%
        select(N.Per.Unit)

      n_per_unit_yr1 <- df_n_per_unit_yr1[1,1]

    }
    totCovCropRes_yr1 <- ccy_yr1 * n_per_unit_yr1


    # total Inputs - Total Outputs - Total Storage Change - Total Cover Crop Residue
    LeachableN_yr1 <- inputTotal_yr1 - totalOutputs_yr1 - total_change_N_yr1 - totCovCropRes_yr1


    
  df_rechargeEstimate_yr1 <- soil %>%
    filter(Organic.Matter.Content == input$soilType & Soil.Drainage.Classification == input$drainClass) %>%
    select(Inches.Recharge)

  rechargeEstimate_yr1 <- df_rechargeEstimate_yr1[1,1]

  Nitrate_N_Estimate_yr1 <- LeachableN_yr1 / rechargeEstimate_yr1 / 0.226


  OP4_yr1 <- round(Nitrate_N_Estimate_yr1, digits = 1)
  OP4_yr1 <- as.character(OP4_yr1)


  if (length(OP4_yr1) == 0){
    paste("NaN")
  }else{
    paste(OP4_yr1)
  }
})

output$Yr2_OP_TLN <- renderUI({
  # Getting Input Value for final calculation
  if (input$irr_yr2 == 1){
    irrigation_yr2 <- input$irri_yr2 * input$irrInch_yr2 * 0.226
  } else{
    irrigation_yr2 <- 0
  }
  precipitation_yr2 <- input$precip_RM_yr2 * input$InchPre_RM_yr2 * 0.226
  cropSeed_yr2 <- 0
  nonsymbioticFixation_yr2 <- 3
  
  # Inputs
  inputTotal_yr2 <- input$fert_yr2 + input$man_yr2 + input$leg_yr2 + irrigation_yr2 + precipitation_yr2 + precipitation_yr2 + cropSeed_yr2  + nonsymbioticFixation_yr2
  
  df_N_unit_yr2 <- crops %>%
    filter(Crop.Type == input$crop_yr2 & Category == input$unitsYr2) %>%
    select(N.Per.Unit)
  
  N_unit_yr2 <- df_N_unit_yr2[1,1]
  
  harvMat_yr2 <- N_unit_yr2 * input$yield_yr2
  
  perLoss_yr2 <- fert %>%
    filter(Fertilizer.N.Source == input$fertform_yr2 & Application.Method == input$appMeth_yr2) %>%
    select(Percent.Lost)
  
  ammLoss_yr2 <- input$fert_yr2 * (perLoss_yr2/100)
  
  if (input$irr_yr2 == 1){
    irrigation_yr2 <- input$irri_yr2 * input$irrInch_yr2 * 0.226
  } else{
    irrigation_yr2 <- 0
  }
  perInoDent_yr2 <- soil %>%
    filter(Organic.Matter.Content == input$soilType_RM & Soil.Drainage.Classification == input$drainClass_RM) %>%
    select(Percent.of.inorganic.denitrified)
  
  dent_yr2 <- (input$fert_yr2 + precipitation_yr2 + irrigation_yr2) * (perInoDent_yr2 / 100)
  erosion_yr2 <- 0
  runoff_yr2 <- 0
  misgas_yr2 <- inputTotal_yr2 * 0.01
  ammAtSen_yr2 <- 0
  
  # Outputs
  totalOutputs_yr2 <- harvMat_yr2 + ammLoss_yr2 + dent_yr2 + erosion_yr2 + runoff_yr2 + misgas_yr2 + ammAtSen_yr2
  
  # Total Storage Change
  total_change_N_yr2 <- input$inoN_RM_yr2 + input$orgN_RM_yr2
  
  # Cover Crop Residue
  if(input$cp_yr2 == 0){
    cct_yr2 <- 0
    c_units_yr2 <- 0
    ccy_yr2 <- 0
  } else{
    cct_yr2 <- input$cct_yr2
    c_units_yr2 <- crops %>%
      filter(Crop.Type == input$cct_yr2 & Category == input$c_units_yr2) %>%
      select(Avg.N.harvested..lb.N.unit.)
    ccy_yr2 <- input$ccy_yr2
  }
  
  if(c_units_yr2 == 0){
    n_per_unit_yr2 <- 0
  } else{
    df_n_per_unit_yr2 <- crops %>%
      filter(Crop.Type == input$cct_yr2 & Category == input$c_units_yr2) %>%
      select(N.Per.Unit)
    
    n_per_unit_yr2 <- df_n_per_unit_yr2[1,1]
  }
  totCovCropRes_yr2 <- ccy_yr2 * n_per_unit_yr2
  
  
  # totalInputs - TotalOutputs - Total Storage Change - Total Cover Crop Residue
  OP1_yr2 <- inputTotal_yr2 - totalOutputs_yr2 - total_change_N_yr2 - totCovCropRes_yr2
  OP1_yr2 <- round(OP1_yr2, digits = 1)
  OP1_yr2 <- as.character(OP1_yr2)
  paste(OP1_yr2)
})

output$Yr2_OP_FWN <- renderUI({
  # Getting Input Value for final calulation
  if (input$irr_yr2 == 1){
    irrigation_yr2 <- input$irri_yr2 * input$irrInch_yr2 * 0.226
  } else{
    irrigation_yr2 <- 0 
  }
  
  precipitation_yr2 <- input$precip_RM_yr2 * input$InchPre_RM_yr2 * 0.226
  
  cropSeed_yr2 <- 0
  nonsymbioticFixation_yr2 <- 3
  # Inputs
  inputTotal_yr2 <- input$fert_yr2 + input$man_yr2 + input$leg_yr2 + irrigation_yr2 + precipitation_yr2 + precipitation_yr2 + cropSeed_yr2 + nonsymbioticFixation_yr2
  
  
  #   
  df_N_unit_yr2 <- crops %>%
    filter(Crop.Type == input$crop_yr2 & Category == input$unitsYr2) %>%
    select(N.Per.Unit)
  
  N_unit_yr2 <- df_N_unit_yr2[1,1]
  
  harvMat_yr2 <- N_unit_yr2 * input$yield_yr2
  
  
  df_perLoss_yr2 <- fert %>%
    filter(Fertilizer.N.Source == input$fertform_yr2 & Application.Method == input$appMeth_yr2) %>%
    select(Percent.Lost)
  
  perLoss_yr2 <- df_perLoss_yr2[1,1]
  
  ammLoss_yr2 <- input$fert_yr2 * (perLoss_yr2/100)
  
  
  df_perInoDent_yr2 <- soil %>%
    filter(Organic.Matter.Content == input$soilType & Soil.Drainage.Classification == input$drainClass) %>%
    select(Percent.of.inorganic.denitrified)
  
  perInoDent_yr2 <- df_perInoDent_yr2[1,1]
  
  dent_yr2 <- (input$fert_yr2 + precipitation_yr2 + irrigation_yr2) * (perInoDent_yr2 / 100)
  
  erosion_yr2 <- 0
  runoff_yr2 <- 0
  misgas_yr2 <- inputTotal_yr2 * 0.01
  ammAtSen_yr2 <- 0
  # Outputs
  totalOutputs_yr2 <- harvMat_yr2 + ammLoss_yr2 + dent_yr2 + erosion_yr2 + runoff_yr2 + misgas_yr2 + ammAtSen_yr2
  
  
  # Total Storage Change
  total_change_N_yr2 <- input$inoN_RM_yr2 + input$orgN_RM_yr2
  
  # Cover Crop Residue
  if(input$cp_yr2 == 0){
    cct_yr2 <- 0
    ccy_yr2 <- 0
  } else{
    cct_yr2 <- input$cct_yr2
    df_c_units_yr2 <- crops %>%
      filter(Crop.Type == input$cct_yr2 & Category == input$c_units_yr2) %>%
      select(Avg.N.harvested..lb.N.unit.)
    
    c_units_yr2 <- df_c_units_yr2[1,1]
    
    ccy_yr2 <- input$ccy_yr2
  }
  
  if(input$cp_yr2 == 0){
    n_per_unit_yr2 <- 0
  } else{
    df_n_per_unit_yr2 <- crops %>%
      filter(Crop.Type == input$cct_yr2 & Category == input$c_units_yr2) %>%
      select(N.Per.Unit)
    
    n_per_unit_yr2 <- df_n_per_unit_yr2[1,1]
    
  }
  totCovCropRes_yr2 <- ccy_yr2 * n_per_unit_yr2
  
  
  # total Inputs - Total Outputs - Total Storage Change - Total Cover Crop Residue
  LeachableN_yr2 <- inputTotal_yr2 - totalOutputs_yr2 - total_change_N_yr2 - totCovCropRes_yr2
  
  
  
  df_rechargeEstimate_yr2 <- soil %>%
    filter(Organic.Matter.Content == input$soilType & Soil.Drainage.Classification == input$drainClass) %>%
    select(Inches.Recharge)
  
  rechargeEstimate_yr2 <- df_rechargeEstimate_yr2[1,1]
  
  Nitrate_N_Estimate_yr2 <- LeachableN_yr2 / rechargeEstimate_yr2 / 0.226
  
  
  OP4_yr2 <- round(Nitrate_N_Estimate_yr2, digits = 1)
  OP4_yr2 <- as.character(OP4_yr2)
  
  
  if (length(OP4_yr2) == 0){
    paste("NaN")
  }else{
    paste(OP4_yr2)
  }
})

output$Yr3_OP_TLN <- renderUI({
  # Getting Input Value for final calculation
  if (input$irr_yr3 == 1){
    irrigation_yr3 <- input$irri_yr3 * input$irrInch_yr3 * 0.226
  } else{
    irrigation_yr3 <- 0
  }
  precipitation_yr3 <- input$precip_RM_yr3 * input$InchPre_RM_yr3 * 0.226
  cropSeed_yr3 <- 0
  nonsymbioticFixation_yr3 <- 3
  
  # Inputs
  inputTotal_yr3 <- input$fert_yr3 + input$man_yr3 + input$leg_yr3 + irrigation_yr3 + precipitation_yr3 + precipitation_yr3 + cropSeed_yr3  + nonsymbioticFixation_yr3
  
  df_N_unit_yr3 <- crops %>%
    filter(Crop.Type == input$crop_yr3 & Category == input$unitsYr3) %>%
    select(N.Per.Unit)
  
  N_unit_yr3 <- df_N_unit_yr3[1,1]
  
  harvMat_yr3 <- N_unit_yr3 * input$yield_yr3
  
  perLoss_yr3 <- fert %>%
    filter(Fertilizer.N.Source == input$fertform_yr3 & Application.Method == input$appMeth_yr3) %>%
    select(Percent.Lost)
  
  ammLoss_yr3 <- input$fert_yr3 * (perLoss_yr3/100)
  
  if (input$irr_yr3 == 1){
    irrigation_yr3 <- input$irri_yr3 * input$irrInch_yr3 * 0.226
  } else{
    irrigation_yr3 <- 0
  }
  perInoDent_yr3 <- soil %>%
    filter(Organic.Matter.Content == input$soilType_RM & Soil.Drainage.Classification == input$drainClass_RM) %>%
    select(Percent.of.inorganic.denitrified)
  
  dent_yr3 <- (input$fert_yr3 + precipitation_yr3 + irrigation_yr3) * (perInoDent_yr3 / 100)
  erosion_yr3 <- 0
  runoff_yr3 <- 0
  misgas_yr3 <- inputTotal_yr3 * 0.01
  ammAtSen_yr3 <- 0
  
  # Outputs
  totalOutputs_yr3 <- harvMat_yr3 + ammLoss_yr3 + dent_yr3 + erosion_yr3 + runoff_yr3 + misgas_yr3 + ammAtSen_yr3
  
  # Total Storage Change
  total_change_N_yr3 <- input$inoN_RM_yr3 + input$orgN_RM_yr3
  
  # Cover Crop Residue
  if(input$cp_yr3 == 0){
    cct_yr3 <- 0
    c_units_yr3 <- 0
    ccy_yr3 <- 0
  } else{
    cct_yr3 <- input$cct_yr3
    c_units_yr3 <- crops %>%
      filter(Crop.Type == input$cct_yr3 & Category == input$c_units_yr3) %>%
      select(Avg.N.harvested..lb.N.unit.)
    ccy_yr3 <- input$ccy_yr3
  }
  
  if(c_units_yr3 == 0){
    n_per_unit_yr3 <- 0
  } else{
    df_n_per_unit_yr3 <- crops %>%
      filter(Crop.Type == input$cct_yr3 & Category == input$c_units_yr3) %>%
      select(N.Per.Unit)
    
    n_per_unit_yr3 <- df_n_per_unit_yr3[1,1]
  }
  totCovCropRes_yr3 <- ccy_yr3 * n_per_unit_yr3
  
  
  # totalInputs - TotalOutputs - Total Storage Change - Total Cover Crop Residue
  OP1_yr3 <- inputTotal_yr3 - totalOutputs_yr3 - total_change_N_yr3 - totCovCropRes_yr3
  OP1_yr3 <- round(OP1_yr3, digits = 1)
  OP1_yr3 <- as.character(OP1_yr3)
  paste(OP1_yr3)
})

output$Yr3_OP_FWN <- renderUI({
  # Getting Input Value for final calulation
  if (input$irr_yr3 == 1){
    irrigation_yr3 <- input$irri_yr3 * input$irrInch_yr3 * 0.226
  } else{
    irrigation_yr3 <- 0 
  }
  
  precipitation_yr3 <- input$precip_RM_yr3 * input$InchPre_RM_yr3 * 0.226
  
  cropSeed_yr3 <- 0
  nonsymbioticFixation_yr3 <- 3
  # Inputs
  inputTotal_yr3 <- input$fert_yr3 + input$man_yr3 + input$leg_yr3 + irrigation_yr3 + precipitation_yr3 + precipitation_yr3 + cropSeed_yr3 + nonsymbioticFixation_yr3
  
  
  #   
  df_N_unit_yr3 <- crops %>%
    filter(Crop.Type == input$crop_yr3 & Category == input$unitsYr3) %>%
    select(N.Per.Unit)
  
  N_unit_yr3 <- df_N_unit_yr3[1,1]
  
  harvMat_yr3 <- N_unit_yr3 * input$yield_yr3
  
  
  df_perLoss_yr3 <- fert %>%
    filter(Fertilizer.N.Source == input$fertform_yr3 & Application.Method == input$appMeth_yr3) %>%
    select(Percent.Lost)
  
  perLoss_yr3 <- df_perLoss_yr3[1,1]
  
  ammLoss_yr3 <- input$fert_yr3 * (perLoss_yr3/100)
  
  
  df_perInoDent_yr3 <- soil %>%
    filter(Organic.Matter.Content == input$soilType & Soil.Drainage.Classification == input$drainClass) %>%
    select(Percent.of.inorganic.denitrified)
  
  perInoDent_yr3 <- df_perInoDent_yr3[1,1]
  
  dent_yr3 <- (input$fert_yr3 + precipitation_yr3 + irrigation_yr3) * (perInoDent_yr3 / 100)
  
  erosion_yr3 <- 0
  runoff_yr3 <- 0
  misgas_yr3 <- inputTotal_yr3 * 0.01
  ammAtSen_yr3 <- 0
  # Outputs
  totalOutputs_yr3 <- harvMat_yr3 + ammLoss_yr3 + dent_yr3 + erosion_yr3 + runoff_yr3 + misgas_yr3 + ammAtSen_yr3
  
  
  # Total Storage Change
  total_change_N_yr3 <- input$inoN_RM_yr3 + input$orgN_RM_yr3
  
  # Cover Crop Residue
  if(input$cp_yr3 == 0){
    cct_yr3 <- 0
    ccy_yr3 <- 0
  } else{
    cct_yr3 <- input$cct_yr3
    df_c_units_yr3 <- crops %>%
      filter(Crop.Type == input$cct_yr3 & Category == input$c_units_yr3) %>%
      select(Avg.N.harvested..lb.N.unit.)
    
    c_units_yr3 <- df_c_units_yr3[1,1]
    
    ccy_yr3 <- input$ccy_yr3
  }
  
  if(input$cp_yr3 == 0){
    n_per_unit_yr3 <- 0
  } else{
    df_n_per_unit_yr3 <- crops %>%
      filter(Crop.Type == input$cct_yr3 & Category == input$c_units_yr3) %>%
      select(N.Per.Unit)
    
    n_per_unit_yr3 <- df_n_per_unit_yr3[1,1]
    
  }
  totCovCropRes_yr3 <- ccy_yr3 * n_per_unit_yr3
  
  
  # total Inputs - Total Outputs - Total Storage Change - Total Cover Crop Residue
  LeachableN_yr3 <- inputTotal_yr3 - totalOutputs_yr3 - total_change_N_yr3 - totCovCropRes_yr3
  
  
  
  df_rechargeEstimate_yr3 <- soil %>%
    filter(Organic.Matter.Content == input$soilType & Soil.Drainage.Classification == input$drainClass) %>%
    select(Inches.Recharge)
  
  rechargeEstimate_yr3 <- df_rechargeEstimate_yr3[1,1]
  
  Nitrate_N_Estimate_yr3 <- LeachableN_yr3 / rechargeEstimate_yr3 / 0.226
  
  
  OP4_yr3 <- round(Nitrate_N_Estimate_yr3, digits = 1)
  OP4_yr3 <- as.character(OP4_yr3)
  
  
  if (length(OP4_yr3) == 0){
    paste("NaN")
  }else{
    paste(OP4_yr3)
  }
})

output$Yr4_OP_TLN <- renderUI({
  # Getting Input Value for final calculation
  if (input$irr_yr4 == 1){
    irrigation_yr4 <- input$irri_yr4 * input$irrInch_yr4 * 0.226
  } else{
    irrigation_yr4 <- 0
  }
  precipitation_yr4 <- input$precip_RM_yr4 * input$InchPre_RM_yr4 * 0.226
  cropSeed_yr4 <- 0
  nonsymbioticFixation_yr4 <- 3
  
  # Inputs
  inputTotal_yr4 <- input$fert_yr4 + input$man_yr4 + input$leg_yr4 + irrigation_yr4 + precipitation_yr4 + precipitation_yr4 + cropSeed_yr4  + nonsymbioticFixation_yr4
  
  df_N_unit_yr4 <- crops %>%
    filter(Crop.Type == input$crop_yr4 & Category == input$unitsYr4) %>%
    select(N.Per.Unit)
  
  N_unit_yr4 <- df_N_unit_yr4[1,1]
  
  harvMat_yr4 <- N_unit_yr4 * input$yield_yr4
  
  perLoss_yr4 <- fert %>%
    filter(Fertilizer.N.Source == input$fertform_yr4 & Application.Method == input$appMeth_yr4) %>%
    select(Percent.Lost)
  
  ammLoss_yr4 <- input$fert_yr4 * (perLoss_yr4/100)
  
  if (input$irr_yr4 == 1){
    irrigation_yr4 <- input$irri_yr4 * input$irrInch_yr4 * 0.226
  } else{
    irrigation_yr4 <- 0
  }
  perInoDent_yr4 <- soil %>%
    filter(Organic.Matter.Content == input$soilType_RM & Soil.Drainage.Classification == input$drainClass_RM) %>%
    select(Percent.of.inorganic.denitrified)
  
  dent_yr4 <- (input$fert_yr4 + precipitation_yr4 + irrigation_yr4) * (perInoDent_yr4 / 100)
  erosion_yr4 <- 0
  runoff_yr4 <- 0
  misgas_yr4 <- inputTotal_yr4 * 0.01
  ammAtSen_yr4 <- 0
  
  # Outputs
  totalOutputs_yr4 <- harvMat_yr4 + ammLoss_yr4 + dent_yr4 + erosion_yr4 + runoff_yr4 + misgas_yr4 + ammAtSen_yr4
  
  # Total Storage Change
  total_change_N_yr4 <- input$inoN_RM_yr4 + input$orgN_RM_yr4
  
  # Cover Crop Residue
  if(input$cp_yr4 == 0){
    cct_yr4 <- 0
    c_units_yr4 <- 0
    ccy_yr4 <- 0
  } else{
    cct_yr4 <- input$cct_yr4
    c_units_yr4 <- crops %>%
      filter(Crop.Type == input$cct_yr4 & Category == input$c_units_yr4) %>%
      select(Avg.N.harvested..lb.N.unit.)
    ccy_yr4 <- input$ccy_yr4
  }
  
  if(c_units_yr4 == 0){
    n_per_unit_yr4 <- 0
  } else{
    df_n_per_unit_yr4 <- crops %>%
      filter(Crop.Type == input$cct_yr4 & Category == input$c_units_yr4) %>%
      select(N.Per.Unit)
    
    n_per_unit_yr4 <- df_n_per_unit_yr4[1,1]
  }
  totCovCropRes_yr4 <- ccy_yr4 * n_per_unit_yr4
  
  
  # totalInputs - TotalOutputs - Total Storage Change - Total Cover Crop Residue
  OP1_yr4 <- inputTotal_yr4 - totalOutputs_yr4 - total_change_N_yr4 - totCovCropRes_yr4
  OP1_yr4 <- round(OP1_yr4, digits = 1)
  OP1_yr4 <- as.character(OP1_yr4)
  paste(OP1_yr4)
})

output$Yr4_OP_FWN <- renderUI({
  # Getting Input Value for final calulation
  if (input$irr_yr4 == 1){
    irrigation_yr4 <- input$irri_yr4 * input$irrInch_yr4 * 0.226
  } else{
    irrigation_yr4 <- 0 
  }
  
  precipitation_yr4 <- input$precip_RM_yr4 * input$InchPre_RM_yr4 * 0.226
  
  cropSeed_yr4 <- 0
  nonsymbioticFixation_yr4 <- 3
  # Inputs
  inputTotal_yr4 <- input$fert_yr4 + input$man_yr4 + input$leg_yr4 + irrigation_yr4 + precipitation_yr4 + precipitation_yr4 + cropSeed_yr4 + nonsymbioticFixation_yr4
  
  
  #   
  df_N_unit_yr4 <- crops %>%
    filter(Crop.Type == input$crop_yr4 & Category == input$unitsYr4) %>%
    select(N.Per.Unit)
  
  N_unit_yr4 <- df_N_unit_yr4[1,1]
  
  harvMat_yr4 <- N_unit_yr4 * input$yield_yr4
  
  
  df_perLoss_yr4 <- fert %>%
    filter(Fertilizer.N.Source == input$fertform_yr4 & Application.Method == input$appMeth_yr4) %>%
    select(Percent.Lost)
  
  perLoss_yr4 <- df_perLoss_yr4[1,1]
  
  ammLoss_yr4 <- input$fert_yr4 * (perLoss_yr4/100)
  
  
  df_perInoDent_yr4 <- soil %>%
    filter(Organic.Matter.Content == input$soilType & Soil.Drainage.Classification == input$drainClass) %>%
    select(Percent.of.inorganic.denitrified)
  
  perInoDent_yr4 <- df_perInoDent_yr4[1,1]
  
  dent_yr4 <- (input$fert_yr4 + precipitation_yr4 + irrigation_yr4) * (perInoDent_yr4 / 100)
  
  erosion_yr4 <- 0
  runoff_yr4 <- 0
  misgas_yr4 <- inputTotal_yr4 * 0.01
  ammAtSen_yr4 <- 0
  # Outputs
  totalOutputs_yr4 <- harvMat_yr4 + ammLoss_yr4 + dent_yr4 + erosion_yr4 + runoff_yr4 + misgas_yr4 + ammAtSen_yr4
  
  
  # Total Storage Change
  total_change_N_yr4 <- input$inoN_RM_yr4 + input$orgN_RM_yr4
  
  # Cover Crop Residue
  if(input$cp_yr4 == 0){
    cct_yr4 <- 0
    ccy_yr4 <- 0
  } else{
    cct_yr4 <- input$cct_yr4
    df_c_units_yr4 <- crops %>%
      filter(Crop.Type == input$cct_yr4 & Category == input$c_units_yr4) %>%
      select(Avg.N.harvested..lb.N.unit.)
    
    c_units_yr4 <- df_c_units_yr4[1,1]
    
    ccy_yr4 <- input$ccy_yr4
  }
  
  if(input$cp_yr4 == 0){
    n_per_unit_yr4 <- 0
  } else{
    df_n_per_unit_yr4 <- crops %>%
      filter(Crop.Type == input$cct_yr4 & Category == input$c_units_yr4) %>%
      select(N.Per.Unit)
    
    n_per_unit_yr4 <- df_n_per_unit_yr4[1,1]
    
  }
  totCovCropRes_yr4 <- ccy_yr4 * n_per_unit_yr4
  
  
  # total Inputs - Total Outputs - Total Storage Change - Total Cover Crop Residue
  LeachableN_yr4 <- inputTotal_yr4 - totalOutputs_yr4 - total_change_N_yr4 - totCovCropRes_yr4
  
  
  
  df_rechargeEstimate_yr4 <- soil %>%
    filter(Organic.Matter.Content == input$soilType & Soil.Drainage.Classification == input$drainClass) %>%
    select(Inches.Recharge)
  
  rechargeEstimate_yr4 <- df_rechargeEstimate_yr4[1,1]
  
  Nitrate_N_Estimate_yr4 <- LeachableN_yr4 / rechargeEstimate_yr4 / 0.226
  
  
  OP4_yr4 <- round(Nitrate_N_Estimate_yr4, digits = 1)
  OP4_yr4 <- as.character(OP4_yr4)
  
  
  if (length(OP4_yr4) == 0){
    paste("NaN")
  }else{
    paste(OP4_yr4)
  }
})

output$Avg_OP_value1 <- renderUI({
  # OP_yr1
  {
    # Getting Input Value for final calculation
    if (input$irr_yr1 == 1){
      irrigation_yr1 <- input$irri_yr1 * input$irrInch_yr1 * 0.226
    } else{
      irrigation_yr1 <- 0
    }
    precipitation_yr1 <- input$precip_RM_yr1 * input$InchPre_RM_yr1 * 0.226
    cropSeed_yr1 <- 0
    nonsymbioticFixation_yr1 <- 3

    # Inputs
    inputTotal_yr1 <- input$fert_yr1 + input$man_yr1 + input$leg_yr1 + irrigation_yr1 + precipitation_yr1 + precipitation_yr1 + cropSeed_yr1  + nonsymbioticFixation_yr1

    df_N_unit_yr1 <- crops %>%
      filter(Crop.Type == input$crop_yr1 & Category == input$unitsYr1) %>%
      select(N.Per.Unit)

    N_unit_yr1 <- df_N_unit_yr1[1,1]

    harvMat_yr1 <- N_unit_yr1 * input$yield_yr1

    perLoss_yr1 <- fert %>%
      filter(Fertilizer.N.Source == input$fertform_yr1 & Application.Method == input$appMeth_yr1) %>%
      select(Percent.Lost)

    ammLoss_yr1 <- input$fert_yr1 * (perLoss_yr1/100)

    if (input$irr_yr1 == 1){
      irrigation_yr1 <- input$irri_yr1 * input$irrInch_yr1 * 0.226
    } else{
      irrigation_yr1 <- 0
    }
    perInoDent_yr1 <- soil %>%
      filter(Organic.Matter.Content == input$soilType_RM & Soil.Drainage.Classification == input$drainClass_RM) %>%
      select(Percent.of.inorganic.denitrified)

    dent_yr1 <- (input$fert_yr1 + precipitation_yr1 + irrigation_yr1) * (perInoDent_yr1 / 100)
    erosion_yr1 <- 0
    runoff_yr1 <- 0
    misgas_yr1 <- inputTotal_yr1 * 0.01
    ammAtSen_yr1 <- 0

    # Outputs
    totalOutputs_yr1 <- harvMat_yr1 + ammLoss_yr1 + dent_yr1 + erosion_yr1 + runoff_yr1 + misgas_yr1 + ammAtSen_yr1

    # Total Storage Change
    total_change_N_yr1 <- input$inoN_RM_yr1 + input$orgN_RM_yr1

    # Cover Crop Residue
    if(input$cp_yr1 == 0){
      cct_yr1 <- 0
      c_units_yr1 <- 0
      ccy_yr1 <- 0
    } else{
      cct_yr1 <- input$cct_yr1
      c_units_yr1 <- crops %>%
        filter(Crop.Type == input$cct_yr1 & Category == input$c_units_yr1) %>%
        select(Avg.N.harvested..lb.N.unit.)
      ccy_yr1 <- input$ccy_yr1
    }

    if(c_units_yr1 == 0){
      n_per_unit_yr1 <- 0
    } else{
      df_n_per_unit_yr1 <- crops %>%
        filter(Crop.Type == input$cct_yr1 & Category == input$c_units_yr1) %>%
        select(N.Per.Unit)

      n_per_unit_yr1 <- df_n_per_unit_yr1[1,1]
    }
    totCovCropRes_yr1 <- ccy_yr1 * n_per_unit_yr1


    # totalInputs - TotalOutputs - Total Storage Change - Total Cover Crop Residue
    OP1_yr1 <- inputTotal_yr1 - totalOutputs_yr1 - total_change_N_yr1 - totCovCropRes_yr1
    OP1_yr1 <- round(OP1_yr1, digits = 1)
  }

  # OP_yr2
  {
    # Getting Input Value for final calculation
    if (input$irr_yr2 == 1){
      irrigation_yr2 <- input$irri_yr2 * input$irrInch_yr2 * 0.226
    } else{
      irrigation_yr2 <- 0
    }
    precipitation_yr2 <- input$precip_RM_yr2 * input$InchPre_RM_yr2 * 0.226
    cropSeed_yr2 <- 0
    nonsymbioticFixation_yr2 <- 3

    # Inputs
    inputTotal_yr2 <- input$fert_yr2 + input$man_yr2 + input$leg_yr2 + irrigation_yr2 + precipitation_yr2 + precipitation_yr2 + cropSeed_yr2  + nonsymbioticFixation_yr2

    df_N_unit_yr2 <- crops %>%
      filter(Crop.Type == input$crop_yr2 & Category == input$unitsYr2) %>%
      select(N.Per.Unit)

    N_unit_yr2 <- df_N_unit_yr2[1,1]

    harvMat_yr2 <- N_unit_yr2 * input$yield_yr2

    perLoss_yr2 <- fert %>%
      filter(Fertilizer.N.Source == input$fertform_yr2 & Application.Method == input$appMeth_yr2) %>%
      select(Percent.Lost)

    ammLoss_yr2 <- input$fert_yr2 * (perLoss_yr2/100)

    if (input$irr_yr2 == 1){
      irrigation_yr2 <- input$irri_yr2 * input$irrInch_yr2 * 0.226
    } else{
      irrigation_yr2 <- 0
    }
    perInoDent_yr2 <- soil %>%
      filter(Organic.Matter.Content == input$soilType_RM & Soil.Drainage.Classification == input$drainClass_RM) %>%
      select(Percent.of.inorganic.denitrified)

    dent_yr2 <- (input$fert_yr2 + precipitation_yr2 + irrigation_yr2) * (perInoDent_yr2 / 100)
    erosion_yr2 <- 0
    runoff_yr2 <- 0
    misgas_yr2 <- inputTotal_yr2 * 0.01
    ammAtSen_yr2 <- 0

    # Outputs
    totalOutputs_yr2 <- harvMat_yr2 + ammLoss_yr2 + dent_yr2 + erosion_yr2 + runoff_yr2 + misgas_yr2 + ammAtSen_yr2

    # Total Storage Change
    total_change_N_yr2 <- input$inoN_RM_yr2 + input$orgN_RM_yr2

    # Cover Crop Residue
    if(input$cp_yr2 == 0){
      cct_yr2 <- 0
      c_units_yr2 <- 0
      ccy_yr2 <- 0
    } else{
      cct_yr2 <- input$cct_yr2
      c_units_yr2 <- crops %>%
        filter(Crop.Type == input$cct_yr2 & Category == input$c_units_yr2) %>%
        select(Avg.N.harvested..lb.N.unit.)
      ccy_yr2 <- input$ccy_yr2
    }

    if(c_units_yr2 == 0){
      n_per_unit_yr2 <- 0
    } else{
      df_n_per_unit_yr2 <- crops %>%
        filter(Crop.Type == input$cct_yr2 & Category == input$c_units_yr2) %>%
        select(N.Per.Unit)

      n_per_unit_yr2 <- df_n_per_unit_yr2[1,1]
    }
    totCovCropRes_yr2 <- ccy_yr2 * n_per_unit_yr2


    # totalInputs - TotalOutputs - Total Storage Change - Total Cover Crop Residue
    OP1_yr2 <- inputTotal_yr2 - totalOutputs_yr2 - total_change_N_yr2 - totCovCropRes_yr2
    OP1_yr2 <- round(OP1_yr2, digits = 1)
  }

  # OP_yr3
  {
  # Getting Input Value for final calculation
  if (input$irr_yr3 == 1){
    irrigation_yr3 <- input$irri_yr3 * input$irrInch_yr3 * 0.226
  } else{
    irrigation_yr3 <- 0
  }
  precipitation_yr3 <- input$precip_RM_yr3 * input$InchPre_RM_yr3 * 0.226
  cropSeed_yr3 <- 0
  nonsymbioticFixation_yr3 <- 3

  # Inputs
  inputTotal_yr3 <- input$fert_yr3 + input$man_yr3 + input$leg_yr3 + irrigation_yr3 + precipitation_yr3 + precipitation_yr3 + cropSeed_yr3  + nonsymbioticFixation_yr3

  df_N_unit_yr3 <- crops %>%
    filter(Crop.Type == input$crop_yr3 & Category == input$unitsYr3) %>%
    select(N.Per.Unit)

  N_unit_yr3 <- df_N_unit_yr3[1,1]

  harvMat_yr3 <- N_unit_yr3 * input$yield_yr3

  perLoss_yr3 <- fert %>%
    filter(Fertilizer.N.Source == input$fertform_yr3 & Application.Method == input$appMeth_yr3) %>%
    select(Percent.Lost)

  ammLoss_yr3 <- input$fert_yr3 * (perLoss_yr3/100)

  if (input$irr_yr3 == 1){
    irrigation_yr3 <- input$irri_yr3 * input$irrInch_yr3 * 0.226
  } else{
    irrigation_yr3 <- 0
  }
  perInoDent_yr3 <- soil %>%
    filter(Organic.Matter.Content == input$soilType_RM & Soil.Drainage.Classification == input$drainClass_RM) %>%
    select(Percent.of.inorganic.denitrified)

  dent_yr3 <- (input$fert_yr3 + precipitation_yr3 + irrigation_yr3) * (perInoDent_yr3 / 100)
  erosion_yr3 <- 0
  runoff_yr3 <- 0
  misgas_yr3 <- inputTotal_yr3 * 0.01
  ammAtSen_yr3 <- 0

  # Outputs
  totalOutputs_yr3 <- harvMat_yr3 + ammLoss_yr3 + dent_yr3 + erosion_yr3 + runoff_yr3 + misgas_yr3 + ammAtSen_yr3

  # Total Storage Change
  total_change_N_yr3 <- input$inoN_RM_yr3 + input$orgN_RM_yr3

  # Cover Crop Residue
  if(input$cp_yr3 == 0){
    cct_yr3 <- 0
    c_units_yr3 <- 0
    ccy_yr3 <- 0
  } else{
    cct_yr3 <- input$cct_yr3
    c_units_yr3 <- crops %>%
      filter(Crop.Type == input$cct_yr3 & Category == input$c_units_yr3) %>%
      select(Avg.N.harvested..lb.N.unit.)
    ccy_yr3 <- input$ccy_yr3
  }

  if(c_units_yr3 == 0){
    n_per_unit_yr3 <- 0
  } else{
    df_n_per_unit_yr3 <- crops %>%
      filter(Crop.Type == input$cct_yr3 & Category == input$c_units_yr3) %>%
      select(N.Per.Unit)

    n_per_unit_yr3 <- df_n_per_unit_yr3[1,1]
  }
  totCovCropRes_yr3 <- ccy_yr3 * n_per_unit_yr3


  # totalInputs - TotalOutputs - Total Storage Change - Total Cover Crop Residue
  OP1_yr3 <- inputTotal_yr3 - totalOutputs_yr3 - total_change_N_yr3 - totCovCropRes_yr3
  OP1_yr3 <- round(OP1_yr3, digits = 1)
}

  # OP_yr4
  {
    # Getting Input Value for final calculation
    if (input$irr_yr4 == 1){
      irrigation_yr4 <- input$irri_yr4 * input$irrInch_yr4 * 0.226
    } else{
      irrigation_yr4 <- 0
    }
    precipitation_yr4 <- input$precip_RM_yr4 * input$InchPre_RM_yr4 * 0.226
    cropSeed_yr4 <- 0
    nonsymbioticFixation_yr4 <- 3

    # Inputs
    inputTotal_yr4 <- input$fert_yr4 + input$man_yr4 + input$leg_yr4 + irrigation_yr4 + precipitation_yr4 + precipitation_yr4 + cropSeed_yr4  + nonsymbioticFixation_yr4

    df_N_unit_yr4 <- crops %>%
      filter(Crop.Type == input$crop_yr4 & Category == input$unitsYr4) %>%
      select(N.Per.Unit)

    N_unit_yr4 <- df_N_unit_yr4[1,1]

    harvMat_yr4 <- N_unit_yr4 * input$yield_yr4

    perLoss_yr4 <- fert %>%
      filter(Fertilizer.N.Source == input$fertform_yr4 & Application.Method == input$appMeth_yr4) %>%
      select(Percent.Lost)

    ammLoss_yr4 <- input$fert_yr4 * (perLoss_yr4/100)

    if (input$irr_yr4 == 1){
      irrigation_yr4 <- input$irri_yr4 * input$irrInch_yr4 * 0.226
    } else{
      irrigation_yr4 <- 0
    }
    perInoDent_yr4 <- soil %>%
      filter(Organic.Matter.Content == input$soilType_RM & Soil.Drainage.Classification == input$drainClass_RM) %>%
      select(Percent.of.inorganic.denitrified)

    dent_yr4 <- (input$fert_yr4 + precipitation_yr4 + irrigation_yr4) * (perInoDent_yr4 / 100)
    erosion_yr4 <- 0
    runoff_yr4 <- 0
    misgas_yr4 <- inputTotal_yr4 * 0.01
    ammAtSen_yr4 <- 0

    # Outputs
    totalOutputs_yr4 <- harvMat_yr4 + ammLoss_yr4 + dent_yr4 + erosion_yr4 + runoff_yr4 + misgas_yr4 + ammAtSen_yr4

    # Total Storage Change
    total_change_N_yr4 <- input$inoN_RM_yr4 + input$orgN_RM_yr4

    # Cover Crop Residue
    if(input$cp_yr4 == 0){
      cct_yr4 <- 0
      c_units_yr4 <- 0
      ccy_yr4 <- 0
    } else{
      cct_yr4 <- input$cct_yr4
      c_units_yr4 <- crops %>%
        filter(Crop.Type == input$cct_yr4 & Category == input$c_units_yr4) %>%
        select(Avg.N.harvested..lb.N.unit.)
      ccy_yr4 <- input$ccy_yr4
    }

    if(c_units_yr4 == 0){
      n_per_unit_yr4 <- 0
    } else{
      df_n_per_unit_yr4 <- crops %>%
        filter(Crop.Type == input$cct_yr4 & Category == input$c_units_yr4) %>%
        select(N.Per.Unit)

      n_per_unit_yr4 <- df_n_per_unit_yr4[1,1]
    }
    totCovCropRes_yr4 <- ccy_yr4 * n_per_unit_yr4


    # totalInputs - TotalOutputs - Total Storage Change - Total Cover Crop Residue
    OP1_yr4 <- inputTotal_yr4 - totalOutputs_yr4 - total_change_N_yr4 - totCovCropRes_yr4
    OP1_yr4 <- round(OP1_yr4, digits = 1)
    
  }
  OP_Avg <- (OP1_yr1 + OP1_yr2 + OP1_yr3 + OP1_yr4) / 4
  paste(OP_Avg)
})
