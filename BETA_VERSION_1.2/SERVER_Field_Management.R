output$OP_value1 <- renderUI({

  legume1 <- crops %>%
   filter(Crop.Type == input$crop1 & Category == input$units1) %>%
   select(Legume)
  
  N_unit1 <- crops %>%
     filter(Crop.Type == input$crop1 & Category == input$units1) %>%
     select(N.Per.Unit)
   harvMat1 <- N_unit1 * input$yield1
  
  ###calculate N fixation from legume
   leg1_est <- (legume1*harvMat1)+0.5*(legume1*harvMat1)
  # ###
 
  
  
  # Getting Input Value for final calculation
  if (input$irr == 1){
    irrigation <- input$irri * input$irrInch * 0.226
  } else{
    irrigation <- 0
  }
  precipitation <- input$precip * input$InchPre * 0.226
  cropSeed1 <- 0
  nonsymbioticFixation1 <- 3
  
  # Inputs
  #inputTotal1 <- input$fert1 + input$man1 + input$leg1 + irrigation + precipitation + precipitation + cropSeed1 + nonsymbioticFixation1
  inputTotal1 <- input$fert1 + input$man1 + leg1_est + irrigation + precipitation + precipitation + cropSeed1 + nonsymbioticFixation1
  
  
  
  N_unit1 <- crops %>%
    filter(Crop.Type == input$crop1 & Category == input$units1) %>%
    select(N.Per.Unit)
  harvMat1 <- N_unit1 * input$yield1
  perLoss1 <- fert %>%
    filter(Fertilizer.N.Source == input$fertform1 & Application.Method == input$appMeth1) %>%
    select(Percent.Lost)
  ammLoss1 <- input$fert1 * (perLoss1/100)
  
  if (input$irr == 1){
    irrigation <- input$irri * input$irrInch * 0.226
  } else{
    irrigation <- 0
  }
  perInoDent <- soil %>%
    filter(Organic.Matter.Content == input$soilType & Soil.Drainage.Classification == input$drainClass) %>%
    select(Percent.of.inorganic.denitrified)
  dent1 <- (input$fert1 + precipitation + irrigation) * (perInoDent / 100)
  erosion1 <- 0
  runoff1 <- 0
  misgas1 <- inputTotal1 * 0.01
  ammAtSen1 <-harvMat1 * 0.05
  # Outputs
  totalOutputs1 <- harvMat1 + ammLoss1 + dent1 + erosion1 + runoff1 + misgas1 + ammAtSen1
  
  # Total Storage Change
  total_change_N <- input$inoN + input$orgN
  
  # Cover Crop Residue
  if(input$cp1 == 0){
    cct1 <- 0
    c_units1 <- 0
    ccy1 <- 0
  } else{
    cct1 <- input$cct1
    c_units1 <- crops %>%
      filter(Crop.Type == input$cct1 & Category == input$c_units1) %>%
      select(Avg.N.harvested..lb.N.unit.)
    ccy1 <- input$ccy1
  }
  
  if(c_units1 == 0){
    n_per_unit1 <- 0
  } else{
    n_per_unit1 <- crops %>%
      filter(Crop.Type == input$cct1 & Category == input$c_units1) %>%
      select(N.Per.Unit)
  }
  totCovCropRes1 <- ccy1 * n_per_unit1
  
  # totalInputs - TotalOutputs - Total Storage Change - Total Cover Crop Residue
  OP1 <- inputTotal1 - totalOutputs1 - total_change_N - totCovCropRes1
  OP1 <- round(OP1, digits = 1)
  OP1 <- as.character(OP1)
  paste(OP1)

})

output$OP_value2 <- renderUI({
  {
    legume2 <- crops %>%
      filter(Crop.Type == input$crop2 & Category == input$units2) %>%
      select(Legume)
    
    N_unit2 <- crops %>%
      filter(Crop.Type == input$crop2 & Category == input$units2) %>%
      select(N.Per.Unit)
    
    harvMat2 <- N_unit2 * input$yield2
    
    ####
    leg2_est <- (legume2*harvMat2)+0.5*(legume2*harvMat2)
    ###
    
    # Getting Input Value for final calculation
    if (input$irr == 1){
      irrigation <- input$irri * input$irrInch * 0.226
    } else{
      irrigation <- 0 
    }
    precipitation <- input$precip * input$InchPre * 0.226
    cropSeed2 <- 0
    nonsymbioticFixation2 <- 3
    
    # Inputs
    inputTotal2 <- input$fert2 + input$man2 + leg2_est + irrigation + precipitation + precipitation + cropSeed2 + nonsymbioticFixation2
    

    

    perLoss2 <- fert %>%
      filter(Fertilizer.N.Source == input$fertform2 & Application.Method == input$appMeth2) %>%
      select(Percent.Lost)
    ammLoss2 <- input$fert2 * (perLoss2/100)
    
    if (input$irr == 1){
      irrigation <- input$irri * input$irrInch * 0.226
    } else{
      irrigation <- 0
    }
    perInoDent <- soil %>%
      filter(Organic.Matter.Content == input$soilType & Soil.Drainage.Classification == input$drainClass) %>%
      select(Percent.of.inorganic.denitrified)
    dent2 <- (input$fert2 + precipitation + irrigation) * (perInoDent / 100)
    erosion2 <- 0
    runoff2 <- 0
    misgas2 <- inputTotal2 * 0.01
    ammAtSen2 <- harvMat2 * 0.05
    # Outputs
    totalOutputs2 <- harvMat2 + ammLoss2 + dent2 + erosion2 + runoff2 + misgas2 + ammAtSen2
    
    # Total Storage Change
    total_change_N <- input$inoN + input$orgN
    
    # Cover Crop Residue
    if(input$cp2 == 0){
      cct2 <- 0
      c_units2 <- 0
      ccy2 <- 0
    } else{
      cct2 <- input$cct2
      c_units2 <- crops %>%
        filter(Crop.Type == input$cct2 & Category == input$c_units2) %>%
        select(Avg.N.harvested..lb.N.unit.)
      ccy2 <- input$ccy2
    }
    
    if(c_units2 == 0){
      n_per_unit2 <- 0
    } else{
      n_per_unit2 <- crops %>%
        filter(Crop.Type == input$cct2 & Category == input$c_units2) %>%
        select(N.Per.Unit)
    }
    totCovCropRes2 <- ccy2 * n_per_unit2
  }
  # totalInputs - TotalOutputs - Total Storage Change - Total Cover Crop Residue
  OP2 <- inputTotal2 - totalOutputs2 - total_change_N - totCovCropRes2
  OP2 <- round(OP2, digits = 1)
  OP2 <- as.character(OP2)
  paste(OP2)
  
})

output$OP_value3 <- renderUI({
  {
    # Getting Input Value for final calulation
    if (input$irr == 1){
      irrigation <- input$irri * input$irrInch * 0.226
    } else{
      irrigation <- 0
    }
    precipitation <- input$precip * input$InchPre * 0.226
    cropSeed1 <- 0
    nonsymbioticFixation1 <- 3
    
    N_unit1 <- crops %>%
      filter(Crop.Type == input$crop1 & Category == input$units1) %>%
      select(N.Per.Unit)
    ##Added legume routine
    legume1 <- crops %>%
      filter(Crop.Type == input$crop1 & Category == input$units1) %>%
      select(Legume)
    ###
    harvMat1 <- N_unit1 * input$yield1
    
    leg1_est <- (legume1*harvMat1)+0.5*(legume1*harvMat1)
    
    # Inputs
    inputTotal1 <- input$fert1 + input$man1 + leg1_est + irrigation + precipitation + precipitation + cropSeed1 + nonsymbioticFixation1
    
    perLoss1 <- fert %>%
      filter(Fertilizer.N.Source == input$fertform1 & Application.Method == input$appMeth1) %>%
      select(Percent.Lost)
    ammLoss1 <- input$fert1 * (perLoss1/100)
    
    if (input$irr == 1){
      irrigation <- input$irri * input$irrInch * 0.226
    } else{
      irrigation <- 0
    }
    perInoDent <- soil %>%
      filter(Organic.Matter.Content == input$soilType & Soil.Drainage.Classification == input$drainClass) %>%
      select(Percent.of.inorganic.denitrified)
    dent1 <- (input$fert1 + precipitation + irrigation) * (perInoDent / 100)
    erosion1 <- 0
    runoff1 <- 0
    misgas1 <- inputTotal1 * 0.01
    ammAtSen1 <- harvMat1 * 0.05
    # Outputs
    totalOutputs1 <- harvMat1 + ammLoss1 + dent1 + erosion1 + runoff1 + misgas1 + ammAtSen1
    
    # Total Storage Change
    total_change_N <- input$inoN + input$orgN
    
    # Cover Crop Residue
    if(input$cp1 == 0){
      cct1 <- 0
      c_units1 <- 0
      ccy1 <- 0
    } else{
      cct1 <- input$cct1
      c_units1 <- crops %>%
        filter(Crop.Type == input$cct1 & Category == input$c_units1) %>%
        select(Avg.N.harvested..lb.N.unit.)
      ccy1 <- input$ccy1
    }
    
    if(c_units1 == 0){
      n_per_unit1 <- 0
    } else{
      n_per_unit1 <- crops %>%
        filter(Crop.Type == input$cct1 & Category == input$c_units1) %>%
        select(N.Per.Unit)
    }
    totCovCropRes1 <- ccy1 * n_per_unit1
    
    # totalInputs - TotalOutputs - Total Storage Change - Total Cover Crop Residue
    LeachableN1 <- inputTotal1 - totalOutputs1 - total_change_N - totCovCropRes1
  }
  
  rechargeEstimate1 <- soil %>%
    filter(Organic.Matter.Content == input$soilType & Soil.Drainage.Classification == input$drainClass) %>%
    select(Inches.Recharge)
  
  Nitrate_N_Estimate1 <- LeachableN1 / rechargeEstimate1 / 0.226
  
  if(is.na(Nitrate_N_Estimate1)){
    OP3 <- 0
  }else{
    if(Nitrate_N_Estimate1 < 0){
      OP3 <- Nitrate_N_Estimate1 <- 0
    }else{
      OP3 <- Nitrate_N_Estimate1
    }
  }
  
  OP3 <- round(OP3, digits = 1)
  OP3 <- as.character(OP3)
  paste(OP3)
  
})

output$OP_value4 <- renderUI({
  {
    legume2 <- crops %>%
      filter(Crop.Type == input$crop2 & Category == input$units2) %>%
      select(Legume)
    
    N_unit2 <- crops %>%
      filter(Crop.Type == input$crop2 & Category == input$units2) %>%
      select(N.Per.Unit)
    
    harvMat2 <- N_unit2 * input$yield2
    
    leg2_est <- (legume2*harvMat2)+0.5*(legume2*harvMat2)
    
    
    
    # Getting Input Value for final calulation
    if (input$irr == 1){
      irrigation <- input$irri * input$irrInch * 0.226
    } else{
      irrigation <- 0 
    }
    precipitation <- input$precip * input$InchPre * 0.226
    cropSeed2 <- 0
    nonsymbioticFixation2 <- 3
    # Inputs
    inputTotal2 <- input$fert2 + input$man2 + leg2_est + irrigation + precipitation + precipitation + cropSeed2 + nonsymbioticFixation2
    

    
    perLoss2 <- fert %>%
      filter(Fertilizer.N.Source == input$fertform2 & Application.Method == input$appMeth2) %>%
      select(Percent.Lost)
    ammLoss2 <- input$fert2 * (perLoss2/100)
    
    if (input$irr == 1){
      irrigation <- input$irri * input$irrInch * 0.226
    } else{
      irrigation <- 0
    }
    perInoDent <- soil %>%
      filter(Organic.Matter.Content == input$soilType & Soil.Drainage.Classification == input$drainClass) %>%
      select(Percent.of.inorganic.denitrified)
    dent2 <- (input$fert2 + precipitation + irrigation) * (perInoDent / 100)
    erosion2 <- 0
    runoff2 <- 0
    misgas2 <- inputTotal2 * 0.01
    ammAtSen2 <- harvMat2 * 0.05
    # Outputs
    totalOutputs2 <- harvMat2 + ammLoss2 + dent2 + erosion2 + runoff2 + misgas2 + ammAtSen2
    
    # Total Storage Change
    total_change_N <- input$inoN + input$orgN
    
    # Cover Crop Residue
    if(input$cp2 == 0){
      cct2 <- 0
      c_units2 <- 0
      ccy2 <- 0
    } else{
      cct2 <- input$cct2
      c_units2 <- crops %>%
        filter(Crop.Type == input$cct2 & Category == input$c_units2) %>%
        select(Avg.N.harvested..lb.N.unit.)
      ccy2 <- input$ccy2
    }
    
    if(c_units2 == 0){
      n_per_unit2 <- 0
    } else{
      n_per_unit2 <- crops %>%
        filter(Crop.Type == input$cct2 & Category == input$c_units2) %>%
        select(N.Per.Unit)
    }
    totCovCropRes2 <- ccy2 * n_per_unit2
    
    # totalInputs - TotalOutputs - Total Storage Change - Total Cover Crop Residue
    inputTotal2 - totalOutputs2 - total_change_N - totCovCropRes2
    
    # totalInputs - TotalOutputs - Total Storage Change - Total Cover Crop Residue
    LeachableN2 <- inputTotal2 - totalOutputs2 - total_change_N - totCovCropRes2
  }
  rechargeEstimate2 <- soil %>%
    filter(Organic.Matter.Content == input$soilType & Soil.Drainage.Classification == input$drainClass) %>%
    select(Inches.Recharge)
  
  Nitrate_N_Estimate2 <- LeachableN2 / rechargeEstimate2 / 0.226
  
  if(is.na(Nitrate_N_Estimate2)){
    OP4 <- 0
  }else{
    if(Nitrate_N_Estimate2 < 0){
      OP4 <- Nitrate_N_Estimate2 <- 0
    }else{
      OP4 <- Nitrate_N_Estimate2
    }
  }
  
  OP4 <- round(OP4, digits = 1)
  OP4 <- as.character(OP4)
  paste(OP4)
  
})

output$OP_value5 <- renderUI({
  {
    N_unit1 <- crops %>%
      filter(Crop.Type == input$crop1 & Category == input$units1) %>%
      select(N.Per.Unit)
    
    ##Added legume routine
    legume1 <- crops %>%
      filter(Crop.Type == input$crop1 & Category == input$units1) %>%
      select(Legume)
    ###
    harvMat1 <- N_unit1 * input$yield1
    
    leg1_est <- (legume1*harvMat1)+0.5*(legume1*harvMat1)
    
    # Getting Input Value for final calulation
    if (input$irr == 1){
      irrigation <- input$irri * input$irrInch * 0.226
    } else{
      irrigation <- 0
    }
    precipitation <- input$precip * input$InchPre * 0.226
    cropSeed1 <- 0
    nonsymbioticFixation1 <- 3
    # Inputs
    inputTotal1 <- input$fert1 + input$man1 + leg1_est + irrigation + precipitation + precipitation + cropSeed1 + nonsymbioticFixation1
    
    N_unit1 <- crops %>%
      filter(Crop.Type == input$crop1 & Category == input$units1) %>%
      select(N.Per.Unit)
    harvMat1 <- N_unit1 * input$yield1
    
    perLoss1 <- fert %>%
      filter(Fertilizer.N.Source == input$fertform1 & Application.Method == input$appMeth1) %>%
      select(Percent.Lost)
    ammLoss1 <- input$fert1 * (perLoss1/100)
    
    if (input$irr == 1){
      irrigation <- input$irri * input$irrInch * 0.226
    } else{
      irrigation <- 0
    }
    perInoDent <- soil %>%
      filter(Organic.Matter.Content == input$soilType & Soil.Drainage.Classification == input$drainClass) %>%
      select(Percent.of.inorganic.denitrified)
    dent1 <- (input$fert1 + precipitation + irrigation) * (perInoDent / 100)
    erosion1 <- 0
    runoff1 <- 0
    misgas1 <- inputTotal1 * 0.01
    ammAtSen1 <- harvMat1 * 0.05
    # Outputs
    totalOutputs1 <- harvMat1 + ammLoss1 + dent1 + erosion1 + runoff1 + misgas1 + ammAtSen1
    
    # Total Storage Change
    total_change_N <- input$inoN + input$orgN
    
    # Cover Crop Residue
    if(input$cp1 == 0){
      cct1 <- 0
      c_units1 <- 0
      ccy1 <- 0
    } else{
      cct1 <- input$cct1
      c_units1 <- crops %>%
        filter(Crop.Type == input$cct1 & Category == input$c_units1) %>%
        select(Avg.N.harvested..lb.N.unit.)
      ccy1 <- input$ccy1
    }
    
    if(c_units1 == 0){
      n_per_unit1 <- 0
    } else{
      n_per_unit1 <- crops %>%
        filter(Crop.Type == input$cct1 & Category == input$c_units1) %>%
        select(N.Per.Unit)
    }
    totCovCropRes1 <- ccy1 * n_per_unit1
    
    # totalInputs - TotalOutputs - Total Storage Change - Total Cover Crop Residue
    LeachableN1 <- inputTotal1 - totalOutputs1 - total_change_N - totCovCropRes1
  }
  {
    # Getting Input Value for final calulation
    legume2 <- crops %>%
      filter(Crop.Type == input$crop2 & Category == input$units2) %>%
      select(Legume)
    
    N_unit2 <- crops %>%
      filter(Crop.Type == input$crop2 & Category == input$units2) %>%
      select(N.Per.Unit)
    
    harvMat2 <- N_unit2 * input$yield2
    
    leg2_est <- (legume2*harvMat2)+0.5*(legume2*harvMat2)
    
    if (input$irr == 1){
      irrigation <- input$irri * input$irrInch * 0.226
    } else{
      irrigation <- 0 
    }
    precipitation <- input$precip * input$InchPre * 0.226
    cropSeed2 <- 0
    nonsymbioticFixation2 <- 3
    # Inputs
    inputTotal2 <- input$fert2 + input$man2 + leg2_est + irrigation + precipitation + precipitation + cropSeed2 + nonsymbioticFixation2
    
    N_unit2 <- crops %>%
      filter(Crop.Type == input$crop2 & Category == input$units2) %>%
      select(N.Per.Unit)
    harvMat2 <- N_unit2 * input$yield2
    perLoss2 <- fert %>%
      filter(Fertilizer.N.Source == input$fertform2 & Application.Method == input$appMeth2) %>%
      select(Percent.Lost)
    ammLoss2 <- input$fert2 * (perLoss2/100)
    
    if (input$irr == 1){
      irrigation <- input$irri * input$irrInch * 0.226
    } else{
      irrigation <- 0
    }
    perInoDent <- soil %>%
      filter(Organic.Matter.Content == input$soilType & Soil.Drainage.Classification == input$drainClass) %>%
      select(Percent.of.inorganic.denitrified)
    dent2 <- (input$fert2 + precipitation + irrigation) * (perInoDent / 100)
    erosion2 <- 0
    runoff2 <- 0
    misgas2 <- inputTotal2 * 0.01
    ammAtSen2 <- harvMat2 * 0.05
    # Outputs
    totalOutputs2 <- harvMat2 + ammLoss2 + dent2 + erosion2 + runoff2 + misgas2 + ammAtSen2
    
    # Total Storage Change
    total_change_N <- input$inoN + input$orgN
    
    # Cover Crop Residue
    if(input$cp2 == 0){
      cct2 <- 0
      c_units2 <- 0
      ccy2 <- 0
    } else{
      cct2 <- input$cct2
      c_units2 <- crops %>%
        filter(Crop.Type == input$cct2 & Category == input$c_units2) %>%
        select(Avg.N.harvested..lb.N.unit.)
      ccy2 <- input$ccy2
    }
    
    if(c_units2 == 0){
      n_per_unit2 <- 0
    } else{
      n_per_unit2 <- crops %>%
        filter(Crop.Type == input$cct2 & Category == input$c_units2) %>%
        select(N.Per.Unit)
    }
    totCovCropRes2 <- ccy2 * n_per_unit2
    
    # totalInputs - TotalOutputs - Total Storage Change - Total Cover Crop Residue
    inputTotal2 - totalOutputs2 - total_change_N - totCovCropRes2
    
    # totalInputs - TotalOutputs - Total Storage Change - Total Cover Crop Residue
    LeachableN2 <- inputTotal2 - totalOutputs2 - total_change_N - totCovCropRes2
  }
  
  OP5 <- ((LeachableN1 * input$acres1) + (LeachableN2 * input$acres2)) / (input$acres1 + input$acres2)
  OP5 <- round(OP5, digits = 1)
  OP5 <- as.character(OP5)
  paste(OP5)
})

output$OP_value6 <- renderUI({
  {
    {
      N_unit1 <- crops %>%
        filter(Crop.Type == input$crop1 & Category == input$units1) %>%
        select(N.Per.Unit)
      
      ##Added legume routine
      legume1 <- crops %>%
        filter(Crop.Type == input$crop1 & Category == input$units1) %>%
        select(Legume)
      ###
      harvMat1 <- N_unit1 * input$yield1
      
      leg1_est <- (legume1*harvMat1)+0.5*(legume1*harvMat1)
      
      # Getting Input Value for final calulation
      if (input$irr == 1){
        irrigation <- input$irri * input$irrInch * 0.226
      } else{
        irrigation <- 0
      }
      precipitation <- input$precip * input$InchPre * 0.226
      cropSeed1 <- 0
      nonsymbioticFixation1 <- 3
      # Inputs
      inputTotal1 <- input$fert1 + input$man1 + leg1_est + irrigation + precipitation + precipitation + cropSeed1 + nonsymbioticFixation1
      
      N_unit1 <- crops %>%
        filter(Crop.Type == input$crop1 & Category == input$units1) %>%
        select(N.Per.Unit)
      harvMat1 <- N_unit1 * input$yield1
      perLoss1 <- fert %>%
        filter(Fertilizer.N.Source == input$fertform1 & Application.Method == input$appMeth1) %>%
        select(Percent.Lost)
      ammLoss1 <- input$fert1 * (perLoss1/100)
      
      if (input$irr == 1){
        irrigation <- input$irri * input$irrInch * 0.226
      } else{
        irrigation <- 0
      }
      perInoDent <- soil %>%
        filter(Organic.Matter.Content == input$soilType & Soil.Drainage.Classification == input$drainClass) %>%
        select(Percent.of.inorganic.denitrified)
      dent1 <- (input$fert1 + precipitation + irrigation) * (perInoDent / 100)
      erosion1 <- 0
      runoff1 <- 0
      misgas1 <- inputTotal1 * 0.01
      ammAtSen1 <- harvMat1 * 0.05
      # Outputs
      totalOutputs1 <- harvMat1 + ammLoss1 + dent1 + erosion1 + runoff1 + misgas1 + ammAtSen1
      
      # Total Storage Change
      total_change_N <- input$inoN + input$orgN
      
      # Cover Crop Residue
      if(input$cp1 == 0){
        cct1 <- 0
        c_units1 <- 0
        ccy1 <- 0
      } else{
        cct1 <- input$cct1
        c_units1 <- crops %>%
          filter(Crop.Type == input$cct1 & Category == input$c_units1) %>%
          select(Avg.N.harvested..lb.N.unit.)
        ccy1 <- input$ccy1
      }
      
      if(c_units1 == 0){
        n_per_unit1 <- 0
      } else{
        n_per_unit1 <- crops %>%
          filter(Crop.Type == input$cct1 & Category == input$c_units1) %>%
          select(N.Per.Unit)
      }
      totCovCropRes1 <- ccy1 * n_per_unit1
      
      # totalInputs - TotalOutputs - Total Storage Change - Total Cover Crop Residue
      LeachableN1 <- inputTotal1 - totalOutputs1 - total_change_N - totCovCropRes1
    }
    {
      # Getting Input Value for final calulation
      legume2 <- crops %>%
        filter(Crop.Type == input$crop2 & Category == input$units2) %>%
        select(Legume)
      
      N_unit2 <- crops %>%
        filter(Crop.Type == input$crop2 & Category == input$units2) %>%
        select(N.Per.Unit)
      
      harvMat2 <- N_unit2 * input$yield2
      
      leg2_est <- (legume2*harvMat2)+0.5*(legume2*harvMat2)
      
      if (input$irr == 1){
        irrigation <- input$irri * input$irrInch * 0.226
      } else{
        irrigation <- 0 
      }
      precipitation <- input$precip * input$InchPre * 0.226
      cropSeed2 <- 0
      nonsymbioticFixation2 <- 3
      # Inputs
      inputTotal2 <- input$fert2 + input$man2 + leg2_est + irrigation + precipitation + precipitation + cropSeed2 + nonsymbioticFixation2
      
      N_unit2 <- crops %>%
        filter(Crop.Type == input$crop2 & Category == input$units2) %>%
        select(N.Per.Unit)
      harvMat2 <- N_unit2 * input$yield2
      perLoss2 <- fert %>%
        filter(Fertilizer.N.Source == input$fertform2 & Application.Method == input$appMeth2) %>%
        select(Percent.Lost)
      ammLoss2 <- input$fert2 * (perLoss2/100)
      
      if (input$irr == 1){
        irrigation <- input$irri * input$irrInch * 0.226
      } else{
        irrigation <- 0
      }
      perInoDent <- soil %>%
        filter(Organic.Matter.Content == input$soilType & Soil.Drainage.Classification == input$drainClass) %>%
        select(Percent.of.inorganic.denitrified)
      dent2 <- (input$fert2 + precipitation + irrigation) * (perInoDent / 100)
      erosion2 <- 0
      runoff2 <- 0
      misgas2 <- inputTotal2 * 0.01
      ammAtSen2 <- harvMat2 * 0.05
      # Outputs
      totalOutputs2 <- harvMat2 + ammLoss2 + dent2 + erosion2 + runoff2 + misgas2 + ammAtSen2
      
      # Total Storage Change
      total_change_N <- input$inoN + input$orgN
      
      # Cover Crop Residue
      if(input$cp2 == 0){
        cct2 <- 0
        c_units2 <- 0
        ccy2 <- 0
      } else{
        cct2 <- input$cct2
        c_units2 <- crops %>%
          filter(Crop.Type == input$cct2 & Category == input$c_units2) %>%
          select(Avg.N.harvested..lb.N.unit.)
        ccy2 <- input$ccy2
      }
      
      if(c_units2 == 0){
        n_per_unit2 <- 0
      } else{
        n_per_unit2 <- crops %>%
          filter(Crop.Type == input$cct2 & Category == input$c_units2) %>%
          select(N.Per.Unit)
      }
      totCovCropRes2 <- ccy2 * n_per_unit2
      
      # totalInputs - TotalOutputs - Total Storage Change - Total Cover Crop Residue
      inputTotal2 - totalOutputs2 - total_change_N - totCovCropRes2
      
      # totalInputs - TotalOutputs - Total Storage Change - Total Cover Crop Residue
      LeachableN2 <- inputTotal2 - totalOutputs2 - total_change_N - totCovCropRes2
    }
    
    totalLeachableN <- ((LeachableN1 * input$acres1) + (LeachableN2 * input$acres2)) / (input$acres1 + input$acres2)
    
  }
  rechargeEstimateT <- soil %>%
    filter(Organic.Matter.Content == input$soilType & Soil.Drainage.Classification == input$drainClass) %>%
    select(Inches.Recharge)
  
  OP6 <- totalLeachableN / rechargeEstimateT / 0.226
  OP6 <- round(OP6, digits = 1)
  OP6 <- as.character(OP6)
  paste(OP6)
})



output$unit_selection1 <- renderUI({
  units1 <- filter(crops, Crop.Type == input$crop1)
  
  selectInput("units1",
              NULL,
              choices = units1$Category)
})


output$unit_selection2 <- renderUI({
  units2 <- filter(crops, Crop.Type == input$crop2)
  
  selectInput("units2",
              NULL,
              choices = units2$Category)
})

output$cover_crop1_selection <- renderUI({
  cover_units1 <- filter(crops, Crop.Type == input$cct1)
  
  selectInput("c_units1",
              NULL,
              choices = cover_units1$Category)
})

output$cover_crop2_selection <- renderUI({
  cover_units2 <- filter(crops, Crop.Type == input$cct2)
  
  selectInput("c_units2",
              NULL,
              choices = cover_units2$Category)
})

output$fert_selection1 <- renderUI({
  fert_sel <- filter(fert, Fertilizer.N.Source == input$fertform1)
  
  selectInput("appMeth1",
              NULL,
              choices = fert_sel$Application.Method)
})

output$fert_selection2 <- renderUI({
  fert_sel <- filter(fert, Fertilizer.N.Source == input$fertform2)
  
  selectInput("appMeth2",
              NULL,
              choices = fert_sel$Application.Method)
})


output$plot1cover <- renderUI({
  if (input$cp1 == 1){
    column(6,
           strong("Cover Crop Type 1"),
           span(shiny::icon("info-circle"),
                id = "info_coverCropType1"),
           selectInput("cct1",
                       NULL,
                       choices = crops$Crop.Type,
                       selected = 1),
           tippy::tippy_this(elementId = "info_coverCropType1",
                             tooltip = "Cover Crops asdfasdfasdf",
                             placement = "right"
           ),
           strong("Cover Crop Units 1"),
           span(shiny::icon("info-circle"),
                id = "info_coverCropunit1"),
           uiOutput("cover_crop1_selection"),
           
           
           strong("Cover Crop Yield 1"),
           span(shiny::icon("info-circle"),
                id = "info_coverCropYield1"),
           numericInput("ccy1",
                        NULL,
                        value = 0,
                        min = 0),
           tippy::tippy_this(elementId = "info_coverCropYield1",
                             tooltip = "Cover Crops asdfasdfasdf",
                             placement = "right")
    )
  }else{
    NULL
  }
  
})

output$plot2cover <- renderUI({
  if (input$cp2 == 1){
    column(6,
           strong("Cover Crop Type 2"),
           span(shiny::icon("info-circle"),
                id = "info_coverCropType2"),
           selectInput("cct2",
                       NULL,
                       choices = crops$Crop.Type,
                       selected = 1),
           tippy::tippy_this(elementId = "info_coverCropType2",
                             tooltip = "Cover Crops asdfasdfasdf",
                             placement = "right"
           ),
           strong("Cover Crop Units 2"),
           span(shiny::icon("info-circle"),
                id = "info_coverCropunit2"),
           uiOutput("cover_crop2_selection"),
           
           strong("Cover Crop Yield 2"),
           span(shiny::icon("info-circle"),
                id = "info_coverCropYield2"),
           numericInput("ccy2",
                        NULL,
                        value = NULL,
                        min = 0),
           
           tippy::tippy_this(elementId = "info_coverCropYield2",
                             tooltip = "Cover Crops asdfasdfasdf",
                             placement = "right")
    )
  }else{
    NULL
  }
  
})

output$orgN_storage <- renderUI({
  orgN_out <- soil %>% filter(Organic.Matter.Content == input$soilType & Soil.Drainage.Classification == input$drainClass) %>%
    select(Organic.N.Storage)
  numericInput("orgN",
               NULL,
               value = orgN_out,
               min = 0)

})

output$irrigation_Qs <- renderUI({
  if (input$irr == 1){
    fluidRow(
      column(6,
             strong("Nitrate-N Concention (mg/L)"),
             span(shiny::icon("info-circle"),
                  id = "info_irri"),
             numericInput("irri",
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
             numericInput("irrInch",
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

output$rhot1 <- renderRHandsontable({
  
  N_Content1 <- crops %>%
    filter(Crop.Type == input$crop1 & Category == input$units1) %>%
    select(Avg.N.harvested..lb.N.unit.)
  
  N_Content2 <- crops %>%
    filter(Crop.Type == input$crop2 & Category == input$units2) %>%
    select(Avg.N.harvested..lb.N.unit.)
  recharge <- soil %>%
    filter(Organic.Matter.Content == input$soilType & Soil.Drainage.Classification == input$drainClass) %>%
    select(Inches.Recharge)
  if (input$irr == 1){
    irr_in <- input$irrInch
  } else{
    irr_in <- 0 
  }
  
  
  cop_info <- matrix(c("Scenario 1", input$crop1, N_Content1, input$yield1, input$acres1, input$soilType, input$drainClass, recharge, irr_in,
                       "Scenario 2", input$crop2, N_Content2, input$yield2, input$acres2, input$soilType, input$drainClass, recharge, irr_in), 
                     nrow = 9, ncol = 2, 
                     dimnames = list(c("","Crop Type", "N Content", "Yield", "Acres", "Soil Org Matter %", "Soil Drainage", "Recharge Estimate (in.)", "Irrigation Applied (in.)")))
  rhandsontable(cop_info, 
                rowHeaderWidth = 240, 
                readOnly = TRUE)
})

output$rhot2 <- renderRHandsontable({
  
  if (input$irr == 1){
    irrigation <- round((input$irri * input$irrInch * 0.226), digits = 1)
  } else{
    irrigation <- 0 
  }
  
  ####Needed to calculate legume
  N_unit1 <- crops %>%
    filter(Crop.Type == input$crop1 & Category == input$units1) %>%
    select(N.Per.Unit)
  N_unit1 <- round(N_unit1, digits = 1)
  harvMat1 <- N_unit1 * input$yield1
  legume1 <- crops %>%
   filter(Crop.Type == input$crop1 & Category == input$units1) %>%
   select(Legume)
  
  leg1_est <- (legume1*harvMat1)+0.5*(legume1*harvMat1)
  ######
  
  ####Needed to calculate legume
  N_unit2 <- crops %>%
    filter(Crop.Type == input$crop2 & Category == input$units2) %>%
    select(N.Per.Unit)
  N_unit2 <- round(N_unit2, digits = 1)
  harvMat2 <- N_unit2 * input$yield2
  legume2 <- crops %>%
    filter(Crop.Type == input$crop2 & Category == input$units2) %>%
    select(Legume)
  
  leg2_est <- (legume2*harvMat2)+0.5*(legume2*harvMat2)
  ######
  
  precipitation <- round((input$precip * input$InchPre * 0.226), digits = 1) 
  
  cropSeed1 <- 0
  cropSeed2 <- 0
  
  nonsymbioticFixation1 <- 3
  nonsymbioticFixation2 <- 3
  
  inputTotal1 <- input$fert1 + input$man1 + leg1_est + irrigation + precipitation + precipitation + cropSeed1 + nonsymbioticFixation1
  inputTotal1 <- round(inputTotal1, digits = 1)
  inputTotal2 <- input$fert2 + input$man2 + leg2_est + irrigation + precipitation + precipitation + cropSeed2 + nonsymbioticFixation2
  inputTotal2 <- round(inputTotal2, digits = 1)
  
  cop_info <- matrix(c("Scenario 1", input$fert1, input$man1, leg1_est, irrigation, precipitation, precipitation, 0, 3, inputTotal1, 
                       "Scenario 2", input$fert2, input$man2, leg2_est, irrigation, precipitation, precipitation, 0, 3, inputTotal2), 
                     nrow = 10, ncol = 2, 
                     dimnames = list(c("","Fertilizer", "Manure", "Symbiotic N fixation (legumes)", "Irrigation", "Precipitation", "Dry Deposition", "Crop Seed", "Nonsymbiotic Fixation", "Total N Input Per Acre")))
  rhandsontable(cop_info, 
                rowHeaderWidth = 240, 
                readOnly = TRUE)
})

output$rhot3 <- renderRHandsontable({
  
  N_unit1 <- crops %>%
    filter(Crop.Type == input$crop1 & Category == input$units1) %>%
    select(N.Per.Unit)
  N_unit1 <- round(N_unit1, digits = 1)
  
  harvMat1 <- N_unit1 * input$yield1
  
  legume1 <- crops %>%
    filter(Crop.Type == input$crop1 & Category == input$units1) %>%
    select(Legume)
  
  leg1_est <- (legume1*harvMat1)+0.5*(legume1*harvMat1)
  
  N_unit2 <- crops %>%
    filter(Crop.Type == input$crop2 & Category == input$units2) %>%
    select(N.Per.Unit)
  N_unit2 <- round(N_unit2, digits = 1)
  
  harvMat2 <- N_unit2 * input$yield2
  
  legume2 <- crops %>%
    filter(Crop.Type == input$crop2 & Category == input$units2) %>%
    select(Legume)
  
  leg2_est <- (legume2*harvMat2)+0.5*(legume2*harvMat2)

  
  perLoss1 <- fert %>%
    filter(Fertilizer.N.Source == input$fertform1 & Application.Method == input$appMeth1) %>%
    select(Percent.Lost)
  perLoss1 <- round(perLoss1, digits = 1)
  perLoss2 <- fert %>%
    filter(Fertilizer.N.Source == input$fertform2 & Application.Method == input$appMeth2) %>%
    select(Percent.Lost)
  perLoss2 <- round(perLoss2, digits = 1)
  
  ammLoss1 <- input$fert1 * (perLoss1/100)
  ammLoss1 <- round(ammLoss1, digits = 1)
  ammLoss2 <- input$fert2 * (perLoss2/100)
  ammLoss2 <- round(ammLoss2, digits = 1)
  
  if (input$irr == 1){
    irrigation <- input$irri * input$irrInch * 0.226
  } else{
    irrigation <- 0
  }
  
  cropSeed1 <- 0
  cropSeed2 <- 0
  
  nonsymbioticFixation1 <- 3
  nonsymbioticFixation2 <- 3
  
  precipitation <- input$precip * input$InchPre * 0.226
  
  inputTotal1 <- input$fert1 + input$man1 + leg1_est + irrigation + precipitation + precipitation + cropSeed1 + nonsymbioticFixation1
  inputTotal1 <- round(inputTotal1, digits = 1)
  inputTotal2 <- input$fert2 + input$man2 + leg2_est + irrigation + precipitation + precipitation + cropSeed2 + nonsymbioticFixation2
  inputTotal2 <- round(inputTotal2, digits = 1)
  
  perInoDent <- soil %>%
    filter(Organic.Matter.Content == input$soilType & Soil.Drainage.Classification == input$drainClass) %>%
    select(Percent.of.inorganic.denitrified)
  
  
  
  dent1 <- (input$fert1 + precipitation + precipitation + irrigation) * (perInoDent / 100)
  dent1 <- round(dent1, digits = 1)
  dent2 <- (input$fert2 + precipitation + precipitation + irrigation) * (perInoDent / 100)
  dent2 <- round(dent2, digits = 1)
  
  erosion1 <- 0
  erosion2 <- 0
  
  runoff1 <- 0
  runoff2 <- 0
  
  misgas1 <- inputTotal1 * 0.01
  misgas1 <- round(misgas1, digits = 1)
  misgas2 <- inputTotal2 * 0.01
  misgas2 <- round(misgas2, digits = 1)
  
  ammAtSen1 <- harvMat1 * 0.05
  ammAtSen2 <- harvMat2 * 0.05
  
  totalOutputs1 <- harvMat1 + ammLoss1 + dent1 + erosion1 + runoff1 + misgas1 + ammAtSen1
  totalOutputs1 <- round(totalOutputs1, digits = 1)
  totalOutputs2 <- harvMat2 + ammLoss2 + dent2 + erosion2 + runoff2 + misgas2 + ammAtSen2
  totalOutputs2 <- round(totalOutputs2, digits = 1)
  
  cop_info <- matrix(c("Scenario 1", harvMat1, ammLoss1, dent1, erosion1, runoff1, misgas1, ammAtSen1, totalOutputs1,
                       "Scenario 2", harvMat2, ammLoss2, dent2, erosion2, runoff2, misgas2, ammAtSen2, totalOutputs2),
                     nrow = 9, ncol = 2, 
                     dimnames = list(c("","Harvested material (Main Crop)", "Ammonia Loss", "Denitrification", "Erosion", "Runoff", "Miscellaneous Gaseous", "Ammonia at Senescence", "Total N Output Per Acre")))
  rhandsontable(cop_info, 
                rowHeaderWidth = 240, 
                readOnly = TRUE)
})

output$rhot4 <- renderRHandsontable({
  
  total_change_N <- input$inoN + input$orgN
  
  cop_info <- matrix(c("Scenario 1", input$inoN, input$orgN, total_change_N,
                       "Scenario 2", input$inoN, input$orgN, total_change_N), 
                     nrow = 4, ncol = 2, 
                     dimnames = list(c("", "Change in Inorganic N", "Change in Organic N", "Total Storage Change")))
  rhandsontable(cop_info, 
                rowHeaderWidth = 240, 
                readOnly = TRUE)
})

output$rhot5 <- renderRHandsontable({
  
  if(input$cp1 == 0){
    cct1 <- 0
    c_units1 <- 0
    ccy1 <- 0
  } else{
    cct1 <- input$cct1
    c_units1 <- crops %>%
      filter(Crop.Type == input$cct1 & Category == input$c_units1) %>%
      select(Avg.N.harvested..lb.N.unit.)
    ccy1 <- input$ccy1
  }
  
  if(input$cp2 == 0){
    cct2 <- 0
    c_units2 <- 0
    ccy2 <- 0
  } else{
    cct2 <- input$cct2
    c_units2 <- crops %>%
      filter(Crop.Type == input$cct2 & Category == input$c_units2) %>%
      select(Avg.N.harvested..lb.N.unit.)
    ccy2 <- input$ccy2
  }
  
  if(c_units1 == 0){
    n_per_unit1 <- 0
  } else{
    n_per_unit1 <- crops %>%
      filter(Crop.Type == input$cct1 & Category == input$c_units1) %>%
      select(N.Per.Unit)
  }
  
  if(c_units2 == 0){
    n_per_unit2 <- 0
  } else{
    n_per_unit2 <- crops %>%
      filter(Crop.Type == input$cct2 & Category == input$c_units2) %>%
      select(N.Per.Unit)
  }
  
  totCovCropRes1 <- ccy1 * n_per_unit1
  totCovCropRes2 <- ccy2 * n_per_unit2
  
  cop_info <- matrix(c("Scenario 1", cct1, c_units1, ccy1, totCovCropRes1, "Scenario 2", cct2, c_units2, ccy2, totCovCropRes2), 
                     nrow = 5, ncol = 2, 
                     dimnames = list(c("","Crop Type", "N Content", "Yield", "Total Cover Crop Residue")))
  rhandsontable(cop_info, 
                rowHeaderWidth = 240, 
                readOnly = TRUE)
})