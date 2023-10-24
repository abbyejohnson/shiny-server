# setwd("G:/usr/Nitrate_Trends/WI_Nitrate_Shiny_App")
# setwd("G:\\Nitrate_Trends\\WI_Nitrate_Shiny_App")

list.of.packages<-c("plyr", "dplyr", "data.table")
new.packages<-list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("plyr")
library("dplyr")
library("data.table")


#Bring in the current master data files
#If you want to save old master data files, copy these and place them elsewhere before running
  tnc <-read.csv("master_tnc.csv", header=TRUE)
  otm <-read.csv("master_otm.csv", header=TRUE)
  muni <-read.csv("master_muni.csv", header=TRUE)
  ntnc <-read.csv("master_ntnc.csv", header=TRUE)

  
#Bring in new files that contain the new data
  #Copy and paste the new data into the 'WI_Nitrate_Trend_Shiny_App' folder and then rename them to "newdata_xxx.csv". "xxx" being the well type
    # tnc2 <-read.csv("newdata_tnc.csv", header=TRUE)
    # otm2 <-read.csv("newdata_otm.csv", header=TRUE)
    # muni2 <-read.csv("newdata_muni.csv", header=TRUE)
    # ntnc2 <-read.csv("newdata_ntnc.csv", header=TRUE)

    
# This is for when you read in one csv file versus 4 indivudual ones
  # Read in new data as one data frame
  full <- read.csv("New_Nitrate_Data.csv", header = TRUE, fileEncoding="UTF-8-BOM")


# Need to give fake code for Nitrate_Trends_Data_Cleaning
  # Should probably explain to Kevin
  full$County.code <- 0
  full$Well.name <- 'Filler'
  full$Land.surface.elevation <- 0
  full$Water.formation <- NA
  
    
# Splitting the data frame into 4 separate uses
  tnc2 <- subset(full, Well.Use == 'TRANSIENT NONCOMMUNITY')
  otm2 <- subset(full, Well.Use == 'OTHER-THAN-MUNICIPAL COMMUNITY')
  muni2 <- subset(full, Well.Use == 'MUNICIPAL COMMUNITY')
  ntnc2 <- subset(full, Well.Use == 'NONTRANSIENT NONCOMMUNITY')

  
# Change how the well use variables are represented
  tnc2$Well.Use <- 'TRANSIENT NON-COMMUNITY'
  otm2$Well.Use <- 'COMMUNITY -- OTM'
  muni2$Well.Use <- 'COMMUNITY MUNICIPALITY'
  ntnc2$Well.Use <- 'NON-TRANSIENT NON-COMMUNITY'
        
    
#This is making the assumption that all columns in the data are named the same thing
#Changing variable names to match the master data set
    tnc3 <- setnames(tnc2 ,  old=c("WI.Unique.Well..", "County", "Well.Use", "Watershed", "Sample.Collection.Date", "Labslip.....Sample.ID",
                                   "Sample.Analytical.Qualifier", "Sample.Analytical.Result.Amount", "Units", "Limit.of.Detection", "Limit.of.Quantitation",
                                   "Storet.Parameter.Code", "Storet.Parameter.Description", "Well.Bottom..ft..", "Bedrock.Depth..ft..", "Casing.Bottom..ft..",
                                   "Static.Water.Level..ft..", "Well.Status", "Preventive.Action.Limit") , 
                     
                             new=c("WI.unique.well..", "County.name", "Well.use.description", "Watershed.code", "Sample.date", "Sample.ID",
                                   "Result.qualifier.description", "Result.amount", "Result.units", "LOD", "LOQ", "Storet.parameter.code",
                                   "Storet.parameter.description", "Well.depth", "Bedrock.depth", "Casing.depth", "Static.water.level", 
                                   "Well.status", "Preventative.Action.Limit"), skip_absent = TRUE)
  
    otm3 <- setnames(otm2 ,  old=c("WI.Unique.Well..", "County", "Well.Use", "Watershed", "Sample.Collection.Date", "Labslip.....Sample.ID",
                                   "Sample.Analytical.Qualifier", "Sample.Analytical.Result.Amount", "Units", "Limit.of.Detection", "Limit.of.Quantitation",
                                   "Storet.Parameter.Code", "Storet.Parameter.Description", "Well.Bottom..ft..", "Bedrock.Depth..ft..", "Casing.Bottom..ft..",
                                   "Static.Water.Level..ft..", "Well.Status", "Preventive.Action.Limit") , 
                     
                             new=c("WI.unique.well..", "County.name", "Well.use.description", "Watershed.code", "Sample.date", "Sample.ID",
                                   "Result.qualifier.description", "Result.amount", "Result.units", "LOD", "LOQ", "Storet.parameter.code",
                                   "Storet.parameter.description", "Well.depth", "Bedrock.depth", "Casing.depth", "Static.water.level", 
                                   "Well.status", "Preventative.Action.Limit"), skip_absent = TRUE)
  
    muni3 <- setnames(muni2 , old=c("WI.Unique.Well..", "County", "Well.Use", "Watershed", "Sample.Collection.Date", "Labslip.....Sample.ID",
                                    "Sample.Analytical.Qualifier", "Sample.Analytical.Result.Amount", "Units", "Limit.of.Detection", "Limit.of.Quantitation",
                                    "Storet.Parameter.Code", "Storet.Parameter.Description", "Well.Bottom..ft..", "Bedrock.Depth..ft..", "Casing.Bottom..ft..",
                                    "Static.Water.Level..ft..", "Well.Status", "Preventive.Action.Limit") , 
                             
                              new=c("WI.unique.well..", "County.name", "Well.use.description", "Watershed.code", "Sample.date", "Sample.ID",
                                    "Result.qualifier.description", "Result.amount", "Result.units", "LOD", "LOQ", "Storet.parameter.code",
                                    "Storet.parameter.description", "Well.depth", "Bedrock.depth", "Casing.depth", "Static.water.level", 
                                    "Well.status", "Preventative.Action.Limit"), skip_absent = TRUE)
  
    ntnc3 <- setnames(ntnc2 , old=c("WI.Unique.Well..", "County", "Well.Use", "Watershed", "Sample.Collection.Date", "Labslip.....Sample.ID",
                                    "Sample.Analytical.Qualifier", "Sample.Analytical.Result.Amount", "Units", "Limit.of.Detection", "Limit.of.Quantitation",
                                    "Storet.Parameter.Code", "Storet.Parameter.Description", "Well.Bottom..ft..", "Bedrock.Depth..ft..", "Casing.Bottom..ft..",
                                    "Static.Water.Level..ft..", "Well.Status", "Preventive.Action.Limit") , 
                     
                              new=c("WI.unique.well..", "County.name", "Well.use.description", "Watershed.code", "Sample.date", "Sample.ID",
                                    "Result.qualifier.description", "Result.amount", "Result.units", "LOD", "LOQ", "Storet.parameter.code",
                                    "Storet.parameter.description", "Well.depth", "Bedrock.depth", "Casing.depth", "Static.water.level", 
                                    "Well.status", "Preventative.Action.Limit"), skip_absent = TRUE)
  
    
    tnc4 <- tnc3 %>% select(WI.unique.well.., County.code, County.name, Well.name, Well.use.description, Well.depth, Bedrock.depth, Casing.depth, Land.surface.elevation, 
                            Static.water.level, Water.formation, Watershed.code, Well.status, Municipality, Sample.date, Sample.ID, Storet.parameter.code, Storet.parameter.description,
                            Result.qualifier.description, Result.amount, Result.units, LOD, LOQ, Enforcement.Standard, Preventative.Action.Limit)
    
    otm4 <- otm3 %>% select(WI.unique.well.., County.code, County.name, Well.name, Well.use.description, Well.depth, Bedrock.depth, Casing.depth, Land.surface.elevation, 
                            Static.water.level, Water.formation, Watershed.code, Well.status, Municipality, Sample.date, Sample.ID, Storet.parameter.code, Storet.parameter.description,
                            Result.qualifier.description, Result.amount, Result.units, LOD, LOQ, Enforcement.Standard, Preventative.Action.Limit)
    
    muni4 <- muni3 %>% select(WI.unique.well.., County.code, County.name, Well.name, Well.use.description, Well.depth, Bedrock.depth, Casing.depth, Land.surface.elevation, 
                            Static.water.level, Water.formation, Watershed.code, Well.status, Municipality, Sample.date, Sample.ID, Storet.parameter.code, Storet.parameter.description,
                            Result.qualifier.description, Result.amount, Result.units, LOD, LOQ, Enforcement.Standard, Preventative.Action.Limit)
    
    ntnc4 <- ntnc3 %>% select(WI.unique.well.., County.code, County.name, Well.name, Well.use.description, Well.depth, Bedrock.depth, Casing.depth, Land.surface.elevation, 
                            Static.water.level, Water.formation, Watershed.code, Well.status, Municipality, Sample.date, Sample.ID, Storet.parameter.code, Storet.parameter.description,
                            Result.qualifier.description, Result.amount, Result.units, LOD, LOQ, Enforcement.Standard, Preventative.Action.Limit)
  
#Combining the new data with the master data
    common_cols <- intersect(colnames(tnc), colnames(tnc4))
    tnc5 <- rbind(
      subset(tnc, select = common_cols), 
      subset(tnc4, select = common_cols))
    
    common_cols <- intersect(colnames(otm), colnames(otm4))
    otm5 <- rbind(
      subset(otm, select = common_cols), 
      subset(otm4, select = common_cols))
    
    common_cols <- intersect(colnames(muni), colnames(muni4))
    muni5 <- rbind(
      subset(muni, select = common_cols), 
      subset(muni4, select = common_cols))
    
    common_cols <- intersect(colnames(ntnc), colnames(ntnc4))
    ntnc5 <- rbind(
      subset(ntnc, select = common_cols), 
      subset(ntnc4, select = common_cols))

  
# #Getting rid of rows that are not needed
#   tnc3 <- tnc2 %>% select(-Preventive.Action.Limit , -Well.Status,
#                         -Bedrock.Depth..ft.. , -Casing.Diameter..in.. , -Casing.Bottom..ft.. , -Static.Water.Level..ft.. , -Well.Bottom..ft.. ,
#                         -Inventory.Date , -FID..PWS.ID. , -Range.Direction , -Range , -Township , -Section)
#   
#   otm4 <- otm3 %>% select(-Preventive.Action.Limit , -Well.Status , -Well.Status.Code , -Well.Use.Code , -Water.Formation , -Land.Surface.Elevation ,
#                         -Bedrock.Depth..ft.. , -Casing.Diameter..in.. , -Casing.Bottom..ft.. , -Static.Water.Level..ft.. , -Well.Bottom..ft.. ,
#                         -Inventory.Date , -FID..PWS.ID. , -Range.Direction , -Range , -Township , -Section)
#   
#   muni4 <- muni3 %>% select(-Preventive.Action.Limit , -Well.Status , -Well.Status.Code , -Well.Use.Code , -Water.Formation , -Land.Surface.Elevation ,
#                         -Bedrock.Depth..ft.. , -Casing.Diameter..in.. , -Casing.Bottom..ft.. , -Static.Water.Level..ft.. , -Well.Bottom..ft.. ,
#                         -Inventory.Date , -FID..PWS.ID. , -Range.Direction , -Range , -Township , -Section)
#   
#   ntnc4 <- ntnc3 %>% select(-Preventive.Action.Limit , -Well.Status , -Well.Status.Code , -Well.Use.Code , -Water.Formation , -Land.Surface.Elevation ,
#                         -Bedrock.Depth..ft.. , -Casing.Diameter..in.. , -Casing.Bottom..ft.. , -Static.Water.Level..ft.. , -Well.Bottom..ft.. ,
#                         -Inventory.Date , -FID..PWS.ID. , -Range.Direction , -Range , -Township , -Section)
  
  
#Save data to Folder. This will write over the old master file. 
  write.csv(tnc5, "new_master_tnc.csv", row.names = FALSE)
  write.csv(otm5, "new_master_otm.csv", row.names = FALSE)
  write.csv(muni5, "new_master_muni.csv", row.names = FALSE)
  write.csv(ntnc5, "new_master_ntnc.csv", row.names = FALSE)
  
  
#################
# Delete these copies of "newdata" files from this folder
  
# Run the "Nitrate_Trends_Data_Cleaning" script