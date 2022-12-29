rm(list = ls())

library(tidyverse)
library(readxl)
library(xlsx)

#Read in Data
dataDir <- "//uwsp.edu/files/CNR/Projects/MuniDashboard/2014 Survey Data/"
inputData <- read_excel(paste(dataDir, "Dashboard2014MuniData.xlsx", sep = ""))

#Remove sorting character from PopulationGroup
inputData <- inputData %>% mutate(PopulationGroup = replace(PopulationGroup, PopulationGroup == 'a 2,500 to 4,999', '2,500 to 4,999'))
inputData <- inputData %>% mutate(PopulationGroup = replace(PopulationGroup, PopulationGroup == 'b 5,000 to 9,999', '5,000 to 9,999'))
inputData <- inputData %>% mutate(PopulationGroup = replace(PopulationGroup, PopulationGroup == 'c 10,000 to 24,999', '10,000 to 24,999'))
inputData <- inputData %>% mutate(PopulationGroup = replace(PopulationGroup, PopulationGroup == 'd 25,000 to 49,999', '25,000 to 49,999'))
inputData <- inputData %>% mutate(PopulationGroup = replace(PopulationGroup, PopulationGroup == 'e 50,000 to 99,999', '50,000 to 99,999'))
inputData <- inputData %>% mutate(PopulationGroup = replace(PopulationGroup, PopulationGroup == 'f 100,000 to 249,999', '100,000 to 249,999'))
inputData <- inputData %>% mutate(PopulationGroup = replace(PopulationGroup, PopulationGroup == 'g 250,000 to 499,000', '250,000 to 499,999'))
inputData <- inputData %>% mutate(PopulationGroup = replace(PopulationGroup, PopulationGroup == 'h 500,000 to 999,999', '500,000 to 999,999'))
inputData <- inputData %>% mutate(PopulationGroup = replace(PopulationGroup, PopulationGroup == 'i 1,000,000 +', '1,000,000 +'))

#Create a copy of inputData for data manipulation
tempData <- inputData

#Create a final dataset to eventually be exported
#Filter to only the identification variables
data <- inputData %>% select(FIPS, CommunityName, StateAbbr, Region, Population, PopulationGroup)

#Sort PopulationGroup into order by population
data <- data %>% mutate(PopulationGroupOrder = if_else(PopulationGroup == '2,500 to 4,999', 1,
                                                       if_else(PopulationGroup == '5,000 to 9,999', 2,
                                                               if_else(PopulationGroup == '10,000 to 24,999', 3,
                                                                       if_else(PopulationGroup == '25,000 to 49,999', 4,
                                                                               if_else(PopulationGroup == '50,000 to 99,999', 5,
                                                                                       if_else(PopulationGroup == '100,000 to 249,999', 6,
                                                                                               if_else(PopulationGroup == '250,000 to 499,999', 7,
                                                                                                       if_else(PopulationGroup == '500,000 to 999,999', 8,
                                                                                                               if_else(PopulationGroup == '1,000,000 +', 9, 99))))))))))

#Abbreviate PopulationGroup variable for better graphic display
data <- data %>% mutate(PopulationGroup = replace(PopulationGroup, PopulationGroup == '2,500 to 4,999', '2.5-4.9k'))
data <- data %>% mutate(PopulationGroup = replace(PopulationGroup, PopulationGroup == '5,000 to 9,999', '5-9.9k'))
data <- data %>% mutate(PopulationGroup = replace(PopulationGroup, PopulationGroup == '10,000 to 24,999', '10-24.9k'))
data <- data %>% mutate(PopulationGroup = replace(PopulationGroup, PopulationGroup == '25,000 to 49,999', '25-49.9k'))
data <- data %>% mutate(PopulationGroup = replace(PopulationGroup, PopulationGroup == '50,000 to 99,999', '50-99.9k'))
data <- data %>% mutate(PopulationGroup = replace(PopulationGroup, PopulationGroup == '100,000 to 249,999', '100-249.9k'))
data <- data %>% mutate(PopulationGroup = replace(PopulationGroup, PopulationGroup == '250,000 to 499,999', '250-499.9k'))
data <- data %>% mutate(PopulationGroup = replace(PopulationGroup, PopulationGroup == '500,000 to 999,999', '500-999.9k'))
data <- data %>% mutate(PopulationGroup = replace(PopulationGroup, PopulationGroup == '1,000,000 +', '1,000k +'))

#Sort RegionOrder into order by Region, order is arbitrary, but we want everything in the same order every time
data <- data %>% mutate(RegionOrder = if_else(Region == 'Midwest', 2,
                                              if_else(Region == 'Northeast', 3,
                                                      if_else(Region == 'South', 4,
                                                              if_else(Region == 'West', 5, 99)))))

#Calculating Indicators
#### Budget and Adequacy ####
dollarsPerCapita <- (inputData$'2_3TotTreBud14'/inputData$Population)
percentOfMuniBud <- (inputData$'2_3TotTreBud14'/inputData$'2_1TotBud14') * 100
dollarsPerPublicTree <- (inputData$'2_3TotTreBud14'/inputData$'6_18.PubTr')
budgetNeeds <- if_else(inputData$'2_10CurrNeeds' == 'Y',100, inputData$'2_10%Need')

raPlan  <- inputData$'2_13TrPlan'
raPrun  <- inputData$'2_13TrPrun'
raRem   <- inputData$'2_13TrRem'
raAdmin <- inputData$'2_13Admin' + inputData$'2_13Super'
raOther <- ( inputData$'2_13OffExp' + inputData$'2_13Inven' + inputData$'2_13Empl' + 
               inputData$'2_13PHCare' + inputData$'2_13Fertl' + inputData$'2_13Water' + 
               inputData$'2_13StrmWr' + inputData$'2_13TrRep' + inputData$'2_13StmpRem' + 
               inputData$'2_13Nurse' + inputData$'2_13SafeTr' + inputData$'2_13PubEd' + inputData$'2_13Other')

publicTreeAssetValue <- inputData$'6_21ValPubTr'
publicTreeAssetInd <- ifelse((inputData$'6_21ValPubTr' > 0) & (inputData$'6_22FinRec' == "Y"), "Y", "N")

#Add the indicators to the dataset
data <- mutate(data, dollarsPerCapita, percentOfMuniBud, dollarsPerPublicTree, budgetNeeds, raPlan, raPrun, raRem, raAdmin, raOther, publicTreeAssetValue, publicTreeAssetInd)

#### Planning and Management ####
treeBoard <- ifelse((inputData$'3_2AuthTrBd' == 'Y'), 100, 0)
ordinance <- if_else((inputData$'3_5MuniOrd' == 1), 100, if_else((inputData$'3_5MuniOrd' == 2), 0, NULL))
ordinanceYear <- if_else((inputData$'3_5YrUpdt' >=  2007), 100, 0) 
writtenStratPlan <- if_else((inputData$'3_9WrtStgPln' == 1), 100, if_else((inputData$'3_9WrtStgPln' == 2), 0, NULL))

#writtenStraPlanUpToDate <- If any of 3_10 are within the last 5 years then yes
tempData$'3_10CityMasYr'    <- ifelse((inputData$'3_10CityMasYr' >= 2009), TRUE, FALSE)
tempData$'3_10InsDsReadDYr'  <- ifelse((inputData$'3_10InsDsReadDYr' >= 2009), TRUE, FALSE)
tempData$'3_10MunWtrYr'     <- ifelse((inputData$'3_10MunWtrYr' >= 2009), TRUE, FALSE)
tempData$'3_10StrEmerDevYr'    <- ifelse((inputData$'3_10StrEmerDevYr' >= 2009), TRUE, FALSE)
tempData$'3_10StrWtrMYr'    <- ifelse((inputData$'3_10StrWtrMYr' >= 2009), TRUE, FALSE)
tempData$'3_10TrRskMYr'     <- ifelse((inputData$'3_10TrRskMYr' >= 2009), TRUE, FALSE)
tempData$'3_10UrbForMgmtYr' <- ifelse((inputData$'3_10UrbForMgmtYr' >= 2009), TRUE, FALSE)
tempData$'3_10UrbForStrYr'  <- ifelse((inputData$'3_10UrbForStrYr' >= 2009), TRUE, FALSE)

writtenStratPlanUpToDate <- tempData$'3_10CityMasYr'|tempData$'3_10InsDsReadDYr'|tempData$'3_10MunWtrYr'|tempData$'3_10StrEmerDevYr'|tempData$'3_10StrWtrMYr'|tempData$'3_10TrRskMYr'|tempData$'3_10UrbForMgmtYr'|tempData$'3_10UrbForStrYr'
writtenStratPlanUpToDate <- ifelse((writtenStratPlanUpToDate), 100, 0)

#Add the indicators to the dataset
data <- mutate(data, treeBoard, ordinance, ordinanceYear, writtenStratPlan, writtenStratPlanUpToDate)

#### Tree Resource Management ####
treeResourceInventoryExists <- if_else((inputData$'6_1TrInv' >= 0), 100, 0)
treeResourceInventory <- if_else((inputData$'6_3YrUpd' >= 2009), 100, 0)

canCovGoal <- if_else((inputData$'6_15CanGl' == 1), 100, 0)

percentCanGoal <- inputData$'6_16%CanGl'
percentCurCan <- inputData$'6_16%CurCn'
yearsToGoal <-  as.integer(inputData$'6_16YrToGl') - 2014
percentCanProgress <- (inputData$'6_16%CurCn' / inputData$'6_16%CanGl') * 100

#Calculating Total area of green space
sqMetersPerAcre <- 4046.86
greenSpaceAreaMeters <- (((inputData$'1_3AcMMP' * sqMetersPerAcre) + (inputData$'1_3AcMGS' * sqMetersPerAcre) + (inputData$'1_3AcMPWTr' * sqMetersPerAcre)) / inputData$Population)
sqFeetPerAcre <- 43560
greenSpaceAreaFeet <- (((inputData$'1_3AcMMP' * sqFeetPerAcre) + (inputData$'1_3AcMGS' * sqFeetPerAcre) + (inputData$'1_3AcMPWTr' * sqFeetPerAcre)) / inputData$Population)

currentPrunInsCyc <- inputData$'7_4CurCyc'
desiredPrunInsCyc <- inputData$'7_4DesCyc'

yearsOfPrunInsCyc <- (inputData$'7_4CurCyc' - inputData$'7_4DesCyc')
percentAttPrunInsCyc <- ((inputData$'7_4DesCyc' / inputData$'7_4CurCyc') * 100)

activeManagement <- if_else((inputData$'7_2SysSch' >= 40), 100, 0)

#Add the indicators to the dataset
data <- mutate(data, treeResourceInventoryExists, treeResourceInventory, canCovGoal, percentCanGoal, percentCurCan, yearsToGoal, percentCanProgress, greenSpaceAreaMeters, greenSpaceAreaFeet, currentPrunInsCyc, desiredPrunInsCyc, yearsOfPrunInsCyc, percentAttPrunInsCyc, activeManagement)

#### Tree Resource ####
#Tree diversity by top 6 trees has no survery questions tied to it
strTrStockingLevel <- ((inputData$'6_19StrTr' / (inputData$'6_19StrTr' + inputData$'6_20StrTr')) * 100)
prkTrStockingLevel <- ((inputData$'6_19PrkTr' / (inputData$'6_19PrkTr' + inputData$'6_20PrkTr')) * 100)
munTrStockingLevel <- ((inputData$'6_19MunTr' / (inputData$'6_19MunTr' + inputData$'6_20MunTr')) * 100)

pubStrTrDensity <- (inputData$'6_19StrTr' / inputData$'1_3MiMS')
manPubTrPerCap <- (inputData$'6_18.PubTr' / inputData$Population)
manPubTrPerEmp <- (inputData$'6_18.PubTr' / inputData$'1_15FullTiEq')

#Omit New York City from plantedTrStab because it skews the average
plantedTrStab <- if_else(data$CommunityName == 'New York', -.0001, inputData$'7_1TrPlnt' - inputData$'7_1TrRem')

#Add the indicators to the dataset
data <- mutate(data, strTrStockingLevel, prkTrStockingLevel, munTrStockingLevel, pubStrTrDensity, manPubTrPerCap, manPubTrPerEmp, plantedTrStab)
data <- data %>% mutate(plantedTrStab = na_if(plantedTrStab, -.0001))

#### Tree City USA ####
#Replace NA with -1
tempData$'3_2AuthTrBd' <- replace_na(inputData$'3_2AuthTrBd', '-1')
tempData$'3_1CityCo' <- replace_na(inputData$'3_1CityCo', '-1')
tempData$'3_1ParksBr' <- replace_na(inputData$'3_1ParksBr', '-1')
tempData$'3_1PubWr' <- replace_na(inputData$'3_1PubWr', '-1')
tempData$'3_1TrBoard' <- replace_na(inputData$'3_1TrBoard', '-1')
tempData$'3_1PlanCom' <- replace_na(inputData$'3_1PlanCom', '-1')
tempData$'3_1ShdeTr' <- replace_na(inputData$'3_1ShdeTr', '-1')
tempData$'3_1Other' <- replace_na(inputData$'3_1Other', '-1')


tcAdvisoryBoard <- if_else(tempData$'3_2AuthTrBd' == 'Y', 100,
                           if_else(tempData$'3_1CityCo' == 'Y' |
                                     tempData$'3_1ParksBr' == 'Y' |
                                     tempData$'3_1PubWr' == 'Y' |
                                     tempData$'3_1TrBoard' == 'Y' |
                                     tempData$'3_1PlanCom' == 'Y' |
                                     tempData$'3_1ShdeTr' == 'Y' |
                                     tempData$'3_1Other' == 'Y', 100,
                                   if_else(tempData$'3_2AuthTrBd' == 'N', 0,
                                           if_else(tempData$'3_1CityCo' == '-1' &
                                                     tempData$'3_1ParksBr' == '-1' &
                                                     tempData$'3_1PubWr' == '-1' &
                                                     tempData$'3_1TrBoard' == '-1' &
                                                     tempData$'3_1PlanCom' == '-1' &
                                                     tempData$'3_1ShdeTr' == '-1' &
                                                     tempData$'3_1Other' == '-1', -1,0))))

#Return NA
tempData <- mutate(tempData, tcAdvisoryBoard)
tempData <- tempData %>% mutate(tcAdvisoryBoard = na_if(tcAdvisoryBoard, -1))
data$tcAdvisoryBoard <- tempData$tcAdvisoryBoard

data$tcOrdinance <- if_else(inputData$'3_5MuniOrd' == 1, 100, 0)

data$tcBudgetValue <- (inputData$'2_3TotTreBud14' / inputData$Population)
data$tcBudget <- if_else(data$tcBudgetValue >= 2, 100,0)

#Replace NA with -1
tempData$'8_8TrCity' <- replace_na(inputData$'8_8TrCity', '-1')
tempData$'8_10ObvsP' <- replace_na(inputData$'8_10ObvsP', '-1')

tcArborDay <- if_else(tempData$'8_8TrCity' == 'Y' | tempData$'8_10ObvsP' == 'Y', 100,
                      if_else(tempData$'8_8TrCity' == 'N' | tempData$'8_10ObvsP' == 'N', 0,-1))
#Return NA
tempData <- mutate(tempData, tcArborDay)
tempData <- tempData %>% mutate(tcArborDay = na_if(tcArborDay, -1))
data$tcArborDay <- tempData$tcArborDay

data$tcScore <- (data$tcAdvisoryBoard + data$tcOrdinance + data$tcBudget + data$tcArborDay) / 4

#### Community Activity Reporting System (CARS) ####
#Replace NA with -1
tempData$'1_132yd' <- replace_na(inputData$'1_132yd', '-1')
tempData$'1_134yd' <- replace_na(inputData$'1_134yd', '-1')
tempData$'1_13Grad' <- replace_na(inputData$'1_13Grad', '-1')
tempData$'1_13ISACA' <- replace_na(inputData$'1_13ISACA', '-1')
tempData$'1_13ISACMS' <- replace_na(inputData$'1_13ISACMS', '-1')
tempData$'1_13ISAAC' <- replace_na(inputData$'1_13ISAAC', '-1')
tempData$'1_13SAFCF' <- replace_na(inputData$'1_13SAFCF', '-1')

carsStaff <- if_else(tempData$'1_132yd' == 'Y' | tempData$'1_134yd' == 'Y'| tempData$'1_13Grad' == 'Y', 100,
                     if_else(tempData$'1_13ISACA' == 'Y' | tempData$'1_13ISACMS' == 'Y'| tempData$'1_13ISAAC' == 'Y' | tempData$'1_13SAFCF' == 'Y', 100, 
                             if_else(tempData$'1_132yd' == '-1' & tempData$'1_134yd' == '-1' & tempData$'1_13Grad' == '-1', 
                                     if_else(tempData$'1_13ISACA' == '-1' & tempData$'1_13ISACMS' == '-1'& tempData$'1_13ISAAC' == '-1' & tempData$'1_13SAFCF' == '-1', -1, 0),0)))

#Return NA
tempData <- mutate(tempData, carsStaff)
tempData <- tempData %>% mutate(carsStaff = na_if(carsStaff, -1))
data$carsStaff <- tempData$carsStaff

#Replace NA with -1
tempData$'3_6RegTrPub' <- replace_na(inputData$'3_6RegTrPub', '-1')
tempData$'3_6RegTrPri' <- replace_na(inputData$'3_6RegTrPri', '-1')
tempData$'3_6ReqTrND' <- replace_na(inputData$'3_6ReqTrND', '-1')
tempData$'3_6ReqTrNP' <- replace_na(inputData$'3_6ReqTrNP', '-1')
tempData$'3_6ReqTrRP' <- replace_na(inputData$'3_6ReqTrRP', '-1')
tempData$'3_6ReqRpPOT' <- replace_na(inputData$'3_6ReqRpPOT', '-1')
tempData$'3_6RegPubTrMn' <- replace_na(inputData$'3_6RegPubTrMn', '-1')
tempData$'3_6ReqTyMn' <- replace_na(inputData$'3_6ReqTyMn', '-1')
tempData$'3_6RegRemDd' <- replace_na(inputData$'3_6RegRemDd', '-1')
tempData$'3_6EstIDCS' <- replace_na(inputData$'3_6EstIDCS', '-1')
tempData$'3_6DefTrReq' <- replace_na(inputData$'3_6DefTrReq', '-1')
tempData$'3_6PrhTrTop' <- replace_na(inputData$'3_6PrhTrTop', '-1')
tempData$'3_6ReqPreDev' <- replace_na(inputData$'3_6ReqPreDev', '-1')
tempData$'3_6ResTrCtPP' <- replace_na(inputData$'3_6ResTrCtPP', '-1')
tempData$'3_6RegAbat' <- replace_na(inputData$'3_6RegAbat', '-1')
tempData$'3_6IdPreHer' <- replace_na(inputData$'3_6IdPreHer', '-1')

carsOrdPlantHave <- if_else(tempData$'3_6RegTrPub' == 'Y' | 
                              tempData$'3_6RegTrPri' == 'Y'| 
                              tempData$'3_6ReqTrND' == 'Y' | 
                              tempData$'3_6ReqTrNP' == 'Y' | 
                              tempData$'3_6ReqTrRP' == 'Y' | 
                              tempData$'3_6ReqRpPOT' == 'Y', 100,
                            if_else(tempData$'3_6RegTrPub' == '-1' & 
                                      tempData$'3_6RegTrPri' == '-1'& 
                                      tempData$'3_6ReqTrND' == '-1' &
                                      tempData$'3_6ReqTrNP' == '-1' & 
                                      tempData$'3_6ReqTrRP' == '-1' & 
                                      tempData$'3_6ReqRpPOT' == '-1', -1, 0))

carsOrdProthave <- if_else(tempData$'3_6RegPubTrMn' == 'Y' | 
                             tempData$'3_6ReqTyMn' == 'Y' | 
                             tempData$'3_6RegRemDd' == 'Y' | 
                             tempData$'3_6EstIDCS' == 'Y' | 
                             tempData$'3_6DefTrReq' == 'Y' | 
                             tempData$'3_6PrhTrTop' == 'Y', 100,
                           if_else(tempData$'3_6RegPubTrMn' == '-1' &
                                     tempData$'3_6ReqTyMn' == '-1' & 
                                     tempData$'3_6RegRemDd' == '-1' & 
                                     tempData$'3_6EstIDCS' == '-1' & 
                                     tempData$'3_6DefTrReq' == '-1' & 
                                     tempData$'3_6PrhTrTop' == '-1', -1,0))

carsOrdMainHave <- if_else(tempData$'3_6ReqPreDev' == 'Y' | 
                             tempData$'3_6ResTrCtPP' == 'Y' | 
                             tempData$'3_6RegAbat' == 'Y' | 
                             tempData$'3_6IdPreHer' == 'Y', 100, 
                           if_else(tempData$'3_6ReqPreDev' == '-1' & 
                                     tempData$'3_6ResTrCtPP' == '-1' & 
                                     tempData$'3_6RegAbat' == '-1' & 
                                     tempData$'3_6IdPreHer' == '-1', -1, 0))
#This works fine because the only possible values for these fields are 100 or missing; getting a 0 would be impossible
carsOrdinance <- if_else((carsOrdPlantHave == 100 & carsOrdProthave == 100 & carsOrdMainHave == 100), 100, -1)
#Return NA
tempData <- mutate(tempData, carsOrdinance)
tempData <- tempData %>% mutate(carsOrdinance = na_if(carsOrdinance, -1))
data$carsOrdinance <- tempData$carsOrdinance

tempData <- mutate(tempData, carsOrdMainHave)
tempData <- tempData %>% mutate(carsOrdMainHave = na_if(carsOrdMainHave, -1))
data$carsOrdMainHave <- tempData$carsOrdMainHave

tempData <- mutate(tempData, carsOrdPlantHave)
tempData <- tempData %>% mutate(carsOrdPlantHave = na_if(carsOrdPlantHave, -1))
data$carsOrdPlantHave <- tempData$carsOrdPlantHave

tempData <- mutate(tempData, carsOrdProthave)
tempData <- tempData %>% mutate(carsOrdProthave = na_if(carsOrdProthave, -1))
data$carsOrdProthave <- tempData$carsOrdProthave

#Replace NA with -1
tempData$'8_8TrCity' <- replace_na(inputData$'8_8TrCity', '-1')
tempData$'3_1CityCo' <- replace_na(inputData$'3_1CityCo', '-1')
tempData$'3_1ParksBr' <- replace_na(inputData$'3_1ParksBr', '-1')
tempData$'3_1PubWr' <- replace_na(inputData$'3_1PubWr', '-1')
tempData$'3_1TrBoard' <- replace_na(inputData$'3_1TrBoard', '-1')
tempData$'3_1PlanCom' <- replace_na(inputData$'3_1PlanCom', '-1')
tempData$'3_1ShdeTr' <- replace_na(inputData$'3_1ShdeTr', '-1')
tempData$'3_1Other' <- replace_na(inputData$'3_1Other', '-1')
tempData$'3_2AuthTrBd' <- replace_na(inputData$'3_2AuthTrBd', '-1')

carsAdvocacy <- if_else(tempData$'8_8TrCity' == 'Y', 100,
                        if_else(tempData$'3_1CityCo' == 'Y' | 
                                  tempData$'3_1ParksBr' == 'Y' | 
                                  tempData$'3_1PubWr' == 'Y' | 
                                  tempData$'3_1TrBoard' == 'Y' | 
                                  tempData$'3_1PlanCom' == 'Y' | 
                                  tempData$'3_1ShdeTr' == 'Y' | 
                                  tempData$'3_1Other' == 'Y', 100,
                                if_else(tempData$'3_2AuthTrBd' == 'Y', 100, 
                                        if_else(tempData$'8_8TrCity' == '-1' &
                                                  tempData$'3_1CityCo' == '-1' & 
                                                  tempData$'3_1ParksBr' == '-1' & 
                                                  tempData$'3_1PubWr' == '-1' & 
                                                  tempData$'3_1TrBoard' == '-1' & 
                                                  tempData$'3_1PlanCom' == '-1' & 
                                                  tempData$'3_1ShdeTr' == '-1' & 
                                                  tempData$'3_1Other' == '-1'&
                                                  tempData$'3_2AuthTrBd' == '-1', -1, 0))))
#Return NA
tempData <- mutate(tempData, carsAdvocacy)
tempData <- tempData %>% mutate(carsAdvocacy = na_if(carsAdvocacy, -1))
data$carsAdvocacy <- tempData$carsAdvocacy

carsManPlan <- if_else(inputData$'3_9WrtStgPln' == 1, 1, 0)
carsManInv <- if_else(inputData$'6_1TrInv' == 'Y', 1, 0)
carsManagement <- if_else(carsManPlan == 1 & carsManInv == 1, 100, 0)

carsScore <- (carsStaff + carsOrdinance + carsAdvocacy + carsManagement) / 4

data <- mutate(data, carsManagement, carsScore)

#### Society of Municipal Foresters ####
smaStaff <- if_else(inputData$'1_13ISACA' == 'Y', 100, 0)
smaMasterPlan <- if_else(inputData$'3_9WrtStgPln' == 1, 100, 0)
smaStatus <- if_else(inputData$'8_8TrCity' == 'Y', 100, 0)
smaAward <- if_else(inputData$'8_4AwdCm' == 1, 100, 0)
smaPreference <- if_else(inputData$'5_4PrefTCI' == 'Y', 100, 0)

#Replace NA with -1
tempData$'5_4ANSIZ1' <- replace_na(inputData$'5_4ANSIZ1', '-1')
tempData$'5_4ANSIA' <- replace_na(inputData$'5_4ANSIA', '-1')
smaStandards <- if_else(tempData$'5_4ANSIZ1' == 'Y' & tempData$'5_4ANSIA' == 'Y', 100, 
                        if_else(tempData$'5_4ANSIZ1' == '-1' & tempData$'5_4ANSIA' == '-1', -1, 0))
#Return NA
tempData <- mutate(tempData, smaStandards)
tempData <- tempData %>% mutate(smaStandards = na_if(smaStandards, -1))
smaStandards <- tempData$smaStandards

smaScore <- (100 + smaStaff + smaMasterPlan + smaStatus + smaAward + smaPreference + smaStandards) / 7

#Add the indicators to the dataset
data <- mutate(data, smaStaff, smaMasterPlan, smaStatus, smaAward, smaPreference, smaStandards, smaScore)

#### Urban Forest Sustainability (Clark  and Matheny Model) ####
#### Resource Management ####
#Replace W with 1 for counting
tempData$'3_10CityMas'    <- ifelse((inputData$'3_10CityMas' == 'W'), 1, 0)
tempData$'3_10InsDsRead'  <- ifelse((inputData$'3_10InsDsRead' == 'W'), 1, 0)
tempData$'3_10MunWtr'     <- ifelse((inputData$'3_10MunWtr' == 'W'), 1, 0)
tempData$'3_10StrEmer'    <- ifelse((inputData$'3_10StrEmer' == 'W'), 1, 0)
tempData$'3_10StrWtrM'    <- ifelse((inputData$'3_10StrWtrM' == 'W'), 1, 0)
tempData$'3_10TrRskM'     <- ifelse((inputData$'3_10TrRskM' == 'W'), 1, 0)
tempData$'3_10UrbForMgmt' <- ifelse((inputData$'3_10UrbForMgmt' == 'W'), 1, 0)
tempData$'3_10UrbForStr'  <- ifelse((inputData$'3_10UrbForStr' == 'W'), 1, 0)
#Replace NA with 0.01
tempData$'3_10CityMas'    <- replace_na(tempData$'3_10CityMas', 0.01)
tempData$'3_10InsDsRead'  <- replace_na(tempData$'3_10InsDsRead', 0.01)
tempData$'3_10MunWtr'     <- replace_na(tempData$'3_10MunWtr', 0.01)
tempData$'3_10StrEmer'    <- replace_na(tempData$'3_10StrEmer', 0.01)
tempData$'3_10StrWtrM'    <- replace_na(tempData$'3_10StrWtrM', 0.01)
tempData$'3_10TrRskM'     <- replace_na(tempData$'3_10TrRskM', 0.01)
tempData$'3_10UrbForMgmt' <- replace_na(tempData$'3_10UrbForMgmt', 0.01)
tempData$'3_10UrbForStr'  <- replace_na(tempData$'3_10UrbForStr', 0.01)




count3_10 <- tempData$'3_10CityMas' +
  tempData$'3_10InsDsRead' +
  tempData$'3_10MunWtr' + 
  tempData$'3_10StrEmer' +
  tempData$'3_10StrWtrM' +
  tempData$'3_10TrRskM' +
  tempData$'3_10UrbForMgmt' +
  tempData$'3_10UrbForStr' 

tempData$'3_9WrtStgPln' <- replace_na(inputData$'3_9WrtStgPln', -1)

cmManagement <- if_else(tempData$'3_9WrtStgPln' == 2, 1, 
                        if_else(tempData$'3_9WrtStgPln' == 3, 1.5,
                                if_else(tempData$'3_9WrtStgPln' == 1,
                                        if_else(count3_10 == .08, -1,
                                                if_else(count3_10 >= 7, 4,
                                                        if_else(count3_10 >= 5, 3.5,
                                                                if_else(count3_10 >= 4, 3,
                                                                        if_else(count3_10 >= 1, 2.5, 2))))), 
                                        if_else(tempData$'3_9WrtStgPln' == -1, -1,-1))))
#Return NA
tempData <- mutate(tempData, cmManagement)
tempData <- tempData %>% mutate(cmManagement = na_if(cmManagement, -1))
data$cmManagement <- tempData$cmManagement

#Replace NA with -1
tempData$'2_10CurrNeeds' <- replace_na(inputData$'2_10CurrNeeds', '-1')
tempData$'7_2SysSch' <- replace_na(inputData$'7_2SysSch', -1)

cmFunding <- if_else(tempData$'2_10CurrNeeds' == 'Y',
                     if_else(tempData$'7_2SysSch' >= 70, 4,
                             if_else(tempData$'7_2SysSch' >= 40 & tempData$'7_2SysSch' < 70, 3,
                                     if_else(tempData$'7_2SysSch' == -1, -1, 2))),
                     if_else(tempData$'2_10CurrNeeds' == 'N',
                             if_else(tempData$'7_2SysSch' >= 40, 2,
                                     if_else(tempData$'7_2SysSch' == -1, -1, 1)),
                             if_else(tempData$'2_10CurrNeeds' == '-1', -1, -1)))
#Return NA
tempData <- mutate(tempData, cmFunding)
tempData <- tempData %>% mutate(cmFunding = na_if(cmFunding, -1))
data$cmFunding <- tempData$cmFunding

#Replace NA with -1
tempData$'1_132yd' <- replace_na(inputData$'1_132yd', '-1')
tempData$'1_134yd' <- replace_na(inputData$'1_134yd', '-1')
tempData$'1_13Grad' <- replace_na(inputData$'1_13Grad', '-1')
tempData$'1_13InHOrJ' <- replace_na(inputData$'1_13InHOrJ', '-1')
tempData$'1_13AttTree' <- replace_na(inputData$'1_13AttTree', '-1')
tempData$'1_6OvrcTr' <- replace_na(inputData$'1_6OvrcTr', -1)
tempData$'1_13ISACA' <- replace_na(inputData$'1_13ISACA', '-1')

cmStaffingDegree <- if_else(tempData$'1_132yd' == 'Y' |  tempData$'1_134yd' == 'Y' | tempData$'1_13Grad' == 'Y', 'Y',
                            if_else(tempData$'1_132yd' == '-1' &  tempData$'1_134yd' == '-1' & tempData$'1_13Grad' == '-1', '-1','N'))
cmStaffing <- if_else(tempData$'1_6OvrcTr' == 1,
                      if_else(tempData$'1_13ISACA' == 'Y' & cmStaffingDegree == 'Y', 4,
                              if_else(tempData$'1_13ISACA' == 'Y' | cmStaffingDegree == 'Y', 3,
                                      if_else(tempData$'1_13InHOrJ' == 'Y' | tempData$'1_13AttTree' == 'Y', 2.5, 2))),
                      if_else(tempData$'1_6OvrcTr' == 2, 1, -1))
#Return NA
tempData <- mutate(tempData, cmStaffing)
tempData <- tempData %>% mutate(cmStaffing = na_if(cmStaffing, -1))
data$cmStaffing <- tempData$cmStaffing

#Replace Y with 1 and N with 0 for counting
tempData$'6_6StrTr'  <- ifelse((inputData$'6_6StrTr' == 'Y'), 1, 0)
tempData$'6_6PrkTr'  <- ifelse((inputData$'6_6PrkTr' == 'Y'), 1, 0)
tempData$'6_6MunGr'  <- ifelse((inputData$'6_6MunGr' == 'Y'), 1, 0)
tempData$'6_6MunWd'  <- ifelse((inputData$'6_6MunWd' == 'Y'), 1, 0)
tempData$'6_6PriTr'  <- ifelse((inputData$'6_6PriTr' == 'Y'), 1, 0)
tempData$'6_6OthrMP' <- ifelse((inputData$'6_6OthrMP'== 'Y'), 1, 0)
#Replace NA with 0.01
tempData$'6_6StrTr'  <- replace_na(tempData$'6_6StrTr', 0.01)
tempData$'6_6PrkTr'  <- replace_na(tempData$'6_6PrkTr', 0.01)
tempData$'6_6MunGr'  <- replace_na(tempData$'6_6MunGr', 0.01)
tempData$'6_6MunWd'  <- replace_na(tempData$'6_6MunWd', 0.01)
tempData$'6_6PriTr'  <- replace_na(tempData$'6_6PriTr', 0.01)
tempData$'6_6OthrMP' <- replace_na(tempData$'6_6OthrMP', 0.01)

tempData$'6_1TrInv' <- replace_na(inputData$'6_1TrInv', '-1')
tempData$'6_8GPSGIS' <- replace_na(inputData$'6_8GPSGIS', '-1')

cmAssesmentTrees <- tempData$'6_6StrTr' +
  tempData$'6_6PrkTr'+
  tempData$'6_6MunGr'+
  tempData$'6_6MunWd' +
  tempData$'6_6PriTr' + 
  tempData$'6_6OthrMP'

cmAssessment <- if_else(tempData$'6_1TrInv' == 'Y',
                        if_else(cmAssesmentTrees >= 2, 
                                if_else(tempData$'6_8GPSGIS' == 'Y', 4,3),2),
                        if_else(tempData$'6_1TrInv' == '-1', -1, 1))
#Return NA
tempData <- mutate(tempData, cmAssessment)
tempData <- tempData %>% mutate(cmAssessment = na_if(cmAssessment, -1))
data$cmAssessment <- tempData$cmAssessment

#Replace Y with 1 and N with 0 for counting
tempData$'3_6ReqPreDev' <- ifelse((inputData$'3_6ReqPreDev' == 'Y'), 1, 0)
tempData$'3_6IdPreHer' <- ifelse((inputData$'3_6IdPreHer' == 'Y'), 1, 0)
tempData$'3_6PrhTrTop' <- ifelse((inputData$'3_6PrhTrTop' == 'Y'), 1, 0)
#Replace NA with 0.01
tempData$'3_6ReqPreDev'  <- replace_na(tempData$'3_6ReqPreDev', 0.01)
tempData$'3_6IdPreHer'  <- replace_na(tempData$'3_6IdPreHer', 0.01)
tempData$'3_6PrhTrTop'  <- replace_na(tempData$'3_6PrhTrTop', 0.01)

tempData$'3_5MuniOrd'  <- replace_na(inputData$'3_5MuniOrd', -1)

cmProtectionOrd <- tempData$'3_6ReqPreDev' +
  tempData$'3_6IdPreHer' + 
  tempData$'3_6PrhTrTop'

cmProtection <- if_else(tempData$'3_5MuniOrd' == 1, 
                        if_else(cmProtectionOrd >= 3, 4,
                                if_else(cmProtectionOrd >= 2, 3,
                                        if_else(cmProtectionOrd >= 1, 2, 1))),
                        if_else(tempData$'3_5MuniOrd' == -1, -1, 1))
#Return NA
tempData <- mutate(tempData, cmProtection)
tempData <- tempData %>% mutate(cmProtection = na_if(cmProtection, -1))
data$cmProtection <- tempData$cmProtection

#Replace NA
tempData$'6_13TrPlnt'  <- replace_na(inputData$'6_13TrPlnt', '-1')
tempData$'6_14SelTrS'  <- replace_na(inputData$'6_14SelTrS', '-1')

cmSelection <- if_else(tempData$'3_5MuniOrd' == 1,
                       if_else(tempData$'3_6RegTrPub' == 'Y' | tempData$'3_6RegTrPri' == 'Y',
                               if_else(tempData$'3_6RegTrPub' == 'Y' & tempData$'3_6RegTrPri' == 'Y',
                                       if_else(tempData$'6_13TrPlnt' == 'Y' | tempData$'6_14SelTrS' =='Y', 4,3.5),3),2),
                       if_else(tempData$'3_5MuniOrd' == -1, -1, 1))
#Return NA
tempData <- mutate(tempData, cmSelection)
tempData <- tempData %>% mutate(cmSelection = na_if(cmSelection, -1))
data$cmSelection <- tempData$cmSelection

#Numbers are stored as a character in the databse so convert to integer for counting
tempData$'3_13Ansi760'  <- ifelse((inputData$'3_13Ansi760' == '1'), 1, 0)
tempData$'3_13AnsiA300'  <- ifelse((inputData$'3_13AnsiA300' == '1'), 1, 0)
tempData$'3_13AnsiZ133'  <- ifelse((inputData$'3_13AnsiZ133' == '1'), 1, 0)
#Replace NA with 0.01
tempData$'3_13Ansi760'  <- replace_na(tempData$'3_13Ansi760', 0.01)
tempData$'3_13AnsiA300'  <- replace_na(tempData$'3_13AnsiA300', 0.01)
tempData$'3_13AnsiZ133'  <- replace_na(tempData$'3_13AnsiZ133', 0.01)

cmStandardsCount <- tempData$'3_13Ansi760' + tempData$'3_13AnsiA300' + tempData$'3_13AnsiZ133'

cmStandards <- if_else(cmStandardsCount >= 3, 4,
                       if_else(cmStandardsCount >= 2, 3,
                               if_else(cmStandardsCount >= 1, 2,
                                       if_else(cmStandardsCount == .03, -1, 1))))
#Return NA
tempData <- mutate(tempData, cmStandards)
tempData <- tempData %>% mutate(cmStandards = na_if(cmStandards, -1))
data$cmStandards <- tempData$cmStandards

#Convert Y to 1 and N to 0 for counting
tempData$'7_15RspCC'  <- ifelse((inputData$'7_15RspCC' == 'Y'), 1, 0)
tempData$'7_15RskInsp'  <- ifelse((inputData$'7_15RskInsp' == 'Y'), 1, 0)
tempData$'7_15RtTrM'  <- ifelse((inputData$'7_15RtTrM' == 'Y'), 1, 0)
tempData$'7_15SrvyID'  <- ifelse((inputData$'7_15SrvyID' == 'Y'), 1, 0)
tempData$'7_15Other'  <- ifelse((inputData$'7_15Other' == 'Y'), 1, 0)
#Replace NA with .01
tempData$'7_15RspCC'  <- replace_na(tempData$'7_15RspCC', .01)
tempData$'7_15RskInsp'  <- replace_na(tempData$'7_15RskInsp', .01)
tempData$'7_15RtTrM'  <- replace_na(tempData$'7_15RtTrM', .01)
tempData$'7_15SrvyID'  <- replace_na(tempData$'7_15SrvyID', .01)
tempData$'7_15Other'  <- replace_na(tempData$'7_15Other', .01)

tempData$'7_14WritTrRsk'  <- replace_na(inputData$'7_14WritTrRsk', '-1')
tempData$'7_12TrRskM'  <- replace_na(inputData$'7_12TrRskM', '-1')

cmSafetyInspcount <- tempData$'7_15RspCC' + tempData$'7_15RskInsp' + tempData$'7_15RtTrM' + tempData$'7_15SrvyID' + tempData$'7_15Other'

cmSafety <- if_else(tempData$'7_12TrRskM' == 'Y',
                    if_else(tempData$'7_14WritTrRsk' == 'Y',
                            if_else(cmSafetyInspcount >= 3, 4,
                                    if_else(cmSafetyInspcount >= 1, 3,2)),
                            if_else(tempData$'7_14WritTrRsk' == 'N', 2, 1)),
                    if_else(tempData$'7_12TrRskM' == 'N', 1, -1))
#Return NA
tempData <- mutate(tempData, cmSafety)
tempData <- tempData %>% mutate(cmSafety = na_if(cmSafety, -1))
data$cmSafety <- tempData$cmSafety

#Convert Y to 1 and N to 0 for counting
tempData$'7_17BfErgy'  <- ifelse((inputData$'7_17BfErgy' == 'Y'), 1, 0)
tempData$'7_17FrWd'  <- ifelse((inputData$'7_17FrWd' == 'Y'), 1, 0)
tempData$'7_17MdFrn'  <- ifelse((inputData$'7_17MdFrn' == 'Y'), 1, 0)
tempData$'7_17Mulch'  <- ifelse((inputData$'7_17Mulch' == 'Y'), 1, 0)
tempData$'7_17ProLum'  <- ifelse((inputData$'7_17ProLum' == 'Y'), 1, 0)
tempData$'7_17SleWd'  <- ifelse((inputData$'7_17SleWd' == 'Y'), 1, 0)
#Replace NA with .01
tempData$'7_17BfErgy' <- replace_na(tempData$'7_17BfErgy', .01)
tempData$'7_17FrWd'  <- replace_na(tempData$'7_17FrWd', .01)
tempData$'7_17MdFrn'  <- replace_na(tempData$'7_17MdFrn', .01)
tempData$'7_17Mulch'  <- replace_na(tempData$'7_17Mulch', .01)
tempData$'7_17ProLum'  <- replace_na(tempData$'7_17ProLum', .01)
tempData$'7_17SleWd'  <- replace_na(tempData$'7_17SleWd', .01)

cmRecyclingCount <- tempData$'7_17BfErgy' + tempData$'7_17FrWd' + tempData$'7_17MdFrn' + tempData$'7_17Mulch' + tempData$'7_17ProLum' + tempData$'7_17SleWd'

cmRecycling <- if_else(cmRecyclingCount >= 3.00, 4,
                      if_else(cmRecyclingCount >= 2.00, 3,
                              if_else(cmRecyclingCount >= 1.00, 2,
                                      #I have absolutely no idea why cmRecyclingCount == 0.06 doesn't work, but >= .06 and <= .061 does
                                      if_else(cmRecyclingCount >= 0.06 & cmRecyclingCount <= 0.061, -1, 1))))
#Return NA
tempData <- mutate(tempData, cmRecycling)
tempData <- tempData %>% mutate(cmRecycling = na_if(cmRecycling, -1))
data$cmRecycling <- tempData$cmRecycling

#Add resource management variables
data$cmResourceManagement <- data$cmManagement + data$cmFunding + data$cmStaffing + data$cmAssessment + data$cmProtection + data$cmSelection + data$cmStandards + data$cmSafety + data$cmRecycling

#### Community Framework ####
cmAgencyCooperation <- if_else(inputData$'1_14AllOpr' == 5, 4,
                               if_else(inputData$'1_14AllOpr' == 4, 3,
                                       if_else(inputData$'1_14AllOpr' == 3, 2.5,
                                               if_else(inputData$'1_14AllOpr' == 2, 2,
                                                       if_else(inputData$'1_14AllOpr' == 1, 1, 1)))))

cmInvolvement <- if_else(inputData$'1_14PriLanOwn' == 5, 4,
                         if_else(inputData$'1_14PriLanOwn' == 4, 3,
                                 if_else(inputData$'1_14PriLanOwn' == 3, 2.5,
                                         if_else(inputData$'1_14PriLanOwn' == 2, 2,
                                                 if_else(inputData$'1_14PriLanOwn' == 1, 1, 1)))))

cmGreenCooperation <- if_else(inputData$'1_14GreInd' == 5, 4,
                              if_else(inputData$'1_14GreInd' == 4, 3,
                                      if_else(inputData$'1_14GreInd' == 3, 2.5,
                                              if_else(inputData$'1_14GreInd' == 2, 2,
                                                      if_else(inputData$'1_14GreInd' == 1, 1, 1)))))

cmAction <- if_else(inputData$'1_14Citzens' == 5, 4,
                    if_else(inputData$'1_14Citzens' == 4, 3,
                            if_else(inputData$'1_14Citzens' == 3, 2.5,
                                    if_else(inputData$'1_14Citzens' == 2, 2,
                                            if_else(inputData$'1_14Citzens' == 1, 1, 1)))))

cmInteraction <- if_else(inputData$'1_14Constits' == 5, 4,
                         if_else(inputData$'1_14Constits' == 4, 3,
                                 if_else(inputData$'1_14Constits' == 3, 2.5,
                                         if_else(inputData$'1_14Constits' == 2, 2,
                                                 if_else(inputData$'1_14Constits' == 1, 1, 1)))))

cmAwareness <- if_else(inputData$'1_14GenPub' == 5, 4,
                       if_else(inputData$'1_14GenPub' == 4, 3,
                               if_else(inputData$'1_14GenPub' == 3, 2.5,
                                       if_else(inputData$'1_14GenPub' == 2, 2,
                                               if_else(inputData$'1_14GenPub' == 1, 1, 1)))))

cmRegionalCooperation <- if_else(inputData$'1_14CoopInt' == 5, 4,
                                 if_else(inputData$'1_14CoopInt' == 4, 3,
                                         if_else(inputData$'1_14CoopInt' == 3, 2.5,
                                                 if_else(inputData$'1_14CoopInt' == 2, 2,
                                                         if_else(inputData$'1_14CoopInt' == 1, 1, 1)))))

cmCommunityFramework <- cmAgencyCooperation + cmInvolvement + cmGreenCooperation + cmAction + cmInteraction + cmAwareness + cmRegionalCooperation

#Add Community Framework indicators to the dataset
data <- mutate(data, cmInteraction, cmGreenCooperation, cmInvolvement, cmAction, cmAgencyCooperation, cmRegionalCooperation, cmAwareness, cmCommunityFramework)

#### Vegetation Resource ####
#Replace NA with -1
#tempData$'6_1TrInv'  <- replace_na(inputData$'6_1TrInv', '-1')
tempData$'6_7CanCo'  <- replace_na(inputData$'6_7CanCo', '-1')
tempData$'6_7RmSens'  <- replace_na(inputData$'6_7RmSens', '-1')
#tempData$'6_8GPSGIS'  <- replace_na(inputData$'6_8GPSGIS', '-1')

cmCanopyCover <- if_else(tempData$'6_1TrInv' == 'Y' & tempData$'6_7CanCo' == 'Y',
                         if_else(tempData$'6_7RmSens' == 'Y',
                                 if_else(tempData$'6_8GPSGIS' == 'Y',4,3),2),
                         if_else(tempData$'6_1TrInv' == '-1' & tempData$'6_7CanCo' == '-1', -1, 1))
#Return NA
tempData <- mutate(tempData, cmCanopyCover)
tempData <- tempData %>% mutate(cmCanopyCover = na_if(cmCanopyCover, -1))
data$cmCanopyCover <- tempData$cmCanopyCover

#Replace NA 
#tempData$'6_6StrTr'  <- replace_na(inputData$'6_6StrTr', .01)
#tempData$'6_6PriTr'  <- replace_na(inputData$'6_6PriTr', .01)
tempData$'6_7iTrEco'  <- replace_na(inputData$'6_7iTrEco', '-1')

cmAge <- if_else(tempData$'6_1TrInv' == 'Y' & tempData$'6_6StrTr' == '1',
                 if_else(tempData$'6_6PriTr' == '1' | tempData$'6_7iTrEco' == 'Y',
                         if_else(tempData$'6_8GPSGIS' == 'Y', 4,3),2),
                 if_else(tempData$'6_1TrInv' == 'N', 1, -1))
#Return NA
tempData <- mutate(tempData, cmAge)
tempData <- tempData %>% mutate(cmAge = na_if(cmAge, -1))
data$cmAge <- tempData$cmAge

#Replace NA with -1
tempData$'6_13TrSpe'  <- replace_na(inputData$'6_1TrInv', '-1')

cmSpeciesMix <- if_else(tempData$'6_1TrInv' == 'Y' & tempData$'6_13TrSpe' == 'Y',
                        if_else(tempData$'6_7iTrEco' == 'Y',
                                if_else(tempData$'6_8GPSGIS' == 'Y',4,3),2),
                        if_else(tempData$'6_1TrInv' == 'N', 1, -1))
#Return NA
tempData <- mutate(tempData, cmSpeciesMix)
tempData <- tempData %>% mutate(cmSpeciesMix = na_if(cmSpeciesMix, -1))
data$cmSpeciesMix <- tempData$cmSpeciesMix

data$cmVegetationResource <- (data$cmCanopyCover + data$cmAge + data$cmSpeciesMix) * (4/3)

#Add total score to the dataset
data$cmScore <- data$cmResourceManagement + data$cmCommunityFramework + data$cmVegetationResource
data$cmRelativeScore <- (data$cmScore - 20) * (100/(80-20))
data$cmResourceManagementRelative <- (data$cmResourceManagement - 9) * (100/(36-9))
data$cmCommunityFrameworkRelative <- (data$cmCommunityFramework - 7) * (100/(28-7))
data$cmVegetationResourceRelative <- (data$cmVegetationResource - 4) * (100/(16-4))

#### Export data to an excel file ####
write.xlsx(data, "//uwsp.edu/files/CNR/Projects/MuniDashboard/2014 Survey Data/R Exported Data/TreeScoreDataset.xlsx")