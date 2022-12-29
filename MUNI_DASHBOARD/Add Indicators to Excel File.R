# Some PopulationGroups have no information for a variable which causes them to be omitted from the dataset
# To fix this problem, I have done casewise insertion of null values when there is no data to be found
# This means when the data changes with a new survey each casewise piece will need to be reevaluated 
# This includes the following variables: percentCanGoal, percentCanProgress, yearsToGoal, greenSpaceAreaMeters, greenSpaceAreaFeet, smaScore

rm(list = ls())

library(tidyverse)
library(readxl)
library(gridExtra)
#### Data Manipulation ####
#Read in Data
dataDir <- "//uwsp.edu/files/CNR/Projects/MuniDashboard/2014 Survey Data/R Exported Data/"
data <- read_excel(paste(dataDir, "TreeScoreDataset.xlsx", sep = ""))

#### Helper Functions ####
#Clean data and Group By
singleRemoveAndGroup <- function(dataVar, groupingVar, orderingVar) {
  
  cleanData <- data %>% drop_na(!!sym(dataVar)) %>% group_by(!!sym(groupingVar), !!sym(orderingVar)) %>% summarise(SummaryValue = mean(!!sym(dataVar)))
  
  cleanData <- ungroup(cleanData)
  
  return(cleanData)
}

groupRegion <- function(dataVar) {
  
  cleanData <- singleRemoveAndGroup(dataVar, 'Region', 'RegionOrder')
  
  return(cleanData)
}

#Accidentally coded all this, might be useful someday
# groupPopulation <- function(dataVar) {
#   
#   cleanData <- singleRemoveAndGroup(dataVar, 'PopulationGroup', 'PopulationGroupOrder')
#   
#   return(cleanData)
# }

nationalAverage <- function(dataVar) {
  
  cleanData <- data %>% drop_na(!!sym(dataVar)) %>% summarise(SummaryValue = mean(!!sym(dataVar))) 
  
  return(cleanData)
}

createGroupedData <- function(dataVar) {
  
  groupedSet <- c(groupRegion(dataVar)$SummaryValue, nationalAverage(dataVar)$SummaryValue) #,groupPopulation(dataVar)$SummaryValue)
  
  return(groupedSet)
}

#Create Labeling Column
Group <- c('Midwest',
           'Northeast',
           'South',
           'West',
           # 
           # '1,000,000 +',
           # '10,000 to 24,999',
           # '100,000 to 249,999',
           # '2,500 to 4,999',
           # '25,000 to 49,999',
           # '250,000 to 499,999',
           # '5,000 to 9,999',
           # '50,000 to 99,999',
           # '500,000 to 999,999',
           
           'National')

Midwest <- 1
Northeast <- 2
South <- 3
West <- 4

# p2500 <- 8
# p5000 <- 11
# p10000 <- 6
# p25000 <- 9
# p50000 <- 12
# p100000 <- 7
# p250000 <- 10
# p500000 <- 13
# p1000000 <- 5

National <- 5 #14

##########################################################################################################################################
########################################################Calculating INDICATORS############################################################

#### Tree Budget & Need ####
#Budget per Capita 
dollarsPerCapita <- createGroupedData('dollarsPerCapita')

#Budget per Public Tree
dollarsPerPublicTree <- createGroupedData('dollarsPerPublicTree')

#Budget percent of Total Municipal Budget
percentOfMuniBud <- createGroupedData('percentOfMuniBud')

#Budget percent of Identified Need
budgetNeeds <- createGroupedData('budgetNeeds')

myData <- tibble(Group, dollarsPerCapita, dollarsPerPublicTree, percentOfMuniBud, budgetNeeds)

#### Resource Budget Allocation ####
#Administration
raAdmin <- createGroupedData('raAdmin')

#Planting
raPlan <- createGroupedData('raPlan')

#Pruning
raPrun <- createGroupedData('raPrun')

#Removal
raRem <- createGroupedData('raRem')

#Other
raOther <- createGroupedData('raOther')

myData <- mutate(myData, raAdmin, raPlan, raPrun, raRem, raOther)

#### Community Trees & Governance ####
#Tree Board or Related Group Exists
treeBoard <- createGroupedData('treeBoard')

#Written Strategic Plan Exists
writtenStratPlan <- createGroupedData('writtenStratPlan')

#Written Strategic Plan up to Date
writtenStratPlanUpToDate <- createGroupedData('writtenStratPlanUpToDate')

#Tree Ordinance Exists
ordinance <- createGroupedData('ordinance')

#Tree Ordinance up to Date
ordinanceYear <- createGroupedData('ordinanceYear')

myData <- mutate(myData, treeBoard, writtenStratPlan, writtenStratPlanUpToDate, ordinance, ordinanceYear)

#### Tree Inventory & Space Allocation ####
#Tree Inventory Exists
treeResourceInventoryExists <- createGroupedData('treeResourceInventoryExists')

#Tree Inventory up to Date
treeResourceInventory <- createGroupedData('treeResourceInventory')

#Public Green Space Area (Meters/Capita)
greenSpaceAreaMeters <- createGroupedData('greenSpaceAreaMeters')

#Public Green Space Area (Feet/Capita)
greenSpaceAreaFeet <- createGroupedData('greenSpaceAreaFeet')

myData <- mutate(myData, treeResourceInventoryExists, treeResourceInventory, greenSpaceAreaMeters, greenSpaceAreaFeet)

#### Tree Canopy Assesment ####
#Tree Canopy Goal Exists
canCovGoal <- createGroupedData('canCovGoal')

#Current Tree Canopy
percentCurCan <- createGroupedData('percentCurCan')

#Tree Canopy Cover Goal Achievement
percentCanGoal <- createGroupedData('percentCanGoal')

#Years to Achieve Tree Canopy Goal
yearsToGoal <- createGroupedData('yearsToGoal')

myData <- mutate(myData, canCovGoal, percentCurCan, percentCanGoal, yearsToGoal)

# #Tree Canopy Progress
# rPercentCanProg <- singleRemoveAndGroup('percentCanProgress', 'Region', 'RegionOrder')
# pPercentCanProg <- singleRemoveAndGroup('percentCanProgress', 'PopulationGroup', 'PopulationGroupOrder')
# #Casewise adding null vaue
# pPercentCanProg <- add_row(pPercentCanProg, PopulationGroup = '5-9.9k', PopulationGroupOrder = 2, SummaryValue = -1, .after = 6)
# pPercentCanProg <- pPercentCanProg %>% mutate(SummaryValue = na_if(SummaryValue, -1))
# nPercentCanProg <- nationalAverage('percentCanProgress')
# percentCanProgress <- c(rPercentCanProg$SummaryValue, pPercentCanProg$SummaryValue, nPercentCanProg$SummaryValue)
# 
# myData <- mutate(myData, percentCanProgress)

#### Tree Stocking Level ####
#Street Tree Stocking Level Attainment
strTrStockingLevel <- createGroupedData('strTrStockingLevel')

#Park Tree Stocking Level Attainment
prkTrStockingLevel <- createGroupedData('prkTrStockingLevel')

#Municipal Tree Stocking Level Attainment
munTrStockingLevel <- createGroupedData('munTrStockingLevel')

#Public Street Tree Density 
pubStrTrDensity <- createGroupedData('pubStrTrDensity')

myData <- mutate(myData, strTrStockingLevel, prkTrStockingLevel, munTrStockingLevel, pubStrTrDensity)

#### Active (Systematic) Management
#Current Inspection and Pruning Cycle
currentPrunInsCyc <- createGroupedData('currentPrunInsCyc')

#Desired Inspection and Pruning Cycle
desiredPrunInsCyc <- createGroupedData('desiredPrunInsCyc')

#Time Inspection and Pruning Off Cycle
yearsOfPrunInsCyc <- createGroupedData('yearsOfPrunInsCyc')

#Attainment of Inspection and Pruning Cycle
percentAttPrunInsCyc <- createGroupedData('percentAttPrunInsCyc')

#Active Management Level of Tree Population
activeManagement <- createGroupedData('activeManagement')

myData <- mutate(myData, currentPrunInsCyc, desiredPrunInsCyc, yearsOfPrunInsCyc, percentAttPrunInsCyc, activeManagement)

#### Index Plotting ####
#### Tree City USA ####
#Annual Budget at Least $2 per capita (Value is budget per capita, not Y/N)
tcBudget <- createGroupedData('tcBudget')

#Have a Tree Ordinance
tcOrdinance <- createGroupedData('tcOrdinance')

#Have Tree Board or Related Department
tcAdvisoryBoard <- createGroupedData('tcAdvisoryBoard')

#Arbor Day Observation & Proclamation
tcArborDay <- createGroupedData('tcArborDay')

#Overall Percent of Criteria Met
tcScore <- createGroupedData('tcScore')

myData <- mutate(myData, tcBudget, tcOrdinance, tcAdvisoryBoard, tcArborDay, tcScore)

#### Community Activity Reporting System (CARS) ####
#Professional Staff with Credential
carsStaff <- createGroupedData('carsStaff')

#Tree Ordinance / Policy Covers All Three Areas
carsOrdinance <- createGroupedData('carsOrdinance')

#Have An Advocacy / Advisory Board
carsAdvocacy <- createGroupedData('carsAdvocacy')

#Management Plan Based on Inventory
carsManagement <- createGroupedData('carsManagement')

#Overall Percent of Criteria Met
carsScore <- createGroupedData('carsScore')

myData <- mutate(myData, carsStaff, carsOrdinance, carsAdvocacy, carsManagement, carsScore)

#### Society of Municipal Aborists ####
#Professional Staff ISA Certified
smaStaff <- createGroupedData('smaStaff')

#Have a Master Plan
smaMasterPlan <- createGroupedData('smaMasterPlan')

#Currently Tree City USA (TCUSA)
smaStatus <- createGroupedData('smaStatus')

#TCUSA Growth Award Past 5 Years
smaAward <- createGroupedData('smaAward')

#Contractor Preference TCIA Accredited
smaPreference <- createGroupedData('smaPreference')

#Professional Standards (Z133 and A300) Adherence
smaStandards <- createGroupedData('smaStandards')

#Overall Percent of Criteria Met and Attained
smaScore <- createGroupedData('smaScore')

#Add SMA Fields to dataset
myData <- mutate(myData, smaStaff, smaMasterPlan, smaStatus, smaAward, smaPreference, smaStandards, smaScore)

##### Urban Forest Sustainability (Clark and Matheny Model) ####
#### Community Framework Indicators ####
#Citizen-Government-Business Interaciton
cmInteraction <- createGroupedData('cmInteraction')

#Green Industry Cooperation
cmGreenCooperation <- createGroupedData('cmGreenCooperation')

#Involve Private/Institutional Landholders
cmInvolvement <- createGroupedData('cmInvolvement')

#Neighborhood Action
cmAction <- createGroupedData('cmAction')

#Public Agency Cooperation
cmAgencyCooperation <- createGroupedData('cmAgencyCooperation')

#Regional Cooperation
cmRegionalCooperation <- createGroupedData('cmRegionalCooperation')

#Trees as a Community Resource Awareness
cmAwareness <- createGroupedData('cmAwareness')

#Component Score 
cmCommunityFrameworkScore <- createGroupedData('cmCommunityFramework')

#Add Community Framework Indicators to dataset
myData <- mutate(myData, cmInteraction, cmGreenCooperation, cmInvolvement, cmAction, cmAgencyCooperation, cmRegionalCooperation, cmAwareness, cmCommunityFrameworkScore)

#### Resource Management Indicators ####
#Assessment Tools
cmAssessment <- createGroupedData('cmAssessment')

#Citizen Safety
cmSafety <- createGroupedData('cmSafety')

#City Staffing
cmStaffing <- createGroupedData('cmStaffing')

#Citywide Tree Management Plan
cmManagement <- createGroupedData('cmManagement')

#City-Wide Funding
cmFunding <- createGroupedData('cmFunding')

#Protection of Existing Trees
cmProtection <- createGroupedData('cmProtection')

#Recycling of Trees
cmRecycling <- createGroupedData('cmRecycling')

#Species and Site Selection
cmSelection <- createGroupedData('cmSelection')

#Standards for Tree care
cmStandards <- createGroupedData('cmStandards')

#Component Score
cmResourceManagementScore <- createGroupedData('cmResourceManagement')

#Add Community Framework Indicators to dataset
myData <- mutate(myData, cmAssessment, cmSafety, cmStaffing, cmManagement, cmFunding, cmProtection, cmRecycling, cmSelection, cmStandards, cmResourceManagementScore)

#### Vegetation Resource Indicators ####
#Age Distribution
cmAge <- createGroupedData('cmAge')

#Canopy Cover
cmCanopyCover <- createGroupedData('cmCanopyCover')

#Native Vegetation (This value was not collected in the first survey)
cmNativeVegetation <- c(rep('NULL', 5))

#Species Distribution
cmSpeciesDistribution <- createGroupedData('cmSpeciesMix')

#Component Score
cmVegetationResourceScore <- createGroupedData('cmVegetationResource')

#Add Vegetation Resource Indicators Indicators to dataset
myData <- mutate(myData, cmAge, cmCanopyCover, cmNativeVegetation, cmSpeciesDistribution, cmVegetationResourceScore)

#### Summary ####
#Overall Score
cmScore <- createGroupedData('cmScore')
#Overall Percent
cmRelativeScore <- createGroupedData('cmRelativeScore')

#Add Summary Scores to dataset
myData <- mutate(myData, cmScore, cmRelativeScore)

#### Create Excel File ####
write.xlsx(myData, "//uwsp.edu/files/CNR/Projects/MuniDashboard/2014 Survey Data/R Exported Data/TreeScoreValues.xlsx")


#### Example UI Usage ####
# dataDir <- "//uwsp.edu/files/CNR/Projects/MuniDashboard/2014 Survey Data/R Exported Data/"
# data <- read_excel(paste(dataDir, "TreeScoreValues.xlsx", sep = ""))
# 
# #Helper Functions
# getUserRegion <- function(userInputRegion) {
#   #Get region data
#   userRegionOut <- if_else(userInputRegion == 'Midwest', 1,
#                            if_else(userInputRegion == 'Northeast', 2,
#                                    if_else(userInputRegion == 'South', 3,
#                                            if_else(userInputRegion == 'West', 4,
#                                                    -1))))
#   return(userRegionOut)
# }
# 
# getRegionValue <- function(varName) {
#   regionValue <- as.double(data[userRegion, varName])
#   
#   return(regionValue)
# }
# 
# getNationalValue <- function(varName) {
#   regionValue <- as.double(data[5, varName])
#   
#   return(regionValue)
# }
# 
# 
# #Function Call Order
# #Get the user input region
# userInputRegion <- 'Midwest'
# #Run this once to set variables for getUserRegion 
# userRegion <- getUserRegion(userInputRegion)
# 
# #Run this for each variable
# myRegionDollarsPerCapita <- getRegionValue('dollarsPerCapita')
# myNationalDollarsPerCapita <- getNationalValue('dollarsPerCapita')
