rm(list = ls())

library(tidyverse)
library(readxl)
library(gridExtra)
#### Data Manipulation ####
#Read in Data
dataDir <- "//uwsp.edu/files/CNR/Projects/MuniDashboard/2014 Survey Data/R Exported Data/"
data <- read_excel(paste(dataDir, "TreeScoreDataset.xlsx", sep = ""))

#Color schemes for graphing 
userColor <- c('MyTreeScore' = 'forestgreen',
               '2.5-4.9k' = 'red',
               '5-9.9k' = 'orange',
               '10-24.9k' = 'yellow',
               '25-49.9k' = 'greenyellow',
               '50-99.9k' = 'green',
               '100-249.9k' = 'blue',
               '250-499.9k' = 'purple4',
               '500-999.9k' = 'plum',
               '1,000k +' = 'violet',
               'National' = 'coral4')

userColor2 <- c('MyTreeScore' = 'forestgreen',
                'Midwest' = 'red',
                'Northeast' = 'blue',
                'South' = 'purple',
                'West' = 'yellow',
                'National' = 'coral4')

#### Helper Functions ####
#Clean data and Group By
singleRemoveAndGroup <- function(dataVar, groupingVar, orderingVar) {
  
  cleanData <- data %>% drop_na(!!sym(dataVar)) %>% group_by(!!sym(groupingVar), !!sym(orderingVar)) %>% summarise(SummaryValue = mean(!!sym(dataVar)))
  
  return(cleanData)
}

#Calculate National average
nationalAverage <- function(dataVar) {
  
  cleanData <- data %>% drop_na(!!sym(dataVar)) %>% summarise(SummaryValue = mean(!!sym(dataVar)))
  
  return(cleanData)
}

#Plotting for Region Data
plotCleanedDataRegionBar <- function(cleanData, xVal, yVal, title = NULL, yLabel = yVal, color, textFormat, 
                                     precision, axisPrecision, lowerBound, upperBound, axisBreaks) {
  
  plottedData <- ggplot(data = cleanData, aes(x = !!sym(xVal), y = !!sym(yVal), fill = !!sym(xVal))) + 
    ggtitle(title) + 
    ylab(yLabel) + 
    geom_bar(stat='identity') + 
    #face = 'bold'
    geom_text(aes(label = sprintf(textFormat, format(round(SummaryValue, digits = precision), big.mark = ',',
                                                     format = 'f'))), vjust = -.25, color = "black", size = 6) +
    scale_y_continuous(labels = scales::number_format(accuracy = (.1 ^ axisPrecision)), 
                       breaks = scales::pretty_breaks(n = axisBreaks)) +
    coord_cartesian(ylim=c(lowerBound, upperBound)) +
    scale_fill_manual(values = color) +
    geom_hline(yintercept = 0, color = "black") +
    theme(#text = element_text(size=20),
      axis.title.x = element_blank(),
      axis.text.x = element_text(face = 'bold',size = 12, margin = margin(t = 0, r = 0, b = 10, l = 10)), # angle = 45
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(size = 12),
      axis.text.y = element_text(size = 12, margin = margin(t = 0, r = 3, b = 0, l = axisPrecision + 5)),
      #legend.direction = 'horizontal', 
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      #axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
      axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
      panel.background = element_rect(fill = "white"))
  
  return(plottedData)
}

#Plotting for Population Data
plotCleanedDataPopulationBar <- function(cleanData, xVal, yVal, title = NULL, yLabel = yVal, color, textFormat, 
                                         precision, axisPrecision, lowerBound, upperBound, axisBreaks) {
  
  plottedData <- ggplot(data = cleanData, aes(x = !!sym(xVal), y = !!sym(yVal), fill = !!sym(xVal))) + 
    ggtitle(title) + 
    ylab(yLabel) + 
    geom_bar(stat='identity') + 
    #face = 'bold'
    geom_text(aes(label = sprintf(textFormat, format(round(SummaryValue, digits = precision), big.mark = ',',
                                                     format = 'f'))), vjust = -.25, color = "black", size = 6) +
    scale_y_continuous(labels = scales::number_format(accuracy = (.1 ^ axisPrecision)), 
                       breaks = scales::pretty_breaks(n = axisBreaks)) +
    coord_cartesian(ylim=c(lowerBound, upperBound)) +
    scale_fill_manual(values = color) +
    geom_hline(yintercept = 0, color = "black") +
    theme(#text = element_text(size=20),
      axis.title.x = element_blank(),
      axis.text.x = element_text(face = 'bold', size = 12, angle = 20, margin = margin(t = 5, r = 0, b = 10, l = 0)),  
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(size = 12),
      axis.text.y = element_text(size = 12, margin = margin(t = 0, r = 3, b = 0, l = axisPrecision + 5)),
      #legend.direction = 'horizontal', 
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      #axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
      axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
      panel.background = element_rect(fill = "white"))
  
  return(plottedData)
}

#Cleans and plots the region data. Only call this function to get a final graph
plotRegion <- function(graphingVar, userData, title, yAxis, format, precision, axisPrecision, lowerBound, upperBound, axisBreaks) {
  
  nationalValue <- pull(nationalAverage(graphingVar))
  
  userTibble <- tibble(Region = 'MyTreeScore', RegionOrder =  0, SummaryValue =  userData)
  nationalTibble <- tibble(Region = 'National', RegionOrder = 100, SummaryValue = nationalValue)
  
  cleanData <- singleRemoveAndGroup(graphingVar, 'Region', 'RegionOrder')
  cleanData <- bind_rows(cleanData, userTibble, nationalTibble)
  
  cleanData <- cleanData %>% ungroup(Region)
  cleanData <- cleanData %>% mutate(Region = fct_reorder(Region, RegionOrder))
  
  plottedData <- plotCleanedDataRegionBar(cleanData, 'Region', 'SummaryValue', title, yAxis, userColor2, 
                                          format, precision, axisPrecision, lowerBound, upperBound, axisBreaks)
  
  return(plottedData)
}

#Cleans and plots the population data. Only call this function to get a final graph
plotPopulation <- function(graphingVar, userData, title, yAxis, format, precision, axisPrecision, lowerBound, upperBound, axisBreaks) {
  
  nationalValue <- pull(nationalAverage(graphingVar))
  
  userTibble <- tibble(PopulationGroup = 'MyTreeScore', PopulationGroupOrder =  0, SummaryValue =  userData)
  nationalTibble <- tibble(PopulationGroup = 'National', PopulationGroupOrder = 100, SummaryValue = nationalValue)
  
  cleanData <- singleRemoveAndGroup(graphingVar, 'PopulationGroup', 'PopulationGroupOrder')
  cleanData <- bind_rows(cleanData, userTibble, nationalTibble)
  
  cleanData <- cleanData %>% ungroup(PopulationGroup)
  cleanData <- cleanData %>% mutate(PopulationGroup = fct_reorder(PopulationGroup, PopulationGroupOrder))
  
  plottedData <- plotCleanedDataPopulationBar(cleanData, 'PopulationGroup', 'SummaryValue', title, yAxis, userColor, 
                                              format, precision, axisPrecision, lowerBound, upperBound, axisBreaks)
  
  return(plottedData)
}

##########################################################################################################################################
########################################################VISUALIZING INDICATORS############################################################

#### Budget & Adequacy ####
#Tree Budget Per Capita
rDollarsPerCapita <- plotRegion('dollarsPerCapita', 6, 'Tree Budget Per Capita', '$/Capita', '%.10s',
                                precision=2, axisPrecision=0, lowerBound=0, upperBound=15, axisBreaks=6)
pDollarsPerCapita <- plotPopulation('dollarsPerCapita', 6, 'Tree Budget Per Capita', '$/Capita', '%.10s',
                                    precision=2, axisPrecision=0, lowerBound=0, upperBound=15, axisBreaks=6)

#Tree Budget % of Total Municipal Budget
rPercentOfMuniBud <- plotRegion('percentOfMuniBud', .5, 'Tree Budget % of Total Municipal Budget', 'Percent', '%.10s',
                                precision=2, axisPrecision=1, lowerBound=0, upperBound=1.4, axisBreaks=8)
pPercentOfMuniBud <- plotPopulation('percentOfMuniBud', .5, 'Tree Budget % of Total Municipal Budget', 'Percent', '%.10s',
                                    precision=2, axisPrecision=1, lowerBound=0, upperBound=1.4, axisBreaks=8)

#Tree Budget Per Public Tree
rDollarsPerPublicTree <- plotRegion('dollarsPerPublicTree', 25, 'Tree Budget Per Public Tree', '$/Tree', '%.10s',
                                    precision=2, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pDollarsPerPublicTree <- plotPopulation('dollarsPerPublicTree', 25, 'Tree Budget Per Public Tree', '$/Tree', '%.10s',
                                        precision=2, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)

#Tree Budget % of Identified Need
rBudgetNeeds <- plotRegion('budgetNeeds', 40, 'Tree Budget % of Identified Need', 'Percent', '%.10s',
                           precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pBudgetNeeds <- plotPopulation('budgetNeeds', 40, 'Tree Budget % of Identified Need', 'Percent', '%.10s',
                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)

#% Resource Allocation - Cant't use the graphing helper methods because this is a different chart type
#Region
#Gather the data
dRaPlan <- singleRemoveAndGroup('raPlan', 'Region', 'RegionOrder')
dRaPlan <- bind_rows(dRaPlan,tibble(Region = 'MyTreeScore', RegionOrder = 0, SummaryValue = 20))
dRaPlan <- bind_rows(dRaPlan,tibble(Region = 'National', RegionOrder = 10, SummaryValue = pull(nationalAverage('raPlan'))))
dRaPlan$Activity <- c(rep('Planting', 6))
dRaPlan$ActivityOrder <- c(rep(2, 6))

dRaPrun <- singleRemoveAndGroup('raPrun', 'Region', 'RegionOrder')
dRaPrun <- bind_rows(dRaPrun,tibble(Region = 'MyTreeScore', RegionOrder = 0, SummaryValue = 20))
dRaPrun <- bind_rows(dRaPrun,tibble(Region = 'National', RegionOrder = 10, SummaryValue = pull(nationalAverage('raPrun'))))
dRaPrun$Activity <- c(rep('Pruning', 6))
dRaPrun$ActivityOrder <- c(rep(3, 6))

dRaRem <- singleRemoveAndGroup('raRem', 'Region', 'RegionOrder')
dRaRem <- bind_rows(dRaRem,tibble(Region = 'MyTreeScore', RegionOrder = 0, SummaryValue = 20))
dRaRem <- bind_rows(dRaRem,tibble(Region = 'National', RegionOrder = 10, SummaryValue = pull(nationalAverage('raRem'))))
dRaRem$Activity <- c(rep('Removal', 6))
dRaRem$ActivityOrder <- c(rep(4, 6))

dRaAdmin <- singleRemoveAndGroup('raAdmin', 'Region', 'RegionOrder')
dRaAdmin <- bind_rows(dRaAdmin,tibble(Region = 'MyTreeScore', RegionOrder = 0, SummaryValue = 20))
dRaAdmin <- bind_rows(dRaAdmin,tibble(Region = 'National', RegionOrder = 10, SummaryValue = pull(nationalAverage('raAdmin'))))
dRaAdmin$Activity <- c(rep('Administrative', 6))
dRaAdmin$ActivityOrder <- c(rep(5, 6))

dRaOther <- singleRemoveAndGroup('raOther', 'Region', 'RegionOrder')
dRaOther <- bind_rows(dRaOther,tibble(Region = 'MyTreeScore', RegionOrder = 0, SummaryValue = 20))
dRaOther <- bind_rows(dRaOther,tibble(Region = 'National', RegionOrder = 10, SummaryValue = pull(nationalAverage('raOther'))))
dRaOther$Activity <- c(rep('Other', 6))
dRaOther$ActivityOrder <- c(rep(10, 6))

mySet <- bind_rows(dRaPlan, dRaPrun, dRaRem, dRaAdmin, dRaOther)
mySet <- mySet %>% ungroup(Region)
mySet <- mySet %>% mutate(Region = fct_reorder(Region, RegionOrder))
mySet <- mySet %>% mutate(Activity = fct_reorder(Activity, ActivityOrder))

#Plot the cleaned region data
rResourceAllocation <- ggplot(data = mySet, aes(fill = Activity, y=SummaryValue, x='')) +
  geom_bar(stat="identity") + facet_grid(facets=. ~ Region) + 
  scale_fill_grey(start = .8, end = .2) +
  ggtitle("Tree Budget Allocation by Activity Area") +
  ylab("") +
  xlab("") +
  coord_polar("y", direction = -1) +
  geom_text(aes(label = sprintf('%.10s', format(round(SummaryValue, digits = 1), big.mark = ',', format = 'f'))),
            position = position_stack(vjust = 0.5), colour = 'white') +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position="bottom",
        legend.direction = 'horizontal',
        strip.background = element_blank())

#Population Group
#Gather the data
dRaPlan <- singleRemoveAndGroup('raPlan', 'PopulationGroup', 'PopulationGroupOrder')
dRaPlan <- bind_rows(dRaPlan,tibble(PopulationGroup = 'MyTreeScore', PopulationGroupOrder = 0, SummaryValue = 20))
dRaPlan <- bind_rows(dRaPlan,tibble(PopulationGroup = 'National', PopulationGroupOrder = 20, SummaryValue = pull(nationalAverage('raPlan'))))
dRaPlan$Activity <- c(rep('Planting', 11))
dRaPlan$ActivityOrder <- c(rep(2, 11))

dRaPrun <- singleRemoveAndGroup('raPrun', 'PopulationGroup', 'PopulationGroupOrder')
dRaPrun <- bind_rows(dRaPrun,tibble(PopulationGroup = 'MyTreeScore', PopulationGroupOrder = 0, SummaryValue = 20))
dRaPrun <- bind_rows(dRaPrun,tibble(PopulationGroup = 'National', PopulationGroupOrder = 20, SummaryValue = pull(nationalAverage('raPrun'))))
dRaPrun$Activity <- c(rep('Pruning', 11))
dRaPrun$ActivityOrder <- c(rep(3, 11))

dRaRem <- singleRemoveAndGroup('raRem', 'PopulationGroup', 'PopulationGroupOrder')
dRaRem <- bind_rows(dRaRem,tibble(PopulationGroup = 'MyTreeScore', PopulationGroupOrder = 0, SummaryValue = 20))
dRaRem <- bind_rows(dRaRem,tibble(PopulationGroup = 'National', PopulationGroupOrder = 20, SummaryValue = pull(nationalAverage('raRem'))))
dRaRem$Activity <- c(rep('Removal', 11))
dRaRem$ActivityOrder <- c(rep(4, 11))

dRaAdmin <- singleRemoveAndGroup('raAdmin', 'PopulationGroup', 'PopulationGroupOrder')
dRaAdmin <- bind_rows(dRaAdmin,tibble(PopulationGroup = 'MyTreeScore', PopulationGroupOrder = 0, SummaryValue = 20))
dRaAdmin <- bind_rows(dRaAdmin,tibble(PopulationGroup = 'National', PopulationGroupOrder = 20, SummaryValue = pull(nationalAverage('raAdmin'))))
dRaAdmin$Activity <- c(rep('Administrative', 11))
dRaAdmin$ActivityOrder <- c(rep(5, 11))

dRaOther <- singleRemoveAndGroup('raOther', 'PopulationGroup', 'PopulationGroupOrder')
dRaOther <- bind_rows(dRaOther,tibble(PopulationGroup = 'MyTreeScore', PopulationGroupOrder = 0, SummaryValue = 20))
dRaOther <- bind_rows(dRaOther,tibble(PopulationGroup = 'National', PopulationGroupOrder = 20, SummaryValue = pull(nationalAverage('raOther'))))
dRaOther$Activity <- c(rep('Other', 11))
dRaOther$ActivityOrder <- c(rep(10, 11))

mySetP <- bind_rows(dRaPlan, dRaPrun, dRaRem, dRaAdmin, dRaOther)
mySetP <- mySetP %>% ungroup(PopulationGroup)
mySetP <- mySetP %>% mutate(PopulationGroup = fct_reorder(PopulationGroup, PopulationGroupOrder))
mySetP <- mySetP %>% mutate(Activity = fct_reorder(Activity, ActivityOrder))

#Plot the cleaned population data
pResourceAllocation <- ggplot(data = mySetP, aes(fill = Activity, y=SummaryValue, x='')) +
  geom_bar(stat="identity") + facet_wrap(facets=. ~ PopulationGroup, ncol = 4) + 
  scale_fill_grey(start = .8, end = .2) +
  ggtitle("Tree Budget Allocation by Activity Area") +
  ylab("") +
  xlab("") +
  coord_polar("y", direction = -1) +
  geom_text(aes(label = sprintf('%.10s', format(round(SummaryValue, digits = 1), big.mark = ',', format = 'f'))),
            position = position_stack(vjust = 0.5), colour = 'white') +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position="bottom",
        legend.direction = 'horizontal',
        strip.background = element_blank())
#Public Tree Asset Value
#DELETED FROM DASHBOARD
# rPublicTreeAssetValue <- plotRegion('publicTreeAssetValue', 111211311, 'Public Tree Asset Value', 'Dollars ($)', '%.15s',
#                                     precision=0, axisPrecision=0, lowerBound=0, upperBound=150000000, axisBreaks=8)

#### Governance ####
#Community Tree Board or Related Group Exists 
rTreeBoard <- plotRegion('treeBoard', 100, 'Community Tree Board or Related Group Exists', 'Yes (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pTreeBoard <- plotPopulation('treeBoard', 100, 'Community Tree Board or Related Group Exists', 'Yes (%)', '%.10s',
                             precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
#Community has a Tree Ordinance
rOrdinance <- plotRegion('ordinance', 100, 'Community has a Tree Ordinance', 'Yes (%)', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pOrdinance <- plotPopulation('ordinance', 100, 'Community has a Tree Ordinance', 'Yes (%)', '%.10s',
                             precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)

#Community Tree Ordinance up to Date
rOrdinanceYear <- plotRegion('ordinanceYear', 100, 'Community Tree Ordinance up to Date', 'Yes (%)', '%.10s',
                             precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pOrdinanceYear <- plotPopulation('ordinanceYear', 100, 'Community Tree Ordinance up to Date', 'Yes (%)', '%.10s',
                                 precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)

#Community has a Written Strategic Plan that Includes Trees
rWrittenStratPlan <- plotRegion('writtenStratPlan', 100, 'Community has a Written Strategic Plan that Includes Trees', 'Yes (%)', '%.10s',
                                precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pWrittenStratPlan <- plotPopulation('writtenStratPlan', 100, 'Community has a Written Strategic Plan that Includes Trees', 'Yes (%)', '%.10s',
                                    precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)

#### Tree Resource Management ####
#Community Tree Inventory up to Date
rTreeResourceInventory <- plotRegion('treeResourceInventory', 100, 'Community Tree Inventory up to Date', 'Yes (%)', '%.10s',
                                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pTreeResourceInventory <- plotPopulation('treeResourceInventory', 100, 'Community Tree Inventory up to Date', 'Yes (%)', '%.10s',
                                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
#Community has Tree Canopy Goal
rCanCovGoal <- plotRegion('canCovGoal', 100, 'Community has Tree Canopy Goal', 'Yes (%)', '%.10s',
                          precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pCanCovGoal <- plotPopulation('canCovGoal', 100, 'Community has Tree Canopy Goal', 'Yes (%)', '%.10s',
                              precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)

#Tree Canopy Cover Goal Achievement
rPercentCanGoal <- plotRegion('percentCanGoal', 100, 'Tree Canopy Cover Goal', 'Tree Canopy (%)', '%.10s',
                              precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pPercentCanGoal <- plotPopulation('percentCanGoal', 100, 'Tree Canopy Cover Goal', 'Tree Canopy (%)', '%.10s',
                                  precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)

#Current Tree Canopy Percent
rPercentCurCan <- plotRegion('percentCurCan', 40, 'Current Tree Canopy Cover', 'Tree Canopy (%)', '%.10s',
                             precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pPercentCurCan <- plotPopulation('percentCurCan', 40, 'Current Tree Canopy Cover', 'Tree Canopy (%)', '%.10s',
                                 precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)

#Tree Canopy Progress
rPercentCanProg <- plotRegion('percentCanProgress', 100, 'Progress Toward Tree Canopy Goal', 'Attained (%)', '%.10s',
                              precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pPercentCanProg <- plotPopulation('percentCanProgress', 100, 'Progress Toward Tree Canopy Goal', 'Attained (%)', '%.10s',
                                  precision=1, axisPrecision=0, lowerBound=0, upperBound=130, axisBreaks=6)

#Years to Achieve Tree Canopy Goal
rYearsToGoal <- plotRegion('yearsToGoal', 7, 'Years to Achieve Tree Canopy Goal', 'Years', '%.10s',
                           precision=1, axisPrecision=0, lowerBound=0, upperBound=16, axisBreaks=7)
pYearsToGoal <- plotPopulation('yearsToGoal', 7, 'Years to Achieve Tree Canopy Goal', 'Years', '%.10s',
                               precision=1, axisPrecision=0, lowerBound=0, upperBound=25, axisBreaks=6)

#Green Space Area Per Person Meters
rGreenSpaceAreaMeters <- plotRegion('greenSpaceAreaMeters', 100, bquote("Green Space Area (" *  m^2 * ") per Person"), bquote("Per Capita (" *  m^2 * ")"), '%.10s',
                                    precision=1, axisPrecision=0, lowerBound=0, upperBound=200, axisBreaks=8)
pGreenSpaceAreaMeters <- plotPopulation('greenSpaceAreaMeters', 100, bquote("Green Space Area (" *  m^2 * ") per Person"), bquote("Per Capita (" *  m^2 * ")"), '%.10s',
                                        precision=1, axisPrecision=0, lowerBound=0, upperBound=200, axisBreaks=8)

#Green Space Area Per Person Feet
rGreenSpaceAreaFeet <- plotRegion('greenSpaceAreaFeet', 1000, bquote("Green Space Area (" *  ft^2 * ") per Person"), bquote("Per Capita (" *  ft^2 * ")"), '%.10s',
                                  precision=1, axisPrecision=0, lowerBound=0, upperBound=2000, axisBreaks=8)
pGreenSpaceAreaFeet <- plotPopulation('greenSpaceAreaFeet', 1000, bquote("Green Space Area (" *  ft^2 * ") per Person"), bquote("Per Capita (" *  ft^2 * ")"), '%.10s',
                                      precision=1, axisPrecision=0, lowerBound=0, upperBound=2050, axisBreaks=8)

#Current Inspection and Pruning Cycle
rCurrentPrunInsCyc <- plotRegion('currentPrunInsCyc', 5, 'Current Inspection and Pruning Cycle', 'Years', '%.10s',
                                 precision=1, axisPrecision=0, lowerBound=0, upperBound=10, axisBreaks=6)
pCurrentPrunInsCyc <- plotPopulation('currentPrunInsCyc', 5, 'Current Inspection and Pruning Cycle', 'Years', '%.10s',
                                     precision=1, axisPrecision=0, lowerBound=0, upperBound=25, axisBreaks=6)

#Desired Inspection and Pruning Cycle
rDesiredPrunInsCyc <- plotRegion('desiredPrunInsCyc', 5, 'Desired Inspection and Pruning Cycle', 'Years', '%.10s',
                                 precision=1, axisPrecision=0, lowerBound=0, upperBound=10, axisBreaks=6)
pDesiredPrunInsCyc <- plotPopulation('desiredPrunInsCyc', 5, 'Desired Inspection and Pruning Cycle', 'Years', '%.10s',
                                     precision=1, axisPrecision=0, lowerBound=0, upperBound=10, axisBreaks=6)

#Inspection and Pruning Years Off Cycle
rYearsOfPrunInsCyc <- plotRegion('yearsOfPrunInsCyc', 1, 'Inspection and Pruning Years Off Cycle', 'Years', '%.10s',
                                 precision=1, axisPrecision=0, lowerBound=0, upperBound=5, axisBreaks=6)
pYearsOfPrunInsCyc <- plotPopulation('yearsOfPrunInsCyc', 1, 'Inspection and Pruning Years Off Cycle', 'Years', '%.10s',
                                     precision=1, axisPrecision=0, lowerBound=0, upperBound=15, axisBreaks=9)

#Attainment for Inspection and Pruning Cycle
rPercentAttPrunInsCyc <- plotRegion('percentAttPrunInsCyc', 60, 'Attainment for Inspection and Pruning Cycle', 'Attained (%)', '%.10s',
                                    precision= 1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pPercentAttPrunInsCyc <- plotPopulation('percentAttPrunInsCyc', 60, 'Attainment for Inspection and Pruning Cycle', 'Attained (%)', '%.10s',
                                        precision= 1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)

#Percentatge Active (Systematic) Management of Tree Population
rActiveManagement <- plotRegion('activeManagement', 100, 'Percentatge Active (Systematic) Management of Tree Population', 'Percent', '%.10s',
                                precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pActiveManagement <- plotPopulation('activeManagement', 100, 'Percentatge Active (Systematic) Management of Tree Population', 'Percent', '%.10s',
                                    precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)

#### Tree Resource ####
#Street Tree Stocking Level Attainment
rStrTrStockingLevel <- plotRegion('strTrStockingLevel', 100, 'Street Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                                  precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pStrTrStockingLevel <- plotPopulation('strTrStockingLevel', 100, 'Street Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                                      precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)

#Park Tree Stocking Level Attained
rPrkTrStockingLevel <- plotRegion('prkTrStockingLevel', 100, 'Park Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                                  precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pPrkTrStockingLevel <- plotPopulation('prkTrStockingLevel', 100, 'Park Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                                      precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)

#Municipal Tree Stocking Level Attainment
rMunTrStockingLevel <- plotRegion('munTrStockingLevel', 100, 'Municipal Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                                  precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pMunTrStockingLevel <- plotPopulation('munTrStockingLevel', 100, 'Municipal Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                                      precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)

#Public Street Tree Density per Street Mile
rPubStrTrDensity <- plotRegion('pubStrTrDensity', 80, 'Public Street Tree Density per Street Mile', 'Trees per Mile', '%.10s',
                               precision=1, axisPrecision=0, lowerBound=0, upperBound=100, axisBreaks=6)
pPubStrTrDensity <- plotPopulation('pubStrTrDensity', 90, 'Public Street Tree Density per Street Mile', 'Trees per Mile', '%.10s',
                                   precision=1, axisPrecision=0, lowerBound=0, upperBound=120, axisBreaks=6)

#Combined Tree Density
#Add all individually, then divide by sum of all

#Managed Public Trees per Capita
rManPubTrPerCap <- plotRegion('manPubTrPerCap', .5, 'Managed Public Trees per Capita', 'Trees per Capita', '%.10s',
                              precision=3, axisPrecision=1, lowerBound=0, upperBound=1.4, axisBreaks=6)
pManPubTrPerCap <- plotPopulation('manPubTrPerCap', .5, 'Managed Public Trees per Capita', 'Trees per Capita', '%.10s',
                                  precision=3, axisPrecision=1, lowerBound=0, upperBound=1.4, axisBreaks=6)
#Managed Public Trees per Employee 
rManPubTrPerEmp <- plotRegion('manPubTrPerEmp', 8000, 'Managed Public Trees per Employee', 'Trees per Employee', '%.10s',
                              precision=0, axisPrecision=0, lowerBound=0, upperBound=15000, axisBreaks=6)
pManPubTrPerEmp <- plotPopulation('manPubTrPerEmp', 8000, 'Managed Public Trees per Employee', 'Trees per Employee', '%.10s',
                                  precision=0, axisPrecision=0, lowerBound=0, upperBound=22000, axisBreaks=6)

#Public Tree Planting Stability
rPlantedTrStab <- plotRegion('plantedTrStab', 20, 'Public Tree Planting Stability', 'Change(# of Trees)', '%.10s',
                             precision=1, axisPrecision=0, lowerBound=-125, upperBound=150, axisBreaks=8)
pPlantedTrStab <- plotPopulation('plantedTrStab', 20, 'Public Tree Planting Stability', 'Change(# of Trees)', '%.10s',
                                 precision=1, axisPrecision=0, lowerBound=-125, upperBound=800, axisBreaks=8)

#### Index Plotting ####
rTreeCity <- plotRegion('tcScore', 100, 'Tree City USA Standards', 'Score (%)', '%.10s',
                        precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pTreeCity <- plotPopulation('tcScore', 100, 'Tree City USA Standards', 'Score (%)', '%.10s',
                            precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)

rCARS <- plotRegion('carsScore', 100, 'Community Accomplishment Reporting System Score', 'Score (%)', '%.10s',
                    precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pCARS <- plotPopulation('carsScore', 100, 'Community Accomplishment Reporting System Score', 'Score (%)', '%.10s',
                        precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)

rSMA <- plotRegion('smaScore', 100, 'Society of Municipal Arborists Accreditation Program Score', 'Score (%)', '%.10s',
                   precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pSMA <- plotPopulation('smaScore', 100, 'Society of Municipal Arborists Accreditation Program Score', 'Score (%)', '%.10s',
                       precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)

rCMRaw <- plotRegion('cmScore', 80, 'Urban Forest Sustainability Score', 'Score', '%.10s',
                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pCMRaw <- plotPopulation('cmScore', 80, 'Urban Forest Sustainability Score', 'Score', '%.10s',
                         precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)

rCMRelative <- plotRegion('cmRelativeScore', 100, 'Urban Forest Sustainability Relative Score', 'Score', '%.10s',
                          precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pCMRelative <- plotPopulation('cmRelativeScore', 100, 'Urban Forest Sustainability Relative Score', 'Score', '%.10s',
                              precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
#Resource Management
rCMResourceManagementRaw <- plotRegion('cmResourceManagement', 36, 'Urban Forest Sustainability Resource Management Score', 'Score', '%.10s',
                                       precision=1, axisPrecision=0, lowerBound=0, upperBound=40, axisBreaks=6)
pCMResourceManagementRaw <- plotPopulation('cmResourceManagement', 36, 'Urban Forest Sustainability Resource Management Score', 'Score', '%.10s',
                                           precision=1, axisPrecision=0, lowerBound=0, upperBound=40, axisBreaks=6)

rCMResourceManagementRelative <- plotRegion('cmResourceManagementRelative', 100, 'Urban Forest Sustainability Resource Management Relative Score', 'Score', '%.10s',
                                            precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pCMResourceManagementRelative <- plotPopulation('cmResourceManagementRelative', 100, 'Urban Forest Sustainability Resource Management Relative Score', 'Score', '%.10s',
                                                precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
#Community Framework
rCMCommunityFrameworkRaw <- plotRegion('cmCommunityFramework', 28, 'Urban Forest Sustainability Community Framework Score', 'Score', '%.10s',
                                       precision=1, axisPrecision=0, lowerBound=0, upperBound=32, axisBreaks=6)
pCMCommunityFrameworkRaw <- plotPopulation('cmCommunityFramework', 28, 'Urban Forest Sustainability Community Framework Score', 'Score', '%.10s',
                                           precision=1, axisPrecision=0, lowerBound=0, upperBound=32, axisBreaks=6)

rCMCommunityFrameworkRelative <- plotRegion('cmCommunityFrameworkRelative', 100, 'Urban Forest Sustainability Community Framework Relative Score', 'Score', '%.10s',
                                            precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pCMCommunityFrameworkRelative <- plotPopulation('cmCommunityFrameworkRelative', 100, 'Urban Forest Sustainability Community Framework Relative Score', 'Score', '%.10s',
                                                precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
#Vegetation Resource
rCMVegetationResourceRaw <- plotRegion('cmVegetationResource', 16, 'Urban Forest Sustainability Vegetation Resource Score', 'Score', '%.10s',
                                       precision=1, axisPrecision=0, lowerBound=0, upperBound=20, axisBreaks=6)
pCMVegetationResourceRaw <- plotPopulation('cmVegetationResource', 16, 'Urban Forest Sustainability Vegetation Resource Score', 'Score', '%.10s',
                                           precision=1, axisPrecision=0, lowerBound=0, upperBound=20, axisBreaks=6)

rCMVegetationResourceRelative <- plotRegion('cmVegetationResourceRelative', 100, 'Urban Forest Sustainability Vegetation Resource Relative Score', 'Score', '%.10s',
                                            precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
pCMVegetationResourceRelative <- plotPopulation('cmVegetationResourceRelative', 100, 'Urban Forest Sustainability Vegetation Resource Relative Score', 'Score', '%.10s',
                                                precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)

#### Combine Plots for Output ####
#Remove legends from individual plots for cleaner output

#Tree Budget
rTreeBudget <- grid.arrange(grobs = list(rDollarsPerCapita + theme(legend.position="none"),
                                         rDollarsPerPublicTree + theme(legend.position="none"),
                                         rPercentOfMuniBud + theme(legend.position="none"),
                                         rBudgetNeeds + theme(legend.position="none")),
                            layout_matrix = rbind(c(1,2),
                                                  c(3,4)))

pTreeBudget <- grid.arrange(grobs = list(pDollarsPerCapita + theme(legend.position="none"),
                                         pDollarsPerPublicTree + theme(legend.position="none"),
                                         pPercentOfMuniBud + theme(legend.position="none"),
                                         pBudgetNeeds + theme(legend.position="none")),
                            layout_matrix = rbind(c(1,2),
                                                  c(3,4)))

#Governance
rGovernance <- grid.arrange(grobs = list(rTreeBoard + theme(legend.position="none"),
                                         rWrittenStratPlan  + theme(legend.position="none"),
                                         rOrdinance + theme(legend.position="none"),
                                         rOrdinanceYear + theme(legend.position="none")),
                            layout_matrix = rbind(c(1,2),
                                                  c(3,4)))

pGovernance <- grid.arrange(grobs = list(pTreeBoard + theme(legend.position="none"),
                                         pWrittenStratPlan  + theme(legend.position="none"),
                                         pOrdinance + theme(legend.position="none"),
                                         pOrdinanceYear + theme(legend.position="none")),
                            layout_matrix = rbind(c(1,2),
                                                  c(3,4)))

#Trees and People
rTreesAndPeople <- grid.arrange(grobs = list(rManPubTrPerCap + theme(legend.position="none"),
                                             rManPubTrPerEmp + theme(legend.position="none"),
                                             rGreenSpaceAreaMeters + theme(legend.position="none"),
                                             rGreenSpaceAreaFeet + theme(legend.position="none")), 
                                layout_matrix = rbind(c(1,2),
                                                      c(3,4)))

pTreesAndPeople <- grid.arrange(grobs = list(pManPubTrPerCap + theme(legend.position="none"),
                                             pManPubTrPerEmp + theme(legend.position="none"),
                                             pGreenSpaceAreaMeters + theme(legend.position="none"),
                                             pGreenSpaceAreaFeet + theme(legend.position="none")),
                                layout_matrix = rbind(c(1,2),
                                                      c(3,4)))

#Tree Stocking
rTreeStocking <- grid.arrange(grobs = list(rStrTrStockingLevel + theme(legend.position="none"),
                                           rPrkTrStockingLevel + theme(legend.position="none"),
                                           rMunTrStockingLevel + theme(legend.position="none"),
                                           rPubStrTrDensity + theme(legend.position="none")),
                              layout_matrix = rbind(c(1,2),
                                                    c(3,4)))

pTreeStocking <- grid.arrange(grobs = list(pStrTrStockingLevel + theme(legend.position="none"),
                                           pPrkTrStockingLevel + theme(legend.position="none"),
                                           pMunTrStockingLevel + theme(legend.position="none"),
                                           pPubStrTrDensity + theme(legend.position="none")),
                              layout_matrix = rbind(c(1,2),
                                                    c(3,4)))

#Tree Canopy
rTreeCanopy <- grid.arrange(grobs = list(rCanCovGoal + theme(legend.position="none"),
                                         rPercentCurCan + theme(legend.position="none"),
                                         rPercentCanGoal + theme(legend.position="none"),
                                         rPercentCanProg + theme(legend.position="none")),
                            layout_matrix = rbind(c(1,2),
                                                  c(3,4)))

pTreeCanopy <- grid.arrange(grobs = list(pCanCovGoal + theme(legend.position="none"),
                                         pPercentCurCan + theme(legend.position="none"),
                                         pPercentCanGoal + theme(legend.position="none"),
                                         pPercentCanProg + theme(legend.position="none")),
                            layout_matrix = rbind(c(1,2),
                                                  c(3,4)))

#Inspection and Pruning Cycle
rInspectionAndPruningCycle <- grid.arrange(grobs = list(rCurrentPrunInsCyc + theme(legend.position="none"),
                                                        rDesiredPrunInsCyc + theme(legend.position="none"),
                                                        rYearsOfPrunInsCyc + theme(legend.position="none"),
                                                        rPercentAttPrunInsCyc + theme(legend.position="none")),
                                           layout_matrix = rbind(c(1,2),
                                                                 c(3,4)))

pInspectionAndPruningCycle <- grid.arrange(grobs = list(pCurrentPrunInsCyc + theme(legend.position="none"),
                                                        pDesiredPrunInsCyc + theme(legend.position="none"),
                                                        pYearsOfPrunInsCyc + theme(legend.position="none"),
                                                        pPercentAttPrunInsCyc + theme(legend.position="none")),
                                           layout_matrix = rbind(c(1,2),
                                                                 c(3,4)))

#Tree Inventory and Management
rTreeInventoryAndManagement <- grid.arrange(grobs = list(rActiveManagement + theme(legend.position="none"),
                                                         rTreeResourceInventory + theme(legend.position="none")),
                                            layout_matrix = rbind(c(1,2)))

pTreeInventoryAndManagement <- grid.arrange(grobs = list(pActiveManagement + theme(legend.position="none"),
                                                         pTreeResourceInventory + theme(legend.position="none")),
                                            layout_matrix = rbind(c(1,2)))

