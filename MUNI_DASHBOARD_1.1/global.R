#load all of the required packages
rm(list=ls())
# Add this later "flexdashboard",
# list.of.packages<-c("devtools","shiny","shinydashboard","dplyr","knitr","geosphere","maps","zipcode","shinyBS","gridExtra","tidyverse","shinyalert","readxl","DT","shinythemes","shinyWidgets", "zipcodeR")
#new.packages<-list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)
#library(devtools)
library(shiny)
library(shinydashboard)
# library(flexdashboard)
library(dplyr)
# library(zipcodeR)
library(tidyverse)
library(shinyBS)
library(shinyalert)
library(dashboardthemes)
library(knitr)
library(shinythemes)
# library(geosphere)
library(DT)
library(readxl)
library(gridExtra)
library(shinyWidgets)
library(maps)
library(readxl)

# data(zip_code)

#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
#read in data

dataDir <- "C:/Users/abjohnso/OneDrive - UWSP/Documents/Data Requests/MUNI_DASHBOARD_1.1/"
#dataDir <- "C:/Users/kmasarik/OneDrive - UWSP/MASARIK/R_CODE/MUNI_DASHBOARD/MUNI_DASHBOARD/"
#dataDir <- "C:/Users/Grant/GroundWaterCenter/MUNI_DASHBOARD/"

 fips<-read.csv(paste(dataDir, "fips.csv", sep = ""))
#fips<-read.csv("C:/Users/Grant/GroundWaterCenter/MUNI_DASHBOARD/fips.csv")


myzips<-read.csv(paste(dataDir, "zipcodes.csv", sep = ""), fileEncoding="UTF-8-BOM")
popdata<-read.csv(paste(dataDir, "sub-est2018_all.csv", sep = ""))
#read in data for visualization
data <- read.csv(paste(dataDir, "TreeScoreDataset.csv", sep = ""))
region <-read.csv(paste(dataDir, "TreeScoreValues.csv", sep = ""))

#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
#globally defined variables
sqMetersPerAcre <- 4046.86
sqFeetPerAcre <- 43560
#Page Title
textWidth='100%'
responseWidth='20%'
w=700
ptitle="Municipal Tree Survey"
dmgr="Demographic Information"
survey="Survey"
sec1="Community Framework"
sec23="Budget and Management"
sec6="Inventory"
sec7="Operations Profile"
sec8="Assistance Programs"
rez="Results"

#Questions and Headers for Survey
checkAll="CHECK ALL THAT APPLY"
askZip="1.\U2007What is your 5-digit ZIP Code?"
askZipb="(Please fill this out first to help us find your city)"
askCity="2.\U2007Select your city:"
dsc="Don't see your city?"
ptc="2.\U2007Please type your city name"
askRegion="4.\U2007Please select your US Census region based on the map below."
ner="Northeast (Blue)"
mwr="Midwest (Red)"
sr="South (Purple)"
wr="West (Green)"
askPop="3.\U2007What is the population of your community?"
askPopb="US Census Estimate for 2018 is generated"
T1_3MiMMS="1.\U2007How many miles of maintained municipal streets does your community have?"
T1_3MiMSWTr="2.\U2007How many miles of maintained municipal streets with trees does your community have?"
T1_3AcMMP="3.\U2007How many acres of managed municipal parks does your community have?"
T1_3AcMGS="4.\U2007How many acres of municipal natural areas or green spaces does your community have?"
T1_3AcMPWTr="5.\U2007How many acres of other municipal properties planted with trees does your community have?"
T1_6OvrcTr="6.\U2007 Does someone in your community (i.e., employee, volunteer, consultant, etc.) oversee the care of municipal street trees, park trees or other public trees?"
T1_11C="Consultant (e.g., Arborist, Forester or Retainer)"
T1_13="7.\U2007What training and/or credentials are collectively held by the staff or consultant responsible for tree activities and/or management of trees?"
T1_13NoTrain="No specific training or workshops"
T1_13InHOrJ="In-house and/or on-the-job-training"
T1_13AttTree="Attend tree care/management workshops"
T1_13ISACA="ISA Certified Arborist"
T1_13ISACMS="ISA Certified Municipal Specialist"
T1_13ISAAC="ISA Advanced Credential (BCMA, TRAQ)"
T1_13SAFCF="SAF Certified Forester"
T1_13StLorCre="State License or Credential"
T1_132yd="Two-year degree (Arboriculture, Forestry, Horticulture, Urban Forestry, or a related field)"
T1_134yd="Four-year degree (Arboriculture, Forestry, Horticulture, Urban Forestry, or a related field)"
T1_13Grad="Graduate degree (Arboriculture, Forestry, Horticulture, Urban Forestry, or a related field)"
T1_14="How strongly do you agree or disagree with these statements characterizing your community and the management of trees?"
T1_14AllOpr="1.\U2007 All city departments operate with common goals and objectives to manage trees"
T1_14PriLanOwn="2.\U2007Large private landowners embrace city-wide goals and objectives through specific resource management plans related to tree care and management in greenspaces"
T1_14GreInd="3.\U2007Green industry (e.g., tree care) at large operates with high professional standards and commits to city-wide goals and objectives"
T1_14Citzens="4.\U2007 Citizens understand and participate in urban forestry management at the neighborhood level"
T1_14Constits="5.\U2007 All constituencies in the community interact for the benefit of the urban forest"
T1_14GenPub="6.\U2007The general public understands the value of trees to the community"
T1_14CoopInt="7.\U2007 Cooperation and interaction occurs among neighboring communities and regional groups"
T1_15="8.  How many public employees, including managers, are involved with the municipal tree management program?"
T1_15TotEmp="8a.\U2007# of Total Employees"
T1_15FullTiEq="8b.\U2007# of Full Time Equivalents (2080 hour base year)"
T2_1TotBud="1.\U2007What is the total municipal budget (excluding school budget) for the current fiscal year?"
T2_3TotTreBud="2.\U2007What is the total annual budget of your municipality funded tree care activities and management from all municipal sources for the current year?"
T2_3TotTreBudb="(Include all tree activity expenses; include personnel, overhead, equipment, supplies, tree care and contract payments)"
T2_10CurrNeeds="3.\U2007Is your budget adequate to meet current needs as defined in your work plan or your identified annual urban forestry budget needs?"
T2_10CurrNeedsb="(This includes planting, maintenance, removal, inventory, education, etc.)"
T2_10Need="3a.\tWhat percent of your identified need is met?"
T2_13="4.\U2007What percent of the total tree management budget from all sources is used for the following activities?"
T2_13b="(All five sections should total 100%)"
T2_13AdminSupr="4a.\U2007 Administration/Supervision"
T2_13TrPlan="4b.\U2007Tree Planting"
T2_13TrPrun="4c.\U2007Tree Pruning"
T2_13TrRem="4d.\U2007Tree Removal"
T2_13Othr="4e.\U2007 All Other Expenses"
T3_1="5.\U2007Which of the following organizations help establish policy for tree management in your community?"
T3_1CityCo="City Council Committee or Community Board"
T3_1ParksBr="Parks Board"
T3_1PubWr="Public Works Board"
T3_1TrBoard="Tree Board"
T3_1PlanCom="Planning Commission"
T3_1ShdeTr="Shade Tree/Urban Forestry Commission"
T3_1Other="Other"
T3_2AuthTrBd="6.\U2007 Does your community have a government-authorized tree board, parks board, city department, commission, or similar group that helps develop and/or administer tree management policy?"
T3_5MuniOrd="7.\U2007 Does your municipality have one or more municipal ordinances that pertain to trees?"
T3_5YrUpdt="7a.\tIn what year was the ordinance last updated or reviewed?"
T3_6="7b.\tWhat topics do your community tree ordinances include?"
T3_61="Planting"
T3_62="Protection"
T3_63="Maintenance"
T3_6ReqPreDev="Requires preservation of trees during development"
T3_6IdPreHer="Identifies preservation of heritage or significant trees"
T3_6PhrTrTop="Prohibits tree topping"
T3_6RegTrPub="Regulates tree species which may or may not be planted on public property (approved tree list)"
T3_6RegTrPri="Regulates tree species which may or may not be planted on private property (approved tree list)"
T3_6RegPubTrMn="Requires regular public tree maintenance"
T3_6ReqTyMn="Requires particular types of maintenance (e.g. pruning)"
T3_6RegRemDd="Regulates removal of dead or diseased trees"
T3_6ReqTrND="Requires tree planting in new developments"
T3_6ReqTrNP="Requires tree planting around new parking lots"
T3_6ReqTrRP="Requires tree planting around reconstructed parking lots"
T3_6ReqRpPOT="Requires replacement of removed publicly owned trees"
T3_6ResTrCtPP="Restricts tree cutting on private property"
T3_6RegAbat="Regulates abatement of hazardous or public nuisance trees"
T3_6EstIDCS="Establishes an insect/disease control strategy"
T3_6DefTrReq="Defines tree maintenance requirements on public property"
T3_9WrtStgPln="8.\U2007 Does your community have a written strategic plan for urban forestry, tree management, open space, green infrastructure, or land use management that includes trees?"
T3_9UpdtText="8a.\tIn what year was your community's most recent review/update to any written strategic plan for urban forestry?"
T3_10="8b.\tWhich types of plans (separate or combined together) do you have that incorporate the management of trees and other vegetation?"
T3_10CityMas="City master/Comprehensive"
T3_10InsDsRead="Insect/disease readiness"
T3_10MunWtr="Municipal watershed"
T3_10StrEmer="Storm/emergency response"
T3_10StrWtrM="Storm water management"
T3_10TrRskM="Tree risk management"
T3_10UrbForMgmt="Tree/urban forest management"
T3_10UrbForStr="Urban forest strategic"
T3_10Other="Other"
T3_13="9.\U2007Which of the following standards of practice does your community officially incorporate into tree management procedures?"
T3_13AnsiZ60="ANSI Z60.1 (Nursery Stock Standard)"
T3_13AnsiA300="ANSI A300 (Tree Care Standard)"
T3_13TrCtyUSA="Tree City USA"
T3_13AnsiZ133="ANSI Z133 (Arboricultural Safety Standard)"
T3_13ISABMP="ISA Best Management Practices"
T5_4="1.\U2007Which of the following are true about contractors hired by your city?"
T5_4ANSIZ6="Require the use of ANSI Z60.1 standards (Nursery Stock Standard)"
T5_4ANSIZ1="Require the use of ANSI Z133 standards (Tree Care Standard)"
T5_4ANSIA="Require use of ANSI A300 standards (Arboricultural Safety Standard)"
T5_4PrefISA="Hiring preference given to ISA Certified Arborists"
T5_4PrefTCI="Hiring preference given to TCIA Accredited Companies"
T6_1TrInv="1.\U2007 Does your community have a tree inventory?"
T6_1TrInvb="(An inventory is any type of record of public trees in your community.)"
T6_3YrUpd="1a.\tIn what year was your most recent update to your tree inventory?"
T6_6="1b.\tWhat areas does the tree inventory include?"
T6_6StrTr="Street trees"
T6_6PrkTr="Park trees"
T6_6MunGr="Municipal greenbelts"
T6_6MunWd="Municipal woodlots"
T6_6PriTr="Private trees (sample survey)"
T6_6Other="Other municipal properties"
T6_7="1c.\tWhich type of inventory collection and analysis methods have been used to describe your community tree population?"
T6_7WndsSr="Windshield survey"
T6_7SmpSr="Sample survey"
T6_7Census="100% population (total, census)"
T6_7RmSens="Remote sensing (i.e., aerial photos, satellites)"
T6_7CanCo="Canopy cover analysis"
T6_7iTrStr="i-Tree Streets analysis"
T6_7iTrEco="i-Tree Eco analysis"
T6_7TreDm="Tree diameter collected"
T6_7TrePlLoc="Tree planting locations"
T6_7SelTrS="Selection of tree species for planting"
T6_8GPSGIS="GPS/GIS"
T6_15CanGl="2.\U2007 Does your municipality have a tree canopy goal?"
T6_16CanGl="2a.\tWhat is your percent tree canopy cover goal?"
T6_16CurCn="2b.\tWhat is your current percent tree canopy cover?"
T6_16YrToGl="2c.\tWhat year is identified to meet your canopy goal?"
T6_18PubTr="3.\U2007What is the total number of publicly owned trees in your community?"
T6_18PubTrLoc="4.\U2007What is the total number of planting locations in your community?"
T6_19="5.\U2007What is the total number of publicly owned trees in the following locations?"
T6_19StrTr="5a.\U2007Street trees"
T6_19StrTrb="(along municipal right of way, between curb and sidewalk, alley trees, etc.)"
T6_19PrkTr="5b.\U2007Park trees"
T6_19PrkTrb="(maintained areas)"
T6_19MuniTr="5c.\U2007Municipal trees on other property"
T6_19MuniTrb="(building grounds, cemeteries, treatment plants, industrial parks, etc.)"
T6_20="6.\U2007How many empty/vacant spaces do you have for potential tree plantings in the following locations?"
T6_20StrTr="6a.\U2007Street trees"
T6_20StrTrb="(along municipal right of way, between curb and sidewalk, alley trees, etc.)"
T6_20PrkTr="6b.\U2007Park trees"
T6_20PrkTrb="(maintained areas)"
T6_20MuniTr="6c.\U2007Municipal trees on other property"
T6_20MuniTrb="(building grounds, cemeteries, treatment plants, industrial parks, etc.)"
T6_21ValPubTr="If known, what is the value of the publicly owned trees?"
T6_22FinRec="Is the value of publicly owned trees carried in the city financial records as a city asset?"
T7_1TrPlnt="2.\U2007What is the average number of trees planted on all municipal properties in the past five years?"
T7_1TrRem="3.\U2007What is the average number of trees removed on all municipal properties in the past five years?"
T7_2="4.\U2007What percent of tree care (pruning, pest control, etc.) is done on a systematic (regularly scheduled) cycle and what percent on demand as reactive (complaints, hazardous situations, crisis, post storm, etc.)? (Total=100%)"
T7_2SysSch="4a.\U2007% Systematic (Scheduled)"
T7_2ReaDem="4b.\U2007% Reactive (on Demand)"
T7_4AppPrun="8.\U2007How would you best describe your tree management program's approach to inspection/pruning?"
T7_4DP="No scheduled pruning"
T7_4PrunAsNeed="Pruning as needed/for emergency only"
T7_4RegPrun="Regular pruning cycle"
T7_4Other="Other"
T7_4CurCyc="8a.\tCurrent inspection/pruning cycle (in years)?"
T7_4DesCyc="8b.\tDesired inspection/pruning cycle (in years)?"
T7_12TrRskM="5.\U2007 Does your community regularly conduct tree risk management (hazard tree identification)?"
T7_14WritTrRsk="6.\U2007 Does your community have a written tree risk management policy?"
T7_15="7.\U2007Which of the following statements reflects your overall tactic to tree risk inspection?"
T7_15RspCC="Response to citizen complaints"
T7_15RSKInsp="Part of a routine tree risk inspection program"
T7_15RtTrM="Part of a routine tree maintenance"
T7_15SrvyID="Windshield survey to identify high risk trees"
T7_15Other="Other"
T7_17="9.\U2007When a public tree is removed, which of the following are typical ways that solid wood/residue is disposed of?"
T7_17BfErgy="Biofuel for energy"
T7_17BrnOp="Burned in open"
T7_17FrWd="Firewood"
T7_17Landfilled="Landfilled"
T7_17MdFrn="Made into furniture/flooring/art"
T7_17Mulch="Mulch"
T7_17ProLum="Processed into lumber"
T7_17SleWd="Sale of round wood (e.g., saw logs, pulp, veneer)"
T7_17Other="Other"
T8_4AwdCm="1.\U2007 During the past five years have you received any awards from any group for your community tree activities or management?"
T8_5EduPr="2.\U2007 Do municipal staff provide educational presentations to city residents in regard to tree care?"
T7_10TecAst="3.\U2007 Does your community provide technical assistance (information) for tree maintenance on private property?"
T7_10FinAst="4.\U2007 Does your community provide financial assistance for specific insect or diseased tree removal on private property?"
T8_8TrCity="5.\U2007Is your community currently a Tree City USA?"
T8_10ObvsP="5.\U2007 Does your community have an Arbor Day observance and proclamation?"
intNatVeg="9.\U2007What is your level of integration of native vegetation into your program do you use?"
intNVLow="Low (No program of integration of native vegetation)"
intNVMod="Moderate (Voluntary use of native vegetation on public projects)"
intNVGood="Good (Require native vegetation on project-appropriate basis)"
intNVOpt="Optimal (Preservation of regional biodiviersity and use of native vegetation in projects)"

#Info buttons
ZIP="ZIP Code"
pop="Population"
reg="US Census Regions"
regInfo="Alaska and Hawaii fall\n into the West US Census\n defined region." #leave '\n' for new lines
plzClick="Please click the button below and complete the table: "
click="Click here"

#Submit buttons/end of page stuff
submit="Submit"
begin="Begin Survey"
highPop="Your population entered is more than 10% greater than our records"
lowPop="Your population entered is more than 10% less than out records"
success="Submitted!"
failure="Incomplete!"
toNext="Please continue to the next section."
tryAgain="Try Again!"
missings="You have not answered each question."
not100="Your percent of tree care that is systematic and reactive do not add up to 100%"
n="No"
y="Yes"
d="Developing"
tblCplt="You have successfully completed the table.  Please continue with the survey."
tblIncplt="You have not completed the table."
compSelText="How would you like to compare?"

CMap="US Census Regions"
CMSTitle="Northeast=Blue,Midwest=Red,South=Purple,West=Green"
ne="Northeast"
mw="Midwest"
south="South"
west="West"

tableScores<-rep(0,7)
text<-c(T1_14AllOpr,T1_14PriLanOwn,T1_14GreInd,T1_14Citzens,T1_14Constits,T1_14GenPub,T1_14CoopInt)
responses<-c("Strongly Disagree","Disagree","Agree","Strongly Agree")

cinfo="US Census Categories"
theAnalysis="The analysis was calculated on "
theAnalysis2=" using the MyTreeScore Dashhboard"
copyrightDate="2019"
indPageLine2="NA=not applicable, community does not have this value"
indPageLine3="Regional and National US Census from 2014 Municipal Tree Care and Management in the United States"
indPageLine4a="(Search Hauer Muni or "
indPageLine4b=" for more information)"
#end of globally defined variables
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------


#helper functions
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
#decimal places
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

#matrix for page 2 table
m = matrix(
  as.character(1:4), nrow = 7, ncol = 4, byrow = TRUE,
  dimnames = list(text[1:7], responses[1:4])
)
for (i in seq_len(nrow(m))) {
  for(j in seq_len(ncol(m))){
    m[i, j] = sprintf(
      '<input type="radio" name="%s" value="%s"/>',
      text[i], m[i, j], ifelse(j==1, 'checked="checked"', "")
    )
  }
}
m

#find the cities based on the zip code provided
getCities<-function(zip){
  index<-which(zip==myzips[,1])
  foundCity<-str_to_title(myzips$City[index])
  foundCity<-append(foundCity,"Other",after=length(foundCity))
  return(foundCity)
}

#return the population of the selected  city
getpop<-function(zip,city){
  location=subset(myzips,Zipcode==zip)
  stateAbr=location[1,3]
  state<-state.name[match(stateAbr,state.abb)]
  m<-subset(popdata, grepl(city,NAME,ignore.case=TRUE) & STNAME == state)
  return(max(m$POPESTIMATE2018[1]))
}

#return the region from zip code
getState<-function(zip){
  index<-which(zip==myzips[,1])
  foundState<-myzips$State[index]
  return(foundState[1])
}

#sum the veg res part of Clark Matheny Model
sumvegres<-function(cc,ageDist,smix,nveg){
  (if(cc == vr1l) {1}
   else if(cc == vr1m) {2}
   else if(cc == vr1g) {3}
   else if(cc == vr1o) {4}) +
    (if(ageDist == vr2l) {1}
     else if(ageDist == vr2m) {2}
     else if(ageDist == vr2g) {3}
     else if(ageDist == vr2o) {4}) +
    (if(smix == vr3l) {1}
     else if(smix == vr3m) {2}
     else if(smix == vr3g) {3}
     else if(smix == vr3o) {4}) +
    (if(nveg == vr4l) {1}
     else if(nveg == vr4m) {2}
     else if(nveg == vr4g) {3}
     else if(nveg == vr4o) {4})
}

#group by population
groupPop<-function(pop){
  if(pop>=1000000){
    return("More than 1,000,000")
  }
  else if(pop>500000){
    return("500,000 to 999,999")
  }
  else if(pop>250000){
    return("250,000 to 499,999")
  }
  else if(pop>100000){
    return("100,000 to 249,999")
  }
  else if(pop>50000){
    return("50,000 to 99,999")
  }
  else if(pop>25000){
    return("25,000 to 49,999")
  }
  else if(pop>10000){
    return("10,000 to 24,999")
  }
  else if(pop>5000){
    return("5,000 to 9,999")
  }
  else{
    return("Less than 4,999")
  }
}

#test for printing the region--can be deleted later
printReg<-function(region){
  if(region==ner){
    return(ne)
  }
  else if(region==mwr){
    return(mw)
  }
  else if(region==sr){
    return(south)
  }
  else if(region==wr){
    return(west)
  }
}

#sum percentage of budget to make sure they total 100%
sumBudPercent<-function(Admin,TPl,TPr,TRm,O){
  x<-0
  Admin<-ifelse(is_empty(Admin),0,Admin)
  TPl<-ifelse(is_empty(TPl),0,TPl)
  TPr<-ifelse(is_empty(TPr),0,TPr)
  TRm<-ifelse(is_empty(TRm),0,TRm)
  O<-ifelse(is_empty(O),0,O)
  x<-sum(Admin,TPl,TPr,TRm,O)
  return(x)
}

#visualizations functions
#Color scheme for graphing based on where the cities population falls
userColor <- c('My TreeScore' = 'forestgreen',
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

userColor2 <- c('My TreeScore' = 'forestgreen',
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

nationalAverage <- function(dataVar) {
  
  cleanData <- data %>% drop_na(!!sym(dataVar)) %>% summarise(SummaryValue = mean(!!sym(dataVar)))
  
  return(cleanData)
}

#Plotting
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
      axis.text.x = element_text(face = 'bold', margin = margin(t = 0, r = 0, b = 0, l = 0)), #size = 12, angle = 45
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(size = 12),
      axis.text.y = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)),
      #legend.direction = 'horizontal', 
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      #axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
      axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
      panel.background = element_rect(fill = "white"))
  
  return(plottedData)
}

#Plotting
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
      axis.text.x = element_text(face = 'bold', angle = 20, margin = margin(t = 5, r = 0, b = 0, l = 0)),  
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(size = 12),
      axis.text.y = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)),
      #legend.direction = 'horizontal', 
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      #axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
      axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
      panel.background = element_rect(fill = "white"))
  
  return(plottedData)
}

#Combines the cleaning and plotting steps with a ordering to get a graph
plotRegion <- function(graphingVar, userData, title, yAxis, format, precision, axisPrecision, lowerBound, upperBound, axisBreaks) {
  
  nationalValue <- pull(nationalAverage(graphingVar))
  
  userTibble <- tibble(Region = 'My TreeScore', RegionOrder =  0, SummaryValue =  userData)
  nationalTibble <- tibble(Region = 'National', RegionOrder = 100, SummaryValue = nationalValue)
  
  cleanData <- singleRemoveAndGroup(graphingVar, 'Region', 'RegionOrder')
  cleanData <- bind_rows(cleanData, userTibble, nationalTibble)
  
  cleanData <- cleanData %>% ungroup(Region)
  cleanData <- cleanData %>% mutate(Region = fct_reorder(Region, RegionOrder))
  
  plottedData <- plotCleanedDataRegionBar(cleanData, 'Region', 'SummaryValue', title, yAxis, userColor2, 
                                          format, precision, axisPrecision, lowerBound, upperBound, axisBreaks)
  
  return(plottedData)
}

plotPopulation <- function(graphingVar, userData, title, yAxis, format, precision, axisPrecision, lowerBound, upperBound, axisBreaks) {
  
  nationalValue <- pull(nationalAverage(graphingVar))
  
  userTibble <- tibble(PopulationGroup = 'My TreeScore', PopulationGroupOrder =  0, SummaryValue =  userData)
  nationalTibble <- tibble(PopulationGroup = 'National', PopulationGroupOrder = 100, SummaryValue = nationalValue)
  
  cleanData <- singleRemoveAndGroup(graphingVar, 'PopulationGroup', 'PopulationGroupOrder')
  cleanData <- bind_rows(cleanData, userTibble, nationalTibble)
  
  cleanData <- cleanData %>% ungroup(PopulationGroup)
  cleanData <- cleanData %>% mutate(PopulationGroup = fct_reorder(PopulationGroup, PopulationGroupOrder))
  
  plottedData <- plotCleanedDataPopulationBar(cleanData, 'PopulationGroup', 'SummaryValue', title, yAxis, userColor, 
                                              format, precision, axisPrecision, lowerBound, upperBound, axisBreaks)
  
  return(plottedData)
}
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------


source('ui.R', local = TRUE)
source('Server.R', local = TRUE)


# Run the application
shinyApp(ui = UI, server = SERVER)

