#load all of the required packages
rm(list=ls())
list.of.packages<-c("devtools","shiny","shinydashboard","flexdashboard","dplyr","maps","zipcode","shinyBS","gridExtra","tidyverse","shinyalert","readxl","DT","shinythemes","shinyWidgets")
new.packages<-list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(devtools)
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(dplyr)
library(zipcode)
library(tidyverse)
library(shinyBS)
library(shinyalert)
library(dashboardthemes)
library(shinythemes)
library(geosphere)
library(DT)
library(readxl)
library(gridExtra)
library(shinyWidgets)
library(maps)
data(zipcode)

#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
#read in data
fips<-read_csv("//uwsp.edu/files/CNR/Projects/MuniDashboard/fips.csv")
myzips<-read_csv("//uwsp.edu/files/CNR/Projects/MuniDashboard/zipcodes.csv")
popdata<-read_csv("//uwsp.edu/files/CNR/Projects/MuniDashboard/sub-est2018_all.csv")
#read in data for visualization
dataDir <- "//uwsp.edu/files/CNR/Projects/MuniDashboard/2014 Survey Data/R Export Test/"
data <- read_excel(paste(dataDir, "TreeScoreDataset.xlsx", sep = ""))

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
T1_11C="Consultant (e.g., Arborist, Forester on Retainer)"
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
T2_10Need="3a.\tPercent below your identified budget need"
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
T3_9UpdtText="8a.\tIn what year was your communities most recent review/update to any written strategic plan for urban forestry?"
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
T7_2="4.\U2007What percent of tree care (pruning, pest control, etc.) is done on a systematic (regularly scheduled) cycle and what percent on demand as reactive (complaints, hazardous situations, crisis, post storm etc.)? (Total=100%)"
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
  rownames(zipcode)=zipcode$zip
  state<-zipcode[zip,3]
  state<-state.name[match(state,state.abb)]
  m<-subset(popdata, grepl(city,NAME) & STNAME == state)
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
#In the UI we will have the correct colors picked by an if then;
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

shinyApp(
  ui=tagList(
    #get rid of the up down arrows on numeric input
    tags$style(HTML("
        input[type=number] {
                    -moz-appearance:textfield;
                    }
                    input[type=number]::{
                    -moz-appearance:textfield;
                    }
                    input[type=number]::-webkit-outer-spin-button,
                    input[type=number]::-webkit-inner-spin-button {
                    -webkit-appearance: none;
                    margin: 0;
                    }
                    ")),
    # tags$head(tags$style(HTML('td:nth-child(1){
    #                           width:300px;
    #                           }
    #                           td:nth-child(2){
    #                           width:100px;
    #                           }
    #                           td:nth-child(3){
    #                           width:100px;
    #                           }
    #                           td:nth-child(4){
    #                           width:100px;
    #                           }
    #                           td:nth-child(5){
    #                           width:100px;
    #                           }'
    # ))),
    #add cushion at top so everything is readable with top bar fixed
    tags$style(type="text/css","body{padding-top: 70px;}"),
    navbarPage(id="survey","",
      #theme can be changed here
      theme=shinytheme('flatly'),
      #fix the top panel
      position="fixed-top",
      #Introduction tab--update text and link to pdf, adding image somehow??
      tabPanel("Introduction",
               h2("Muni Trees/App Title",align="center",width=textWidth),
               #line across screen to seperate title from content
               hr(),
               h4("This page will have some introductory statement about the app and a downloadable PDF of the questions asked/info needed to complete."),
               h4(a("Information needed to complete the survey",href="https://www.uwsp.edu/cnr/Documents/MTCUS%20-%20Forestry/Municipal%202014%20Final%20Report.pdf")),
               br(),
               #submit button centered
               fluidRow(
                 column(6, align="center", offset=2,
                        actionButton("beginSurvey",begin),
                        tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
                 ))),
      #Master tab with all subsections of survey
      navbarMenu("MyTreeScore Survey",
      #make the demographics tab
      tabPanel("Demographics",
               mainPanel(useShinyalert(),
                         h3(dmgr,align="center",width=textWidth),
               #line across screen to seperate title from content           
               hr(),
               #ZIP Code
               strong(askZip,width=textWidth),
               em(askZipb,width=textWidth),
               textInput("ZIP",
                         "",width=responseWidth),
               #city drop down
               strong(askCity,width=textWidth),
               selectInput("city",
                           "",
                           choices=NULL,selected=character(0),width=responseWidth),
               #let them type city
               uiOutput("textCity"),
               #population input
               strong(askPop,width=textWidth),
               em(askPopb,width=textWidth),
               numericInput("pop",
                            "",
                            value=character(0),
                            width=responseWidth),
               #get region
               strong(askRegion,width=textWidth),
               selectInput("region",
                           "",
                           c("",ner,mwr,sr,wr),width=responseWidth),
               #Plot the US Census Regions
               strong(reg),
               plotOutput("plot1",width='50%'),
               br(),
               #submit button cenered
               fluidRow(
                  column(6, align="center", offset=2,
                         actionButton("subdemo",submit),
                         tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
                  ))
               )),           
      #create the statements about community tab
      tabPanel("Statements About Community",
               h3("Statements About Your Community",align="center",width=textWidth),
               #line to seperate title and content
               hr(),
               strong(T1_14),
               #button to create table
               h5(plzClick),
               br(),
               actionBttn("btnContinue",click),
               br()),
      #create the community framework tab
      tabPanel("Community Framework",
               mainPanel(h3(sec1,align="center",width=textWidth),
                         #line to seperate the title and content
                         hr(),
                         #Sec 1 Q 3
                         strong(T1_3MiMMS,width=textWidth),
                         numericInput("Q1_3MiMMS","",value=character(0),step=.01,width=responseWidth),
                         strong(T1_3MiMSWTr,width=textWidth),
                         numericInput("Q1_3MiMSWTr","",value=character(0),step=.01,width=responseWidth),
                         strong(T1_3AcMMP,width=textWidth),
                         numericInput("Q1_3AcMMP","",value=character(0),step=.01,width=responseWidth),
                         strong(T1_3AcMGS,width=textWidth),
                         numericInput("Q1_3AcMGS","",value=character(0),step=.01,width=responseWidth),
                         strong(T1_3AcMPWTr,width=textWidth),
                         numericInput("Q1_3AcMPWTr","",value=character(0),step=.01,width=responseWidth),
                         strong(T1_6OvrcTr,align="center",width=textWidth),
                         #Sec 1 Q6
                         radioButtons("Q1_6OvrcTr",
                                      "",
                                      c(y,n),
                                      selected=character(0),width=responseWidth),
                         #Sec 1 Q 13
                         strong(T1_13,width=textWidth),
                         em(checkAll,width=textWidth),
                         checkboxGroupInput("Q1_13",
                                            "",
                                            c(T1_13NoTrain,T1_13InHOrJ,T1_13AttTree,T1_13ISACA,T1_13ISACMS,T1_13ISAAC,T1_13SAFCF,T1_13StLorCre,T1_132yd,T1_134yd,T1_13Grad,T1_11C),
                                            selected=character(0),width=textWidth),
                         #Sec 1 Q 15
                         strong(T1_15,width=textWidth),
                         br(),
                         br(),
                         strong(T1_15TotEmp,width=textWidth),
                         numericInput("Q1_15TotEmp","",value=character(0),step=1,width=responseWidth),
                         strong(T1_15FullTiEq,width=textWidth),
                         numericInput("Q1_15FullTiEq","",value=character(0),step=0.05,width=responseWidth),
                         #native vegetation question
                         strong(intNatVeg,width=textWidth),
                         br(),
                         radioButtons("QintNatVeg",
                                      "",
                                      c(intNVLow,intNVMod,intNVGood,intNVOpt),
                                      selected=character(0),width=textWidth),
                         br(),
                         br(),
                         #submit button centered
                         fluidRow(
                           column(6, align="center", offset = 2,
                                  actionButton("subcf",submit),
                                  tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
                           )))),
      #create the budget and management tab
      tabPanel("Budget and Management",
               mainPanel(h3(sec23,align="center",width=textWidth),
                         #line to seperate title and content
                         hr(),
                         #Sec 2 Q 1
                         strong(T2_1TotBud,width=textWidth),
                         numericInput("Q2_1TotBud","",value=character(0),step=0.01,width=responseWidth),
                         #Sec 2 Q 3
                         strong(T2_3TotTreBud,width=textWidth),
                         em(T2_3TotTreBudb,width=textWidth),
                         numericInput("Q2_3TotTreBud","",value=character(0),step=0.01,width=responseWidth),
                         #Sec 2 Q 10
                         strong(T2_10CurrNeeds,width=textWidth),
                         em(T2_10CurrNeedsb,width=textWidth),
                         radioButtons("Q2_10CurrNeeds",
                                      "",
                                      c(y,n),
                                      selected=character(0),width=responseWidth),
                         #Sec 2 Q 10 Getting unmet need
                         htmlOutput("unmetNeedText"),
                         uiOutput("unmetNeed"),
                         #Sec 2 Q 13
                         strong(T2_13,width=textWidth),
                         em(T2_13b,width=textWidth),
                         br(),
                         br(),
                         strong(T2_13AdminSupr,width=textWidth),
                         numericInput("Q2_13AdminSupr","",value=character(0),step=1,width=responseWidth),
                         strong(T2_13TrPlan,width=textWidth),
                         numericInput("Q2_13TrPlan","",value=character(0),step=1,width=responseWidth),
                         strong(T2_13TrPrun,width=textWidth),
                         numericInput("Q2_13TrPrun","",value=character(0),step=1,width=responseWidth),
                         strong(T2_13TrRem,width=textWidth),
                         numericInput("Q2_13TrRem","",value=character(0),step=1,width=responseWidth),
                         strong(T2_13Othr,width=textWidth),
                         numericInput("Q2_13Othr","",value=character(0),step=1,width=responseWidth),
                         htmlOutput("budPercentSum"),
                         br(),
                         #Sec 3 Q 1
                         strong(T3_1,width=textWidth),
                         em(checkAll,width=textWidth),
                         checkboxGroupInput("Q3_1",
                                            "",
                                            c(T3_1CityCo,T3_1ParksBr,T3_1PubWr,T3_1TrBoard,T3_1PlanCom,T3_1ShdeTr,T3_1Other,"None of the above"),
                                            selected=character(0),width=textWidth),
                         #Sec 3 Q 2
                         strong(T3_2AuthTrBd,width=textWidth),
                         radioButtons("Q3_2AuthTrBd",
                                      "",
                                      c(y,n),
                                      selected=character(0),width=responseWidth),
                         #Sec 3 Q 5
                         strong(T3_5MuniOrd,width=textWidth),
                         radioButtons("Q3_5MuniOrd",
                                      "",
                                      c(y,n,d),
                                      selected=character(0),width=responseWidth),
                         #Get what year their ordinance was updated and what the ordinances are if they have one
                         htmlOutput("muniOrdUpdtText"),
                         uiOutput("muniOrdUpdt"),
                         htmlOutput("muniOrdText"),
                         br(),
                         uiOutput("muniOrd1"),
                         uiOutput("muniOrd2"),
                         uiOutput("muniOrd3"),
                         #Sec 3 Q 9
                         strong(T3_9WrtStgPln,width=textWidth),
                         radioButtons("Q3_9WrtStgPln",
                                      "",
                                      c(y,n,d),
                                      selected=character(0),width=responseWidth),
                         #Get what year their written plan was developed and what their written plan includes
                         htmlOutput("writtenPlanUpdtText"),
                         uiOutput("writtenPlanUpdt"),
                         htmlOutput("writtenPlanText"),
                         uiOutput("writtenPlan"),
                         #Sec 3 Q 13
                         strong(T3_13,width=textWidth),
                         em(checkAll,width=textWidth),
                         checkboxGroupInput("Q3_13",
                                            "",
                                            c(T3_13AnsiA300,T3_13AnsiZ60,T3_13AnsiZ133,T3_13ISABMP,T3_13TrCtyUSA),
                                            selected=character(0),width=textWidth),
                         br(),
                         #center submit button
                         fluidRow(
                           column(6, align="center", offset = 2,
                                  actionButton("subbm",submit),
                                  tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
                           )))),
      #Create the inventory tab
      tabPanel("Inventory",
                mainPanel(h3(sec6,align="center",width=textWidth),
                          #line seperating title and content
                          hr(),
                          #Sec 6 Q 1
                          strong(T6_1TrInv,width=textWidth),
                          em(T6_1TrInvb,width=textWidth),
                          radioButtons("Q6_1TrInv",
                                       "",
                                       c(y,n),
                                       selected=character(0),width=responseWidth),
                          #Get what year their tree inventory was last updated and what it includes if they have one
                          htmlOutput("trInvUpdtText"),
                          uiOutput("trInvUpdt"),
                          htmlOutput("trInvText"),
                          uiOutput("trInvIncl"),
                          htmlOutput("invCollText"),
                          uiOutput("invColl"),
                          #Sec 6 Q 15
                          strong(T6_15CanGl,width=textWidth),
                          radioButtons("Q6_15CanGl",
                                       "",
                                       c(y,n,d),
                                       selected=character(0),width=responseWidth),
                          #If they have a canopy goal, get what the goal is, what they are currently at and how many years they have to reach the goal
                          htmlOutput("canGoalText"),
                          uiOutput("canGoal"),
                          htmlOutput("canCurrText"),
                          uiOutput("canCurr"),
                          htmlOutput("yearsToGlText"),
                          uiOutput("yearsToGl"),
                          #Sec 6 Q 18
                          strong(T6_18PubTr,width=textWidth),
                          numericInput("Q6_18PubTr","",value=character(0),step=1,width=responseWidth),
                          strong(T6_18PubTrLoc,width=textWidth),
                          numericInput("Q6_18PubTrLoc","",value=character(0),step=1,width=responseWidth),
                          br(),
                          #Sec 6 Q 19
                          strong(T6_19,width=textWidth),
                          br(),
                          br(),
                          strong(T6_19StrTr,width=textWidth),
                          em(T6_19StrTrb,width=textWidth),
                          numericInput("Q6_19StrTr","",value=character(0),step=1,width=responseWidth),
                          strong(T6_19PrkTr,width=textWidth),
                          em(T6_19PrkTrb,width=textWidth),
                          numericInput("Q6_19PrkTr","",value=character(0),step=1,width=responseWidth),
                          strong(T6_19MuniTr,width=textWidth),
                          em(T6_19MuniTrb,width=textWidth),
                          numericInput("Q6_19MuniTr","",value=character(0),step=1,width=responseWidth),
                          br(),
                          #Sec 6 Q 20
                          strong(T6_20,width=textWidth),
                          br(),
                          br(),
                          strong(T6_20StrTr,width=textWidth),
                          em(T6_20StrTrb,width=textWidth),
                          numericInput("Q6_20StrTr","",value=character(0),step=1,width=responseWidth),
                          strong(T6_20PrkTr,width=textWidth),
                          em(T6_20PrkTrb,width=textWidth),
                          numericInput("Q6_20PrkTr","",value=character(0),step=1,width=responseWidth),
                          strong(T6_20MuniTr,width=textWidth),
                          em(T6_20MuniTrb,width=textWidth),
                          numericInput("Q6_20MuniTr","",value=character(0),step=1,width=responseWidth),
                          br(),
                          #center submit button
                          fluidRow(
                            column(6, align="center", offset = 2,
                                   actionButton("subci",submit),
                                   tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))))),
      #Create the operations proile tab
      tabPanel("Operations Profile",
               mainPanel(h3(sec7,align="center",width=textWidth),
                         #line seperating the title and content
                         hr(),
                         #Sec 5 Q 4
                         strong(T5_4,width=textWidth),
                         em(checkAll,width=textWidth),
                         checkboxGroupInput("Q5_4",
                                            "",
                                            c(T5_4ANSIA,T5_4ANSIZ6,T5_4ANSIZ1,T5_4PrefISA,T5_4PrefTCI),
                                            selected=character(0),width=textWidth),
                         #Sec 7 Q 1
                         strong(T7_1TrPlnt,width=textWidth),
                         numericInput("Q7_1TrPlnt","",value=character(0),step=1,width=responseWidth),
                         strong(T7_1TrRem,width=textWidth),
                         numericInput("Q7_1TrRem","",value=character(0),step=1,width=responseWidth),
                         strong(T7_2,width=textWidth),
                         br(),
                         br(),
                         #Sec 7 Q 2
                         strong(T7_2SysSch,width=textWidth),
                         numericInput("Q7_2SysSch","",value=character(0),width=responseWidth),
                         strong(T7_2ReaDem,width=textWidth),
                         numericInput("Q7_2ReaDem","",value=character(0),width=responseWidth),
                         #Sum Systematic and Reactive percentages
                         htmlOutput("TCPer"),
                         br(),
                         #Sec 7 Q 12
                         strong(T7_12TrRskM,width=textWidth),
                         radioButtons("Q7_12TrRskM",
                                      "",
                                      c(y,n),
                                      selected=character(0),width=responseWidth),
                         #Sec 7 Q 14
                         strong(T7_14WritTrRsk,width=textWidth),
                         radioButtons("Q7_14WritTrRsk",
                                      "",
                                      c(y,n),
                                      selected=character(0),width=responseWidth),
                         #Sec 7 Q 15
                         strong(T7_15,width=textWidth),
                         em(checkAll,width=textWidth),
                         checkboxGroupInput("Q7_15",
                                            "",
                                            c(T7_15RspCC,T7_15RSKInsp,T7_15RtTrM,T7_15SrvyID,T7_15Other),
                                            selected=character(0),width=textWidth),
                         #Sec 7 Q 4
                         strong(T7_4AppPrun,width=textWidth),
                         radioButtons("Q7_4AppPrun",
                                      "",
                                      c(T7_4DP,T7_4PrunAsNeed,T7_4RegPrun),
                                      selected=character(0),width=textWidth),
                         #Get their current and desired pruning cycle if they have a regular cycle
                         htmlOutput("currPrunStatement"),
                         uiOutput("currPrun"),
                         htmlOutput("desPrunStatement"),
                         uiOutput("desPrun"),
                         #Sec 7 Q 17
                         strong(T7_17,width=textWidth),
                         em(checkAll,width=textWidth),
                         checkboxGroupInput("Q7_17",
                                            "",
                                            c(T7_17BfErgy,T7_17BrnOp,T7_17FrWd,T7_17Landfilled,T7_17MdFrn,T7_17Mulch,T7_17ProLum,T7_17SleWd,T7_17Other),
                                            selected=character(0),width=textWidth),
                         #Center submit button
                         fluidRow(
                           column(6, align="center", offset = 2,
                                  actionButton("sub7",submit),
                                  tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))))),
      #Create the assistance programs tab
      tabPanel("Assistance Programs",
               mainPanel(h3(sec8,align="center",width=textWidth),
                         #line seperating title and content
                         hr(),
                         #Sec 8 Q 4
                         strong(T8_4AwdCm,width=textWidth),
                         radioButtons("Q8_4AwdCm",
                                      "",
                                      c(y,n),
                                      selected=character(0),width=responseWidth),
                         #Sec 8 Q 5
                         strong(T8_5EduPr,width=textWidth),
                         radioButtons("Q8_5EduPr",
                                      "",
                                      c(y,n),
                                      selected=character(0),width=responseWidth),
                         #Sec 7 Q 10 Technical Assistance
                         strong(T7_10TecAst,width=textWidth),
                         radioButtons("Q7_10TecAst",
                                      "",
                                      c(y,n),
                                      selected=character(0),width=responseWidth),
                         #Sec 7 Q 10 Financial Assistance
                         strong(T7_10FinAst,width=textWidth),
                         radioButtons("Q7_10FinAst",
                                      "",
                                      c(y,n),
                                      selected=character(0),width=responseWidth),
                         #Sec 8 Q 10
                         strong(T8_10ObvsP,width=textWidth),
                         radioButtons("Q8_10ObvsP",
                                      "",
                                      c(y,n),
                                      selected=character(0),width=responseWidth),
                         #center Submit button
                         fluidRow(
                           column(6, align="center", offset = 2,
                                  actionButton("sub8",submit),
                                  tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")))))),
      #TreeScore Indicators Master Tab
      navbarMenu("MyTreeScore Indicators",
      #12 tabs-one for each of the 12 indicators?
      tabPanel("Tree Budget",
               h3("Tree Budget",align="center"),
               hr(),
               radioButtons("compSelTB",
                            compSelText,
                            c(reg,pop),
                            width=responseWidth,inline=F),
               plotOutput("plotTB",height='800px')
               ),
      
      tabPanel("Governance",
               h3("Governanace",align="center"),
               hr(),
               radioButtons("compSelG",
                            compSelText,
                            c(reg,pop),
                            width=responseWidth,inline=F),
               plotOutput("plotG",height='800px')
               ),
      tabPanel("Trees and People",
               h3("Trees and People",align="center"),
               hr(),
               radioButtons("compSelTP",
                            compSelText,
                            c(reg,pop),
                            width=responseWidth,inline=F),
               plotOutput("plotTP",height='800px')),
      tabPanel("Tree Stocking",
               h3("Tree Stocking",align="center"),
               hr(),
               radioButtons("compSelTS",
                            compSelText,
                            c(reg,pop),
                            width=responseWidth,inline=F),
               plotOutput("plotTS",height='800px')),
      tabPanel("Tree Canopy",
               h3("Tree Canopy",align="center"),
               hr(),
               radioButtons("compSelTC",
                            compSelText,
                            c(reg,pop),
                            width=responseWidth,inline=F),
               plotOutput("plotTC",height='800px')
               ),
      tabPanel("Inspection and Pruning Cycle",
               h3("Inspection and Pruning Cycle",align="center"),
               hr(),
               radioButtons("compSelIPC",
                            compSelText,
                            c(reg,pop),
                            width=responseWidth,inline=F),
               plotOutput("plotIPC",height='800px')),
      tabPanel("Tree Inventory and Management",
               h3("Tree Inventory and Management",align="center"),
               hr(),
               radioButtons("compSelTIM",
                            compSelText,
                            c(reg,pop),
                            width=responseWidth,inline=F),
               plotOutput("plotTIM",height='400px'))),
      #TreeScore Model Mastertab
      navbarMenu("MyTreeScore Indexes",
      #Create a tab for the CARS Model
      tabPanel("CARS",
               h3("CARS Model",align="center"),
               hr(),
               radioButtons("compSelCARS",
                           compSelText,
                           c(reg,pop),
                           width=responseWidth,inline=F),
               plotOutput("plotCARS",height='400px')
      ),
      #Create a tab for the Clark Matheny Model
      tabPanel("Clark Matheny",
               h3("Clark Matheny Model",align="center"),
               hr(),
               radioButtons("compSelCM",
                            compSelText,
                            c(reg,pop),
                            width=responseWidth,inline=F),
               plotOutput("plotCM",height='400px')
      ),
      #Create a tab for the SMA Model
      tabPanel("Society of Municipal Arborists",
               h3("Society of Municipal Arborists Model",align="center"),
               hr(),
               radioButtons("compSelSMA",
                            compSelText,
                            c(reg,pop),
                            width=responseWidth,inline=F),
               plotOutput("plotSMA",height='400px')
      ),
      #Create a tab for the Tree City USA Model
      tabPanel("Tree City USA",
               h3("Tree City USA Model",align="center"),
               hr(),
               radioButtons("compSelTCUSA",
                            compSelText,
                            c(reg,pop),
                            width=responseWidth,inline=F),
               plotOutput("plotTCUSA",height='400px')
      )
               ),
      #TreeScore Summary Tab
      navbarMenu("MyTreeScore Summary",
      tabPanel("MyTreeScore Indicators Summary",
               htmlOutput("myTSIndiHeader"),
               hr(),
               h4(cinfo,align="center",width=5),
               tableOutput("myTSIndic1"),
               hr(),
               htmlOutput("endMsg1"),
               htmlOutput("endMsg2"),
               htmlOutput("endMsg3"),
               htmlOutput("endMsg4")
               ),
      tabPanel("MyTreeScore Indexes Summary",
               htmlOutput("myTSIndeHeader"),
               hr())
    )
    )
    ),
  
  server=function(session,input,output){
    #us map
    output$plot1<-renderPlot({
      width<-session$cliendData$output_plot1_width
      height<-session$clientData$output_plot1_height
      states<-map_data("state")
      #northeast map
      ne<-which(states$region %in% c("maine","new hampshire","vermont","massachusetts","rhode island","connecticut","new york","pennsylvania","new jersey"))
      ne<-states[ne,]
      #midwest map
      mw<-which(states$region %in% c("north dakota","south dakota","nebraska","kansas","missouri","iowa","minnesota","wisconsin","michigan","ohio","indiana","illinois"))
      mw<-states[mw,]
      #south map
      s<-which(states$region %in% c("texas","oklahoma","arkansas","louisiana","mississippi","alabama","georgia","florida","tennessee","kentucky","south carolina","north carolina","virginia","west virginia","delaware","maryland","district of columbia"))
      s<-states[s,]
      #west map
      w<-which(states$region %in% c("new mexico","colorado","wyoming","montana","idaho","utah","arizona","nevada","california","oregon","washington"))
      w<-states[w,]
      #us regional map
      ggplot(data=ne)+
        geom_polygon(aes(x=long,y=lat,group=group),fill="blue",color="black")+
        geom_polygon(data=mw,aes(x=long,y=lat,group=group),fill="red",color="black")+
        geom_polygon(data=s,aes(x=long,y=lat,group=group),fill="purple",color="black")+
        geom_polygon(data=w,aes(x=long,y=lat,group=group),fill="green",color="black")+
        labs(subtitle=CMSTitle)+
        annotate("text",x=-95,y=42.5,label="Midwest",size=12,angle=27)+
        annotate("text",x=-88,y=34,label="South",size=12,angle=27)+
        annotate("text",x=-74,y=43.5,label="Northeast",size=12,angle=27)+
        annotate("text",x=-113,y=40,label="West",size=12,angle=27)+
        annotate("text",x=-115,y=27,label=regInfo,size=5,color="red")+
        theme_bw()+
        theme(panel.background=element_rect(fill="cyan",color="cyan"),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              axis.title=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank())
        })
    #update city based on entered zip code
    observe({
      req(input$ZIP)
      x<-getCities(input$ZIP)
      updateSelectInput(session,"city","",choices=x)
    })
    #update population for the selected city
    observe({
      if(input$city=="Other"){
        req(input$tCity)
        areapop<-getpop(input$ZIP,input$tCity)
        updateNumericInput(session,"pop","",
                           value=unique(areapop),min=1,max=9000000,step=1)
      }
      else{
        areapop<-getpop(input$ZIP,input$city)
        updateNumericInput(session,"pop","",
                           value=unique(areapop),min=1,max=9000000,step=1)
      }
    })
    #update region based on location provided
    observe({
      req(input$ZIP)
      x<-getState(input$ZIP)
      if(!is_empty(x)){
        updateSelectInput(session,"region",
                          "",
                          c("",ner,mwr,sr,wr),
                          selected=(if(x %in% c("ME","NH","vT","NY","MA","RI","CT","NJ","PA")){ner}
                                   else if(x %in% c("OH","IN","IL","MI","WI","MN","IA","MO","NE","KS","ND","SD")){mwr}
                                   else if(x %in% c("DE","MD","DC","WV","VA","TN","SC","NC","FL","AL","AR","LA","KY","OK","TX","GA")){sr}
                                   else if(x %in% c("NM","AZ","CO","WY","MT","ID","UT","NV","WA","OR","CA","HI","AK")){wr}
                                   else{""}))}
      else{updateSelectInput(session,"region",
                             "",
                             c("",ner,mwr,sr,wr))}
    })

    # create the page 2 datatable
    dtWithRadioButton<-reactiveValues(dt = m)
    
    observeEvent(input$btnContinue,{
      showModal(modalDialog(
        title=paste(T1_14,"Please answer each question"),
        size="l",easyClose=FALSE, fade=TRUE,
        DTOutput("datatable"),
        footer=tagList(
          actionBttn(inputId="btnProcess",label=submit,style="float",size="md",color="success")
        )
      ))
    })
    #creat the table for Sec 1 Q 14
    output$datatable = renderDT(
      datatable(dtWithRadioButton$dt, selection = "none", escape=FALSE,
                options= list(
                  dom = 't',
                  paging = FALSE,
                  ordering = FALSE
                 ),
                callback = JS(
                  "table.rows().every(function(i, tab, row) {
                     var $this = $(this.node());
                     $this.attr('id', this.data()[0]);
                     $this.addClass('shiny-input-radiogroup');
});
                     Shiny.unbindAll(table.table().node());
                     Shiny.bindAll(table.table().node());"),
                rownames = TRUE), 
      server = FALSE
    )
    #display table on button click
    observeEvent(input$btnProcess,{
      dt<-dtWithRadioButton$dt
      x<-rep(0,7)
      i=1
      for(resp in text){
        req(input[[resp]])
        x[i]<-input[[resp]]
        i=i+1
      }
      tableScores<<-x
      newtab <- switch(input$survey, "Statements About Community" = "Community Framework","Community Framework" = "Statements About Community")
      updateTabItems(session, "survey", newtab) 
      removeModal(session)
    })
    
    #allow user to type in city
    output$textCity<-renderUI({
      req(input$city)
      if(input$city=="Other"){
      textInput("tCity","2a. Please type in your city",width=responseWidth)
      }
    })
    #text for % unmet need S2 Q10
    output$unmetNeedText<-renderText({
      req(input$Q2_10CurrNeeds)
      if(input$Q2_10CurrNeeds==n){
        paste("<b>",T2_10Need,"</b>")
      }
    })
    #ask what % of need is not met for S2 Q10 if they say need is not met
    output$unmetNeed<-renderUI({
      req(input$Q2_10CurrNeeds)
      if(input$Q2_10CurrNeeds==n){
        numericInput("Q2_10Need","",value=character(0),step=1,width=responseWidth)
      }
    })
    #ask what other for sec 3 q 1
    output$otherOrg<-renderUI({
      if(input$Q3_1Other){
        textInput("other31",ask31Other,width=responseWidth)
      }
    })
    #text for when muni ord was last written/updated
    output$muniOrdUpdtText<-renderText({
      req(input$Q3_5MuniOrd)
      if(input$Q3_5MuniOrd==y){
        paste("<b>",T3_5YrUpdt,"</b>")
      }
    })
    #ask when their muni ord was last written/updated
    output$muniOrdUpdt<-renderUI({
      req(input$Q3_5MuniOrd)
      if(input$Q3_5MuniOrd==y){
        numericInput("Q3_5YrUpdt","",value=character(0),step=1,width=responseWidth)
      }
    })
    #text for muni ord
    output$muniOrdText<-renderText({
      req(input$Q3_5MuniOrd)
      if(input$Q3_5MuniOrd==y){
      paste("<b>",T3_6,"</b>","<em>",checkAll,"</em>")
      }
    })
    #ask what muni ordinence they have about planting
    output$muniOrd1<-renderUI({
      req(input$Q3_5MuniOrd)
      if(input$Q3_5MuniOrd==y){
        checkboxGroupInput("Q3_61",
                           T3_61,
                           c(T3_6RegTrPub,T3_6RegTrPri,T3_6ReqTrND,T3_6ReqTrNP,T3_6ReqTrRP,T3_6ReqRpPOT),
                           selected=character(0),width=textWidth)
      }
    })
    #ask what muni ordinence they have about
    output$muniOrd2<-renderUI({
      req(input$Q3_5MuniOrd)
      if(input$Q3_5MuniOrd==y){
        checkboxGroupInput("Q3_62",
                           T3_62,
                           c(T3_6ReqPreDev,T3_6ResTrCtPP,T3_6RegAbat,T3_6IdPreHer),
                           selected=character(0),width=textWidth)
      }
    })
    #ask what muni ordinence they have about
    output$muniOrd3<-renderUI({
      req(input$Q3_5MuniOrd)
      if(input$Q3_5MuniOrd==y){
        checkboxGroupInput("Q3_63",
                           T3_63,
                           c(T3_6RegPubTrMn,T3_6ReqTyMn,T3_6RegRemDd,T3_6EstIDCS,T3_6DefTrReq,T3_6PhrTrTop),
                           selected=character(0),width=textWidth)
      }
    })
    #text for what year written plan was last updated
    output$writtenPlanUpdtText<-renderText({
      req(input$Q3_9WrtStgPln)
      if(input$Q3_9WrtStgPln==y){
        paste("<b>",T3_9UpdtText,"</b>")
      }
    })
    #ask what year the written plan was last updated
    output$writtenPlanUpdt<-renderUI({
      req(input$Q3_9WrtStgPln)
      if(input$Q3_9WrtStgPln==y){
        numericInput("Q3_9Updt","",value=character(0),step=1,width=responseWidth)
      }
    })
    #text for what the written plan entails
    output$writtenPlanText<-renderText({
      req(input$Q3_9WrtStgPln)
      if(input$Q3_9WrtStgPln==y){
        paste("<b>",T3_10,"</b>","<em>",checkAll,"</em>")
      }
    })
    #ask what written plan entails
    output$writtenPlan<-renderUI({
      req(input$Q3_9WrtStgPln)
      if(input$Q3_9WrtStgPln==y){
        checkboxGroupInput("Q3_10",
                           "",
                           c(T3_10CityMas,T3_10InsDsRead,T3_10MunWtr,T3_10StrEmer,T3_10StrWtrM,T3_10TrRskM,T3_10UrbForMgmt,T3_10UrbForStr,T3_10Other),
                           selected=character(0),width=textWidth)
      }
    })
    # allow user to specify other plan
    output$otherMngmt<-renderUI({
      if(input$Q3_10Other){
        textInput("other310",ask310Other,width=responseWidth)
      }
    })
    #text for year tree inventory was last updated
    output$trInvUpdtText<-renderText({
      req(input$Q6_1TrInv)
      if(input$Q6_1TrInv==y){
      paste("<b>",T6_3YrUpd,"</b>")
      }
    })
    #get what year the tree inventory was last updated
    output$trInvUpdt<-renderUI({
      req(input$Q6_1TrInv)
      if(input$Q6_1TrInv==y){
      numericInput("Q6_3YrUpd","",value=character(0),step=1,width=responseWidth)
      }
    })

    #text for tree inventory inclusion
    output$trInvText<-renderText({
      req(input$Q6_1TrInv)
      if(input$Q6_1TrInv==y){
       paste("<b>",T6_6,"</b>","<em>",checkAll,"</em>")
      }
    })
    #ask what the tree inventory includes
    output$trInvIncl<-renderUI({
      req(input$Q6_1TrInv)
      if(input$Q6_1TrInv==y){
         checkboxGroupInput("Q6_6",
                            "",
                            c(T6_6StrTr,T6_6PrkTr,T6_6MunGr,T6_6MunWd,T6_6PriTr,T6_6Other),
                            selected=character(0),width=textWidth)
      }
    })
    #text for tree inventory collection method
    output$invCollText<-renderText({
      req(input$Q6_1TrInv)
      if(input$Q6_1TrInv==y){
        paste("<b>",T6_7,"</b>","<em>",checkAll,"</em>")
      }
    })
    #ask what the tree inventory collection method is
    output$invColl<-renderUI({
      req(input$Q6_1TrInv)
      if(input$Q6_1TrInv==y){
        checkboxGroupInput("Q6_7",
                           "",
                           c(T6_7WndsSr,T6_7SmpSr,T6_7Census,T6_7RmSens,T6_7CanCo,T6_7iTrStr,T6_7iTrEco,T6_8GPSGIS,T6_7TreDm,T6_7TrePlLoc,T6_7SelTrS),
                           selected=character(0),width=textWidth)
      }
    })
    #text for canopy goal
    output$canGoalText<-renderText({
      req(input$Q6_15CanGl)
      if(input$Q6_15CanGl==y){
        paste("<b>",T6_16CanGl,"</b>")
      }
    })
    #ask what the canopy goal % is
    output$canGoal<-renderUI({
      req(input$Q6_15CanGl)
      if(input$Q6_15CanGl==y){
        numericInput("Q6_16CanGl","",value=character(0),step=1,width=responseWidth)
      }
    })
    #text for what the current canopy is
    output$canCurrText<-renderText({
      req(input$Q6_15CanGl)
      if(input$Q6_15CanGl==y){
        paste("<b>",T6_16CurCn,"</b>")
      }
    })
    #ask what the current canopy % is
    output$canCurr<-renderUI({
      req(input$Q6_15CanGl)
      if(input$Q6_15CanGl==y){
        numericInput("Q6_16CurCn","",value=character(0),step=1,width=responseWidth)
      }
    })
    #text for how many years to accomplish the canopy goal
    output$yearsToGlText<-renderText({
      req(input$Q6_15CanGl)
      if(input$Q6_15CanGl==y){
        paste("<b>",T6_16YrToGl,"</b>")
      }
    })
    #ask how many years they have to accomplish the goal
    output$yearsToGl<-renderUI({
      req(input$Q6_15CanGl)
      if(input$Q6_15CanGl==y){
        numericInput("Q6_16YrToGl","",value=character(0),step=1,width=responseWidth)
      }
    })
    #print the statement for regular pruning
    output$currPrunStatement<-renderText({
      req(input$Q7_4AppPrun)
      if(input$Q7_4AppPrun==T7_4RegPrun){
        paste("<b>",T7_4CurCyc,"</b>")
      }
    })
    #get the current pruning cycle
    output$currPrun<-renderUI({
      req(input$Q7_4AppPrun)
      if(input$Q7_4AppPrun==T7_4RegPrun){
        numericInput("Q7_4CurCyc","",value=character(0),step=1,width=responseWidth)
      }
    })
    #print the statement for desired pruning cycle
    output$desPrunStatement<-renderText({
      req(input$Q7_4AppPrun)
      if(input$Q7_4AppPrun==T7_4RegPrun){
        paste("<b>",T7_4DesCyc,"</b>")
      }
    })
    #get the desired pruning cycle
    output$desPrun<-renderUI({
      req(input$Q7_4AppPrun)
      if(input$Q7_4AppPrun==T7_4RegPrun){
        numericInput("Q7_4DesCyc","",value=character(0),step=1,width=responseWidth)
      }
    })
   
    #advance from into to survey
    observeEvent(input$beginSurvey,{
     newtab<-switch(input$survey, "Introduction" = "Demographics","Demographics" = "Introduction")
     updateTabItems(session,"survey",newtab)
    })
    #demographics submit button responses and advance
    observeEvent(input$subdemo,{
      if(input$ZIP==""){
        shinyalert(failure,"Please enter a zip code",type="warning")
      }
      else if(input$city=="Other"&input$tCity==""){
        shinyalert("failure","Please enter a city",type="warning")
      }
      else if(is.na(input$pop)){
        shinyalert(failure,"Please enter a population",type="warning")
      }
      else if(input$region==""){
        shinyalert(failure,"Please select your region",type="warning")
      }
      else if(input$city=="Other"){
        if(length(getpop(input$ZIP,input$tCity))==0){
          newtab <- switch(input$survey, "Demographics" = "Statements About Community","Statements About Community" = "Demographics")
          updateTabItems(session, "survey", newtab)  
        }
        else if(input$pop >= 1.1*unique(getpop(input$ZIP,input$tCity))){
          shinyalert(highPop,paste("Our records show a population of ",unique(getpop(input$ZIP,input$tCity))," for ",input$tCity,".  Please check your entry.  If it is correct, proceed.  If it is not correct, re-enter population and re-submit.", sep=""),
                     showCancelButton=T,cancelButtonText="Fix Population",
                     confirmButtonText="Continue",type="warning",closeOnClickOutside=T,closeOnEsc=T,
                     callbackR=function(x){if(x!=FALSE){
            newtab <- switch(input$survey, "Demographics" = "Statements About Community","Statements About Community" = "Demographics")
            updateTabItems(session, "survey", newtab)}}
            )
          }
        else if(input$pop <= 0.9*unique(getpop(input$ZIP,input$tCity))){
          shinyalert(lowPop,paste("Our records show a population of ",unique(getpop(input$ZIP,input$tCity))," for ",input$tCity,".  Please check your entry.  If it is correct, proceed.  If it is not correct, re-enter population and re-submit.", sep=""),
                     showCancelButton=T,cancelButtonText="Fix Population",
                     confirmButtonText="Continue",type="warning",closeOnClickOutside=T,closeOnEsc=T,
                     callbackR=function(x){if(x!=FALSE){
                       newtab <- switch(input$survey, "Demographics" = "Statements About Community","Statements About Community" = "Demographics")
                       updateTabItems(session, "survey", newtab)}})
        }
        else{
          newtab <- switch(input$survey, "Demographics" = "Statements About Community","Statements About Community" = "Demographics")
          updateTabItems(session, "survey", newtab)    
        }
      }
      else{
        if(length(getpop(input$ZIP,input$city))==0){
          newtab <- switch(input$survey, "Demographics" = "Statements About Community","Statements About Community" = "Demographics")
          updateTabItems(session, "survey", newtab)  
        }
        else if(input$pop >= 1.1*unique(getpop(input$ZIP,input$city))){
          shinyalert(highPop,paste("Our records show a population of ",unique(getpop(input$ZIP,input$city))," for ",input$city,".  Please check your entry.  If it is correct, proceed.  If it is not correct, re-enter population and re-submit.", sep=""),
                     showCancelButton=T,cancelButtonText="Fix Population",
                     confirmButtonText="Continue",type="warning",closeOnClickOutside=T,closeOnEsc=T,
                     callbackR=function(x){if(x!=FALSE){
                       newtab <- switch(input$survey, "Demographics" = "Statements About Community","Statements About Community" = "Demographics")
                       updateTabItems(session, "survey", newtab)}})
        }
        else if(input$pop <= 0.9*unique(getpop(input$ZIP,input$city))){
          shinyalert(lowPop,paste("Our records show a population of ",unique(getpop(input$ZIP,input$city))," for ",input$city,".  Please check your entry.  If it is correct, proceed.  If it is not correct, re-enter population and re-submit.", sep=""),
                     showCancelButton=T,cancelButtonText="Fix Population",
                     confirmButtonText="Continue",type="warning",closeOnClickOutside=T,closeOnEsc=T,
                     callbackR=function(x){if(x!=FALSE){
                       newtab <- switch(input$survey, "Demographics" = "Statements About Community","Statements About Community" = "Demographics")
                       updateTabItems(session, "survey", newtab)}})
        }
        else{
          newtab <- switch(input$survey, "Demographics" = "Statements About Community","Statements About Community" = "Demographics")
          updateTabItems(session, "survey", newtab)    
        }
      }
    })
    #community framework submit button and advance page
    observeEvent(input$subcf,{
      if(is.na(input$Q1_15TotEmp)|is.na(input$Q1_15FullTiEq)|is.na(input$Q1_3MiMMS)|is.na(input$Q1_3MiMSWTr)|is.na(input$Q1_3AcMMP)|is.na(input$Q1_3AcMGS)|is.na(input$Q1_3AcMPWTr)|is_empty(input$Q1_6OvrcTr)|is_empty(input$Q1_13)|is_empty(input$QintNatVeg)){
        shinyalert(failure,paste("You have not finished question(s): ",if(is.na(input$Q1_3MiMMS)){"1."},if(is.na(input$Q1_3MiMSWTr)){"2."},if(is.na(input$Q1_3AcMMP)){"3."},if(is.na(input$Q1_3AcMGS)){"4."},if(is.na(input$Q1_3AcMPWTr)){"5."},if(is_empty(input$Q1_6OvrcTr)){"6."},if(is_empty(input$Q1_13)){"7."},if(is.na(input$Q1_15TotEmp)){"8a."},if(is.na(input$Q1_15FullTiEq)){"8b."},if(is_empty(input$QintNatVeg)){"9."}),type="error")
      }
      else if(input$Q1_15TotEmp<input$Q1_15FullTiEq){
        shinyalert(failure,"You have more Full Time Equivelent than Total Employees",type="error")
      }
      else if(input$Q1_3MiMSWTr>input$Q1_3MiMMS){
        shinyalert(failure,"You have more miles of streets with trees than miles of streets",type="error")
      }
      else{
        newtab <- switch(input$survey, "Community Framework" = "Budget and Management","Budget and Management" = "Community Framework")
        updateTabItems(session, "survey", newtab)
      }
    })
    
    #budget and management submit button and advance page
    observeEvent(input$subbm,{
      if(is.na(input$Q2_1TotBud)|is.na(input$Q2_3TotTreBud)|is_empty(input$Q2_10CurrNeeds)|is.na(input$Q2_13AdminSupr)|is.na(input$Q2_13TrPlan)|is.na(input$Q2_13TrPrun)|is.na(input$Q2_13TrRem)|is.na(input$Q2_13Othr)|is_empty(input$Q3_1)|is_empty(input$Q3_2AuthTrBd)|is_empty(input$Q3_5MuniOrd)|is_empty(input$Q3_9WrtStgPln)|is_empty(input$Q3_13)){
        shinyalert(failure,paste("You have not finished question(s): ",if(is.na(input$Q2_1TotBud)){"1."},if(is.na(input$Q2_3TotTreBud)){"2."},if(is_empty(input$Q2_10CurrNeeds)){"3."},if(is.na(input$Q2_13AdminSupr)){"4a."},if(is.na(input$Q2_13TrPlan)){"4b."},if(is.na(input$Q2_13TrPrun)){"4c."},if(is.na(input$Q2_13TrRem)){"4d."},if(is.na(input$Q2_13Othr)){"4e."},if(is_empty(input$Q3_1)){"5."},
            if(is_empty(input$Q3_2AuthTrBd)){"6."},if(is_empty(input$Q3_5MuniOrd)){"7."},if(is_empty(input$Q3_9WrtStgPln)){"8."},if(is_empty(input$Q3_13)){"9."},sep=""),type="warning")
      }
      else if((input$Q3_5MuniOrd==y&(is_empty(input$Q3_61)&is_empty(input$Q3_62)&is_empty(input$Q3_63)))|(input$Q3_9WrtStgPln==y&is_empty(input$Q3_10))){
        shinyalert(failure,paste("You have not answered question(s): ",if(input$Q3_5MuniOrd==y&(is_empty(input$Q3_61)&is_empty(input$Q3_62)&is_empty(input$Q3_63))){"7b. "},if(input$Q3_9WrtStgPln==y&is_empty(input$Q3_10)){"8b. "}),type="warning")
      }
      else if(sumBudPercent(input$Q2_13AdminSupr,input$Q2_13TrPlan,input$Q2_13TrPrun,input$Q2_13TrRem,input$Q2_13Othr)!=100){
        shinyalert(failure,paste("Your percent of budgets totals ",sumBudPercent(input$Q2_13AdminSupr,input$Q2_13TrPlan,input$Q2_13TrPrun,input$Q2_13TrRem,input$Q2_13Othr),"%. It should total 100%.",sep=""),type="error")
      }
      else if((0.02*input$Q2_1TotBud)<input$Q2_3TotTreBud){
        shinyalert("Check for accuracy",paste("Are you sure your Total Budget of $",input$Q2_1TotBud," and Total Tree Budget of $",input$Q2_3TotTreBud," is accurate?",sep=""),
                   showCancelButton=T,cancelButtonText="No",
                   confirmButtonText="Yes, this is correct",type="warning",closeOnClickOutside=T,closeOnEsc=T,
                   callbackR=function(x){if(x!=FALSE){
                     newtab <- switch(input$survey, "Budget and Management" = "Inventory","Inventory" = "Budget and Management")
                     updateTabItems(session, "survey", newtab)}}) 
      }
      #This works
      else if((input$Q3_5MuniOrd==y&is.na(input$Q3_5YrUpdt))|(input$Q2_10CurrNeeds==n&is.na(input$Q2_10Need))|(input$Q3_9WrtStgPln==y&is.na(input$Q3_9Updt))){
          shinyalert(failure,paste("You have not answered question(s): ",if(input$Q2_10CurrNeeds==n&is.na(input$Q2_10Need)){"3a. "},if(input$Q3_5MuniOrd==y&is.na(input$Q3_5YrUpdt)){"7a. "},if(input$Q3_9WrtStgPln==y&is.na(input$Q3_9Updt)){"8a. "}),type="warning")
      }
      else if(input$Q2_10CurrNeeds==n&(input$Q2_10Need>100|input$Q2_10Need<0)){
        shinyalert("Whoops!","Percent need identified (3a.) must be between 0% and 100%",type="error")}
      # #update to pull year from computer
      else if(input$Q3_5MuniOrd==y&input$Q3_5YrUpdt>2019){
        shinyalert("Whoops!","Please enter a valid year and not a future date for 7a",type="error")
      }
      else if(input$Q3_9WrtStgPln==y&input$Q3_9Updt>2019){
        shinyalert("'Whoops!","Please enter a valid year and not a future date for 8a",type="error")
      }
      else{
        newtab <- switch(input$survey, "Budget and Management" = "Inventory","Inventory" = "Budget and Management")
        updateTabItems(session, "survey", newtab)
      }
    })
    
    #inventory submit and advance button
    observeEvent(input$subci,{
      if(is_empty(input$Q6_1TrInv)|is_empty(input$Q6_15CanGl)|is.na(input$Q6_18PubTr)|is.na(input$Q6_18PubTrLoc)|is.na(input$Q6_19StrTr)|is.na(input$Q6_19PrkTr)|is.na(input$Q6_19MuniTr)|is.na(input$Q6_20StrTr)|is.na(input$Q6_20PrkTr)|is.na(input$Q6_20MuniTr)){
        shinyalert(failure,paste("You have not answered question(s): ",if(is_empty(input$Q6_1TrInv)){"1. "},if(is_empty(input$Q6_15CanGl)){"2. "},if(is.na(input$Q6_18PubTr)){"3. "},if(is.na(input$Q6_18PubTrLoc)){"4. "},if(is.na(input$Q6_19StrTr)){"5a. "},
                   if(is.na(input$Q6_19PrkTr)){"5b. "},if(is.na(input$Q6_19MuniTr)){" 5c. "},if(is.na(input$Q6_20StrTr)){"6a. "},if(is.na(input$Q6_20PrkTr)){"6b. "},if(is.na(input$Q6_20MuniTr)){"6c. "}),type="warning")
      }
      else if(input$Q6_18PubTr<=sum(input$Q6_19StrTr,input$Q6_19PrkTr,input$Q6_19MuniTr)){
        shinyalert("Whoops!","You have more Street Trees (5a.), Park Trees (5b.), and Municipal Trees (5c.) than publicly owned trees in your community (3).",type="error")
      }
      else if(input$Q6_18PubTrLoc<(input$Q6_20StrTr+input$Q6_20PrkTr+input$Q6_20MuniTr)){
        shinyalert("Whoops!","You have more Street Trees (6a.), Park Trees (6b.), and Municipal Trees (6c.) than publicly owned trees in your community (4).",type="error")
      }
      else if(input$Q6_1TrInv==y&(is_empty(input$Q6_6)|is_empty(input$Q6_7))){
        shinyalert(failure,paste("You have not answered question(s): ",if(input$Q6_1TrInv==y&is_empty(input$Q6_6)){"1b. "},if(input$Q6_1TrInv==y&is_empty(input$Q6_7)){"1c. "}),type="warning")
      }
      else if((input$Q6_1TrInv==y&(is.na(input$Q6_3YrUpd)))|(input$Q6_15CanGl==y&(is.na(input$Q6_16CanGl)|is.na(input$Q6_16CurCn)|is.na(input$Q6_16YrToGl)))){
        shinyalert(failure,paste("Your have not answered question(s): ",if(input$Q6_1TrInv==y&is.na(input$Q6_3YrUpd)){"1a. "},if(input$Q6_15CanGl==y&is.na(input$Q6_16CanGl)){"2a. "},
                   if(input$Q6_15CanGl==y&is.na(input$Q6_16CurCn)){"2b. "},if(input$Q6_15CanGl==y&is.na(input$Q6_16YrToGl)){"2c. "}),type="warning")
      }
      else if(input$Q6_1TrInv==y&input$Q6_3YrUpd>2019){
        shinyalert("Whoops!","Please enter a valid year and not a future date (1a.)",type="error")
      }
      else if(input$Q6_15CanGl==y&(input$Q6_16CanGl>100|input$Q6_16CurCn>100|input$Q6_16CanGl<0|input$Q6_16CurCn<0)){
        shinyalert("Whoops!","Percentages must be between 0% and 100% (See question 2)",type="error")
      }
      else{
        newtab <- switch(input$survey, "Inventory" = "Operations Profile","Operations Profile" = "Inventory")
        updateTabItems(session, "survey", newtab)
      }
    })
    
    #operations profile submit button
    observeEvent(input$sub7,{
      if(is_empty(input$Q5_4)|is.na(input$Q7_1TrPlnt)|is.na(input$Q7_1TrRem)|is.na(input$Q7_2SysSch)|is.na(input$Q7_2ReaDem)|is_empty(input$Q7_14WritTrRsk)|is_empty(input$Q7_12TrRskM)|is_empty(input$Q7_15)|is_empty(input$Q7_4AppPrun)|is_empty(input$Q7_17)){
        shinyalert(failure,paste("You have not answered question(s): ",if(is_empty(input$Q5_4)){"1. "},if(is.na(input$Q7_1TrPlnt)){"2. "},if(is.na(input$Q7_1TrRem)){"3. "},if(is.na(input$Q7_2SysSch)){"4a. "},if(is.na(input$Q7_2ReaDem)){"4b. "},
                                 if(is_empty(input$Q7_12TrRskM)){"5. "},if(is_empty(input$Q7_14WritTrRsk)){"6. "},if(is_empty(input$Q7_15)){"7. "},if(is_empty(input$Q7_4AppPrun)){"8. "},if(is_empty(input$Q7_17)){"9."}),type="warning")
      }
      else if((input$Q7_4AppPrun==T7_4RegPrun&is.na(input$Q7_4CurCyc))|(input$Q7_4AppPrun==T7_4RegPrun&is.na(input$Q7_4DesCyc))){
        shinyalert("failure","You haven't answered the cycles",type="warning")
      }
      else if(input$Q7_4AppPrun==T7_4RegPrun&(input$Q7_2SysSch>100|input$Q7_2SysSch<0|input$Q7_2ReaDem>100|input$Q7_2ReaDem<0)){
        shinyalert("Whoops!","Percentages must be between 0% and 100%",type="error")
      }
      else if(input$Q7_4AppPrun==T7_4RegPrun&(input$Q7_2SysSch+input$Q7_2ReaDem)!=100){
        shinyalert(tryAgain,paste(not100, "Total percent is currently ",sum(input$Q7_2SysSch,input$Q7_2ReaDem),"%.",sep=""),type="error")
      }
      else{
        newtab<-switch(input$survey, "Operations Profile" = "Assistance Programs","Assistance Programs" = "Operations Profile")
        updateTabItems(session,"survey",newtab)
      }
    })
    
    #assistance programs submit button
    observeEvent(input$sub8,{
      if(is_empty(input$Q8_4AwdCm)|is_empty(input$Q8_5EduPr)|is_empty(input$Q7_10TecAst)|is_empty(input$Q7_10FinAst)|is_empty(input$Q8_10ObvsP)){
        shinyalert(failure,paste("You have not answered question(s): ",if(is_empty(input$Q8_4AwdCm)){"1. "},if(is_empty(input$Q8_5EduPr)){"2. "},if(is_empty(input$Q7_10TecAst)){"3. "},if(is_empty(input$Q7_10FinAst)){"4. "},
                                 if(is_empty(input$Q8_10ObvsP)){"5. "}),type="warning")
      }
      else{
        newtab<-switch(input$survey, "Assistance Programs" = "Tree Budget","Tree Budget" = "Assistance Programs")
        updateTabItems(session,"survey",newtab)
      }
    })
    #write total percent for budget (Q2_13)-this works but only once all  4 have numbers
    output$budPercentSum<-renderText({
      paste("<b>Total percent of budget: ", sum(input$Q2_13AdminSupr,input$Q2_13TrPlan,input$Q2_13TrPrun,input$Q2_13TrRem,input$Q2_13Othr),"</b>",sep="")
    })
    #write totalpercent for tree care schedule reactive vs systematic
    output$TCPer<-renderText({
      paste("<b>Total percent: ",sum(input$Q7_2ReaDem,input$Q7_2SysSch),"</b>",sep="")
    })
    #write population group
    output$popGroup<-renderText({
      paste("<b>Your population group is: ",groupPop(as.numeric(input$pop)),"</b>",sep="")
    })
    #write region
    output$regionText<-renderText({
      paste("<b>Your region is: ", printReg(input$region),"</b>",sep="")
    })
    #write table scores
    output$tScore<-renderText({
      paste("<b>Your table score is: ",tableScores,"</b>")
    })
    
    
    #Tree Budget indicator page
    output$plotTB<-renderPlot({
      if(input$compSelTB==reg){
        grid.arrange(grobs = list(if(is_empty(input$pop)|is_empty(input$Q2_3TotTreBud)){
                                   plotRegion('dollarsPerCapita', 0, 'Tree Budget Per Capita', '$/Capita', '%.10s',
                                               precision=2, axisPrecision=0, lowerBound=0, upperBound=15, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                  plotRegion('dollarsPerCapita', (input$Q2_3TotTreBud/input$pop), 'Tree Budget Per Capita', '$/Capita', '%.10s',
                                                                     precision=2, axisPrecision=0, lowerBound=0, upperBound=15, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q2_3TotTreBud)|is_empty(input$Q6_18PubTr)){
                                    plotRegion('dollarsPerPublicTree', 0, 'Tree Budget Per Public Tree', '$/Tree', '%.10s',
                                               precision=2, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                  plotRegion('dollarsPerPublicTree', (input$Q2_3TotTreBud/input$Q6_18PubTr), 'Tree Budget Per Public Tree', '$/Tree', '%.10s',
                                             precision=2, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q2_3TotTreBud)|is_empty(input$Q2_1TotBud)){
                                    plotRegion('percentOfMuniBud', 0, 'Tree Budget % of Total Municipal Budget', 'Percent', '%.10s',
                                               precision=2, axisPrecision=1, lowerBound=0, upperBound=1.4, axisBreaks=8) + theme(legend.position="none")
                                  }
                                  else{
                                  plotRegion('percentOfMuniBud', (input$Q2_3TotTreBud/input$Q2_1TotBud), 'Tree Budget % of Total Municipal Budget', 'Percent', '%.10s',
                                             precision=2, axisPrecision=1, lowerBound=0, upperBound=1.4, axisBreaks=8) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q2_10CurrNeeds)){
                                    plotRegion('budgetNeeds', 0, 'Tree Budget % of Identified Need', 'Percent', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else if(input$Q2_10CurrNeeds==n){
                                    plotRegion('budgetNeeds', input$Q2_10Need, 'Tree Budget % of Identified Need', 'Percent', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                  plotRegion('budgetNeeds', 100, 'Tree Budget % of Identified Need', 'Percent', '%.10s',
                                             precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")}),
                     layout_matrix = rbind(c(1,2),
                                           c(3,4)))
      }
      else if(input$compSelTB==pop){
        grid.arrange(grobs = list(if(is_empty(input$pop)|is_empty(input$Q2_3TotTreBud)){
                                   plotPopulation('dollarsPerCapita', 0, 'Tree Budget Per Capita', '$/Capita', '%.10s',
                                                   precision=2, axisPrecision=0, lowerBound=0, upperBound=15, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                  plotPopulation('dollarsPerCapita', (input$Q2_3TotTreBud/input$pop), 'Tree Budget Per Capita', '$/Capita', '%.10s',
                                                                         precision=2, axisPrecision=0, lowerBound=0, upperBound=15, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q2_3TotTreBud)|is_empty(input$Q6_18PubTr)){
                                    plotPopulation('dollarsPerPublicTree', 0, 'Tree Budget Per Public Tree', '$/Tree', '%.10s',
                                                   precision=2, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                  plotPopulation('dollarsPerPublicTree', (input$Q2_3TotTreBud/input$Q6_18PubTr), 'Tree Budget Per Public Tree', '$/Tree', '%.10s',
                                                 precision=2, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q2_10CurrNeeds)){
                                    plotPopulation('percentOfMuniBud', 0, 'Tree Budget % of Total Municipal Budget', 'Percent', '%.10s',
                                                   precision=2, axisPrecision=1, lowerBound=0, upperBound=1.4, axisBreaks=8) + theme(legend.position="none")
                                  }
                                  else{
                                  plotPopulation('percentOfMuniBud', (input$Q2_3TotTreBud/input$Q2_1TotBud), 'Tree Budget % of Total Municipal Budget', 'Percent', '%.10s',
                                                 precision=2, axisPrecision=1, lowerBound=0, upperBound=1.4, axisBreaks=8) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q2_10CurrNeeds)){
                                    plotPopulation('budgetNeeds', 0, 'Tree Budget % of Identified Need', 'Percent', '%.10s',
                                                   precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else if(input$Q2_10CurrNeeds==n){
                                    plotPopulation('budgetNeeds', input$Q2_10Need, 'Tree Budget % of Identified Need', 'Percent', '%.10s',
                                                   precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                  plotPopulation('budgetNeeds', 100, 'Tree Budget % of Identified Need', 'Percent', '%.10s',
                                                 precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")}),
                     layout_matrix = rbind(c(1,2),
                                           c(3,4)))
      }
    })
    
    #governance indicator page
    output$plotG<-renderPlot({
      if(input$compSelG==reg){
        grid.arrange(grobs = list(if(is_empty(input$Q3_2AuthTrBd)){
                                  plotRegion('treeBoard', 0, 'Community Tree Board or Related Group Exists', 'Yes (%)', '%.10s',
                                                                     precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else if(input$Q3_2AuthTrBd==y){
                                    plotRegion('treeBoard', 100, 'Community Tree Board or Related Group Exists', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotRegion('treeBoard', 0, 'Community Tree Board or Related Group Exists', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q3_9WrtStgPln)){
                                    plotRegion('writtenStratPlan', 0, 'Community has a Written Strategic Plan that Includes Trees', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")}
                                  else if(input$Q3_9WrtStgPln==y){
                                    plotRegion('writtenStratPlan', 100, 'Community has a Written Strategic Plan that Includes Trees', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotRegion('writtenStratPlan', 0, 'Community has a Written Strategic Plan that Includes Trees', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q3_5MuniOrd)){
                                  plotRegion('ordinance', 0, 'Community has a Tree Ordinance', 'Yes (%)', '%.10s',
                                             precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else if(input$Q3_5MuniOrd==y){
                                    plotRegion('ordinance', 100, 'Community has a Tree Ordinance', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotRegion('ordinance', 0, 'Community has a Tree Ordinance', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q3_5MuniOrd)){
                                  plotRegion('ordinanceYear', 0, 'Community Tree Ordinance up to Date', 'Yes (%)', '%.10s',
                                             precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else if((as.numeric(format(Sys.Date(),'%Y'))-input$Q3_5YrUpdt)<5){
                                    plotRegion('ordinanceYear', 100, 'Community Tree Ordinance up to Date', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotRegion('ordinanceYear', 0, 'Community Tree Ordinance up to Date', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }),
                     layout_matrix = rbind(c(1,2),
                                           c(3,4)))
      }
      else if(input$compSelG==pop){
        grid.arrange(grobs = list(if(is_empty(input$Q3_2AuthTrBd)){
                                  plotPopulation('treeBoard', 0, 'Community Tree Board or Related Group Exists', 'Yes (%)', '%.10s',
                                                 precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else if(input$Q3_2AuthTrBd==y){
                                    plotPopulation('treeBoard', 100, 'Community Tree Board or Related Group Exists', 'Yes (%)', '%.10s',
                                                   precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotPopulation('treeBoard', 0, 'Community Tree Board or Related Group Exists', 'Yes (%)', '%.10s',
                                                   precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q3_9WrtStgPln)){
                                    plotPopulation('writtenStratPlan', 0, 'Community has a Written Strategic Plan that Includes Trees', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")}
                                  else if(input$Q3_9WrtStgPln==y){
                                    plotPopulation('writtenStratPlan', 100, 'Community has a Written Strategic Plan that Includes Trees', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotPopulation('writtenStratPlan', 0, 'Community has a Written Strategic Plan that Includes Trees', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q3_5MuniOrd)){
                                    plotPopulation('ordinance', 0, 'Community has a Tree Ordinance', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else if(input$Q3_5MuniOrd==y){
                                    plotPopulation('ordinance', 100, 'Community has a Tree Ordinance', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotPopulation('ordinance', 0, 'Community has a Tree Ordinance', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q3_5MuniOrd)){
                                    plotPopulation('ordinanceYear', 0, 'Community Tree Ordinance up to Date', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else if((as.numeric(format(Sys.Date(),'%Y'))-input$Q3_5YrUpdt)<5){
                                    plotPopulation('ordinanceYear', 100, 'Community Tree Ordinance up to Date', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotPopulation('ordinanceYear', 0, 'Community Tree Ordinance up to Date', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }),
                     layout_matrix = rbind(c(1,2),
                                           c(3,4)))
      }
    })
    
    #Trees and People indicator page
    output$plotTP<-renderPlot({
      if(input$compSelTP==reg){
        grid.arrange(grobs = list(if(is_empty(input$Q6_18PubTr)|is_empty(input$pop)){
                                    plotRegion('manPubTrPerCap', 0, 'Managed Public Trees per Capita', 'Trees per Capita', '%.10s',
                                               precision=3, axisPrecision=1, lowerBound=0, upperBound=1.4, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                     plotRegion('manPubTrPerCap', (input$Q6_18PubTr/input$pop), 'Managed Public Trees per Capita', 'Trees per Capita', '%.10s',
                                                 precision=3, axisPrecision=1, lowerBound=0, upperBound=1.4, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q6_18PubTr)|is_empty(input$Q1_15TotEmp)){
                                     plotRegion('manPubTrPerEmp', 0, 'Managed Public Trees per Employee', 'Trees per Employee', '%.10s',
                                                precision=0, axisPrecision=0, lowerBound=0, upperBound=15000, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotRegion('manPubTrPerEmp', (input$Q6_18PubTr/input$Q1_15TotEmp), 'Managed Public Trees per Employee', 'Trees per Employee', '%.10s',
                                               precision=0, axisPrecision=0, lowerBound=0, upperBound=15000, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q1_3AcMMP)|is_empty(input$Q1_3AcMGS)|is_empty(input$Q1_3AcMPWTr)|is_empty(input$pop)){
                                    plotRegion('greenSpaceAreaMeters', 0, bquote("Green Space Area (" *  m^2 * ") per Person"), bquote("Per Capita (" *  m^2 * ")"), '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=200, axisBreaks=8) + theme(legend.position="none")
                                  }
                                  else{
                                    plotRegion('greenSpaceAreaMeters', (((input$Q1_3AcMMP*sqMetersPerAcre)+(input$Q1_3AcMGS*sqMetersPerAcre)+(input$Q1_3AcMPWTr*sqMetersPerAcre))/input$pop), bquote("Green Space Area (" *  m^2 * ") per Person"), bquote("Per Capita (" *  m^2 * ")"), '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=200, axisBreaks=8) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q1_3AcMMP)|is_empty(input$Q1_3AcMGS)|is_empty(input$Q1_3AcMPWTr)|is_empty(input$pop)){
                                    plotRegion('greenSpaceAreaFeet', 0, bquote("Green Space Area (" *  ft^2 * ") per Person"), bquote("Per Capita (" *  ft^2 * ")"), '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=2000, axisBreaks=8) + theme(legend.position="none")
                                  }
                                  else{
                                    plotRegion('greenSpaceAreaFeet', (((input$Q1_3AcMMP*sqFeetPerAcre)+(input$Q1_3AcMGS*sqFeetPerAcre)+(input$Q1_3AcMPWTr*sqFeetPerAcre))/input$pop), bquote("Green Space Area (" *  ft^2 * ") per Person"), bquote("Per Capita (" *  ft^2 * ")"), '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=2000, axisBreaks=8) + theme(legend.position="none")
                                  }), 
                     layout_matrix = rbind(c(1,2),
                                           c(3,4)))
      }
      else if(input$compSelTP==pop){
        grid.arrange(grobs = list(if(is_empty(input$Q6_18PubTr)|is_empty(input$pop)){
                                  plotPopulation('manPubTrPerCap', 0, 'Managed Public Trees per Capita', 'Trees per Capita', '%.10s',
                                               precision=3, axisPrecision=1, lowerBound=0, upperBound=1.4, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotPopulation('manPubTrPerCap', (input$Q6_18PubTr/input$pop), 'Managed Public Trees per Capita', 'Trees per Capita', '%.10s',
                                               precision=3, axisPrecision=1, lowerBound=0, upperBound=1.4, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q6_18PubTr)|is_empty(input$Q1_15TotEmp)){
                                    plotPopulation('manPubTrPerEmp', 0, 'Managed Public Trees per Employee', 'Trees per Employee', '%.10s',
                                               precision=0, axisPrecision=0, lowerBound=0, upperBound=15000, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotPopulation('manPubTrPerEmp', (input$Q6_18PubTr/input$Q1_15TotEmp), 'Managed Public Trees per Employee', 'Trees per Employee', '%.10s',
                                               precision=0, axisPrecision=0, lowerBound=0, upperBound=15000, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q1_3AcMMP)|is_empty(input$Q1_3AcMGS)|is_empty(input$Q1_3AcMPWTr)|is_empty(input$pop)){
                                    plotPopulation('greenSpaceAreaMeters', 0, bquote("Green Space Area (" *  m^2 * ") per Person"), bquote("Per Capita (" *  m^2 * ")"), '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=200, axisBreaks=8) + theme(legend.position="none")
                                  }
                                  else{
                                    plotPopulation('greenSpaceAreaMeters', (((input$Q1_3AcMMP*sqMetersPerAcre)+(input$Q1_3AcMGS*sqMetersPerAcre)+(input$Q1_3AcMPWTr*sqMetersPerAcre))/input$pop), bquote("Green Space Area (" *  m^2 * ") per Person"), bquote("Per Capita (" *  m^2 * ")"), '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=200, axisBreaks=8) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q1_3AcMMP)|is_empty(input$Q1_3AcMGS)|is_empty(input$Q1_3AcMPWTr)|is_empty(input$pop)){
                                    plotPopulation('greenSpaceAreaFeet', 0, bquote("Green Space Area (" *  ft^2 * ") per Person"), bquote("Per Capita (" *  ft^2 * ")"), '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=2000, axisBreaks=8) + theme(legend.position="none")
                                  }
                                  else{
                                    plotPopulation('greenSpaceAreaFeet', (((input$Q1_3AcMMP*sqFeetPerAcre)+(input$Q1_3AcMGS*sqFeetPerAcre)+(input$Q1_3AcMPWTr*sqFeetPerAcre))/input$pop), bquote("Green Space Area (" *  ft^2 * ") per Person"), bquote("Per Capita (" *  ft^2 * ")"), '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=2000, axisBreaks=8) + theme(legend.position="none")
                                  }
                                  ),
                     layout_matrix = rbind(c(1,2),
                                           c(3,4)))
      }
    })
    
    #Tree Stocking indicator page
    output$plotTS<-renderPlot({
      if(input$compSelTS==reg){
        grid.arrange(grobs = list(if(is_empty(input$Q6_19StrTr)|is_empty(input$Q6_20StrTr)){
                                    plotRegion('strTrStockingLevel', 0, 'Street Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotRegion('strTrStockingLevel', ((input$Q6_19StrTr/(input$Q6_19StrTr+input$Q6_20StrTr))*100), 'Street Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q6_19PrkTr)|is_empty(input$Q6_20PrkTr)){
                                    plotRegion('prkTrStockingLevel', 0, 'Park Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotRegion('prkTrStockingLevel', ((input$Q6_19PrkTr/(input$Q6_19PrkTr+input$Q6_20PrkTr))*100), 'Park Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q6_19MuniTr)|is_empty(input$Q6_20MuniTr)){
                                    plotRegion('munTrStockingLevel', 0, 'Municipal Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotRegion('munTrStockingLevel', ((input$Q6_19MuniTr/(input$Q6_19MuniTr+input$Q6_20MuniTr))*100), 'Municipal Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q6_19StrTr)|is_empty(input$Q1_3MiMMS)){
                                    plotRegion('pubStrTrDensity', 0, 'Public Street Tree Density per Street Mile', 'Trees per Mile', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=100, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotRegion('pubStrTrDensity', (input$Q6_19StrTr/input$Q1_3MiMMS), 'Public Street Tree Density per Street Mile', 'Trees per Mile', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=100, axisBreaks=6) + theme(legend.position="none")
                                  }),
                     layout_matrix = rbind(c(1,2),
                                           c(3,4)))
      }
      else if(input$compSelTS==pop){
        grid.arrange(grobs = list(if(is_empty(input$Q6_19StrTr)|is_empty(input$Q6_20StrTr)){
                                  plotPopulation('strTrStockingLevel', 0, 'Street Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotPopulation('strTrStockingLevel', ((input$Q6_19StrTr/(input$Q6_19StrTr+input$Q6_20StrTr))*100), 'Street Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q6_19PrkTr)|is_empty(input$Q6_20PrkTr)){
                                    plotPopulation('prkTrStockingLevel', 0, 'Park Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotPopulation('prkTrStockingLevel', ((input$Q6_19PrkTr/(input$Q6_19PrkTr+input$Q6_20PrkTr))*100), 'Park Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q6_19MuniTr)|is_empty(input$Q6_20MuniTr)){
                                    plotPopulation('munTrStockingLevel', 0, 'Municipal Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotPopulation('munTrStockingLevel', ((input$Q6_19MuniTr/(input$Q6_19MuniTr+input$Q6_20MuniTr))*100), 'Municipal Tree Stocking Level Attainment', 'Attained (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q6_19StrTr)|is_empty(input$Q1_3MiMMS)){
                                  plotPopulation('pubStrTrDensity', 0, 'Public Street Tree Density per Street Mile', 'Trees per Mile', '%.10s',
                                                 precision=1, axisPrecision=0, lowerBound=0, upperBound=120, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotPopulation('pubStrTrDensity', (input$Q6_19StrTr/input$Q1_3MiMMS), 'Public Street Tree Density per Street Mile', 'Trees per Mile', '%.10s',
                                                   precision=1, axisPrecision=0, lowerBound=0, upperBound=120, axisBreaks=6) + theme(legend.position="none")}),
                     layout_matrix = rbind(c(1,2),
                                           c(3,4)))
      }
    })
    
    
    #Tree Canopy Indicator page
      output$plotTC<-renderPlot({
       if(input$compSelTC==reg){
        grid.arrange(grobs = list(if(is_empty(input$Q6_15CanGl)){
                                                 plotRegion('canCovGoal', 0, 'Community has Tree Canopy Goal', 'Yes (%)', '%.10s',
                                                            precision=1, axisPrecision=0, lowerBound=0, upperBound=110, axisBreaks=6) + theme(legend.position="none")
                                                 }
                                                 else{
                                                 plotRegion('canCovGoal', ifelse(input$Q6_15CanGl==y,100,0), 'Community has Tree Canopy Goal', 'Yes (%)', '%.10s',
                                                            precision=1, axisPrecision=0, lowerBound=0, upperBound=110, axisBreaks=6) + theme(legend.position="none")
                                                 },
                                                 if(is_empty(input$Q6_16CurCn)){
                                                 plotRegion('percentCurCan', 0, 'Current Tree Canopy Percent', 'Tree Canopy (%)', '%.10s',
                                                            precision=1, axisPrecision=0, lowerBound=0, upperBound=110, axisBreaks=6) + theme(legend.position="none")
                                                 }
                                                 else{
                                                 plotRegion('percentCurCan', input$Q6_16CurCn, 'Current Tree Canopy Percent', 'Tree Canopy (%)', '%.10s',
                                                            precision=1, axisPrecision=0, lowerBound=0, upperBound=110, axisBreaks=6) + theme(legend.position="none")
                                                 },
                                                 if(is_empty(input$Q6_16CanGl)){
                                                   plotRegion('percentCanGoal', 0, 'Tree Canopy Cover Goal', 'Tree Canopy (%)', '%.10s',
                                                              precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                                 }
                                                 else{
                                                   plotRegion('percentCanGoal', input$Q6_16CanGl, 'Tree Canopy Cover Goal', 'Tree Canopy (%)', '%.10s',
                                                              precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                                 },
                                                 if(is_empty(input$Q6_16YrToGl)){
                                                   plotRegion('percentCanProgress', 0, 'Progress Toward Tree Canopy Goal', 'Attained (%)', '%.10s',
                                                              precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")  
                                                 }
                                                 else{
                                                   plotRegion('percentCanProgress', ((input$Q6_16CurCn/input$Q6_16CanGl)*100), 'Progress Toward Tree Canopy Goal', 'Attained (%)', '%.10s',
                                                              precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                                 }),
                                    layout_matrix = rbind(c(1,2),
                                                          c(3,4)))
      }
      else if(input$compSelTC==pop){
        grid.arrange(grobs = list(if(is_empty(input$Q6_16CanGl)){
                                                 plotPopulation('canCovGoal', 0, 'Community has Tree Canopy Goal', 'Yes (%)', '%.10s',
                                                                precision=1, axisPrecision=0, lowerBound=0, upperBound=110, axisBreaks=6) + theme(legend.position="none")
                                                 }
                                                 else{
                                                 plotPopulation('canCovGoal', ifelse(input$Q6_15CanGl==y,100,0), 'Community has Tree Canopy Goal', 'Yes (%)', '%.10s',
                                                                precision=1, axisPrecision=0, lowerBound=0, upperBound=110, axisBreaks=6) + theme(legend.position="none")
                                                 },
                                                 if(is_empty(input$Q6_16CurCn)){
                                                   plotPopulation('percentCurCan', 0, 'Current Tree Canopy Percent', 'Tree Canopy (%)', '%.10s',
                                                                  precision=1, axisPrecision=0, lowerBound=0, upperBound=110, axisBreaks=6) + theme(legend.position="none")
                                                 }
                                                 else{
                                                 plotPopulation('percentCurCan', input$Q6_16CurCn, 'Current Tree Canopy Percent', 'Tree Canopy (%)', '%.10s',
                                                                precision=1, axisPrecision=0, lowerBound=0, upperBound=110, axisBreaks=6) + theme(legend.position="none")
                                                 },
                                                 if(is_empty(input$Q6_16CanGl)){
                                                   plotPopulation('percentCanGoal', 0, 'Tree Canopy Cover Goal', 'Tree Canopy (%)', '%.10s',
                                                                  precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
                                                 }
                                                 else{
                                                   plotPopulation('percentCanGoal', input$Q6_16CanGl, 'Tree Canopy Cover Goal', 'Tree Canopy (%)', '%.10s',
                                                                  precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
                                                 },
                                                 if(is_empty(input$Q6_16YrToGl)){
                                                   plotPopulation('percentCanProgress', 0, 'Progress Toward Tree Canopy Goal', 'Attained (%)', '%.10s',
                                                                  precision=1, axisPrecision=0, lowerBound=0, upperBound=130, axisBreaks=6) + theme(legend.position="none")
                                                 }
                                                 else{
                                                   plotPopulation('percentCanProgress', ((input$Q6_16CurCn/input$Q6_16CanGl)*100), 'Progress Toward Tree Canopy Goal', 'Attained (%)', '%.10s',
                                                                  precision=1, axisPrecision=0, lowerBound=0, upperBound=130, axisBreaks=6) + theme(legend.position="none")
                                                 }),
                                    layout_matrix = rbind(c(1,2),
                                                          c(3,4)))
      }
    })
    
    #Inspection and Pruning Cycle page
    output$plotIPC<-renderPlot({
      if(input$compSelIPC==reg){
        grid.arrange(grobs = list(if(is_empty(input$Q7_4CurCyc)){
                                    plotRegion('currentPrunInsCyc', 0, 'Current Inspection and Pruning Cycle', 'Years', '%.10s',
                                                                       precision=1, axisPrecision=0, lowerBound=0, upperBound=10, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotRegion('currentPrunInsCyc', input$Q7_4CurCyc, 'Current Inspection and Pruning Cycle', 'Years', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=10, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q7_4DesCyc)){
                                    plotRegion('desiredPrunInsCyc', 0, 'Desired Inspection and Pruning Cycle', 'Years', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=10, axisBreaks=5) + theme(legend.position="none")
                                  }
                                  else{
                                    plotRegion('desiredPrunInsCyc', input$Q7_4DesCyc, 'Desired Inspection and Pruning Cycle', 'Years', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=10, axisBreaks=5) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q7_4CurCyc)|is_empty(input$Q7_4DesCyc)){
                                    plotRegion('yearsOfPrunInsCyc', 0, 'Inspection and Pruning Years Off Cycle', 'Years', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=5, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotRegion('yearsOfPrunInsCyc', (input$Q7_4CurCyc-input$Q7_4DesCyc), 'Inspection and Pruning Years Off Cycle', 'Years', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=5, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q7_4CurCyc)|is_empty(input$Q7_4DesCyc)){
                                    plotRegion('percentAttPrunInsCyc', 0, 'Attainment for Inspection and Pruning Cycle', 'Attained (%)', '%.10s',
                                               precision= 1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotRegion('percentAttPrunInsCyc', ((input$Q7_4DesCyc/input$Q7_4CurCyc)*100), 'Attainment for Inspection and Pruning Cycle', 'Attained (%)', '%.10s',
                                               precision= 1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }),
                     layout_matrix = rbind(c(1,2),
                                           c(3,4)))
      }
      else if(input$compSelIPC==pop){
        grid.arrange(grobs = list(if(is_empty(input$Q7_4CurCyc)){
                                    plotPopulation('currentPrunInsCyc', 0, 'Current Inspection and Pruning Cycle', 'Years', '%.10s',
                                                   precision=1, axisPrecision=0, lowerBound=0, upperBound=25, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotPopulation('currentPrunInsCyc', input$Q7_4CurCyc, 'Current Inspection and Pruning Cycle', 'Years', '%.10s',
                                                   precision=1, axisPrecision=0, lowerBound=0, upperBound=25, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q7_4DesCyc)){
                                    plotPopulation('desiredPrunInsCyc', 0, 'Desired Inspection and Pruning Cycle', 'Years', '%.10s',
                                                   precision=1, axisPrecision=0, lowerBound=0, upperBound=10, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotPopulation('desiredPrunInsCyc', input$Q7_4DesCyc, 'Desired Inspection and Pruning Cycle', 'Years', '%.10s',
                                                   precision=1, axisPrecision=0, lowerBound=0, upperBound=10, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q7_4CurCyc)|is_empty(input$Q7_4DesCyc)){
                                    plotPopulation('yearsOfPrunInsCyc', 0, 'Inspection and Pruning Years Off Cycle', 'Years', '%.10s',
                                                   precision=1, axisPrecision=0, lowerBound=0, upperBound=15, axisBreaks=9) + theme(legend.position="none")
                                  }
                                  else{
                                    plotPopulation('yearsOfPrunInsCyc', (input$Q7_4CurCyc-input$Q7_4DesCyc), 'Inspection and Pruning Years Off Cycle', 'Years', '%.10s',
                                                   precision=1, axisPrecision=0, lowerBound=0, upperBound=15, axisBreaks=9) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q7_4CurCyc)|is_empty(input$Q7_4DesCyc)){
                                    plotPopulation('percentAttPrunInsCyc', 0, 'Attainment for Inspection and Pruning Cycle', 'Attained (%)', '%.10s',
                                               precision= 1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotPopulation('percentAttPrunInsCyc', ((input$Q7_4DesCyc/input$Q7_4CurCyc)*100), 'Attainment for Inspection and Pruning Cycle', 'Attained (%)', '%.10s',
                                               precision= 1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }),
                     layout_matrix = rbind(c(1,2),
                                           c(3,4)))
      }
    })
    
    #Tree Inventory and Management
    output$plotTIM<-renderPlot({
      if(input$compSelTIM==reg){
        grid.arrange(grobs = list(if(is.na(input$Q7_2SysSch)){
                                    plotRegion('activeManagement', 0, 'Percentatge Active (Systematic) Management of Tree Population', 'Percent', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else if(input$Q7_2SysSch <= 40){
                                    plotRegion('activeManagement', 0, 'Percentatge Active (Systematic) Management of Tree Population', 'Percent', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotRegion('activeManagement', 100, 'Percentatge Active (Systematic) Management of Tree Population', 'Percent', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q6_1TrInv)){
                                    plotRegion('treeResourceInventory', 0, 'Community Tree Inventory up to Date', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else if(input$Q6_1TrInv==n|is_empty(input$Q6_3YrUpd)){
                                    plotRegion('treeResourceInventory', 0, 'Community Tree Inventory up to Date', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else if((as.numeric(format(Sys.Date(),'%Y'))-input$Q6_3YrUpd)>5){
                                    plotRegion('treeResourceInventory', 0, 'Community Tree Inventory up to Date', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotRegion('treeResourceInventory', 100, 'Community Tree Inventory up to Date', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }),
                     layout_matrix = rbind(c(1,2)))
      }
      else if(input$compSelTIM==pop){
        grid.arrange(grobs = list(if(is.na(input$Q7_2SysSch)){
                                    plotPopulation('activeManagement', 0, 'Percentatge Active (Systematic) Management of Tree Population', 'Percent', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else if(input$Q7_2SysSch <= 40){
                                    plotPopulation('activeManagement', 0, 'Percentatge Active (Systematic) Management of Tree Population', 'Percent', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotPopulation('activeManagement', 100, 'Percentatge Active (Systematic) Management of Tree Population', 'Percent', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  },
                                  if(is_empty(input$Q6_1TrInv)){
                                    plotPopulation('treeResourceInventory', 0, 'Community Tree Inventory up to Date', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else if(input$Q6_1TrInv==n|is_empty(input$Q6_3YrUpd)){
                                    plotPopulation('treeResourceInventory', 0, 'Community Tree Inventory up to Date', 'Yes (%)', '%.10s',
                                                   precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else if((as.numeric(format(Sys.Date(),'%Y'))-input$Q6_3YrUpd)>5){
                                    plotPopulation('treeResourceInventory', 0, 'Community Tree Inventory up to Date', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }
                                  else{
                                    plotPopulation('treeResourceInventory', 100, 'Community Tree Inventory up to Date', 'Yes (%)', '%.10s',
                                               precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6) + theme(legend.position="none")
                                  }),
                     layout_matrix = rbind(c(1,2)))
      }
    })
    
    output$plotCARS<-renderPlot({
      if(input$compSelCARS==reg){
        plotRegion('carsScore', (ifelse(is_empty(input$Q1_13),0,ifelse(input$Q1_13 %in% c(T1_132yd,T1_134yd,T1_13Grad,T1_13ISACA,T1_13ISACMS,T1_13ISAAC,T1_13SAFCF),25,0))+
                                   ifelse(is_empty(input$Q3_61)|is_empty(input$Q3_62)|is_empty(input$Q3_63),0,ifelse(input$Q3_61 %in% c(T3_6RegTrPub,T3_6RegTrPri,T3_6ReqTrND,T3_6ReqTrNP,T3_6ReqTrRP,T3_6ReqRpPOT) & input$Q3_62 %in% c(T3_6ReqPreDev,T3_6ResTrCtPP,T3_6RegAbat,T3_6IdPreHer) & input$Q3_63 %in% c(T3_6RegPubTrMn,T3_6ReqTyMn,T3_6RegRemDd,T3_6EstIDCS,T3_6DefTrReq,T3_6PhrTrTop),25,0))+
                                   ifelse(is_empty(input$Q3_2AuthTrBd),0,ifelse(input$Q3_2AuthTrBd==y,25,ifelse(is_empty(input$Q3_1),0,ifelse(input$Q3_1!="None of the above",25,0))))+
                                   ifelse(is_empty(input$Q6_1TrInv)|is_empty(input$Q3_9WrtStgPln),0,ifelse(input$Q6_1TrInv==y & input$Q3_9WrtStgPln==y,25,0))),
                   'Community Accomplishment Reporting System Score', 'Score (%)', '%.10s',
                   precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
      }
      else if(input$compSelCARS==pop){
        plotPopulation('carsScore', (ifelse(is_empty(input$Q1_13),0,ifelse(input$Q1_13 %in% c(T1_132yd,T1_134yd,T1_13Grad,T1_13ISACA,T1_13ISACMS,T1_13ISAAC,T1_13SAFCF),25,0))+
                                       ifelse(is_empty(input$Q3_61)|is_empty(input$Q3_62)|is_empty(input$Q3_63),0,ifelse(input$Q3_61 %in% c(T3_6RegTrPub,T3_6RegTrPri,T3_6ReqTrND,T3_6ReqTrNP,T3_6ReqTrRP,T3_6ReqRpPOT) & input$Q3_62 %in% c(T3_6ReqPreDev,T3_6ResTrCtPP,T3_6RegAbat,T3_6IdPreHer) & input$Q3_63 %in% c(T3_6RegPubTrMn,T3_6ReqTyMn,T3_6RegRemDd,T3_6EstIDCS,T3_6DefTrReq,T3_6PhrTrTop),25,0))+
                                       ifelse(is_empty(input$Q3_2AuthTrBd),0,ifelse(input$Q3_2AuthTrBd==y,25,ifelse(is_empty(input$Q3_1),0,ifelse(input$Q3_1!="None of the above",25,0))))+
                                       ifelse(is_empty(input$Q6_1TrInv)|is_empty(input$Q3_9WrtStgPln),0,ifelse(input$Q6_1TrInv==y & input$Q3_9WrtStgPln==y,25,0))), 
                       'Community Accomplishment Reporting System Score', 'Score (%)', '%.10s',
                       precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
      }
    })
    
    output$plotCM<-renderPlot({
      len310<-length(input$Q3_10)
      if(input$compSelCM==reg){
        plotRegion('cmRelativeScore', (as.numeric(tableScores[1])+as.numeric(tableScores[2])+as.numeric(tableScores[3])+as.numeric(tableScores[4])+as.numeric(tableScores[5])+as.numeric(tableScores[6])+as.numeric(tableScores[7])+
                                         ifelse(is_empty(input$Q3_9WrtStgPln)|is_empty(input$Q3_10),0,ifelse(input$Q3_9WrtStgPln==n,1,ifelse(input$Q3_9WrtStgPln==d,1.5,ifelse(input$Q3_9WrtStgPln==y&is_empty(input$Q3_10),2,ifelse(input$Q3_9WrtStgPln==y&(len310==1|len310==2),2.5,ifelse(input$Q3_9WrtStgPln==y&(len310==3|len310==4),3,ifelse(input$Q3_9WrtStgPln==y&(len310==5|len310==6),3.5,ifelse(input$Q3_9WrtStgPln==y&(len310==7|len310==8),4,0))))))))+
                                         ifelse(is_empty(input$Q2_10CurrNeeds)|is_empty(input$Q7_2SysSch),0,ifelse(input$Q2_10CurrNeeds==y&input$Q7_2SysSch>=70,4,ifelse(input$Q2_10CurrNeeds==y&input$Q7_2SysSch>=40,3,ifelse(input$Q2_10CurrNeeds==y|input$Q7_2SysSch>=40,2,ifelse(input$Q2_10CurrNeeds==n|input$Q7_2SysSch<40,1,0)))))+
                                         ifelse(is_empty(input$Q1_6OvrcTr)&is_empty(input$Q1_13),0,ifelse(input$Q1_6OvrcTr==n&is_empty(input$Q1_13),1,ifelse(input$Q1_6OvrcTr==y&is_empty(input$Q1_13),2,ifelse(input$Q1_6OvrcTr==y & T1_13ISACA %in% input$Q1_13 & (T1_132yd %in% input$Q1_13|T1_134yd %in% input$Q1_13|T1_13Grad %in% input$Q1_13),4,ifelse(input$Q1_6OvrcTr==y & (T1_13ISACA %in% input$Q1_13|T1_132yd %in% input$Q1_13|T1_134yd %in% input$Q1_13|T1_13Grad %in% input$Q1_13),3,ifelse(input$Q1_6OvrcTr==y & (T1_13InHOrJ %in% input$Q1_13|T1_13AttTree %in% input$Q1_13),2.5,ifelse(input$Q1_6OvrcTr==y,2,ifelse(input$Q1_6OvrcTr==n,1,0))))))))+
                                         ifelse(is_empty(input$Q6_1TrInv),0,ifelse(input$Q6_1TrInv==n,1,))), 
                   'Urban Forest Sustainability Relative Score', 'Score', '%.10s',
                   precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
      }
      else if(input$compSelCM==pop){
        plotPopulation('cmRelativeScore', 100, 'Urban Forest Sustainability Relative Score', 'Score', '%.10s',
                       precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
      }
    })
     
    output$plotSMA<-renderPlot({
      if(input$compSelSMA==reg){
        plotRegion('smaScore', (ifelse(is_empty(input$Q1_13),0,ifelse(input$Q1_13 %in% T1_13ISACA,100/6,0))+
                                  ifelse(is_empty(input$Q3_9WrtStgPln),0,ifelse(input$Q3_9WrtStgPln==y,100/6,0))+
                                  ifelse(is_empty(input$Q3_2AuthTrBd)|is_empty(input$Q3_1)|is_empty(input$Q3_5MuniOrd)|is_empty(input$Q2_3TotTreBud)|is_empty(input$pop)|is_empty(input$Q8_10ObvsP),0,
                                         ifelse(input$Q3_2AuthTrBd==y&input$Q3_1!="None of the above"&input$Q3_5MuniOrd==y&(input$Q2_3TotTreBud/input$pop)>=2&input$Q8_10ObvsP==y,100/6,0))+
                                  ifelse(is_empty(input$Q8_4AwdCm),0,ifelse(input$Q8_4AwdCm==y,100/6,0))+
                                  ifelse(is_empty(input$Q5_4),0,ifelse(T5_4PrefTCI %in% input$Q5_4,100/6,0))+
                                  ifelse(is_empty(input$Q3_13),0,ifelse(T3_13AnsiZ133 %in% input$Q3_13 & T3_13AnsiA300 %in% input$Q3_13,100/6,0))), 
                   'Society of Municipal Arborists Accreditation Program Score', 'Score (%)', '%.10s',
                   precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
      }
      else if(input$compSelSMA==pop){
        plotPopulation('smaScore', (ifelse(is_empty(input$Q1_13),0,ifelse(input$Q1_13 %in% T1_13ISACA,100/6,0))+
                                      ifelse(is_empty(input$Q3_9WrtStgPln),0,ifelse(input$Q3_9WrtStgPln==y,100/6,0))+
                                      ifelse(is_empty(input$Q3_2AuthTrBd)|is_empty(input$Q3_1)|is_empty(input$Q3_5MuniOrd)|is_empty(input$Q2_3TotTreBud)|is_empty(input$pop)|is_empty(input$Q8_10ObvsP),0,
                                             ifelse(input$Q3_2AuthTrBd==y&input$Q3_1!="None of the above"&input$Q3_5MuniOrd==y&(input$Q2_3TotTreBud/input$pop)>=2&input$Q8_10ObvsP==y,100/6,0))+
                                      ifelse(is_empty(input$Q8_4AwdCm),0,ifelse(input$Q8_4AwdCm==y,100/6,0))+
                                      ifelse(is_empty(input$Q5_4),0,ifelse(T5_4PrefTCI %in% input$Q5_4,100/6,0))+
                                      ifelse(is_empty(input$Q3_13),0,ifelse(T3_13AnsiZ133 %in% input$Q3_13 & T3_13AnsiA300 %in% input$Q3_13,100/6,0))), 
                       'Society of Municipal Arborists Accreditation Program Score', 'Score (%)', '%.10s',
                       precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
      }
    })
    
    output$plotTCUSA<-renderPlot({
      if(input$compSelTCUSA==reg){
        plotRegion('tcScore', (ifelse(is_empty(input$Q3_2AuthTrBd),0,ifelse(input$Q3_2AuthTrBd==y,25,ifelse(is_empty(input$Q3_1),0,ifelse(input$Q3_1!="None of the above",25,0))))+
                                 ifelse(is_empty(input$Q3_5MuniOrd),0,ifelse(input$Q3_5MuniOrd==y,25,0))+
                                 ifelse(is_empty(input$Q2_3TotTreBud)|is_empty(input$pop),0,ifelse((input$Q2_3TotTreBud/input$pop)>=2,25,0))+
                                 ifelse(is_empty(input$Q8_10ObvsP),0,ifelse(input$Q8_10ObvsP==y,25,0)))
                   , 'Tree City USA Standards', 'Score (%)', '%.10s',
                   precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
      }
      else if(input$compSelTCUSA==pop){
        plotPopulation('tcScore', (ifelse(is_empty(input$Q3_2AuthTrBd),0,ifelse(input$Q3_2AuthTrBd==y,25,ifelse(is_empty(input$Q3_1),0,ifelse(input$Q3_1!="None of the above",25,0))))+
                                     ifelse(is_empty(input$Q3_5MuniOrd),0,ifelse(input$Q3_5MuniOrd==y,25,0))+
                                     ifelse(is_empty(input$Q2_3TotTreBud)|is_empty(input$pop),0,ifelse((input$Q2_3TotTreBud/input$pop)>=2,25,0))+
                                     ifelse(is_empty(input$Q8_10ObvsP),0,ifelse(input$Q8_10ObvsP==y,25,0)))
                       , 'Tree City USA Standards', 'Score (%)', '%.10s',
                       precision=1, axisPrecision=0, lowerBound=0, upperBound=105, axisBreaks=6)
      }
    })
    #--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #Titles for sumamry pages
    output$myTSIndiHeader<-renderText({
      paste("<h3>","MyTreeScore Indicator for ",ifelse(input$city=="Other",input$tCity,input$city),"</h3>")
    })
    
    output$myTSIndeHeader<-renderText({
      paste("<h3>","MyTreeScore Indexes",ifelse(input$city=="Other",input$tCity,input$city),"</h3>")
    })
    
    #table text test
    output$myTSIndic1<-renderTable(
      data.frame("Indicators_and_Categories"=c("<b><u>Indicators & Categories</b></u>","<b><u>Tree Budget & Need</u></b>","Budget per Capita","Budget per Public Tree","Budget percent of Total Municipal Budget","Budget percent of Identified Need"#,"<hr></hr>"
                                               ,"<b><u>Resource Budget Allocation</b></u>","Administration","Planting","Pruning","Removal","Other"#,"<hr></hr>"
                                               ,"<b><u>Community Trees & Governance</b></u>","Tree Board or Related Group Exists","Written Strategic Plan Exists","Written Strategic Plan up to Date","Tree Ordinance Exists","Tree Ordinance up to Date"#,"<hr></hr>"
                                               ,"<b><u>Tree Inventory & Space Allocation</b></u>","Tree Inventory Exists","Tree Inventory up to Date","Public Green Space Area","Public Green Space Area"#,"<hr></hr>"
                                               ,"<b><u>Tree Canopy Assessment</b></u>","Tree Canopy Goal Exists","Current Tree Canopy","Tree Canopy Cover Goal Achievement","Years to Achieve Tree Canopy Goal"#,"<hr></hr>"
                                               ,"<b><u>Tree Stocking Level</b></u>","Street Tree Stocking Level Attainment","Park Tree Stocking Level Attainment","Public Tree Stocking Level Attainment","Public Street Tree Density"#,"<hr></hr>"
                                               ,"<b><u>Active (Systematic) Management</b></u>","Current Inspection and Pruning Cycle","Desired Inspectino and Pruning Cycle","Time Inspection and Pruning Off Cycle","Attainment of Inspection and Pruning Cycle","Active Management Level of Tree Population"
                                               ),
                 "MyTreeScore"=c("<b><u>MyTreeScore</b></u>","",specify_decimal((input$Q2_3TotTreBud/input$pop),2),specify_decimal((input$Q2_3TotTreBud/input$Q6_18PubTr),2),specify_decimal((input$Q2_3TotTreBud/input$Q2_1TotBud),2),ifelse(is_empty(input$Q2_10CurrNeeds),'NA',ifelse(input$Q2_10CurrNeeds==y,100,input$Q2_10Need))#,"<hr></hr>"
                                 ,"",input$Q2_13AdminSupr,input$Q2_13TrPlan,input$Q2_13TrPrun,input$Q2_13TrRem,input$Q2_13Othr#,"<hr></hr>"
                                 ,"",ifelse(is_empty(input$Q3_2AuthTrBd),'NA',ifelse(input$Q3_2AuthTrBd==y,100,0)),ifelse(is_empty(input$Q3_9WrtStgPln),'NA',ifelse(input$Q3_9WrtStgPln==y,100,0)),ifelse(is_empty(input$Q3_9WrtStgPln),'NA',ifelse(input$Q3_9WrtStgPln==y,(ifelse(((as.numeric(format(Sys.Date(),'%Y'))-input$Q3_9Updt)<=5),100,0)),0)),ifelse(is_empty(input$Q3_5MuniOrd),'NA',ifelse(input$Q3_5MuniOrd==y,100,0)),ifelse(is_empty(input$Q3_5MuniOrd),ifelse(input$Q3_5MuniOrd==y,(ifelse(((as.numeric(format(Sys.Date(),'%Y'))-input$Q3_5YrUpdt)<=5),100,0)),0))#,"<hr></hr>"
                                 ,"",ifelse(is_empty(input$Q6_1TrInv),'NA',ifelse(input$Q6_1TrInv==y,100,0)),ifelse(is_empty(input$Q6_1TrInv),'NA',ifelse(is_empty(input$Q6_3YrUpd),'NA',ifelse((as.numeric(format(Sys.Date(),'%Y'))-input$Q6_3YrUpd)<=5),100,0)),ifelse((is_empty(input$Q1_3AcMMP)|is_empty(input$Q1_3AcMGS)|is_empty(input$Q1_3AcMPWTr)),'NA',(((input$Q1_3AcMMP*sqMetersPerAcre)+(input$Q1_3AcMGS*sqMetersPerAcre)+(input$Q1_3AcMPWTr*sqMetersPerAcre))/input$pop)),ifelse((is_empty(input$Q1_3AcMMP)|is_empty(input$Q1_3AcMGS)|is_empty(input$Q1_3AcMPWTr)),'NA',(((input$Q1_3AcMMP*sqFeetPerAcre)+(input$Q1_3AcMGS*sqFeetPerAcre)+(input$Q1_3AcMPWTr*sqFeetPerAcre))/input$pop))#,"<hr></hr>"
                                 ,"",ifelse(is_empty(input$Q6_15CanGl),'NA',ifelse(input$Q6_15CanGl==y,100,0)),ifelse(is_empty(input$Q6_16CurCn),'NA',input$Q6_16CurCn),ifelse((is_empty(input$CurCn)|is_empty(input$Q6_16CanGl)),'NA',(input$Q6_16CurCn/input$Q6_16CanGl)),ifelse(is_empty(input$Q6_16YrsToGl),'NA',(input$Q6_16YrsToGl-as.numeric(format(Sys.Date(),'%Y'))))#,"<hr></hr>"
                                 ,"",(input$Q6_19StrTr/(input$Q6_19StrTr+input$Q6_20StrTr)),(input$Q6_19PrkTr/(input$Q6_19PrkTr+input$Q6_20PrkTr)),(input$Q6_19MuniTr/(input$Q6_19MuniTr+input$Q6_20MuniTr)),(input$Q6_19StrTr/input$Q1_3MiMMS)#,"<hr></hr>"
                                 ,"",ifelse(is_empty(input$Q7_4AppPrun),'NA',ifelse(is_empty(input$Q7_4CurCyc),'NA',input$Q7_4CurCyc)),ifelse(is_empty(input$Q7_4AppPrun),'NA',ifelse(is_empty(input$Q7_4DesCyc),'NA',input$Q7_4DesCyc)),ifelse((is_empty(input$Q7_4CurCyc)|is_empty(input$Q7_4DesCyc)),'NA',(input$Q7_4CurCyc-input$Q7_4DesCyc)),ifelse((is_empty(input$Q7_4CurCyc)|is_empty(input$Q7_4DesCyc)),'NA',(input$Q7_4CurCyc/input$Q7_4DesCyc)),input$Q7_2SysSch
                                 ),
                 "Regional"=c("<b><u>Regional</b></u>","",8.90,37.58,0.62,88#,"<hr></hr>"
                              ,"",9,33,21,23,14#,"<hr></hr>"
                              ,"",79,87,52,89,52#,"<hr></hr>"
                              ,"",49,51,112,678#,"<hr></hr>"
                              ,"",31,33,52,7#,"<hr></hr>"
                              ,"",78,62,97,131.8#,"<hr></hr>"
                              ,"",6.9,4.8,3.1,52,57
                              ),
                 "National"=c("<b><u>National</b></u>","",specify_decimal(mean(data$dollarsPerCapita,na.rm=T),2),specify_decimal(mean(data$dollarsPerPublicTree,na.rm=T),2),specify_decimal(mean(data$percentOfMuniBud,na.rm=T),2),specify_decimal(mean(data$budgetNeeds,na.rm=T),2)#,"<hr></hr>"
                              ,"",specify_decimal(mean(data$raAdmin,na.rm=T),2),specify_decimal(mean(data$raPlan,na.rm=T),2),specify_decimal(mean(data$raPrun,na.rm=T),2),specify_decimal(mean(data$raRem,na.rm=T),2),specify_decimal(mean(data$raOther,na.rm=T),2)#,"<hr></hr>"
                              ,"",specify_decimal(mean(data$treeBoard,na.rm=T),2),specify_decimal(mean(data$writtenStratPlan,na.rm=T),2),61,specify_decimal(mean(data$ordinance,na.rm=T),2),specify_decimal(mean(data$ordinanceYear,na.rm=T),2)#,"<hr></hr>"
                              ,"",specify_decimal(mean(data$treeResourceInventory,na.rm=T),2),53,specify_decimal(mean(data$greenSpaceAreaMeters,na.rm=T),2),specify_decimal(mean(data$greenSpaceAreaFeet,na.rm=T),2)#,"<hr></hr>"
                              ,"",26,22,61,11#,"<hr></hr>"
                              ,"",specify_decimal(mean(data$strTrStockingLevel,na.rm=T),2),specify_decimal(mean(data$prkTrStockingLevel,na.rm=T),2),88,specify_decimal(mean(data$pubStrTrDensity,na.rm=T),2)#,"<hr></hr>"
                              ,"",specify_decimal(mean(data$currentPrunInsCyc,na.rm=T),2),specify_decimal(mean(data$desiredPrunInsCyc,na.rm=T),2),specify_decimal(mean(data$yearsOfPrunInsCyc,na.rm=T),2),specify_decimal(mean(data$percentAttPrunInsCyc,na.rm=T),2),specify_decimal(mean(data$activeManagement,na.rm=T),2)
                              ),
                 "Unit"=c("<b><u>Unit</b></u>","","($/capita)","($/tree)","(%)","(%)"#,"<hr></hr>"
                          ,"","(%)","(%)","(%)","(%)","(%)"#,"<hr></hr>"
                          ,"","(% yes)","(% yes)","(% yes)","(% yes)","(% yes)"#,"<hr></hr>"
                          ,"","(% yes)","(% yes)","(sq. m/capita)","(sq. ft/capita)"#,"<hr></hr>"
                          ,"","(% yes)","(%)","(% Goal Met)","(years)"#,"<hr></hr>"
                          ,"","(% Attained)","(% Attained)","(% Attained)","(Trees/Mile)"#,"<hr></hr>"
                          ,"","(years)","(years)","(years)","(% Attained)","(%)"
                          )
                 ),width='100%',align="lcccc",colnames=F,spacing="xs",
    sanitize.text.function=function(x){x})
    
    #Bottom of indicators summary text
    output$endMsg1<-renderText({
      paste(theAnalysis,format(Sys.Date(),"%B %d, %Y"),theAnalysis2,"&copy;",copyrightDate)
    })
    output$endMsg2<-renderText({
      indPageLine2
    })
    output$endMsg3<-renderText({
      indPageLine3
    })
    output$endMsg4<-renderText({
      paste(indPageLine4a,a("click here",href="https://www.uwsp.edu/cnr/Pages/Forestry---MTCUS.aspx"),indPageLine4b)
    })
  }
)


