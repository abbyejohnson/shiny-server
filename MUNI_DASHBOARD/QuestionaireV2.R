rm(list=ls())
list.of.packages<-c("devtools","shiny","shinydashboard","flexdashboard","dplyr","zipcode","tidyverse","shinyBS","shinyjs","geosphere","mapdata","maps","stringr","viridis","shinyalert","DT","shinythemes","shinyWidgets")
new.packages<-list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(devtools)
# if(!("dashboardthemes" %in% installed.packages()[,"Package"])) install_github("nik01010/dashboardthemes")
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(dplyr)
library(zipcode)
library(tidyverse)
library(shinyBS)
library(shinyalert)
# library(dashboardthemes)
library(shinythemes)
library(geosphere)
library(DT)
library(shinyWidgets)
library(mapdata)
library(maps)
library(stringr)
library(viridis)
library(shinyjs)
data(zipcode)


#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
#read in data
fips<-read_csv("//uwsp.edu/files/CNR/Projects/MuniDashboard/fips.csv")



#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
#globally defined variables
#Page Title
w=700
ptitle="Municipal Tree Survey"
dmgr="Demographic Information"
survey="Survey"
sec1="Community Framework"
# sec2="Page 2"
sec23="Budget and Management"
# sec4="Page 4"
sec6="Inventory and Contractors"
sec7="Operations Profile"
sec8="Assistance Programs"
rez="Results"

#Questions and Headers for Survey
askZip="What is your 5 digit ZIP Code? (Please fill this out first to help us find your city)"
askCity="Select your city:"
dsc="Don't see your city?"
ptc="Please type your city name"
askRegion="Please select your region based on the map below."
ner="Northeast (Blue)"
mwr="Midwest (Red)"
sr="South (Purple)"
wr="West (Green)"
askPop="What is the population of your community?"
T1_3MiMSWTr="How many miles of maintained municipal streest with trees does your community have?"
T1_3AcMMP="How many acres of managed municipal parks does your community have?"
T1_3AcMGS="How many acres of municipal natural areas or green spaces does your community have?"
T1_3AcMPWTr="How many acres of other municipal properties planted with trees does your community have?"
T1_6OvrcTr="Does someone in your community (i.e., employee, volunteer, consultatne, etc.) oversee the care of municipal street trees, park trees or other public trees?"
T1_11PriDaiMan="As the primary person accountable  for the daily management of the tree program or activities, what title best describes you?"
T1_11N="None, no one in charge"
T1_11AF="Arborist/Forister (City/Municipal/Urban)"
T1_11CCT="City Clerk/Treasurer"
T1_11CE="City Engineer"
T1_11CP="City Planner"
T1_11C="Consultant (e.g., Arborist, Forester on Retainer)"
T1_11FF="Forestry Foreman"
T1_11LA="Landscape Artist"
T1_11PRD="Parks & Recreation Director/Manager"
T1_11PWD="Public Works Director/Manager"
T1_11PWF="Public Works Foreman/Superintendent"
T1_11SF="Street Foreman/Superintendent"
T1_11TW="Tree Warden"
T1_11EPO="Elected Public Official"
T1_11O="Other"
T1_13="What training and/or credentials are collectively held by the staff responsible for tree activities and/or management of trees?"
T1_13NoTrain="No specific training or workshops"
T1_13InHOrJ="In-house and/or on-the-job-training"
T1_13AttTree="Attend tree care/management workshops"
T1_13ISACA="ISA Certified Arborist"
T1_13ISACMS="ISA Certified Municipal Specialist"
T1_13ISAAC="ISA Advanced Credential (BCMA, TRAQ)"
T1_13SAFCF="SAF Certified Forester"
T1_13StLorCre="State License or Credential"
T1_132yd="Two year degree"
T1_134yd="Four year degree"
T1_13Grad="Graduate degree"
T1_14="How strongly do you agree or disagree with these statements characterizing your community and the management of trees?"
T1_14AllOpr="All city departments operate with common goals and objectives to manage trees"
T1_14PriLanOwn="Large private landowners embrace city-wide goals and objectives through specific resource management plans related to tree care and management in greenspaces"
T1_14GreInd="Green industry (e.g., treecare) at large operates with high professional standards and commits to city-wide goals and objectives"
T1_14Citzens="Citizens understand and participate in urban forestry management at the neighborhood level"
T1_14Constits="All constituencies in the community interact for the benefit of the urban forest"
T1_14GenPub="The general public understands the value of trees to the community"
T1_14CoopInt="Cooperation and interaction occurs among neighboring communities and regional groups"
T1_15="How many public employees, including managers, are inolved with the municipal tree management program?"
T1_15TotEmp="# of Total Employees"
T1_15FullTiEq="# of Full Time Equivalents (2080 hour base year)"
T2_1TotBud="What is the total municipal budget (excluding school budget) for the current year?"
T2_3TotTreBud="What is the total annual budget of your municipality funded tree care activities and management from all municipal sources for the current year? (Include all tree activity expenses; include personnel, overhead, equipment, supplies, tree care and contract payments)"
T2_10CurrNeeds="Is your budget adequeate to meet current needs as defined in your work plan or your identified annual urban forestry budget needs? (This includes planting, maintenance, removal, inventory, education, etc.)"
T2_13="What percent of the total tree management budget from all sources is used for the following activities? (If it does not fall into one of the three areas, include it under other.  All should total 100%)"
T2_13TrPlan="Tree Planting"
T2_13TrPrun="Tree Pruning"
T2_13TrRem="Tree Removal"
T2_13Othr="Other"
T3_1="Which of the following organizations help establish policy for tree management in your community?"
T3_1CityCo="City Council Committee or Community Board"
T3_1ParksBr="Parks Board"
T3_1PubWr="Public Works Board"
T3_1TrBoard="Tree Board"
T3_1PlanCom="Planning Commission"
T3_1ShdeTr="Shade Tree/Urban Forestry Commission"
T3_1Other="Other"
T3_2AuthTrBd="Does your community have a government-authorized tree board, parks board, city department, commission, or similar group that helps develop and/or administer tree management policy?"
T3_5MuniOrd="Does your municipality have one or more municipal ordinances that pertain to trees?"
T3_5YrUpdt="In what year was the ordinance last updated or reviewed?"
T3_6="What topics do your community tree ordinances include? (Check all that apply)"
T3_6ReqPreDev="Requires preservation of trees during development"
T3_6IdPreHer="Identifies preservation of heritage or significant trees"
T3_6PhrTrTop="Prohibits tree topping"
T3_6RegTrPub="Regulates tree species which may or may not be planted on public property (approved tree list)"
T3_6RegTrPri="Regulates tree species which may or may not be planted on private property (approved tree lsit)"
T3_6None="We have no ordinances regarding any of these"
T3_9WrtStgPln="Does your community have a written strategic plan for urban forestry, tree management, open space, green infrastructure, or land use management that includes trees?"
T3_10="Which types of plans (seperate or combined together) do you have that incorporate the management of trees and other vegetation?"
T3_10CityMas="City master/Comprehensive"
T3_10CityMasYrUpdt="In what year was your City master/Comprehensive plan last reviewed or updated?"
T3_10InsDsRead="Insect/disease readiness"
T3_10InsDsReadYrUpdt="In what year was your Insect/disease readiness plan last reviewed or updated?"
T3_10MunWtr="Municipal watershed"
T3_10MunWtrYrUpdt="In what year was your Municipal watershed plan last reviewed or updated?"
T3_10StrEmer="Storm/emergency response"
T3_10StrEmerYrUpdt="In what year was your Storm/emergency response plan last reviewed or updated?"
T3_10StrWtrM="Storm water management"
T3_10StrWtrMYrUpdt="In what year was your Storm water management plan last reviewed or updated?"
T3_10TrRskM="Tree risk management"
T3_10TrRskMYrUpdt="In what year was your Tree risk management plan last reviewed or updated?"
T3_10UrbForMgmt="Tree/urban forest management"
T3_10UrbForMgmtYrUpdt="In what year ws your Tree/Urban forest management plan last reviewed or updated?"
T3_10UrbForStr="Urban forest strategic"
T3_10UrbForStrYrUpdt="In what year was your Urban forest strategic plan last reviewed or updated?"
T3_10Other="Other"
# T3_10OtherYrUpdt=paste("In what year was your ", input$other310," plan last reviewed or updated?",sep="")
T3_13="Which of the following standards of practice does your community officially incorporate into tree management procedures?"
T3_13Ansi760="ANSI Z60.1"
T3_13AnsiA300="ANSI A300"
T3_13TrCtyUSA="Tree City USA"
T3_13AnsiZ133="ANSI Z133.1"
T3_13ISABMP="ISA Best Management Practices"
T3_13NoneStd="I have not heard of these standards"
T5_4="Which of the following are true about contractors hired by your city?"
T5_4ANSIZ6="Require the use of ANSI Z60.1 standards"
T5_4ANSIZ1="Require the use of ANSI Z133.1 standards"
T5_4ANSIA="Requre use of ANSI A300 standards"
T5_4PrefISA="Hiring preference given to ISA Certified Arborists"
T5_4PrefTCI="Hiring preference given to TCIA Accredited companies"
T5_4None="None of these are true"
T6_1TrInv="Does your community have a tree inventory? (An inventory is any type of record of public trees in your community.)"
T6_3YrUpd="In what year was your most recent update to your tree inventory?"
T6_6="What areas does the tree inventory include?"
T6_6StrTr="Street Trees"
T6_6PrkTr="Park Trees"
T6_6MunGr="Municipal greenbelts"
T6_6MunWd="Municipal woodlots"
T6_6PriTr="Private trees (sample survey)"
T6_6Other="Other municipal properties"
T6_6None="No tree inventory"
T6_7="Which type of inventory collection and analysis methods have been used to describe your community tree population?"
T6_7WndsSr="Windshield survey"
T6_7SmpSr="Sample survey"
T6_7Census="100% population (total, census)"
T6_7RmSens="Remote sensing (i.e., aerial photos, satellites)"
T6_7CanCo="Canopy cover analysis"
T6_7iTrStr="i-Tree Streets analysis"
T6_7iTrEco="i-Tree Eco analysis"
T6_7Other="Other"
T6_8="Which ways is your tree inventory linked to spatial locatins?"
T6_8CtyPar="City parcel data"
T6_8GPSGIS="GPS/GIS"
T6_8StrAdr="Street address"
T6_8Other="Other"
T6_13TreDm="Is your tree inventory used to identify tree diameter?"
T6_14SelTrS="Is your tree inventory used to direct the selection of tree species for planting?"
T6_15CanGl="Does your municipality have a tree canopy goal?"
T6_16CanGl="What is your tree canopy cover goal?"
T6_16CurCn="What is your percent canopy cureently?"
T6_16YrToGl="What year is identified to meet your canopy goal?"
T6_18PubTr="What is the total number of publically owned trees in your community?"
T6_19="What is the total number of publically owned trees in the following locations?"
T6_19StrTr="Street trees (along municipal right of way, between curb and sidewalk, alley trees, etc.)"
T6_19PrkTr="Park trees (maintained areas)"
T6_19MuniTr="Municipal trees on other property (building grounds, cemeteries, treatment plants, industrial parks, etc.)"
T6_20="How many empty/vacant spaces do you have for potential tree plantings in the following locations?"
T6_20StrTr="Stree trees (along municipal right of way, between curb and sidewalk, alley trees, etc.)"
T6_20PrkTr="Park trees (maintained areas)"
T6_20MuniTr="Municipal trees on other property (building grounds, cemeteries, treatment plants, industrial parks, etc.)"
T6_21ValPubTr="If known, what is the value of the publically owned trees?"
T6_22FinRec="Is the value of publically owned trees carried in the city financial records as a city asset?"
T7_1TrPlnt="What is the number of trees planted on all municipal properties in the last year?"
T7_1TrRem="What is the number of trees pruned on all municipal properties in the last year?"
T7_2="What percent of tree care (pruning, pest control, etc.) is done on a systematic (regualrly scheduled) cycle and what percent on demand as reactive (complaints, hazardous situations, crisis, post storm etc.)? (Total=100%)"
T7_2SysSch="% Systematic (Scheduled)"
T7_2ReaDem="% Reactive (on Demand)"
T7_4AppPrun="How would you best describe your tree management programs's  approach to pruning?"
T7_4DP="Don't prune"
T7_4PrunAsNeed="Pruning as needed/for emergency only"
T7_4RegPrun="Regular pruning cycle"
T7_4Other="Other"
askOtherPrun="How would you describe your current pruning practices?"
T7_4CurCyc="What is your current pruning cycle (in years)?"
T7_4DesCyc="What is your desired pruning cycle (in years)?"
T7_10TecAst="Does your community provide technical assistance (information) for tree maintenance on private property?"
T7_10FinAst="Doe your community provide financial assistance for specific insect or diseased tree removal on private property?"
T7_12TrRskM="Does your commuunity regularly conduct tree risk management (hazard tree identification)?"
T7_14WritTrRsk="Does your community have a written tree risk managemnt policy?"
T7_15="Which of the following statements reflects your overall tactic to tree risk inspection?"
T7_15RspCC="Response to citizen complaints"
T7_15RSKInsp="Part of a routine tree risk inspection program"
T7_15RtTrM="Part of a routine tree maintenance"
T7_15SrvyID="Windshield survey to identify high risk trees"
T7_15Other="Other"
T7_17="When a public tree is removed, which of the following are typical ways that solid wood/residue is disposed of?"
T7_17BfErgy="Biofuel for energy"
T7_17BrnOp="Burned in open"
T7_17FrWd="Firewood"
T7_17Landfilled="Landfilled"
T7_17MdFrn="Made into furniture/flooring/art"
T7_17Mulch="Mulch"
T7_17ProLum="Processed into lumber"
T7_17SleWd="Sale of round wood (e.g., sawlogs, pulp, veneer)"
T7_17Other="Other"
T8_4AwdCm="During the past five years have you received any awards from any groupfor your community tree activities or management?"
T8_5EduPr="Do municipal staff provide educationaal presentations to city residents in regard to tree care?"
T8_8TrCity="Is your community currently a Tree City USA?"
T8_10ObvsP="Does your community have an Arbor Day observance and proclamation?"
ask31Other="What other organization do you have that helps establish policy for tree management in your community?"
ask310Other="What other type of plan do you have that incorporate the management of trees and other vegetation?"
ask66Other="What other municipal properties do your tree inventory include?"
ask67Other="What other type of inventory collection and analysis method do you use?"
ask68Other="What other way is your tree inventory linked to spatial locations?"
ask715Other="What other tree inspection tactic do you use?"
ask717Other="What other disposal type do you use for public trees that are removed?"

#Info buttons
infoButton="Additional Information"
ZIP="ZIP Code"
zipInfo="Use the 5 digit ZIP code of your community as given by the US Post Office."
pop="Population"
popInfo="Use the most recent estimate of the number of people living in your community"
reg="US Census Regions"
regInfo="Alaska and Hawaii fall into the West US Census defined Region."
regButton="HI & AK Info"
plzClick="Please click the button below and complete the table: "
click="Click here"

#Submit buttons/end of page stuff
submit="Submit"
highPop="Your populaiton entered is more than 10% greater than our records"
lowPop="Your population entered is more than 10% less than out records"
success="Submitted!"
failure="Incomplete!"
toNext="Please continue to the next section."
tryAgain="Try Again!"
missings="You have not answered each question."
not100="Your percent of tree care that is systematic and reactive do not add up to 100%"
not100Bud="Your percent of tree management budget from all sources does not equal 100%"
n="No"
y="Yes"
d="Developing"
tblCplt="You have successfully completed the table.  Please continue with the survey."
tblIncplt="You have not completed the table."

CMap="US Census Regions"
CMSTitle="Northeaste=Blue,Midwest=Red,South=Purple,West=Green"
ne="Northeast"
mw="Midwest"
south="South"
west="West"
#end of globally defined variables
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

text<-c(T1_14AllOpr,T1_14PriLanOwn,T1_14GreInd,T1_14Citzens,T1_14Constits,T1_14GenPub,T1_14CoopInt)
responses<-c("Strongly Disagree","Disagree","Agree","Strongly Agree")
tableScores<-rep(0,7)

#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
#helper functions
#return the cities in the provided zip code
getCities<-function(zip){
  rownames(zipcode)=zipcode$zip
  index<-which(zip==zipcode[,1])
  foundCity<-zipcode$city[index]
  return(foundCity)
}

#return the population of the selected  city
getpop<-function(zip,city){
  rownames(zipcode)=zipcode$zip
  state<-zipcode[zip,3]
  i<-which(state==fips[,1] & city==fips[,2])
  p<-(as.numeric(as.character((fips$Population[i]))))
  return(p[1])
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


#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

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


body<-dashboardBody(
  # shinyDashboardThemes("onenote"),
  tabItems(
    tabItem(tabName="demographics",
          useShinyalert(),
          fluidRow(column(width=6,align="center",h2(dmgr))),
          #ZIP Code
          fluidRow(box(textInput("ZIP",
                    askZip),
          actionLink("zipCode",infoButton),
          br(),
          br(),
          bsModal("zipDef",ZIP,"zipCode",
                  p(zipInfo)))),
          #city drop down
          fluidRow(box(selectInput("city",
                      askCity,
                      choices=NULL),
          #if city doesn't populate
          checkboxInput("diffCity",dsc),
          uiOutput("condCity"))),
          #population input
          fluidRow(box(textInput("pop",
                    askPop),
          actionLink("popInfo",infoButton))),
          bsModal("popDef",pop,"popInfo",
                  p(popInfo)),
          #region input
          fluidRow(box(selectInput("region",
                                   askRegion,
                                   c(ner,mwr,sr,wr)),
                       actionLink("regionInfo",regButton))),
          bsModal("regDef",reg,"regionInfo",
                  p(regInfo)),
          fluidRow(box(strong(reg),
                       plotOutput("plot1"))),
          br(),
          fluidRow(
            column(6, align="center",
                   actionButton("subdemo",submit),
                   tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
            ))
          ),
   tabItem(tabName="s1",
          fluidRow(column(width=12,align="center",h2(sec1))),
          fluidRow(column(width=6,box(width=NULL,
            radioButtons("Q1_6OvrcTr",
                       T1_6OvrcTr,
                       c(y,n),
                       selected=character(0),width='100%')),
          box(width=NULL,
               checkboxGroupInput("Q1_13",
                             T1_13,
                             c(T1_13NoTrain,T1_13AttTree,T1_13ISACA,T1_13ISACMS,T1_13ISAAC,T1_13SAFCF,T1_13StLorCre,T1_132yd,T1_134yd,T1_13Grad),
                             selected=character(0),width='75%')),
          box(width=NULL,
              numericInput("Q1_3MiMSWTr",T1_3MiMSWTr,value=character(0),step=.01)),
          box(width=NULL,
              numericInput("Q1_3AcMMP",T1_3AcMMP,value=character(0),step=.01)),
          box(width=NULL,
              numericInput("Q1_3AcMGS",T1_3AcMGS,value=character(0),step=.01)),
          box(width=NULL,
              numericInput("Q1_3AcMPWTr",T1_3AcMPWTr,value=character(0),step=.01))
          ),
          column(width=6,box(width=NULL,strong(paste(plzClick,T1_14,sep="")),
                             br(),
                             actionButton("btnContinue","Click here for table")),
          box(width=NULL,
                     radioButtons("Q1_11PriDaiMan",
                                  T1_11PriDaiMan,
                                  c(T1_11N,T1_11AF,T1_11CCT,T1_11CE,T1_11CP,T1_11C,T1_11FF,T1_11LA,T1_11PRD,T1_11PWD,T1_11PWF,T1_11SF,T1_11TW,T1_11EPO,T1_11O),
                                  selected=character(0),width='100%',inline=F),
                     uiOutput("elPubOff"),
                     uiOutput("otherTitleName")),
          box(width=NULL,
              strong(T1_15),
              numericInput("Q1_15TotEmp",T1_15TotEmp,value=character(0),step=1),
              numericInput("Q1_15FullTiEq",T1_15FullTiEq,value=character(0),step=0.05)))),
          fluidRow(
            column(6, align="center", offset = 3,
                   actionButton("sub1",submit),
                                tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
                   ))
          ),
   tabItem(tabName="s23",
           fluidRow(column(width=12,align="center",h2(sec23))),
           fluidRow(column(width=6,box(width=NULL,
                                       numericInput("Q2_1TotBud",T2_1TotBud,value=character(0),step=0.01)),
           box(width=NULL,
                numericInput("Q2_3TotTreBud",T2_3TotTreBud,value=character(0),step=0.01)),
           box(width=NULL,
                radioButtons("Q2_10CurrNeeds",
                        T2_10CurrNeeds,
                        c(y,n),
                        selected=character(0))),
           box(width=NULL,
               checkboxGroupInput("Q3_6",
                                  T3_6,
                                  c(T3_6ReqPreDev,T3_6IdPreHer,T3_6PhrTrTop,T3_6RegTrPub,T3_6RegTrPri,T3_6None),
                                  selected=character(0))),
           box(width=NULL,
               radioButtons("Q3_9WrtStgPln",
                            T3_9WrtStgPln,
                            c(y,n),
                            selected=character(0))),
           box(width=NULL,
               radioButtons("Q3_5MuniOrd",
                            T3_5MuniOrd,
                            c(y,n),
                            selected=character(0)),
               uiOutput("ordUpdt")),
           box(width=NULL,
               radioButtons("Q3_2AuthTrBd",
                            T3_2AuthTrBd,
                            c(y,n),
                            selected=character(0))),
           box(width=NULL,checkboxGroupInput("Q3_13",
                                             T3_13,
                                             c(T3_13Ansi760,T3_13AnsiA300,T3_13TrCtyUSA,T3_13AnsiZ133,T3_13ISABMP,T3_13NoneStd),
                                             selected=character(0)))
           ),
           column(width=6,
                  box(width=NULL,strong(T2_13),
                      numericInput("Q2_13TrPlan",T2_13TrPlan,value=character(0),step=1),
                      numericInput("Q2_13TrPrun",T2_13TrPrun,value=character(0),step=1),
                      numericInput("Q2_13TrRem",T2_13TrRem,value=character(0),step=1),
                      numericInput("Q2_13Othr",T2_13Othr,value=character(0),step=1)),
                  box(width=NULL,checkboxGroupInput("Q3_1",
                                                    T3_1,
                                                    c(T3_1CityCo,T3_1ParksBr,T3_1PubWr,T3_1TrBoard,T3_1PlanCom,T3_1ShdeTr),
                                                    selected=character(0)),
                      checkboxInput("Q3_1Other",T3_1Other),
                      uiOutput("otherOrg")),
                  box(width=NULL,checkboxGroupInput("Q3_10",
                                                    T3_10,
                                                    c(T3_10CityMas,T3_10InsDsRead,T3_10MunWtr,T3_10StrEmer,T3_10StrWtrM,T3_10TrRskM,T3_10UrbForMgmt,T3_10UrbForStr),
                                                    selected=character(0)),
                      checkboxInput("Q3_10Other",T3_10Other),
                      uiOutput("otherMngmt"),
                      uiOutput("cityMasUpdt"),
                      uiOutput("insRedUpdt"),
                      uiOutput("muniWtUpdt"),
                      uiOutput("stormResUpdt"),
                      uiOutput("stormWM"),
                      uiOutput("trRskManUpdt"),
                      uiOutput("urbForManUpdt"),
                      uiOutput("UFStratUpdt"),
                      uiOutput("OPlanUpdt")))),
           fluidRow(
             column(6, align="center", offset = 3,
                    actionButton("sub23",submit),
                    tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
             ))
   ),
   tabItem(tabName="s6",
           fluidRow(column(width=12,align="center",h2(sec6))),
           fluidRow(column(width=6,box(width=NULL,checkboxGroupInput("Q5_4",
                                                                     T5_4,
                                                                     c(T5_4ANSIZ6,T5_4ANSIZ1,T5_4ANSIA,T5_4PrefISA,T5_4PrefTCI,T5_4None),
                                                                     selected=character(0))),
          box(width=NULL,radioButtons("Q6_1TrInv",
                                      T6_1TrInv,
                                      c(y,n),
                                      selected=character(0),width='100%')),
           box(width=NULL,numericInput("Q6_3YrUpd",T6_3YrUpd,value=character(0),step=1)),             
           box(width=NULL,checkboxGroupInput("Q6_6",
                              T6_6,
                              c(T6_6StrTr,T6_6PrkTr,T6_6MunGr,T6_6MunWd,T6_6PriTr),
                              selected=character(0),width='100%'),
                checkboxInput("Q6_6Other",T6_6Other),
                uiOutput("otherTI")),
           box(width=NULL,checkboxGroupInput("Q6_7",
                              T6_7,
                              c(T6_7WndsSr,T6_7SmpSr,T6_7Census,T6_7RmSens,T6_7CanCo,T6_7iTrStr,T6_7iTrEco),
                              selected=character(0),width='100%'),
                checkboxInput("Q6_7Other",T6_7Other),
                uiOutput("otherIC")),
          box(width=NULL,numericInput("Q6_21ValPubTr",T6_21ValPubTr,value=character(0),step=0.01)),
          box(width=NULL,radioButtons("Q6_22FinRec",
                                      T6_22FinRec,
                                      c(y,n),
                                      selected=character(0),width='100%'))),
           column(width=6,box(width=NULL,checkboxGroupInput("Q6_8",
                              T6_8,
                              c(T6_8CtyPar,T6_8GPSGIS,T6_8StrAdr),
                              selected=character(0),width='100%'),
                              checkboxInput("Q6_8Other",T6_8Other),
                              uiOutput("otherSL")),
           box(width=NULL,radioButtons("Q6_13TreDm",
                         T6_13TreDm,
                        c(y,n),
                        selected=character(0),width='100%')),
           box(width=NULL,radioButtons("Q6_14SelTrS",
                        T6_14SelTrS,
                        c(y,n),
                        selected=character(0),width='100%')),
           box(width=NULL,radioButtons("Q6_15CanGl",
                                       T6_15CanGl,
                                       c(y,n,d),
                                       selected=character(0),width='100%'),
               uiOutput("canGoal"),
               uiOutput("canCurr"),
               uiOutput("yearsToGl")),
           box(width=NULL,numericInput("Q6_18PubTr",T6_18PubTr,value=character(0),step=1)),
           box(width=NULL,strong(T6_19),
                          numericInput("Q6_19StrTr",T6_19StrTr,value=character(0),step=1),
                          numericInput("Q6_19PrkTr",T6_19PrkTr,value=character(0),step=1),
                          numericInput("Q6_19MuniTr",T6_19MuniTr,value=character(0),step=1)),
           box(width=NULL,strong(T6_20),
               numericInput("Q6_20StrTr",T6_20StrTr,value=character(0),step=1),
               numericInput("Q6_20PrkTr",T6_20PrkTr,value=character(0),step=1),
               numericInput("Q6_20MuniTr",T6_20MuniTr,value=character(0),step=1))
           )),
           fluidRow(
             column(6, align="center", offset = 3,
                    actionButton("sub6",submit),
                    tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
             ))
           ),
   tabItem(tabName="s7",
           fluidRow(column(width=12,align="center",h2(sec7))),
           fluidRow(column(width=6,
           box(width=NULL,numericInput("Q7_1TrPlnt",T7_1TrPlnt,value=character(0),step=1),
                          numericInput("Q7_1TrRem",T7_1TrRem,value=character(0),step=1)),
           box(width=NULL,strong(T7_2),
               numericInput("Q7_2SysSch",T7_2SysSch,value=NULL,min=0,max=100),
               numericInput("Q7_2ReaDem",T7_2ReaDem,value=NULL,min=0,max=100)),
           box(width=NULL,radioButtons("Q7_4AppPrun",
                                       T7_4AppPrun,
                                       c(T7_4DP,T7_4PrunAsNeed,T7_4RegPrun,T7_4Other),
                                       selected=character(0),width='100%'),
               uiOutput("currCyc"),
               uiOutput("desCyc"),
               uiOutput("prunApp")),
           box(width=NULL,radioButtons("Q7_12TrRskM",
                                       T7_12TrRskM,
                                       c(y,n),
                                       selected=character(0),width='100%'))),
           column(width=6,box(width=NULL,radioButtons("Q7_10FinAst",
                                                      T7_10FinAst,
                                                      c(y,n),
                                                      selected=character(0))),
                  box(width=NULL,radioButtons("Q7_10TecAst",
                                              T7_10TecAst,
                                              c(y,n),
                                              selected=character(0))),
                  box(width=NULL,radioButtons("Q7_14WritTrRsk",
                                                      T7_14WritTrRsk,
                                                      c(y,n),
                                                      selected=character(0),width='100%')),
                  box(width=NULL,checkboxGroupInput("Q7_15",
                              T7_15,
                              c(T7_15RspCC,T7_15RSKInsp,T7_15RtTrM,T7_15SrvyID),
                              selected=character(0),width='100%'),
                              checkboxInput("Q7_15Other",T7_15Other),
                              uiOutput("otherTactic")),
                  box(width=NULL,checkboxGroupInput("Q7_17",
                                                      T7_17,
                                                      c(T7_17BfErgy,T7_17BrnOp,T7_17FrWd,T7_17Landfilled,T7_17MdFrn,T7_17Mulch,T7_17ProLum,T7_17SleWd),
                                                      selected=character(0),width='100%'),
                      checkboxInput("Q7_17Other",T7_17Other),
                      uiOutput("otherDisposal")))),
           fluidRow(
             column(6, align="center", offset = 3,
                    actionButton("sub7",submit),
                    tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
             ))
           ),
   tabItem(tabName="s8",
           fluidRow(column(width=12,align="center",h2(sec8))),
           fluidRow(column(width=6,
                  box(width=NULL,radioButtons("Q8_4AwdCm",
                                               T8_4AwdCm,
                                               c(y,n),
                                               selected=character(0))),
                  box(width=NULL,radioButtons("Q8_5EduPr",
                                               T8_5EduPr,
                                               c(y,n),
                                               selected=character(0)))),
           column(width=6,box(width=NULL,radioButtons("Q8_8TrCity",
                                                      T8_8TrCity,
                                                      c(y,n),
                                                      selected=character(0))),
                  box(width=NULL,radioButtons("Q8_10ObvsP",
                                              T8_10ObvsP,
                                              c(y,n),
                                              selected=character(0))))
           ),
           fluidRow(
             column(6, align="center", offset = 3,
                    actionButton("sub8",submit),
                    tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
   ))),
   tabItem(tabName="srez",
           fluidRow(column(width=12,align="center",h2(rez))),
           fluidRow(column(width=12,align="center",h4("Results Dashboard will be displayed here"))),
           fluidRow(column(width=12,align="left",textOutput("popGroup"))),
           fluidRow(column(width=12,align="left",textOutput("regionText"))))
   ))

sidebar<-dashboardSidebar(
  sidebarMenu(id="tabs",
    menuItem(dmgr,tabName="demographics",icon=icon("user-edit")),
    menuItem(survey,icon=icon("poll"),
             menuSubItem(sec1,tabName="s1"),
             # menuSubItem(sec2,tabName="s2"),
             menuSubItem(sec23,tabName="s23"),
             # menuSubItem(sec4,tabName="s4"),
             menuSubItem(sec6,tabName="s6"),
             menuSubItem(sec7,tabName="s7"),
             menuSubItem(sec8,tabName="s8")),
    menuItem(rez,tabName="srez")
    )
)

ui<-dashboardPage(
  dashboardHeader(title=ptitle),
  sidebar,
  body,
  useShinyjs()
)

server<-function(session,input,output){
  #hide sidebar after new tab is selected
  observeEvent(input$tabs,{
    #desktop version
    addClass(selector = "body",class="sidebar-collapse")
    #mobile version-do we need this?
    removeClass(selector="body",class="sidebar-open")
  })
  
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
      annotate("text",x=-113,y=40,label="West",size=12,angle=27)
    })
  
  #update city based on entered zip code
  observe({
    x<-getCities(input$ZIP)
    updateSelectInput(session,"city",askCity,choices=unique(x))
  })
  #update city based on user input
  observe({
    x<-input$tCity
    updateSelectInput(session,"city",askCity,choices=x)
  })
  #update population for the selected city
  observe({
    areapop<-getpop(input$ZIP,input$city)
    updateNumericInput(session,"pop",askPop,
                       value=unique(areapop),min=1,max=9000000,step=1)
  })
  #allow the user to enter their city
  output$condCity<-renderUI({
    if(input$diffCity){
      textInput("tCity",ptc)
    }
  })
  #ask what there publicly elected official title is
  output$elPubOff<-renderUI({
    req(input$Q1_11PriDaiMan)
    if(input$Q1_11PriDaiMan==T1_11EPO){
      textInput("officialTitle",askOffTitle)
    }
  })
  #ask what their title is
  output$otherTitleName<-renderUI({
    req(input$Q1_11PriDaiMan)
    if(input$Q1_11PriDaiMan==T1_11O){
      textInput("otherTitle",askOthTitle)
    }
  })
  #ask what the other is for Sec 3 Q. 1
  output$otherOrg<-renderUI({
    if(input$Q3_1Other){
      textInput("other31",ask31Other)
    }
  })
  #get when the ordinance was last updated
  output$ordUpdt<-renderUI({
    req(input$Q3_5MuniOrd)
    if(input$Q3_5MuniOrd==y){
      numericInput("Q3_5YrUpdt",T3_5YrUpdt,value=character(0),step=1)
    }
  })
  #ask what the other is for Sec 3 Q. 10
  output$otherMngmt<-renderUI({
    if(input$Q3_10Other){
      textInput("other310",ask310Other)
    }
  })
  #ask year for selected answers in Sec 3 Q 10
  #Get year updated for city master/comprehensive plan
  output$cityMasUpdt<-renderUI({
    req(input$Q3_10)
    t=F
    for(i in 1:length(input$Q3_10)){
      if(input$Q3_10[i]==T3_10CityMas){
        t=T
      }
    }
    if(t){
      numericInput("Q3_10CityMasYrUpdt",T3_10CityMasYrUpdt,value=character(0),step=1)
    }
  })
  #Get year updated for insect/disease readiness
  output$insRedUpdt<-renderUI({
    req(input$Q3_10)
    t=F
    for(i in 1:length(input$Q3_10)){
      if(input$Q3_10[i]==T3_10InsDsRead){
        t=T
      }
    }
    if(t){
      numericInput("Q3_10InsDsReadYrUpdt",T3_10InsDsReadYrUpdt,value=character(0),step=1)
    }
  })
  #Get year updated for municipal watershed
  output$muniWtUpdt<-renderUI({
    req(input$Q3_10)
    t=F
    for(i in 1:length(input$Q3_10)){
      if(input$Q3_10[i]==T3_10MunWtr){
        t=T
      }
    }
    if(t){
      numericInput("Q3_10MunWtrYrUpdt",T3_10MunWtrYrUpdt,value=character(0),step=1)
    }
  })
  #Get year updated for storm/emergency response
  output$stormResUpdt<-renderUI({
    req(input$Q3_10)
    t=F
    for(i in 1:length(input$Q3_10)){
      if(input$Q3_10[i]==T3_10StrEmer){
        t=T
      }
    }
    if(t){
      numericInput("Q3_10StrEmerYrUpdt",T3_10StrEmerYrUpdt,value=character(0),step=1)
    }
  })
  #Get year updated for storm water management
  output$stormWM<-renderUI({
    req(input$Q3_10)
    t=F
    for(i in 1:length(input$Q3_10)){
      if(input$Q3_10[i]==T3_10StrWtrM){
        t=T
      }
    }
    if(t){
      numericInput("Q3_10StrWtrMYrUpdt",T3_10StrWtrMYrUpdt,value=character(0),step=1)
    }
  })
  #Get year updated for tree risk management
  output$trRskManUpdt<-renderUI({
    req(input$Q3_10)
    t=F
    for(i in 1:length(input$Q3_10)){
      if(input$Q3_10[i]==T3_10TrRskM){
        t=T
      }
    }
    if(t){
      numericInput("Q3_10TrRskMYrUpdt",T3_10TrRskMYrUpdt,value=character(0),step=1)
    }
  })
  #Get year updated for tree/urban forest management
  output$urbForManUpdt<-renderUI({
    req(input$Q3_10)
    t=F
    for(i in 1:length(input$Q3_10)){
      if(input$Q3_10[i]==T3_10UrbForMgmt){
        t=T
      }
    }
    if(t){
      numericInput("Q3_10UrbForMgmtYrUpdt",T3_10UrbForMgmtYrUpdt,value=character(0),step=1)
    }
  })
  #Get year updated for urban forest strategic plan
  output$UFStratUpdt<-renderUI({
    req(input$Q3_10)
    t=F
    for(i in 1:length(input$Q3_10)){
      if(input$Q3_10[i]==T3_10UrbForStr){
        t=T
      }
    }
    if(t){
      numericInput("Q3_10UrbForStrYrUpdt",T3_10UrbForStrYrUpdt,value=character(0),step=1)
    }
  })
  #Get year updated for other plan
  output$OPlanUpdt<-renderUI({
    req(input$other310)
    if(input$Q3_10Other)
      numericInput("Q3_10UrbForStrYrUpdt",paste("In what year was your ", input$other310," plan last reviewed or updated?",sep=""),value=character(0),step=1)
  })
  #ask what other for Sec 6 Q 6
  output$otherTI<-renderUI({
    if(input$Q6_6Other){
    textInput("other66",ask66Other)
    }
  })
  #ask what other for Sec 6 Q 7
  output$otherIC<-renderUI({
    if(input$Q6_7Other){
    textInput("other67",ask67Other)
    }
  })
  #ask what other for Sec 6 Q 8
  output$otherSL<-renderUI({
    if(input$Q6_8Other){
    textInput("other68",ask68Other)
    }
  })
  #ask what the canopy goal % is
  output$canGoal<-renderUI({
    req(input$Q6_15CanGl)
    if(input$Q6_15CanGl==y){
      numericInput("Q6_16CanGl",T6_16CanGl,value=character(0),step=1)
    }
  })
  #ask what the current canopy % is
  output$canCurr<-renderUI({
    req(input$Q6_15CanGl)
    if(input$Q6_15CanGl==y){
    numericInput("Q6_16CurCn",T6_16CurCn,value=character(0),step=1)
    }
  })
  #ask how many years they have to accomplish the goal
  output$yearsToGl<-renderUI({
    req(input$Q6_15CanGl)
    if(input$Q6_15CanGl==y){
      numericInput("Q6_16YrToGl",T6_16YrToGl,value=character(0),step=1)
    }
  })
  #ask what the current cycle length is
  output$currCyc<-renderUI({
    req(input$Q7_4AppPrun)
    if(input$Q7_4AppPrun==T7_4RegPrun){
      numericInput("Q7_4CurCyc",T7_4CurCyc,value=character(0),step=1)
    }
  })
  #ask what the desired cycle length is
  output$desCyc<-renderUI({
    req(input$Q7_4AppPrun)
    if(input$Q7_4AppPrun==T7_4RegPrun){
      numericInput("Q7_4DesCyc",T7_4DesCyc,value=character(0),step=1)
    }
  })
  #specify Sec 7 Q 4 other
  output$prunApp<-renderUI({
    req(input$Q7_4AppPrun)
    if(input$Q7_4AppPrun==T7_4Other){
      textInput("otherPrun",askOtherPrun)
    }
  })
  #ask what other for Sec 7 Q 15
  output$otherTactic<-renderUI({
    if(input$Q7_15Other){
      textInput("other715",ask715Other)
    }
  })
  #ask what other for Sec 7 Q 17
  output$otherDisposal<-renderUI({
    if(input$Q7_17Other){
      textInput("other717",ask717Other)
    }
  })
  
  #demographics submit button responses
  observeEvent(input$subdemo,{
    req(input$ZIP)
    req(input$pop)
    if(length(getpop(input$ZIP,input$city))==0){
      newtab <- switch(input$tabs, "demographics" = "s1","s1" = "demographics")
      updateTabItems(session, "tabs", newtab)  
    }
    else if(input$pop >= 1.1*unique(getpop(input$ZIP,input$city))){
      shinyalert(highPop,paste("Our records show a population of ",unique(getpop(input$ZIP,input$city))," for ",input$city,".  Please check your entry.  If it is correct, proceed.  If it is not correct, re-enter population and re-submit.", sep=""),type="warning")
    }
    else if(input$pop <= 0.9*unique(getpop(input$ZIP,input$city))){
      shinyalert(lowPop,paste("Our records show a population of ",unique(getpop(input$ZIP,input$city))," for ",input$city,".  Please check your entry.  If it is correct, proceed.  If it is not correct, re-enter population and re-submit.", sep=""),type="warning")
    }
    else{
      newtab <- switch(input$tabs, "demographics" = "s1","s1" = "demographics")
      updateTabItems(session, "tabs", newtab)    
    }
  })

  #sec1 submit button
 observeEvent(input$sub1,{
   req(input$Q1_15TotEmp)
   req(input$Q1_15FullTiEq)
   req(input$Q1_3AcMGS)
   req(input$Q1_3AcMMP)
   req(input$Q1_3AcMPWTr)
   req(input$Q1_3MiMSWTr)
   if(sum(as.numeric(tableScores==0))){
     shinyalert(failure,tblIncplt,type="error")
   }
   else if(is_empty(input$Q1_6OvrcTr)|is_empty(input$Q1_13)|is_empty(input$Q1_11PriDaiMan)){
      shinyalert(failure,missings,type="warning")
   }
   else{
     # shinyalert(success,sum(as.numeric(tableScores,type="success")))
     newtab <- switch(input$tabs, "s1" = "s23","s23" = "s1")
     updateTabItems(session, "tabs", newtab)
   }
  })

  #sec2&3 submit button
  observeEvent(input$sub23,{
    req(input$Q2_1TotBud)
    req(input$Q2_3TotTreBud)
    req(input$Q2_13TrPlan)
    req(input$Q2_13TrRem)
    req(input$Q2_13TrPrun)
    req(input$Q2_13Othr)
    if(input$Q2_13TrPlan+input$Q2_13TrRem+input$Q2_13TrPrun+input$Q2_13Othr!=100){
      shinyalert(failure,not100Bud,type="error")
    }
    else if(is_empty(input$Q2_10CurrNeeds)|is_empty(input$Q3_5MuniOrd)|is_empty(input$Q3_6)|is_empty(input$Q3_9WrtStgPln)|(is_empty(input$Q3_10)&!input$Q3_10Other)|is_empty(input$Q3_13)|(is_empty(input$Q3_1)&!input$Q3_1Other)|is_empty(input$Q3_2AuthTrBd)){
      shinyalert(failure,missings,type="warning")
    }
    else{
      newtab <- switch(input$tabs, "s23" = "s6","s6" = "s23")
      updateTabItems(session, "tabs", newtab)
    }
  })
  
  #sec6 submit button
  observeEvent(input$sub6,{
    req(input$Q6_3YrUpd)
    if(input$Q6_3YrUpd>3019|input$Q6_3YrUpd<1519){
      shinyalert(failure,"Invalid year",type="error")
    }
    else if(is_empty(input$Q6_1TrInv)|(is_empty(input$Q6_6)&!input$Q6_6Other)|(is_empty(input$Q6_7)&!input$Q6_7Other)|(is_empty(input$Q6_8)&!input$Q6_8Other)|is_empty(input$Q6_13TreDm)|is_empty(input$Q6_14SelTrS)|is_empty(input$Q6_16CanGl)|is_empty(input$Q6_18PubTr)|is_empty(input$Q6_19StrTr)|is_empty(input$Q6_19PrkTr)|is_empty(input$Q6_19MuniTr)|is_empty(input$Q6_20StrTr)|is_empty(input$Q6_20PrkTr)|is_empty(input$Q6_20MuniTr)|is_empty(input$Q6_21ValPubTr)|is_empty(input$Q6_22FinRec)|is_empty(input$Q5_4)){
      shinyalert(failure,missings,type="warning")
    }
    else{
      newtab <- switch(input$tabs, "s6" = "s7","s7" = "s6")
      updateTabItems(session, "tabs", newtab)
    }
  })
  
  #sec7 submit button
  observeEvent(input$sub7,{
    req(input$Q7_2SysSch)
    req(input$Q7_2ReaDem)
    req(input$Q7_1TrPlnt)
    req(input$Q7_1TrRem)
    if((input$Q7_2SysSch+input$Q7_2ReaDem)!=100){
      shinyalert(tryAgain,not100,type="error")
    }
    else if(is_empty(input$Q7_4AppPrun)|is_empty(input$Q7_12TrRskM)|is_empty(input$Q7_14WritTrRsk)|is_empty(input$Q7_10TecAst)|is_empty(input$Q7_10FinAst)|(is_empty(input$Q7_15)&!input$Q7_15Other)|(is_empty(input$Q7_17)&!input$Q7_17Other)){
      shinyalert(failure,missings,type="warning")
    }
    else{
      newtab<-switch(input$tabs, "s7" = "s8","s8" = "s7")
      updateTabItems(session,"tabs",newtab)
    }
  })
  
  #sec8 submit button
  observeEvent(input$sub8,{
    if(is_empty(input$Q8_4AwdCm)|is_empty(input$Q8_5EduPr)|is_empty(input$Q8_8TrCity)|is_empty(input$Q8_10ObvsP)){
      shinyalert(failure,missings,type="warning")
    }
    else{
      newtab<-switch(input$tabs, "s8" = "srez","srez" = "s8")
      updateTabItems(session,"tabs",newtab)
      # shinyalert(success,toNext,type="success")
    }
  })
  
# create the page 2 datatable
  dtWithRadioButton<-reactiveValues(dt = m)
  
  observeEvent(input$btnContinue,{
    showModal(modalDialog(
      size="l",easyClose=TRUE, fade=TRUE,
      DTOutput("datatable"),
      footer=tagList(
        actionBttn(inputId="btnProcess",label="Process",style="float",size="xs",color="success")
      )
    ))
  })
  
  output$datatable = renderDT(
    datatable(dtWithRadioButton$dt, selection = "none", escape=FALSE,
                   options= list(
                     dom = 't',
                     paging = FALSE,
                     ordering = FALSE),
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
  
  observeEvent(input$btnProcess,{
    dt<-dtWithRadioButton$dt
    x<-rep(0,7)
    i=1
    for(resp in text){
      x[i]<-input[[resp]]
      i=i+1
    }
    tableScores<<-x
    shinyalert(success,tblCplt,type="success")
    removeModal(session)
  })
  
  #write population group
  output$popGroup<-renderText({
    paste("Your population group is: ",groupPop(as.numeric(input$pop)),sep="")
  })
  
  #write region
  output$regionText<-renderText({
    paste("Your region is: ", printReg(input$region),sep="")
  })
  
  # output$tableQuestions = DT::renderDataTable(
  #   m, escape = FALSE, selection = 'none', server = FALSE,
  #   options = list(dom = 't', paging = FALSE, ordering = FALSE,autoWidth=FALSE,columnDefs=list(list(width='200px',targets=responses[1:4]))))
}

shinyApp(ui=ui,server=server) 