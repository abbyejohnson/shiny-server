rm(list=ls())
list.of.packages<-c("devtools","shiny","shinydashboard","flexdashboard","dplyr","zipcode","shinyBS","shinyalert")
new.packages<-list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(devtools)
if(!("dashboardthemes" %in% installed.packages()[,"Package"])) install_github("nik01010/dashboardthemes")
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(dplyr)
library(zipcode)
library(tidyverse)
library(shinyBS)
library(shinyalert)
library(dashboardthemes)
library(geosphere)
data(zipcode)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#globally defined variables
w=700

#Page Title
ptitle="Municipal Tree Survey"

#tab Names
dmgr="Demographics"
treeCity="Tree City USA"
CMMod="Clark & Matheny Model"
vegresor="Vegetation Resources"
cframe="Community Framework"
rmngmt="Resource Management"
results="Results"

#Questions and Headers for survey
demoHeader="Please fill out the following demographics about your community"
askZip="What is your 5 digit ZIP code? (Please fill this out first to help us find your city)"
askCity="Select your city:"
askPop="What is the population of your community?"
TCUSAHeader="Tree City USA Community Information"
TC1="1. Do you have a treeboard?"
TC2="2. Do you have a tree care ordinance?"
TC3="3. What is your tree budget?"
TC4="4. Does your community have an Arbor Day Celebration?"
vrHeader="Criteria and Performance Indicators for Vegetation Resources"
vrSubHeader="For each of the following, please choose the option that best describes your community's method for assessing each area."
vr1="Canopy Cover:"
vr2="Age-distribution of trees in the community:"
vr3="Species mix:"
vr4="Native vegetation:"
cfHeader="Criteria and Performance Indicators for Community Framework"
cfSubHeader="For each of the following, please choose the option that best describes your community."
cf1="Public agency cooperation"
cf2="Involvement of large private and institutional land holders:"
cf3="Green industry cooperation:"
cf4="Neighborhood action:"
cf5="Citizen-governemnt-business interaction:"
cf6="General awareness of trees as community resources:"
cf7="Regional cooperation:"
rmHeader="Criteria and Performance Indicators for Resource Management"
rmSubHeader="For each of the following, please choose the option that best describes your community"
rm1="City-wide management plan:"
rm2="City-wide funding:"
rm3="City staffing"
rm4="Assessment tools"
rm5="Protection of existing trees"
rm6="Species and site selection"
rm7="Standards for tree care"
rm8="Citizen safety"
rm9="Recycling"

#answer text
vr1l="No assessment"
vr1m="Visual assessment(i.e. photographic)"
vr1g="Sampling of tree cover using aerial photographs"
vr1o="Information on urban forests included in city-wide geographic information system(GIS)"
vr2l="No assessment"
vr2m="Street tree inventory(complete or sample)"
vr2g="Public-private sampling"
vr2o="Included in city-wide geographic information system(GIS)"
vr3l="No assessment"
vr3m="Street tree inventory"
vr3g="City-wide assessment of species mix"
vr3o="Included in city-wide geographic information system(GIS)"
vr4l="No program of integration"
vr4m="Voluntary use on public projects"
vr4g="Requirements for use of native species on a project-appropriate basis"
vr4o="Preservation of regional biodiversity"
cf1l="Conflicting goals among departments"
cf1m="No cooperation"
cf1g="Informal working teams"
cf1o="Formal working teams with staff cooperation"
cf2l="Ignorance of issues"
cf2m="Education materials and advice available to land-holders"
cf2g="Clear goals for tree resources by private land-holders; incentives for preservation of private trees"
cf2o="Land-holders develop comprehensive tree management plans(including funding)"
cf3l="No cooperation among segments of industry(nursery, contractor, arborist). No adherence to industry standards"
cf3m="General cooperation among nurseries-contractors-arborists, etc."
cf3g="Specific cooperation arrangements such as purchase certificates for right trees, right place"
cf3o="Shared vision and goals including the use of professional standards"
cf4l="No action"
cf4m="Isolated and/or limited number of active groups"
cf4g="City-wide coverage and interaction"
cf4o="All neighborhoods organized and cooperating"
cf5l="Conflicting goals among constituencies"
cf5m="No interaction among constituencies"
cf5g="Informal and/or general cooperation"
cf5o="Formal interaction, e.g., tree board with staff coordination"
cf6l="Low-trees as problems; a drain on budgets"
cf6m="Moderate-trees as important to community"
cf6g="High-trees acknowleged to provide environmental services"
cf6o="Very high-trees as vital components of economy and environment"
cf7l="Communities operate independently"
cf7m="Communities share similar policy vehicles"
cf7g="Regional planning"
cf7o="Regional planning coordination and/or management plans"
rm1l="No plan"
rm1m="Existing plan limited in scope and implementation"
rm1g="Government-wide plan,accepted and implemented"
rm1o="Citizen-government-business resource management plan, accepted and implemented"
rm2l="Funding by crisis management"
rm2m="Funding to optimize existing population"
rm2g="Adequate funding to provide for net increase in population and care"
rm2o="Adequate funding, private and public, to sustain maximum potential benefits"
rm3l="No staff"
rm3m="No training"
rm3g="Certified arborists on staff"
rm3o="Professional tree care staff"
rm4l="No on-going program of assessment"
rm4m="Partial inventory"
rm4g="Complete inventory"
rm4o="Information on urban forests included in city-wide GIS"
rm5l="No policy vehicle or policy not enforced"
rm5m="Tree preservation ordinance present and enforced"
rm5g="Tree preservation plan required for all projects...public, private, commercial, residential"
rm5o="Integrated planning program for conservation and development"
rm6l="Arbitrary species prohibitions"
rm6m="No consideration of undesirable species"
rm6g="Identification/prohibition of undesirable species"
rm6o="On-going use of adapted high-performing species with good site-species match"
rm7l="None"
rm7m="Standards for public tree care"
rm7g="Standards for pruning, stock, etc. for all trees"
rm7o="Standards pard of community-wide vision"
rm8l="Crisis management"
rm8m="Informal inspections"
rm8g="Comprehensive hazard(failure,tripping, etc.) program"
rm8o="Safety part of cost-benefit program"
rm9l="Simple disposal(i.e. land filling) of green waste"
rm9m="Green waste recycling"
rm9g="Green and wood waste recycling-reuse"
rm9o="Closed system-no outside disposal"

#results tab text
TCResults="Tree City USA Progress"
TCtoDo="Do these to achieve 'Tree City USA' Status:"
CMSText="Clark & Matheny Score"
CMLow="These are areas your community scored low in:"
CMSS="Clark & Matheny Sub-Scores:"
tbRecc="Create a tree board."
ordRecc="Create an ordinance."
adRecc="Declare an Arbor Day Celebration."
Comp="Here is how communities near you score:"
atsMsg="Average total score: "
avrsMsg="Average Vegetation Resource Score in your area: "
armsMsg="Average Resource Mangement Score in your area: "
acfsMsg="Average Community Framework Score in your area: "

#End of page submission pop ups
highPop="Your populaiton entered is more than 10% greater than our records"
lowPop="Your population entered is more than 10% less than out records"
success="Submitted!"
failure="Incomplete!"
nozip="You have not entered a ZIP Code!  Please enter a 5 digit ZIP Code and re-submit."
nopop="You have not entered a population for your community!  Please enter a population and re-submit."
plzcomplete="You have not answered all the questions in this section.  You must answer all questions before moving on."
toTC="Please continue to the 'Tree City USA' tab."
toVR="Please continue to the 'Vegetation Resources' tab."
toCF="Please continue to the 'Community Framework' tab."
toRM="Please continue to the 'Resource Management' tab."
toRes="Please continue to the 'Results' tab."
VRScoreMsg="Your Vegetation Resources Score is: "
CFScoreMsg="Your Community Framework Score is: "
RMScoreMsg="Your Resource Management Score is: "
lowVR="Vegetation Resources: To improve in this area, try increasing canopy cover, age distribution, species mix, or native vegetation."
lowCF="Community Framework: To improve in this area, try to improve regional, business, citizen and neighborhood cooperations."
lowRM="Resource Management: To improve in this area, try introducing a city plan or budget, increase staff training/education, protecting existing trees, carefully selecting species, and other areas investigated under the 'Resource Mangement' tab."

#info Button stuff
ZIP="ZIP Code"
zipInfo="Use the 5 digit ZIP code of your community as given by the US Post Office"
pop="Population"
popInfo="Use the most recent estimate of the number of people living in your community"
tb="Tree Board"
tbinfo="A 'Tree Board' would consist of a group of professional foresters, arborists, city department, citizens or a combination that is responsible for tree care decisions."
ordn="Ordinance"
ordInfo="An ordinance assigns public tree care responsibilities and ideally would provide guidence for planting, maintaining and removing public trees."
tbud="Tree Budget"
budInfo="A tree budget is the dollar amount set toward planing, care for, removal and planning of public trees."
adc="Arbor Day Celebration"
adInfo="An Arbor Day Celebration is a declared annual ceremony for citizens to celebrate the work of planting and care for trees.  It can be a week-long or single day celebration."
cancov="Canopy Coverage"
ccko="Achieve climate-appropriate degree of tree cover, community-wide."
agedist="Age-distribution of trees"
adko="Provide for uneven age distribution."
spmix="Species mix"
smko="Provide for species diversity."
naveg="Native vegetation"
nvko="Preserve and manage regional biodiversity.  Maintain the biological integrity of native remant forests.  Maintain wildlife corridors to and from the city."
pacoop="Public agency cooperation"
packo="Insure all city departments operate with common goals and objectives."
lcorpinv="Involvement of large private and institutional land holders"
ilko="Large private landholders embrace city-wide goals and objectives through species resource management plans."
greenind="Green industry cooperation"
giko="The green industry operates with high professional standards and commits to city-wide goals and objectives."
nbhact="Neighborhood action"
nako="At the neighborhood level, citizens understand and participate in urban forest management."
cgovint="Citizen-governemnt-business interaction"
cgko="All constituencies in the community interact for the benefit of the urban forest."
genaware="General awareness fo trees as community resource"
gatcko="The general public understands the value of trees to the community."
rcoop="Regional Cooperation"
rcko="Provide for cooperation and interaction among neighboring communities and regional groups."
cwmp="City-wide management plan"
mpko="Develop and implement a management plan for trees and forests on public and private property."
cwf="City-wide funding"
cfko="Develop and maintain adequate funding to implement a city-wide management plan."
cs="City staffing"
csko="Employ and train adequate staff to implement city-wide management plan."
at="Assessment tools"
atko="Develop methods to collect information about the urban forest on a routine basis."
poet="Protection of existing trees"
ptko="Conserve existing resources, planted and natural, to ensure maximum function."
sss="Species and site selection"
sssko="Provide guidelines and specifications for species use, including a mechanism for evaluating the site."
stc="Standards for tree care"
stcko="Adopt and adhere to professional standards for tree care."
cs="Citizen safety"
csko="Maximize public safety with respect to trees."
recycle="Recycling"
rko="Create a closed system for tree waste."

#general responses/button text
n="No"
y="Yes"
infoButton="Additional Information"
ko="Key Objective"
submit="Submit"
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#end of globally defined variables

#read in data
fips<-read_csv("//uwsp.edu/files/CNR/Projects/MuniDashboard/fips.csv")
myzips<-read_csv("//uwsp.edu/files/CNR/Projects/MuniDashboard/zipcodes.csv")

#helper functions
getCities<-function(zip){
  rownames(zipcode)=zipcode$zip
  index<-which(zip==zipcode[,1])
  foundCity<-zipcode$city[index]
  return(foundCity)
}

getfips<-function(zip,city){
  rownames(zipcode)=zipcode$zip
  i<-which(zip==zipcode[,1])
  state<-zipcode$state[i]
  fi<-which(city==fips[,2]&state==fips[,1])
  return(fips$FIPS_Code[fi])
}

getpop<-function(zip,city){
  rownames(zipcode)=zipcode$zip
  state<-zipcode[zip,3]
  i<-which(state==fips[,1] & city==fips[,2])
  return(as.numeric(as.character((fips$Population[i]))))
}

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

sumcomfram<-function(pac,illh,gic,nbhda,cgbi,gat,regcop){
  (if(pac == cf1l) {1}
   else if(pac == cf1m) {2}
   else if(pac == cf1g) {3}
   else if(pac == cf1o) {4}) +
    (if(illh == cf2l) {1}
     else if(illh == cf2m) {2}
     else if(illh == cf2g) {3}
     else if(illh == cf2o) {4}) +
    (if(gic == cf3l) {1}
     else if(gic == cf3m) {2}
     else if(gic == cf3g) {3}
     else if(gic == cf3o) {4}) +
    (if(nbhda == cf4l) {1}
     else if(nbhda == cf4m) {2}
     else if(nbhda == cf4g) {3}
     else if(nbhda == cf4o) {4}) +
    (if(cgbi == cf5l) {1}
     else if(cgbi == cf5m) {2}
     else if(cgbi == cf5g) {3}
     else if(cgbi == cf5o) {4}) +
    (if(gat == cf6l) {1}
     else if(gat == cf6m) {2}
     else if(gat == cf6g) {3}
     else if(gat == cf6o) {4}) +
    (if(regcop == cf7l) {1}
     else if(regcop == cf7m) {2}
     else if(regcop == cf7g) {3}
     else if(regcop == cf7o) {4})
}

sumrezman<-function(ctypln,ctyfnd,ctystf,asmtt,pet,sass,sftc,csfty,rec){
  (if(ctypln == rm1l) {1}
   else if(ctypln == rm1m) {2}
   else if(ctypln == rm1g) {3}
   else if(ctypln == rm1o) {4}) +
    (if(ctyfnd == rm2l) {1}
     else if(ctyfnd == rm2m) {2}
     else if(ctyfnd == rm2g) {3}
     else if(ctyfnd == rm2o) {4}) +
    (if(ctystf == rm3l) {1}
     else if(ctystf == rm3m) {2}
     else if(ctystf == rm3g) {3}
     else if(ctystf == rm3o) {4}) +
    (if(asmtt == rm4l) {1}
     else if(asmtt == rm4m) {2}
     else if(asmtt == rm4g) {3}
     else if(asmtt == rm4o) {4}) +
    (if(pet == rm5l) {1}
     else if(pet == rm5m) {2}
     else if(pet == rm5g) {3}
     else if(pet == rm5o) {4}) +
    (if(sass == rm6l) {1}
     else if(sass == rm6m) {2}
     else if(sass == rm6g) {3}
     else if(sass == rm6o) {4}) +
    (if(sftc == rm7l) {1}
     else if(sftc == rm7m) {2}
     else if(sftc == rm7g) {3}
     else if(sftc == rm7o) {4}) +
    (if(csfty == rm8l) {1}
     else if(csfty == rm8m) {2}
     else if(csfty == rm8g) {3}
     else if(csfty == rm8o) {4}) +
    (if(rec == rm9l) {1}
     else if(rec == rm9m) {2}
     else if(rec == rm9g) {3}
     else if(rec == rm9o) {4})
}

body<-dashboardBody(
  shinyDashboardThemes("onenote"),
  tabItems(
    tabItem(tabName="demo",
            useShinyalert(),
            h2(demoHeader),
            #ZIP Code
            textInput("ZIP",
                      askZip),
            actionLink("zipCode",infoButton),
            br(),
            br(),
            bsModal("zipDef",ZIP,"zipCode",
                    p(zipInfo)),
            #city drop down
            selectInput("city",
                        askCity,
                        choices = NULL),
            #population
            textInput("pop",
                      askPop,
                       value=character(0)),
            actionLink("popInf",infoButton),
            br(),
            br(),
            actionButton("subdemo",submit),
            bsModal("popDef",pop,"popInf",
                    p(popInfo))
    ),
    tabItem(tabName="t1",
            h2(TCUSAHeader),
            #treeboard
            radioButtons("tb",
                         TC1,
                         c(n,y),
                         selected=character(0)),
            actionLink("treeboard",infoButton),
            br(),
            br(),
            bsModal("tbDef",tb,"treeboard",
                    p(tbinfo)),
            #ordinance
            radioButtons("ord",
                         TC2,
                         c(n,y),
                         selected=character(0)),
            actionLink("ordex",infoButton),
            br(),
            br(),
            bsModal("ordDef",ordn,"ordex",
                    p(ordInfo)),
            #tree budget
            numericInput("bud",
                         TC3, 
                         min=0, step=0.01,value=character(0)),
            actionLink("budex",infoButton),
            br(),
            br(),
            bsModal("budDef",tbud,"budex",
                    p(budInfo)),
            #arbor day
            radioButtons("aday",
                         TC4,
                         c(n,y),
                         selected=character(0)),
            actionLink("adex",infoButton),
            br(),
            br(),
            bsModal("adDef",adc,"adex",
                    p(adInfo)),
            actionButton("subt1",submit)
    ),
    tabItem(tabName="t2",
            h2(vrHeader),
            h4(vrSubHeader),
            #fluidRow(column(2,h4("Objective")),
            #         column(2,h4("Low")),
            #         column(2,h4("Moderate")),
            #         column(2,h4("Good")),
            #         column(2,h4("Optimal")),
            #         column(2,h4("Key Objective"))),
            #canopy cover
            #tags$style(HTML(".radio-inline {margin-right: 75px;}")),
            #fluidRow(column(2,vr1),
                     # column(8,
            radioButtons("cc", 
                         vr1,
                         c(vr1l,vr1m,vr1g,vr1o),
                         selected=character(0),width=w),
            actionLink("ccex",ko),
            br(),
            br(),
            bsModal("ccDef",cancov,"ccex",
                    p(ccko)),
            #age of tree assessment
            radioButtons("ageDist",
                         vr2,
                         c(vr2l,vr2m,vr2g,vr2o),
                         selected=character(0),width=w),
            actionLink("agex",ko),
            br(),
            br(),
            bsModal("agDef",agedist,"agex",
                    p(adko)),
            #species mix
            radioButtons("smix",
                         vr3,
                         c(vr3l,vr3m,vr3g,vr3o),
                         selected=character(0),width=w),
            actionLink("smixex",ko),
            br(),
            br(),
            bsModal("smDef",spmix,"smixex",
                    p(smko)),
            #native vegitation
            radioButtons("nveg",
                         vr4,
                         c(vr4l,vr4m,vr4g,vr4o),
                         selected=character(0),width=w),
            actionLink("nvegex",ko),
            br(),
            br(),
            bsModal("nvDef",naveg,"nvegex",
                    p(nvko)),
            actionButton("subt2",submit)
    ),
    tabItem(tabName="t3",
            h2(cfHeader),
            h4(cfSubHeader),
            radioButtons("pac",
                         cf1,
                         c(cf1l,cf1m,cf1g,cf1o),
                         selected=character(0),width=w),
            actionLink("pacex",ko),
            br(),
            br(),
            bsModal("pacDef",pacoop,"pacex",
                    p(packo)),
            radioButtons("illh",
                         cf2,
                         c(cf2l,cf2m,cf2g,cf2o),
                         selected=character(0),width=w),
            actionLink("illhex",ko),
            br(),
            br(),
            bsModal("illhDef",lcorpinv,"illhex",
                    p(ilko)),
            radioButtons("gic",
                         cf3,
                         c(cf3l,cf3m,cf3g,cf3o),
                         selected=character(0),width=w),
            actionLink("gicex",ko),
            br(),
            br(),
            bsModal("gicDef",greenind,"gicex",
                    p(giko)),
            radioButtons("nbhda",
                         cf4,
                         c(cf4l,cf4m,cf4g,cf4o),
                         selected=character(0),width=w),
            actionLink("nbhdaex",ko),
            br(),
            br(),
            bsModal("nbhdaDef",nbhact,"nbhdaex",
                    p(nako)),
            radioButtons("cgbi",
                         cf5,
                         c(cf5l,cf5m,cf5g,cf5o),
                         selected=character(0),width=w),
            actionLink("cgbiex",ko),
            br(),
            br(),
            bsModal("cgbiDef",cgovint,"cgbiex",
                    p(cgko)),
            radioButtons("gat",
                         cf6,
                         c(cf6l,cf6m,cf6g,cf6o),
                         selected=character(0),width=w),
            actionLink("gatex",ko),
            br(),
            br(),
            bsModal("gatDef",genaware,"gatex",
                    p(gatcko)),
            radioButtons("regcop",
                         cf7,
                         c(cf7l,cf7m,cf7g,cf7o),
                         selected=character(0),width=w),
            actionLink("regcopex",ko),
            br(),
            br(),
            bsModal("regcopDef",rcoop,"regcopex",
                    p(rcko)),
            actionButton("subt3",submit)
    ),
    tabItem(tabName="t4",
            h2(rmHeader),
            h4(rmSubHeader),
            radioButtons("ctypln",
                         rm1,
                         c(rm1l,rm1m,rm1g,rm1o),
                         selected=character(0),width=w),
            actionLink("ctyplnex",ko),
            br(),
            br(),
            bsModal("cpDef",cwmp,"ctyplnex",
                    p(mpko)),
            radioButtons("ctyfnd",
                         rm2,
                         c(rm2l,rm2m,rm2g,rm2o),
                         selected=character(0),width=w),
            actionLink("ctyfndex",ko),
            br(),
            br(),
            bsModal("cfDef",cwf,"ctyfndex",
                    p(cfko)),
            radioButtons("ctystf",
                         rm3,
                         c(rm3l,rm3m,rm3g,rm3o),
                         selected=character(0),width=w),
            actionLink("ctystfex",ko),
            br(),
            br(),
            bsModal("csDef",cs,"ctystfex",
                    p(csko)),
            radioButtons("asmtt",
                         rm4,
                         c(rm4l,rm4m,rm4g,rm4o),
                         selected=character(0),width=w),
            actionLink("asmttex",ko),
            br(),
            br(),
            bsModal("atDef",at,"asmttex",
                    p(atko)),
            radioButtons("pet",
                         rm5,
                         c(rm5l,rm5m,rm5g,rm5o),
                         selected=character(0),width=w),
            actionLink("petex",ko),
            br(),
            br(),
            bsModal("petDef",poet,"petex",
                    p(ptko)),
            radioButtons("sass",
                         rm6,
                         c(rm6l,rm6m,rm6g,rm6o),
                         selected=character(0),width=w),
            actionLink("sssi",ko),
            br(),
            br(),
            bsModal("sssDef",sss,"sssi",
                    p(sssko)),
            radioButtons("sftc",
                         rm7,
                         c(rm7l,rm7m,rm7g,rm7o),
                         selected=character(0),width=w),
            actionLink("sftcex",ko),
            br(),
            br(),
            bsModal("sftcDef",stc,"sftcex",
                    p(stcko)),
            radioButtons("csfty",
                         rm8,
                         c(rm8l,rm8m,rm8g,rm8o),
                         selected=character(0),width=w),
            actionLink("csftyex",ko),
            br(),
            br(),
            bsModal("csftyDef",cs,"csftyex",
                    p(csko)),
            radioButtons("rec",
                         rm9,
                         c(rm9l,rm9m,rm9g,rm9o),
                         selected=character(0),width=w),
            actionLink("recex",ko),
            br(),
            br(),
            bsModal("recDef",recycle,"recex",
                    p(rko)),
            actionButton("subt4",submit)
    ),
    tabItem(tabName="results",
            #Tree City USA results
            h2(TCResults,align="center"),
            fluidRow(
              column(8,
                     gaugeOutput("TCUSAGauge")),
              column(4,
                     h4(TCtoDo),
                     textOutput("maketb"),
                     textOutput("makeord"),
                     textOutput("incbud"),
                     textOutput("decad"))),
            br(),
            #Clarck & Matheny Score
            h2(CMSText,align="center"),
            fluidRow(
              column(8,
                     gaugeOutput("CMSGauge"),
                     h4(CMLow),
                     textOutput("vegres"),
                     textOutput("comfram"),
                     textOutput("rezman")),
              column(4,
                     h4(CMSS),
                     textOutput("vrScore"),
                     textOutput("cfScore"),
                     textOutput("rmScore"))),
            fluidRow(12,
                     h4(Comp),
                     textOutput("ats"),
                     textOutput("avrs"),
                     textOutput("arms"),
                     textOutput("acfs"))
    )
  )
)

sidebar<-dashboardSidebar(
  sidebarMenu(
    menuItem(dmgr,tabName="demo",icon=icon("user-edit")),
    menuItem(treeCity,tabName="t1",icon=icon("tree")),
    menuItem(CMMod, icon=icon("poll"),
             menuSubItem(vegresor,tabName="t2",icon=icon("leaf")),
             menuSubItem(cframe,tabName="t3",icon=icon("city")),
             menuSubItem(rmngmt,tabName="t4",icon=icon("tasks"))),
    menuItem(results,tabName="results",icon=icon("chalkboard-teacher"))
  )
)

ui<-dashboardPage(
  # dashboardHeader(title=shinyDashboardLogo(theme="blue_gradient")),
  dashboardHeader(title=ptitle),
  sidebar,
  body
)


server<-function(session,input,output){
  observe({
    x<-getCities(input$ZIP)
    updateSelectInput(session,"city",askCity,choices=unique(x))
  })
  
  observe({
    areapop<-getpop(input$ZIP,input$city)
    updateNumericInput(session,"pop",askPop,
                       value=unique(areapop),min=1,max=9000000,step=1)
  })
  
  observeEvent(input$subdemo,{
    if(identical(input$ZIP,character(0))){
      shinyalert(failure,nozip,type="error")
    }
    else if(identical(input$pop,character(0))){
      shinyalert(failure,nopop,type="error")
    }
    else if(length(getpop(input$ZIP,input$city))==0){
      shinyalert(success,toTC,type="success")
    }
    else if(input$pop >= 1.1*unique(getpop(input$ZIP,input$city))){
      shinyalert(highPop,paste("Our records show a population of",unique(getpop(input$ZIP,input$city))," for ",input$city,".  Please check your entry.  If it is correct, proceed.  If it is not correct, re-enter population and re-submit.", sep=""),type="warning")
    }
    else if(input$pop <= 0.9*unique(getpop(input$ZIP,input$city))){
      shinyalert(lowPop,paste("Our records show a population of ",unique(getpop(input$ZIP,input$city))," for ",input$city,".  Please check your entry.  If it is correct, proceed.  If it is not correct, re-enter population and re-submit.", sep=""),type="warning")
    }
    else{
      shinyalert(success,toTC, type="success")
    }
  })
  
  observeEvent(input$subt1,{
    if(is_empty(input$tb) | is_empty(input$ord) | is.na(input$bud) | is_empty(input$aday)){
      shinyalert(failure,plzcomplete,type="error")
    }
    else{
      shinyalert(success,toVR,type="success")
    }
  })
  
  observeEvent(input$subt2,{
    if(is_empty(input$cc) | is_empty(input$ageDist) | is_empty(input$smix) | is_empty(input$nveg)){
      shinyalert(failure,plzcomplete,type="error")
    }
    else{
      shinyalert(success,toCF,type="success")
    }
  })
  
  observeEvent(input$subt3,{
    if(is_empty(input$pac) | is_empty(input$illh) | is_empty(input$gic) | is_empty(input$nbhda) | is_empty(input$cgbi) | is_empty(input$gat) | is_empty(input$regcop)){
      shinyalert(failure,plzcomplete,type="error")
    }
    else{
      shinyalert(success,toRM,type="success")
    }
  })
  
  observeEvent(input$subt4,{
    if(is_empty(input$ctypln) | is_empty(input$ctyfnd) | is_empty(input$ctystf) | is_empty(input$asmtt) | is_empty(input$pet) | is_empty(input$sass) | is_empty(input$csfty) | is_empty(input$rec) | is_empty(input$sftc)){
      shinyalert(failure,plzcomplete,type="error")
    }
    else{
      shinyalert(success,toRes,type="success")
    }
  })
  
  output$TCUSAGauge=renderGauge({
    gauge((if(input$tb == y) {25} else {0}) +
          (if(input$ord == y) {25} else {0}) +
          (if((input$bud / as.numeric(input$pop)) >= 2.00) {25} else {0}) +
          (if (input$aday == y) {25} else {0}),
          min=0,
          max=100,
          symbol='%',
          sectors=gaugeSectors(success=c(76,100),
                               warning=c(49,75),
                               danger=c(0,48)))
  })
  
  output$maketb=renderText({
          if(input$tb == n) {tbRecc} else {}
  })
  
  output$makeord=renderText({
    if(input$ord == n) {ordRecc} else {}
  })
  
  output$incbud=renderText({
    if((input$bud / as.numeric(input$pop)) < 2.00) {paste("Increase per capita tree budget to exceed $2/capita.  You are at $",format(round((input$bud/as.numeric(input$pop)),2)),"/capita.",sep="")} else {}
  })
  
  output$decad=renderText({
    if(input$aday == n) {adRecc} else {}
  })
  
  output$CMSGauge=renderGauge({
    gauge(sumvegres(input$cc,input$ageDist,input$smix,input$nveg) +
          sumcomfram(input$pac,input$illh,input$gic,input$nbhda,input$cgbi,input$gat,input$regcop) +
          sumrezman(input$ctypln,input$ctyfnd,input$ctystf,input$asmtt,input$pet,input$sass,input$sftc,input$csfty,input$rec),
          min=20,max=80,
           sectors=gaugeSectors(success=c(61,80),
                               warning=c(41,60),
                               danger=c(20,40)))
  })
  
  #vegetation resources score: vrScore
  output$vrScore=renderText({
    paste(VRScoreMsg, as.character(sumvegres(input$cc,input$ageDist,input$smix,input$nveg)),sep="")
  })
  #community framework score: cfScore
  output$cfScore=renderText({
    paste(CFScoreMsg,as.character(sumcomfram(input$pac,input$illh,input$gic,input$nbhda,input$cgbi,input$gat,input$regcop)),sep="")
  })
  #resource management score: rmScore
  output$rmScore=renderText({
    paste(RMScoreMsg,as.character(sumrezman(input$ctypln,input$ctyfnd,input$ctystf,input$asmtt,input$pet,input$sass,input$sftc,input$csfty,input$rec)),sep="")
  })

  #low areas here
  #vegetation resources low score: vegreg
  output$vegres=renderText({
    if(sumvegres(input$cc,input$ageDist,input$smix,input$nveg) <= 10){
      lowVR
    }
  })
  #community framework low score: comfram
  output$comfram=renderText({
    if(sumcomfram(input$pac,input$illh,input$gic,input$nbhda,input$cgbi,input$gat,input$regcop) <= 18){
      lowCF
    }
  })  
  #resource management low score: rezman
  output$rezman=renderText({
    if(sumrezman(input$ctypln,input$ctyfnd,input$ctystf,input$asmtt,input$pet,input$sass,input$sftc,input$csfty,input$rec) <= 23){
      lowRM
    }
  })
  
  #comparison to surrounding cities
  #average total score near citiies
  output$ats=renderText({
    paste(atsMsg,as.character(stats[1]))
  })
  #average veg resource score near city
  output$avrs=renderText({
    paste(avrsMsg,as.character(stats[2]))
  })
  #average res man score near city
  output$arms=renderText({
    paste(armsMsg,as.character(stats[3]))
  })
  #average comm fram score near city
  output$acfs=renderText({
    paste(acfsMsg,as.character(stats[4]))
  })
}

shinyApp(ui=ui,server=server) 
