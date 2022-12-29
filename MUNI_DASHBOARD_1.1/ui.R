UI <- tagList(
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
                                 mainPanel(useShinyalert(force = TRUE),
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
                                           htmlOutput("totalTrees"),
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
                                           htmlOutput("totalPlantingLoc"),
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
                                 downloadButton("indicatorReport","Generate Report")
                        ),
                        tabPanel("MyTreeScore Indexes Summary",
                                 htmlOutput("myTSIndeHeader"),
                                 hr(),
                                 downloadButton("indexReport","Generate Report"))
             )
  )
)


