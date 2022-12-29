rm(list = ls())

library(shiny)
library(shinydashboard)
library(flexdashboard)
library(shinyBS)
#library(shinyjs) <- for reactive elements e.g. hiding a panel until the user selects a certain option
library(tidyverse)
library(readxl)
#library(ggplot2)
#library(dplyr)
library(gridExtra)


#Read in Data
dataDir <- "//uwsp.edu/files/CNR/Projects/MuniDashboard/2014 Survey Data/R Export Test/"
data <- read_excel(paste(dataDir, "test.xlsx", sep = ""))

#Color scheme for graphing based on where the cities population falls
myPopulationGroupColor <- c('Your City' = 'red',
                            '2,500 to 4,999' = 'grey',
                            '5,000 to 9,999' = 'grey',
                            '10,000 to 24,999' = 'grey',
                            '25,000 to 49,999' = 'grey',
                            '50,000 to 99,999' = 'green',
                            '100,000 to 249,999' = 'grey',
                            '250,000 to 499,999' = 'grey',
                            '500,000 to 999,999' = 'grey',
                            '1,000,000 +' = 'grey')

myRegionColor <- c('Your City' = 'red',
                   'Midwest' = 'grey',
                   'Northeast' = 'grey',
                   'South' = 'green',
                   'West' = 'grey')

#Figuring out the comparison criteria usageN
#Use backticks ` ` to refer to fields that start with a number

#Clean data and Group By
singleRemoveAndGroup <- function(dataVar, groupingVar, orderingVar) {
  
  cleanData <- data %>% drop_na(!!sym(dataVar)) %>% group_by(!!sym(groupingVar), !!sym(orderingVar)) %>% summarise(SummaryValue = mean(!!sym(dataVar)))
  
  return(cleanData)
}

#Plotting
plotCleanedDataBar <- function(cleanData, xVal, yVal, title = NULL, yLabel = yVal, color) {
  
  plottedData <- ggplot(data = cleanData, aes(x = !!sym(xVal), y = !!sym(yVal), fill = !!sym(xVal))) + 
    ggtitle(title) + 
    ylab(yLabel) + 
    geom_bar(stat='identity') + 
    geom_text(aes(label = sprintf("%0.2f", round(SummaryValue, digits = 2))), vjust = 0, color = "black", size = 3.5) +
    scale_y_continuous() +
    scale_fill_manual(values = color) +
    geom_hline(yintercept = 0, color = "black") +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), 
          legend.direction = 'horizontal', 
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          #axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))
  
  return(plottedData)
}

##########################################################################################################################################
########################################################VISUALIZING INDICATORS############################################################

#Average $ spent per public tree by Population Group
cleanedData <- singleRemoveAndGroup('dollarsPerPublicTree', 'PopulationGroup', 'PopulationGroupOrder')

cleanedData <- bind_rows(cleanedData, tibble(PopulationGroup = 'Your City',PopulationGroupOrder =  0,SummaryValue =  .25))
cleanedData$PopulationGroup <- factor(cleanedData$PopulationGroup, as.character(cleanedData$PopulationGroup))
cleanedData$PopulationGroup <- reorder(cleanedData$PopulationGroup, cleanedData$PopulationGroupOrder)

plottedData <- plotCleanedDataBar(cleanedData, 'PopulationGroup', 'SummaryValue', 
                                  'Average Dollars per Public Tree by Population Group', 'Average Dollars per Tree', myPopulationGroupColor)

#Average $ spent per public tree by Region
cleanedData2 <- singleRemoveAndGroup('dollarsPerPublicTree', 'Region', 'RegionOrder')

cleanedData2 <- bind_rows(cleanedData2, tibble(Region = 'Your City',RegionOrder =  1,SummaryValue =  .25))
cleanedData2$Region <- factor(cleanedData2$Region, as.character(cleanedData2$Region))
cleanedData2$Region <- reorder(cleanedData2$Region, cleanedData2$RegionOrder)

plottedData2 <- plotCleanedDataBar(cleanedData2, 'Region', 'SummaryValue', 
                                   'Average Dollars per Public Tree by Region', 'Average Dollars per Tree', myRegionColor)

#Tree Stability by Region
cleanedData3 <- singleRemoveAndGroup('plantedTrStab', 'Region', 'RegionOrder')

cleanedData3 <- bind_rows(cleanedData3, tibble(Region = 'Your City',RegionOrder =  1,SummaryValue =  20))
cleanedData3$Region <- factor(cleanedData3$Region, as.character(cleanedData3$Region))
cleanedData3$Region <- reorder(cleanedData3$Region, cleanedData3$RegionOrder)

plottedData3 <- plotCleanedDataBar(cleanedData3, 'Region', 'SummaryValue', 
                                   'Average Tree Stability by Region', 'Average Tree Stability', myRegionColor)

#$ Per Capita
cleanedData4 <- singleRemoveAndGroup('dollarsPerCapita', 'Region', 'RegionOrder')

cleanedData4 <- bind_rows(cleanedData4, tibble(Region = 'Your City',RegionOrder =  1,SummaryValue =  6))
cleanedData4$Region <- factor(cleanedData4$Region, as.character(cleanedData4$Region))
cleanedData4$Region <- reorder(cleanedData4$Region, cleanedData4$RegionOrder)

plottedData4 <- plotCleanedDataBar(cleanedData4, 'Region', 'SummaryValue', 
                                   'Average Dollars Per Capita by Region', 'Dollars', myRegionColor)

#% of Municipal Budget
cleanedData5 <- singleRemoveAndGroup('percentOfMuniBud', 'Region', 'RegionOrder')

cleanedData5 <- bind_rows(cleanedData5, tibble(Region = 'Your City',RegionOrder =  1,SummaryValue =  .005))
cleanedData5$Region <- factor(cleanedData5$Region, as.character(cleanedData5$Region))
cleanedData5$Region <- reorder(cleanedData5$Region, cleanedData5$RegionOrder)

plottedData5 <- plotCleanedDataBar(cleanedData5, 'Region', 'SummaryValue', 
                                   'Average % of Municipal Budget by Region', '% of Municipal Budget', myRegionColor)

#Budget % Need
cleanedData6 <- singleRemoveAndGroup('budgetNeeds', 'Region', 'RegionOrder')

cleanedData6 <- bind_rows(cleanedData6, tibble(Region = 'Your City',RegionOrder =  1,SummaryValue =  40))
cleanedData6$Region <- factor(cleanedData6$Region, as.character(cleanedData6$Region))
cleanedData6$Region <- reorder(cleanedData6$Region, cleanedData6$RegionOrder)

plottedData6 <- plotCleanedDataBar(cleanedData6, 'Region', 'SummaryValue', 
                                   'Average % Budget Needs by Region', '% Budget Needs', myRegionColor)

#Green Space Area Per Person
cleanedData7 <- singleRemoveAndGroup('greenSpaceArea', 'Region', 'RegionOrder')

cleanedData7 <- bind_rows(cleanedData7, tibble(Region = 'Your City',RegionOrder =  1,SummaryValue =  .01))
cleanedData7$Region <- factor(cleanedData7$Region, as.character(cleanedData7$Region))
cleanedData7$Region <- reorder(cleanedData7$Region, cleanedData7$RegionOrder)

plottedData7 <- plotCleanedDataBar(cleanedData7, 'Region', 'SummaryValue', 
                                   'Average Green Space by Region in Square Meters', 'Square Meters', myRegionColor)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(plottedData3)

#Read in internal Data
dataDir <- "//uwsp.edu/files/CNR/Projects/MuniDashboard/2014 Survey Data/"
studyData <- read_csv(paste(dataDir, "Benchmark TCUSA.csv", sep = ""))

dashboardMuniData <- read_excel(paste(dataDir, "Dashboard2014MuniData.xlsx", sep = ""))

body <- dashboardBody(
  tabItems(
    #Tree City USA Requirements
    tabItem(tabName = "treeCityUSATab", 
            column(8,
                   radioButtons("treeCityUSA", "Is your community currently a Tree City USA?",choices = c("Yes", "No"), selected = character(0)),
                   
                   checkboxGroupInput("treeCityQuestions", "If you are not a Tree City USA community, which of the four standards for Tree City USA recognition do you believe your community meets? Check all that apply",
                                      choices = c("We have a tree board, department or committe responsible for public tree care" = "1",
                                                  "We have a a community tree ordinance or provisions establishig public tree policy" = "2",
                                                  "We have an annual tree budget of $2 per capita which can include community labor and expenditures, grants, volunteer time and some utility line clearance and brush removal" = "3",
                                                  "We have an Arbor Day observance and proclamation" = "4"), 
                                      width = "100%")
            ),
            
            column(4,
                   h3("Tree City USA Progress", align = "center"),
                   gaugeOutput("treeCityGuage", width="100%")
            )
            
    ),
    
    #Wisconsin DNR CARS Reporting
    tabItem(tabName = "cars", 
            column(8,
                   h3('Answer the following questions. Click "Additional Information" to see more about each question.'),
                   #CARS Staff Question
                   radioButtons("carsS",
                                "1. Are your professional staff educated, trained, and experienced in urban forestry, arboriculture, and/or horticulture?",
                                c("Yes","No"),
                                selected=character(0), width = "100%"),
                   
                   actionLink("carsSL", "Additional Information"), 
                   br(),
                   br(),
                   
                   bsModal(id = "carsSModal", title = "Professional Staff Information", trigger = "carsSL", 
                           #Modal Content
                           h3("Intent", align = "center"),
                           p("Professional staff members have education, training, and experience in the fields of urban forestry, arboriculture, 
                       and/or horticulture. These requirements are intended to ensure that the person with the primary responsibility for 
                       program management has the training and experience to properly and professionally manage the urban forest resource 
                       and advance the community's U&CF program."),
                           h3("Definition", align = "center"),
                           p("Individuals who have one or more of the following credentials, and who the community directly employs or retains 
                       through written agreement to advise and/or assist in the development or management of their urban and community 
                       forestry program: 1) a degree in urban forestry or a closely related field (e.g., forestry, horticulture, 
                       arboriculture, etc.), and/or; 2) International Society of Arboriculture Certified Arborist (ISA) or equivalent 
                       professional certification such as a graduate of the Wisconsin Community Tree Management Institute (CTMI). 
                       For Example: "),
                           tags$ol(type = "a",
                                   tags$li("The city arborist or city urban forester who is employed full- or part-time and responsible for the 
                              planting, protection and maintenance of a city's trees and forests."),
                                   tags$li("A public works or parks employee who is an ISA Certified Arborist or CTMI graduate and who supervises 
                              the town's tree crews responsible for the pruning, maintenance, and removal of public trees."),
                                   tags$li("A credentialed, locally-based resource professional that provides urban forestry and arboricultural 
                              consultation, services throughout the year to the community through a written Memorandum of Understanding. 
                              (Note: State U&CF program staff who provide advice to communities does not meet the intent of this section.)"),
                                   tags$li("Any person that is an ISA Certified Arborist, American Society of Consulting Arborists Registered Arborist 
                              or equivalent that is retained to provide urban forestry and arboriculture consultation services by a city
                              or town through written agreement")),
                           h3("Documentation", align = "center"),
                           p(HTML(paste0('ISA certification verification is available online here: ',
                                         a(href = 'https://www.treesaregood.org/findanarborist/verify', 
                                           'https://www.treesaregood.org/findanarborist/verify'), 
                                         ' CTMI graduates can be verified using this spreadsheet. LINK???'))),
                           p("It is the responsibility of each Urban Forestry Coordinator to track professional staff (as defined above) for 
                       communities in their service area as they see fit. One must be able to provide verification of a professional staff 
                       member within 5 business datas of a request.")),
                   
                   #CARS Ordinances Question
                   radioButtons("carsO",
                                "2. Are your ordinances and/or policies codified, followed, and/or routinely enforced?",
                                c("Yes","No"),
                                selected=character(0), width = "100%"),
                   
                   actionLink("carsOL", "Additional Information"), 
                   br(),
                   br(),
                   
                   bsModal(id = "carsOModal", title = "Ordinances/Policies Information", trigger = "carsOL", 
                           #Modal Content
                           h3("Intent", align = "center"),
                           p('Ordinances and/or policies must be codified, be followed and/or routinely enforced by some mechanism within 
                       the community, and guide the community in the proper care, establishment and protection of community trees 
                       and forests. The definition and examples below recognize the fact that effective public policies are not 
                       always contained in a single "Tree Ordinance."'),
                           h3("Definition", align = "center"),
                           p("Status or regulations that direct citizens and local governments in the planting, protection and maintenance
                       of urban and community trees and forests. For Example:"),
                           tags$ol(type = "a",
                                   tags$li('A town "Tree Ordinance" that dictates how trees are to be planted and maintained in the community 
                                     and under what conditions trees can be removed. Depending on the jurisdiction, the ordinance may 
                                     apply to just public trees, or public and private trees.'),
                                   tags$li("A comprehensive set of community regulations and/or policies on tree preservation and landscaping 
                                     that may include sections of the Zoning Ordinance, Code and Public Facilities Manual."),
                                   tags$li('City regulations that contain specific forest management requirements developed to be 
                                     in compliance with a state "Watershed Protection Ordinance." The regulations may establish 
                                     tree and natural areas preservation, buffer requirements, reforestation and building 
                                     restrictions for each watershed in the community.'),
                                   tags$li('A local ordinance established under a state mandate that requires each local jurisdiction 
                                     to adopt tree protection standards and employ a "Tree Warden", or equivalent, with specific 
                                     statutory responsibilities to oversee the planting, protection and maintenance of trees and 
                                     forests in the community')),
                           h3("Documentation", align = "center"),
                           p("A listing of ordinances by community is located here LINK???. It is the responsibility of 
                       each Urban Forestry Coordinator to track ordinances and policies (as defined above) for communities 
                       in their service area as they see fit. One must be able to provide an electronic or paper copy of the ordinance
                       or policy within 5 business days of a request.")),
                   
                   #CARS Advocacy Question
                   radioButtons("carsA",
                                "3. Do you have an advocacy or advisory organization?",
                                c("Yes","No"),
                                selected=character(0), width = "100%"),
                   
                   actionLink("carsAL", "Additional Information"), 
                   br(),
                   br(),
                   
                   bsModal(id = "carsAModal", title = "Advocacy/Advisory Organization Information", trigger = "carsAL", 
                           #Modal Content
                           h3("Intent", align = "center"),
                           p("Many local U&CF programs began through the efforts of local citizen's groups, and these groups 
                       often serve as a catalyst to encourage active local urban forest resource management for the long term.
                       This measure aims to ensure that community residents and program stakeholders are informed, educated, 
                       and engaged in the development and implementation of a sound community forestry program at the local level."),
                           h3("Definition", align = "center"),
                           p("Organizations that are formalized or chartered to advise (organizations established by the local government)
                       or advocate or act (non-governmental organizations active in the community) for the planting, protection 
                       and maintenance of urban and community trees and forests. For example:"),
                           tags$ol(type = "a",
                                   tags$li("A board of citizens appointed by local elected officials, such as tree or park boards, 
                                     to advise policy makers on needed tree ordinances, policies, and management."),
                                   tags$li('A voluntary citizens group such as "City ReLeaf" that is active in advocating 
                                     for tree planting, preservation and management in communities.')),
                           p("Note that if a community is recognized by the Arbor Day Foundation Tree City USA program, it is considered
                       to have met the requirement of having an advocacy/advisory organization."),
                           h3("Documentation", align = "center"),
                           p(HTML(paste0('Communities that have earned Tree City USA status can be found in this spreadsheet LINK??? or online at: ',
                                         a(href = 'https://www.arborday.org/programs/treecityUSA/directory.cfm', 
                                           'https://www.arborday.org/programs/treecityUSA/directory.cfm'))),
                             p("It is the responsibility of each Urban Forestry Coordinator to track advocacy/advisory organizations 
                       (as defined above) for communities in their service area as they see fit. One must be able to provide 
                       verification of the organization within 5 business days of a request."))),
                   
                   #CARS Plan Questions
                   radioButtons("carsP",
                                "4. Do you have a management plan that you use and periodically update?",
                                c("Yes","No"),
                                selected=character(0), width = "100%"),
                   
                   actionLink("carsPL", "Additional Information"), 
                   br(),
                   br(),
                   
                   bsModal(id = "carsPModal", title = "Management Plans Information", trigger = "carsPL", 
                           #Modal Content
                           h3("Intent", align = "center"),
                           p("Possessing, using and periodically updating a management plan demonstrates a community's commitment 
                       to the comprehensive management of its community tree and forest resources."),
                           h3("Definition", align = "center"),
                           p("A detailed document or set of documents that outlines the future management of the community's trees 
                       and forests. The plan must be active (i.e. in use by the community), current (i.e. less than 10 years old, 
                       and reviewed every 5 years) and developed from professionally-based inventories/resource assessments 
                       (i.e. data collected and analyzed by a consultant, trained staff member or trained volunteer/student groups).
                       Examples include: "),
                           tags$ol(type = "a",
                                   tags$li("An Urban Forest Master Plan, based on satellite imagery/GIS or other inventories and 
                                     assessments, that sets goals for tree canopy cover, reccomends areas for reforestation, 
                                     recommends areas for preservation, promotes community education and outreach efforts, and 
                                     recommends tree maintenance policies for town/city/county properties."),
                                   tags$li("A Public Tree Planting and Maintenance Plan, including Pest and Disease Response Plans, 
                                     bassed on an inventory of trees and open spaces in street rights-of-way and parkland. 
                                     These types of plans include information such as a prioritized list of tree pruning and removals, 
                                     a prioritized list of replacement and new tree plantings, a recommended yearly budget,
                                     and a recommended list of tree species for replanting."),
                                   tags$li("A community's Comprehensive Land Use Plan that incorporates specific management 
                                     recommendations for the community's trees and forest resources"),
                                   tags$li("A Hazard Tree Reduction and Replanting Plan or Storm Event Recovery Plan based 
                                     on an inventory of community trees")),
                           h3("Documentation", align = "center"),
                           p("It is the responsibility of each Urban Forestry Coordinator to track the management plan (as defined above)
                       for each community in their service area as they see fit. One must be able to produce an electronic
                       or paper copy of the plan within 5 business days of a request."))
            ),
            
            column(4,
                   h3("CARS Progress", align = "center"),
                   gaugeOutput("carsS", width="100%")
            )
    ),
    
    tabItem(tabName = "testing", 
            h2("Testing loading in external data"),
            selectInput("testIn", "Plotting Variable:", choices=colnames(studyData)), hr(),
            plotOutput("testPlot"),
            
            br(),
            br(),
            
            downloadButton("CARSData", label = "Download"),
            fileInput("file1", "Choose CSV File",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")
            ),
            tableOutput("contents")
    )
  )
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Tree City USA Standards", tabName = "treeCityUSATab", icon = icon("table")),
    menuItem("CARS Reporting",  tabName = "cars",   icon = icon("car-side")),
    menuItem("Testing External Data",   tabName = "testing",  icon = icon("adjust"))
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "Navigation"),
  sidebar,
  body
)

dlmodule <- function(input, output, session, dataFile) {
  #Downloading Data
  downloadData <- downloadHandler(
    filename = function() {
      paste("carsDATA-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(dataFile, file, row.names = FALSE)
    }
  )
  
  return(downloadData)
}

#Graphing Helper Functions
#Clean data and Group By
singleRemoveAndGroup <- function(dataVar, groupingVar, orderingVar) {
  
  cleanData <- data %>% drop_na(!!sym(dataVar)) %>% group_by(!!sym(groupingVar), !!sym(orderingVar)) %>% summarise(SummaryValue = mean(!!sym(dataVar)))
  
  return(cleanData)
}

#Plotting
plotCleanedDataBar <- function(cleanData, xVal, yVal, title = NULL, yLabel = yVal, color) {
  
  plottedData <- ggplot(data = cleanData, aes(y = !!sym(yVal), fill = !!sym(xVal))) + 
    ggtitle(title) + 
    ylab(yLabel) + 
    geom_bar(data = cleanData,aes(x = !!sym(xVal)),stat='identity') + 
    scale_y_continuous() +
    scale_fill_manual(values = color) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  return(plottedData)
}


server <- shinyServer(function(input, output, session) {
  
  #Tree City USA output Guage
  output$treeCityGuage <- renderGauge({
    gauge((if (is.null(input$treeCityUSA))
    {(     if (length(input$treeCityQuestions) == 0) {0}
           else if (length(input$treeCityQuestions) == 1) {25}
           else if (length(input$treeCityQuestions) == 2) {50}
           else if (length(input$treeCityQuestions) == 3) {75}
           else if (length(input$treeCityQuestions) == 4) {100}
           else {0})}
    else if (input$treeCityUSA == "Yes") {100}
    else if (input$treeCityUSA == "No")
    {(     if (length(input$treeCityQuestions) == 0) {0}
           else if (length(input$treeCityQuestions) == 1) {25}
           else if (length(input$treeCityQuestions) == 2) {50}
           else if (length(input$treeCityQuestions) == 3) {75}
           else if (length(input$treeCityQuestions) == 4) {100}
           else {0})}),
    
    min = 0, max = 100, symbol = "%",
    
    gaugeSectors(success = c(76, 100), warning = c(26,75), danger = c(0, 25), colors = c("#008000", "#FFFF00", "#FF0000"))
    )
  })
  
  #CARS Data output Guage
  output$carsGuage <- renderGauge({
    gauge((if (length(input$carsQuestions) == 0) {0}
           else if (length(input$carsQuestions) == 1) {25}
           else if (length(input$carsQuestions) == 2) {50}
           else if (length(input$carsQuestions) == 3) {75}
           else if (length(input$carsQuestions) == 4) {100}
           else {0}),
          
          min = 0, max = 100, symbol = "%",
          
          gaugeSectors(success = c(76, 100), warning = c(26,75), danger = c(0, 25), colors = c("#008000", "#FFFF00", "#FF0000"))
    )
  })
  
  #Creating a Dataframe
  Data <- reactive({
    df <- data.frame(CARSS = (if(is.null(input$carsS)) {-1} 
                              else if(input$carsS == "Yes") {1} 
                              else if(input$carsS == "No")  {0}),
                     CARSO = (if(is.null(input$carsO)) {-1} 
                              else if(input$carsO == "Yes") {1} 
                              else if(input$carsO == "No")  {0}),
                     CARSA = (if(is.null(input$carsA)) {-1} 
                              else if(input$carsA == "Yes") {1} 
                              else if(input$carsA == "No")  {0}),
                     CARSP = (if(is.null(input$carsP)) {-1} 
                              else if(input$carsP == "Yes") {1} 
                              else if(input$carsP == "No")  {0}))
    return(list(df=df))
  })
  
  #Download CSv File
  output$CARSData <- callModule(dlmodule, "CARSData", dataFile = Data()$df)
  
  #Read in CSV Data
  output$contents <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    userData <- read.csv(inFile$datapath)
    
    #Change buttons based on user data
    updateRadioButtons(session, "carsS", selected = (if(userData[1,1] == 1) {"Yes"} else if(userData[1,1] == 0) {"No"} else {NULL}))
    updateRadioButtons(session, "carsO", selected = (if(userData[1,2] == 1) {"Yes"} else if(userData[1,2] == 0) {"No"} else {NULL}))
    updateRadioButtons(session, "carsA", selected = (if(userData[1,3] == 1) {"Yes"} else if(userData[1,3] == 0) {"No"} else {NULL}))
    updateRadioButtons(session, "carsP", selected = (if(userData[1,4] == 1) {"Yes"} else if(userData[1,4] == 0) {"No"} else {NULL}))
    
    #Displaying the user data for testing purposes
    userData[1,1:4]
  })
  
  #Plotting externally loaded data
  output$testPlot <- renderPlot({
    grid.arrange(grid.arrange(arrangeGrob(plottedData3 + theme(legend.position="none"),
                                          plottedData2 + theme(legend.position="none"),
                                          plottedData4 + theme(legend.position="none"),
                                          plottedData6 + theme(legend.position="none"),
                                          nrow=2),
                              mylegend, nrow=2,heights=c(10, 1)))
    
  })
})

shinyApp(ui = ui, server = server)
