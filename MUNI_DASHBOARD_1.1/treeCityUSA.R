rm(list=ls())
library(shiny)
library(shinydashboard)
library(flexdashboard)

body<-dashboardBody(
  tabItems(
    tabItem(tabName="question",
            h2("Tree City USA Community Information"),
            #treeboard
            radioButtons(inputId="treeBoard",label="Do you have a treeboard?",c("No","Yes"),inline=TRUE),
            #ordinence
            radioButtons(inputId="ordinence",label="Do you have a tree care ordinence",c("No","Yes"),inline=TRUE),
            #population
            numericInput(inputId="population",label="What is the population of your community?",min=1,max=9000000,step=1,value=1),
            #tree budget
            numericInput(inputId="budget",label="What is your tree budget?", min=0, step=0.01,value=1),
            #arbor day
            radioButtons(inputId="arborDay",label="Does your community have an Arbor Day Celebration?",c("No","Yes"),inline=TRUE)
    ),
    tabItem(tabName="sped",
            h2("Tree City USA Progress"),
            gaugeOutput("gaugeProgress")
    )
  )
)

sidebar<-dashboardSidebar(
  sidebarMenu(
    menuItem("Tree City USA Questionaire",tabName="question",icon=icon("table")),
    menuItem("Tree City USA Progress",tabName="sped",icon=icon("tachometer"))
  )
)

ui<-dashboardPage(
  dashboardHeader(title="Tree City USA"),
  sidebar,
  body
)


server<-function(input,output){
  output$gaugeProgress=renderGauge({
    gauge((if(input$treeBoard == 'Yes') {25} else {0}) +
          (if(input$ordinence == 'Yes') {25} else {0}) +
          (if((input$budget / input$population) >= 2.00) {25} else {((input$budget / input$population) / 2)*25}) +
          (if (input$arborDay == 'Yes') {25} else {0}),
          min=0,
          max=100,
          symbol='%',
          sectors=gaugeSectors(success=c(99.9999,100),
                               danger=c(0,100)))
  })
}

shinyApp(ui=ui,server=server) 
