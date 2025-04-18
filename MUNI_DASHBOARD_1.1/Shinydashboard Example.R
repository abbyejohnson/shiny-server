library(shinyWidgets)

## Not run: 

if (interactive()) {
  
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  
  # example taken from ?box
  
  ui <- fluidPage(
    tags$h2("Classic shiny"),
    
    # use this in non shinydashboard app
    setBackgroundColor(color = "ghostwhite"),
    useShinydashboard(),
    # -----------------
    
    # infoBoxes
    fluidRow(
      infoBox(
        "Orders", uiOutput("orderNum2"), "Subtitle", icon = icon("credit-card")
      ),
      infoBox(
        "Approval Rating", "60%", icon = icon("line-chart"), color = "green",
        fill = TRUE
      ),
      infoBox(
        "Progress", uiOutput("progress2"), icon = icon("users"), color = "purple"
      )
    ),
    
    # valueBoxes
    fluidRow(
      valueBox(
        uiOutput("orderNum"), "New Orders", icon = icon("credit-card"),
        href = "http://google.com"
      ),
      valueBox(
        tagList("60", tags$sup(style="font-size: 20px", "%")),
        "Approval Rating", icon = icon("line-chart"), color = "green"
      ),
      valueBox(
        htmlOutput("progress"), "Progress", icon = icon("users"), color = "purple"
      )
    ),
    
    # Boxes
    fluidRow(
      box(status = "primary",
          sliderInput("orders", "Orders", min = 1, max = 2000, value = 650),
          selectInput("progress", "Progress",
                      choices = c("0%" = 0, "20%" = 20, "40%" = 40, "60%" = 60, "80%" = 80,
                                  "100%" = 100)
          )
      ),
      box(title = "Histogram box title",
          status = "warning", solidHeader = TRUE, collapsible = TRUE,
          plotOutput("plot", height = 250)
      )
    )
  )
  
  server <- function(input, output, session) {
    
    output$orderNum <- renderText({
      prettyNum(input$orders, big.mark=",")
    })
    
    output$orderNum2 <- renderText({
      prettyNum(input$orders, big.mark=",")
    })
    
    output$progress <- renderUI({
      tagList(input$progress, tags$sup(style="font-size: 20px", "%"))
    })
    
    output$progress2 <- renderUI({
      paste0(input$progress, "%")
    })
    
    
    output$plot <- renderPlot({
      hist(rnorm(input$orders))
    })
    
  }
  
  shinyApp(ui, server)
  
}


## End(Not run)