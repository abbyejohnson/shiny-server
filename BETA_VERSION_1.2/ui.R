# Shiny Dashboard Header
header <- dashboardHeader(title = "Nitrate Leaching Calculator", titleWidth = 300)

# Shiny Dashboard Sidebar
sidebar <- dashboardSidebar(collapsed = T,
                            width = 200,
                            sidebarMenu(menuItem("About", 
                                                 icon = icon("question-circle"), 
                                                 tabName = "about"),
                                        menuItem("Field Management", 
                                                 icon = icon("buromobelexperte"), # Icons can be selected from https://fontawesome.com/v5.15/icons?d=gallery&p=2&q=th
                                                 tabName = "field")
                               #         menuItem("Rotation Management", 
                               #                  icon = icon("calendar-alt"), # Icons can be selected from https://fontawesome.com/v5.15/icons?d=gallery&p=2&q=th
                               #                  tabName = "rotation") 
                            )
)

# Shiny Dashboard Body
body <- dashboardBody(
  theme_grey_light,
  tabItems(
    source('UI_About_Tab.R')$value,
 #   source('UI_Rotation_Management.R')$value,
    source('UI_Field_Management.R')$value
  )
)

UI <- dashboardPage(skin = "green", header, sidebar, body)
