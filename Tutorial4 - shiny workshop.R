library(shiny)
library(shinydashboard)

# loading data ----
df <- read.csv("Blackwell_Hist_Sample.csv")


# app creation ----
# ui
ui <- dashboardPage(
  
  dashboardHeader(title = "My first shiny app"),
  
  dashboardSidebar(
    menuItem(text = "1st option", tabName = "op1", icon = icon("fas fa-anchor"),
             menuSubItem(text = "Histogram", tabName = "subop1"),
             sliderInput(inputId = "Cool_bar", label = "Number of bins: "
                         ,min = 0, max = 100, value = 50)),
    menuItem(text = "2st option", tabName = "op2", icon = icon("fas fa-anchor")),
    menuItem(text = "3st option", tabName = "op3", icon = icon("fas fa-anchor"))
  ),
  
  dashboardBody(
    fluidRow(
      box(plotOutput(outputId = "box1", height = 250))
    )
  )
  
)

# server
server <- function(input, output) {
  
  output$box1 <- renderPlot({
    hist(df$age, breaks = input$Cool_bar)
  })
  
}

# function
shinyApp(ui, server)