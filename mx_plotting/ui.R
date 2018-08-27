#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Managing parametrs and files"),
  sidebarLayout
  (
    sidebarPanel
    (
        fileInput('file1', 'Choose csv to upload', accept = c('text/csv')),
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 1000,
                  value = 200),
        uiOutput("dataSelector"),
      checkboxInput("checkbox", label = "Coordinates from D_X/YPOS_ID", value = FALSE)
      
    ),
    mainPanel(
      fluidRow(
        column(8, align="center", offset = 2,
               strong(textOutput("text1")))),
      tableOutput('stat'),
      
      fluidRow(
      plotOutput("distPlot"),
      #plotOutput("wafferPlot", height="1024px"),
      #plotOutput("violinPlot",width = "1024px", height = "400px" )
      
      
      plotOutput("wafferPlot", height="900px"),
      plotOutput("violinPlot" ),
      plotOutput("spcPlot"),
      plotOutput("spcPlot_sixSigma"))
      
    )
   
    
  )
)

