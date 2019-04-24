

library(shiny)


ui <- fluidPage(
  sliderInput(inputId = "Age",label = "Enter Age", 
              value = 10, min = 1, max = 100),
  
  sliderInput(inputId = "Weight",label = "Enter Weight", 
              value = 1, min = 1, max = 300), 
  
  plotOutput("hist")
  
) 

server <- function(input,output) {
  output$hist <- renderPlot({
    hist(rnorm(input$Age))
    })
  
} 

shinyApp(ui = ui, server = server)