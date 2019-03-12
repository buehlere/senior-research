library(shiny)

ui <- fluidPage(
  checkboxGroupInput(inputId = "groups",
                     label = "choose a group",
                     choices = c("1","2","3","4")) ,
  plotOutput(output = "Clusters") 
) 

server <- function(input,output){
  output$Clusters <- renderPlot({
    plot_frame <- cluster_frame %>% filter(cluster == input$groups)
    principal_comp <- prcomp(plot_frame[,-c(1,9)],scale.=TRUE)
    autoplot(principal_comp, data = plot_frame, colour = 'cluster')
  })
}

shinyApp(ui = ui, server = server)