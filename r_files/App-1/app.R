

library(shiny)
library(readr)
library(ggplot2)
library(caret)
setwd("C:/Users/buehl/Documents")
load("RandomForestDia.rda")
load("RandomForestSys.rda")


ui <- fluidPage(
  sliderInput(inputId = "Age",label = "Enter Age (yrs.)", 
              value = 10, min = 1, max = 100),
  
  sliderInput(inputId = "Weight",label = "Enter Weight (kg)", 
              value = 1, min = 1, max = 300), 
  
  sliderInput(inputId = "BMI",label = "Enter BMI (kg/m^2)", 
              value = 1, min = 1, max = 300), 
  
  sliderInput(inputId = "BMI",label = "Enter Waist Circumference (cm)", 
              value = 1, min = 1, max = 300), 
  
  sliderInput(inputId = "BMI",label = "Enter Height (cm)", 
              value = 1, min = 1, max = 300), 
  
  selectInput(inputId = "Race", label = "Enter Race", choices = c("White","Black","Hispanic","Asian")), 
  
  selectInput(inputId = "Gender", label = "Enter Gender", choices = c("Male","Female")), 
  
  sliderInput(inputId = "Diastolic",label = "Enter Observed Diastolic Blood Pressure", 
              value = 1, min = 1, max = 300), 
  
  sliderInput(inputId = "Systolic",label = "Enter Observed Systolic Blood Pressure Blood Pressure", 
              value = 1, min = 1, max = 300), 
  
  verbatimTextOutput("status")
  
) 

server <- function(input,output) {
  
  output$status <- renderPrint({
    prediction <- data.frame(systolic = 0, diastolic = 0)
    observed <- data.frame(as.numeric(input$Systolic), as.numeric(input$Diastolic))
    ###sys 
    linear = lm(sys_mean~ridageyr+bmxwt,lin_frame_clean)
    prediction_frame <- data.frame(ridageyr = input$Age,bmxwt = input$Weight)
    prediction$systolic <- predict(linear, newdata = prediction_frame)
    
    
    ###dia 
    linear = lm(dia_mean~ridageyr+bmxwt,lin_frame_clean)
    prediction_frame <- data.frame(ridageyr = input$Age,bmxwt = input$Weight)
    prediction$diastolic <- predict(linear, newdata = prediction_frame)
    head(prediction)
    #head(observed)


    #distance <- mahalanobis(prediction, observed, cov(prediction,observed))
    
  }) 
  
} 

shinyApp(ui = ui, server = server)