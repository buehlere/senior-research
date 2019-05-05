

library(shiny)
library(readr)
library(ggplot2)
library(caret)
setwd("C:/Users/buehl/Documents")
load("RandomForestDia.rda")
load("RandomForestSys.rda")
mse <- read.csv("mse.csv")


ui <- fluidPage(
  h1("Please Enter Patient Information:"),
  sliderInput(inputId = "Age",label = "Enter Age (yrs.)", 
              value = 10, min = 1, max = 100),
  
  sliderInput(inputId = "Weight",label = "Enter Weight (kg)", 
              value = 1, min = 1, max = 300), 
  
  sliderInput(inputId = "BMI",label = "Enter BMI (kg/m^2)", 
              value = 1, min = 1, max = 300), 
  
  sliderInput(inputId = "BMI",label = "Enter Height (cm)", 
              value = 1, min = 1, max = 300), 
  
  selectInput(inputId = "Gender", label = "Enter Gender", choices = c("Male","Female")), 
  
  h1("Please Enter Observed Blood Pressure:"),
  
  sliderInput(inputId = "Diastolic",label = "Enter Observed Diastolic Blood Pressure", 
              value = 5, min = 1, max = 300), 
  
  sliderInput(inputId = "Systolic",label = "Enter Observed Systolic Blood Pressure Blood Pressure", 
              value = 300, min = 1, max = 300), 
  
  h1("The Reading Is:"),
  
  verbatimTextOutput("status")
  
) 

server <- function(input,output) {
  
  output$status <- renderPrint({
    ###sys 
    linear = lm(sys_mean~ridageyr+bmxwt,lin_frame_clean)
    prediction_frame <- data.frame(ridageyr = input$Age,bmxwt = input$Weight)
    prediction_sys <- predict(linear, prediction_frame)
    
    
    ###dia 
    linear = lm(dia_mean~ridageyr+bmxwt,lin_frame_clean)
    prediction_frame <- data.frame(ridageyr = input$Age,bmxwt = input$Weight)
    prediction_dia <- predict(linear, newdata = prediction_frame)
    
    ##distnace
    prediction <- data.frame(preds = prediction_sys,predd = prediction_dia)
    observed <- data.frame(systolic = input$Systolic,diastolic = input$Diastolic)
    a <- cbind(observed$systolic, observed$diastolic) 
    b <- cbind(prediction$preds, prediction$predd)
    
    
    if(is.na(distance))
    {
      print("valid")
    } else 
    {
      if(distance > 149)
      {
        print("not valid")
      }
      else
      {
        print("valid")
      } 
    }

    
  }) 
  
} 

shinyApp(ui = ui, server = server)