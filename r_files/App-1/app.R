

library(shiny)
library(caret)
library(readr)
library(ggplot2)

load("RandomForestDia.rda")
load("RandomForestSys.rda")
mse <- read_csv("mse.csv")

ui <- fluidPage(
  h1("Please Enter Patient Information:"),
  sliderInput(inputId = "Age",label = "Enter Age (yrs.)", 
              value = 1, min = 1, max = 100),
  
  sliderInput(inputId = "Weight",label = "Enter Weight (kg)", 
              value = 1, min = 1, max = 300), 
  
  sliderInput(inputId = "Waist",label = "Enter Waist Circumference (cm)", 
              value = 1, min = 1, max = 300), 
  
  sliderInput(inputId = "Cholesterol",label = "Enter Cholesterol (mg/dl)", 
              value = 1, min = 1, max = 500), 
  
  sliderInput(inputId = "LDL",label = "Enter LDL Cholesterol (mg/dl)", 
              value = 1, min = 1, max = 300), 
  
  sliderInput(inputId = "HDL",label = "Enter HDL Cholesterol (mg/dl)", 
              value = 1, min = 1, max = 300), 
  
  sliderInput(inputId = "BMI",label = "Enter BMI (kg/m^2)", 
              value = 1, min = 1, max = 100), 
  
  sliderInput(inputId = "ArmC",label = "Enter Upper Arm Circumference (cm)", 
              value = 1, min = 1, max = 100), 
  
  sliderInput(inputId = "Abdominal",label = "Enter Abdominal Circumference (cm)", 
              value = 1, min = 1, max = 100), 
  
  sliderInput(inputId = "Height",label = "Enter Height (cm)", 
              value = 1, min = 1, max = 300), 
  
  sliderInput(inputId = "LowerA",label = "Enter Arm Circumference (cm)", 
              value = 1, min = 1, max = 100),
  
  sliderInput(inputId = "LegC",label = "Enter Leg Cirumference (cm)", 
              value = 1, min = 1, max = 100),
  
  sliderInput(inputId = "glucose",label = "Enter Glucose (mg/dL)", 
              value = 1, min = 1, max = 500),
  
  selectInput(inputId = "Gender", label = "Enter Gender", choices = c("Male","Female")), 
  
  selectInput(inputId = "Race", label = "Enter Race", choices = c("White","Black","Asian","Hispanic")),
  
  selectInput(inputId = "Diabetes", label = "Diabetes?", choices = c("Yes", "No")),
  
  selectInput(inputId = "Hypertension", label = "Hypertensive?", choices = c("Yes","No")), 
  
  selectInput(inputId = "Hypertension2", label = "Hypertensive Medication?", choices = c("Yes","No")),
  
  
  
  h1("Please Enter Observed Blood Pressure:"),
  
  sliderInput(inputId = "Diastolic",label = "Enter Observed Diastolic Blood Pressure", 
              value = 50, min = 1, max = 200), 
  
  sliderInput(inputId = "Systolic",label = "Enter Observed Systolic Blood Pressure Blood Pressure", 
              value = 50, min = 1, max = 200), 
  
  h1("The Reading Is:"),
  
  verbatimTextOutput("status")
  
) 

server <- function(input,output) {
  
  statement <- reactive({
    ###sys 
    prediction_frame <- data.frame(ridageyr = input$Age, 
                                   bmxwt = input$Weight, 
                                   bmxwaist = input$Waist, 
                                   lbxtc = input$Cholesterol,
                                   lbdldl = input$LDL,
                                   lbdhdd = input$HDL,
                                   bmxbmi = input$BMI,
                                   bmxarmc = input$ArmC,
                                   bmxsad1 = input$Abdominal,
                                   bmxht = input$Height,
                                   bmxarml = input$LowerA,
                                   bmxleg = input$LegC, 
                                   lbxglu = input$glucose, 
                                   riagendr = input$Gender,
                                   bpq020 = input$Hypertension,
                                   bpq040a = input$Hypertension2,
                                   diq010 = input$Diabetes, 
                                   white = 0, 
                                   black = 0, 
                                   asian = 0,
                                   hispanic = 0)
    
    ###Medical Dummies 
    prediction_frame$riagendr <- ifelse(prediction_frame$riagendr == "Male",1,0)
    prediction_frame$bpq020 <- ifelse(prediction_frame$bpq020 == "Yes",1,0)
    prediction_frame$bpq040a <- ifelse(prediction_frame$bpq040a == "Yes",1,0)
    prediction_frame$diq010 <- ifelse(prediction_frame$diq010 == "Yes",1,0)
    
    ###race Dummies 
    prediction_frame$white <- ifelse(input$Race == "White",1,0)
    prediction_frame$black <- ifelse(input$Race == "Black",1,0)
    prediction_frame$asian <- ifelse(input$Race == "Asian",1,0)
    prediction_frame$hispanic <- ifelse(input$Race == "Hispanic",1,0)
    
    ###making prediction
    rf_dia_pred <- predict(rf_dia_tree, newdata = prediction_frame)
    rf_sys_pred <- predict(rf_sys_tree, newdata = prediction_frame)
    
    ###frames for distance calculation 
    predicted <- cbind(rf_dia_pred,rf_sys_pred)
    observed <- cbind(input$Diastolic,input$Systolic)
    
    ###information for the covariance matrix 
    mal_pred <- cbind(mse$rf_pred_dia,mse$rf_pred_sys)
    mal_true <- cbind(mse$true_dia,mse$true_sys)
    
    ###distance calculation 
    distance <- ((observed - predicted) %*% solve(cov(mal_pred,mal_true)) %*% t((observed-predicted)))
    if(distance > 149)
    {
      statement <- "MEASUREMENT ERROR"
    } else 
    {
      statement <- "VALID"
    }
    statement
    
  })
  output$status <- renderPrint({

    print(statement()) 

  }) 
  
} 

shinyApp(ui = ui, server = server)