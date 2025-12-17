library(plumber)
library(dplyr)

stroke_model <- readRDS("models/modelstepv3.rds")

#* @apiTitle Stroke Risk Prediction API
#* @apiDescription This API predicts the probability of suffering a stroke based on the GLM Stepwise V3 model.

#* Predict stroke risk
#* @param gender Gender (Male, Female, Other)
#* @param age Age (numeric)
#* @param hypertension Hypertension (No, Yes)
#* @param heart_disease Heart Disease (No, Yes)
#* @param ever_married Ever Married? (No, Yes)
#* @param work_type Work Type (Private, Self-employed, Govt_job, children, Never_worked)
#* @param residence Residence Type (Urban, Rural)
#* @param avg_glucose_level Average Glucose Level (numeric)
#* @param BMI Body Mass Index (numeric)
#* @param smoking_status Smoking Status (formerly smoked, never smoked, smokes, Unknown)
#* @post /predict
function(gender, age, hypertension, heart_disease, ever_married, 
         work_type, residence, avg_glucose_level, BMI, smoking_status) {
  
    input_data <- data.frame(
    gender = as.factor(gender),
    age = as.numeric(age),
    hypertension = factor(hypertension, levels = c("No", "Yes")),
    heart_disease = factor(heart_disease, levels = c("No", "Yes")),
    ever_married = as.factor(ever_married),
    work_type = as.factor(work_type),
    residence = as.factor(residence),
    avg_glucose_level = as.numeric(avg_glucose_level),
    BMI = as.numeric(BMI),
    smoking_status = as.factor(smoking_status)
  )
  
  probability <- predict(stroke_model, newdata = input_data, type = "response")
  
 
  classification <- ifelse(probability >= 0.1, "High Risk", "Low Risk")
  
  # Return JSON
  return(list(
    status = "success",
    stroke_probability = round(as.numeric(probability), 4),
    alert_level = classification,
    received_input = input_data
  ))
}