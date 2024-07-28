#myAPI.R 
library(plumber)
library(leaflet)
library(tidyverse)
library(forcats)

# Reading in the data

data <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")
data <- data |>
  mutate(Diabetes_binary = as.factor(Diabetes_binary),
         HighBP = as.factor(HighBP),
         HighChol = as.factor(HighChol),
         Smoker = as.factor(Smoker),
         Age = as.factor(Age),
         Education = as.factor(Education),
         PhysActivity = as.factor(PhysActivity),
         Fruits = as.factor(Fruits),
         Veggies = as.factor(Veggies)
  )
Diabetes_data <- data |>
  select(where(is.factor), BMI) |>
  mutate(Diabetes_binary = fct_recode(Diabetes_binary,"NoDiabetes" = "0",
                                      "Diabetes" = "1"),
         HighBP = fct_recode(HighBP, "No" = "0",
                             "Yes" = "1"),
         HighChol = fct_recode(HighChol, "No" = "0",
                               "Yes" = "1"),
         Smoker = fct_recode(Smoker, "No" = "0",
                             "Yes" = "1"),
         PhysActivity = fct_recode(PhysActivity, "No" = "0",
                                   "Yes" = "1"),
         Fruits = fct_recode(Fruits, "No" = "0",
                             "Yes" = "1"),
         Veggies = fct_recode(Veggies, "No" = "0",
                              "Yes" = "1"),
         Age = fct_recode(Age, 
                          "18-24" = "1",
                          "25-29" = "2",
                          "30-34" = "3",
                          "35-39" = "4",
                          "40-44" = "5",
                          "45-49" = "6",
                          "50-54" = "7",
                          "55-59" = "8",
                          "60-64" = "9",
                          "65-69" = "10",
                          "70-74" = "11",
                          "75-79" = "12",
                          "80+" = "13"),
         Education = fct_recode(Education,
                                "Noschool" = "1",
                                "Grades 1-8" = "2",
                                "Grades 9-11" = "3",
                                "High School Graduate" = "4",
                                "College 1-3 Years" = "5",
                                "College Graduate" = "6"))

Logistic_Model_3 <-glm(Diabetes_binary ~ HighBP + HighChol + Smoker + Education + BMI + Veggies + PhysActivity + Age, data = Diabetes_data,
                       family = binomial())

#* @param predictor
#* @get /pred
function(predictor, data = Diabetes_data) {
  if (is.numeric(data[[predictor]])) {
    value <- mean(data[[predictor]])
    message <- paste("The mean of", predictor, "is", value)
    return(message)
  } else if (is.factor(data[[predictor]])) {
    table <- table(data[[predictor]])
    max <- names(table)[which.max(table)]
    message <- paste("The most frequent class of", predictor, "is", max)
    return(paste(message))
  } else {
    stop("Invalid predictor.")
  }
}

#http://localhost:PORT/pred?predictor="HighBP"
#http://localhost:PORT/pred?predictor="BMI"
#http://localhost:PORT/pred?predictor="Education"

#* @get /info
function(){
  "Luke Freudenheim, the URL for the GitHub is https://lukefreud.github.io/ST-558-Final-Project/"
}

#http://localhost:PORT/info
