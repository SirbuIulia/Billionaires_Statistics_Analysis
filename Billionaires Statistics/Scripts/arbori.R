install.packages("rpart.plot")
library(dplyr)
library(rsample)
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(tree)
library(ipred)

data <- read.csv("C:/Users/IULIA SÎRBU/Downloads/Output_ProiectBD.csv")

data$gdp_country <- as.numeric(gsub('[$,]', '', data$gdp_country))

data$gender <- as.factor(data$gender)
data <- data %>% mutate(finalWorth = ifelse(finalWorth <= median(finalWorth, na.rm = TRUE), "Low", "High"))
data <- data %>% mutate(finalWorth = factor(finalWorth))
table(data$finalWorth)

#Arbori de decizie
set.seed(123)
data_split <- initial_split(data, prop = 0.7, strata = "finalWorth")
train_data <- training(data_split)
test_data <- testing(data_split)
table(train_data$finalWorth)
table(test_data$finalWorth)

m1 <- rpart(
  formula = finalWorth ~ gender + age + gross_tertiary_education_enrollment + 
    gross_primary_education_enrollment_country + gdp_country + 
    cpi_change_country + cpi_country,
  data = train_data,
  method = "class"
)
rpart.plot(m1)
m1

pred_m1 <- predict(m1, newdata = test_data, type = "class")
confusionMatrix(pred_m1, test_data$finalWorth)


m2 <- rpart(finalWorth ~ gender + age + gross_tertiary_education_enrollment + 
              gross_primary_education_enrollment_country + gdp_country + 
              cpi_change_country + cpi_country,
            data = train_data,
            method = "class",
            control = list(cp=0))
rpart.plot(m2)


pred_m2 <- predict(m2, newdata = test_data, type = "class")
confusionMatrix(pred_m2, test_data$finalWorth)


# Modelul random forest
set.seed(123)
m1_rf <- randomForest(
  formula = finalWorth ~ gender + age + gross_tertiary_education_enrollment + 
    gross_primary_education_enrollment_country + gdp_country + 
    cpi_change_country + cpi_country,
  data = train_data
)
plot(m1_rf)
print(m1_rf)

rf_preds <- predict(m1_rf, newdata = test_data)
confusionMatrix(factor(rf_preds), test_data$finalWorth)

