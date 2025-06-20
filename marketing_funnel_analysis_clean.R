
## ---------------------------------------------------------------
## Marketing Funnel Analysis for FinTech App
## Author: Hara Mitsidou
## Description: Data cleaning, anomaly detection, funnel analysis,
##              and logistic regression modeling
## ---------------------------------------------------------------

# 🧹 Setup Environment ------------------------------------------------------
rm(list = ls())

# 📚 Load Libraries
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(countrycode)
library(tidyr)
library(ggcorrplot)
library(tibble)
library(car)

# 📂 Load Data -------------------------------------------------------------
data_path <- "C:/Users/User/Downloads/analyst-test-task-data-set.zip"
testdata <- read_csv(data_path)

# 📊 Initial Overview ------------------------------------------------------
head(testdata)
summary(testdata)
str(testdata)

# 🔧 Data Cleaning ---------------------------------------------------------
binary_vars <- c("Email Verified", "Is Verified", "Deposited", "Test Profile")
testdata[binary_vars] <- lapply(testdata[binary_vars], function(x) ifelse(x == "Yes", 1, 0))

testdata <- testdata %>%
  mutate(IsRealUser = `Test Profile` == 0)

missing_pct <- sapply(testdata, function(x) sum(is.na(x)) / length(x))
print(round(missing_pct * 100, 1))

duplicate_count <- sum(duplicated(testdata))
cat("✅ Duplicates found:", duplicate_count, "\n")

testdata$Country <- str_trim(testdata$Country)
testdata[binary_vars] <- lapply(testdata[binary_vars], as.factor)
categorical_vars <- c("Country", "UTM Source")
testdata[categorical_vars] <- lapply(testdata[categorical_vars], as.factor)

# 📌 Filter to Real Users --------------------------------------------------
real_users <- testdata %>%
  filter(IsRealUser) %>%
  mutate(
    Days_To_Verify = as.numeric(difftime(`Verified At`, `Registration Date`, units = "days")),
    Days_To_Deposit = as.numeric(difftime(`First Deposit Date`, `Registration Date`, units = "days")),
    Is_Referred = ifelse(is.na(`Referred By`), 0, 1),
    Traded = ifelse(!is.na(`Last Trade Date`), 1, 0)
  )

# 🧠 Logistic Regression Models --------------------------------------------

model_data <- real_users %>%
  filter(!is.na(`Is Verified`), !is.na(`Email Verified`), !is.na(Questionnaire)) %>%
  mutate(
    `Is Verified` = ifelse(`Is Verified` == 1, 1, 0),  # Ensures only 1 or 0
    `Email Verified` = as.numeric(`Email Verified`),
    `Deposited` = as.numeric(`Deposited`),
    `Questionnaire` = ifelse(tolower(Questionnaire) == "approved", 1, 0)
  )


# 1️⃣ Verification Model
model_verif <- glm(`Is Verified` ~ `Email Verified` + `Questionnaire` + Country + `UTM Source`,
                   data = model_data, family = binomial)
summary(model_verif)
cat("\n--- VIF: Verification Model ---\n")
print(vif(model_verif))

# 2️⃣ Deposit Model
model_deposit <- glm(`Deposited` ~ `Email Verified` + `Questionnaire` + Country + `UTM Source`,
                     data = model_data, family = binomial)
summary(model_deposit)
cat("\n--- VIF: Deposit Model ---\n")
print(vif(model_deposit))

# 3️⃣ Trade Model
model_trade <- glm(`Traded` ~ `Email Verified` + `Questionnaire` + Country + `UTM Source`,
                   data = model_data, family = binomial)
summary(model_trade)
cat("\n--- VIF: Trade Model ---\n")
print(vif(model_trade))

# 💾 Export Cleaned Data ---------------------------------------------------
write.csv(real_users, "real_users.csv", row.names = FALSE)
