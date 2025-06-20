
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

# 📂 Load Data -------------------------------------------------------------
data_path <- "C:/Users/User/Downloads/analyst-test-task-data-set.zip"
testdata <- read_csv(data_path)

# 📊 Initial Overview ------------------------------------------------------
head(testdata)
summary(testdata)
str(testdata)

# 🔧 Data Cleaning ---------------------------------------------------------
# 1. Convert Yes/No to binary
binary_vars <- c("Email Verified", "Is Verified", "Deposited", "Test Profile")
testdata[binary_vars] <- lapply(testdata[binary_vars], function(x) ifelse(x == "Yes", 1, 0))

# 2. Create IsRealUser flag
testdata <- testdata %>%
  mutate(IsRealUser = `Test Profile` == 0)

# 3. Handle missing values
missing_pct <- sapply(testdata, function(x) sum(is.na(x)) / length(x))
print(round(missing_pct * 100, 1))

# 4. Remove duplicates
duplicate_count <- sum(duplicated(testdata))
cat("✅ Duplicates found:", duplicate_count, "\n")

# 5. Clean categorical variables
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
    Is_Referred = ifelse(is.na(`Referred By`), 0, 1)
  )

# 📉 Anomaly Detection -----------------------------------------------------
anomaly_verified_no_docs <- real_users %>%
  filter(`Is Verified` == 1 &
           (is.na(Questionnaire) | is.na(`Proof of Address`) | is.na(`Passport / National ID`)))

cat("🔍 Verified without all docs:", nrow(anomaly_verified_no_docs), "\n")

anomaly_deposit_unverified <- real_users %>%
  filter(Deposited == 1 & `Is Verified` == 0)

cat("🔍 Deposited but not verified:", nrow(anomaly_deposit_unverified), "\n")

anomaly_wrong_dates <- real_users %>%
  filter((!is.na(`First Deposit Date`) & `First Deposit Date` < `Registration Date`) |
         (!is.na(`Last Trade Date`) & `Last Trade Date` < `Registration Date`))

cat("🔍 Activity before registration:", nrow(anomaly_wrong_dates), "\n")

# ⏱️ Delayed Processes -----------------------------------------------------
long_verification <- real_users %>% filter(Days_To_Verify > 30)
long_deposit <- real_users %>% filter(Days_To_Deposit > 30)

cat("📅 Delays >30 days - Verification:", nrow(long_verification), "Deposit:", nrow(long_deposit), "\n")

# 📈 Funnel View -----------------------------------------------------------
funnel <- tibble(
  Stage = c("Registered", "Email Verified", "All Docs Approved", "Verified", "Deposited", "Traded"),
  Count = c(
    nrow(real_users),
    sum(real_users$`Email Verified` == 1, na.rm = TRUE),
    nrow(real_users %>%
           filter(tolower(Questionnaire) == "approved",
                  tolower(`Proof of Address`) == "approved",
                  tolower(`Passport / National ID`) == "approved")),
    sum(real_users$`Is Verified` == 1, na.rm = TRUE),
    sum(real_users$Deposited == 1, na.rm = TRUE),
    sum(!is.na(real_users$`Last Trade Date`))
  )
)

ggplot(funnel, aes(x = reorder(Stage, -Count), y = Count)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = Count), vjust = -0.5) +
  labs(title = "User Funnel: Registration to Trading", x = "Stage", y = "Users") +
  theme_minimal()

# 🧠 Logistic Regression ---------------------------------------------------
model_data <- real_users %>%
  filter(!is.na(`Is Verified`), !is.na(`Email Verified`), !is.na(Questionnaire)) %>%
  mutate(
    `Is Verified` = as.numeric(`Is Verified`),
    `Email Verified` = as.numeric(`Email Verified`),
    `Questionnaire` = ifelse(tolower(Questionnaire) == "approved", 1, 0)
  )

model <- glm(`Is Verified` ~ `Email Verified` + `Questionnaire` + Country + `UTM Source`,
             data = model_data, family = binomial)

summary(model)

# 💾 Export Cleaned Data ---------------------------------------------------
write.csv(real_users, "real_users.csv", row.names = FALSE)
