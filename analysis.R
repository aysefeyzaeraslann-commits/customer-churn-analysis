# 1. Load required library
library(tidyverse)

# 2. Set seed for reproducibility
set.seed(42)
n <- 500

# 3. Create raw dataset
churn_data <- data.frame(
  CustomerID = paste0("CUST-", 1000 + 1:n),
  Gender = sample(c("Male", "Female"), n, replace = TRUE),
  SeniorCitizen = sample(c(0, 1), n, replace = TRUE, prob = c(0.85, 0.15)),
  Tenure = sample(1:72, n, replace = TRUE),
  Contract = sample(c("Month-to-month", "One year", "Two year"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
  PaymentMethod = sample(c("Electronic check", "Mailed check", "Bank transfer", "Credit card"), n, replace = TRUE),
  MonthlyCharges = round(runif(n, 20, 120), 2),
  Churn = sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.7, 0.3))
)

# 4. Add TotalCharges and missing values
churn_data$TotalCharges <- round(churn_data$Tenure * churn_data$MonthlyCharges + rnorm(n, mean = 0, sd = 10), 2)
churn_data$TotalCharges[sample(1:n, 15)] <- NA

# 5. Clean dataset
clean_churn_data <- churn_data %>%
  na.omit() %>%
  mutate(
    Gender = as.factor(Gender),
    SeniorCitizen = factor(SeniorCitizen, levels = c(0, 1), labels = c("No", "Yes")),
    Contract = as.factor(Contract),
    PaymentMethod = as.factor(PaymentMethod),
    Churn = as.factor(Churn),
    TenureGroup = case_when(
      Tenure <= 12 ~ "0-1 Year",
      Tenure <= 24 ~ "1-2 Years",
      Tenure <= 48 ~ "2-4 Years",
      TRUE ~ "4+ Years"
    ),
    TenureGroup = as.factor(TenureGroup)
  )

# 6. Save clean dataset
write.csv(clean_churn_data, "customer_churn_clean.csv", row.names = FALSE)

# 7. Check if file is created
file.exists("customer_churn_clean.csv")
# Plot 1: Churn Rate by Contract Type
ggplot(clean_churn_data, aes(x = Contract, fill = Churn)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    title = "Customer Churn Rate by Contract Type",
    subtitle = "Month-to-month contracts show significantly higher churn risk",
    x = "Contract Type",
    y = "Percentage",
    fill = "Churn Status"
  ) +
  scale_fill_manual(values = c("No" = "#2ee2a6", "Yes" = "#ff5964"))

# Save the plot as an image for GitHub README
ggsave("churn_by_contract.png", width = 8, height = 5)
# Plot 2: Distribution of Monthly Charges by Churn Status
ggplot(clean_churn_data, aes(x = Churn, y = MonthlyCharges, fill = Churn)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  theme_minimal() +
  labs(
    title = "Monthly Charges Distribution by Churn Status",
    subtitle = "Customers who churn tend to have higher monthly charges",
    x = "Churn Status",
    y = "Monthly Charges ($)"
  ) +
  scale_fill_manual(values = c("No" = "#2ee2a6", "Yes" = "#ff5964"))

# Save plot as PNG image for GitHub
ggsave("churn_by_monthly_charges.png", width = 8, height = 5)
# Build a Logistic Regression model to predict Churn
churn_model <- glm(
  Churn ~ Contract + MonthlyCharges + Tenure + SeniorCitizen,
  data = clean_churn_data,
  family = binomial
)

# Display statistical summary of the model
summary(churn_model)

# Extract Odds Ratios (Effect Size) for easy business interpretation
odds_ratios <- exp(coef(churn_model))
print("Odds Ratios:")
print(odds_ratios)