# Load required libraries
library(dplyr)
library(ggplot2)
library(survey)
library(tidyr)

# Assuming survey_data is already loaded in your environment
# If not, run the previous code first

# 1. Basic Data Exploration
# ------------------------
# View first few rows
head(survey_data)

# Check data structure
str(survey_data)

# Summary statistics
summary(survey_data)

# 2. Regional Analysis
# -------------------
# Create survey design object
survey_design <- svydesign(
    id = ~1,
    strata = ~region,
    weights = ~weight,
    data = survey_data
)

# Calculate regional satisfaction
regional_analysis <- svyby(~satisfaction, ~region, survey_design, svymean)
print("Regional Satisfaction Analysis:")
print(regional_analysis)

# Visualize regional satisfaction
ggplot(survey_data, aes(x = region, y = satisfaction, fill = region)) +
    geom_boxplot() +
    theme_minimal() +
    labs(
        title = "Customer Satisfaction by Region",
        x = "Region",
        y = "Satisfaction Score"
    )

# 3. Store Size Impact Analysis
# ----------------------------
# Calculate store size satisfaction
size_analysis <- svyby(~satisfaction, ~store_size, survey_design, svymean)
print("\nStore Size Satisfaction Analysis:")
print(size_analysis)

# Visualize store size satisfaction
ggplot(survey_data, aes(x = store_size, y = satisfaction, fill = store_size)) +
    geom_violin() +
    geom_boxplot(width = 0.1) +
    theme_minimal() +
    labs(
        title = "Customer Satisfaction by Store Size",
        x = "Store Size",
        y = "Satisfaction Score"
    )

# 4. Customer Type Analysis
# ------------------------
# Calculate customer type satisfaction
customer_analysis <- svyby(~satisfaction, ~customer_type, survey_design, svymean)
print("\nCustomer Type Satisfaction Analysis:")
print(customer_analysis)

# Visualize customer type satisfaction
ggplot(survey_data, aes(x = customer_type, y = satisfaction, fill = customer_type)) +
    geom_boxplot() +
    theme_minimal() +
    labs(
        title = "Customer Satisfaction by Customer Type",
        x = "Customer Type",
        y = "Satisfaction Score"
    )

# 5. Cross Analysis
# ----------------
# Region and Store Size interaction
cross_analysis <- survey_data %>%
    group_by(region, store_size) %>%
    summarise(
        mean_satisfaction = mean(satisfaction),
        sd_satisfaction = sd(satisfaction),
        n = n()
    )

# Visualize cross analysis
ggplot(cross_analysis, aes(x = region, y = mean_satisfaction, fill = store_size)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal() +
    labs(
        title = "Average Satisfaction by Region and Store Size",
        x = "Region",
        y = "Mean Satisfaction Score"
    )

# 6. Statistical Tests
# -------------------
# ANOVA test for regional differences
region_aov <- aov(satisfaction ~ region, data = survey_data)
print("\nANOVA Test Results for Regional Differences:")
print(summary(region_aov))

# 7. Generate Summary Report
# ------------------------
summary_report <- data.frame(
    Metric = c(
        "Overall Mean Satisfaction",
        "Highest Region",
        "Lowest Region",
        "Best Store Size",
        "Best Customer Type"
    ),
    Value = c(
        mean(survey_data$satisfaction),
        regional_analysis$region[which.max(regional_analysis$satisfaction)],
        regional_analysis$region[which.min(regional_analysis$satisfaction)],
        size_analysis$store_size[which.max(size_analysis$satisfaction)],
        customer_analysis$customer_type[which.max(customer_analysis$satisfaction)]
    ),
    Score = c(
        round(mean(survey_data$satisfaction), 2),
        round(max(regional_analysis$satisfaction), 2),
        round(min(regional_analysis$satisfaction), 2),
        round(max(size_analysis$satisfaction), 2),
        round(max(customer_analysis$satisfaction), 2)
    )
)

print("\nFinal Summary Report:")
print(summary_report)

# 8. Save Results (optional)
# ------------------------
# Save plots
ggsave("regional_satisfaction.png", width = 10, height = 6)
ggsave("store_size_satisfaction.png", width = 10, height = 6)
ggsave("customer_type_satisfaction.png", width = 10, height = 6)

# Save summary report
write.csv(summary_report, "satisfaction_summary.csv", row.names = FALSE)