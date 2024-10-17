# Load required libraries
library(dplyr)
library(survey)

# Set seed for reproducibility
set.seed(123)

# Create sample survey dataset
n_responses <- 1000

# Create the survey_data dataframe
survey_data <- data.frame(
    # Customer ID
    customer_id = 1:n_responses,
    
    # Region (North: 30%, South: 25%, East: 20%, West: 25%)
    region = sample(
        c("North", "South", "East", "West"),
        n_responses,
        prob = c(0.3, 0.25, 0.2, 0.25),
        replace = TRUE
    ),
    
    # Store Size
    store_size = sample(
        c("Small", "Medium", "Large"),
        n_responses,
        prob = c(0.3, 0.4, 0.3),
        replace = TRUE
    ),
    
    # Customer Type
    customer_type = sample(
        c("Regular", "Premium", "Occasional"),
        n_responses,
        prob = c(0.5, 0.2, 0.3),
        replace = TRUE
    )
)

# Generate satisfaction scores based on region, store size, and customer type
survey_data <- survey_data %>%
    mutate(
        # Base satisfaction score
        satisfaction = case_when(
            region == "West" ~ rnorm(n(), 4.2, 0.5),
            region == "North" ~ rnorm(n(), 4.0, 0.5),
            region == "East" ~ rnorm(n(), 3.9, 0.5),
            region == "South" ~ rnorm(n(), 3.7, 0.5)
        ),
        
        # Adjust for store size
        satisfaction = case_when(
            store_size == "Small" ~ satisfaction + rnorm(n(), -0.1, 0.2),
            store_size == "Medium" ~ satisfaction + rnorm(n(), 0.2, 0.2),
            store_size == "Large" ~ satisfaction
        ),
        
        # Adjust for customer type
        satisfaction = case_when(
            customer_type == "Premium" ~ satisfaction + rnorm(n(), 0.3, 0.2),
            customer_type == "Regular" ~ satisfaction,
            customer_type == "Occasional" ~ satisfaction - rnorm(n(), 0.2, 0.2)
        ),
        
        # Ensure satisfaction stays within 1-5 range
        satisfaction = pmin(pmax(satisfaction, 1), 5)
    )

# Calculate weights based on population distribution
survey_data <- survey_data %>%
    group_by(region) %>%
    mutate(
        weight = case_when(
            region == "North" ~ 0.3,
            region == "South" ~ 0.25,
            region == "East" ~ 0.2,
            region == "West" ~ 0.25
        ) / (n() / n_responses)
    ) %>%
    ungroup()

# Analysis using survey package
# Create survey design object
survey_design <- svydesign(
    id = ~1,
    strata = ~region,
    weights = ~weight,
    data = survey_data
)

# Overall satisfaction
overall_satisfaction <- svymean(~satisfaction, survey_design)
print("Overall Satisfaction:")
print(overall_satisfaction)

# Satisfaction by region
regional_satisfaction <- svyby(~satisfaction, ~region, survey_design, svymean)
print("\nSatisfaction by Region:")
print(regional_satisfaction)

# Satisfaction by store size
size_satisfaction <- svyby(~satisfaction, ~store_size, survey_design, svymean)
print("\nSatisfaction by Store Size:")
print(size_satisfaction)

# Satisfaction by customer type
customer_satisfaction <- svyby(~satisfaction, ~customer_type, survey_design, svymean)
print("\nSatisfaction by Customer Type:")
print(customer_satisfaction)

# Basic summary statistics
summary_stats <- survey_data %>%
    group_by(region, store_size, customer_type) %>%
    summarise(
        mean_satisfaction = mean(satisfaction),
        sd_satisfaction = sd(satisfaction),
        n = n()
    )

print("\nDetailed Summary Statistics:")
print(summary_stats)

# Optional: Save the dataset
# write.csv(survey_data, "retail_satisfaction_survey.csv", row.false = FALSE)