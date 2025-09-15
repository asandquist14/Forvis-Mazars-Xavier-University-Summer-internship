#### Packages and Data ####

library(tidyverse)
library(viridis)
library(ggpmisc)
library(plotly)
library(car)
library(glmnet)
library(randomForest)
library(rpart)
library(rpart.plot)

data <- read_csv("COMBINED_DATA_8.csv")

data <-  data %>% 
  mutate("17Below_Per" = (POP_MALE_17BELOW+POP_FEMALE_17BELOW)/ESTIMATED_POPULATION) %>%
  mutate("18-24_Per" = (POP_MALE_18TO24+POP_FEMALE_18TO24)/ESTIMATED_POPULATION) %>% 
  mutate("25-44_Per" = (POP_MALE_25TO44+POP_FEMALE_25TO44)/ESTIMATED_POPULATION) %>% 
  mutate("45-64_Per" = (POP_MALE_45TO64+POP_FEMALE_45TO64)/ESTIMATED_POPULATION) %>% 
  mutate("65-84_Per" = (POP_MALE_65TO84+POP_FEMALE_65TO84)/ESTIMATED_POPULATION) %>% 
  mutate("85Up_Per" = (POP_MALE_85UP+POP_FEMALE_85UP)/ESTIMATED_POPULATION) %>% 
  mutate("prop_cost_uncomp" = COST_UNCOMP/TOT_COST) %>% 
  mutate("total_house" = HOUSEHOLD_INCOME_30BELOW + HOUSEHOLD_INCOME_30TO50 + HOUSEHOLD_INCOME_50TO100 + HOUSEHOLD_INCOME_100TO200 + HOUSEHOLD_INCOME_200UP) %>% 
  mutate("HOUSEHOLD_INCOME_30BELOW_PER" = (HOUSEHOLD_INCOME_30BELOW/total_house)) %>% 
  mutate("HOUSEHOLD_INCOME_30to50_PER" = (HOUSEHOLD_INCOME_30TO50/total_house)) %>% 
  mutate("HOUSEHOLD_INCOME_50to100_PER" = (HOUSEHOLD_INCOME_50TO100/total_house)) %>% 
  mutate("HOUSEHOLD_INCOME_100to200_PER" = (HOUSEHOLD_INCOME_100TO200/total_house)) %>% 
  mutate("HOUSEHOLD_INCOME_200UP_PER" = (HOUSEHOLD_INCOME_200UP/total_house))
  

#### Plots with Age Cohorts ####

age_groups <- list(
  "0–17" = c("POP_MALE_17BELOW", "POP_FEMALE_17BELOW"),
  "18–24" = c("POP_MALE_18TO24", "POP_FEMALE_18TO24"),
  "25–44" = c("POP_MALE_25TO44", "POP_FEMALE_25TO44"),
  "45–64" = c("POP_MALE_45TO64", "POP_FEMALE_45TO64"),
  "65–84" = c("POP_MALE_65TO84", "POP_FEMALE_65TO84"),
  "85+"   = c("POP_MALE_85UP",    "POP_FEMALE_85UP")
)

# Calculate total population and proportions
df_age <- df %>%
  mutate(
    TOTAL_POP = rowSums(select(., unlist(age_groups)), na.rm = TRUE),
    UNCOMP_RATIO = COST_UNCOMP / TOT_COST
  )

# Create long-format data with age group proportions
long_df_age <- map_dfr(names(age_groups), function(group) {
  cols <- age_groups[[group]]
  df_age %>%
    mutate(
      AGE_GROUP = group,
      PERCENT_IN_GROUP = rowSums(select(., all_of(cols)), na.rm = TRUE) / TOTAL_POP
    ) %>%
    select(STATE_EXPANDED_MEDICAID,PROVIDER_CCN, AGE_GROUP, PERCENT_IN_GROUP, UNCOMP_RATIO)
})

# Filter to remove NAs
long_df_age <- long_df_age %>% filter(!is.na(PERCENT_IN_GROUP), !is.na(UNCOMP_RATIO))

# Plot with ggplot2
ggplot(long_df_age, aes(x = PERCENT_IN_GROUP, y = UNCOMP_RATIO)) +
  geom_point(alpha = 0.6, color = "#0073C2") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_poly_eq(
    aes(label = after_stat(rr.label)),
    formula = y ~ x,
    parse = TRUE,
    size = 3.5,
    npcx = 1,
    npcy = 0.5,
    color = "black"
  ) +
  facet_grid(
    STATE_EXPANDED_MEDICAID ~ AGE_GROUP,
    labeller = labeller(
      STATE_EXPANDED_MEDICAID = c(`FALSE` = "Non-Expansion State", `TRUE` = "Expansion State")
    )
  ) +
  labs(
    title = "Uncompensated Cost vs Age Group Proportions",
    x = "Proportion of Population in Age Group",
    y = "Uncompensated Cost Proportion"
  ) +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

# The 0-17 age group matches very well with the 25-44 age group. (Their parents)
# Then the 18-24 group, which is still on their parents insurance

#### Plots with Household Income ####

# Define income cohorts
income_groups <- c(
  "HOUSEHOLD_INCOME_30BELOW",
  "HOUSEHOLD_INCOME_30TO50",
  "HOUSEHOLD_INCOME_50TO100",
  "HOUSEHOLD_INCOME_100TO200",
  "HOUSEHOLD_INCOME_200UP"
)

# Compute total households
data_income <- df %>%
  mutate(HOUSEHOLD_TOTAL = rowSums(select(., all_of(income_groups)), na.rm = TRUE))

# Reshape to long format
income_long <- data_income %>%
  select(
    PROVIDER_CCN,
    STATE_EXPANDED_MEDICAID,
    COST_UNCOMP,
    TOT_COST,
    all_of(income_groups),
    HOUSEHOLD_TOTAL
  ) %>%
  pivot_longer(
    cols = all_of(income_groups),
    names_to = "HOUSEHOLD_INCOME_GROUP",
    values_to = "COUNT_IN_GROUP"
  ) %>%
  mutate(PERCENT_IN_GROUP = COUNT_IN_GROUP / HOUSEHOLD_TOTAL) %>%
  select(
    PROVIDER_CCN,
    STATE_EXPANDED_MEDICAID,
    HOUSEHOLD_INCOME_GROUP,
    PERCENT_IN_GROUP,
    COST_UNCOMP,
    TOT_COST
  )
income_long <-  income_long %>% 
  mutate(prop_uncomp_cost = COST_UNCOMP/TOT_COST)

income_long <- income_long %>%
  mutate(HOUSEHOLD_INCOME_GROUP = factor(
    HOUSEHOLD_INCOME_GROUP,
    levels = c(
      "HOUSEHOLD_INCOME_30BELOW",
      "HOUSEHOLD_INCOME_30TO50",
      "HOUSEHOLD_INCOME_50TO100",
      "HOUSEHOLD_INCOME_100TO200",
      "HOUSEHOLD_INCOME_200UP"
    )
  ))

# Now plot with ordered facets
ggplot(income_long, aes(x = PERCENT_IN_GROUP, y = prop_uncomp_cost)) +
  geom_point(alpha = 0.6, color = "#0073C2") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_poly_eq(
    aes(label = after_stat(rr.label)),
    formula = y ~ x,
    parse = TRUE,
    size = 3.5,
    npcx = 0.95,  # Right side
    npcy = 0.5,
    color = "black"
  ) +
  facet_grid(
    STATE_EXPANDED_MEDICAID ~ HOUSEHOLD_INCOME_GROUP,
    labeller = labeller(
      HOUSEHOLD_INCOME_GROUP = c(
        "HOUSEHOLD_INCOME_30BELOW" = "<30",
        "HOUSEHOLD_INCOME_30TO50" = "30–50",
        "HOUSEHOLD_INCOME_50TO100" = "50–100",
        "HOUSEHOLD_INCOME_100TO200" = "100–200",
        "HOUSEHOLD_INCOME_200UP" = ">200"
      ),
      STATE_EXPANDED_MEDICAID = c(
        "FALSE" = "Non Expansion State",
        "TRUE" = "Expansion State"
      )
    )
  ) +
  labs(
    title = "Uncompensated Cost vs Household Income Cohort Proportions",
    x = "Proportion of Households in Cohort",
    y = "Uncompensated Cost Ratio"
  ) +
  scale_y_continuous(limits = c(0, 0.5)) +
  scale_x_continuous(limits = c(0, 0.75)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

#### Plots for Presentation ####

df %>% 
  ggplot(aes(x = CST_CHRG)) +
  geom_histogram(binwidth = 0.03, fill = "skyblue", color = "black") +
  labs(
    title = "Cost to Charge Ratio",
    x = "Cost to Charge Ratio",
    y = "Frequency"
  ) +
  scale_x_continuous(limits = c(0, 1)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center + bold
    axis.title = element_text(face = "bold"),               # Axis titles bold
    axis.text = element_text(face = "bold")                 # Axis tick labels bold
  )

df %>% 
  ggplot(aes(x = cool_prop)) +
  geom_histogram(binwidth = 0.01, fill = "skyblue", color = "black") +
  labs(
    title = "Proportion of Costs that are Uncompensated",
    x = "Uncompensated Cost Proportion",
    y = "Frequency"
  ) +
  scale_x_continuous(limits = c(0, 0.2)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )

# Poverty Rate vs. Uncomp Cost
ggplot(df, aes(x = POVERTY_RATE, y = cool_prop)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Poverty Rate vs Uncompensated Cost",
    x = "Poverty Rate",
    y = "Uncompensated Cost Proportion"
  ) +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )

ggplot(data, aes(x = POVERTY_RATE, y = PERCENT_UNCOMP_COST)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Poverty Rate vs Uncompensated Cost",
       x = "Poverty Rate",
       y = "Proportion of Costs that are Uncompensated") +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme_minimal()

# Poverty Rate vs. Cost to Charge Ratio
ggplot(data, aes(x = POVERTY_RATE, y = CST_CHRG)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "orange") +
  labs(title = "Poverty Rate vs Cost to Charge Ratio",
       x = "Poverty Rate",
       y = "Cost to Charge Ratio") +
  scale_x_continuous(limits = c(0, 0.4)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

# Income Per Capita vs Uncomp Cost
ggplot(df, aes(x = INCOME_PER_CAPITA, y = cool_prop)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Income Per Capita vs Uncompensated Cost",
    x = "Income Per Capita",
    y = "Uncompensated Cost Proportion"
  ) +
  scale_y_continuous(limits = c(0, 0.5)) +
  scale_x_continuous(limits = c(0, 100000)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )

ggplot(data, aes(x = INCOME_PER_CAPITA, y = (COST_UNCOMP/TOT_COST))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  labs(title = "Income Per Capita vs Uncompensated Cost",
       x = "Income Per Capita",
       y = "Proportion of Costs that are Uncompensated") +
  scale_y_continuous(limits = c(0, 0.5)) +
  scale_x_continuous(limits = c(0, 100000)) +
  theme_minimal()


ggplot(data, aes(x = INCOME_PER_CAPITA, y = ((TOT_UNCOMP_COST - COST_UNCOMP)/TOT_UNCOMP_COST))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  labs(title = "Income Per Capita vs Uncompensated Cost",
       x = "Income Per Capita",
       y = "Proportion of Costs that are Uncompensated") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 100000)) +
  theme_minimal() +
  facet_wrap(~STATE_EXPANDED_MEDICAID)


ggplot(df, aes(x = INCOME_PER_CAPITA, y = cool_prop)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Income Per Capita vs Uncompensated Cost",
    x = "Income Per Capita",
    y = "Uncompensated Cost Proportion"
  ) +
  scale_y_continuous(limits = c(0, 0.5)) +
  scale_x_continuous(limits = c(0, 100000)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    strip.text = element_text(face = "bold")  # Bold facet labels
  ) +
  facet_wrap(
    ~STATE_EXPANDED_MEDICAID,
    labeller = as_labeller(c(`FALSE` = "Not Expanded", `TRUE` = "Expanded"))
  )
data %>% # Histogram for Uncomp Cost
  ggplot(aes(x = TOT_COST)) +
  geom_histogram(binwidth = 10000000, fill = "skyblue", color = "white") +
  scale_x_continuous(limits = c(0, 10000000000)) +
  labs(title = "Cost to Charge Ratio",
       x = "Cost to Charge Ratio",
       y = "Frequency") +
  theme_minimal()



#### Linear Regression ####

model <- lm(prop_cost_uncomp ~ `17Below_Per` * `18-24_Per` * `25-44_Per` * `45-64_Per` * `65-84_Per`, data = data)

model2 <- lm(prop_cost_uncomp ~ `HOUSEHOLD_INCOME_30BELOW_PER` * `HOUSEHOLD_INCOME_30to50_PER` * `HOUSEHOLD_INCOME_50to100_PER` * `HOUSEHOLD_INCOME_100to200_PER`, data = data)

the_hyper_GOATED_model <- lm(prop_cost_uncomp ~ `17Below_Per` * `18-24_Per` * `25-44_Per` * `45-64_Per` * `65-84_Per` * `HOUSEHOLD_INCOME_30BELOW_PER` * `HOUSEHOLD_INCOME_30to50_PER` * `HOUSEHOLD_INCOME_50to100_PER` * `HOUSEHOLD_INCOME_100to200_PER`, data = data)

summary(model)

summary(model2)

summary(the_hyper_GOATED_model)

model_simple <- lm(prop_cost_uncomp ~ `17Below_Per` + `18-24_Per` + `25-44_Per` + `45-64_Per` + `65-84_Per` + 
                     `HOUSEHOLD_INCOME_30BELOW_PER` + `HOUSEHOLD_INCOME_30to50_PER` + 
                     `HOUSEHOLD_INCOME_50to100_PER` + `HOUSEHOLD_INCOME_100to200_PER`, data = data)
summary(model_simple)


model_interaction <- lm(prop_cost_uncomp ~ (`17Below_Per` + `18-24_Per` + `25-44_Per` + `45-64_Per` + `65-84_Per`) * 
                          (`HOUSEHOLD_INCOME_30BELOW_PER` + `HOUSEHOLD_INCOME_30to50_PER`), data = data)
summary(model_interaction)

vif(model_simple)

tree_model <- rpart(
  LINE30vsTOTCOST ~ 
    POP_17BELOW_PERCENT +
    POP_18_24_PERCENT +
    POP_25TO44_PERCENT +
    POP_45TO64_PERCENT +
    POP_65TO84_PERCENT +
    HOUSEHOLD_INCOME_30BELOW_PERCENT +
    HOUSEHOLD_INCOME_30TO50_PERCENT +
    HOUSEHOLD_INCOME_50TO100_PERCENT +
    HOUSEHOLD_INCOME_100TO200_PERCENT +
    STATE_EXPANDED_MEDICAID,
  data = data,
  method = "anova",
  control = rpart.control(cp = 0.005, minsplit = 10, minbucket = 5)
)

# Predict and evaluate
predictions <- predict(tree_model, data)
actual <- data$LINE30vsTOTCOST
rss <- sum((actual - predictions)^2)
tss <- sum((actual - mean(actual))^2)
r_squared <- 1 - rss / tss

cat("R-squared:", round(r_squared, 4), "\n")

# Plot tree
rpart.plot(tree_model, type = 3, fallen.leaves = TRUE, cex = 0.7)




#### Health Insurance Percents ####

df <- read_csv("data/COMBINED_DATA_10.csv")

#### Histogram of Total Health Insurance Coverage ####

df %>% 
  ggplot(aes(x = HI_COVERAGE_TOTAL)) +
  geom_histogram(fill = "#69b3a2", color = "white", bins = 30) +  # You can change `bins`
  labs(
    title = "Distribution of Total Health Insurance Coverage",
    x = "Total Health Insurance Coverage",
    y = "Count"
  ) +
  scale_x_continuous(limits = c(.90, 1)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#### Histogram of 1-Type of Health Insurance (All Ages) ####

df %>% 
  mutate(HI_COVERAGE_1HITYPE = HI_COVERAGE_19BELOW_1HITYPE +
           HI_COVERAGE_19TO34_1HITYPE + HI_COVERAGE_35TO64_1HITYPE + 
           HI_COVERAGE_65UP_1HITYPE) %>% 
  ggplot(aes(x = HI_COVERAGE_1HITYPE)) +
  geom_histogram(fill = "#69b3a2", color = "white", bins = 30) +  # You can change `bins`
  labs(
    title = "Distribution of Total Health Insurance Coverage",
    x = "Total Health Insurance Coverage",
    y = "Count"
  ) +
  scale_x_continuous(limits = c(.5, .9)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#### Histogram of Employer Based Health Insurance ####

df %>% 
  mutate(HI_COVERAGE_1HITYPE_EMPLOYERBASED = HI_COVERAGE_19BELOW_1HITYPE_EMPLOYERBASED +
           HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED + HI_COVERAGE_35TO64_1HITYPE_EMPLOYERBASED + 
           HI_COVERAGE_65UP_1HITYPE_EMPLOYERBASED) %>% 
  ggplot(aes(x = HI_COVERAGE_1HITYPE_EMPLOYERBASED)) +
  geom_histogram(fill = "#69b3a2", color = "white", bins = 30) +  # You can change `bins`
  labs(
    title = "Distribution of Total Health Insurance Coverage",
    x = "Total Health Insurance Coverage",
    y = "Count"
  ) +
  scale_x_continuous(limits = c(.15, .8)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#### Histogram of Medicaid Health Insurance ####

df %>% 
  mutate(HI_COVERAGE_1HITYPE_MEDICAID = HI_COVERAGE_19BELOW_1HITYPE_MEDICAID +
           HI_COVERAGE_19TO34_1HITYPE_MEDICAID + HI_COVERAGE_35TO64_1HITYPE_MEDICAID) %>% 
  ggplot(aes(x = HI_COVERAGE_1HITYPE_MEDICAID)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +  # You can change `bins`
  labs(
    title = "Distribution of Medicaid Proportions",
    x = "Proportion of Population on Medicaid",
    y = "Count"
  ) +
  scale_x_continuous(limits = c(0, .5)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#### Scatter Plots of Proportion of Population with each type of Health Insurance Based on Age ####

# Step 1: Create proportion of uncompensated care
df <- df %>%
  mutate(`30/202` = COST_UNCOMP / TOT_COST) %>% 
  mutate(`31/202` = TOT_UNCOMP_COST / TOT_COST)


df <- df %>%
  rename(cool_prop = `30/202`)

# Step 2: Select only relevant insurance columns
insurance_cols <- names(df)[
  (
    str_detect(names(df), "^HI_COVERAGE.*1HITYPE_") &
      !str_detect(names(df), "^HI_COVERAGE_\\d+TO\\d+_1HITYPE$|^HI_COVERAGE_\\d+BELOW_1HITYPE$|^HI_COVERAGE_\\d+UP_1HITYPE$")
  ) |
    str_detect(names(df), "^HI_COVERAGE_(19BELOW|19TO34|35TO64|65UP)_NOINSURANCE$")
]

# Step 3: Pivot to long format, extract age group and insurance type
long_df <- df %>%
  select(PROVIDER_CCN, cool_prop, `31/202`, all_of(insurance_cols)) %>%
  pivot_longer(
    cols = all_of(insurance_cols),
    names_to = "insurance_col",
    values_to = "coverage_rate"
  ) %>%
  mutate(
    age_group = str_extract(insurance_col, "(19BELOW|\\d+TO\\d+|65UP)"),
    insurance_type = case_when(
      str_detect(insurance_col, "1HITYPE_") ~ str_extract(insurance_col, "1HITYPE_.*") %>%
        str_remove("1HITYPE_") %>%
        str_to_upper(),
      str_detect(insurance_col, "NOINSURANCE") ~ "NOINSURANCE",
      TRUE ~ NA_character_
    ),
    insurance_type = case_when(
      insurance_type %in% c("TRICARE", "VACARE") ~ "MILITARYCARE",
      TRUE ~ insurance_type
    )
  ) %>%
  filter(!is.na(coverage_rate), !is.na(`31/202`), !is.na(age_group), !is.na(insurance_type))
# Step 4: Plot
ggplot(long_df, aes(x = coverage_rate, y = cool_prop)) +
  geom_point(alpha = 0.6, color = "#0073C2") +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.8) +
  stat_poly_eq(
    aes(label = after_stat(rr.label)),
    formula = y ~ x,
    parse = TRUE,
    size = 3.5,
    npcx = 0.95,      # Right side
    npcy = 0.5,       # Middle vertically
    color = "black"
  ) +
  facet_grid(rows = vars(age_group), cols = vars(insurance_type)) +
  labs(
    title = "Uncompensated Care vs Insurance Coverage",
    x = "Insurance Coverage Rate",
    y = "Uncompensated Care Proportion"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

#### Actually Good Linear Model ####

model <- lm(cool_prop ~ `HI_COVERAGE_35TO64_NOINSURANCE`, data = df)

summary(model)

#### Decision Tree ####

data_clean <- df %>% 
  rename(
    Uninsured_35To64 = HI_COVERAGE_35TO64_NOINSURANCE,
    Uninsured_19Below = HI_COVERAGE_19BELOW_NOINSURANCE,
    Uninsured_19To34 = HI_COVERAGE_19TO34_NOINSURANCE,
    Uninsured_65Up = HI_COVERAGE_65UP_NOINSURANCE,
    Medicaid_35To64 = HI_COVERAGE_35TO64_1HITYPE_MEDICAID,
    Medicaid_19To34 = HI_COVERAGE_19TO34_1HITYPE_MEDICAID,
    EmployerBased_19To34 = HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED,
    Medicaid_19Below = HI_COVERAGE_19BELOW_1HITYPE_MEDICAID,
    Expanded_Medicaid = STATE_EXPANDED_MEDICAID
  )

tree_model <- rpart(
  cool_prop ~ 
    Uninsured_35To64 +
    Uninsured_19Below +
    Uninsured_19To34 +
    Uninsured_65Up +
    Medicaid_19To34 +
    Medicaid_19To34 +
    EmployerBased_19To34 +
    Medicaid_19Below +
    Expanded_Medicaid,
  data = data_clean,
  method = "anova",
  control = rpart.control(cp = 0.008, minsplit = 10, minbucket = 5)
)

# Predict and evaluate
predictions <- predict(tree_model, data_clean)
actual <- data_clean$cool_prop
rss <- sum((actual - predictions)^2)
tss <- sum((actual - mean(actual))^2)
r_squared <- 1 - rss / tss

cat("R-squared:", round(r_squared, 4), "\n")

# Plot tree
rpart.plot(
  tree_model,
  main = "Decision Tree for Uncompensated Costs",  # ✅ TITLE
  type = 4,                                 # Labels below boxes
  extra = 101,                              # ✅ Class + Prob + Percent
  box.palette = "Blues",                    # Color scheme
  fallen.leaves = TRUE,
  cex = 0.7)

#### Random Forest ####

data_rf <- df %>% 
  select(cool_prop,HI_COVERAGE_35TO64_NOINSURANCE,
         HI_COVERAGE_19BELOW_NOINSURANCE, HI_COVERAGE_19TO34_NOINSURANCE,
         HI_COVERAGE_19TO34_1HITYPE_MEDICAID,
         HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED,
         HI_COVERAGE_19BELOW_1HITYPE_MEDICAID,STATE_EXPANDED_MEDICAID)

data_rf <- data_rf %>%
  filter(!(HI_COVERAGE_35TO64_NOINSURANCE %in% NA))

rf_model <- randomForest(
  cool_prop ~ 
    HI_COVERAGE_35TO64_NOINSURANCE +
    HI_COVERAGE_19BELOW_NOINSURANCE +
    HI_COVERAGE_19TO34_NOINSURANCE +
    HI_COVERAGE_19TO34_1HITYPE_MEDICAID +
    HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED +
    HI_COVERAGE_19BELOW_1HITYPE_MEDICAID +
    STATE_EXPANDED_MEDICAID,
  data = data_rf,
  ntree = 500,
  mtry = 3,
  importance = TRUE
)

# Predict and evaluate
predictions <- predict(rf_model, data_rf)
actual <- data_rf$cool_prop
rss <- sum((actual - predictions)^2)
tss <- sum((actual - mean(actual))^2)
r_squared <- 1 - rss / tss

cat("Random Forest R-squared:", round(r_squared, 4), "\n")

#### Testing the RF Model ####

# Predict on your data set
predictions <- predict(rf_model, newdata = data_rf)

# Create a comparison data frame
comparison_df <- data.frame(
  actual = data_rf$cool_prop,
  predicted = predictions
)

comparison_df <- comparison_df %>% 
  mutate(residuals = (predicted - actual)/actual)

comparison_df %>% 
  ggplot(aes(x = residuals)) +
  geom_histogram(fill = "#69b3a2", color = "white", bins = 30) +
  labs(
    title = "Distribution of Total Health Insurance Coverage",
    x = "Total Health Insurance Coverage",
    y = "Count"
  ) +
  scale_x_continuous(limits = c(0, 2)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#### RF Function for App ####

library(tidyverse)
library(randomForest)

set.seed(167)

df <- read_csv("data/COMBINED_DATA_10.csv")

df <- df %>%
  mutate(`30/202` = COST_UNCOMP / TOT_COST) %>% 
  mutate(`31/202` = TOT_UNCOMP_COST / TOT_COST)

df <- df %>%
  rename(cool_prop = `30/202`)

data_rf <- df %>% 
  select(cool_prop,HI_COVERAGE_35TO64_NOINSURANCE,
         HI_COVERAGE_19BELOW_NOINSURANCE, HI_COVERAGE_19TO34_NOINSURANCE,
         HI_COVERAGE_19TO34_1HITYPE_MEDICAID,
         HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED,
         HI_COVERAGE_19BELOW_1HITYPE_MEDICAID,STATE_EXPANDED_MEDICAID,
         HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE,
         HI_COVERAGE_35TO64_1HITYPE_MEDICAID,
         HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID)

data_rf <- data_rf %>%
  filter(!(HI_COVERAGE_35TO64_NOINSURANCE %in% NA))

rf_model <- randomForest(
  cool_prop ~ 
    HI_COVERAGE_35TO64_NOINSURANCE +
    HI_COVERAGE_19BELOW_NOINSURANCE +
    HI_COVERAGE_19TO34_NOINSURANCE +
    HI_COVERAGE_19TO34_1HITYPE_MEDICAID +
    HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED +
    HI_COVERAGE_19BELOW_1HITYPE_MEDICAID +
    HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE +
    HI_COVERAGE_35TO64_1HITYPE_MEDICAID +
    HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID +
    STATE_EXPANDED_MEDICAID,
  data = data_rf,
  ntree = 500,
  mtry = 3,
  importance = TRUE
)

predict_uncomp_cost_rf <- function(ccn = 360003)
  {
  
  target_row <- df[df$PROVIDER_CCN == ccn, ]
  
  variables_for_rf <- data.frame(
    HI_COVERAGE_35TO64_NOINSURANCE = target_row$HI_COVERAGE_35TO64_NOINSURANCE,
    HI_COVERAGE_19BELOW_NOINSURANCE = target_row$HI_COVERAGE_19BELOW_NOINSURANCE,
    HI_COVERAGE_19TO34_NOINSURANCE = target_row$HI_COVERAGE_19TO34_NOINSURANCE,
    HI_COVERAGE_19TO34_1HITYPE_MEDICAID = target_row$HI_COVERAGE_19TO34_1HITYPE_MEDICAID,
    HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED = target_row$HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED,
    HI_COVERAGE_19BELOW_1HITYPE_MEDICAID = target_row$HI_COVERAGE_19BELOW_1HITYPE_MEDICAID,
    HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE = target_row$HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE,
    HI_COVERAGE_35TO64_1HITYPE_MEDICAID = target_row$HI_COVERAGE_35TO64_1HITYPE_MEDICAID,
    STATE_EXPANDED_MEDICAID = target_row$STATE_EXPANDED_MEDICAID,
    HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID = target_row$HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID
  )
  
  predicted_proportion <- predict(rf_model, newdata = variables_for_rf)
  
  return(predicted_proportion)
  
}





predict_uncomp_cost_rf_BBB <- function(ccn = 360003,HI_35TO64_MEDICAID = "50", 
                                       HI_19TO34_MEDICAID = "50", 
                                       HI_19TO34_DIRECT_PURCHASE = "60")
{

  HI_35TO64_MEDICAID <- (as.numeric(HI_35TO64_MEDICAID)/100)
  HI_19TO34_MEDICAID <- (as.numeric(HI_19TO34_MEDICAID)/100)
  HI_19TO34_DIRECT_PURCHASE <- (as.numeric(HI_19TO34_DIRECT_PURCHASE)/100)
  
  HI_MEDICAREANDMEDICAID <- -1

  target_row <- df[df$PROVIDER_CCN == ccn, ]
  
  variables_for_rf <- data.frame(
    HI_COVERAGE_35TO64_NOINSURANCE = target_row$HI_COVERAGE_35TO64_NOINSURANCE,
    HI_COVERAGE_19BELOW_NOINSURANCE = target_row$HI_COVERAGE_19BELOW_NOINSURANCE,
    HI_COVERAGE_19TO34_NOINSURANCE = target_row$HI_COVERAGE_19TO34_NOINSURANCE,
    HI_COVERAGE_19TO34_1HITYPE_MEDICAID = target_row$HI_COVERAGE_19TO34_1HITYPE_MEDICAID,
    HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED = target_row$HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED,
    HI_COVERAGE_19BELOW_1HITYPE_MEDICAID = target_row$HI_COVERAGE_19BELOW_1HITYPE_MEDICAID,
    HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE = target_row$HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE,
    HI_COVERAGE_35TO64_1HITYPE_MEDICAID = target_row$HI_COVERAGE_35TO64_1HITYPE_MEDICAID,
    STATE_EXPANDED_MEDICAID = target_row$STATE_EXPANDED_MEDICAID,
    HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID = target_row$HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID
  )
  
  variables_for_rf <- variables_for_rf %>%
    mutate(HI_COVERAGE_35TO64_NOINSURANCE =  HI_COVERAGE_35TO64_NOINSURANCE - (HI_35TO64_MEDICAID *  HI_COVERAGE_35TO64_1HITYPE_MEDICAID) 
           - (HI_MEDICAREANDMEDICAID * HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID)) %>%
    mutate(HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID = HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID * (1 - HI_MEDICAREANDMEDICAID)) %>%
    mutate(HI_COVERAGE_35TO64_1HITYPE_MEDICAID = HI_COVERAGE_35TO64_1HITYPE_MEDICAID * (1 - HI_35TO64_MEDICAID))%>%
    mutate(HI_COVERAGE_19TO34_NOINSURANCE = HI_COVERAGE_19TO34_NOINSURANCE - (HI_COVERAGE_19TO34_1HITYPE_MEDICAID * HI_19TO34_MEDICAID) 
           - (HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE * HI_19TO34_DIRECT_PURCHASE)) %>%
    mutate(HI_COVERAGE_19TO34_1HITYPE_MEDICAID =  HI_COVERAGE_19TO34_1HITYPE_MEDICAID * (1 - HI_19TO34_MEDICAID)) %>%
    mutate(HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE = HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE * (1 - HI_19TO34_DIRECT_PURCHASE))

  predicted_proportion <- predict(rf_model, newdata = variables_for_rf)
  
  return(predicted_proportion)
  
}

predict_uncomp_cost_rf()
predict_uncomp_cost_rf_BBB()







