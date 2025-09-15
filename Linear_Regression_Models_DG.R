library(tidyverse)
library(dplyr)
library(tidycensus)
library(rsq)



#the data being used for each model and regression
WORKING_DATA <- read_csv("data/COMBINED_DATA_10.CSV")


#run the first two hashtag things

#### MODEL WITH ONLY SIGNIFICANT VALUES ####


# TURN STATE EXPANDED MEDICAID INTO A USEABLE VALUE
if (!is.factor(WORKING_DATA$STATE_EXPANDED_MEDICAID) ||
    !all(levels(WORKING_DATA$STATE_EXPANDED_MEDICAID) %in% c("NOTEXPANDED", "EXPANDED"))) {
  
  # Ensure numeric version exists or is correctly set
  WORKING_DATA$STATE_EXPANDED_MEDICAID_NUM <- ifelse(
    WORKING_DATA$STATE_EXPANDED_MEDICAID %in% c(TRUE, 1), 1,
    ifelse(WORKING_DATA$STATE_EXPANDED_MEDICAID %in% c(FALSE, 0), 0, NA)
  )
  
  # Convert to factor with labels
  WORKING_DATA$STATE_EXPANDED_MEDICAID <- factor(
    WORKING_DATA$STATE_EXPANDED_MEDICAID_NUM,
    levels = c(0, 1),
    labels = c("NOTEXPANDED", "EXPANDED")
  )
}


if (!is.factor(WORKING_DATA$MEDICAID_WORK_REQUIREMENT) ||
    !all(levels(WORKING_DATA$MEDICAID_WORK_REQUIREMENT) %in% c("NOT-REQUIRED", "REQUIRED"))) {
  
  WORKING_DATA$MEDICAID_WORK_REQUIREMENT <- ifelse(
    WORKING_DATA$MEDICAID_WORK_REQUIREMENT %in% c(TRUE, 1), 1,
    ifelse(WORKING_DATA$MEDICAID_WORK_REQUIREMENT %in% c(FALSE, 0), 0, NA)
  )
  
  WORKING_DATA$MEDICAID_WORK_REQUIREMENT <- factor(
    WORKING_DATA$MEDICAID_WORK_REQUIREMENT,
    levels = c(0, 1),
    labels = c("NOT-REQUIRED", "REQUIRED")
  )
}

unique(WORKING_DATA$STATE_EXPANDED_MEDICAID)
str(WORKING_DATA$STATE_EXPANDED_MEDICAID)
unique(WORKING_DATA$MEDICAID_WORK_REQUIREMENT)
str(WORKING_DATA$MEDICAID_WORK_REQUIREMENT)

# FORMULA
significant_values_model_formula <- (COST_UNCOMP/TOT_COST) ~
  HI_COVERAGE_65UP_1HITYPE_DIRECTPURCHASE +
  HI_COVERAGE_65UP_2UPHITYPE_DIRECTANDMEDICARE +
  HI_COVERAGE_35TO64_1HITYPE_MEDICAID +
  HI_COVERAGE_35TO64_1HITYPE_TRICARE +
  HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID +
  HI_COVERAGE_35TO64_2UPHITYPE_PRIVATEONLY +
  HI_COVERAGE_35TO64_2UPHITYPE_PUBLICONLY + 
  HI_COVERAGE_35TO64_2UPHITYPE_OTHERCOVERAGE +
  HI_COVERAGE_35TO64_NOINSURANCE +
  HI_COVERAGE_19TO34_NOINSURANCE +
  HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED +
  HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE +
  HI_COVERAGE_19TO34_1HITYPE_MEDICAID +
  STATE_EXPANDED_MEDICAID +  
  MEDICAID_WORK_REQUIREMENT

#  + HOUSEHOLD_INCOME_50TO100_PERCENT + HOUSEHOLD_INCOME_100TO200_PERCENT +
# HOUSEHOLD_INCOME_30BELOW_PERCENT

# MODEL 
glm_model_for_significan_values <- glm(
  formula = significant_values_model_formula,
  family = gaussian(link = "identity"),
  data = WORKING_DATA
)

# VIEW RESULTS
summary(glm_model_for_significan_values)

rsq(glm_model_for_significan_values)









#####  DG - trying Ridge, Lasso, and elastic net linear regression #####

# Load glmnet
library(glmnet)

# Predictor matrix (drop non-numeric or irrelevant variables)
# This selects all columns except the response
predictor_vars <- WORKING_DATA[,c(57:121,137:138)]

good_rows <- complete.cases(predictor_vars, WORKING_DATA$COST_UNCOMP, WORKING_DATA$TOT_COST)

# Filter both x and y using the same rows
x <- model.matrix(~ ., data = predictor_vars[good_rows, ])[, -1]
y <- WORKING_DATA$COST_UNCOMP[good_rows] / WORKING_DATA$TOT_COST[good_rows]

set.seed(123)  # reproducibility
cv_fit <- cv.glmnet(x, y, alpha = .5)  # alpha = 0.5 = elastic net

best_lambda <- cv_fit$lambda.min
enet_model <- glmnet(x, y, alpha = .5, lambda = best_lambda)

coef(enet_model)

y_pred <- as.vector(predict(enet_model, newx = x))

rmse <- sqrt(mean((y - y_pred)^2))

sst <- sum((y - mean(y))^2)
sse <- sum((y - y_pred)^2)
r_squared <- 1 - sse / sst

rmse
r_squared

residuals_enet <- y - y_pred

plot(y_pred, residuals_enet,
     xlab = "Predicted Values",
     ylab = "Residuals",
     main = "Residual Plot - Elastic Net",
     pch = 20, col = "blue")
abline(h = 0, col = "red", lty = 2)

#####  DG - trying beta regression, since our response variable is a proportion #####

# Install and load required package
library(betareg)

# Step 1: Prepare the response variable (bounded between 0 and 1)
epsilon <- 1e-5
WORKING_DATA$y <- with(WORKING_DATA, COST_UNCOMP / TOT_COST)
WORKING_DATA$y <- pmin(pmax(WORKING_DATA$y, epsilon), 1 - epsilon)

# Step 2: Define the model formula using your predictors
model_formula <- y ~ 
  HI_COVERAGE_65UP +
  HI_COVERAGE_65UP_1HITYPE_EMPLOYERBASED +
  HI_COVERAGE_65UP_1HITYPE_DIRECTPURCHASE +
  HI_COVERAGE_65UP_1HITYPE_MEDICARE +
  HI_COVERAGE_65UP_1HITYPE_TRICARE +
  HI_COVERAGE_65UP_1HITYPE_VACARE +
  HI_COVERAGE_65UP_2UPHITYPE_EMPLOYERANDMEDICARE +
  HI_COVERAGE_65UP_2UPHITYPE_DIRECTANDMEDICARE +
  HI_COVERAGE_65UP_2UPHITYPE_PRIVATEONLY +
  HI_COVERAGE_65UP_2UPHITYPE_PUBLICONLY + 
  HI_COVERAGE_65UP_2UPHITYPE_OTHERCOVERAGE +
  HI_COVERAGE_65UP_NOINSURANCE +
  HI_COVERAGE_35TO64 +
  HI_COVERAGE_35TO64_1HITYPE_EMPLOYERBASED +
  HI_COVERAGE_35TO64_1HITYPE_DIRECTPURCHASE +
  HI_COVERAGE_35TO64_1HITYPE_MEDICARE +
  HI_COVERAGE_35TO64_1HITYPE_MEDICAID +
  HI_COVERAGE_35TO64_1HITYPE_TRICARE +
  HI_COVERAGE_35TO64_1HITYPE_VACARE +
  HI_COVERAGE_35TO64_2UPHITYPE_EMPLOYERANDMEDICARE +
  HI_COVERAGE_35TO64_2UPHITYPE_DIRECTANDMEDICARE +
  HI_COVERAGE_35TO64_2UPHITYPE_MEDICAREANDMEDICAID +
  HI_COVERAGE_35TO64_2UPHITYPE_PRIVATEONLY +
  HI_COVERAGE_35TO64_2UPHITYPE_PUBLICONLY + 
  HI_COVERAGE_35TO64_2UPHITYPE_OTHERCOVERAGE +
  HI_COVERAGE_35TO64_NOINSURANCE +
  HI_COVERAGE_19TO34 +
  HI_COVERAGE_19TO34_1HITYPE_EMPLOYERBASED +
  HI_COVERAGE_19TO34_1HITYPE_DIRECTPURCHASE +
  HI_COVERAGE_19TO34_1HITYPE_MEDICARE +
  HI_COVERAGE_19TO34_1HITYPE_MEDICAID +
  HI_COVERAGE_19TO34_1HITYPE_TRICARE +
  HI_COVERAGE_19TO34_1HITYPE_VACARE +
  HI_COVERAGE_19TO34_2UPHITYPE_EMPLOYERANDMEDICARE +
  HI_COVERAGE_19TO34_2UPHITYPE_MEDICAREANDMEDICAID +
  HI_COVERAGE_19TO34_2UPHITYPE_PRIVATEONLY +
  HI_COVERAGE_19TO34_2UPHITYPE_PUBLICONLY + 
  HI_COVERAGE_19TO34_2UPHITYPE_OTHERCOVERAGE +
  HI_COVERAGE_19TO34_NOINSURANCE +
  STATE_EXPANDED_MEDICAID +  
  MEDICAID_WORK_REQUIREMENT

# Step 3: Remove rows with any NA values used in the model
vars_used <- all.vars(significant_values_model_formula)
working_data_clean <- WORKING_DATA[complete.cases(WORKING_DATA[, vars_used]), ]

# Step 4: Fit the beta regression model
beta_model <- betareg(significant_values_model_formula, data = working_data_clean)

# Step 5: View model summary
summary(beta_model)


# Histogram
hist(residuals(beta_model, type = "quantile"), main = "Quantile Residuals", breaks = 30)

# QQ plot
qqnorm(residuals(beta_model, type = "quantile"))
qqline(residuals(beta_model, type = "quantile"), col = "red")


fitted_vals <- fitted(beta_model)
resids <- residuals(beta_model, type = "response")

hist(quantile_resids,xlim = c(-.1,.1),breaks=50)

# Plot residuals vs fitted values
plot(WORKING_DATA$CASE_AMOUNT[complete.cases(WORKING_DATA[, vars_used])], resids,
     xlab = "Fitted Values",
     ylab = "Quantile Residuals",
     main = "Residual Plot - Beta Regression")
abline(h = 0, col = "red", lty = 2)





#### Trying PCR - principle component regression ####

# Select predictor columns (adjust indices as needed)
predictor_vars <- WORKING_DATA[,c(56:121,123:129)]

# Remove columns with zero variance (constant columns)
nzv_cols <- sapply(predictor_vars, function(col) var(col, na.rm = TRUE) == 0)
predictor_vars_filtered <- predictor_vars[, !nzv_cols]

# Remove rows with NA or infinite values in predictors
rows_to_keep <- apply(predictor_vars_filtered, 1, function(row) all(!is.na(row) & !is.infinite(row)))
predictor_vars_clean <- predictor_vars_filtered[rows_to_keep, ]

# Scale predictors
predictor_scaled <- scale(predictor_vars_clean)

# Run PCA
pca_res <- prcomp(predictor_scaled, center = TRUE, scale. = TRUE)

# Check PCA result summary
summary(pca_res)

# Subset response variable to match cleaned predictor rows
y_clean <- WORKING_DATA$COST_UNCOMP / WORKING_DATA$TOT_COST
y_clean <- y_clean[rows_to_keep]

# Decide how many PCs to keep (e.g., enough to explain 90% variance)
explained_var <- summary(pca_res)$importance[3, ]
num_pcs <- which(cumsum(explained_var) >= 0.9)[1]

# Get the selected PCs scores
pc_scores <- pca_res$x[, 1:num_pcs]

# Fit linear regression of response on selected PCs
pc_model <- lm(y_clean ~ ., data = as.data.frame(pc_scores))

summary(pc_model)
