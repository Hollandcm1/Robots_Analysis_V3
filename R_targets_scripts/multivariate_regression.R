# multivariate_regression

# Load necessary library
library(MASS)

# Assume 'data' is your dataset and DV1, DV2 are the dependent variables
# IV1, IV2 are independent variables
model <- lm(cbind(DV1, DV2) ~ IV1 + IV2, data=data)

# Summary of the multivariate regression model
summary(model)