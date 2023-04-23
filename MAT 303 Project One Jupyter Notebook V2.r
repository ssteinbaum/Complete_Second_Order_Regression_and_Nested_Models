
housing <- read.csv(file="housing_v2.csv", header=TRUE, sep=",")

# converting appropriate variables to factors  
housing <- within(housing, {
   view <- factor(view)
   backyard <- factor(backyard)
})

# number of columns
ncol(housing)

# number of rows
nrow(housing)

plot(housing$sqft_living, housing$price, 
     main = "Scatterplot of Price vs. Living Area",
     xlab = "Living Area", ylab = "Price",
     xlim=c(0, 5000),
     ylim=c(0, 1500000),
     col="red", 
     pch = 19, frame = FALSE)

plot(housing$age, housing$price, 
     main = "Scatterplot of Price vs. Age of Home",
     xlab = "Age of Home", ylab = "Price",
     xlim=c(0, 120),
     ylim=c(0, 1500000),
     col="red", 
     pch = 19, frame = FALSE)

# Selecting price, sqft_living, and age variables to subset the data
myvars <- c("price","sqft_living","age")
housing_subset <- housing[myvars]

# Print the first 10 rows
print("head")
head(housing_subset, 10)

# Print the correlation matrix
print("cor")
corr_matrix <- cor(housing_subset, method = "pearson")
round(corr_matrix, 4)

myvars <- c("price","sqft_living","sqft_above","age","bathrooms","view")
housing_subset <- housing[myvars]

# Create the model
model1 <- lm(price ~ sqft_living + sqft_above + age + bathrooms + view, data=housing_subset)
summary(model1)

# predicted values
print("fitted")
fitted_values <- fitted.values(model1) 
fitted_values

# residuals
print("residuals")
residuals <- residuals(model1)
residuals

plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     col="red", 
     pch = 19, frame = FALSE)

qqnorm(residuals, pch = 19, col="red", frame = FALSE)
qqline(residuals, col = "blue", lwd = 2)

newdata <- data.frame(sqft_living=2150, sqft_above=1050, age=15, bathrooms=3, view='0')

print("prediction interval")
prediction_pred_int <- predict(model1, newdata, interval="predict", level=0.90) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model1, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int, 4)

newdata <- data.frame(sqft_living=4250, sqft_above=2100, age=5, bathrooms=5, view='2')

print("prediction interval")
prediction_pred_int <- predict(model1, newdata, interval="predict", level=0.90) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model1, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int, 4)

plot(housing$school_rating, housing$price, 
     main = "Scatterplot of Price vs. Average School Rating",
     xlab = "School Rating", ylab = "Price",
     col="red", 
     pch = 19, frame = FALSE)

plot(housing$crime, housing$price, 
     main = "Scatterplot of Price vs. The Crime Rate per 100,000 People",
     xlab = "Crime Rate", ylab = "Price",
     col="red", 
     pch = 19, frame = FALSE)

# Create the second order regression model and print the statistics
model2 <- lm(price ~ school_rating + crime + school_rating:crime + I(school_rating^2) + I(crime^2) , data=housing)
summary(model2)

# predicted values
print("fitted")
fitted_values <- fitted.values(model2) 
fitted_values

# residuals
print("residuals")
residuals <- residuals(model2)
residuals

plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     col="red", 
     pch = 19, frame = FALSE)

qqnorm(residuals, pch = 19, col="red", frame = FALSE)
qqline(residuals, col = "blue", lwd = 2)

newdata <- data.frame(school_rating=9.80, crime=81.02)

print("prediction interval")
prediction_pred_int <- predict(model2, newdata, interval="predict", level=0.90) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model2, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int, 4)

newdata <- data.frame(school_rating=4.28, crime=215.50)

print("prediction interval")
prediction_pred_int <- predict(model2, newdata, interval="predict", level=0.90) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model2, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int, 4)

model3 <- lm(price ~ school_rating + crime + school_rating:crime, data=housing)
summary(model3)

# Create the complete model
fit_complete <- lm(price ~ school_rating + crime + school_rating:crime + I(school_rating^2) + I(crime^2), data=housing_subset)

# Create the reduced model
fit_reduced <- lm(price ~ school_rating + crime + school_rating:crime, data=housing_subset)

# Perform the F-test
anova(fit_complete, fit_reduced)
