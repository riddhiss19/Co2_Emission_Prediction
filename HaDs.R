library(tidyr)
library(dplyr)
library(car)
library(class)
library(ggplot2)
library(reshape2)
library(caret)

data <-  read.csv("fuel.csv")
head(data)


###########################################################################################
###########################################################################################
###########################################################################################


# Check for missing values
is.na(data)
#missing values
sapply(data, function(x) sum(is.na(x)))

# Remove rows with missing values
clean_data <- na.omit(data)
count(clean_data)

# Replace missing values with a specific value
clean_data <- data %>% 
  replace_na(list(column_name = 0))

# Identify duplicate rows
duplicated_rows <- duplicated(data)
sum(duplicated_rows)

# Visualize and identify 
par(mfrow=c(2,3))
boxplot(data$CO2EMISSIONS, main="CO2Emission")
#boxplot(data$VEHICLECLASS, main="VEHICALCLASS")
boxplot(data$ENGINESIZE, main="ENGINSIZW")
boxplot(data$CYLINDERS, main="CYLINDERS")
boxplot(data$FUELCONSUMPTION_COMB_MPG, main="FUELCONSUMPTION_COMB_MPG")

# Identify outliers in the ENGINESIZE column using the interquartile range method
Q1 <- quantile(data$ENGINESIZE, 0.25)
Q3 <- quantile(data$ENGINESIZE, 0.75)
IQR <- Q3 - Q1
lower <- Q1 - 1.5 * IQR
upper <- Q3 + 1.5 * IQR

# Remove rows with outliers
data <- data[data$ENGINESIZE >= lower & data$ENGINESIZE <= upper, ]
boxplot(data$ENGINESIZE, main="After Removing Outliers ENGINESIZE")

# Identify outliers in the FUELCONSUMPTION_COMB_MPG column using the interquartile range method
Q1 <- quantile(data$FUELCONSUMPTION_COMB_MPG, 0.25)
Q3 <- quantile(data$FUELCONSUMPTION_COMB_MPG, 0.75)
IQR <- Q3 - Q1
lower <- Q1 - 1.5 * IQR
upper <- Q3 + 1.5 * IQR

# Remove rows with outliers
data <- data[data$FUELCONSUMPTION_COMB_MPG >= lower & data$FUELCONSUMPTION_COMB_MPG <= upper, ]
boxplot(data$FUELCONSUMPTION_COMB_MPG, main="outliers removed forFUELCONSUMPTION_COMB_MPG")





###########################################################################################
###########################################################################################
###########################################################################################


#1 #2
# Check correlations between variables
num_cols <- data[, c("ENGINESIZE", "CYLINDERS", "FUELCONSUMPTION_CITY", "FUELCONSUMPTION_HWY",
                     "FUELCONSUMPTION_COMB", "FUELCONSUMPTION_COMB_MPG", "CO2EMISSIONS")]
cor_matrix <- cor(num_cols)
print(cor_matrix)

# Select columns of interest
cols <- c("ENGINESIZE", "CYLINDERS", "FUELCONSUMPTION_COMB", "CO2EMISSIONS")
data_subset <- data[, cols]

# Calculate correlation matrix
cor_matrix <- cor(data_subset)

# Plot correlation matrix as a heatmap with values inside each box
dev.off()
ggplot(data = melt(cor_matrix), aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + 
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1,1)) +
  labs(title = "Correlation Matrix",
       x = "", y = "",
       fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cor_df <- data.frame(variable_1 = rep(cols, times = length(cols)),
                     variable_2 = rep(cols, each = length(cols)),
                     correlation = as.vector(cor_matrix))

# Display correlation coefficients in a table
cor_df

###########################################################################################
###########################################################################################
###########################################################################################


#3. Determining what is the amount of CO2 emission depending on Vehicle class, Engine size, Cylinders, Fuel consumption, etc.

# Set plot size and font size


# FUELCONSUMPTION_COMB vs emisson
ggplot(data, aes(x=FUELCONSUMPTION_COMB, y=CO2EMISSIONS, color=CYLINDERS)) +
  geom_point() +
  labs(x="FUELCONSUMPTION_COMB", y="Emission", color="FUELCONSUMPTION_COMB")
head(data[c("FUELCONSUMPTION_COMB", "CO2EMISSIONS")])

#engineSize
ggplot(data, aes(x=ENGINESIZE, y=CO2EMISSIONS, color=ENGINESIZE)) +
  geom_point(size=3) +
  labs(x="Engine size", y="Emission", color="Engine size") +
  scale_color_gradient(low="red", high="blue", limits=c(1, 6))
head(data[c("ENGINESIZE", "CO2EMISSIONS")])

#Vechical Class
cdf_top10 <- data %>%
  filter(VEHICLECLASS %in% c("COMPACT", "SUV - SMALL", "MID-SIZE", "MINICOMPACT", "SUBCOMPACT", 
                             "TWO-SEATER", "FULL-SIZE", "VAN - PASSENGER", "SUV - STANDARD", 
                             "VAN - CARGO"))
# Plot a bar plot of CO2 emissions by vehicle class
ggplot(cdf_top10, aes(x=VEHICLECLASS, y=CO2EMISSIONS)) +
  geom_bar(stat="summary", fun=mean, fill="lightblue", color="black") +
  labs(x="Vehicle class", y="Mean CO2 Emissions")

#cylinder
ggplot(data, aes(x=CYLINDERS, y=CO2EMISSIONS)) +
  geom_bar(stat="identity", fill="lightblue", color="black") +
  labs(x="Cylinders", y="CO2 Emissions")


###########################################################################################
###########################################################################################
###########################################################################################



#linear Regression
df2 <- data[, c("ENGINESIZE", "CO2EMISSIONS")]
head(df2)

x <- df2[, "ENGINESIZE"]   # Feature
y <- df2[, "CO2EMISSIONS"] # target


ggplot(df2, aes(x = ENGINESIZE, y = CO2EMISSIONS, color = CO2EMISSIONS)) + 
  geom_point() +
  labs(title = "Scatter plot of Enginesize vs CO2 Emissions",
       x = "ENGINESIZE", y = "CO2 Emissions", color = "CO2 Emissions") +
  theme_minimal()

simple_lr_model <- lm(CO2EMISSIONS ~ ENGINESIZE, data=df2)
set.seed(1)
split <- createDataPartition(data$ENGINESIZE, p=0.8, list=FALSE)
train_data <- df2[split,]
test_data <- df2[-split,]

x_train <- train_data$ENGINESIZE
y_train <- train_data$CO2EMISSIONS
x_test <- test_data$ENGINESIZE
y_test <- test_data$CO2EMISSIONS

simple_lr_model <- lm(CO2EMISSIONS ~ ENGINESIZE, data=train_data)

y_pred <- predict(simple_lr_model, data.frame(ENGINESIZE=x_test))

ggplot(data = test_data, aes(x = ENGINESIZE, y = CO2EMISSIONS)) +
  geom_point() +
  geom_line(aes(x = x_test, y = y_pred), color = "green") +
  xlab("ENGINESIZE") +
  ylab("CO2EMISSIONS")

# Calculate R-squared value
r2 <- R2(y_test, y_pred)
print(paste0("R2 Value: ", round(r2, 2)))

constant <- simple_lr_model$coefficients[1]
weight <- simple_lr_model$coefficients[2]
print(paste("Constant : ", constant))
print(paste("Weight/Coefficient : ", weight))

cat("Our Model Equation : y =", constant, "+", weight, "x")

y <- 124.234586 + (39.548745)*(6.0)
print(paste("Estimated CO2EMISSION of our new car:", round(y, 2), "Units"))


#______________________________________________________________________________________________________#


#multiple linear regression
# Train the linear regression model
model <- lm(CO2EMISSIONS ~ ENGINESIZE + CYLINDERS + FUELCONSUMPTION_COMB_MPG, data = data)

ggplot(data = data, aes(x = CO2EMISSIONS)) +
  geom_point(aes(y = ENGINESIZE), color = "blue") +
  geom_smooth(aes(y = ENGINESIZE), method = "lm", color = "blue") +
  geom_point(aes(y = CYLINDERS), color = "red") +
  geom_smooth(aes(y = CYLINDERS), method = "lm", color = "red") +
  geom_point(aes(y = FUELCONSUMPTION_COMB_MPG), color = "green") +
  geom_smooth(aes(y = FUELCONSUMPTION_COMB_MPG), method = "lm", color = "green") +
  labs(title = "Scatterplot Matrix of Multiple Linear Regression with Lines for All Variables", x = "CO2EMISSIONS", y = "") +
  theme_bw()

# Make predictions on the test set
predictions <- predict(model, data)
predictions

# Calculate the MSE
#mse <- mean((testLabels - predictions)^2)
#print(paste("MSE:", round(mse, 2)))

# Print the summary of the model
summary(model)

r2 <- summary(model)$r.squared
print(paste("R-squared:", round(r2, 2)))

coefficients <- coef(model)
intercept <- coefficients[1]
engine_coeff <- coefficients[2]
cylinders_coeff <- coefficients[3]
mpg_coeff <- coefficients[4]

# Print the formula
cat(paste("CO2EMISSIONS = ", round(intercept, 2), " + ", round(engine_coeff, 2), "*ENGINESIZE + ",
          round(cylinders_coeff, 2), "*CYLINDERS + ", round(mpg_coeff, 2), "*FUELCONSUMPTION_COMB_MPG"))

new_data <- data.frame(ENGINESIZE = 6.2, CYLINDERS = 8, FUELCONSUMPTION_COMB_MPG = 19)
predicted <- predict(model, new_data)
cat("Predicted CO2 emissions:", predicted)



###########################################################################################
###########################################################################################
###########################################################################################


#4 KNN
class_levels <- unique(data$VEHICLECLASS)
class_levels
class_numbers <- 1:length(class_levels)
class_numbers
names(class_numbers) <- class_levels
names

data$VEHICLECLASS <- factor(data$VEHICLECLASS, levels = class_levels)
data$VEHICLECLASS <- as.numeric(data$VEHICLECLASS)

class_levels1 <- unique(data$TRANSMISSION)
class_levels1
class_numbers1 <- 1:length(class_levels)
class_numbers1
names(class_numbers1) <- class_levels
names

data$TRANSMISSION <- factor(data$TRANSMISSION, levels = class_levels1)
data$TRANSMISSION <- as.numeric(data$TRANSMISSION)

class_levels2 <- unique(data$MODEL)
class_levels2
class_numbers2 <- 1:length(class_levels2)
class_numbers2
names(class_numbers2) <- class_levels
names
data$MODEL <- factor(data$MODEL, levels = class_levels2)
data$MODEL <- as.numeric(data$MODEL)


class_levels3 <- unique(data$MAKE)
class_levels3
class_numbers3 <- 1:length(class_levels3)
class_numbers3
names(class_numbers3) <- class_levels
names
data$MAKE <- factor(data$MAKE, levels = class_levels3)
data$MAKE <- as.numeric(data$MAKE)



library(caTools)
intrain<- createDataPartition(data$CO2EMISSIONS,p=0.9,list=FALSE,1)
train_data <- data[intrain,]
test_data <- data[-intrain,]
nrow(train_data)
nrow(test_data)



library(class)
knn_model <- knn(train_data[,c("CO2EMISSIONS", "FUELCONSUMPTION_COMB_MPG", "ENGINESIZE", "CYLINDERS", "TRANSMISSION", "MODEL", "MAKE")], test_data[,c("CO2EMISSIONS", "FUELCONSUMPTION_COMB_MPG", "ENGINESIZE", "CYLINDERS", "TRANSMISSION", "MODEL", "MAKE")], train_data$VEHICLECLASS, k = 3)


predicted_class <- knn_model
predicted_class

actual_class <- test_data$VEHICLECLASS
accuracy <- sum(predicted_class == actual_class) / length(actual_class)
accuracy

cat("Predicted category:\n")
print(predicted_class)
accuracy=(accuracy*100)
cat("\nAccuracy:", accuracy)

# Calculate the accuracy for different values of k
library(class)
k_max <- 3
accuracy <- rep(0, k_max)
for (k in 1:k_max) {
  knn_model <- knn(train_data[,c("CO2EMISSIONS", "FUELCONSUMPTION_COMB_MPG", "ENGINESIZE", "CYLINDERS")], test_data[,c("CO2EMISSIONS", "FUELCONSUMPTION_COMB_MPG", "ENGINESIZE", "CYLINDERS")], train_data$VEHICLECLASS, k = k)
  predicted_class <- knn_model
  actual_class <- test_data$VEHICLECLASS
  accuracy[k] <- sum(predicted_class == actual_class) / length(actual_class)
}

# Plot the accuracy vs. k
plot(1:k_max, accuracy, type = "o", xlab = "k", ylab = "Accuracy", main = "Accuracy vs. k")



###########################################################################################
###########################################################################################
###########################################################################################