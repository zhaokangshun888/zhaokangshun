setwd("XX")
###################################################################per capita consumption stability
# Loading data
df <- read.csv("Per capita consumption.csv",header=TRUE,check.names = FALSE)
head(df)
# Standardize each row of data
# Select the variable columns that you want to standardize
cols_to_standardize <- names(df)[-1] # Exclude the first column"Name"
# Standardize the processing of each row of data
df[, cols_to_standardize] <- t(apply(df[, cols_to_standardize], 1, function(x) {
  (x - min(x)) / (max(x) - min(x))
}))
head(df)

#Set the first column to the row name
rownames(df) <- df[, 1]
#Check if the name is a country
rownames(df)
write.table(df, file = "Per capita consumption in 220 countries after standardization.csv", sep = ",", row.names = FALSE)
# Split data
df_1961_1990 <- df[!(rownames(df) %in%c("American Samoa","Armenia","Azerbaijan","Belarus","Belgium","Bonaire","Bosnia and Herzegovina",
                                        "Cayman Islands","Croatia","Curacao","Czech Republic","Eritrea","Estonia","Ethiopia","Falkland Islands",
                                        "Georgia","Kazakhstan","Kyrgyzstan","Latvia","Lesotho","Lithuania","Luxembourg",
                                        "Moldova","Monaco","Montenegro","Niue","North Macedonia","Palestine","Russia","Saint Barthelemy",
                                        "Saint Martin","Saint Pierre and Miquelon","Serbia","Sint Maarten","Slovakia","Slovenia","South Sudan","Sudan","Tajikistan","Timor-Leste",
                                        "Turkmenistan","Ukraine","Uzbekistan")), c("Country", "1961":"1990")]

df_1991_2019 <- df[!(rownames(df) %in%c("American Samoa","Armenia","Azerbaijan","Belarus","Belgium","Bonaire","Bosnia and Herzegovina",
                                        "Cayman Islands","Croatia","Curacao","Czech Republic","Eritrea","Estonia","Ethiopia","Falkland Islands",
                                        "Georgia","Kazakhstan","Kyrgyzstan","Latvia","Lesotho","Lithuania","Luxembourg",
                                        "Moldova","Monaco","Montenegro","Niue","North Macedonia","Palestine","Russia","Saint Barthelemy",
                                        "Saint Martin","Saint Pierre and Miquelon","Serbia","Sint Maarten","Slovakia","Slovenia","South Sudan","Sudan","Tajikistan","Timor-Leste",
                                        "Turkmenistan","Ukraine","Uzbekistan")), c("Country", "1991":"2019")]

write.table(df_1961_1990, file = "Per capita consumption in 177 countries after standardization-1961_1990.csv", sep = ",", row.names = FALSE)
write.table(df_1991_2019, file = "Per capita consumption in 177 countries after standardization-1991_2019.csv", sep = ",", row.names = FALSE)


# Define functions for linear regression and de-trending for each country
# install.packages("nlme")
library(nlme)

detrend <- function(df) {
  # Yield data and year information were extracted
  production <- as.numeric(unlist(df[,-1]))
  year <- as.numeric(colnames(df)[2:ncol(df)])
  # The linear model is constructed and the time autocorrelation structure is considered
  model <- gls(production ~ year, correlation = corAR1(form = ~ year),method="REML")
  # Extract model summary
  summary_model <- summary(model)
  # Extract slope and p-value
  coef_model <- coef(summary(model))
  slope <- coef_model[2, "Value"]
  # Extract p-value
  p_value <- summary(model)$tTable[2, "p-value"]
  # Extract confidence interval
  conf_interval <- confint(model)[2, ]
  lower_bound <- conf_interval[1]
  upper_bound <- conf_interval[2]
  # Residual
  resids <- residuals(model)
  # Standard deviation
  detrended <- sd(resids)
  # Calculate the stability after detrending
  stability <- mean(production) / detrended
  # Extract AIC
  aic_value <- AIC(model)
  # Return stability and slope
  return(data.frame(stability = stability, slope = slope, p_value = p_value, lower_bound = lower_bound, upper_bound = upper_bound, aic = aic_value))
}

###Test the accuracy of the function
df <- read.csv("Per capita consumption in 173 countries after standardization-test.csv",header=TRUE,check.names = FALSE)
head(df)
country_data <- subset(df, Country == "China")
model <- gls(production ~ year, correlation = corAR1(form = ~ year),method="ML",data=country_data)
summary(model)
acf(residuals(model))
#Confidence interval
conf_interval <- confint(model)[2, ]
conf_interval


# Apply the detrend function to each country separately and generate a list object
stability_1961_1990 <- lapply(split(df_1961_1990, df_1961_1990$Country), detrend)
stability_1991_2019 <- lapply(split(df_1991_2019, df_1991_2019$Country), detrend)

stability_1961_1990<-do.call(rbind,stability_1961_1990)
stability_1991_2019<-do.call(rbind,stability_1991_2019)
# Export the file to a CSV file
write.table(stability_1961_1990, file = "stability_per capita consumption_1961_1990.csv", sep = ",", row.names = TRUE)
write.table(stability_1991_2019, file = "stability_per capita consumption_1991_2019.csv", sep = ",", row.names = TRUE)


###################################################################per capita production stability
# Loading data
df <- read.csv("Per capita production.csv",header=TRUE,check.names = FALSE)
head(df)
# Standardize each row of data
# Select the variable columns that you want to standardize
cols_to_standardize <- names(df)[-1] # Exclude the first column"Name"
# Standardize the processing of each row of data
df[, cols_to_standardize] <- t(apply(df[, cols_to_standardize], 1, function(x) {
  (x - min(x)) / (max(x) - min(x))
}))
head(df)

#Set the first column to the row name
rownames(df) <- df[, 1]
#Check if the name is a country
rownames(df)

write.table(df, file = "Production per capita for 220 countries after standardization.csv", sep = ",", row.names = FALSE)

# Split data
df_1961_1990 <- df[!(rownames(df) %in%c("American Samoa","Armenia","Azerbaijan","Belarus","Belgium","Bonaire","Bosnia and Herzegovina",
                                        "Cayman Islands","Croatia","Curacao","Czech Republic","Eritrea","Estonia","Ethiopia","Falkland Islands",
                                        "Georgia","Kazakhstan","Kyrgyzstan","Latvia","Lesotho","Lithuania","Luxembourg",
                                        "Moldova","Monaco","Montenegro","Niue","North Macedonia","Palestine","Russia","Saint Barthelemy",
                                        "Saint Martin","Saint Pierre and Miquelon","Serbia","Sint Maarten","Slovakia","Slovenia","South Sudan","Sudan","Tajikistan","Timor-Leste",
                                        "Turkmenistan","Ukraine","Uzbekistan")), c("Country", "1961":"1990")]

df_1991_2019 <- df[!(rownames(df) %in%c("American Samoa","Armenia","Azerbaijan","Belarus","Belgium","Bonaire","Bosnia and Herzegovina",
                                        "Cayman Islands","Croatia","Curacao","Czech Republic","Eritrea","Estonia","Ethiopia","Falkland Islands",
                                        "Georgia","Kazakhstan","Kyrgyzstan","Latvia","Lesotho","Lithuania","Luxembourg",
                                        "Moldova","Monaco","Montenegro","Niue","North Macedonia","Palestine","Russia","Saint Barthelemy",
                                        "Saint Martin","Saint Pierre and Miquelon","Serbia","Sint Maarten","Slovakia","Slovenia","South Sudan","Sudan","Tajikistan","Timor-Leste",
                                        "Turkmenistan","Ukraine","Uzbekistan")), c("Country", "1991":"2019")]

write.table(df_1961_1990, file = "Production per capita for 177 countries after standardization-1961_1990.csv", sep = ",", row.names = FALSE)
write.table(df_1991_2019, file = "Production per capita for 177 countries after standardization-1991_2019.csv", sep = ",", row.names = FALSE)

# Apply the detrend function to each country separately and generate a list object
stability_1961_1990 <- lapply(split(df_1961_1990, df_1961_1990$Country), detrend)
stability_1991_2019 <- lapply(split(df_1991_2019, df_1991_2019$Country), detrend)

stability_1961_1990<-do.call(rbind,stability_1961_1990)
stability_1991_2019<-do.call(rbind,stability_1991_2019)
# Export the file to a CSV file
write.table(stability_1961_1990, file = "stability_per capita production_1961_1990.csv", sep = ",", row.names = TRUE)
write.table(stability_1991_2019, file = "stability_per capita production_1991_2019.csv", sep = ",", row.names = TRUE)


###################################################################per capita aquaculture stability
# Loading data
df <- read.csv("Per capita aquaculture.csv",header=TRUE,check.names = FALSE)
head(df)
# Standardize each row of data
# Select the variable columns that you want to standardize
cols_to_standardize <- names(df)[-1] # Exclude the first column "Name"
# Standardize the processing of each row of data
df[, cols_to_standardize] <- t(apply(df[, cols_to_standardize], 1, function(x) {
  (x - min(x)) / (max(x) - min(x))
}))
head(df)

#Set the first column to the row name
rownames(df) <- df[, 1]
#Check if the name is a country
rownames(df)

write.table(df, file = "Production per capita in 220 countries after standardization.csv", sep = ",", row.names = FALSE)

head(df)
df_1991_2019 <- df[!(rownames(df) %in%c("American Samoa","Armenia","Azerbaijan","Belarus","Belgium","Bonaire","Bosnia and Herzegovina",
                                        "Cayman Islands","Croatia","Curacao","Czech Republic","Eritrea","Estonia","Ethiopia","Falkland Islands",
                                        "Georgia","Kazakhstan","Kyrgyzstan","Latvia","Lesotho","Lithuania","Luxembourg",
                                        "Moldova","Monaco","Montenegro","Niue","North Macedonia","Palestine","Russia","Saint Barthelemy",
                                        "Saint Martin","Saint Pierre and Miquelon","Serbia","Sint Maarten","Slovakia","Slovenia","South Sudan","Sudan","Tajikistan","Timor-Leste",
                                        "Turkmenistan","Ukraine","Uzbekistan",
                                        "Anguilla","Bermuda","Comoros","Djibouti","Greenland","Grenadines","Maldives","Mauritania","Micronesia","Mongolia",
                                        "Montserrat","Saint Helena","Sao Tome and Principe","Somalia","Turks and Caicos Islands","Wallis and Futuna")), c("Country", "1991":"2019")]
head(df_1991_2019)
write.table(df_1991_2019, file = "Production per capita in 161 countries after standardization-1991-2019.csv", sep = ",", row.names = FALSE)


#Remove countries that include multiple 0 and NA values from the data
df_1991_2019 <- df_1991_2019[!(rownames(df_1991_2019) %in%c("Antigua","Aruba","Barbados","Botswana","Chad","Cook Islands","Equatorial Guinea","Grenada","Guinea-Bissau",
                                        "Marshall Islands","Nauru","Northern Mariana Islands","Palau","Samoa","Tonga","United Arab Emirates","Vanuatu","Virgin Islands",
                                        "Yemen")), c("Country", "1991":"2019")]

head(df_1991_2019)

write.table(df_1991_2019, file = "Per capita production in 142 screened countries-1991-2019.csv", sep = ",", row.names = FALSE)


# Apply the detrend function to each country separately and generate a list object
stability_1991_2019 <- lapply(split(df_1991_2019, df_1991_2019$Country), detrend)

stability_1991_2019<-do.call(rbind,stability_1991_2019)
# Export the file to a CSV file
write.table(stability_1991_2019, file = "stability_per capita aquaculture_1991_2019.csv", sep = ",", row.names = TRUE)



###################################################################per capita capture stability
# Loading data
df <- read.csv("Per capita capture.csv",header=TRUE,check.names = FALSE)
head(df)
# Standardize each row of data
# Select the variable columns that you want to standardize
cols_to_standardize <- names(df)[-1] # Exclude the first column "Name"
# Standardize the processing of each row of data
df[, cols_to_standardize] <- t(apply(df[, cols_to_standardize], 1, function(x) {
  (x - min(x)) / (max(x) - min(x))
}))
head(df)

#Set the first column to the row name
rownames(df) <- df[, 1]
#Check if the name is country
rownames(df)

write.table(df, file = "Per capita catch of 220 countries after standardization.csv", sep = ",", row.names = FALSE)

df_1991_2019 <- df[!(rownames(df) %in%c("American Samoa","Armenia","Azerbaijan","Belarus","Belgium","Bonaire","Bosnia and Herzegovina",
                                        "Cayman Islands","Croatia","Curacao","Czech Republic","Eritrea","Estonia","Ethiopia","Falkland Islands",
                                        "Georgia","Kazakhstan","Kyrgyzstan","Latvia","Lesotho","Lithuania","Luxembourg",
                                        "Moldova","Monaco","Montenegro","Niue","North Macedonia","Palestine","Russia","Saint Barthelemy",
                                        "Saint Martin","Saint Pierre and Miquelon","Serbia","Sint Maarten","Slovakia","Slovenia","South Sudan","Sudan","Tajikistan","Timor-Leste",
                                        "Turkmenistan","Ukraine","Uzbekistan")), c("Country", "1991":"2019")]

write.table(df_1991_2019, file = "Per capita catch for 177 countries after standardization-1991-2019.csv", sep = ",", row.names = FALSE)

#Remove countries that include multiple 0 and NA values from the data
head(df_1991_2019)
write.table(df_1991_2019, file = "Per capita catch of 177 screened countries-1991_2019.csv", sep = ",", row.names = FALSE)

# Apply the detrend function to each country separately and generate a list object
stability_1991_2019 <- lapply(split(df_1991_2019, df_1991_2019$Country), detrend)

stability_1991_2019<-do.call(rbind,stability_1991_2019)
# Export the file to a CSV file
write.table(stability_1991_2019, file = "stability_per capita capture_1991_2019.csv", sep = ",", row.names = TRUE)
