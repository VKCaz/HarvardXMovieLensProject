# automatically install missing packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(zoo)) install.packages("zoo", repos = "http://cran.us.r-project.org")
if(!require(kernlab)) install.packages("kernlab", repos = "http://cran.us.r-project.org")
if(!require(pls)) install.packages("pls", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")

# load necessary libraries
library(tidyverse)
library(caret)
library(knitr)
library(readxl)
library(ggcorrplot) 
library(zoo)
library(kernlab)
library(pls)
library(xgboost)

### Import data to, and calculate the backing ratio for the ECCU.

raw_backing_ratio <- read.csv("Data/Backing_ratio_Q_ECCU.csv")

# rename the columns, then delete the opening rows of the file.
colnames(raw_backing_ratio) <- raw_backing_ratio[7,]
raw_backing_ratio <- raw_backing_ratio[-(1:7),]

# inspect column data types.
str(raw_backing_ratio)

# widen data to easily calculate backing ratio.
wide_backing_ratio <- pivot_wider(raw_backing_ratio, names_from = `Indicator Label`, values_from = Amount)

# rename column heading to easily reference columns.
wide_backing_ratio <- rename(wide_backing_ratio, Total_External_Assets = `Total External Assets (International Reserves)`, Total_Demand_Liabilities = `Total Demand Liabilities`)

# check for NAs.
sum(is.na(wide_backing_ratio$Total_External_Assets))
sum(is.na(wide_backing_ratio$Total_Demand_Liabilities))

# convert dates from chr to date format.
wide_backing_ratio$Date <- ymd(wide_backing_ratio$Date)

# convert columns from chr to num. Commas must be removed from chr first to avoid NAs, therefore, parse_number function utilized.
wide_backing_ratio$Total_External_Assets <- parse_number(wide_backing_ratio$Total_External_Assets)
wide_backing_ratio$Total_Demand_Liabilities <- parse_number(wide_backing_ratio$Total_Demand_Liabilities)
wide_backing_ratio$Total_External_Assets <- as.numeric(as.character(wide_backing_ratio$Total_External_Assets))
wide_backing_ratio$Total_Demand_Liabilities <- as.numeric(as.character(wide_backing_ratio$Total_Demand_Liabilities))

# convert data frame to tibble.
as_tibble(wide_backing_ratio)

# calculate backing ratio and append to table, then visualize ratio on a line graph.
backing_ratio_eccu <- wide_backing_ratio %>% select(Date, Unit, Total_External_Assets, Total_Demand_Liabilities) %>% arrange(Date) %>% mutate(backing_ratio = Total_External_Assets/Total_Demand_Liabilities*100)

### Calculation of backing ratio completed.

### Import inflation forecast data for OECD countries.

raw_inflation_OECD <- read.csv("Data/Inflation_forecasts_Q_OECD.csv")

# inspect column data types.
str(raw_inflation_OECD)

# widen data for easier interpretation.
wide_inflation_OECD <- pivot_wider(raw_inflation_OECD, names_from = INDICATOR, values_from = Value)

# rename column heading to easily reference columns.
wide_inflation_OECD <- rename(wide_inflation_OECD, Country = ï..LOCATION, Date = TIME, CPI_forecast = CPIFORECAST)

# check for NAs.
sum(is.na(wide_inflation_OECD$CPI_forecast))

# convert TIME from Q1 ... Q4 values to date format.
year <- str_extract(wide_inflation_OECD$Date, "(19|20)\\d{2}")
qdate <- str_extract(wide_inflation_OECD$Date, "Q\\d+")
qdate <- str_replace(qdate, "Q1", "03-31")
qdate <- str_replace(qdate, "Q2", "06-30")
qdate <- str_replace(qdate, "Q3", "09-30")
qdate <- str_replace(qdate, "Q4", "12-31")
wide_inflation_OECD$Date <- paste(year, qdate, sep = '-')
wide_inflation_OECD$Date <- ymd(wide_inflation_OECD$Date)

# convert data frame to tibble.
as_tibble(wide_inflation_OECD)

### End of "inflation forecast data for OECD countries" wrangling.

### Import long term interest rates forecast data for OECD countries.

raw_interest_rates_OECD <- read.csv("Data/Long_term_interest_rates_forecast_Q_OECD.csv")

# inspect column data types.
str(raw_interest_rates_OECD)

# widen data for easier interpretation.
wide_interest_rates_OECD <- pivot_wider(raw_interest_rates_OECD, names_from = INDICATOR, values_from = Value)

# rename column heading to easily reference columns.
wide_interest_rates_OECD <- rename(wide_interest_rates_OECD, Country = ï..LOCATION, Date = TIME, Interest_forecast = LTINTFORECAST)

# check for NAs.
sum(is.na(wide_interest_rates_OECD$Interest_forecast))

# convert TIME from Q1 ... Q4 values to date format.
year <- str_extract(wide_interest_rates_OECD$Date, "(19|20)\\d{2}")
qdate <- str_extract(wide_interest_rates_OECD$Date, "Q\\d+")
qdate <- str_replace(qdate, "Q1", "03-31")
qdate <- str_replace(qdate, "Q2", "06-30")
qdate <- str_replace(qdate, "Q3", "09-30")
qdate <- str_replace(qdate, "Q4", "12-31")
wide_interest_rates_OECD$Date <- paste(year, qdate, sep = '-')
wide_interest_rates_OECD$Date <- ymd(wide_interest_rates_OECD$Date)

# convert data frame to tibble.
as_tibble(wide_interest_rates_OECD)

### End of "long term interest rates forecast data for OECD countries" wrangling.

### Import unemployment rates forecast data for OECD countries.

raw_unemployment_OECD <- read.csv("Data/Unemployment_rates_forecast_Q_OECD.csv")

# inspect column data types.
str(raw_unemployment_OECD)

# widen data for easier interpretation.
wide_unemployment_OECD <- pivot_wider(raw_unemployment_OECD, names_from = INDICATOR, values_from = Value)

# rename column heading to easily reference columns.
wide_unemployment_OECD <- rename(wide_unemployment_OECD, Country = ï..LOCATION, Date = TIME, Unemployment_forecast = UNEMPFORECAST)

# check for NAs.
sum(is.na(wide_unemployment_OECD$Unemployment_forecast))

# convert TIME from Q1 ... Q4 values to date format.
year <- str_extract(wide_unemployment_OECD$Date, "(19|20)\\d{2}")
qdate <- str_extract(wide_unemployment_OECD$Date, "Q\\d+")
qdate <- str_replace(qdate, "Q1", "03-31")
qdate <- str_replace(qdate, "Q2", "06-30")
qdate <- str_replace(qdate, "Q3", "09-30")
qdate <- str_replace(qdate, "Q4", "12-31")
wide_unemployment_OECD$Date <- paste(year, qdate, sep = '-')
wide_unemployment_OECD$Date <- ymd(wide_unemployment_OECD$Date)

# convert data frame to tibble.
as_tibble(wide_unemployment_OECD)

### End of "unemployment rates forecast data for OECD countries" wrangling.

#visualize the backing ratio on a line graph
backing_ratio_eccu %>% ggplot(aes(Date, backing_ratio)) + geom_line() + scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) + geom_hline(yintercept = 60) + annotate("text", x = as_date("2000-01-01"), y = 63, label = "Legal minimum = 60%", hjust = 0) + xlab("Year") + ylab("Backing Ratio")

# select countries which are major source markets for tourism and partners for trade
select_countries <-  c('CAN', 'CHN', 'GBR', 'JPN', 'USA')

# visualize select countries' inflation over period
inflation_OECD <- wide_inflation_OECD %>% select(Country, Date, CPI_forecast)
inflation_OECD %>% filter(Country %in% select_countries) %>% filter(Date >= "2000-03-31") %>% group_by(Country) %>% ggplot(aes(Date, CPI_forecast, col = Country)) + geom_line() + geom_hline(yintercept = 0)  + xlab("Year") + ylab("Inflation forecast")

# visualize select countries' interest rates over period
interest_rates_OECD <- wide_interest_rates_OECD %>% select(Country, Date, Interest_forecast)
interest_rates_OECD %>% filter(Country %in% select_countries) %>% filter(Date >= "2000-03-31") %>% group_by(Country) %>% ggplot(aes(Date, Interest_forecast, col = Country)) + geom_line() + geom_hline(yintercept = 0)  + xlab("Year") + ylab("Interest rate forecast")
# note that the People's Republic of China is absent from the data

# visualize select countries' unemployment over period
unemployment_OECD <- wide_unemployment_OECD %>% select(Country, Date, Unemployment_forecast)
unemployment_OECD %>% filter(Country %in% select_countries) %>% filter(Date >= "2000-03-31") %>% group_by(Country) %>% ggplot(aes(Date, Unemployment_forecast, col = Country)) + geom_line() + geom_hline(yintercept = 0)  + xlab("Year") + ylab("Unemployment forecast")
# note that the People's Republic of China is absent from the data

# remove data frames that are no longer necessary.
rm(raw_backing_ratio, wide_backing_ratio)

# remove data frames that are no longer necessary.
rm(raw_inflation_OECD, wide_inflation_OECD)

# remove data frames that are no longer necessary.
rm(raw_interest_rates_OECD, wide_interest_rates_OECD)

# remove data frames that are no longer necessary.
rm(raw_unemployment_OECD, wide_unemployment_OECD)

### Combine data into one tibble.

# select all the target variable data, and dates.
br <- backing_ratio_eccu %>% select(Date, backing_ratio)

# select only necessary data from the inflation_OECD tibble. Filter countries. Select dates within the same range as that of the target variable.
infOECD <- inflation_OECD %>% filter(Country %in% select_countries) %>% filter(Date >= "2000-03-31") %>% select(Country, Date, CPI_forecast)

# widen the data so that each country's data gets its own column.
infOECD <- pivot_wider(infOECD, names_from = Country, values_from = CPI_forecast)

# rename all the columns with suffix "CPI".
colnames(infOECD) <- paste(colnames(infOECD),"CPI",sep="_")

# revert to the name "Date" for the date column.
infOECD <- rename(infOECD, Date = 'Date_CPI')

# select only necessary data from the interest_rates_OECD tibble. Filter countries. Select dates within the same range as that of the target variable.
intOECD <- interest_rates_OECD %>% filter(Country %in% select_countries) %>% filter(Date >= "2000-03-31") %>% select(Country, Date, Interest_forecast)

# widen the data so that each country's data gets its own column.
intOECD <- pivot_wider(intOECD, names_from = Country, values_from = Interest_forecast)

# rename all the columns with suffix "interest".
colnames(intOECD) <- paste(colnames(intOECD),"interest",sep="_")

# revert to the name "Date" for the date column.
intOECD <- rename(intOECD, Date = 'Date_interest')

# select only necessary data from the unemployment_OECD tibble. Filter countries. Select dates within the same range as that of the target variable.
unempOECD <- unemployment_OECD %>% filter(Country %in% select_countries) %>% filter(Date >= "2000-03-31") %>% select(Country, Date, Unemployment_forecast)

# widen the data so that each country's data gets its own column.
unempOECD <- pivot_wider(unempOECD, names_from = Country, values_from = Unemployment_forecast)

# rename all the columns with suffix "unemployment".
colnames(unempOECD) <- paste(colnames(unempOECD),"unemployment",sep="_")

# revert to the name "Date" for the date column.
unempOECD <- rename(unempOECD, Date = 'Date_unemployment')

# combine into one tibble, joining records by date.
econ_data <- left_join(br, infOECD, by = join_by(Date))
econ_data <- left_join(econ_data, intOECD, by = join_by(Date))
econ_data <- left_join(econ_data, unempOECD, by = join_by(Date))

# remove tibbles that are no longer necessary.
rm(br, infOECD, intOECD, unempOECD)

# For this implementation of the project, remove the date column as it will not be used from this point forward.
econ_data <- subset(econ_data, select = -Date)

### Data are now ready for analysis.

# Pre-processing

# test for near zero variance so that certain variable/features can be removed.
# nearZeroVar returns the index of columns which have near zero variance.
nzv <- nearZeroVar(econ_data)
nzv
# no variable/feature showed near zero variance. Therefore, keep all, or find another way of deleting some.

# create and visualize correlation matrix to gain insight into data.
correlation_matrix <- round(cor(econ_data),2)
ggcorrplot(correlation_matrix, method = 'square')

# Very weak to no association: 0.0 to 0.2
# Weak correlation: 0.2 to 0.4
# Moderate correlation: 0.4 to 0.6
# strong correlation: 0.6 to 0.8
# Very strong correlation: 0.8 to 1.0

# rename target variable to "y" for easier referencing.
econ_data <- rename(econ_data, y = 'backing_ratio')

# Root Mean Square Error function to judge model performance.
RMSE <- function(predicted_value, observed_value){
  sqrt(mean((predicted_value - observed_value)^2))
}

# Partition data into training and testing sets.
# Due to size of data set, 90% of the data will be used for training and 10% for testing.
# Please note that in this initial implementation of the project, data will be treated as if independent and identically distributed (i.i.d.).
# For future implementations, the data are time series data and will be treated as such.

seed <- 1983 # set seed for reproducibility. As an ECCB nugget, 1983 will be used.
set.seed(seed) # if using version later than R 3.5, add sample.kind = "Rounding" argument to set.seed.

test_index <- createDataPartition(y = econ_data$y, times = 1,
                                  p = 0.1, list = FALSE)
train_set <- econ_data[-test_index,]
test_set <- econ_data[test_index,]

# Use of machine learning models for regression.
# build the model with backing_ratio as target and all other variables/features as predictors.

### Generalized Linear Model (glm) will be used as the baseline model.

set.seed(seed)

# glm can fit both linear and non-linear data.
train_glm <- train(y ~ ., method = "glm", data = train_set)

# create a data frame to record the performance indicators on the training set.
train_results <- data_frame(getTrainPerf(train_glm))

# make predictions on the target variable using the test_set.
y_hat_glm <- predict(train_glm, test_set, type = "raw")

# convert output to a numeric format.
y_hat_glm <- as.numeric(y_hat_glm)

#calculate RMSE of glm prediction versus test data.
glm_value <- RMSE(y_hat_glm, test_set$y)

# create data frame to record RMSEs of the algorithms on the test set.
rmse_results <- data_frame(Method= "Generalized Linear Model (Baseline)", RMSE_value = glm_value)

# print the data frame to the console.
rmse_results %>% knitr::kable()

### End of glm (baseline model)

### k-Nearest Neighbors

set.seed(seed)

# use 10-fold cross validation to tune knn algorithm instead of default bootstrap method.
control_10fold <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(y ~ ., method = "knn",
                      data = train_set,
                      tuneGrid = data.frame(k = seq(1, 11, 1)),
                      trControl = control_10fold)

# visualize results of cross validation.
ggplot(train_knn_cv, highlight = TRUE)  + xlab("Number of neighbors, k") + ylab("RMSE (10-fold cross-validation)")

# details of model selected.
#train_knn_cv$finalModel

# append training performance indicators of the training set to the train_results data frame.
train_results <- bind_rows(train_results, data_frame(getTrainPerf(train_knn_cv)))

# make predictions on the target variable using the test_set.
y_hat_knn <- predict(train_knn_cv, test_set, type = "raw", k = train_knn_cv$bestTune)

# calculate RMSE of knn prediction versus test data.
knn_value <- RMSE(y_hat_knn, test_set$y)

# print result to data frame.
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="k-Nearest Neighbours",
                                     RMSE_value = knn_value))

# print the data frame to the console.
rmse_results %>% knitr::kable()

### End of knn

### Random Forest

set.seed(seed)

# default bootstrap method utilized.
train_rf <- train(y ~ ., method = "rf", data = train_set, tuneGrid = data.frame(mtry = seq(1, 13, 1)))

# visualize results of bootstrap method.
ggplot(train_rf, highlight = TRUE)  + xlab("Number of randomly select predictors, mtry")

# details of model selected.
#train_rf$finalModel

# append training performance indicators of the training set to the train_results data frame.
train_results <- bind_rows(train_results, data_frame(getTrainPerf(train_rf)))

# make predictions on the target variable using the test_set.
y_hat_rf <- predict(train_rf, test_set, type = "raw", mtry = train_rf$bestTune)

# convert output to a numeric format.
y_hat_rf <- as.numeric(y_hat_rf)

# calculate RMSE of rf prediction versus test data.
rf_value <- RMSE(y_hat_rf, test_set$y)

# print result to data frame.
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Random Forest",
                                     RMSE_value = rf_value))

# print the data frame to the console.
rmse_results %>% knitr::kable()

### End of rf

### Support Vector Machine with Polynomial Kernel

set.seed(seed)

# default bootstrap method utilized.
train_svmPoly <- train(y ~ ., method = "svmPoly", data = train_set, tuneGrid = expand.grid(degree = seq(1, 5, 1), scale = seq(0.01, 0.15, 0.01), C = seq(0.1, 0.5, 0.1)))

# visualize results of bootstrap method.
ggplot(train_svmPoly, highlight = TRUE)

# details of model selected.
#train_svmPoly$finalModel

# append training performance indicators of the training set to the train_results data frame.
train_results <- bind_rows(train_results, data_frame(getTrainPerf(train_svmPoly)))

# make predictions on the target variable using the test_set.
y_hat_svmPoly <- predict(train_svmPoly, test_set, type = "raw", degree = train_svmPoly$bestTune$degree, scale = train_svmPoly$bestTune$scale, C = train_svmPoly$bestTune$C)

# calculate RMSE of svmPoly prediction versus test data.
svmPoly_value <- RMSE(y_hat_svmPoly, test_set$y)

# print result to data frame.
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Support Vector Machine with Polynomial Kernel",
                                     RMSE_value = svmPoly_value))

# print the data frame to the console.
rmse_results %>% knitr::kable()

### End of svmPoly

### Principal Component Analysis

set.seed(seed)

# use 5-fold cross-validation.
control_5fold <- trainControl(method = "cv", number = 5, p = .9)
train_pcr <- train(y ~ ., method = "pcr", data = train_set, tuneGrid = data.frame(ncomp = seq(1, 14, 1)), trControl = control_5fold)

# visualize results of cross validation.
ggplot(train_pcr, highlight = TRUE)  + xlab("Number of components, ncomp") + ylab("RMSE (5-fold cross-validation)")

# details of model selected.
#train_pcr$finalModel

# append training performance indicators of the training set to the train_results data frame.
train_results <- bind_rows(train_results, data_frame(getTrainPerf(train_pcr)))

# make predictions on the target variable using the test_set.
y_hat_pcr <- predict(train_pcr, test_set, type = "raw", ncomp = train_pcr$bestTune)

# calculate RMSE of pcr prediction versus test data.
pcr_value <- RMSE(y_hat_pcr, test_set$y)

# print result to data frame.
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Principal Component Analysis",
                                     RMSE_value = pcr_value))

# print the data frame to the console.
rmse_results %>% knitr::kable()

### End of pcr

### eXtreme Gradient Boosting

set.seed(seed)

# default parameters allowed.
train_xgboost <- train(y ~ ., method = "xgbLinear", data = train_set)

# append training performance indicators of the training set to the train_results data frame.
train_results <- bind_rows(train_results, data_frame(getTrainPerf(train_xgboost)))

# make predictions on the target variable using the test_set.
y_hat_xgboost <- predict(train_xgboost, test_set, type = "raw")

# calculate RMSE of xgboost prediction versus test data.
xgboost_value <- RMSE(y_hat_xgboost, test_set$y)

# print result to data frame.
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="eXtreme Gradient Boosting",
                                     RMSE_value = xgboost_value))

# print the data frame to the console.
rmse_results %>% knitr::kable()

### End of xgboost

### Ensemble

# Combine results of all algorithms except the baseline.
alg_results <- data_frame(knn = y_hat_knn, pcr = y_hat_pcr, rf = y_hat_rf, svmPoly = y_hat_svmPoly, xgboost = y_hat_xgboost)

# use the mean after the highest and lowest values are removed.
ensemble_pred <- alg_results %>% rowwise() %>% mutate(trimmed_means = mean(c_across(c(knn, pcr, rf, svmPoly, xgboost)), trim=0.2)) %>% select(trimmed_means)
as_tibble(ensemble_pred)

# calculate RMSE of ensemble prediction versus test data.
ensemble_value <- RMSE(ensemble_pred$trimmed_means, test_set$y)

# print result to data frame.
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Ensemble",
                                     RMSE_value = ensemble_value))

# print the data frame to the console.
rmse_results %>% knitr::kable()

### End of ensemble

#print train_results
train_results %>% knitr::kable()

# sort data frame by RMSE values so that the algorithm with the least value is first.
rmse_results <- rmse_results %>% arrange(RMSE_value)

# print the data frame to the console.
rmse_results %>% knitr::kable()