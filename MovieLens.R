if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(dplyr)
library(stringr)
library(knitr)
library(lubridate)
library(forcats)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data

seed <- 1
# set.seed(seed, sample.kind="Rounding") # if using R 3.6 or later
set.seed(seed) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#####Some of the code which follows was used to respond to the exam questions on edx.
#Exploratory data analysis

str(edx) %>% knitr::kable()

num_rec <- nrow(edx)

head(edx) %>% knitr::kable()

findNAs <- is.na(edx$rating)
sum(findNAs)

findNAs <- is.na(edx$title)
sum(findNAs)

findNAs <- is.na(edx$userId)
sum(findNAs)

findNAs <- is.na(edx$movieId)
sum(findNAs)

findNAs <- is.na(edx$genres)
sum(findNAs)

findNAs <- is.na(edx$timestamp)
sum(findNAs)

edx %>%
  group_by(title) %>%
  summarize(count = n()) %>%
  arrange(-count) %>%
  top_n(10, count) %>%
  ggplot(aes(count, reorder(title, count))) +
  geom_bar(color = "black", fill = "grey44", stat = "identity") +
  geom_text(aes(label=count), hjust=-0.1, size = 2) +
  xlab("Number of ratings") + ylab(NULL) + xlim(0, 40000)

#Question 3
num_movies <- n_distinct(edx$movieId)

edx %>% group_by(userId) %>%
  summarize(count = n()) %>%
  ggplot(aes(count)) +
  geom_histogram(color = "black", fill = "grey44", bins = 40) +
  xlab("Number of Ratings") +
  ylab("Number of Users") +
  scale_x_log10()

#Question 4
num_users <- n_distinct(edx$userId)

#top genres table
top_genr <- edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

top_genr %>% knitr::kable()

edx %>% select(rating) %>% summary() %>% knitr::kable()

#Question 2
#sum(edx$rating == 0)
sum(edx$rating == 0.5)
sum(edx$rating == 3)

# Does the following code reproduce the results of Q3 and Q4? Yes, it does.
# movielens %>%
#   summarize(n_movies = n_distinct(movieId), n_users = n_distinct(userId))

#Question 5
genre_ratings <- edx %>% group_by(genres) %>% summarise(count = n())

index <- str_detect(genre_ratings$genres, "([A-Za-z]*\\|)*Drama(\\|[A-Za-z]*)*")
sum(index * genre_ratings$count)

index <- str_detect(genre_ratings$genres, "([A-Za-z]*\\|)*Comedy(\\|[A-Za-z]*)*")
sum(index * genre_ratings$count)

index <- str_detect(genre_ratings$genres, "([A-Za-z]*\\|)*Thriller(\\|[A-Za-z]*)*")
sum(index * genre_ratings$count)

index <- str_detect(genre_ratings$genres, "([A-Za-z]*\\|)*Romance(\\|[A-Za-z]*)*")
sum(index * genre_ratings$count)

#Question 6
most_ratings <- edx %>% group_by(title) %>% summarise(count = n()) %>% arrange(desc(count))

#Question 7
freq_ratings <- edx %>% group_by(rating) %>% summarise(count = n()) %>% arrange(desc(count))

group <-  ifelse((edx$rating == 1 |edx$rating == 2 | edx$rating == 3 | 
                    edx$rating == 4 | edx$rating == 5) ,
                 "whole_star", 
                 "half_star") 

explore_ratings <- data.frame(edx$rating, group)

ggplot(explore_ratings, aes(x= edx.rating, fill = group)) +
  geom_histogram( binwidth = 0.2) +
  scale_x_continuous(breaks=seq(0, 5, by= 0.5)) +
  scale_fill_manual(values = c("half_star"="grey44", "whole_star"="grey22")) +
  labs(x="Rating", y="Number of ratings")
#####

# Create an additional partition of training and test sets from the provided

# set.seed(seed, sample.kind="Rounding") # if using R 3.6 or later
set.seed(seed) # if using R 3.5 or earlier

test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# To make sure we don't include users and movies in the test set that do not appear in the training set, we removed these.
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test_set set back into train_set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

rm(test_index, temp, removed)

# Loss function - Root Mean Squared Error (RMSE) can be interpreted as standard deviation.
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

### Building the Recommendation System

# A first model
mu_hat <- mean(train_set$rating)
mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)
#naive_rmse

# The following code confirms that any number other than mu_hat would result into a higher RMSE
#predictions <- rep(2.5, nrow(test_set))
#RMSE(test_set$rating, predictions)

# append results to tibble.
rmse_results <- tibble(Model = "Naive - mean user rating", RMSE = naive_rmse)
rmse_results %>% knitr::kable()

#End of naive model

# Modeling movie effect

mu <- mean(train_set$rating)
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 30, data = ., color = I("black")) + xlab("Movie bias") + ylab("Count")

predicted_ratings_movie_effect <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

model_1_rmse <- RMSE(predicted_ratings_movie_effect, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(Model="Movie Effect",
                                 RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()

# End of Movie Effect Model

# Modeling Movie + User Effect

train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black") + xlab("User bias") + ylab("Count")

user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings_movie_user_effect <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

model_2_rmse <- RMSE(predicted_ratings_movie_user_effect, test_set$rating)
#model_2_rmse

rmse_results <- bind_rows(rmse_results,
                          tibble(Model="Movie + User Effect",  
                                 RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

# End of Movie + User Effect Model

## Regularization

#Regularized Movie Effect Model

lambdas <- seq(0, 10, 0.25)
model_3_rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  just_the_sum <- train_set %>%
    group_by(movieId) %>% 
    summarize(s = sum(rating - mu), n_i = n())
  predicted_ratings_reg_movie_effect <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings_reg_movie_effect, test_set$rating))
})

qplot(lambdas, model_3_rmses)  + xlab("Lambda") + ylab("RMSE")
lambda <- lambdas[which.min(model_3_rmses)]
#lambda

rmse_results <- bind_rows(rmse_results,
                          tibble(Model="Regularized Movie Effect",  
                                 RMSE = min(model_3_rmses) ))
rmse_results %>% knitr::kable()

#End of Regularized Movie Effect Model

#Regularized Movie + User Effect Model

lambdas <- seq(0, 10, 0.25)
model_4_rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings_reg_movie_user_effect <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings_reg_movie_user_effect, test_set$rating))
})

qplot(lambdas, model_4_rmses) + xlab("Lambda") + ylab("RMSE")
lambda_rmu <- lambdas[which.min(model_4_rmses)]
#lambda_rmu

rmse_results <- bind_rows(rmse_results,
                          tibble(Model="Regularized Movie + User Effect",  
                                 RMSE = min(model_4_rmses)))
rmse_results %>% knitr::kable()    

#End of Regularized Movie + User Effect Model

#Parallel Matrix Factorization using recosystem package

library(recosystem)

# set.seed(seed, sample.kind="Rounding") # if using R 3.6 or later
set.seed(seed) # if using R 3.5 or earlier

#put data into acceptable format for package
train <- with(train_set, data_memory(user_index = userId, item_index = movieId, rating = rating))
test <- with(test_set, data_memory(user_index = userId, item_index = movieId, rating = rating))

#create a reco object
r <- Reco()

#tune the recommender system's parameters. This is time intensive.
tuned_parameters <- r$tune(train, opts = list(dim = c(20, 30),
                                              costp_l2 = c(0.01, 0.1),
                                              costq_l2 = c(0.01, 0.1),
                                              lrate = c(0.01, 0.1),
                                              nthread = 4,
                                              niter = 10))

#train using tuned parameters
r$train(train, opts = c(tuned_parameters$min, nthread = 4, niter = 30))

#make a prediction
predicted_ratings_matrix_factorization <- r$predict(test, out_memory())

#compare prediction to actual values
model_5_rmse <- RMSE(predicted_ratings_matrix_factorization, test_set$rating)

#report RMSE
rmse_results <- bind_rows(rmse_results,
                          tibble(Model="Matrix Factorization",  
                                 RMSE = model_5_rmse))
rmse_results %>% knitr::kable() 

#End of Parallel Matrix Factorization Model

# Below, use best model on final_holdout_test

# set.seed(seed, sample.kind="Rounding") # if using R 3.6 or later
set.seed(seed) # if using R 3.5 or earlier

edx_train <- with(edx, data_memory(user_index = userId, item_index = movieId, rating = rating))
holdout_test <- with(final_holdout_test, data_memory(user_index = userId, item_index = movieId, rating = rating))
r <- Reco()

#parameters have already been tuned.

r$train(edx_train, opts = c(tuned_parameters$min, nthread = 4, niter = 30))

predicted_ratings_matrix_factorization <- r$predict(holdout_test, out_memory())

model_5_rmse_holdout <- RMSE(predicted_ratings_matrix_factorization, final_holdout_test$rating)

rmse_results <- bind_rows(rmse_results,
                          tibble(Model="Matrix Factorization (on final_holdout_test)",  
                                 RMSE = model_5_rmse_holdout))
rmse_results %>% arrange(RMSE) %>% knitr::kable() 

#End of Parallel Matrix Factorization Model on final_holdout_test

###