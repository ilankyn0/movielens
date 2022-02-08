# Introduction
# Loading all necessary libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
library(ggplot2)
library(lubridate)
library(caret)
library(tidyverse)
library(gridExtra)
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
# Importing Dataset
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
             title = as.character(title), genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
             title = as.character(title), genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Data Pre-processing
# function to calculate the RMSE values
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2, na.rm = T))}

# Split Data: Train and Test Sets containing 90% of data
# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
# remove unnecessary columns on edx and validation dataset
rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Modifying the Year & Genre
# Modify the year as a column in the edx & validation datasets
edx <- edx %>% mutate(year = as.numeric(str_sub(title,-5,-2)))
validation_CM <- validation  
validation <- validation %>% select(-rating)
validation <- validation %>% mutate(year = as.numeric(str_sub(title,-5,-2)))
validation_CM <- validation_CM %>% mutate(year = as.numeric(str_sub(title,-5,-2)))
# Modify the genres variable in the edx & validation dataset (column separated)
# Extract the genre in edx datasets
split_edx  <- edx  %>% separate_rows(genres, sep = "\\|")
# Extract the genre in validation datasets
split_valid <- validation   %>% separate_rows(genres, sep = "\\|")
split_valid_CM <- validation_CM  %>% separate_rows(genres, sep = "\\|")

# Data Interpretation & Exploration
head(edx)
nrow(split_edx)
ncol(split_edx)
summary(edx)
#The code above confirms that there are no missing values.

# No. of zeros were given as ratings in the edx dataset
edx %>% filter(rating == 0) %>% tally()

# No. of threes given as ratings in the edx dataset
edx %>% filter(rating == 3) %>% tally()

# Five most given ratings in descending order
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%
  arrange(desc(count))

total_movies <- n_distinct(edx$movieId)
total_movies
total_users <- n_distinct(edx$userId)
total_users
# In total, 69878 unique users provided ratings and 10677 unique movies were rated.

# Rating Distribution
edx %>% ggplot(aes(rating)) + geom_histogram(fill = "steelblue", bins = 30) + 
  labs(title = "Histogram of ratings", x = "Ratings", y = "Count", 
       fill = element_blank()) + theme_classic()

# average rating
avg_rating <- mean(edx$rating)
# median rating
med_rating <- median(edx$rating)

# Ratings by Movie
edx_movies <- edx %>% group_by(movieId) %>%
  summarize(num_ratings = n(), avg_rating = mean(rating)) %>%
  arrange(desc(num_ratings))
head(edx_movies)
edx_movies %>% ggplot(aes(movieId, num_ratings, color = avg_rating)) +
  geom_point() + scale_color_gradientn(colors = cm.colors(5)) +
  labs(x = "MovieId", y = "Number of Ratings", 
       title = "Ratings by Movie", color = "Average Rating")

# Average Rating & User Rating Distribution
cols<- c(Mean = "darkgreen", Median = "yellow")
stats_users <- edx %>% group_by(userId) %>% 
  summarize(countRating=n(), meanRating=mean(rating), medianRating=median(rating))
# Load the gridextra package
plot1 <- ggplot(stats_users,aes(x = meanRating)) +
  geom_histogram(binwidth = 0.1, colour = "midnightblue", fill = "gray") +
  geom_vline(aes(xintercept=median(`meanRating`), color = "Median"), size = 2) +
  geom_vline(aes(xintercept=mean(`meanRating`), color = "Mean"), size = 2) +
  scale_colour_manual(name = "Bars", values = cols) +
  labs(title = "User Average Ratings", x = "Average rating", y = "Count") +
  scale_x_continuous(breaks = c(1:5, round(median(stats_users$meanRating), 2))) +
  theme(legend.position='bottom')

plot2 <- stats_users %>%  ggplot(aes(x = countRating)) +
  geom_histogram(bins = 50, colour = "midnightblue", fill = "gray") +
  geom_vline(aes(xintercept = median(`countRating`), color = "Median"), size = 2) +
  geom_vline(aes(xintercept = mean(`countRating`), color = "Mean"), size = 2) +
  scale_colour_manual(name = "Bars",values=cols) +
  labs(title = "No. of User Ratings", x = "No. of ratings", y = "Count") +
  scale_x_log10(breaks = c(10,50,100,250, 500, 1000,5000, 
  round(median(stats_users$countRating), 0), round(mean(stats_users$countRating), 0))) +
  theme(legend.position='bottom')
# Will display graphs one over the other
grid.arrange(plot1, plot2, ncol = 1)
#These graphs also highlight the mean and median of movie ratings

# Half star ratings: Less common
edx %>% group_by(rating) %>% summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count, size = I(2))) + geom_line(color = "#E7B800") +
  ggtitle("Correlation between half-star & full-star ratings")

# Ratings by Users
edx_users <- edx %>% group_by(userId) %>%
  summarize(num_ratings = n(), avg_rating = mean(rating)) %>% 
  arrange(desc(num_ratings))
# arranges in descending order
head(edx_users)
edx_users %>% ggplot(aes(userId, num_ratings, color = avg_rating)) +
  geom_point() + scale_color_gradientn(colours = heat.colors(5)) +
  labs(x = "UserId", y = "Number of Ratings", 
       title = "Ratings by User", color = "Average Rating")

# Movies distribution
edx %>% count(movieId) %>% ggplot(aes(n, fill = ..count..)) + 
  geom_histogram(bins = 30, color = "black") + scale_x_log10() + 
  ggtitle("Movies Distribution") + labs(x = "movieId", y = "No. of ratings")

# Model Building
# Sample estimate- mean
rmse_results <- data_frame()
# Calculate the average of all movies
mu <- mean(edx$rating)  
mu

# Penalty Term - Movie Effect
movie_avgs_norm <- edx %>% group_by(movieId) %>% summarize(b_i = mean(rating - mu))
# Using the gridextra package
plot11 <- movie_avgs_norm %>% qplot(b_i, geom ="histogram", bins = 20, data = ., 
                          color = I("black"), fill = I("#C71585"))

# Penalty Term - User Effect
user_avgs_norm <- edx %>% left_join(movie_avgs_norm, by='movieId') %>%
  group_by(userId) %>% summarize(b_u = mean(rating - mu - b_i))
plot21 <- user_avgs_norm %>% qplot(b_u, geom ="histogram", bins = 30, data = ., 
                         color = I("black"), fill = I("#7FFF00"))
grid.arrange(plot11, plot21, ncol = 2) #show plots side by side

# Naive Model
# Calculate the average of all movies
naive_rmse <- RMSE(validation_CM$rating, mu)
naive_rmse
rmse_results <- data_frame(method = "Using mean only", RMSE = naive_rmse)
rmse_results

# Movie Effect Model
# Movie effects only 
predicted_ratings_movie_norm <- validation %>% 
  left_join(movie_avgs_norm, by='movieId') %>%
  mutate(pred = mu + b_i) 
model_1_rmse <- RMSE(validation_CM$rating,predicted_ratings_movie_norm$pred)
rmse_results <- bind_rows(rmse_results, data_frame(method = "Movie Effect Model",  
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()
rmse_results

# Movie and User Effect Model
predicted_ratings_user_norm <- validation %>% 
  left_join(movie_avgs_norm, by = 'movieId') %>%
  left_join(user_avgs_norm, by = 'userId') %>%
  mutate(pred = mu + b_i + b_u)
model_2_rmse <- RMSE(validation_CM$rating, predicted_ratings_user_norm$pred)
rmse_results <- bind_rows(rmse_results, data_frame(method = "Movie and User Effect Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()
rmse_results

# Regularized Movie and User Effect Model
lambdas <- seq(0, 10, 0.25)
# Compute the predicted ratings on validation dataset using different values of lambda
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  # Calculate the average by user
  b_i <- edx %>% 
    group_by(movieId) %>% summarize(b_i = sum(rating - mu)/(n() + l))
  # Calculate the average by user
  b_u <- edx %>% 
    left_join(b_i, by = "movieId") %>% 
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n() + l))
  predicted_ratings <- validation %>% 
    left_join(b_i, by = "movieId") %>% 
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>% .$pred
  return(RMSE(validation_CM$rating, predicted_ratings))
})

qplot(lambdas, rmses)
# Get the lambda value that minimize the RMSE
lambda <- lambdas[which.min(rmses)]
lambda

# Compute regularized estimates of b_i using lambda
movie_avgs_reg <- edx %>% group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())

# Compute regularized estimates of b_u using lambda
user_avgs_reg <- edx %>% 
  left_join(movie_avgs_reg, by='movieId') %>% group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n() + lambda), n_u = n())

predicted_ratings_reg <- validation %>% 
  left_join(movie_avgs_reg, by='movieId') %>%
  left_join(user_avgs_reg, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>% .$pred

# Test and save results
model_3_rmse <- RMSE(validation_CM$rating,predicted_ratings_reg)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Regularized Movie and User Effect Model", 
                                     RMSE = model_3_rmse))
rmse_results %>% knitr::kable()
rmse_results

# Regularized With All Effects Model
# b_y and b_g represent the year & genre effects, respectively
lambdas <- seq(0, 20, 1)
# Compute the predicted ratings on validation dataset using different values of lambda
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  # Calculate the average by user
  b_i <- split_edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  # Calculate the average by user
  b_u <- split_edx %>% left_join(b_i, by="movieId") %>% 
    group_by(userId) %>% summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_y <- split_edx %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+lambda), n_y = n())
  
  b_g <- split_edx %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_y, by = 'year') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u - b_y)/(n()+lambda), n_g = n())
# Compute the predicted ratings on validation dataset 
  predicted_ratings <- split_valid %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_y, by = 'year') %>%
    left_join(b_g, by = 'genres') %>%
    mutate(pred = mu + b_i + b_u + b_y + b_g) %>% 
    .$pred
  # Predict the RMSE on the validation set
  return(RMSE(split_valid_CM$rating,predicted_ratings))
})
# Compute new predictions using the optimal lambda
qplot(lambdas, rmses)
# Get the lambda value that minimize the RMSE
lambda_2 <- lambdas[which.min(rmses)]
lambda_2

movie_reg_avgs_2 <- split_edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda_2), n_i = n())

user_reg_avgs_2 <- split_edx %>% 
  left_join(movie_reg_avgs_2, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lambda_2), n_u = n())

year_reg_avgs <- split_edx %>%
  left_join(movie_reg_avgs_2, by='movieId') %>%
  left_join(user_reg_avgs_2, by='userId') %>%
  group_by(year) %>%
  summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+lambda_2), n_y = n())

genre_reg_avgs <- split_edx %>%
  left_join(movie_reg_avgs_2, by='movieId') %>%
  left_join(user_reg_avgs_2, by='userId') %>%
  left_join(year_reg_avgs, by = 'year') %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u - b_y)/(n()+lambda_2), n_g = n())

# Compute the predicted ratings on validation dataset
predicted_ratings <- split_valid %>% 
  left_join(movie_reg_avgs_2, by='movieId') %>%
  left_join(user_reg_avgs_2, by='userId') %>%
  left_join(year_reg_avgs, by = 'year') %>%
  left_join(genre_reg_avgs, by = 'genres') %>%
  mutate(pred = mu + b_i + b_u + b_y + b_g) %>% 
  .$pred

model_4_rmse <- RMSE(split_valid_CM$rating,predicted_ratings)
rmse_results <- bind_rows(rmse_results, 
            data_frame(method = "Reg Movie, User, Year, and Genre Effect Model", 
                                     RMSE = model_4_rmse))
rmse_results %>% knitr::kable()

# Results
# RMSE overview
rmse_results %>% knitr::kable()
# Rating Prediction using Model 4
lambda_3 <- 14
movie_reg_avgs_2 <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda_3), n_i = n())

user_reg_avgs_2 <- edx %>% 
  left_join(movie_reg_avgs_2, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lambda_3), n_u = n())

year_reg_avgs <- edx %>%
  left_join(movie_reg_avgs_2, by='movieId') %>%
  left_join(user_reg_avgs_2, by='userId') %>%
  group_by(year) %>%
  summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+lambda_3), n_y = n())

genre_reg_avgs <- edx %>%
  left_join(movie_reg_avgs_2, by='movieId') %>%
  left_join(user_reg_avgs_2, by='userId') %>%
  left_join(year_reg_avgs, by = 'year') %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u - b_y)/(n()+lambda_3), n_g = n())

# Compute the predicted ratings on validation dataset
predicted_ratings <- split_valid %>% 
  left_join(movie_reg_avgs_2, by='movieId') %>%
  left_join(user_reg_avgs_2, by='userId') %>%
  left_join(year_reg_avgs, by = 'year') %>%
  left_join(genre_reg_avgs, by = 'genres') %>%
  mutate(pred = mu + b_i + b_u + b_y + b_g) %>%
  group_by(userId,movieId) %>% summarize(pred_2 = mean(pred))

# Showing the results from the results dataset
rmse_results %>% knitr::kable()
#Since, model 4 yielded the best RMSE result, we will consider it as the final prediction model
