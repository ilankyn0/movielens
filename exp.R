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
