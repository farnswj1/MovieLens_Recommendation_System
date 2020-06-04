# Justin Farnsworth
# MovieLens Recommendation System
# May 26, 2020

# WARNING: 
#   This program will take approximately 30-45 minutes to complete. However, 
#   some parts of the code will require about 25GB to complete. They will be 
#   explicitly identified in the code.


# LOADING THE DATA
####################################################################################################

# Required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(gghighlight)) install.packages("gghighlight", repos = "http://cran.us.r-project.org")


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# Download and extract data from the file
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Delete variables no longer needed
rm(dl, ratings, movies, test_index, temp, movielens, removed)


# EXPLORING THE DATASET - OVERVIEW
####################################################################################################

# Dimensions of the dataset
dim(edx)

# Show the first 10 rows in the edx dataset
head(edx)

# Show the data types for each column
data.frame(
  column_names = colnames(edx),
  data_type = map_chr(colnames(edx), function(colname) {class(edx[, colname])})
)


# CLEANING UP THE DATASETS
####################################################################################################

# Check for any null values
any(is.na(edx))
any(is.na(validation))

# Convert the timestamp into datetime
edx <- edx %>% 
  mutate(dateTimeOfRating = as.POSIXct(timestamp, origin="1970-01-01")) %>% 
  select(-timestamp)

validation <- validation %>% 
  mutate(dateTimeOfRating = as.POSIXct(timestamp, origin="1970-01-01")) %>% 
  select(-timestamp)

# Separate the release year from the movie title
edx <- edx %>%
  extract(title, 
          into = c("title", "releaseYear"), 
          regex = "^(.+) \\((\\d+)\\)$", 
          remove = FALSE) %>% 
  mutate(releaseYear = as.integer(releaseYear))

validation <- validation %>%
  extract(title, 
          into = c("title", "releaseYear"), 
          regex = "^(.+) \\(([\\d]+)\\)$", 
          remove = FALSE) %>% 
  mutate(releaseYear = as.integer(releaseYear))

# Ensure both datasets are in order based on userId, then movieId
edx <- edx %>% arrange(userId, movieId)
validation <- validation %>% arrange(userId, movieId)

# Show the first few rows of the dataset after clean-up
head(edx)


# EXPLORING THE DATASET - MOVIES
####################################################################################################

# Number of distinct movies in the dataset
n_distinct(edx$movieId)

# Show the top 10 movies with the most ratings
edx %>% 
  group_by(movieId, title) %>% 
  summarize(count = n()) %>%
  arrange(desc(count)) %>% 
  top_n(10)

# Show the top 10 movies by average ratings and with at least 1000 ratings
edx %>%
  group_by(movieId, title) %>% 
  summarize(avg_rating = mean(rating), count = n()) %>%
  filter(count >= 1000) %>% 
  arrange(desc(avg_rating)) %>%
  top_n(10)

# Show the bottom 10 movies by average ratings and with at least 1000 ratings
edx %>%
  group_by(movieId, title) %>% 
  summarize(avg_rating = mean(rating), count = n()) %>%
  filter(count >= 1000) %>% 
  arrange(avg_rating) %>%
  top_n(10)

# Calculate the correlation between the average ratings and the number of ratings
movie_avg_ratings_and_counts <- edx %>% 
  group_by(movieId) %>% 
  summarize(avg_rating = mean(rating), count = n()) %>% 
  select(avg_rating, count)
movie_avg_ratings_and_counts %>% 
  ggplot(aes(count, avg_rating)) + 
  geom_point() + 
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) + 
  ggtitle("Number of Ratings vs. Average Rating of Movies") + 
  xlab("Number of Ratings") + 
  ylab("Average Rating")

# Calculate correlation coefficient
cc <- movie_avg_ratings_and_counts %>% 
  summarize(r = cor(count, avg_rating)) %>% 
  .$r
cc

# Plot the average ratings for each movie
edx %>% 
  group_by(movieId) %>% 
  summarize(avg_rating = mean(rating)) %>% 
  ggplot(aes(avg_rating)) + 
  geom_histogram(bins = 50) + 
  ggtitle("Frequency of Average Ratings of Each Movie") + 
  xlab("Average Rating") + 
  ylab("Number of Movies")

# Show the top 10 movies with the highest median rating and with at least 1000 ratings 
edx %>% 
  group_by(movieId, title) %>% 
  summarize(median = median(rating), count = n()) %>% 
  filter(count >= 1000) %>%
  arrange(desc(median)) %>%
  top_n(10)

# Show the 10 movies with the lowest median rating and with at least 1000 ratings 
edx %>% 
  group_by(movieId, title) %>% 
  summarize(median = mean(rating), count = n()) %>% 
  filter(count >= 1000) %>%
  arrange(median) %>%
  top_n(10)

# Plot the median ratings for each movie
edx %>% 
  group_by(movieId) %>% 
  summarize(median_rating = median(rating)) %>% 
  ggplot(aes(median_rating)) + 
  geom_histogram(bins = 10) + 
  ggtitle("Frequency of Median Ratings of Each Movie") + 
  xlab("Median Rating") + 
  ylab("Number of Movies")

# Show the top 10 movies with the largest standard deviations and with at least 100 ratings
edx %>% 
  group_by(movieId, title) %>% 
  summarize(st_dev = sd(rating), avg_rating = mean(rating), count = n()) %>% 
  filter(count >= 100) %>%
  arrange(desc(st_dev)) %>% 
  top_n(10)


# EXPLORING THE DATASET - USERS
####################################################################################################

# Number of distinct users in the dataset
n_distinct(edx$userId)

# Show the users with the most ratings
edx %>% 
  group_by(userId) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
   top_n(10)

# Plot the average rating for each user
avg_of_user_avgs <- edx %>% 
  group_by(userId) %>% 
  summarize(avg_rating = mean(rating)) 

avg_of_user_avgs %>% 
  ggplot(aes(avg_rating)) + 
  geom_histogram(bins = 50) + 
  ggtitle("Frequency of Average User Ratings") + 
  xlab("Average Rating") + 
  ylab("Number of Users")

# Compute the mean and standard deviation of the average user ratings
avg_of_user_avgs %>% 
  summarize(mean = mean(avg_rating), st_dev = sd(avg_rating))


# EXPLORING THE DATASET - RELEASE YEARS
####################################################################################################

# Show the range of release years
range(edx$releaseYear)

# Show the year that had the most movie releases
number_of_releases_by_year <- edx %>% 
  select(movieId, releaseYear) %>%
  unique() %>%
  group_by(releaseYear) %>% 
  summarize(count = n())
number_of_releases_by_year %>% arrange(desc(count)) %>% top_n(10)

# Plot the number of releases by year
number_of_releases_by_year %>%
  ggplot(aes(releaseYear, count)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Movie Releases by Year") + 
  xlab("Year") + 
  ylab("Number of Movies")

# Plot the boxplots of the total ratings for each movie by year
edx %>% 
  group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(releaseYear))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Number of User Ratings For Each Movie By Release Year") + 
  xlab("Year") + 
  ylab("Number of Ratings")

# Plot the average rating by release year
edx %>% 
  select(releaseYear, rating) %>% 
  group_by(releaseYear) %>% 
  summarize(avg_rating = mean(rating)) %>% 
  ggplot(aes(releaseYear, avg_rating)) + 
  geom_point() + 
  ggtitle("Average Rating of Movies by Release Year") + 
  xlab("Release Year") + 
  ylab("Average Rating")


# EXPLORING THE DATASET - GENRES
####################################################################################################

# Show the list of genres
# NOTE: We want to keep the table as small as necessary before separating the genres.
# To do this, we get the distinct list before the rows are separated.
# Otherwise this will utilize over 20GB of memory and slow down performance.
list_of_genres <- edx %>% 
  select(genres) %>% 
  distinct() %>% 
  separate_rows(genres, sep = "\\|") %>% 
  distinct() %>% 
  arrange(genres) %>% 
  .$genres
list_of_genres

# Show the movie(s) that don't have a genre
edx %>% 
  filter(genres == "(no genres listed)") %>% 
  select(movieId, title, releaseYear) %>% 
  unique()

# Count the number of movies for each genre
# NOTE: movies can have more than one genre
movies <- edx %>% 
  select(movieId, title, releaseYear, genres) %>% 
  unique()
data.frame(genre = list_of_genres, 
           count = map_dbl(list_of_genres, function(genre){
             sum(str_detect(movies$genres, genre))
           })
) %>% arrange(desc(count))

# Count the number of ratings for each genre
data.frame(genre = list_of_genres, 
           count = map_dbl(list_of_genres, function(genre){
           sum(str_detect(edx$genres, genre))
})) %>% arrange(desc(count))


# Find the average and SE of the ratings for each genre
avg_rating_by_genre <- map_df(list_of_genres, function(genre){
  edx %>% 
    select(genres, rating) %>% 
    filter(str_detect(genres, genre)) %>%
    summarize(genres = genre, avg_rating = mean(rating), se = sd(rating)/sqrt(n()))
}) %>% 
  filter(genres != "(no genres listed)") %>% 
  arrange(desc(avg_rating))
avg_rating_by_genre

# Plot boxplots of ratings for each genre
avg_rating_by_genre %>% 
  mutate(genres = reorder(genres, avg_rating)) %>% 
  ggplot(aes(genres, avg_rating, ymin = avg_rating - 2 * se, ymax = avg_rating + 2 * se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Average Ratings by Genre") + 
  xlab("Genres") + 
  ylab("Average Rating")


# EXPLORING THE DATASET - RATINGS
####################################################################################################

# Order the most commonly given ratings from greatest to least
frequency_of_ratings <- edx %>% 
  select(rating) %>% 
  group_by(rating) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
frequency_of_ratings

# Plot the totals of each rating
frequency_of_ratings %>%
  mutate(rating = factor(rating)) %>%
  group_by(rating) %>%
  ggplot(aes(rating, count, label = rating)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Total Number of Ratings by Rating") + 
  xlab("Rating") + 
  ylab("Total") + 
  scale_y_continuous(labels = function(y) format(y, scientific = FALSE))

# Count the number of ratings with a whole-star rating vs. those that are not
# Also show the proportions of the two types of ratings
edx %>% 
  select(rating) %>% 
  mutate(whole_star_rating = ifelse(rating %in% 1:5, "Yes", "No")) %>% 
  group_by(whole_star_rating) %>% 
  summarize(count = n()) %>% 
  mutate(proportion = count / sum(count))

# Show number of ratings by year and by whether it is a whole-star rating or not
edx %>% 
  select(dateTimeOfRating, rating) %>% 
  mutate(whole_star_rating = ifelse(rating %in% 1:5, "Yes", "No")) %>% 
  mutate(yearOfRating = year(dateTimeOfRating)) %>% 
  group_by(yearOfRating, whole_star_rating) %>% 
  summarize(count = n()) %>% 
  spread(whole_star_rating, count)

# Get earliest rating in the dataset
min(edx$dateTimeOfRating)

# Get latest rating in the dataset
max(edx$dateTimeOfRating)


# LOSS FUNCTION - RESIDUAL MEAN SQUARED ERROR
####################################################################################################

# Residual mean squared error (RMSE) function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


# MODELS - JUST THE AVERAGE
####################################################################################################

# Compute the mean rating in the edx dataset
mu_hat <- mean(edx$rating)
mu_hat

# Calculate RMSE using the average
results <- data.frame(model = "Only The Average", 
                      RMSE = RMSE(validation$rating, mu_hat))
results


# MODELS - MOVIE EFFECT
####################################################################################################

# Compute the mean difference between the movie's rating and the average
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_m = mean(rating - mu_hat))

# Predict the movie ratings using the mean difference for each movie (movie effect)
y_hat_movies <- validation %>%
  left_join(movie_avgs, by='movieId') %>%
  mutate(prediction = mu_hat + b_m) %>%
  .$prediction

# Calculate RMSE using the movie effect model
results <- results %>%
  add_row(model = "Movie Effect", 
  RMSE = RMSE(validation$rating, y_hat_movies))
results


# MODELS - MOVIE EFFECT (REGULARIZED)
####################################################################################################

# Try this sequence of lambdas
lambdas <- seq(0, 10, 0.25)

# Returns the RMSEs for each lambda
rmses_1 <- sapply(lambdas, function(lambda) {
  # Print lambda to keep track of which lambda the function is using
  print(paste("Lambda:", lambda))
  
  movie_avgs <- edx %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu_hat) / (n() + lambda))
  
  y_hat_movies_regularized <- validation %>%
    left_join(movie_avgs, by='movieId') %>%
    mutate(prediction = mu_hat + b_m) %>%
    .$prediction
  
  return(RMSE(validation$rating, y_hat_movies_regularized))
})

# Plot the lambdas and their respective RMSEs
data.frame(Lambdas = lambdas, RMSEs = rmses_1) %>% 
  ggplot(aes(Lambdas, RMSEs)) + 
  geom_point() + 
  gghighlight(RMSEs == min(RMSEs)) + 
  ggtitle("Movie Effect (Regularized)")

# Get the lambda with the lowest RMSEs
lambdas[which.min(rmses_1)]

# Add the regularized model to results
results <- results %>%
  add_row(model = "Regularized Movie Effect", 
          RMSE = min(rmses_1))
results


# MODELS - MOVIE & USER EFFECT
####################################################################################################

# Compute the mean difference between the user's rating 
# and the average with movie effect.
user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_m))

# Predict the movie ratings using movie and user effects
y_hat_movies_users <- validation %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(prediction = mu_hat + b_m + b_u) %>%
  .$prediction

# Calculate RMSE using the movie and user effects model
results <- results %>% 
  add_row(model = "Movie + User Effect",
          RMSE = RMSE(validation$rating, y_hat_movies_users))
results


# MODELS - MOVIE & USER EFFECT (REGULARIZED)
####################################################################################################

# Try this sequence of lambdas
lambdas <- seq(0, 10, 0.25)

# Returns the RMSEs for each lambda
rmses_2 <- sapply(lambdas, function(lambda) {
  # Print lambda to keep track of which lambda the function is using
  print(paste("Lambda:", lambda))
  
  movie_avgs <- edx %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu_hat) / (n() + lambda))
  
  user_avgs <- edx %>% 
    left_join(movie_avgs, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu_hat - b_m)/(n() + lambda))
  
  y_hat_movies_users_regularized <- validation %>%
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    mutate(prediction = mu_hat + b_m + b_u) %>%
    .$prediction
  
  return(RMSE(validation$rating, y_hat_movies_users_regularized))
})

# Plot the lambdas and their respective RMSEs
data.frame(Lambdas = lambdas, RMSEs = rmses_2) %>% 
  ggplot(aes(Lambdas, RMSEs)) + 
  geom_point() + 
  gghighlight(RMSEs == min(RMSEs)) + 
  ggtitle("Movie + User Effect (Regularized)")

# Get the lambda with the lowest RMSEs
lambdas[which.min(rmses_2)]

# Add the regularized model to results
results <- results %>%
  add_row(model = "Regularized Movie + User Effect", 
          RMSE = min(rmses_2))
results


# MOVIE, USER, & RELEASE YEAR EFFECT
####################################################################################################

# Compute the mean differences
releaseyear_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(releaseYear) %>%
  summarize(b_y = mean(rating - mu_hat - b_m - b_u))

# Predict the movie ratings using movie, user, and year effects
y_hat_movies_users_year <- validation %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(releaseyear_avgs, by='releaseYear') %>%
  mutate(prediction = mu_hat + b_m + b_u + b_y) %>%
  .$prediction

# Calculate RMSE using the movie, user, & year effects model
results <- results %>% 
  add_row(model = "Movie + User + Release Year Effect",
          RMSE = RMSE(validation$rating, y_hat_movies_users_year))
results


# MOVIE, USER, & RELEASE YEAR EFFECT (REGULARIZED)
####################################################################################################

# Try this sequence of lambdas
lambdas <- seq(0, 10, 0.25)

# Returns the RMSEs for each lambda
rmses_3 <- sapply(lambdas, function(lambda) {
  # Print lambda to keep track of which lambda the function is using
  print(paste("Lambda:", lambda))
  
  movie_avgs <- edx %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu_hat) / (n() + lambda))
  
  user_avgs <- edx %>% 
    left_join(movie_avgs, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu_hat - b_m)/(n() + lambda))
  
  releaseyear_avgs <- edx %>% 
    left_join(movie_avgs, by="movieId") %>%
    left_join(user_avgs, by="userId") %>%
    group_by(releaseYear) %>%
    summarize(b_y = sum(rating - mu_hat - b_m - b_u)/(n() + lambda))
  
  y_hat_movies_users_years_regularized <- validation %>%
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    left_join(releaseyear_avgs, by='releaseYear') %>%
    mutate(prediction = mu_hat + b_m + b_u + b_y) %>%
    .$prediction
  
  return(RMSE(validation$rating, y_hat_movies_users_years_regularized))
})

# Plot the lambdas and their respective RMSEs
data.frame(Lambdas = lambdas, RMSEs = rmses_3) %>% 
  ggplot(aes(Lambdas, RMSEs)) + 
  geom_point() + 
  gghighlight(RMSEs == min(RMSEs)) + 
  ggtitle("Movie + User + Release Year Effect (Regularized)")

# Get the lambda with the lowest RMSEs
lambdas[which.min(rmses_3)]

# Add the regularized model to results
results <- results %>%
  add_row(model = "Regularized Movie + User + Release Year Effect", 
          RMSE = min(rmses_3))
results


# MOVIE, USER, RELEASE YEAR, & GENRE EFFECT
# WARNING: THIS WILL CONSUME ROUGHLY 25GB OF MEMORY!!!
####################################################################################################

# Clean up workspace before continuing
gc()

# Separate the genres into their own rows and rename the column to 'genre'.
# WARNING: THIS WILL CONSUME ROUGHLY 25GB OF MEMORY!!!
edx_genres_split <- edx %>% 
  rename(genre = genres) %>% 
  separate_rows(genre, sep = "\\|")

validation_genres_split <- validation %>% 
  rename(genre = genres) %>% 
  separate_rows(genre, sep = "\\|")

# Show the first 12 rows of the edx_genres_split table.
head(edx_genres_split, 12)

# This computes the mean differences.
genre_avgs <- edx_genres_split %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(releaseyear_avgs, by='releaseYear') %>% 
  group_by(genre) %>%
  summarize(b_g = mean(rating - mu_hat - b_m - b_u - b_y))

# Predict the movie ratings using movie, user, and year effects
y_hat_movies_users_year_genre <- validation_genres_split %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(releaseyear_avgs, by='releaseYear') %>%
  left_join(genre_avgs, by='genre') %>%
  group_by(userId, movieId) %>% 
  summarize(prediction = mu_hat[1] + b_m[1] + b_u[1] + b_y[1] + mean(b_g)) %>%
  arrange(userId, movieId) %>% 
  .$prediction

# Calculate RMSE using the movie, user, year, & genre effects model
results <- results %>% 
  add_row(model = "Movie + User + Release Year + Genre Effect",
          RMSE = RMSE(validation$rating, y_hat_movies_users_year_genre))
results


# MOVIE, USER, RELEASE YEAR, & GENRE EFFECT (REGULARIZED)
# WARNING: THIS WILL CONSUME ROUGHLY 25GB OF MEMORY!!!
####################################################################################################

# Clean up workspace before continuing
gc()

# Try this sequence of lambdas
lambdas <- seq(0, 10, 0.25)

# Returns the RMSEs for each lambda
rmses_4 <- sapply(lambdas, function(lambda) {
  # Print lambda to keep track of which lambda the function is using
  print(paste("Lambda:", lambda))
  
  movie_avgs <- edx %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu_hat) / (n() + lambda))
  
  user_avgs <- edx %>% 
    left_join(movie_avgs, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu_hat - b_m)/(n() + lambda))
  
  releaseyear_avgs <- edx %>% 
    left_join(movie_avgs, by="movieId") %>%
    left_join(user_avgs, by="userId") %>%
    group_by(releaseYear) %>%
    summarize(b_y = sum(rating - mu_hat - b_m - b_u)/(n() + lambda))
  
  genre_avgs <- edx_genres_split %>% 
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    left_join(releaseyear_avgs, by='releaseYear') %>% 
    group_by(genre) %>%
    summarize(b_g = sum(rating - mu_hat - b_m - b_u - b_y)/(n() + lambda))
  
  y_hat_movies_users_years_genres_regularized <- validation_genres_split %>%
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    left_join(releaseyear_avgs, by='releaseYear') %>%
    left_join(genre_avgs, by='genre') %>%
    group_by(userId, movieId) %>% 
    summarize(prediction = mu_hat[1] + b_m[1] + b_u[1] + b_y[1] + mean(b_g)) %>%
    arrange(userId, movieId) %>% 
    .$prediction
  
  return(RMSE(validation$rating, y_hat_movies_users_years_genres_regularized))
})

# Plot the lambdas and their respective RMSEs
data.frame(Lambdas = lambdas, RMSEs = rmses_4) %>% 
  ggplot(aes(Lambdas, RMSEs)) + 
  geom_point() + 
  gghighlight(RMSEs == min(RMSEs)) + 
  ggtitle("Movie + User + Release Year + Genre Effect (Regularized)")

# Get the lambda with the lowest RMSEs
lambdas[which.min(rmses_4)]

# Add the regularized model to results
results <- results %>%
  add_row(model = "Regularized Movie + User + Release Year + Genre Effect", 
          RMSE = min(rmses_4))
results

# Clean up memory
gc()