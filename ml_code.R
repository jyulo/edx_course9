# This file is for the code section of MovieLens
# EdX Data Science course 9: Movielens Project

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

# Create edx set, validation set (final hold-out test set)

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(caret)
library(data.table)
library(dslabs)
library(dplyr)
library(lubridate)
library(ggplot2)


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# Downloading the MovieLens data
dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::",
                             "\t",
                             readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, 
                                          "ml-10M100K/movies.dat")),
                          "\\::",
                          3)

colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                            title = as.character(title),
#                                            genres = as.character(genres))

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- 
  createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

## Exploratory Analysis

# Format of the Dataset
str(edx)

# Dimensions of the Dataset
nrow(edx) # 9000055 rows

ncol(edx) # 6 cols

# number of unique movies
length(unique(edx$movieId))

# number of unique users
n_distinct(edx$userId)

# distribution of movies rated among users
edx %>%
  group_by(userId) %>%
  summarize(movies_rated = n()) %>%
  summarize(max = max(movies_rated),
            min = min(movies_rated),
            mean = mean(movies_rated),
            median = median(movies_rated))

# distribution of movies rated among users, but plotted
edx %>%
  group_by(userId) %>%
  summarize(movies_rated = n()) %>%
  ggplot(aes(movies_rated)) +
  geom_histogram(binwidth = 10) +
  xlim(c(0, 2000)) +
  ggtitle('Distribution of Movies Rated by Each User')

# quick glance at the most frequently rated movies
edx %>% group_by(title) %>% summarize(ratings = n()) %>%
  arrange(desc(ratings)) %>%
  head(.,20)

# distribution of how frequently each movie is rated
edx %>%
  group_by(movieId) %>%
  summarize(ratings_received = n()) %>%
  summarize(max = max(ratings_received),
            min = min(ratings_received),
            mean = mean(ratings_received),
            median = median(ratings_received))

# same thing but plot
edx %>%
  group_by(movieId) %>%
  summarize(ratings_received = n()) %>%
  ggplot(aes(ratings_received)) +
  geom_histogram(binwidth = 10) +
  xlim(c(0, 10000)) +
  ggtitle('Distribution of Ratings Received by Each Film')

# time span covered in data
range(as_datetime(edx$timestamp))

# How many genres are there?
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# genre groups
edx %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# examining data format
edx %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(.,20)

# Distribution of ratings
mean(edx$rating)
median(edx$rating)

# quick glance at the most commonly given out ratings
edx %>%
  group_by(rating) %>%
  summarize(counts = n()) %>%
  arrange(desc(counts))

# plot distribution of rating scores
edx %>%
  group_by(rating) %>%
  summarize(frequency = n() / nrow(edx)) %>%
  ggplot(aes(x = factor(rating), y = frequency)) +
  geom_bar(stat = 'identity') +
  ggtitle('Frequencies of Ratings Given')

## Cleaning and Wrangling

# genre vector
genre_list <- c('Drama',
                'Comedy',
                'Action',
                'Thriller',
                'Adventure',
                'Romance',
                'Sci-Fi',
                'Crime',
                'Fantasy',
                'Children',
                'Horror',
                'Mystery',
                'War',
                'Animation',
                'Musical',
                'Western',
                'Film-Noir',
                'Documentary',
                'IMAX',
                '(no genres listed)')

# timestamp conversion
# convert timestamp to year, month, and date
edx <- edx %>%
  mutate(year = year(as_datetime(timestamp)),
         month = month(as_datetime(timestamp)),
         date = date(as_datetime(timestamp))) %>%
  select(-c(timestamp, title))

# Repeat the same process with validation set
validation <- validation %>%
  mutate(year = year(as_datetime(timestamp)),
         month = month(as_datetime(timestamp)),
         date = date(as_datetime(timestamp))) %>%
  select(-c(timestamp, title))

# test-train partition of edx
# split your edx data into train and test before modeling!
# Let's assign 90 % for train, 10 % for test
set.seed(1, sample.kind = 'Rounding') # arbitrary seed for reproducibility

test_index <- createDataPartition(y = edx$rating,
                                  times = 1,
                                  p = 0.1,
                                  list = F)


# Test-train split
edx_train <- edx[-test_index,]

edx_temp <- edx[test_index,]

# making sure that the movie and user IDs overlap using semi_join
edx_test <- edx_temp %>%
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

# Add rows removed from edx_test back into edx_train
removed <- anti_join(edx_temp, edx_test)
edx_train <- rbind(edx_train, removed)
rm(edx_temp)
rm(removed)
rm(test_index)

# computing the baseline avg (mu)
base_avg <- mean(edx_train$rating)

# RMSE function
RMSE <- function(pred, real) {
  sqrt(mean((pred - real)^2))
}

## Results

# model 1: movie and user ids only
# Progressively narrower lambdas for tuning
lambdas <- seq(0, 20, 1)
lambdas <- seq(4, 6, 0.1)
lambdas <- seq(4.85, 5, 0.01)

rmses <- sapply(lambdas, function(l) { # do everything below for each lambda
  # the mu
  base_avg <- mean(edx_train$rating)
  # movie effect
  movie_avg <- edx_train %>%
    group_by(movieId) %>%
    summarize(bi = sum(rating - base_avg)/(n() + l))
  # user effect
  user_avg <- edx_train %>%
    left_join(movie_avg, by = 'movieId') %>%
    group_by(userId) %>%
    summarize(bu = sum(rating - base_avg - bi)/(n() + l))
  # fitting (for each given lambda l)
  pred <-
    edx_test %>%
    left_join(movie_avg, by = 'movieId') %>%
    left_join(user_avg, by = 'userId') %>%
    mutate(pred = base_avg + bi + bu) %>%
    pull(pred)
  # grab RMSE
  return(RMSE(pred, edx_test$rating))
})
plot(lambdas, rmses) # for visualization during tuning

# saving the lambda and RMSE for comparison later
lambda_1 <- lambdas[which.min(rmses)]
train_rmse_1 <- min(rmses)

# Iteration 2a1: year effect modeled by grouped averages

lambdas <- seq(0, 10, 1)
lambdas <- seq(4.5, 5.5, 0.1)
lambdas <- seq(4.9, 5.1, 0.01)

rmses <- sapply(lambdas, function(l) { # do everything below for each lambda
  # the mu
  base_avg <- mean(edx_train$rating)
  # movie effect
  movie_avg <- edx_train %>%
    group_by(movieId) %>%
    summarize(bi = sum(rating - base_avg)/(n() + l))
  # user effect
  user_avg <- edx_train %>%
    left_join(movie_avg, by = 'movieId') %>%
    group_by(userId) %>%
    summarize(bu = sum(rating - base_avg - bi)/(n() + l))
  # year effect
  year_avg <- edx_train %>%
    left_join(movie_avg, by = 'movieId') %>%
    left_join(user_avg, by = 'userId') %>%
    group_by(year) %>%
    summarize(byr = sum(rating - base_avg - bi - bu)/(n() + l))
  # fitting (for each given lambda l)
  pred <-
    edx_test %>%
    left_join(movie_avg, by = 'movieId') %>%
    left_join(user_avg, by = 'userId') %>%
    left_join(year_avg, by = 'year') %>%
    mutate(pred = base_avg + bi + bu + byr) %>%
    pull(pred)
  # grab RMSE
  return(RMSE(pred, edx_test$rating))
})
plot(lambdas, rmses) # for tuning purposes only

lambda_2a1 <- lambdas[which.min(rmses)]
train_rmse_2a1 <- min(rmses)

# iteration 2a2: year, month modeled by grouped avgs

lambdas <- seq(0, 10, 1)
lambdas <- seq(4, 6, 0.1)
lambdas <- seq(4.95, 5.1, 0.01)

rmses <- sapply(lambdas, function(l) { # do everything below for each lambda
  # the mu
  base_avg <- mean(edx_train$rating)
  # movie effect
  movie_avg <- edx_train %>%
    group_by(movieId) %>%
    summarize(bi = sum(rating - base_avg)/(n() + l))
  # user effect
  user_avg <- edx_train %>%
    left_join(movie_avg, by = 'movieId') %>%
    group_by(userId) %>%
    summarize(bu = sum(rating - base_avg - bi)/(n() + l))
  # year effect
  year_avg <- edx_train %>%
    left_join(movie_avg, by = 'movieId') %>%
    left_join(user_avg, by = 'userId') %>%
    group_by(year) %>%
    summarize(byr = sum(rating - base_avg - bi - bu)/(n() + l))
  # month effect
  month_avg <- edx_train %>%
    left_join(movie_avg, by = 'movieId') %>%
    left_join(user_avg, by = 'userId') %>%
    left_join(year_avg, by = 'year') %>%
    group_by(month) %>%
    summarize(bmo = sum(rating - base_avg - bi - bu - byr)/(n() + l))
  # fitting (for each given lambda l)
  pred <-
    edx_test %>%
    left_join(movie_avg, by = 'movieId') %>%
    left_join(user_avg, by = 'userId') %>%
    left_join(year_avg, by = 'year') %>%
    left_join(month_avg, by = 'month') %>%
    mutate(pred = base_avg + bi + bu + byr + bmo) %>%
    pull(pred)
  # grab RMSE
  return(RMSE(pred, edx_test$rating))
})
plot(lambdas, rmses) # for tuning only

lambda_2a2 <- lambdas[which.min(rmses)]
train_rmse_2a2 <- min(rmses)

# Iteration 2b1: some kind of model on dates (chose loess smooth)

span <- 30 / as.numeric(diff(range(edx_train$date)))

lambdas <- seq(0, 10, 1)
lambdas <- seq(4.4, 5.6, 0.1)
lambdas <- seq(5.15, 5.25, 0.01)

rmses <- sapply(lambdas, function(l) { # do everything below for each lambda
  # the mu
  base_avg <- mean(edx_train$rating)
  # movie effect
  movie_avg <- edx_train %>%
    group_by(movieId) %>%
    summarize(bi = sum(rating - base_avg)/(n() + l))
  # user effect
  user_avg <- edx_train %>%
    left_join(movie_avg, by = 'movieId') %>%
    group_by(userId) %>%
    summarize(bu = sum(rating - base_avg - bi)/(n() + l))
  # date effect; will smooth
  date_avg <- edx_train %>%
    left_join(movie_avg, by = 'movieId') %>%
    left_join(user_avg, by = 'userId') %>%
    group_by(date) %>%
    summarize(bd = sum(rating - base_avg - bi - bu)/(n() + l))
  #modeling a smooth function out of the date effect
  date_effect_model <- loess(bd ~ as.numeric(date),
                             data = date_avg,
                             span = span)
  #...then make predictions based on it
  date_effect_preds <- predict(date_effect_model, edx_test$date)
  # fitting (for each given lambda l)
  pred <-
    edx_test %>%
    left_join(movie_avg, by = 'movieId') %>%
    left_join(user_avg, by = 'userId') %>%
    mutate(pred = base_avg + bi + bu + date_effect_preds) %>%
    pull(pred)
  # grab RMSE
  return(RMSE(pred, edx_test$rating))
})
plot(lambdas, rmses) # for tuning only

lambda_2b1 <- lambdas[which.min(rmses)]
train_rmse_2b1 <- min(rmses)

# is 2b1 overfitting?

# consider using different spans; plot your smooth functions
# to avoid overfitting

# holding lambda constant, let's see how small the span could get
# before the function overfits...

l <- lambda_2b1
# the mu
base_avg <- mean(edx_train$rating)
# movie effect
movie_avg <- edx_train %>%
  group_by(movieId) %>%
  summarize(bi = sum(rating - base_avg)/(n() + l))
# user effect
user_avg <- edx_train %>%
  left_join(movie_avg, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(bu = sum(rating - base_avg - bi)/(n() + l))
# date effect; will smooth
date_avg <- edx_train %>%
  left_join(movie_avg, by = 'movieId') %>%
  left_join(user_avg, by = 'userId') %>%
  group_by(date) %>%
  summarize(bd = sum(rating - base_avg - bi - bu)/(n() + l))

# plotting the date effect, under different spans
# but controlling for lambda
spans <- c(30, 60, 90, 180, 270, 365)

par(mfrow = c(3, 2))  # 3 rows and 2 columns

plots <- for (s in spans) {
  plot <- date_avg %>%
    ggplot(aes(x = date, y = bd)) +
    geom_point() +
    geom_smooth(method = 'loess',
                span = s / as.numeric(diff(range(edx_train$date)))) +
    ggtitle(paste('Span of', as.character(s), 'Days', sep = ' '))
  print(plot)
}

# 2b2: optimized span of a year; re-tune lambda

span <- 365 / as.numeric(diff(range(edx_train$date)))
# increasing narrow lambdas for tuning
lambdas <- seq(0, 10, 1)
lambdas <- seq(4, 6, 0.1)
lambdas <- seq(5.05, 5.15, 0.01)
rmses <- sapply(lambdas, function(l) { # do everything below for each lambda
  # the mu
  base_avg <- mean(edx_train$rating)
  # movie effect
  movie_avg <- edx_train %>%
    group_by(movieId) %>%
    summarize(bi = sum(rating - base_avg)/(n() + l))
  # user effect
  user_avg <- edx_train %>%
    left_join(movie_avg, by = 'movieId') %>%
    group_by(userId) %>%
    summarize(bu = sum(rating - base_avg - bi)/(n() + l))
  # date effect; will smooth
  date_avg <- edx_train %>%
    left_join(movie_avg, by = 'movieId') %>%
    left_join(user_avg, by = 'userId') %>%
    group_by(date) %>%
    summarize(bd = sum(rating - base_avg - bi - bu)/(n() + l))
  #modeling a smooth function out of the date effect
  date_effect_model <- loess(bd ~ as.numeric(date),
                             data = date_avg,
                             span = span)
  #...then make predictions based on it
  date_effect_preds <- predict(date_effect_model, edx_test$date)
  # fitting (for each given lambda l)
  pred <-
    edx_test %>%
    left_join(movie_avg, by = 'movieId') %>%
    left_join(user_avg, by = 'userId') %>%
    mutate(pred = base_avg + bi + bu + date_effect_preds) %>%
    pull(pred)
  # grab RMSE
  return(RMSE(pred, edx_test$rating))
})
plot(lambdas, rmses) # tuning only

lambda_2b2 <- lambdas[which.min(rmses)]
train_rmse_2b2 <- min(rmses)

# plot loess funcs again
l <- lambda_2b2
# the mu
base_avg <- mean(edx_train$rating)
# movie effect
movie_avg <- edx_train %>%
  group_by(movieId) %>%
  summarize(bi = sum(rating - base_avg)/(n() + l))
# user effect
user_avg <- edx_train %>%
  left_join(movie_avg, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(bu = sum(rating - base_avg - bi)/(n() + l))
# date effect; will smooth
date_avg <- edx_train %>%
  left_join(movie_avg, by = 'movieId') %>%
  left_join(user_avg, by = 'userId') %>%
  group_by(date) %>%
  summarize(bd = sum(rating - base_avg - bi - bu)/(n() + l))

spans <- c(30, 60, 90, 180, 270, 365)
par(mfrow = c(3, 2))  # 3 rows and 2 columns

plots <- for (s in spans) {
  plot <- date_avg %>%
    ggplot(aes(x = date, y = bd)) +
    geom_point() +
    geom_smooth(method = 'loess',
                span = s / as.numeric(diff(range(edx_train$date)))) +
    ggtitle(paste('span of', as.character(s), 'days', sep = ' '))
  print(plot)
}
# not much has changed.

# removing the objects used to make the plots
rm(date_avg)
rm(movie_avg)
rm(user_avg)
rm(spans)
rm(plots)
rm(plot)

# a quick comparison of different time effect models
data.table(time_effects = c('year only', 'year and month'),
           grouped_means = c(train_rmse_2a1, train_rmse_2a2),
           smoothed = c(train_rmse_2b2, train_rmse_2b1))

# Iteration 3a: Genre groups avg 

span <- 365 / as.numeric(diff(range(edx_train$date)))

lambdas <- seq(0, 10, 1)
lambdas <- seq(4, 6, 0.1)
lambdas <- seq(4.95, 5.15, 0.01)

rmses <- sapply(lambdas, function(l) { # do everything below for each lambda
  # the mu
  base_avg <- mean(edx_train$rating)
  # movie effect
  movie_avg <- edx_train %>%
    group_by(movieId) %>%
    summarize(bi = sum(rating - base_avg)/(n() + l))
  # user effect
  user_avg <- edx_train %>%
    left_join(movie_avg, by = 'movieId') %>%
    group_by(userId) %>%
    summarize(bu = sum(rating - base_avg - bi)/(n() + l))
  # date effect; will smooth
  date_avg <- edx_train %>%
    left_join(movie_avg, by = 'movieId') %>%
    left_join(user_avg, by = 'userId') %>%
    group_by(date) %>%
    summarize(bd = sum(rating - base_avg - bi - bu)/(n() + l))
  #modeling a smooth function out of the date effect
  date_effect_model <- loess(bd ~ as.numeric(date),
                             data = date_avg,
                             span = span)
  # predict using model on train set...
  train_date_preds <- predict(date_effect_model, edx_train$date)
  # then subtract predictions to get train set residuals not explained by model
  # genre group effect
  genres_avg <- edx_train %>%
    left_join(movie_avg, by = 'movieId') %>%
    left_join(user_avg, by = 'userId') %>%
    mutate(bd = train_date_preds) %>% # rows already correspond to edx_train
    group_by(genres) %>%
    summarize(bgg =
                sum(rating - base_avg - bi - bu - bd)/(n() + l))
  
  # fitting (for each given lambda l)
  pred <-
    edx_test %>%
    left_join(movie_avg, by = 'movieId') %>%
    left_join(user_avg, by = 'userId') %>%
    mutate(bd_test = predict(date_effect_model, date)) %>% # fit model on test set for results, same as in 2b1/2b2
    left_join(genres_avg, by = 'genres') %>%
    mutate(pred = base_avg + bi + bu + bd_test + bgg) %>%
    pull(pred)
  # grab RMSE
  return(RMSE(pred, edx_test$rating))
})

plot(lambdas, rmses) # tuning only

lambda_3a <- lambdas[which.min(rmses)]
train_rmse_3a <- min(rmses)

# Genre matrices for model 3b

edx_train_genres <-
  edx_train[,4] # the column number with genre information, after the previous modifications
# only the needed column is chosen, to avoid overextention of memory during computation

edx_train_genres <-
  sapply(genre_list, function(g) {
    edx_train_genres$g <- str_detect(g, edx_train_genres$genres)
  })

edx_test_genres <-
  edx_test[,4]

edx_test_genres <-
  sapply(genre_list, function(g) {
    edx_test_genres$g <- str_detect(g, edx_test_genres$genres)
  })

head(edx_train_genres, 3)

gc() # freeing some memory

# Iteration 3b: Genres examined separately

span <- 365 / as.numeric(diff(range(edx_train$date)))

lambdas <- seq(0, 6, 1)
lambdas <- seq(4.5, 5.5, 0.1)
lambdas <- seq(5.05, 5.15, 0.01)

rmses <- sapply(lambdas, function(l) { # do everything below for each lambda
  # the mu
  base_avg <- mean(edx_train$rating)
  # movie effect
  movie_avg <- edx_train %>%
    group_by(movieId) %>%
    summarize(bi = sum(rating - base_avg)/(n() + l))
  # user effect
  user_avg <- edx_train %>%
    left_join(movie_avg, by = 'movieId') %>%
    group_by(userId) %>%
    summarize(bu = sum(rating - base_avg - bi)/(n() + l))
  # date effect; will smooth
  date_avg <- edx_train %>%
    left_join(movie_avg, by = 'movieId') %>%
    left_join(user_avg, by = 'userId') %>%
    group_by(date) %>%
    summarize(bd = sum(rating - base_avg - bi - bu)/(n() + l))
  #modeling a smooth function out of the date effect
  date_effect_model <- loess(bd ~ as.numeric(date),
                             data = date_avg,
                             span = span)
  # predict using model on train set...
  train_date_preds <- predict(date_effect_model, edx_train$date)
  # then subtract predictions to get train set residuals not explained by model
  # residuals for genre avg computations
  resids <- edx_train %>%
    left_join(movie_avg, by = 'movieId') %>%
    left_join(user_avg, by = 'userId') %>%
    mutate(bd = train_date_preds) %>% # rows already correspond to edx_train
    mutate(resids = rating - base_avg - bi - bu - bd)
  
  # find genre avgs of residual variability
  genre_avgs <- sapply(genre_list, function(genre) {
    resids %>% 
      # filter for rows in train set AND genre = T
      filter(edx_train_genres[,genre]) %>% 
      # regularized average resid by genre
      summarize(avg = sum(resids) / (n() + l))
  })
  
  
  # fitting without the genre effects/resids
  pred <-
    edx_test %>%
    left_join(movie_avg, by = 'movieId') %>%
    left_join(user_avg, by = 'userId') %>%
    mutate(bd_test = predict(date_effect_model, date)) %>% # same as in 2b1/2b2
    mutate(pred = base_avg + bi + bu + bd_test) %>%
    pull(pred)
  
  # adding average genre effects
  for (g in genre_list) {
    # genre effect avg, searched from corresponding genre position in the vector genre_list
    bg <- as.numeric(genre_avgs[which(genre_list == g)])
    # add that avg to positions of predicted values vector (pred) corresponding to edx_genres$g == T,
    pred[edx_test_genres[,g]] <-
      pred[edx_test_genres[,g]] + bg
  }
  # grab RMSE
  return(RMSE(pred, edx_test$rating))
})

plot(lambdas, rmses) # tuning only

lambda_3b <- lambdas[which.min(rmses)]
train_rmse_3b <- min(rmses)

# Comparing model iterations
data.table(effects = c('model 1 (movie and user effects)',
                       'model 2b2 (movie, user, and date effects)',
                       'model 3a (movie, user, date, and genre group effects)'),
           RMSE = c(train_rmse_1,
                    train_rmse_2b2,
                    train_rmse_3a),
           lambda = c(lambda_1,
                      lambda_2b2,
                      lambda_3a))

# removing genre matrices; no longer have use for them
rm(edx_train_genres)
rm(edx_test_genres)
rm(genre_list)

gc() # freeing some memory

# validation run:
# Iteration 3a: Genre groups avg, but retrained
# now using edx as train set and validation as "test" set
# lambda is lambda_3a

span <- 365 / as.numeric(diff(range(edx_train$date)))

rmse_final <- function(l = lambda_3a) { 
  # the mu
  base_avg <- mean(edx$rating)
  # movie effect
  movie_avg <- edx %>%
    group_by(movieId) %>%
    summarize(bi = sum(rating - base_avg)/(n() + l))
  # user effect
  user_avg <- edx %>%
    left_join(movie_avg, by = 'movieId') %>%
    group_by(userId) %>%
    summarize(bu = sum(rating - base_avg - bi)/(n() + l))
  # date effect; will smooth
  date_avg <- edx %>%
    left_join(movie_avg, by = 'movieId') %>%
    left_join(user_avg, by = 'userId') %>%
    group_by(date) %>%
    summarize(bd = sum(rating - base_avg - bi - bu)/(n() + l))
  #modeling a smooth function out of the date effect
  date_effect_model <- loess(bd ~ as.numeric(date),
                             data = date_avg,
                             span = span)
  # predict using model on train set (edx)...
  train_date_preds <- predict(date_effect_model, edx$date)
  # then subtract predictions to get edx residuals not explained by model
  # genre group effect
  genres_avg <- edx %>%
    left_join(movie_avg, by = 'movieId') %>%
    left_join(user_avg, by = 'userId') %>%
    mutate(bd = train_date_preds) %>% # rows already correspond to edx
    group_by(genres) %>%
    summarize(bgg =
                sum(rating - base_avg - bi - bu - bd)/(n() + l))
  
  # fitting (for each given lambda l)
  pred <-
    validation %>%
    left_join(movie_avg, by = 'movieId') %>%
    left_join(user_avg, by = 'userId') %>%
    mutate(bd_test = predict(date_effect_model, date)) %>% # fit model on dates in test set (validation) for results
    left_join(genres_avg, by = 'genres') %>%
    mutate(pred = base_avg + bi + bu + bd_test + bgg) %>%
    pull(pred)
  # grab RMSE
  return(RMSE(pred, validation$rating))
}

rmse_final(lambda_3a) 
final_rmse <- rmse_final(lambda_3a)

