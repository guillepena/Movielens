## ----RMSE_function1, echo = FALSE------------------------------------------------------------------------------------------------------------------------------------
RMSE <- function(predicted_ratings, true_ratings){
  sqrt(mean((predicted_ratings - true_ratings)^2))
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate")
if(!require(ggthemes)) install.packages("ggthemes")
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(ggthemes)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# using R 4.0
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

#Joining data
movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of the movieLens data
set.seed(1,sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# userId and movieId in validation set as in edx set assurance
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Remove objects we are not using
rm(dl, ratings, movies, test_index, temp, movielens, removed)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(edx)

summarize(edx)

edx %>% summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25, color = "black") +
  ggtitle("Rating distribution") +
  theme_economist(base_size = 10, base_family = "sans",
  horizontal = TRUE, dkpanel = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Number of movies") +
  ggtitle("Number of ratings per movie") +
  theme_economist(base_size = 10, base_family = "sans",
  horizontal = TRUE, dkpanel = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
edx %>%
  group_by(movieId) %>%
  summarize(count = n()) %>%
  filter(count == 1) %>%
  left_join(edx, by = "movieId") %>%
  group_by(title) %>%
  summarize(rating = rating, n_rating = count) %>%
  slice(1:20) %>%
  knitr::kable()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  xlab("Number of ratings") + 
  ylab("Number of users") +
  ggtitle("Number of ratings given by users") + 
  theme_economist(base_size = 10, base_family = "sans",
  horizontal = TRUE, dkpanel = FALSE)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot mean movie ratings given by users
edx %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black") +
  xlab("Mean rating") +
  ylab("Number of users") +
  ggtitle("Mean movie ratings given by users") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  theme_economist(base_size = 10, base_family = "sans",
  horizontal = TRUE, dkpanel = FALSE)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
avg <- mean(edx$rating)

model_avg_rmse <- RMSE(validation$rating, avg)

model_avg_rmse


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
rmse_results_df <- data_frame(method_used = "Average movie rating model", RMSE = model_avg_rmse)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
movie_avgs <- edx %>% group_by(movieId) %>% summarize(b_i = mean(rating - avg))
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"), ylab = "Number of movies", main = "Number of movies with the computed b_i") +
                theme_economist(base_size = 10, base_family = "sans", horizontal = TRUE, dkpanel = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
predicted_ratings <- avg +  validation %>% left_join(movie_avgs, by='movieId') %>% pull(b_i)
model_moviebias_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results_df <- bind_rows(rmse_results_df, data_frame(method_used="Movie bias effect model", RMSE = model_moviebias_rmse ))
model_moviebias_rmse


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
user_avgs <- edx %>% left_join(movie_avgs, by='movieId') %>% group_by(userId) %>% summarize(b_u = mean(rating - avg - b_i))
user_avgs %>% qplot(b_u, geom ="histogram", bins = 30, data = ., color = I("black"), ylab = "Number of movies", main = "Number of movies with the computed b_u") + 
              theme_economist(base_size = 10, base_family = "sans", horizontal = TRUE, dkpanel = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
predicted_ratings <- validation%>% left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId') %>% mutate(pred = avg + b_i + b_u) %>% pull(pred)
                                                             
model_movieuser_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results_df <- bind_rows(rmse_results_df, data_frame(method_used="Movie and user bias effect model", RMSE = model_movieuser_rmse))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
model_movieuser_rmse



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
lambdas <- seq(3, 8, 0.25)

rmses <- sapply(lambdas, function(l){
                                                               
                  avg <- mean(edx$rating)
 
                  b_i <- edx %>% group_by(movieId) %>% summarize(b_i = sum(rating - avg)/(n()+l))
                                                               
                  b_u <- edx %>% left_join(b_i, by="movieId") %>% group_by(userId) %>% summarize(b_u = sum(rating - b_i - avg)/(n()+l))
                                                               
                  predicted_ratings <- validation %>% left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% mutate(pred = avg + b_i + b_u) %>%
                                        pull(pred)
                                                               
                  return(RMSE(predicted_ratings, validation$rating))
                                    })


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
qplot(lambdas, rmses) + theme_economist(base_size = 10, base_family = "sans", horizontal = TRUE, dkpanel = FALSE)

usedlambda <- lambdas[which.min(rmses)]
usedlambda

rmse_results_df <- bind_rows(rmse_results_df, data_frame(method_used="Regularized movie and user bias effect model", RMSE = min(rmses)))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
genre_avgs <- edx %>% left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId') %>% group_by(genres)  %>% summarize(b_g = mean(rating - avg - b_i - b_u))
genre_avgs %>% qplot(b_g, geom ="histogram", bins = 30, data = ., color = I("black"), ylab = "Number of movies", main = "Number of movies with the computed b_g") + 
              theme_economist(base_size = 10, base_family = "sans", horizontal = TRUE, dkpanel = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
lambdas <- seq(3, 8, 0.25)

rmses <- sapply(lambdas, function(l){
                                                               
                  avg <- mean(edx$rating)
 
                  b_i <- edx %>% group_by(movieId) %>% summarize(b_i = sum(rating - avg)/(n()+l))
                                                               
                  b_u <- edx %>% left_join(b_i, by="movieId") %>% group_by(userId) %>% summarize(b_u = sum(rating - b_i - avg)/(n()+l))
                  
                  b_g <- edx %>% left_join(b_i, by='movieId') %>% left_join(b_u, by='userId') %>% group_by(genres)  %>% summarize(b_g = mean(rating - avg - b_i - b_u))
                                                               
                  predicted_ratings <- validation %>% left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% left_join(b_g,by="genres") %>% mutate(pred = avg + b_i + b_u + b_g) %>%
                                        pull(pred)
                                                               
                  return(RMSE(predicted_ratings, validation$rating))
                                    })

qplot(lambdas, rmses) + theme_economist(base_size = 10, base_family = "sans", horizontal = TRUE, dkpanel = FALSE)

usedlambda <- lambdas[which.min(rmses)]
usedlambda

rmse_results_df <- bind_rows(rmse_results_df, data_frame(method_used="Regularized with genre, movie and user bias effect model", RMSE = min(rmses)))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
rmse_results_df %>% knitr::kable()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
version

