#HarvardX: PH125.9x
#Data Science: Capstone
#Project: MovieLens
#Author: Giorgos Morakis


#Description: Code for the Movielens Capstone project, divided into sections for different parts of the analysis and report.
#             Given the computing and memory intensive nature of some procedures (especially data creation and regularization) 
#             It is recommended that code execution be done in small chunks.

#########################################################################################################################################################################################
# Install Required Packages
#########################################################################################################################################################################################

#Install packages if needed
if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(caret)) install.packages("caret") 
if(!require(data.table)) install.packages("data.table")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(stringr)) install.packages("stringr") 
if(!require(lubridate)) install.packages("lubridate") 
if(!require(rmarkdown)) install.packages("rmarkdown")
if(!require(ggplot2)) install.packages("ggplot2")

#########################################################################################################################################################################################
# Load Packages
#########################################################################################################################################################################################

# Load all packages to be used through the exercise
library(tidyverse)
library(kableExtra)
library(caret)
library(data.table)
library(stringr)
library(lubridate)
library(rmarkdown)
library(ggthemes)
library(ggplot2)

#########################################################################################################################################################################################
# Download and create edx set and validation set
#########################################################################################################################################################################################

# Create text using code from https://courses.edx.org/courses/course-v1:HarvardX+PH125.9x+2T2019/courseware/dd9a048b16ca477a8f0aaf1d888f0734/e8800e37aa444297a3a2f35bf84ce452/?child=last

# Note: this process could take a couple of minutes

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

# if using R 3.5 or earlier, use `set.seed(1)` instead
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

rm(dl, ratings, movies, test_index, temp, movielens, removed)


#########################################################################################################################################################################################
# Data Structure
#########################################################################################################################################################################################

# Explore edx and validation structure
str(edx)
str(validation)

# Check if there are any NAs in the data
summary(edx)
summary(validation)

#Seeing the first rows
head(edx)

#########################################################################################################################################################################################
# Initial Data Exploration and Adjustments
#########################################################################################################################################################################################

#Setting theme for all visualizations
theme_set(theme_classic())

# Identifying distribution of ratings
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Visualizing rating distribution, whole ratings are considerably more common than half star ratings.
edx %>%
  ggplot(aes(rating)) + 
  geom_histogram(bins = 10, color = "black") + 
  ggtitle("Ratings") + 
  labs(x = "Ratings", y = "Count")

# Identifying distinct movies, seems that there is a slight mismatch between titles and Id
n_distinct(edx$movieId)
n_distinct(edx$title)

  # Extracting the movie titles, we will save this data frame for later use
  movie_titles <- edx %>% 
    select(movieId, title) %>%
    distinct()
  
  # Confirming there is indeed a duplicate
  duplicated(movie_titles$title) %>% table()
  
  # Identifying duplicated film 
  movie_titles %>%
    filter(duplicated(movie_titles$title) == TRUE)
  
  # To which other Id "War of the Worlds (2005)" is associated
  movie_titles %>% 
    filter(title == "War of the Worlds (2005)")
  
  # Seeing how many times is repeated in edx and validation sets, the genres are also distinct
  edx %>%
    group_by(movieId, title, genres) %>%
    filter(title == "War of the Worlds (2005)") %>%
    count()
  
  validation %>%
    group_by(movieId, title, genres) %>%
    filter(title == "War of the Worlds (2005)") %>%
    count()
  
  # Replace movie Id and checking results
  edx$genres[edx$movieId == 64997] <- "Action|Adventure|Sci-Fi|Thriller"
  edx$movieId[edx$movieId == 64997] <- 34048
  
  edx %>%
    group_by(movieId) %>%
    filter(title == "War of the Worlds (2005)") %>%
    count()
  
  n_distinct(edx$movieId)
  n_distinct(edx$title)
  
  ### Where is the change for the validation sub set?
  
  movie_titles <- movie_titles[-which(movie_titles$movieId == 64997),]
  
# Movies with highest number of ratings
edx %>%
  group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Distribution of number of ratings per movie, some movies are rated more than others
edx %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies") +
  labs(x = "Number of ratings", y = "Count")

# Mean Movie Ratings
edx %>%
  group_by(movieId) %>%
  summarise(mean = mean(rating)) %>%
  ggplot(aes(mean)) +
  geom_histogram(bins = 30, color = "black") +
  ggtitle("Mean Rating per Movie") + 
  labs(x = "Mean", y = "Frequency")

  
# Identifying distinct users
n_distinct(edx$userId)

# Distribution of number of ratings by user, some users are more active raters
edx %>% 
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Users") +
  labs(x = "Number of ratings", y = "Count")

# Mean rating per user
edx %>%
  group_by(userId) %>%
  summarise(mean = mean(rating)) %>%
  ggplot(aes(mean)) +
  geom_histogram(bins = 30, color = "black") +
  ggtitle("Mean Rating per User") + 
  labs(x = "Mean", y = "Frequency")

#########################################################################################################################################################################################
# Pre-Processing Data Wrangling
#########################################################################################################################################################################################

# Extracting movie release year from the title

# Setting pattern to identify release year in movie's title
pattern <- "\\(\\d{4}\\)$"

# Seeing if pattern is present on all obervations of the edx and validation sets
sum(str_detect(edx$title, pattern))
sum(str_detect(validation$title, pattern))

# Seeing that patter extraction is succesful, we will use str_extract twice to keep only the year within the parenthesis
edx %>% 
  mutate(release_year = as.numeric(str_extract(str_extract(edx$title, pattern), "\\d{4}"))) %>%
  group_by(release_year) %>%
  arrange(release_year) %>%
  head()

# Creating the release_year variable
edx <- edx %>% 
  mutate(release_year = as.numeric(str_extract(str_extract(edx$title, pattern), "\\d{4}")))

# Seeing the new variable's structure
edx %>% head()

class(edx$release_year)

summary(edx$release_year)

# Earliest films
edx %>% group_by(release_year) %>%
  arrange(release_year) %>%
  head()

# Latest films
edx %>% group_by(release_year) %>%
  arrange(desc(release_year)) %>%
  head()

# Visualizing number of ratings by film's release year
edx %>% 
  group_by(movieId) %>%
  summarize(n = n(), release_year = as.character(first(release_year))) %>%
  qplot(release_year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Movie Ratings per Year") +
  labs(y = "Number of ratings") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90), 
        axis.ticks = element_blank())

# Seeing if a movie's age has an effect on the average rating
edx %>% 
  group_by(release_year) %>% 
  summarize(avg_rating_year = mean(rating)) %>%
  arrange(desc(avg_rating_year))

# Visualising average year rating, there seems to be a time effect
edx %>%
  group_by(release_year) %>%
  summarize(avg_rating_year = mean(rating)) %>%
  ggplot(aes(release_year, avg_rating_year)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Release Year vs Average Movie Rating") +
  labs(y = "Mean Rating") +
  theme(axis.title.x = element_blank())
  

#Timestamp

#Timestamp can be transformed into a more readable date
edx %>%
  mutate(rating_date = (as_datetime(timestamp))) %>%
  head()

# Perhaps there is a time effect on the rating weeks
edx %>%
  mutate(rating_date = round_date(as_datetime(timestamp), "week")) %>%
  group_by(rating_date) %>%
  summarize(avg_rating_date = mean(rating)) %>%
  arrange(desc(avg_rating_date))

# Visualizing if there is a time effect in the rating week, seems to be mild
edx %>%
  mutate(rating_date = round_date(as_datetime(timestamp), "week")) %>%
  group_by(rating_date) %>%
  summarize(avg_rating_date = mean(rating)) %>%
  ggplot(aes(rating_date, avg_rating_date)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Rating Week vs Average Movie Rating") +
  labs(y = "Mean Rating") +
  theme(axis.title.x = element_blank()) +
  scale_x_continuous(limits = c(2, 4.5))
  

# Visualizing if there is a time effect in the rating month, seems to be mild
edx %>%
  mutate(rating_date = round_date(as_datetime(timestamp), "month")) %>%
  group_by(rating_date) %>%
  summarize(avg_rating_date = mean(rating)) %>%
  ggplot(aes(rating_date, avg_rating_date)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Rating Month vs Average Movie Rating") +
  labs(y = "Mean Rating") +
  theme(axis.title.x = element_blank())

# We will add a rating_date variable for use, we will remove timestamp.
edx <- edx %>%
  mutate(rating_date = round_date(as_datetime(timestamp), "month")) %>%
  select(-timestamp)

summary(edx$rating_date)

#Earliest rating dates
edx %>% group_by(rating_date) %>%
  arrange(rating_date) %>%
  head()

#Latest rating dates
edx %>% group_by(rating_date) %>%
  arrange(desc(rating_date)) %>%
  head()

#########################################################################################################################################################################################
#Subsets from edx for making intermediate models
#########################################################################################################################################################################################

#Before testing modelling options, we will divide the edx into subsets

#Partition to divide edx into to subsets, we'll use a p similar to the relation between edx and validation
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index,]

#Temporary file 
temp <- edx[test_index,]

#Making sure that edx_train & edx_test contain the same movieId and UserId
edx_test <- temp %>%
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId") 

removed <- anti_join(temp, edx_test)

#Create final edx_train
edx_train <- rbind(edx_train, removed)
rm(temp, removed, test_index)

#Check bothg edx_train & edx_test
head(edx_train)
head(edx_test)

#########################################################################################################################################################################################
# Models
#########################################################################################################################################################################################

# Defining the Root Mean Square Error Function 
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Naive Model
mu_hat <- mean(edx_train$rating)
mu_hat

  # Naive Model Result
  naive_rmse <- RMSE(edx_test$rating, mu_hat)
  naive_rmse
  
  #Store result for future comparison
  rmse_results <- tibble(method = "Just the Average", RMSE = naive_rmse)
  rmse_results

#Movie Effect Model
mu <- mean(edx_train$rating)

movie_avgs <- edx_train %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

  # Visualizing
  movie_avgs %>%
    ggplot(aes(b_i)) +
    geom_histogram(bins = 10, color = "black") +
    ggtitle("Movie Effect") +
    labs(y = "Count")
  
  # Predicting
  predicted_ratings <- mu_hat + edx_test %>%
    left_join(movie_avgs, by = 'movieId') %>%
    .$b_i
  
  # Save result
  model_1_rmse <- RMSE(edx_test$rating, predicted_ratings)
  
  # Store result
  rmse_results <- bind_rows(rmse_results,
                            tibble(method = "Movie Effect Model",
                                   RMSE = model_1_rmse))
  
  rmse_results %>% knitr::kable()

# Adding User Effect
user_avgs <- edx_train %>%
  left_join(movie_avgs, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Visualizing
user_avgs %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 10, color = "black") +
  ggtitle("User Effect") +
  labs(y = "Count")

# Prediction
predicted_ratings <- edx_test %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(edx_test$rating, predicted_ratings)
rmse_results <- bind_rows(rmse_results,
                          tibble(method = "Movie + User Effect Model",
                                 RMSE = model_2_rmse))
# Storing new results
rmse_results %>% knitr::kable()

#Adding Release Year Effect
year_avgs <- edx_train %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = "userId") %>%
  group_by(release_year) %>%
  summarize(b_y = mean(rating - mu - b_i - b_u))

# Visualization
year_avgs %>%
  ggplot(aes(b_y)) +
  geom_histogram(bins = 10, color = "black") +
  ggtitle("Release Year Effect") +
  labs(y = "Count")

# Prediction
predicted_ratings <- edx_test %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(year_avgs, by = "release_year") %>%
  mutate(pred = mu + b_i + b_u + b_y) %>%
  .$pred

model_3_rmse <- RMSE(edx_test$rating, predicted_ratings)
rmse_results <- bind_rows(rmse_results,
                          tibble(method = "Movie + User + Year Effect Model",
                                 RMSE = model_3_rmse))
# Storing results
rmse_results %>% knitr::kable()

# Adding Rating Date effect
date_avgs <- edx_train %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(year_avgs, by = "release_year") %>%
  group_by(rating_date) %>%
  summarize(b_t = mean(rating - mu - b_i - b_u - b_y))

# Visualization
date_avgs %>%
  ggplot(aes(b_t)) +
  geom_histogram(bins = 20, color = "black") +
  ggtitle("Rating Date Effect") +
  labs(y = "Count")

# Prediction
predicted_ratings <- edx_test %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(year_avgs, by = "release_year") %>%
  left_join(date_avgs, by = "rating_date") %>%
  mutate(pred = mu + b_i + b_u + b_y + b_t) %>%
  .$pred

model_4_rmse <- RMSE(edx_test$rating, predicted_ratings)
rmse_results <- bind_rows(rmse_results,
                          tibble(method = "Movie + User + Year + Rating Date Effect Model",
                                 RMSE = model_4_rmse))

rmse_results %>% knitr::kable()
#########################################################################################################################################################################################
# Exploration to check for Regularization Needs
#########################################################################################################################################################################################
  
# Let's see the largest errors, not necessarily obscure movies
  edx_test %>%
  left_join(movie_avgs, by = 'movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%
  select(title, residual) %>% 
  slice(1:20) %>%
  knitr::kable()

# Best and worst movies and the number of times they were rated
edx_train %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:20) %>% 
  knitr::kable()

edx_train %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:20) %>% 
  knitr::kable()

# Films with lowest ratings
edx %>%
  group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(count)

#########################################################################################################################################################################################
# Cross Validation and Regularization
#########################################################################################################################################################################################

#Create data frame to store results
df.result <- data.frame("k" = integer(0),
                        "l" = numeric(0),
                        "RMSE" = numeric(0))

#Defining number of folds for cross validation
k <- 10

#Create k folds using caret package
set.seed(10)
folds <- createFolds(edx_train$rating, k = k, returnTrain = FALSE)

#Compute lambdas from 0 to 10
lambdas <- seq(0, 10, 0.25)

#REGULARIZATION LOOP FOR K-FOLDS, THIS PROCESS CAN TAKE MINUTES
for (i in 1:k){
  
  #separate the train/test set for the fold #i
  train_set <- edx_train[-folds[[i]],]
  temp <- edx_train[folds[[i]],]
  
  #Ensure userId and movieId coincide in both train and test set
  test_set <- temp %>%
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")
  
  #Adding rows removed from test_set back to train_set
  removed <- anti_join(temp, test_set)
  train_set <- rbind(train_set, removed)
  rm(removed, temp)
  
  print(paste("Computing RMSE for fold:", i, dim(train_set)[1], dim(test_set)[1]))
  
  #Model with lambdas
  
    for(l in lambdas){
      
      mu <- mean(train_set$rating)
      
      b_i <- train_set %>%
        group_by(movieId) %>%
        summarize(b_i = sum(rating - mu)/(n() + l))
      
      b_u <- train_set %>%
        left_join(b_i, by = "movieId") %>%
        group_by(userId) %>%
        summarize(b_u = sum(rating - mu - b_i)/(n() + l))
      
      b_y <- train_set %>%
        left_join(b_i, by = 'movieId') %>%
        left_join(b_u, by = "userId") %>%
        group_by(release_year) %>%
        summarize(b_y = sum(rating - mu - b_i - b_u)/(n() + l))
      
      b_t <- train_set %>%
        left_join(b_i, by = 'movieId') %>%
        left_join(b_u, by = "userId") %>%
        left_join(b_y, by = "release_year") %>%
        group_by(rating_date) %>%
        summarize(b_t = sum(rating - mu - b_i - b_u - b_y)/(n() + l))
      
      b_g <- train_set %>%
        left_join(b_i, by = 'movieId') %>%
        left_join(b_u, by = "userId") %>%
        left_join(b_y, by = "release_year") %>%
        left_join(b_t, by = "rating_date") %>%
        group_by(genres) %>%
        summarize(b_g = mean(rating - mu - b_i - b_u - b_y - b_t)/(n() + l))
      
      predicted_ratings <- test_set %>%
        left_join(b_i, by = 'movieId') %>%
        left_join(b_u, by = "userId") %>%
        left_join(b_y, by = "release_year") %>%
        left_join(b_t, by = "rating_date") %>%
        left_join(b_g, by = "genres") %>%
        mutate(pred = mu + b_i + b_u + b_y + b_t + b_g) %>%
        .$pred
      
      kfold_rmse <- RMSE(test_set$rating, predicted_ratings)
    
    #print
    df.result <- rbind(df.result, data.frame(k = i, l = l, RMSE = kfold_rmse))
    
  }
}

head(df.result, n = 20)

#Choosing the best lambda, in this case is equal to

#Arranging results per lambda
df.result_adj <- df.result %>% 
  group_by(l) %>%
  summarize(minRMSE = min(RMSE), medianRMSE = median(RMSE), meanRMSE = mean(RMSE), maxRMSE = max(RMSE))

head(df.result_adj, n = 20)

#Visualization
df.result_adj %>% group_by(l) %>%
  ggplot(aes(l, meanRMSE)) +
  geom_point() +
  ggtitle("Mean RMSE per lambda") +
  labs(y = "avg RMSE", x = "lambda")

#Visualization
df.result_adj %>% group_by(l) %>%
  ggplot(aes(l, minRMSE)) +
  geom_point() +
  ggtitle("Minimum RMSE per lambda") +
  labs(y = "min RMSE", x = "lambda")

#Extracting the best lambda
kv_best <- df.result_adj[df.result_adj$meanRMSE == min(df.result_adj$meanRMSE),]

#Showing the best lambda
kv_best %>% knitr::kable()

l <- kv_best$l

#The best lambda turns out to be 5
l

#Model with tuned lambda tested with edx_test
mu <- mean(edx_train$rating)

b_i <- edx_train %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n() + l))

b_u <- edx_train %>%
  left_join(b_i, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n() + l))

b_y <- edx_train %>%
  left_join(b_i, by = 'movieId') %>%
  left_join(b_u, by = "userId") %>%
  group_by(release_year) %>%
  summarize(b_y = sum(rating - mu - b_i - b_u)/(n() + l))

b_t <- edx_train %>%
  left_join(b_i, by = 'movieId') %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_y, by = "release_year") %>%
  group_by(rating_date) %>%
  summarize(b_t = sum(rating - mu - b_i - b_u - b_y)/(n() + l))

b_g <- edx_train %>%
  left_join(b_i, by = 'movieId') %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_y, by = "release_year") %>%
  left_join(b_t, by = "rating_date") %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u - b_y - b_t)/(n() + l))

predicted_ratings <- edx_test %>%
  left_join(b_i, by = 'movieId') %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_y, by = "release_year") %>%
  left_join(b_t, by = "rating_date") %>%
  left_join(b_g, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_y + b_t + b_g) %>%
  .$pred

model_reg_rmse <- RMSE(edx_test$rating, predicted_ratings)
model_reg_rmse

rmse_results <- bind_rows(rmse_results,
                          tibble(method = "Regularized Movie + User + Year + Review Date + Genre Effect Model",
                                 RMSE = model_reg_rmse))

rmse_results %>% knitr::kable()

#########################################################################################################################################################################################
# Final Model
#########################################################################################################################################################################################

# Preparing Validation set

  # Corecting for War of the Worlds (2005)
  validation$genres[validation$movieId == 64997] <- "Action|Adventure|Sci-Fi|Thriller"
  validation$movieId[validation$movieId == 64997] <- 34048
  
  # Adding release_year and rating_date to the validation set
  validation <- validation %>% 
    mutate(release_year = as.numeric(str_extract(str_extract(validation$title, pattern), "\\d{4}")), 
           rating_date = round_date(as_datetime(timestamp), "month"))
  
# Final Model with Validation set

mu <- mean(edx$rating)

b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n() + l))

b_u <- edx %>%
  left_join(b_i, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n() + l))

b_y <- edx %>%
  left_join(b_i, by = 'movieId') %>%
  left_join(b_u, by = "userId") %>%
  group_by(release_year) %>%
  summarize(b_y = sum(rating - mu - b_i - b_u)/(n() + l))

b_t <- edx %>%
  left_join(b_i, by = 'movieId') %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_y, by = "release_year") %>%
  group_by(rating_date) %>%
  summarize(b_t = sum(rating - mu - b_i - b_u - b_y)/(n() + l))

b_g <- edx %>%
  left_join(b_i, by = 'movieId') %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_y, by = "release_year") %>%
  left_join(b_t, by = "rating_date") %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u - b_y - b_t)/(n() + l))

predicted_ratings <- validation %>%
  left_join(b_i, by = 'movieId') %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_y, by = "release_year") %>%
  left_join(b_t, by = "rating_date") %>%
  left_join(b_g, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_y + b_t + b_g) %>%
  .$pred

model_final_rmse <- RMSE(validation$rating, predicted_ratings)

# Final Result
model_final_rmse

rmse_results <- bind_rows(rmse_results,
                          tibble(method = "Final Regularized Model",
                                 RMSE = model_final_rmse))
# Final result table
rmse_results %>% knitr::kable()



count <- edx %>% group_by(genres) %>% summarise(rating_mean = mean(rating), n = n()) %>% as.data.frame() %>% 
  separate_rows(genres, sep = "\\|") %>% group_by(genres) %>% summarize(count_ratings = sum(n), mean_rating = mean(rating_mean)) %>% 
  arrange(desc(count_ratings))

genre_count <- data.frame(count)

train_set %>% mutate(main_genre = split(genres, sep = "\\|"))


#Defining number of folds for cross validation
k <- 100

#Create k folds using caret package
set.seed(10)
folds <- createFolds(edx_train$rating, k = k, returnTrain = FALSE)
train_set <- edx_train[folds[[1]],]
test_set <- edx_train[folds[[2]],] 

fit <- randomForest(rating ~ movieId + userId +  release_year + rating_date,
                    data=train_set, 
                    importance=TRUE, 
                    ntree=200)
varImp(fit)
varImpPlot(fit)

p_fit <- predict(fit, test_set)
RMSE(test_set$rating, p_fit)


#########################################################################################################################################################################################
# FMatrix Factorization
#########################################################################################################################################################################################

if(!require(recosystem)) 
  install.packages("recosystem", repos = "http://cran.us.r-project.org")

library(recosystem)

edx_train_factm <- edx_train %>% select(movieId,userId,rating) %>% as.matrix()
edx_test_factm <- edx_test %>% select(movieId,userId,rating) %>% as.matrix()
validation_factm <- validation %>% select(movieId,userId,rating) %>% as.matrix()


# Convert the train and test sets into recosystem input format, Recosystem needs to save the files as tables on hard disk
write.table(edx_train_factm , file = "trainset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(edx_test_factm , file = "testset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)

write.table(validation_factm, file = "validationset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)

# data_file is specific recosystem command

set.seed(1)
training_dataset <- data_file( "trainset.txt")
test_dataset <- data_file( "testset.txt")

validation_dataset <- data_file( "validationset.txt")

# We reate a model object (a Reference Class object in R) by calling the function Reco()

r = Reco()
opts = r$tune(training_dataset, opts = list(dim = seq(10,50,10), 
                                            lrate = c(0.1, 0.2),
                                            costp_l2 = c(0.01, 0.1), 
                                            costq_l2 = c(0.01, 0.1),costp_l1 = 0, costq_l1 = 0,
                                            nthread = 1, niter = 30))

opts$res %>% group_by(dim) %>% summarize(loss_fun_min = min(loss_fun)) %>% arrange(dim)

opts$min
opts$res

opts$res   %>%
  ggplot(aes(x = dim, y = loss_fun)) +
  geom_point() +
  geom_abline(aes(intercept = min(loss_fun), slope = 0))


#values_opts_mf <- as.data.frame(opts$res)

set.seed(1)
r$train(training_dataset, opts = c(opts$min, nthread = 1, niter = 30))

# We write predictions to a tempfile on HDisk

stored_prediction = tempfile() 

r$predict(test_dataset, out_file(stored_prediction))  

real_ratings <- read.table("testset.txt", header = FALSE, sep = " ")$V3

pred_ratings <- scan(stored_prediction)

RMSE(real_ratings,pred_ratings)




#  

r$predict(validation_dataset, out_file(stored_prediction)) 

real_ratings <- read.table("validationset.txt", header = FALSE, sep = " ")$V3

pred_ratings <- scan(stored_prediction)

RMSE(real_ratings,pred_ratings)










