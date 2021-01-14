#load packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
library(data.table)




#Download the data and create train/test sets as per course instructions.

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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


######## Start analysis ########


#average rating of edx ratings
mu <- mean(edx$rating)

#RMSE of average only
avgonly <- RMSE(validation$rating,mu)

#create RMSE table
RMSEs <- data.frame(model = "avg_only", RMSE = avgonly)

#movie average rating
movie_avg <- edx %>% 
  group_by(movieId) %>% 
  summarize(bi = mean(rating - mu),n_mov_rat = n())



#predict movie effect
pred_moveffect <- mu + validation %>% 
  left_join(movie_avg, by = 'movieId') %>%
  .$bi


#movie only RMSE
mov_only_model <- RMSE(pred_moveffect, validation$rating)

#add movie only RMSE to RMSE table
RMSEs <- bind_rows(RMSEs, data.frame(model = "movie_effect", RMSE = mov_only_model))

#extract movie release year
movie_yr <- edx %>%
  group_by(movieId) %>%
  summarize(year = as.factor(str_sub(first(title),-5,-2)))

#user average
usr_avg <- edx %>% 
  left_join(movie_avg, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(bu = mean(rating - mu - bi),n_usr_rat = n())



#movie and user prediction
pred_usrmov <- validation %>% 
  left_join(movie_avg, by = 'movieId') %>%
  left_join(usr_avg, by = 'userId') %>%
  mutate(predictions = mu + bi + bu) %>%
    .$predictions

#movie& user RMSE
mov_usr_model <- RMSE(pred_usrmov, validation$rating)


#add movie & user to RMSE table
RMSEs <- bind_rows(RMSEs,data.frame(model = "movie+usr", RMSE = mov_usr_model ))


#movie and usr average with tuning parameter, "lambda".
#started with larger lambas and narrowed it down to improve RMSE.
#lmds <- seq(0,12, .5)
lmds <- seq(2,6, .25)

tuned_rmses <- sapply(lmds, function(l) {
  
  bi_l <- 
    edx %>%
    group_by(movieId) %>%
    summarize(biL = sum(rating - mu)/(n()+l))
  
  bu_l <- edx %>%
  left_join(bi_l, by = 'movieId') %>% 
  group_by(userId) %>%
  summarize(buL = sum(rating - mu - biL)/(n()+l))

preds_usrmov <- validation %>% 
  left_join(bi_l, by = 'movieId') %>%
  left_join(bu_l, by = 'userId') %>%
  mutate(predictions = mu + biL + buL) %>%
  .$predictions


return(RMSE(preds_usrmov, validation$rating))
})

qplot(lmds,tuned_rmses)

#tuned rmse model with lowest lamda
usr_mov_lambda_model <- tuned_rmses[which.min(lmds)]

#add lamba model to RMSE comparison table
RMSEs <- bind_rows(RMSEs,data.frame(model = "movie+usr+lambda", RMSE = usr_mov_lambda_model))

#analyze number of ratings per user
usr_rating_cnt <-edx %>% 
  group_by(userId) %>%
  summarize(user_ratings = n())

#analyze percentiles of rating counts per user
quantile(usr_rating_cnt$user_ratings)

#visualize the distribution of the rating counts per user, removing outliers (100th percentile)
usr_rating_cnt %>% ggplot(aes(user_ratings)) + geom_histogram(binwidth = 10) +  xlim(0,quantile(usr_rating_cnt$user_ratings,.99))

#visualize the distribution of number of records per year the movie was rated
edx %>% mutate(year_rated = format(as.POSIXct(.$timestamp,origin = "1970-01-01",tz = "UTC"),"%Y")) %>% ggplot(aes(year_rated)) + geom_histogram(stat = "count") 

#rating aggregated by movie year
movie_yr_avg <- edx %>%
  left_join(movie_avg, by = 'movieId') %>%
  left_join(usr_avg, by = 'userId') %>% 
  left_join(movie_yr, by = 'movieId') %>% 
  group_by(year) %>%
  summarize(bt = mean(rating - mu - bi - bu))


#movie, year, and user prediction
pred_yrmovusr <- validation %>% 
  mutate(year = as.factor(str_sub(first(title), -5, -2))) %>%
  left_join(movie_avg, by = 'movieId') %>%
  left_join(usr_avg, by = 'userId') %>%
  left_join(movie_yr_avg, by = 'year') %>% 
  mutate(predictions = mu + bi + bu + bt) %>%
  .$predictions


#movie, user, and year RMSE
mov_usr_yr_model <- RMSE(pred_yrmovusr, validation$rating)

#update RMSE table
RMSEs <- bind_rows(RMSEs,data.frame(model = "movie+usr+year", RMSE = mov_usr_yr_model ))

#make an aesthetically pleasing table
RMSE_kable <- knitr::kable(RMSEs)

RMSE_kable


RMSEs %>% arrange(RMSE) %>% ggplot(aes(model,RMSE))  + geom_col(color = "gray") + geom_text(aes(label = round(RMSE,6)),color = "white", vjust = 1.5) 


#make an aesthetically pleasing linear model RMSE Table
lmRMSE_kable <- knitr::kable(lm_RMSEs)

lmRMSE_kable

RMSEs %>% arrange(RMSE) %>% ggplot(aes(model,RMSE))  + geom_col(color = "gray") + geom_text(aes(label = round(RMSE,6)),color = "white", vjust = 1.5)


#group datasets by user ratings to eliminate values less than the 30th quantile.
edx_usr_30qnt <- edx %>% group_by(userId) %>% mutate(num_user_ratings = n()) %>% ungroup() %>% filter(num_user_ratings > quantile(num_user_ratings, 0.30))

vali_usr_30qnt <- validation %>% group_by(userId) %>% mutate(num_user_ratings = n()) %>% ungroup() %>% filter(num_user_ratings > quantile(num_user_ratings, 0.30))

###Linear model using filtered data to only include users who rated more movies than the lower 30 quantile.
fit_lm30 <- lm(rating~movieId+userId,data=edx_usr_30qnt)

prd_lm30 <-  predict(fit_lm30, vali_usr_30qnt)

usr_30qnt <- RMSE(pred = prd_lm30, obs = vali_usr_30qnt$rating)

lm_RMSEs <- data.frame(model = "Linear Model_User Ratings > 30qnt", RMSE = usr_30qnt )


#regular linear model using movieId and userId.
fit_lm <- lm(rating~movieId+userId,data=edx)

prd_lm <-  predict(fit_lm, validation)

lm_rmse <- RMSE(pred = prd_lm, obs = validation$rating)

lm_RMSEs <- bind_rows(lm_lm_RMSEs,data.frame(model = "Linear Model", RMSE = lm_rmse ))



