##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

# Installing required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2");
if(!require(stringr)) install.packages("string");
if(!require(tidyr)) install.packages("tidyr");
if(!require(data.table)) install.packages("data.table")
if(!require(summarytools)) install.packages("summarytools")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(kableExtra)) install.packages(("kableExtra"))

# Loading required packages
library(tidyverse)
library(caret)
library(rafalib)
library(ggplot2)
library(knitr)
library(raster)
library(dslabs)
library(data.table)
library(dplyr)
library(summarytools)
library(gridExtra)
library(kableExtra)


#---------------Helper Functions-------------------------------------------
#Basic function to report the basic stats of a category
statFunc <- function(inData, group, groupy = TRUE) {
  if( groupy ) {
    localData <- inData %>% group_by(inData[[group]])
  }
  else {
    localData <- inData
  }
  
  localData %>% summarize(ratings=n(), avg=mean(rating), med=median(rating),
                          stdev=sd(rating), high=max(rating), low=min(rating))
}

# The RMSE function that will be used for calculations
RMSE <- function(verificationRations = NULL, predictedRatings = NULL) {
  sqrt(mean((verificationRations - predictedRatings)^2))
}

#---------------End Helper Functions---------------------------------------


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

#This code below is provided by the course so there's not going to be many comments
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
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
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

# ---------------Begin Introduction/Overview section-----------------------
#Here's a glimpse of the data set
edx %>% as_tibble()

#In an effort to get a feel for the data I'm going to take a few basic statistics
uniqueUsers = length(unique(edx$userId));
uniqueMovies = length(unique(edx$movieId));
uniqueGenres = str_extract_all(unique(edx$genres), "[^|]+") %>%
                unlist() %>%
                unique()


#Calculate the statistics group by a column (except for total stats)
totalStats = statFunc(edx, FALSE, FALSE);
userIdStats = statFunc(edx, "userId", TRUE);
movieStats = statFunc(edx, "movieId", TRUE);
genreStats = statFunc(edx, "genres", TRUE)

colnames(totalStats) <- c("Ratings", "Avg", "Median", "Std. Dev", "High", "Low")
kable(totalStats, format = "html")

tab <- cbind(uniqueUsers,uniqueMovies,length(uniqueGenres), length(unique(edx$genres)))
rownames(tab) <- 'Data Set'
colnames(tab) <- c('Users', 'Movies', 'Genres', 'Total Genres');
kable(tab, format = "html")

#There seems to be more information available by parsing out the timestamp
#Extract the timestamp and convert it to a human readable date
edx$date <- as.POSIXct(edx$timestamp, origin="1970-01-01");

#Pull out the year and month from the timestamp of the rating
edx$ratingYear = as.numeric(format(edx$date, "%Y"));
edx$ratingMonth = as.numeric(format(edx$date, "%m"));

# Get the year of the movie. Each movie title seemed to have the release year appended to the end
# Get the parenthesis and what is inside
k <- str_extract_all(edx$title, "\\([^()]+\\)$");
# Remove parenthesis
k <- substring(k, 2, nchar(k)-1);
# Remove all unwanted leftover special characters and transpose the data so it will
# be a new column in the data frame
k <- as.data.frame(matrix(as.numeric(gsub(")","", k))));
edx$movieYear = k$V1;
edx$yearDelta = as.numeric(edx$ratingYear)-as.numeric(edx$movieYear);

#Separate the genres, this will make multiple movie entries for titles that have multiple genres listed
edx <- edx %>% separate_rows(genres,sep = "\\|") %>% mutate(value=1)
genresIndependent <- edx %>% group_by(genres) %>% summarize(n=n())

#Do the same thing above for the final_holdout_test data. Should make this a function but that's for next time.
#There seems to be more information available by parsing out the timestamp
#Extract the timestamp and convert it to a human readable date
final_holdout_test$date <- as.POSIXct(final_holdout_test$timestamp, origin="1970-01-01");

#Pull out the year and month from the timestamp of the rating
final_holdout_test$ratingYear = as.numeric(format(final_holdout_test$date, "%Y"));
final_holdout_test$ratingMonth = as.numeric(format(final_holdout_test$date, "%m"));

# Get the year of the movie. Each movie title seemed to have the release year appended to the end
# Get the parenthesis and what is inside
k <- str_extract_all(final_holdout_test$title, "\\([^()]+\\)$");
# Remove parenthesis
k <- substring(k, 2, nchar(k)-1);
# Remove all unwanted leftover special characters and transpose the data so it will
# be a new column in the data frame
k <- as.data.frame(matrix(as.numeric(gsub(")","", k))));
final_holdout_test$movieYear = k$V1;
final_holdout_test$yearDelta = as.numeric(final_holdout_test$ratingYear)-as.numeric(final_holdout_test$movieYear);

#Separate the genres, this will make multiple movie entries for titles that have multiple genres listed
#Separate the genres for the final test data as it will be needed later ;) ;)
final_holdout_test <- final_holdout_test %>% separate_rows(genres,sep = "\\|") %>% mutate(value=1)

# --------------- Initial Plots of data set --------------------------------
#Plotting some of the data to get an idea of how things look to determine what variables
#are important for the prediction process

#Raw data plots before any real analysis
ratingsPerTitle <- ggplot(movieStats, aes(`inData[[group]]`, ratings/1000))+geom_col(col="blue") + 
                    labs(
                      title = "Ratings per movie",
                      x = "Movie ID",
                      y = "Ratings/1000"
                    ) +
                    theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))

ratingsPerUser <- ggplot(userIdStats, aes(`inData[[group]]`, ratings/1000))+geom_col(col="blue") + 
                    labs(
                      title = "Ratings per user",
                      x = "User ID",
                      y = "Ratings/1000"
                    ) +
                    theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))

ratingsPerGenre <- ggplot(genreStats, aes(1:length(genreStats[[1]]), ratings/1000))+geom_col(col="blue") + 
                    labs(
                      title = "Ratings per genre",
                      x = "Genre Category",
                      y = "Ratings/1000"
                    ) +
                    theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))

grid.arrange(ratingsPerTitle, ratingsPerUser, ratingsPerGenre, ncol = 3);

#More meaningful plots of data after some processing
#Mean of ratings per some feature
meanRatingPerMonth <-  edx %>%
                    group_by(ratingMonth) %>%
                    summarize(meanRating = mean(rating)) %>%
                    ggplot(aes(ratingMonth, meanRating)) +
                    geom_col(col="blue") + 
                    labs(
                      title = "Mean rating by month", 
                      x = "Rating month",
                      y = "Rating mean"
                    ) +
                    theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))

meanRatingPerYear <-  edx %>%
  group_by(ratingYear) %>%
  summarize(meanRating = mean(rating)) %>%
  ggplot(aes(ratingYear, meanRating)) +
  geom_col(col="blue") + 
  labs(
    title = "Mean rating by year", 
    x = "Rating year",
    y = "Rating mean"
  ) +
  theme(axis.text.x = element_text(angle=90, hjust=1))

meanRatingsvsDelta <- edx %>%
                    group_by(yearDelta) %>%
                    summarize(deltamean = mean(rating)) %>%
                    ggplot(aes(yearDelta, deltamean)) +
                    geom_col(col="blue") + 
                    labs(
                      title = "Mean rating by (Rating year - Release year)", 
                      x = "Rating year - Release year",
                      y = "Rating mean"
                    ) +
                    theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))


grid.arrange(meanRatingPerMonth, meanRatingPerYear, meanRatingsvsDelta, ncol = 3);

#Median of ratings per some feature
medianRatingPerMonth <- edx %>%
                          group_by(ratingMonth) %>%
                          summarize(medianRating = median(rating)) %>%
                          ggplot(aes(ratingMonth, medianRating)) +
                          geom_col(col="blue") + 
                          labs(
                            title = "Median rating by month", 
                            x = "Rating month",
                            y = "Rating median"
                          ) +
                          theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))

medianRatingPerYear <-edx %>%
                          group_by(ratingYear) %>%
                          summarize(medianRating = median(rating)) %>%
                          ggplot(aes(ratingYear, medianRating)) +
                          geom_col(col="blue") + 
                          labs(
                            title = "Median rating by year", 
                            x = "Rating year",
                            y = "Rating median"
                          ) +
                          theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))

medianRatingsvsDelta <- edx %>%
                          group_by(yearDelta) %>%
                          summarize(deltaMedian = median(rating)) %>%
                          ggplot(aes(yearDelta, deltaMedian)) +
                          geom_col(col="blue") + 
                          labs(
                            title = "Median rating by (Rating year - Release year)", 
                            x = "Rating year - Release year",
                            y = "Rating median"
                          ) +
                          theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))

grid.arrange(medianRatingPerMonth, medianRatingPerYear, medianRatingsvsDelta, ncol = 3);

#Distribution plots
#Ratings per user histogram
ratingsPerUser <- userIdStats %>%
                    ggplot(aes(x=ratings)) +
                    geom_histogram(bins=50, fill="blue") +
                    scale_x_log10() +
                    labs(title = "Log Distribution of Number of Ratings per User",
                         x = "Log Number of Ratings",
                         y = "Number of Users"
                    )+
                    theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))
ratingsPerUser

ratingsPerMovie <- movieStats %>%
  ggplot(aes(x=ratings)) +
  geom_histogram(bins=50, fill="blue") +
  scale_x_log10() +
  labs(title = "Log Distribution of Number of Ratings per Title",
       x = "Log Number of Ratings",
       y = "Number of Users"
  )+
  theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))
ratingsPerMovie

grid.arrange(ratingsPerMovie, ratingsPerUser, nrow = 2);

# Mean rating per movie 
meanPerTitle <- edx %>%
                  group_by(movieId) %>%
                  summarise(meanRating = mean(rating)) %>%
                  ggplot(aes(meanRating)) +
                  geom_histogram(bins=20, fill = "blue") +
                  labs(title = "Mean Distribution per Title",
                       x = "Mean Rating",
                       y = "Frequency"
                  )+
                  theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))

medianPerTitle <- edx %>%
                    group_by(movieId) %>%
                    summarise(medianRating = median(rating)) %>%
                    ggplot(aes(medianRating)) +
                    geom_histogram(bins=20, fill = "blue") +
                    labs(title = "Median Distribution per Title",
                         x = "Median Rating",
                         y = "Frequency"
                    )+
                    theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))
# Mean rating per user 
meanPerUser <- edx %>%
                group_by(userId) %>%
                summarise(meanRating = mean(rating)) %>%
                ggplot(aes(meanRating)) +
                geom_histogram(bins=20, fill = "blue") +
                labs(title = "Mean Distribution per User",
                     x = "Mean Rating",
                     y = "Frequency"
                )+
                theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))

medianPerUser <- edx %>%
                  group_by(userId) %>%
                  summarise(medianRating = median(rating)) %>%
                  ggplot(aes(medianRating)) +
                  geom_histogram(bins=20, fill = "blue") +
                  labs(title = "Median Distribution per User",
                       x = "Median Rating",
                       y = "Frequency"
                  )+
                  theme(axis.text.x = element_text(angle=90, hjust=1),plot.title = element_text(hjust = 0.5))

grid.arrange(meanPerTitle, medianPerTitle, meanPerUser, medianPerUser, nrow = 2, ncol = 2)

#Top rated movies
topMoviesFrequency <- edx %>%
  group_by(title) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(n=20) %>%
  ggplot(aes(title, count)) +
  geom_col(fill = "blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Top 20 movies ratings disribution",
       x = "Title",
       y = "Frequency")
topMoviesFrequency

edx %>%
  group_by(title) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(n=20) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 12,
                full_width = TRUE)


# --------------- Analysis section ----------------------------------------
#Just a note that this is using the a separating train/test data set than is 
#provided by the edx course.  This is due to the class instructions that the 
#final test data should only be used on the final model and not the interim
#versions

#---------------Partitioning training and test sets Functions-------------------------------------------
# The project instructions say to not use the final holdout data until the final model so here we're going
# to make a training and test data set from the edx data
testIndex <-createDataPartition(y = edx$rating, times = 1, p = 0.1, list = F)
trainEdx <-edx[-testIndex,]
tempEdx <-edx[testIndex,]

#Again, Make sure userId and movieId are in both the train and test sets
testEdx <- tempEdx %>%
  semi_join(trainEdx, by = "movieId") %>%
  semi_join(trainEdx, by = "userId")

#Add the Rows removed from the edx_test back into edx_train
removed <-anti_join(tempEdx, testEdx)
trainEdx <-rbind(trainEdx, removed)
rm(tempEdx, testIndex, removed)

#------------------ Simple Average  -----------------------------------
# Y = u + epsilon

mu <- mean(trainEdx$rating)
meanRMSE <- RMSE(testEdx$rating, mu)

resultsMean <- tibble(Method = "Mean", RMSE = meanRMSE)
kable(resultsMean) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 12,
                full_width = TRUE)

# --------------- Movie Effect model -----------------------------------
# Y = u + b + epsilon
titleFit <- trainEdx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

pred <- testEdx %>%
  left_join(titleFit, by = "movieId") %>% 
  mutate(y = mu + b_i) %>%
  pull(y)

pred <- clamp(pred, 0.5, 5)

resultsMovieEffect <- RMSE(pred, testEdx$rating)

resultsMean <- bind_rows(
  resultsMean,
  tibble(
    Method = "Movie Effect Model",
    RMSE = resultsMovieEffect
  )
)

kable(resultsMean) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 12,
                full_width = TRUE)


# --------------- Movie Effects + user effects model -----------------------------------
# Y = u + b_i + b_u + epsilon

userFit <- trainEdx %>%
  left_join(titleFit, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

pred <- testEdx %>%
  left_join(titleFit, by = "movieId") %>%
  left_join(userFit, by = "userId") %>%
  mutate(y = mu + b_i + b_u) %>%
  pull(y)

pred <- clamp(pred, 0.5, 5)

resultsMoviePlusUserEffect <- RMSE(pred, testEdx$rating)

resultsMean <- bind_rows(
  resultsMean,
  tibble(
    Method = "Movie + User Effect",
    RMSE = resultsMoviePlusUserEffect
  )
)

kable(resultsMean) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 12,
                full_width = TRUE)


# --------------- Movie + user + year delta effects model -----------------------------------
# Y = u + b_i + b_u + b_y + epsilon

dateDelta <- trainEdx %>%
  left_join(titleFit, by = "movieId") %>%
  left_join(userFit, by = "userId") %>%
  group_by(yearDelta) %>%
  summarize(b_y = mean(rating - mu - b_i - b_u))

pred <- testEdx %>%
  left_join(titleFit, by = "movieId") %>%
  left_join(userFit, by = "userId") %>%
  left_join(dateDelta, by = "yearDelta") %>%
  mutate(y = mu + b_i + b_u + b_y) %>%
  pull(y)

pred <- clamp(pred, 0.5, 5)

resultsMoviePlusUserEffectPluseDateDelta <- RMSE(pred, testEdx$rating)
resultsMean <- bind_rows(
  resultsMean,
  tibble(
    Method = "Movie + User + Year delta Effect",
    RMSE = resultsMoviePlusUserEffectPluseDateDelta
  )
)

kable(resultsMean) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 12,
                full_width = TRUE)

# --------------- Movie + user + year effects model -----------------------------------
# Y = u + b_i + b_u + b_y + epsilon

yearOfMovie <- trainEdx %>%
  left_join(titleFit, by = "movieId") %>%
  left_join(userFit, by = "userId") %>%
  group_by(ratingYear) %>%
  summarize(b_y = mean(rating - mu - b_i - b_u))

pred <- testEdx %>%
  left_join(titleFit, by = "movieId") %>%
  left_join(userFit, by = "userId") %>%
  left_join(yearOfMovie, by = "ratingYear") %>%
  mutate(y = mu + b_i + b_u + b_y) %>%
  pull(y)

pred <- clamp(pred, 0.5, 5)

resultsMoviePlusUserEffectPluseDateDelta <- RMSE(pred, testEdx$rating)
resultsMean <- bind_rows(
  resultsMean,
  tibble(
    Method = "Movie + User + Movie year Effect",
    RMSE = resultsMoviePlusUserEffectPluseDateDelta
  )
)

kable(resultsMean) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 12,
                full_width = TRUE)

# --------------- Movie + user + genre effects model -----------------------------------
# Y = u + b_i + b_u + b_g + epsilon

# Separate the genres out so a movie can be listed by individual genre and not the combined list
genreAvg <- trainEdx %>%
  left_join(titleFit, by = "movieId") %>%
  left_join(userFit, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))

pred <- testEdx %>%
  left_join(titleFit, by = "movieId") %>%
  left_join(userFit, by = "userId") %>%
  left_join(genreAvg, by = "genres") %>%
  mutate(y = mu + b_i + b_u + b_g) %>%
  pull(y)

pred <- clamp(pred, 0.5, 5)

resultsMoviePlusUserEffectPluseDateDelta <- RMSE(pred, testEdx$rating)
resultsMean <- bind_rows(
  resultsMean,
  tibble(
    Method = "Movie + User + Genre Effect",
    RMSE = resultsMoviePlusUserEffectPluseDateDelta
  )
)

kable(resultsMean) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 12,
                full_width = TRUE)

# --------------- Movie + user + year delta regularized effects model -----------------------------------
# Y = u + b_i + b_u + b_y + epsilon
# Regularized parameter
lambdas <- seq(0, 10, 0.5)

# Grid search to tune the regularized parameter lambda
rmses <- sapply(lambdas, function(l) {
  mu <- mean(trainEdx$rating)
  
  b_i <- trainEdx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu) / (n() + l))
  
  b_u <- trainEdx %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu) / (n() + l))
  
  b_y <- trainEdx %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(yearDelta) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u) / (n() + l))
  
  predicted_ratings <- testEdx %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by = "yearDelta") %>%
    mutate(pred = mu + b_i + b_u + b_y) %>%
    pull(pred)
  
  predicted_ratings <- clamp(predicted_ratings, 0.5, 5)
  
  return(RMSE(predicted_ratings, testEdx$rating))
})

plot_rmses <- qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]

resultsMean <- bind_rows(
  resultsMean,
  tibble(
    Method = "Regularized Movie + User + Genre delta Effect",
    RMSE = min(rmses)
  )
)

kable(resultsMean) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 12,
                full_width = TRUE)

# Results section ---------------------------------------------------------
# We're not going to use the best model on the final data
# The model that worked the best was the Movie + User + Independent Genre one
# so that's what I'm going to run it on the final_holdout_test data
# --------------- Movie + User + Independent Genre Effect model------------
# Y = u + b_i + b_u + b_g  + epsilon
titleFit <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

userFit <- edx %>%
  left_join(titleFit, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

genreFit <- edx %>%
  left_join(titleFit, by = "movieId") %>%
  left_join(userFit, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))

pred <- final_holdout_test %>%
  left_join(titleFit, by = "movieId") %>%
  left_join(userFit, by = "userId") %>%
  left_join(genreAvg, by = "genres") %>%
  mutate(y = mu + b_i + b_u + b_g) %>%
  pull(y)

pred <- clamp(pred, 0.5, 5)

finalRMSE <- RMSE(pred, final_holdout_test$rating)
resultsMean <- tibble(Method = "Movie + User + Independent Genre Effects", RMSE = finalRMSE)
kable(resultsMean) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 12,
                full_width = TRUE)

