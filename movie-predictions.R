if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(Metrics) 
library(tidyverse)
library(caret)
library(data.table)
library(stats)
library(ggplot2)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
# download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines("ml-10M100K/ratings.dat")),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines("ml-10M100K/movies.dat"), "\\::", 3)
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

## ALL CODE ABOVE IS PROVIDED BY THE CORSE



head(edx)
dim(edx)
#Here I create a linear model to guess the average rating of a movie based on how many ratings the movie has and the year it was released, to do this
#first get the average rating of each title, and the number of ratings it has in its
# own data set, and the year the movie was created, then run a linear regression seeing how year and number or ratings can predict the average rating

# Manipulate and sort the data and group by movie and add the year and average and number of ratings columns

# RatingSum is the total of all the ratings of the movie
# count is the total number of reviews/ratings for the movie
# avgRating is the average rating of all the ratings for a particular movie, RatingSum/count
# year is the year the movie was released, get this by parsing the title string, we do this by getting the last 5 digits of 
# the title and removing any parethesis i.e: ( )  

# edx_by_title gives us a data frame of the title, average rating, year released, and number of ratings for each movie
# filter by count >= 5 because movies can have skewwed averages because of a low rating count

edx_by_title <- edx %>% group_by(movieId) %>%
  mutate(RatingSum = sum(rating),
  count=n(),
  avgRating=RatingSum/count,
  year= strtoi(str_remove_all(str_sub(title,-5,-1),"[()]"))) %>%
  summarise(title,avgRating,year,count) %>%
  filter(count>=9) %>%
  unique(.) %>%
  arrange(-avgRating) 

# next run a two factor linear regression to see coefficients and P values
lmmov <- lm(avgRating ~ count + year, data=edx_by_title)

#the P values are less the .05 meaning these variables are significant in predicting the average rating
summary(lmmov)

#creating a new data frame with the data we are going to run our prediction on, using the two columns of data (count and year) used in the linear model
newdata <- data.frame(count=edx_by_title$count,year=edx_by_title$year)

#create the vector of predicted ratings using the predict function, linear model from above (lmmov) and the new data frame (newdata)
predicted_Ratings <- predict(lmmov,newdata)

# using the root mean square error function to determine how accurate the predictions were compared to the actual values
# here we meet the goal of getting a RMSE < .8649
rmse(edx_by_title$avgRating,predicted_Ratings)

#to see the relationship between avg rating and the count of rating to see movies with higher ratings tend to have hight rating counts
plot(edx_by_title$count,edx_by_title$avgRating)

# adding prediction column to data set 
edx_by_title$prediction <- predicted_Ratings


#now to do it with just count to see if rmse is improved 

# run a single variable linear regression with just the count variable
lmmov2 <- lm(avgRating ~ count, data=edx_by_title)

# see the p value is still significant 
summary(lmmov2)

#create new data frame with just the count data
newdata2 <- data.frame(count=edx_by_title$count)

#plug that into the new single linear model (lmmov2) with new set of data (newdata2)
predicted_Ratings2 <- predict(lmmov2,newdata2)

#lastly we see how far off the actual values are compared to the new prediction,
# the rmse is still under .8649 however not as low as the RMSE above, meaning adding the year
# variable improves accuracy 
rmse(edx_by_title$avgRating,predicted_Ratings2)






