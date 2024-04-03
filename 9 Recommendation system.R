library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

# Compute the number of ratings for each movie and then plot it against the year the movie 
# came out using a boxplot for each year. Use the square root transformation on the y-axis (number of ratings) when creating your plot

movielens %>%
  group_by(movieId) %>%
  summarise(n = n(), year = as.character(first(year))) %>%
  ggplot(aes(x = year, y = sqrt(n))) +
  geom_boxplot() +
  coord_trans(y = "sqrt") +
  theme(axis.text = element_text(angle = 90, hjust = 1)) +
  labs(x = "Year", y = "Square Root of Ratings Count") +
  ggtitle("Boxplot of Ratings Count vs Year")

# calculating mean of 25 movies that came  1993 - 2018 that got high ratings

movielens %>% filter(year >= 1993) %>% group_by(movieId) %>% summarise(n = n(), years = 2018 - first(year),
                                                                       title = title[1],
                                                                       rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25,rate) %>%
  arrange(desc(rate))

# movie genre with low ratings

movielens %>%
  group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>%
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2 * se, ymax = avg + 2 * se)) +
  geom_point() +
  geom_errorbar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Genres", y = "Average Rating", title = "Average Rating by Genres")