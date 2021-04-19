#Movie Rating Analysis: Has Fandango Changed Their Rating System After Hickey's Analysis?

library(readr)
library(tidyr)
library(tidyverse)
library(dbplyr)
library(dplyr)
library(stringr)
library(ggplot2)

fandango <- read.csv("~/Google Drive/Work/R programming/Guided Projects/Fandango Reanalysis/Movie_ratings_2016_17-master/fandango_score_comparison.csv")
movie_ratings <- read.csv("~/Google Drive/Work/R programming/Guided Projects/Fandango Reanalysis/Movie_ratings_2016_17-master/movie_ratings_16_17.csv")

#Selecting columns
fandango <- fandango %>%
  select('FILM', 'Fandango_Stars', 'Fandango_Ratingvalue', 'Fandango_votes', 'Fandango_Difference')

movie_ratings <- movie_ratings %>%
    select('movie', 'year', 'fandango')


set.seed(1)
sample <- sample_n(movie_ratings, size = 10)

ratings <- tibble(reviews = c(5260, 7214, 7247, 12041, 30210, 276, 13654, 1202, 56746, 9167))
bind_cols(sample, ratings)

```{r}
We noticed that the fandango and movie_ratings dbs have multiple years, so we filter only 2015 and 2016 respectively.
```{r message=FALSE}

fandango_2015 <- fandango %>%
  filter(str_detect(FILM, '2015'))

movies_2016 <- movie_ratings %>%
  filter(str_detect(year, '2016'))

#Comparing Distribution Shapes for 2015 and 2016

fandango_2015_density <- fandango_2015 %>%
  group_by(Fandango_Stars) %>%
  summarize(Freq = n())

movies_2016_density <- movie_ratings %>%
  group_by(fandango) %>%
  summarize(Freq = n())

ggplot(data = fandango_2015_density,
       aes(x = Fandango_Stars)) +
  geom_density() +
  geom_density(data = movies_2016_density,
               aes(x = fandango), color = 'blue') +
  labs(title = 'Fandango Star Rating Distribution 2015 vs. 2016',
       x = 'Fandango Star Rating', 
       y = 'Density') +
  scale_x_continuous(breaks=seq(0, 5, by = .5), limits = c(0,5))

```{r}
We see that both distributions are left-skewed but are Gaussian in shape. Both shapes are different in that the center of the 2016 distribution is shifted left versus the 2015 distribution, constituing a change in Fandangos rating system.
```{r message=FALSE}

#Comparing Relative Frequencies

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

fandango_2015_stats <- fandango_2015 %>%
  summarize(year = 2015,
            mean = mean(Fandango_Stars),
            median = median(Fandango_Stars),
            mode = mode(Fandango_Stars))

movie_ratings_stats <- movie_ratings %>%
  summarize(year = 2016,
            mean = mean(fandango),
            median = median(fandango),
            mode = mode(fandango))

stats <- bind_rows(fandango_2015_stats, movie_ratings_stats)

stats <- 
  gather(stats, key = "statistic", value = "value", mean:mode)

stats$year <- as.integer(stats$year)
typeof(stats$year[1])
stats

ggplot(data = stats,
       aes(x = statistic, y = value, fill = year)) +
  geom_bar(position = "dodge2", stat = "identity") +
  labs(title = "Comparing Summary Statistics: 2015 vs. 2016",
       x = '',
       y = 'Stars')



