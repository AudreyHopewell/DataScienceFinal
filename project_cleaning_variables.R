library(readr)
library(tidyverse)
library(dplyr)
library(lubridate)
movies <- read.csv("movies_metadata.csv")

# excluding unnecessary columns from the data
drop <- c("homepage", "original title", "popularity", "poster_path")
movies <- movies[!names(movies) %in% drop]

# cleaning the data
movies1 <- movies[movies$adult=="False",]
movies2 <- movies1[movies1$original_language=="en",]
movies3 <- filter(movies2, grepl("United States of America", production_countries))
movies4 <- movies3[movies3$video=="False",]
movies5 <- movies4[movies4$status=="Released",]
movies6 <- filter(movies5, !grepl("Documentary", genres))
movies6$release_date <- ymd(movies6$release_date)
movies7 <- movies6[movies6$release_date>"2000-01-01",]
movies7$genres <- gsub('[[:punct:] ]+', ' ', movies7$genres)
movies7$genres <- gsub('id', '', movies7$genres)
movies7$genres <- gsub('name', '', movies7$genres)
movies7$genres <- gsub('[0-9]+', '', movies7$genres)
movies7$revenue <- as.numeric(movies7$revenue)
movies8 <- movies7[movies7$revenue>0,]
movies8$budget <- as.numeric(movies8$budget)
movies9 <- movies8[movies8$budget>0,]
movies9$production_companies <- gsub('[[:punct:] ]+', ' ', movies9$production_companies)
movies9$production_companies <- gsub('id', '', movies9$production_companies)
movies9$production_companies <- gsub('name', '', movies9$production_companies)
movies9$production_companies <- gsub('[0-9]+', '', movies9$production_companies)
movies9$production_countries <- gsub('[[:punct:] ]+', ' ', movies9$production_countries)
movies9$production_countries <- gsub('id', '', movies9$production_countries)
movies9$production_countries <- gsub('name', '', movies9$production_countries)
movies9$production_countries <- gsub('[0-9]+', '', movies9$production_countries)
clean_movies <- movies9

# removing movies with NA values for revenue or rating
clean_movies <- drop_na(clean_movies, c(revenue, vote_average))


# creating movie "categories"
mean_rev <- mean(clean_movies$revenue)
clean_movies <- mutate(clean_movies,
                       revenue_category = case_when(
                         revenue <= mean_rev ~ "low",
                         revenue > mean_rev ~ "high"
                       )
                       )
mean_rating <- mean(clean_movies$vote_average)
clean_movies <- mutate(clean_movies,
                       rating_category = case_when(
                         vote_average <= mean_rating ~ "low",
                         vote_average > mean_rating ~ "high"
                       )
                       )
clean_movies <- mutate(clean_movies,
                       category = case_when(
                         revenue_category=="low" & rating_category=="low" ~ "flop",
                         revenue_category=="low" & rating_category=="high" ~ "cult classic",
                         revenue_category=="high" & rating_category=="low" ~ "bad blockbuster",
                         revenue_category=="high" & rating_category=="high" ~ "smash hit"
                       ))



clean_movies$revenue_category <- factor(clean_movies$revenue_category, 
                                       levels = c("low", "high"), ordered = FALSE)
clean_movies$rating_category <- factor(clean_movies$rating_category, 
                                       levels = c("low", "high"), ordered = FALSE)


# creating k-means clustering classes
clusterdata <- as.data.frame(cbind(clean_movies$revenue, clean_movies$vote_average))

clusters <- kmeans(clusterdata, 4)
clean_movies <- as.data.frame(cbind(clean_movies, clusters$cluster))


# creating independent variables

# does the movie belong to a collection/series?
clean_movies <- clean_movies %>% 
  mutate(collection = if_else(
    grepl("id", belongs_to_collection), 1, 0
  ))


# identifying the unique genres: 
genre.df <- separate(clean_movies, genres, into = c("a", "b", "c", "d", "e", 
                                                    "f", "g", "h", "i", "j", 
                                                    "k", "l", "m"))
genre.df <- genre.df[,4:16]
genre.df2 <- as.vector(as.matrix(genre.df))
genres.unique <- unique(genre.df2)

# creating genre dummy variables
clean_movies <- clean_movies %>% 
  mutate(animation=if_else(
    grepl("Animation", genres), 1, 0
  )
  )

clean_movies <- clean_movies %>% 
  mutate(adventure=if_else(
    grepl("Adventure", genres), 1, 0
  )
  )

clean_movies <- clean_movies %>% 
  mutate(comedy=if_else(
    grepl("Comedy", genres), 1, 0
  )
  )

clean_movies <- clean_movies %>%
  mutate(action=if_else(
    grepl("Action", genres), 1, 0
  ))

clean_movies <- clean_movies %>% 
  mutate(family=if_else(
    grepl("Family", genres), 1, 0
  )
  )

clean_movies <- clean_movies %>% 
  mutate(history=if_else(
    grepl("History", genres), 1, 0
  )
  )

clean_movies <- clean_movies %>% 
  mutate(drama=if_else(
    grepl("Drama", genres), 1, 0
  )
  )

clean_movies <- clean_movies %>% 
  mutate(crime=if_else(
    grepl("Crime", genres), 1, 0
  )
  )

clean_movies <- clean_movies %>% 
  mutate(science=if_else(
    grepl("Science", genres), 1, 0
  )
  )

clean_movies <- clean_movies %>% 
  mutate(fantasy=if_else(
    grepl("Fantasy", genres), 1, 0
  )
  )

clean_movies <- clean_movies %>% 
  mutate(music=if_else(
    grepl("Music", genres), 1, 0
  )
  )

clean_movies <- clean_movies %>% 
  mutate(horror=if_else(
    grepl("Horror", genres), 1, 0
  )
  )


clean_movies <- clean_movies %>% 
  mutate(mystery=if_else(
    grepl("Mystery", genres), 1, 0
  )
  )

clean_movies <- clean_movies %>% 
  mutate(thriller=if_else(
    grepl("Thriller", genres), 1, 0
  )
  )

clean_movies <- clean_movies %>% 
  mutate(romance=if_else(
    grepl("Romance", genres), 1, 0
  )
  )

clean_movies <- clean_movies %>% 
  mutate(western=if_else(
    grepl("Western", genres), 1, 0
  )
  )

clean_movies <- clean_movies %>% 
  mutate(war=if_else(
    grepl("War", genres), 1, 0
  )
  )

clean_movies <- clean_movies %>% 
  mutate(fiction=if_else(
    grepl("Fiction", genres), 1, 0
  )
  )

clean_movies <- clean_movies %>% 
  mutate(foreign=if_else(
    grepl("Foreign", genres), 1, 0
  )
  )

# creating dummy variables for each of the major production companies
clean_movies <- clean_movies %>%
  mutate(Universal=ifelse(
    grepl("Universal Pictures", production_companies), 1, 0
  ))

clean_movies <- clean_movies %>%
  mutate(Paramount=ifelse(
    grepl("Paramount Pictures", production_companies), 1, 0
  ))

clean_movies <- clean_movies %>%
  mutate(Warner=ifelse(
    grepl("Warner Bros", production_companies), 1, 0
  ))

clean_movies <- clean_movies %>%
  mutate(Columbia=ifelse(
    grepl("Columbia Pictures", production_companies), 1, 0
  ))

clean_movies <- clean_movies %>%
  mutate(Disney=ifelse(
    grepl("Walt Disney", production_companies), 1, 0
  ))

clean_movies <- clean_movies %>%
  mutate(Fox=ifelse(
    grepl("Twentieth Century Fox", production_companies), 1, 0
  ))

clean_movies <- clean_movies %>%
  mutate(MGM=ifelse(
    grepl("MGM", production_companies), 1, 0
  ))

clean_movies <- clean_movies %>%
  mutate(Dreamworks=ifelse(
    grepl("Dreamworks", production_companies), 1, 0
  ))

# identifying all unique (non-US) production countries and sorting by frequency
country.df <- separate(clean_movies, production_countries, into = 
                         c("a", "b", "c", "d", "e", "f", "g", "h",
                           "i", "j", "k", "l", "m"), sep = " iso   ")
country.df <- country.df[,11:22]
country.df <- as.vector(as.matrix(country.df))
common.countries <- as.data.frame(prop.table(table(country.df))) %>% arrange(desc(Freq))

# finding the top 10 most common (besides U.S.)
head(common.countries, 16)

# creating dummy variables for each of those countries
clean_movies <- clean_movies %>%
  mutate(UK=ifelse(
    grepl("GB", production_countries), 1, 0
  ))

clean_movies <- clean_movies %>%
  mutate(Germany=ifelse(
    grepl("DE", production_countries), 1, 0
  ))

clean_movies <- clean_movies %>%
  mutate(Canada=ifelse(
    grepl("CA", production_countries), 1, 0
  ))

clean_movies <- clean_movies %>%
  mutate(France=ifelse(
    grepl("FR", production_countries), 1, 0
  ))

clean_movies <- clean_movies %>%
  mutate(Australia=ifelse(
    grepl("AU", production_countries), 1, 0
  ))

clean_movies <- clean_movies %>%
  mutate(Japan=ifelse(
    grepl("JP", production_countries), 1, 0
  ))

clean_movies <- clean_movies %>%
  mutate(China=ifelse(
    grepl("CN", production_countries), 1, 0
  ))

clean_movies <- clean_movies %>%
  mutate(Italy=ifelse(
    grepl("IT", production_countries), 1, 0
  ))

clean_movies <- clean_movies %>%
  mutate(Ireland=ifelse(
    grepl("IE", production_countries), 1, 0
  ))

clean_movies <- clean_movies %>%
  mutate(Spain=ifelse(
    grepl("ES", production_countries), 1, 0
  ))

# getting release season
release_month <- month(clean_movies$release_date)

clean_movies <- clean_movies %>%
  mutate(winter=case_when(
   release_month<=2 ~ 1,
   release_month==12 ~ 1,
   release_month>2 & release_month<12 ~ 0
  )
  )

clean_movies <- clean_movies %>%
  mutate(spring=ifelse(
    release_month>=3 & release_month<=5, 1, 0
  ))

clean_movies <- clean_movies %>%
  mutate(summer=ifelse(
    release_month>=6 & release_month<=8, 1, 0
  ))

clean_movies<- clean_movies %>%
  mutate(fall=ifelse(
    release_month>=9 & release_month<=11, 1, 0
  ))



# dropping all the columns we don't care about for prediction

# creating a dataset with target variable 1: the intuitive categories
clusterdrop <- c("adult", "belongs_to_collection", "genres", "imdb_id", 
               "original_language", "original_title", "overview", "production_companies", 
               "production_countries", "release_date", "spoken_languages", "status", 
               "tagline", "title", "video", "vote_average", "release_month", 
               "revenue_category", "rating_category", "runtime", "revenue", "vote_count", 
               "id", "clusters$cluster")
movies.cat <- clean_movies[!names(clean_movies) %in% clusterdrop]


# creating a dataset with target variable 2: the k-means clustering classes
categorydrop <- c("adult", "belongs_to_collection", "genres", "imdb_id", 
                  "original_language", "original_title", "overview", "production_companies", 
                  "production_countries", "release_date", "spoken_languages", "status", 
                  "tagline", "title", "video", "vote_average", "release_month", 
                  "revenue_category", "rating_category", "runtime", "revenue", "vote_count", 
                  "id", "category")
movies.cluster <- clean_movies[!names(clean_movies) %in% categorydrop]
colnames(movies.cluster)[2] <- "cluster"


