library(data.table)
library(dplyr)
library(ggplot2)
install.packages('R.utils')
setwd("/Users/ianveilleux/repos/btma431/Group Assignment") #set your WD to where your IMDB files are

##### Download the 'title.ratings.tsv.gz' and 'title.basics.tsv.gz' from https://www.imdb.com/interfaces/
## note: this will take a while
names<-as.data.frame(fread('title.basics.tsv.gz')) #movie names and info
ratings<-as.data.frame(fread('title.ratings.tsv.gz')) #movie ratings

movies <- names %>%
  filter(grepl('movie', titleType)) #filter to only movies (no tv etc)

url <- "https://www.imdb.com/search/title/?title_type=feature&count=25" #url for imdb site

tconst <- read_html(url) %>% #scrape the tconst (primary key) from imdb site
  html_nodes(".lister-item") %>%
  html_nodes(".lister-top-right > div") %>%
  html_attr("data-tconst")

test <- read_html(url) %>% #scrape the movie title from imdb site
  html_nodes(".lister-item") %>%
  html_nodes(".lister-item-content") %>%
  html_nodes(".lister-item-header > a") %>%
  html_text

df <- data.frame(matrix(unlist(test), nrow=25, byrow=TRUE),stringsAsFactors=FALSE)
df$tconst <- tconst
colnames(df) <- 'primaryTitle' #create df of titles and tconst scraped from imdb

testTconstRatingsMovies <- df %>% 
  inner_join(movies, by="tconst") %>%
  inner_join(ratings, by="tconst") #join movie data and ratings from the CSVs to the scraped data

cleanDf <- testTconstRatingsMovies[, 
                                   c("tconst", "originalTitle", "startYear", "runtimeMinutes", 
                                     "genres", "averageRating", "numVotes")] #clean dataframe





