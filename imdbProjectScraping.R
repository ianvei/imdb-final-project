library(data.table)
library(dplyr)
library(ggplot2)
install.packages('R.utils')
library(rvest)
setwd("/Users/ianveilleux/repos/btma431/Group Assignment") #set your WD to where your IMDB files are

##### Download the 'title.ratings.tsv.gz' and 'title.basics.tsv.gz' from https://www.imdb.com/interfaces/
## note: this will take a while
names<-as.data.frame(fread('title.basics.tsv.gz')) #movie names and info
ratings<-as.data.frame(fread('title.ratings.tsv.gz')) #movie ratings

movies <- names %>%
  filter(grepl('movie', titleType)) #filter to only movies (no tv etc)

#url for imdb site
url <- "https://www.imdb.com/search/title/?title_type=feature&count=250"

sequence <- seq(from =1, to=100000, by=250)

fullDf <- data.frame(matrix(ncol=2, byrow=TRUE),stringsAsFactors=FALSE)
colnames(fullDf) <- c('primaryTitle','tconst')
for(num in sequence){ #iterate over imdb pages to grab 100,000 titles, 250 per page
  #get imdb page starting at num movie
  siteUrl <- paste0(
    "https://www.imdb.com/search/title/?title_type=feature&count=250&start=",num,"&ref_=adv_nxt")
  
  imdbHtml <- read_html(siteUrl) %>% #grab current imdb page
    html_nodes(".lister-item")
  
  tconst <-  imdbHtml %>% #scrape the tconst (primary key) from imdb site
    html_nodes(".lister-top-right > div") %>%
    html_attr("data-tconst")
  
  titles <- imdbHtml %>% #scrape the title name from imdb site
    html_nodes(".lister-item-content") %>%
    html_nodes(".lister-item-header > a") %>%
    html_text
  
  titleTconstDF <- data.frame(matrix(unlist(titles), nrow=250, byrow=TRUE),stringsAsFactors=FALSE)
  titleTconstDF$tconst <- tconst
  colnames(titleTconstDF) <- c('primaryTitle', 'tconst') 
  
  fullDf <- rbind(fullDf, titleTconstDF) #bind this df to master df
}

tConstRatingsMovies <- fullDf %>% 
  inner_join(movies, by="tconst") %>%
  inner_join(ratings, by="tconst") #join movie data and ratings from the CSVs to the scraped data

principals <- as.data.frame(fread('title.principals.tsv.gz')) #principal crew of movies
 
principals <- principals %>% #filter only the #1 most 'principal' actor/actress on each film
  filter(ordering==1)

actorNames <- as.data.frame(fread('name.basics.tsv.gz')) #actor names

principalsTconstRatingsMovies <- tConstRatingsMovies %>% #inner join principals to master DF 
  inner_join(principals, by="tconst")

#inner join names to master DF based on primary key nconst
principalsTconstRatingsMoviesActors <- principalsTconstRatingsMovies %>%
  inner_join(actorNames, by="nconst")

#get only the columns that we need
cleanImdbDf <- principalsTconstRatingsMoviesActors[, 
                                   c("tconst", "originalTitle", "startYear", "runtimeMinutes", 
                                     "genres", "averageRating", "numVotes","nconst",
                                     "category", "primaryName", "primaryProfession",
                                     "birthYear")] #clean dataframe
#cleaning up data - removing NA or \N values - TSV version of NA
cleanImdbDf <- na.omit(cleanImdbDf)
cleanImdbDf <- cleanImdbDf %>%
  filter(startYear != "\\N") %>%
  filter(birthYear != "\\N")

#calculate actor age at time of film based on birth year and year of film release
cleanImdbDf$actorAge <- 
  as.numeric(cleanImdbDf$startYear) - as.numeric(cleanImdbDf$birthYear)

#save as CSV
write.csv(cleanImdbDf, "imdbDF.csv")






