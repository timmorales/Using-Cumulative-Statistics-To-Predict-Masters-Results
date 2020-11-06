#PGA SCRAPPPER
library(robotstxt)
library(rvest)
library(stringr)
library(plyr)
library(tidyverse)
###############
###STAT LINKS##
###############


#this is the default base url 
pga<-"https://www.pgatour.com"

#this URL is used to get all the different links to stats 
base_url<- "https://www.pgatour.com/stats/categories.ROTT_INQ.html"

#get STATID for Driving stats 
statID_drive<- read_html("https://www.pgatour.com/stats/categories.ROTT_INQ.html") %>%
  html_nodes(".table-content a") %>%
  html_attr("href")

#approach the green 
statID_approach<- read_html('https://www.pgatour.com/stats/categories.RAPP_INQ.html') %>%
  html_nodes(".table-content a") %>%
  html_attr("href")

#around the green
statID_atg<- read_html('https://www.pgatour.com/stats/categories.RARG_INQ.html') %>%
  html_nodes(".table-content a") %>%
  html_attr("href")

#putting
statID_putt<- read_html('https://www.pgatour.com/stats/categories.RPUT_INQ.html') %>%
  html_nodes(".table-content a") %>%
  html_attr("href")

#scoring 
statID_score<- read_html("https://www.pgatour.com/stats/categories.RSCR_INQ.html") %>%
  html_nodes(".table-content a") %>%
  html_attr("href")

#streaks
statID_streak<- read_html("https://www.pgatour.com/stats/categories.RSTR_INQ.html") %>%
  html_nodes(".table-content a") %>%
  html_attr("href")

#Money and finish 
statID_money<- read_html("https://www.pgatour.com/stats/categories.RMNY_INQ.html") %>%
  html_nodes(".table-content a") %>%
  html_attr("href")

#points and rankings 
statID_rank<- read_html("https://www.pgatour.com/stats/categories.RPTS_INQ.html") %>%
  html_nodes(".table-content a") %>%
  html_attr("href")

#I take all the stats together into one vector to get all the stat IDs
statID_raw<-c(statID_drive,statID_approach,statID_atg,statID_putt,statID_score,statID_streak,statID_money,statID_rank)

#i trim the attributes to get just the number and save as statID
res <- str_match(statID_raw, "/stats/stat.\\s*(.*?)\\.html")
statID<-res[,2]

#some repeats
statID<-unique(statID)


####################
##TOURNAMENT LINKS##
####################
##2020####
###########
#we get Tournament IDs I made the CSV by hand 
tournamentID_raw<-read.csv("/Users/timmorales/Desktop/STAT 6366/Consulting Project/2020 tournament info.csv")
tournamentID_str<-str_match(tournamentID_raw$Link, "eon.\\s*(.*?)\\.html")
tournamentID<-tournamentID_str[,2]

#########
##2019###
#########
tournamentID_2019<-read.csv("/Users/timmorales/Desktop/STAT 6366/Consulting Project/2019 tournament info.csv")
tournamentID_str19<-str_match(tournamentID_2019$Link, "eon.\\s*(.*?)\\.html")
tournamentID19<-tournamentID_str19[,2]




#################
##OVERALL PAGES##
#################
  

#################
### 2020 ####
#############
#now we run two sets of for loops to replace the strings 


#this is listed as the default 
page_default<-rep("https://www.pgatour.com/content/pgatour/stats/stat.02567.y2020.eon.t060.html", length(statID))


pages1<- c()
#we get a default for every statID
for (i in 1:length(statID)){
  pages1[i]<-gsub(pattern = "02567", replacement = statID[i],x=page_default[i])
}

#for every stat ID, get all tournament links 
pages_mat<- matrix(nrow=length(statID),ncol = length(tournamentID))
pages_mat[,1]<-pages1
for (i in 1:length(statID)){
  for (k in 2:length(tournamentID)){
  pages_mat[i,k]<-gsub(pattern = "t060", replacement = tournamentID[k], x=pages_mat[i,1])
}
}

#make the matrix a vector 
#so here we have a big vector of all the pages we need for 2020 that will get us all the data
#its also 1710 long so gonna need to delay the loop through 
pages2020<-as.vector(pages_mat)


#################
### 2019 #### just recycling the same code with few changes 
#############

#this is listed as the default 
page_default<-rep("https://www.pgatour.com/stats/stat.413.y2019.eon.t060.html", length(statID))


pages1<- c()
#we get a default for every statID
for (i in 1:length(statID)){
  pages1[i]<-gsub(pattern = "413", replacement = statID[i],x=page_default[i])
}

#for every stat ID, get all tournament links 
pages_mat<- matrix(nrow=length(statID),ncol = length(tournamentID19))
pages_mat[,1]<-pages1
for (i in 1:length(statID)){
  for (k in 2:length(tournamentID19)){
    pages_mat[i,k]<-gsub(pattern = "t060", replacement = tournamentID19[k], x=pages_mat[i,1])
  }
}

#make the matrix a vector 
#so here we have a big vector of all the pages we need for 2020 that will get us all the data
#its also 1710 long so gonna need to delay the loop through 
pages2019<-as.vector(pages_mat)


######################
# BE AWARE THERE ####
#ARE OVER 20000 pages to scrape for 2019#
##################


#code to scrape all the pages just needs a loop 

#taking from matrix and just doing 1 tourney at time 
tourney1<-pages_mat[,1]
df<-list()
for (i in 1:length(tourney1)){
temp <- read_html(tourney1[i]) %>%
  html_nodes(xpath = '//*[@id="statsTable"]') %>%
  html_table()
df[[i]]<-data.frame(temp)

#getting topic of page 
topic <- read_html(tourney1[i]) %>%
  html_nodes('h1') %>%
  html_text()

#need to add pages description to each column name 
#this tells us the stat that the table refers too 
#will need to modify each then add to column names 
colnames(df[[i]])[colnames(df[[i]])!="PLAYER.NAME"]<-paste(topic,colnames(df[[i]][colnames(df[[i]])!="PLAYER.NAME"]),sep= ".")
}

#now we need to find a way to merge the files 
#if there were zero rows, i denote as null
for (i in 1:length(df)){
  if (nrow(df[[i]])!=0){
    q[i]<-df[i]
  }
}
#this removes items denoted as NULL in list 
p<-q %>% discard(is.null)

#we're gonna need to find a way to remove items that do not have column name 
#"Player Name" in this case it was some FEDEX index 366
TourChampionshipStats<-p[-366] %>% reduce(left_join)

#write_csv(TourChampionshipStats, "/Users/timmorales/Desktop/STAT 6366/Consulting Project/TourChampStats.csv")



#code to clean column names
## clean column 
####janitor::clean_names()





###################
#test if urls work#
##################
url_exists <- function(x, non_2xx_return_value = FALSE, quiet = FALSE,...) {
  
  suppressPackageStartupMessages({
    require("httr", quietly = FALSE, warn.conflicts = FALSE)
  })
  
  # you don't need thse two functions if you're alread using `purrr`
  # but `purrr` is a heavyweight compiled pacakge that introduces
  # many other "tidyverse" dependencies and this doesnt.
  
  capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
    tryCatch(
      list(result = code, error = NULL),
      error = function(e) {
        if (!quiet)
          message("Error: ", e$message)
        
        list(result = otherwise, error = e)
      },
      interrupt = function(e) {
        stop("Terminated by user", call. = FALSE)
      }
    )
  }
  
  safely <- function(.f, otherwise = NULL, quiet = TRUE) {
    function(...) capture_error(.f(...), otherwise, quiet)
  }
  
  sHEAD <- safely(httr::HEAD)
  sGET <- safely(httr::GET)
  
  # Try HEAD first since it's lightweight
  res <- sHEAD(x, ...)
  
  if (is.null(res$result) || 
      ((httr::status_code(res$result) %/% 200) != 1)) {
    
    res <- sGET(x, ...)
    
    if (is.null(res$result)) return(NA) # or whatever you want to return on "hard" errors
    
    if (((httr::status_code(res$result) %/% 200) != 1)) {
      if (!quiet) warning(sprintf("Requests for [%s] responded but without an HTTP status code in the 200-299 range", x))
      return(non_2xx_return_value)
    }
    
    return(TRUE)
    
  } else {
    return(TRUE)
  }
  
}

played_college <- NA
for (i in 1:100){
  played_college[i] <- url_exists(pages2019[i])
}

urls2 <- pages2019[which(played_college == TRUE)]
player_list2 <- player_list[which(played_college == TRUE)]


###################
