library(dplyr) 
library(lubridate) 
library(caret) 
library(zoo) 
library(plyr) 
library(wordcloud) 
library(httr)
library(jsonlite) 
library(ggmap) 
library(maps)  
library(scales)

#1  
get_summary_stats <- function(df){
  crunchbase$year <- year(crunchbase$announced_on) 
  crunchbase$year <- as.numeric(crunchbase$year)
  
  cb <- filter(crunchbase, raised_amount_usd != "")  
  cb$raised_amount_usd <- gsub(",","", cb$raised_amount_usd)  
  
  
  
  cb$raised_amount_usd <- as.integer(cb$raised_amount_usd) 
  
  cb <- filter(cb, !is.na(raised_amount_usd))
  
  cb <- filter(cb, year >= 1995)
  
  years <- unique(cb$year) %>% sort 
  
  #total, average
  total_mkt_val <- rep(NA, length(years)) 
  avg_mkt_val <- rep(NA, length(years)) 
  
  #volatility of market value
  vol_mkt_val <- rep(NA, length(years))
  
  for (i in 1:length(years)){  
    cy <- years[i]
    cur_df <- filter(cb, year == cy)   
    total <- sum(cur_df$raised_amount_usd)  
    avg <- mean(cur_df$raised_amount_usd) 
    vol <- sd(cur_df$raised_amount_usd)
    total_mkt_val[i] <- total  
    avg_mkt_val[i] <- avg 
    vol_mkt_val[i] <- vol
  } 
  
  summary.stat.df <- data.frame(cbind(years, total_mkt_val, avg_mkt_val,  vol_mkt_val))
  
  return(summary.stat.df) 
} 



#2
get_investor_history <- function(crunchbase, investor_name){  
    crunchbase$year <- year(crunchbase$announced_on)
    crunchbase$year <- as.numeric(crunchbase$year)

    crunchbase$raised_amount_usd <- gsub(",","", crunchbase$raised_amount_usd)
    crunchbase$raised_amount_usd <- as.integer(crunchbase$raised_amount_usd)

    #plot years after 1995 since data before is sparse
    cb <- filter(crunchbase, year >= 1995)
    years <- unique(cb$year) %>% sort
    crunchbase$investor_names <- gsub("Lead - ", "", crunchbase$investor_names)
    plot.df <- data.frame(years=years)

    deals <- rep(NA, length(years))
    for (i in 1:length(years)){
      cy <- years[i]
      investor.annual <- filter(crunchbase, year == cy)
      v.names <- strsplit(investor.annual$investor_names, split = ",")  %>% unlist
      num <- sum(v.names == investor_name)
      deals[i] <- num
    }
    plot.df <- data.frame(cbind(plot.df, deals))



    return(plot.df)
    }


#3 
get_round_counts <- function(crunchbase, round_type){  
  crunchbase$year <- year(crunchbase$announced_on) 
  crunchbase$year <- as.numeric(crunchbase$year) 
  
  #plot years after 1995 since data before is sparse
  cb <- filter(crunchbase, year >= 1995)
  years <- unique(cb$year) %>% sort 
  round.df <- data.frame(years = years) 
  
  deals <- rep(NA, length(years))
  for (i in 1:length(years)){ 
    cy <- years[i] 
    round.yr.df <- filter(crunchbase, year == cy, funding_round_type == round_type)  
    deal.num <- nrow(round.yr.df)  
    deals[i] <- deal.num 
  } 
  round.df <- cbind(round.df,deals)
  
  
  names(round.df) <- c("years", round_type)
  return(round.df)
}


#4 
get_round_mkt_val <- function(crunchbase, round_type){  
  
  crunchbase$year <- year(crunchbase$announced_on) 
  crunchbase$year <- as.numeric(crunchbase$year)  
  crunchbase$raised_amount_usd <- gsub(",","", crunchbase$raised_amount_usd)
  crunchbase$raised_amount_usd <- as.integer(crunchbase$raised_amount_usd)  
  
  cb <- filter(crunchbase, raised_amount_usd != "")  
  cb <- filter(cb, !is.na(cb$raised_amount_usd))
  
  
  #plot years after 1995 since data before is sparse
  cb <- filter(cb, year >= 1995)
  years <- unique(cb$year) %>% sort
  round.df <- data.frame(years = years) 
  
  deal.values <- rep(NA, length(years))
  for (i in 1:length(years)){ 
    cy <- years[i] 
    round.yr.df <- filter(cb, year == cy, funding_round_type == round_type)  
    deal.num <- sum(round.yr.df$raised_amount_usd)  
    deal.values[i] <- deal.num 
  } 
  round.df <- cbind(round.df,deal.values)
  
  
  names(round.df) <- c("years", round_type)
  return(round.df)
}


  
#5 
get_usa_coordinates <- function(yr, api, df){ 
  
  df$year <- year(df$announced_on) 
  df$year <- as.numeric(df$year)
  filt.df <- filter(df, year == yr, country_code == "USA") 
  
  #filter out empty entries
  cities <- filt.df$city  
  cities <- cities[cities != ""]   
  cities <- gsub(" ", "+", cities)
  
  lat <- rep(NA, length(cities)) 
  lng <- rep(NA, length(cities)) 
  
  for (i in 1:length(cities)){
    cc <- cities[i]  
    url <- paste("https://maps.googleapis.com/maps/api/geocode/json?address=", cc, "&key=", api, sep = "") 
    x <- fromJSON(url)  
    latitude <- x$results$geometry$location$lat 
    longitude <- x$results$geometry$location$lng 
    lat[i] <- latitude
    lng[i] <- longitude   
  }
  
  out <- as.data.frame(cbind(lat,lng))
  
  
  
  return(out)
}


#6 
get_kmeans <- function(df, centers, iter){
  kmeans <- kmeans(df, centers = centers, iter.max = iter, algorithm = "Hartigan-Wong") 
  return(kmeans)
}  

#7 
vertical_word_cloud <- function(crunchbase, year){  
  crunchbase$year <- year(crunchbase$announced_on) 
  crunchbase$year <- as.numeric(crunchbase$year)
  cb.year <- filter(crunchbase, year == year)   
  cb.words <- strsplit(cb.year$company_category_list, split = ",") 
  cb.words <- sort(unlist(cb.words))
  cb.words <- gsub("[[:punct:]]", "", cb.words) 
  
  word.table <- as.data.frame(table(cb.words)) 
  total <- sum(word.table[2]) 
  
  word.table$weight <- word.table[2] / total
  
  names(word.table) <- c("words", "count") 
  
  word.cloud <- wordcloud(word.table[[1]], word.table[[2]], max.words = 100)
  return(word.cloud)
}

