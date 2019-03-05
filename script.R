library(httr)
library(xml2)
library(lubridate)
library(dplyr)

get_repo_by_date <- function(date) {
  k <- TRUE
  i = 1 # max row in the request
  while(k) {
    path <- "http://iss.moex.com/iss/history/engines/stock/markets/ccp/trades.xml?date="
    dt <- as.character(date)
    if(i == 1) {
      link <- paste0(path, dt)
    } else {
      link <- paste0(path, dt, "&start=", as.character(i + 1))
    }
  
    xml <- read_xml(GET(link))
    rows <- xml_find_all(xml, "//row")
    
    if(length(rows) == 0) break
    
    if(is.null(dim(df))) {
      df <- data.frame(
        TRADEDATE = xml_attr(rows, "TRADEDATE"),
        TRADETIME = xml_attr(rows, "REPORATE"),
        BOARDID = xml_attr(rows, "BOARDID"),
        SECID = xml_attr(rows, "SECID"),
        REPORATE = xml_attr(rows, "REPORATE")
      )    
    } else {
      df_add <- data.frame(
        TRADEDATE = xml_attr(rows, "TRADEDATE"),
        TRADETIME = xml_attr(rows, "REPORATE"),
        BOARDID = xml_attr(rows, "BOARDID"),
        SECID = xml_attr(rows, "SECID"),
        REPORATE = xml_attr(rows, "REPORATE")
      ) 
      df <- rbind(df, df_add)
      rm(df_add)
    }
    i = i + 5000
  }  
}




