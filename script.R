library(httr)
library(xml2)
library(lubridate)
library(dplyr)



k <- TRUE
i = 5000 # max row in the request
while(k) {
  path <- "http://iss.moex.com/iss/history/engines/stock/markets/ccp/trades.xml?date="
  date <- "2019-03-04"
  
  r <- GET(paste0(path, date))
  xml <- read_xml(r)
  
  rows <- xml_find_all(xml, "//row")
  df <- data.frame(
    TRADEDATE = xml_attr(rows, "TRADEDATE"),
    BOARDID = xml_attr(rows, "BOARDID"),
    SECID = xml_attr(rows, "SECID"),
    REPORATE = xml_attr(rows, "REPORATE")
  )
}


xml_attr(rows, "TRADEDATE")


for(i in 1:30) {
  
  
  read_xml(paste0())
}
