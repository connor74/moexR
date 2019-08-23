
get_params_isin <- function(isin = "") {
  ##############################################
  # 
  # ФУНКЦИЯ: Получение параметров бумаги по ISIN
  # @ isin - isin код искомой бумаги
  #
  #############################################  
  
  require(xml2)
  
  require(httr)
  path <- "https://iss.moex.com/iss/securities/"
  k <- read_xml(GET(paste0(path, isin, ".xml"))) 
  
  iss <- read_xml("RU000A0JXUC1.xml")
  param_list <- xml_find_all(iss, ".//row")
  
  l <- list(
    'name' = xml_attr(param_list, "name")[1:27],
    'title' = xml_attr(param_list, "title")[1:27],
    'value' = xml_attr(param_list, "value")[1:27]
  )
  
  df <- as.data.frame(t(l$value))
  names(df) <- l$name 
  
  return(df)
}

read_by_date <- function(date, boards="EQOB") {
  
  #############################################
  # 
  # ФУНКЦИЯ: Получение итогов торгов за дату
  # date - дата торгов
  # boards - площадки
  #
  #############################################  
  require(dplyr)
  require(jsonlite)
  i = 0
  l = list()
  while(TRUE) {
    path <- paste0("http://iss.moex.com/iss/history/engines/stock/markets/bonds/boards/",boards,"/securities.json?date=",date)
    if(i != 0) {
      path <- paste0(path, "&start=", as.character(i + 1))
    }
    jdata <- fromJSON(path, simplifyDataFrame = T)$history[2:3]
    
    if(length(jdata$data) == 0) break
    
    df_ <- as.data.frame(jdata$data)
    names(df_) <- jdata$columns
    l[[i+1]] <- df_
    
    i = i + 100
  }
  
  df <- bind_rows(l)
  
  if(any(dim(df) == 0)) return(NULL)
  df <- df %>% 
    mutate_at(
      vars(TRADEDATE, MATDATE, OFFERDATE, BUYBACKDATE, LASTTRADEDATE),
      funs(as.Date(.))
    )  %>%
    mutate_at(
      vars(NUMTRADES, VALUE, LOW, HIGH, CLOSE, LEGALCLOSEPRICE, ACCINT, WAPRICE,
           YIELDCLOSE, OPEN, VOLUME, MARKETPRICE2, MARKETPRICE3, ADMITTEDQUOTE, 
           MP2VALTRD, MARKETPRICE3TRADESVALUE, ADMITTEDVALUE, DURATION, YIELDATWAP,
           IRICPICLOSE, BEICLOSE, COUPONPERCENT, COUPONVALUE, FACEVALUE, CBRCLOSE,
           YIELDTOOFFER, YIELDLASTCOUPON),
      funs(as.numeric(as.character(.)))
    ) %>% 
    mutate_at(
      vars(SHORTNAME, SECID),
      funs(as.character(.))
    ) 
  df <- df %>%
    mutate(
      ISDEALS = if_else(NUMTRADES > 0, 1, 0)
    )
  return(df)
}



read_days <- function(date, boards="EQOB", tdays=30) {
  ###############################################################
  # 
  # Получение итогов торгов за несколько торговых дней
  # boards - площадка
  # date - дата последнего торгового дня для обратного отсчета
  # t_days - количество торговых дней
  ###############################################################
  require(dplyr)
  require(jsonlite)
  l <- list()
  date <- as.Date(date)
  while(tdays != 0) {
    df1 <- read_by_date(as.character(date), "EQOB")
    if(!is.null(df1)) {
      l[[as.character(date)]] <- df1
      date = date - 1
      tdays = tdays - 1
    } else {
      date = date - 1
    }
  }
  return(bind_rows(l))
}

get_sec_params <- function() {
  ############################################
  # Получить статичные параметры по всем облигациям
  #
  #
  ##############################################
  require(jsonlite)
  require(dplyr)
  jdata <- fromJSON("http://iss.moex.com/iss/engines/stock/markets/bonds/securities.json")
  
  df <- data.frame(jdata$securities$data)
  names(df) <- jdata$securities$columns
  
  df$BUYBACKDATE<- as.character(df$BUYBACKDATE)
  df$BUYBACKDATE <- ifelse(df$BUYBACKDATE == "0000-00-00", NA, df$BUYBACKDATE) 
  
  df <- df %>% 
    mutate_at(
      vars(NEXTCOUPON, MATDATE, PREVDATE, BUYBACKDATE, OFFERDATE, SETTLEDATE),
      funs(as.Date(.))
    ) 
  
  df <- df%>%
    mutate_at(
      vars(PREVWAPRICE:COUPONVALUE, ACCRUEDINT:FACEVALUE, COUPONPERIOD:PREVADMITTEDQUOTE, MINSTEP, ISSUESIZEPLACED, COUPONPERCENT, LOTVALUE),
      funs(as.numeric(as.character(.)))
    )
  
  df <- df%>%
    mutate_at(
      vars(SHORTNAME, SECNAME, LATNAME, REGNUMBER, SECID),
      funs(as.character(.))
    )
  return(df)
}


library(jsonlite)
require(dplyr)
from <- "2019-08-19"
till <- "2019-08-22"
name <- "AFLT"
board <- "TQBR"
js <- fromJSON(paste0("http://iss.moex.com/iss/history/engines/stock/markets/shares/boards/",board,"/securities/",name,".json?from=",from,"&till=",till))
df <- as.data.frame(js$history$data)
names(df) <- js$history$columns
options(digits = 12)
df <- df %>% 
  mutate_at(
    vars(NUMTRADES:WAVAL),
    funs(as.numeric(as.character(.)))
  )

str(df)
