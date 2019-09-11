#################################################################################################################
# Unofficial ISS MOEX API on R
#
# http://iss.moex.com/iss/reference/
#################################################################################################################



##################################################
#
# 1. List of securities on MOEX.
#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
mx_get_all_sec <- function() {
  
  require(jsonlite)
  require(dplyr)
  
  i = 1; l  = TRUE; df <- data.frame()
  while(l) {
    jsn <- fromJSON(paste0("http://iss.moex.com/iss/securities.json?start=", i))
    l = (length(jsn$securities$data) != 0)  
    temp <- as.data.frame(jsn$securities$data, stringsAsFactors = FALSE)
    df <- rbind(df, temp)
    i = i + 100
  }
  names(df) <- fromJSON("http://iss.moex.com/iss/securities.json")$securities$columns
  df <- df %>% 
    mutate_at(
      vars(is_traded, type:marketprice_boardid),
      funs(as.factor(.))
    )
  return(df)
}

sec <-mx_get_all_sec()

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#
# 2. Get a specification of security
#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Fields of output list:
# ----------------------------------
# SECID - Код ценной бумаги
# NAME - Полное наименование
# SHORTNAME - Краткое наименование
# ISIN - ISIN код
# REGNUMBER - Номер государственной регистрации
# ISSUESIZE - Объем выпуска
# FACEVALUE - Номинальная стоимость
# FACEUNIT - Валюта номинала
# ISSUEDATE - Дата начала торгов
# LATNAME - Английское наименование
# LISTLEVEL - Уровень листинга
# ISQUALIFIEDINVESTORS - Бумаги для квалифицированных инвесторов
# TYPENAME - Вид/категория ценной бумаги
# GROUP - Код типа инструмента
# TYPE - Тип бумаги
# GROUPNAME - Типа инструмента
# EMITTER_ID - Код эмитента
# MATDATE - Дата погашения
# INITIALFACEVALUE - Первоначальная номинальная стоимость
#

mx_get_spec_sec <- function(secid) {
  jsn <- fromJSON(paste0("https://iss.moex.com/iss/securities/", secid, ".json"))
  if(length(jsn$description$data) == 0) {
    message(paste0("Не найден финансовый инструмент: ", secid))
    stop()
  }
  df <- as.data.frame(jsn$description$data, stringsAsFactors = FALSE)
  
  l <- as.list(setNames(df$V3, df$V1))
  
  number <- df %>% filter(V4 == 'number') 
  l[number$V1] <- lapply(l[number$V1], as.numeric)
  date <- df %>% filter(V4 == 'date') 
  l[date$V1] <- lapply(l[date$V1], as.Date)
  
  b <- as.data.frame(jsn$boards$data, stringsAsFactors = FALSE)
  names(b) <- jsn$boards$columns
  summary(b)
  b <- boards %>% 
    mutate_at(
      vars(board_group_id, market_id, market_id, market, engine_id, engine, is_traded, is_primary, currencyid), 
      funs(as.factor)
    ) %>% 
    mutate_at(vars(history_from:listed_till), funs(as.Date))
  
  return(list(params=l, boards = b)) 
}


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#
# 3. Get global directories
#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

mx_get_directories<- function() {
  require(jsonlite)
  require(dplyr)
  jsn <- fromJSON("https://iss.moex.com/iss/index.json")
  nm <- names(jsn)
  return(
    sapply(jsn, function(x){
      k <- as.data.frame(x$data, stringsAsFactors = FALSE)
      names(k) <- x$columns
      l <- list(k)
    })
  )
}

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#
# 5. Get a list of engines
#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

mx_get_list_engines <- function() {
  require(jsonlite)
  require(dplyr)
  jsn <- fromJSON("https://iss.moex.com/iss/engines.json")
  df <- as.data.frame(jsn$engines$data, stringsAsFactors = FALSE)
  names(df) <- jsn$engines$columns
  return(df)
}

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#
# 6. Get a list of markets
#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

mx_get_list_markets <- function(engine="stock") {
  require(jsonlite)
  require(dplyr)
  jsn <- fromJSON(paste0("https://iss.moex.com/iss/engines/",engine,"/markets.json"))
  df <- as.data.frame(jsn$markets$data, stringsAsFactors = FALSE)
  names(df) <- jsn$markets$columns
  return(df)
}

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#
# 7. Get a list of boards
#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

mx_get_list_boards <- function(engine="stock", market="shares") {
  require(jsonlite)
  require(dplyr)
  jsn <- fromJSON(paste0("https://iss.moex.com/iss/engines/",engine,"/markets/",market,"/boards.json"))
  df <- as.data.frame(jsn$boards$data, stringsAsFactors = FALSE)
  names(df) <- jsn$boards$columns
  return(df)
}

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#
# 8. Get a list of securities by mode
#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

mx_get_list_sec <- function(date, engine="stock", market="bonds", board="EQOB") {
  require(jsonlite)
  require(dplyr)
  
  link <- paste0("https://iss.moex.com/iss/engines/stock/markets/bonds/boards/EQOB/securities.json?date=2019-08-26&start=1")
  jsn <- fromJSON(link)
  jsn$securities
  
  link <- paste0("https://iss.moex.com/iss/engines/",engine,"/markets/",market,"/boards/",board,"/securities.json?date=",date,"&start=")
  
  k <- TRUE; i <- 1; df <- data.frame()
  while(k) {
    jsn <- fromJSON(paste0("https://iss.moex.com/iss/engines/stock/markets/bonds/boards/EQOB/securities.json?date=2019-08-26&start=", i))
    if(length(jsn$history$data) == 0) {
      k <- FALSE;  
      names(df) <- jsn$history$columns
    }
    else {
      i <- i + 100 
      temp <- as.data.frame(jsn$history$data, stringsAsFactors = FALSE)
      df <- rbind(df, temp)
    }
  }
  
  v <- sapply(jsn$history$columns, function(x){jsn$history$metadata[[x]]$type}) # coloumn type
  df <- df %>% 
    mutate_at(vars(names(v[v == 'double'])), funs(as.numeric)) %>% 
    mutate_at(vars(names(v[v == 'date'])), funs(as.Date)) %>% 
    mutate_at(vars(BOARDID, CURRENCYID, FACEUNIT), funs(as.factor))
  
  return(df)
}

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#
# _. Get shares history (by security ISIN)
#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

mx_sec_hist <- function(isin, dt_begin='', dt_end='') {
  require(jsonlite)
  require(dplyr)
  
  sec <- fromJSON('http://iss.moex.com/iss/engines/stock/markets/shares/securities.json')
  if(isin %in% sec$securities$data[,20]) {
    sec_type = 'shares'
    secid <- sec$securities$data[which(sec$securities$data[,20] == isin),][1]
  } else if(isin %in% fromJSON('http://iss.moex.com/iss/engines/stock/markets/bonds/securities.json')$securities$data[,30]) {
    sec_type = 'bonds'
    secid <- isin
  } else {
    return(message('Security not found'))
  }
  
  sec_param <- fromJSON(paste0('https://iss.moex.com/iss/securities/', secid ,'.json'))
  board <- sec_param$boards$data[sec_param$boards$data[,15] == 1][2] 
  
  if(dt_begin == '' | dt_end == '') {
    dt_begin <- as.character(Sys.Date() - 31) 
    dt_end <- as.character(Sys.Date() - 1) 
  }
  n <- 1
  k <- TRUE    
  temp <- data.frame(stringsAsFactors = FALSE)
  while(k) {
    hist <- fromJSON(paste0('http://iss.moex.com/iss/history/engines/stock/markets/',sec_type,'/securities/',
                            secid,'.json?from=',dt_begin,'&till=',dt_end,'&start=', n))
    if(!is.null(dim(hist$history$data)[1])) {
      temp <- rbind(temp, hist$history$data, stringsAsFactors = FALSE)
      n <- n + 100
    } else { k <- FALSE }
  }
  
  temp <- temp[temp$V1 == board,]
  names(temp) <- hist$history$columns
  types <- sapply(hist$history$columns,
                  function(x) { hist$history$metadata[[x]]$type })
  
  df <- temp %>% 
    mutate_at(vars(names(types[types == 'double'])), funs(as.numeric)) %>% 
    mutate_at(vars(names(types[types == 'date'])), funs(as.Date)) %>% 
    mutate_at(vars(BOARDID, CURRENCYID, FACEUNIT), funs(as.factor))
  
  return(df)
}