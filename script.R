#################################################################################################################
# Unofficial ISS MOEX API on R
#
# http://iss.moex.com/iss/reference/
#################################################################################################################


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#
# 1. List of securities on MOEX.
#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
get_all_securities <- function() {
  
  require(jsonlite)
  require(dplyr)
  
  i = 1; l  = TRUE; df <- data.frame()
  while(l) {
    jsn <- fromJSON(paste0("https://iss.moex.com/iss/securities.json?start=", i))
    l = (length(jsn$securities$data) != 0) 
    temp <- as.data.frame(jsn$securities$data, stringsAsFactors = FALSE)
    df <- rbind(df, temp)
    i = i + 100
  }
  names(df) <- fromJSON("https://iss.moex.com/iss/securities.json")$securities$columns
  df <- df %>%
    mutate_at(
      vars(is_traded, type:marketprice_boardid),
      funs(as.factor(.))
    )
  return(df)
}

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
#
#
#
#
#
#
#
get_spec_security <- function(secid) {
  require(jsonlite)
  require(dplyr)
  jsn <- fromJSON(paste0("https://iss.moex.com/iss/securities/", secid, ".json"))
  if(length(jsn$description$data) == 0) {
    message(paste0("Security not found: ", secid))
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
  b <- b %>%
    mutate_at(
      vars(board_group_id, market_id, market_id, market, engine_id, engine, is_traded, is_primary, currencyid),
      funs(as.factor)
    ) %>%
    mutate_at(vars(history_from:listed_till), funs(as.Date))
  return(list(params=l, boards = b))
}

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#
# 3. Get global ISS directories
#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

jsn <- fromJSON("https://iss.moex.com/iss/index.json")
engines <- as.data.frame(jsn$engines$data)
names(engines) <- jsn$engines$columns

jsn$markets$columns


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#
# _. Get shares history (by security ID)
#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



get_share_id <- function(secid, dt_begin='', dt_end='') {
  require(jsonlite)
  require(dplyr)
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
    hist <- fromJSON(paste0('http://iss.moex.com/iss/history/engines/stock/markets/shares/securities/',
                            secid,'.json?from=',dt_begin,'&till=',dt_end,'&start=', n))
    if(!is.null(dim(hist$history$data)[1])) {
      temp <- rbind(temp, hist$history$data, stringsAsFactors = FALSE)
      n <- n + 100
    }
    else { k <- FALSE }
  }
  names(temp) <- hist$history$columns
  
  df <- temp %>%
    filter(BOARDID == board) %>%
    select(-WAVAL) %>%
    mutate_all(as.character) %>%
    mutate_at(
      vars(NUMTRADES:ADMITTEDVALUE),
      funs(as.numeric(.))
    ) %>%
    mutate(TRADEDATE = as.Date(TRADEDATE)) 
  
  return(df)
}
