# file_tibble <- fx_street_directory()


# data_2018 <- 
#   current_data <- get_corr_data( 
#     symbols =   c(
#       "AUD_USD","EUR_JPY",
#       "GBP_JPY","NZD_USD",
#       "USD_CHF","CAD_JPY","EUR_SEK","GBP_USD",
#       "GBP_CAD","CHF_JPY","GBP_NZD","GBP_AUD",
#       "AUD_NZD","XAG_USD","XCU_USD",
#       "SUGAR_USD","AUD_JPY","GBP_CHF",
#       "USD_DKK","EUR_DKK","USD_SGD","USD_SEK","EUR_CAD",
#       "NZD_CHF","AUD_SGD"
#     ),
#     how_far_back_var = 750 ,
#     time_frame = "D",
#     time = "T15:00:00.000000000Z",
#     date_var_start = NULL,
#     date_var = "2018-01-01",
#     log_or_pip_values = "pip"
#   ) 
# 
# data_2018_dat <- data_2018$data

wrap_clean_fx_street_spread_sheet <- function(path = "data/daily_fx_macro"){
  
  clean_fx_street_spread_sheet_sfly <- 
    safely(.f = clean_fx_street_spread_sheet, otherwise = NULL)
  
  files <- fx_street_directory(path = directory) %>%
    pmap(clean_fx_street_spread_sheet_sfly) %>%
    map_dfr(
      ~ .x$result
    )
  
  returned_data <- files %>%  
    select(time,date_time,date,symbol,event,actual) %>%
    mutate(event = str_remove_all(.data$event, pattern = "Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec"  )) %>%
    mutate(event = str_remove_all(.data$event, pattern = "\\( [0-9]+\\)|\\( [0-9]+ \\)|\\([0-9]+\\)"  )) %>%
    mutate(event = str_remove_all(.data$event, pattern = "\\(\\)"  )) %>%
    mutate(
      event = 
        case_when(
          str_detect(event, "Q1|Q2|Q3|Q4") ~ str_remove(event, "Q1|Q2|Q3|Q4"),
          TRUE ~ event
        )
    ) %>%
    mutate(
      event = str_trim(event)
    )
  
  return(returned_data)
  
}

clean_fx_spread_prelim <- function(path = file_tibble$file_path[27]) {
  
  data <- read_csv(file = file_path, trim_ws = T)
  data <- data[,1:9] 
  
  year <- str_extract(string = file_path,"[0-9]+")
  
  return(data)
}

clean_fx_street_spread_sheet <- function(data,
                                         year_value){
  
  
  
  names(data) <- c("time", "date", "symbol","event", "impact", "actual", "deviation", "consensus", "previous")
  
  data2 <- data %>% 
    filter(!str_detect(string = .data$time, pattern = "Add eventDashboardNotify")) %>%
    mutate(time = ifelse(!str_detect(time, "[a-z]|[A-Z]|[0-9]") , NA, time)) %>% 
    mutate(date = ifelse(!str_detect(date, "[a-z]|[A-Z]|[0-9]"), NA, date)) %>%
    fill(date, .direction = "down") %>%
    select(-impact) %>%
    filter(str_detect(actual, "[0-9]+")) %>%
    mutate(across(.cols = c("actual", "deviation", "consensus", "previous"), 
                  .fns = ~ numerical_transformation_vec(.)
    )) %>%
    filter(str_detect(time,":")) %>%
    mutate(day_value = str_remove_all(string = .data$date, pattern = "[a-z]") %>% 
             str_remove_all(pattern = "[A-Z]") %>% str_remove_all(",") %>% str_trim()) %>% 
    mutate(day_value = ifelse(nchar(.data$day_value) < 2, paste0("0",.data$day_value), .data$day_value)) %>%
    mutate(
      date_time = lubridate::as_datetime(glue::glue("{year_value}-{convert_month(date)}-{day_value} {time}:00 GMT"), tz="GMT")
    ) %>%
    mutate(
      date_time = as_datetime(date_time, tz = "Australia/Canberra") 
    ) %>%
    mutate(
      date = as_date(date_time, tz = "Australia/Canberra")
    )
  
  return(data2)
  
}

fx_street_directory <- function(path = "data/daily_fx_macro"){
  
  list.files(path = path, full.names = T) %>%
    as_tibble() %>%
    filter(str_detect(value, "[0-9]+_daily_fx")) %>%
    mutate(year_value = str_extract(value, "[0-9]+")) %>%
    rename(file_path = value)
  
}

numerical_transformation_vec <- function(.vec){
  
  returned <- 
    case_when(
      str_detect(.vec, pattern = "[0-9]K") &  str_detect(.vec, pattern = "\\.") ~ 
        str_replace(string = .vec, pattern = "\\.", replacement = "") %>%
        str_replace( pattern = "K", replacement = "00"),
      str_detect(.vec, pattern = "[0-9]K") &  !str_detect(.vec, pattern = "\\.") ~ 
        str_replace(string = .vec, pattern = "\\.", replacement = "") %>%
        str_replace( pattern = "K", replacement = "000"),
      TRUE ~ .vec
    )
  
  returned2 <- 
    case_when(
      str_detect(returned, pattern = "[0-9]M") &  str_detect(.vec, pattern = "\\.") ~ 
        str_replace(string = returned, pattern = "\\.", replacement = "") %>%
        str_replace( pattern = "M", replacement = "00000"),
      str_detect(returned, pattern = "[0-9]M") &  !str_detect(.vec, pattern = "\\.") ~ 
        str_replace(string = returned, pattern = "\\.", replacement = "") %>%
        str_replace( pattern = "M", replacement = "000000"),
      TRUE ~ returned
    )
  
  returned3 <- 
    case_when(
      str_detect(returned2, pattern = "[0-9]B") &  str_detect(.vec, pattern = "\\.") ~ 
        str_replace(string = returned2, pattern = "\\.", replacement = "") %>%
        str_replace( pattern = "B", replacement = "00000000"),
      str_detect(returned2, pattern = "[0-9]B") &  !str_detect(.vec, pattern = "\\.") ~ 
        str_replace(string = returned2, pattern = "\\.", replacement = "") %>%
        str_replace( pattern = "B", replacement = "000000000"),
      TRUE ~ returned2
    )
  
  returned4 <- returned3 %>% 
    as_tibble() %>% 
    mutate(value = str_remove(value, "\\$")) %>%
    mutate(percent_or_number = str_detect(value, "%")) %>%
    mutate(
      number = 
        case_when(
          percent_or_number == TRUE ~ 
            str_remove_all(string = value, pattern = "%") %>%
            str_extract(pattern = "([0-9]+\\.[0-9]+)|(\\.[0-9]+)|([0-9]+)") %>%
            as.numeric()/100
          ,
          percent_or_number == FALSE ~ 
            str_remove_all(string = value, pattern = "[a-z]|[A-Z]") %>%
            str_extract(pattern = "([0-9]+\\.[0-9]+)|(\\.[0-9]+)|([0-9]+)") %>%
            as.numeric()
        )
    ) %>%
    pull(number)
  
  return(returned4)
  
}

days_of_week <- function(abbrev = FALSE){
  ifelse(
    abbrev, 
    c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
    c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday") 
  )
}

months_of_year <- function(abbrev = FALSE){
  
  if(abbrev == T){
    return(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  }else{
    return(c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) 
  }
  
  
}

convert_month <- function(.vec){
  
  .vec %>% 
    tolower() %>%
    as_tibble() %>%
    mutate(
      number = 
        case_when(
          str_detect(value, "jan")|str_detect(value, "Jan") ~ "01",
          str_detect(value, "feb")|str_detect(value, "Feb") ~ "02",
          str_detect(value, "mar")|str_detect(value, "Mar") ~ "03",
          str_detect(value, "apr")|str_detect(value, "Apr") ~ "04",
          str_detect(value, "may")|str_detect(value, "May") ~ "05",
          str_detect(value, "jun")|str_detect(value, "Jun") ~ "06",
          str_detect(value, "jul")|str_detect(value, "Jul") ~ "07",
          str_detect(value, "aug")|str_detect(value, "Aug") ~ "08",
          str_detect(value, "sep")|str_detect(value, "Sep") ~ "09",
          str_detect(value, "oct")|str_detect(value, "Oct") ~ "10",
          str_detect(value, "nov")|str_detect(value, "Nov") ~ "11",
          str_detect(value, "dec")|str_detect(value, "Dec") ~ "12"
        )
    ) %>%
    pull(number)
  
}
