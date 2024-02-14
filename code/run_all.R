helpeR::load_custom_functions()
library(RSelenium)


# driver <- RSelenium::rsDriver(chromever = "101.0.4951.15", port = 4566L)
driver <- RSelenium::rsDriver(browser = "firefox", port = 4569L, chromever = "108.0.5359.71" )

urlx <- "https://www.dukascopy.com/plugins/fxMarketWatch/?economic_calendar"
url_daily <- "https://www.fxstreet.com/economic-calendar"
url_trading_eco <- "https://tradingeconomics.com/calendar#"

driver$client$open()
driver$client$navigate(url = url_daily)

get_calender_button <- '//*[@id="Content_C164_Col00"]/div[1]/div/div[2]/div[1]/div/div/div[1]/div/button'
this_month_button <- '//*[@id="Content_C164_Col00"]/div[1]/div/div[2]/div[1]/div/div/div[1]/div/div/div[1]/button[6]'
apply_settings_button <- '//*[@id="Content_C164_Col00"]/div[1]/div/div[2]/div[1]/div/div/div[1]/div/div/div[2]/button'


download.file(url = "https://raw.githubusercontent.com/nikhilchandra-stats/macrodatasetsraw/master/data/daily_fx_macro_data.csv", 
              destfile = glue::glue("data/daily_fx_macro_data{lubridate::today() %>% format('%Y_%m_%d')}.csv") %>% as.character())

latest_data <- fs::dir_info("data/") %>% 
  slice_max(modification_time) %>% 
  pull(path) %>% 
  read_csv()

page_source <- driver$client$getPageSource()

html_read <- page_source %>%
  pluck(1) %>%
  xml2::read_html()

table_extracted <- html_read %>%
  rvest::html_table() %>%
  pluck(1)

cleaned_table_2023 <- clean_fx_street_spread_sheet(
                            data = table_extracted[,1:9], 
                             year_value = "2023", year_filter_string = "January|February")

cleaned_table_2024 <- clean_fx_street_spread_sheet(
  data = table_extracted[,1:9], 
  year_value = "2024", year_filter_string = "April|May|June|July|August|November|October|December")

cleaned_table <- cleaned_table_2023 %>% bind_rows(cleaned_table_2024)

driver$client$closeall()

latest_data_new <- cleaned_table %>% 
  distinct()

previous_latest_data <- latest_data

remove_duplicates <- latest_data_new %>% 
  anti_join(previous_latest_data %>% mutate(time = as.character(time)))

new_data_for_upload <- remove_duplicates %>% 
  bind_rows(previous_latest_data %>% mutate(time = as.character(time)))

write.csv(new_data_for_upload, "data_for_upload/daily_fx_macro_data.csv", row.names = F)
write.csv(previous_latest_data, 
          glue::glue("archived_data/daily_fx_macro_data_{lubridate::today() %>% format('%Y_%m_%d')}.csv"), 
          row.names = F)
