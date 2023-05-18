helpeR::load_custom_functions()
library(RSelenium)


# driver <- RSelenium::rsDriver(chromever = "101.0.4951.15", port = 4566L)
driver <- RSelenium::rsDriver(browser = "firefox", port = 4569L, chromever = "108.0.5359.71" )

driver$client$open()
driver$client$navigate(url = "https://www.pmc.gov.au/publications/abbreviations-and-acronyms-groups-or-topics")

html_read <- driver$client$getPageSource() %>% 
  pluck(1) %>% 
  rvest::read_html()

base_xpath <- "/html/body/div/div/div/div[2]/div/div[3]/main/section/div[2]/div/article/div/div/dl[xxxx]"
"/html/body/div/div/div/div[2]/div/div[3]/main/section/div[2]/div/article/div/div/dl[26]"
paths_vec <- c()

for (i in 1:26) {
  
  paths_vec[i] <- 
    base_xpath %>% 
    str_replace("xxxx", as.character(i))
  
}

strip_table_PMC <- function(
  html_read = html_read,
  xpath_x = "/html/body/div/div/div/div[2]/div/div[3]/main/section/div[2]/div/article/div/div/dl[1]"
    ) {
  
  table_dat <- 
    html_read %>% 
    rvest::html_element(xpath = xpath_x) %>% 
    rvest::html_children() %>% 
    rvest::html_text2()
  
  abbrev_table <- 
    tibble(
      abbr = rep("0", length(table_dat)/2), 
      full_word = rep("0", length(table_dat)/2)
    )
  
  loop_seq <- seq(1, (length(table_dat)/2), 2)
  
  for (i in loop_seq) {
    
    abbrev_table[i, 1] <- table_dat[i]
    abbrev_table[i, 2] <- table_dat[i + 1]
    
  }  
  
  return(abbrev_table)
  
}


safely_extract <- purrr::safely(strip_table_PMC, otherwise = NULL)

tables_list <- paths_vec %>% 
  map(
    ~ safely_extract( html_read = html_read,
                      xpath_x = .x) %>%
      pluck(1)
  ) %>% 
  keep(~ !is.null(.x)) %>%
  map_dfr(bind_rows) %>%
  filter(abbr != 0)

write.csv(tables_list, file = "aus_gov_abbrevations.csv", row.names = F)  
