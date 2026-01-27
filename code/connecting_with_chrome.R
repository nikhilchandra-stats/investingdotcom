helpeR::load_custom_functions()
library(RSelenium)


# driver <- RSelenium::rsDriver(chromever = "101.0.4951.15", port = 4566L)
driver <- RSelenium::rsDriver(
                              port = 4999L, 
                              phantomver = NULL, 
                              chromever = "141.0.7390.66")

urlx <- "https://www.investing.com/"

driver$client$open()
driver$client$navigate(url = urlx)

gc()
gc()
