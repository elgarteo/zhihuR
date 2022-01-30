library(httr)
library(digest)
library(jsonlite)
library(purrr)
library(tibble)
library(dplyr)

# Start encryption API before use!
.encryption_api <- "127.0.0.1:3000/" #ori: https://service-denf06ck-1253616191.gz.apigw.tencentcs.com/release/secret/

# Function to convert list object into URL encoded format
.urlencode <- function(x) {
  URLencode(paste0(paste(names(x), x, sep = "="), collapse = "&"))
}

# Function to extract URL parameters into list object
.urlextract <- function(x) {
  params <- gsub("^[^?]+\\?", "", x)
  params <- strsplit(params, "&")[[1]]
  params <- strsplit(params, "=")
  setNames(map(params, function(y) y[2]), map_chr(params, function(y) y[1]))
}

# Function to encrypt x-zse-96 header 
.encrypt <- function(p) {
  raw <- sprintf("101_3_2.0+/api/v4/search_v3?%s+\"AIAd0x-RZROPTgLVVevqLR6wbdu2E2cngB8=|1626013553\"", .urlencode(p))
  md5 <- digest(raw, "md5", serialize = FALSE)
  res <- GET(paste0(.encryption_api, md5))
  res <- rawToChar(res$content)
  res
}

# List of user agents
.user_agents <- readLines("user-agents.txt")

#' Function to perform search on Zhihu sort by time
#'
#' @param keyword Search keyword
#' @param offset integer, offset of search results, default to `1`
#' @param all logical, whether to fetch all search results, default to `TRUE`
#' @param debug logical, debugging mode, default to `FALSE`
#' 
search_zhihu <- function(keyword, offset = 0, all = TRUE, debug = FALSE) {
  notdone <- TRUE
  data <- tibble()
  counter <- 1
  while (notdone) {
    message("Fetching page ", counter, "...")
    url_params <- list(
      t = "general", q = keyword, correction = 1, offset = offset, limit = 20, filter_fields = "", 
      lc_idx = offset, show_all_topics = 0, search_source = "Filter", # or Normal
      vertical = "answer", sort = "created_time"
    )
    custom_header <- add_headers(
      #`User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:96.0) Gecko/20100101 Firefox/96.0",
      `User-Agent` = sample(.user_agents, 1),
      `x-api-version` = "3.0.91",
      `x-zse-93` = "101_3_2.0",
      cookie = "d_c0=\"AIAd0x-RZROPTgLVVevqLR6wbdu2E2cngB8=|1626013553\"",
      `x-zse-96` = paste0("2.0_", .encrypt(url_params))
    )
    search_api <- "https://www.zhihu.com/api/v4/search_v3"
    raw <- GET(search_api, custom_header, query = url_params, user_agent(""))
    if (raw$status_code != 200) {
      message("Error encountered when retrieving data from API!\nReturning whatever have been retrieved.")
      if (debug)
        return(list(data, raw))
      return(data)
    }
    res <- content(raw, as = "text", encoding = "UTF-8")
    res <- fromJSON(res)
    data <- bind_rows(data, res$data$object)
    #
    if (res$paging$is_end | !all) {
      notdone <- FALSE
    } else {
      offset <- as.integer(.urlextract(res$paging["next"])$offset)
      counter <- counter + 1
      Sys.sleep(sample(seq(1, 3, by = .001), 1))
    }
  }
  message("All data has been retrieved.")
  data
}

test <- search_zhihu("河南春晚", debug = TRUE)
