library(httr)
library(digest)
library(jsonlite)
library(purrr)
library(tibble)
library(dplyr)

# Start sign API before use!
.sign_api <- "127.0.0.1:3000/" #or: https://service-denf06ck-1253616191.gz.apigw.tencentcs.com/release/secret/

.cookie_string <- "\"AIAd0x-RZROPTgLVVevqLR6wbdu2E2cngB8=|1626013553\""

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

# Function to sign x-zse-96 header 
.sign <- function(p, endpoint) {
  raw <- sprintf("101_3_2.0+/api/v4/%s?%s+%s", endpoint, .urlencode(p), .cookie_string)
  md5 <- digest(raw, "md5", serialize = FALSE)
  res <- tryCatch({
    GET(paste0(.sign_api, md5))
    }, error = function(e) {
      stop(e, "\n Has the sign API started correctly?")
    }
  )
  res <- rawToChar(res$content)
  res
}

# List of user agents
.user_agents <- readLines("user-agents.txt")

# Function to compile the proper request to retrieve data from Zhihu
.fetch_data <- function(endpoint, url_params) {
  custom_header <- add_headers(
    `User-Agent` = sample(.user_agents, 1),
    `x-api-version` = "3.0.91",
    `x-zse-93` = "101_3_2.0",
    cookie = paste0("d_c0=", .cookie_string),
    `x-zse-96` = paste0("2.0_", .sign(url_params, endpoint))
  )
  url <- sprintf("https://www.zhihu.com/api/v4/%s", endpoint)
  raw <- GET(paste(url, .urlencode(url_params), sep = "?"), custom_header, user_agent(""))
  if (raw$status_code != 200)
    stop("Error encountered when retrieving data from API!")
  res <- content(raw, as = "text", encoding = "UTF-8")
  res <- fromJSON(res, flatten = TRUE)
  res
}

#' Function to perform search on Zhihu sort by time
#'
#' @param keyword Search keyword
#' @param offset integer, offset of search results, default to `0`
#' @param all logical, whether to fetch all search results, default to `TRUE`
#' 
search_zhihu <- function(keyword, offset = 0, all = TRUE) {
  notdone <- TRUE
  data <- tibble()
  counter <- 1
  message("Searching content with keyword ", keyword, "...")
  while (notdone) {
    message("Fetching page ", counter, "...")
    url_params <- list(
      t = "general", q = keyword, correction = 1, offset = offset, limit = 20, filter_fields = "", 
      lc_idx = offset, show_all_topics = 0, search_source = "Filter", # or Normal
      sort = "created_time" # add vertical = "answer" to retrieve only answers
    )
    res <- tryCatch({
        .fetch_data("search_v3", url_params)
      }, error = function(e) {
        message(e, "\nReturning whatever have been retrieved.")
        break
      })
    data <- bind_rows(data, res$data)
    #
    if (res$paging$is_end | !all) {
      notdone <- FALSE
    } else {
      offset <- as.integer(.urlextract(res$paging["next"])$offset)
      counter <- counter + 1
      Sys.sleep(sample(seq(1, 3, by = .001), 1))
    }
  }
  if (!notdone) # print message only if completed all cycles
    message("All data has been retrieved.")
  data
}

#' Function to scrap answers of a given question
#'
#' @param id Unique id of the question
#' @param offset integer, offset of search results, default to `0`
#' @param all logical, whether to fetch all search results, default to `TRUE`
#' 
answers_zhihu <- function(id, offset = 0, all = TRUE) {
  include_string <- "data[*].is_normal,admin_closed_comment,reward_info,is_collapsed,annotation_action,annotation_detail,collapse_reason,is_sticky,collapsed_by,suggest_edit,comment_count,can_comment,content,editable_content,attachment,voteup_count,reshipment_settings,comment_permission,created_time,updated_time,review_info,relevant_info,question,excerpt,is_labeled,paid_info,paid_info_content,relationship.is_authorized,is_author,voting,is_thanked,is_nothelp,is_recognized;data[*].mark_infos[*].url;data[*].author.follower_count,vip_info,badge[*].topics;data[*].settings.table_of_content.enabled"
  include_string <- gsub("%2A", "*", URLencode(include_string, reserved = TRUE)) # the asterisk should not be encoded
  notdone <- TRUE
  data <- tibble()
  counter <- 1
  message("Fetching answers from question ", id, "...")
  while (notdone) {
    message("Fetching page ", counter, "...")
    url_params <- list(
      include = include_string,
      offset = offset, limit = 20, sort_by = "updated"
    )
    endpoint <- sprintf("questions/%s/answers", id)
    res <- tryCatch({
      .fetch_data(endpoint, url_params)
    }, error = function(e) {
      message(e, "\nReturning whatever have been retrieved.")
      break
    })
    res$master_id <- id
    data <- bind_rows(data, res$data)
    #
    if (res$paging$is_end | !all) {
      notdone <- FALSE
    } else {
      offset <- as.integer(.urlextract(res$paging["next"])$offset)
      counter <- counter + 1
      Sys.sleep(sample(seq(1, 3, by = .001), 1))
    }
  }
  if (!notdone)
    message("All data has been retrieved.")
  data
}

#' Function to scrap comments
#'
#' @param id Unique id of the content
#' @param type Type of the content, can be `"answer"`, `"article"`, 
#' `"question"`, `"videoanwser"`, or `"zvideo"`
#' @param offset integer, offset of search results, default to `0`
#' @param all logical, whether to fetch all search results, default to `TRUE`
#' 
comment_zhihu <- function(id, type, offset = 0, all = TRUE) {
  notdone <- TRUE
  data <- tibble()
  counter <- 1
  message("Fetching comments from content ", id, "...")
  while (notdone) {
    message("Fetching page ", counter, "...")
    url_params <- list(
      order = "reverse", # "normal" for default order
      limit = 20, offset = offset, status = "open"
    )
    endpoint <- sprintf("%s/%s/root_comments", paste0(type, "s"), id)
    res <- tryCatch({
      .fetch_data(endpoint, url_params)
    }, error = function(e) {
      message(e, "\nReturning whatever have been retrieved.")
      break
    })
    #res <- res[-28] # remove useless column child_comments 
    res$master_id <- id # attach id of master content
    data <- bind_rows(data, res$data)
    #
    if (res$paging$is_end | !all) {
      notdone <- FALSE
    } else {
      offset <- as.integer(.urlextract(res$paging["next"])$offset)
      counter <- counter + 1
      Sys.sleep(sample(seq(1, 3, by = .001), 1))
    }
  }
  if (!notdone)
    message("All data has been retrieved.")
  data
}

#' Function to scrap child comments
#'
#' @param id Unique id of the root comment
#' @param offset integer, offset of search results, default to `0`
#' @param all logical, whether to fetch all search results, default to `TRUE`
#' 
child_comment_zhihu <- function(id, type, offset = 0, all = TRUE) {
  notdone <- TRUE
  data <- tibble()
  counter <- 1
  message("Fetching child comments of comment ", id, "...")
  while (notdone) {
    message("Fetching page ", counter, "...")
    url_params <- list(
      limit = 20, offset = offset
    )
    endpoint <- sprintf("comments/%s/child_comments", id)
    res <- tryCatch({
      .fetch_data(endpoint, url_params)
    }, error = function(e) {
      message(e, "\nReturning whatever have been retrieved.")
      break
    })
    res$master_id <- id
    data <- bind_rows(data, res$data)
    #
    if (res$paging$is_end | !all) {
      notdone <- FALSE
    } else {
      offset <- as.integer(.urlextract(res$paging["next"])$offset)
      counter <- counter + 1
      Sys.sleep(sample(seq(1, 3, by = .001), 1))
    }
  }
  if (!notdone)
    message("All data has been retrieved.")
  data
}
