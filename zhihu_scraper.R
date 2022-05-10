library(httr)
library(digest)
library(jsonlite)
library(purrr)
library(tibble)
library(dplyr)

# Start sign API before use!
.sign_api <- "127.0.0.1:3000/" #or: https://service-denf06ck-1253616191.gz.apigw.tencentcs.com/release/secret/

.cookie_string <- "\"AIAd0x-RZROPTgLVVevqLR6wbdu2E2cngB8=|1626013553\""

# Function to convert list object into encoded URL parameters
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
      stop(e, "Has the sign API started?")
    }
  )
  res <- rawToChar(res$content)
  res
}

# List of user agents
.user_agents <- readLines("user-agents.txt")

# Function to compile the proper request to retrieve data from Zhihu
.access_api <- function(endpoint, url_params) {
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
    stop("API refuses to return data (HTTP ", raw$status_code, ")")
  res <- content(raw, as = "text", encoding = "UTF-8")
  res <- fromJSON(res, flatten = TRUE)
  res
}

# Function to download and combine data from multiple pages
.fetch_data <- function(endpoint, url_params, all) {
  notdone <- TRUE
  data <- tibble()
  counter <- 1
  total <- NA
  while (notdone) {
    message("Fetching page ", counter, ifelse(is.na(total), "", paste0(" of ", ceiling(total / 20))), "...")
    res <- try({
      .access_api(endpoint, url_params)
    }, silent = TRUE)
    if (inherits(res, "try-error")) {
      message("Encountered the following error when retrieving data:\n",
              "`````\n", res, "`````\nReturning whatever has been retrieved")
      break
    }
    data <- bind_rows(data, res$data)
    #
    if (res$paging$is_end | !all) {
      notdone <- FALSE
    } else {
      url_params$offset <- as.integer(.urlextract(res$paging["next"])$offset)
      total <- ifelse("totals" %in% names(res), as.integer(res$paging["totals"]), NA)
      counter <- counter + 1
      Sys.sleep(sample(seq(1, 3, by = .001), 1))
    }
  }
  if (!notdone) # print message only if cycle completed
    message("All data has been retrieved")
  data
}

#' Function to perform search on Zhihu sort by time
#'
#' @param keyword Search keyword
#' @param type type of posts to search, default to `all`
#' @param time time interval to search, default to `all`
#' @param sort how should the result be sorted, default to `default`
#' @param offset integer, offset of search results, default to `0`
#' @param all logical, whether to fetch all search results, default to `TRUE`
#' 
search_zhihu <- function(keyword, 
                         type = c("all", "answer", "article", "zvideo"), 
                         time = c("all", "a_day", "a_week", "a_month", "three_month", "half_a_year", "a_year"),
                         sort = c("default", "upvoted_count", "created_time"),
                         offset = 0, all = TRUE) {
  message("Searching content with keyword ", keyword, "...")
  url_params <- list(
    t = "general", q = keyword, correction = 1, offset = offset, limit = 20, 
    filter_fields = "", lc_idx = offset, show_all_topics = 0, 
    search_source = "Filter"#, # "Normal" if no additional parameters afterwards
    #sort = "created_time" # add vertical = "answer" to retrieve only answers
  )
  if (type[1] != "all") {
    url_params[["vertical"]] <- type[1]
  }
  if (time[1] != "all") {
    url_params[["time_interval"]] <- time[1]
  }
  if (sort[1] != "default") {
    url_params[["sort"]] <- sort[1]
  }
  .fetch_data("search_v3", url_params, all)
}

#' Function to scrap answers of a given question
#'
#' @param id Unique id of the question
#' @param offset integer, offset of search results, default to `0`
#' @param all logical, whether to fetch all search results, default to `TRUE`
#' 
zhihu_answer <- function(id, offset = 0, all = TRUE) {
  message("Fetching answers from question ", id, "...")
  include_string <- "data[*].is_normal,admin_closed_comment,reward_info,is_collapsed,annotation_action,annotation_detail,collapse_reason,is_sticky,collapsed_by,suggest_edit,comment_count,can_comment,content,editable_content,attachment,voteup_count,reshipment_settings,comment_permission,created_time,updated_time,review_info,relevant_info,question,excerpt,is_labeled,paid_info,paid_info_content,relationship.is_authorized,is_author,voting,is_thanked,is_nothelp,is_recognized;data[*].mark_infos[*].url;data[*].author.follower_count,vip_info,badge[*].topics;data[*].settings.table_of_content.enabled"
  include_string <- gsub("%2A", "*", URLencode(include_string, reserved = TRUE)) # asterisks are not encoded
  url_params <- list(
    include = include_string,
    offset = offset, limit = 20, sort_by = "updated"
  )
  endpoint <- sprintf("questions/%s/answers", id)
  .fetch_data(endpoint, url_params, all)
}

#' Function to scrap comments of a given post
#'
#' @param id Unique id of the content
#' @param type Type of post, typically `"answer"`, `"article"`, 
#' `"question"`, `"videoanwser"`, or `"zvideo"`, or as specified in the 
#' column `object.type` in the result returned by `search_zhihu()`
#' @param offset integer, offset of search results, default to `0`
#' @param all logical, whether to fetch all search results, default to `TRUE`
#' 
zhihu_comment <- function(id, type, offset = 0, all = TRUE) {
  message("Fetching comments from content ", id, "...")
  url_params <- list(
    order = "reverse", # "normal" for default order
    limit = 20, offset = offset, status = "open"
  )
  endpoint <- sprintf("%s/%s/root_comments", paste0(type, "s"), id)
  .fetch_data(endpoint, url_params, all)
}

#' Function to scrap child comments of a given comment
#'
#' @param id Unique id of the root comment
#' @param offset integer, offset of search results, default to `0`
#' @param all logical, whether to fetch all search results, default to `TRUE`
#' 
zhihu_child_comment <- function(id, type, offset = 0, all = TRUE) {
  message("Fetching child comments of comment ", id, "...")
  url_params <- list(
    limit = 20, offset = offset
  )
  endpoint <- sprintf("comments/%s/child_comments", id)
  .fetch_data(endpoint, url_params, all)
}

#' Function to scrap user information
#'
#' @param handle Unique handle of a user
#' 
zhihu_user <- function(handle) {
  message("Fetching information of user `", handle, "`...")
  include_string <- "juror,locations,employments,gender,educations,business,voteup_count,thanked_Count,follower_count,following_count,cover_url,following_topic_count,following_question_count,following_favlists_count,following_columns_count,avatar_hue,answer_count,zvideo_count,articles_count,pins_count,question_count,columns_count,commercial_question_count,favorite_count,favorited_count,logs_count,included_answers_count,included_articles_count,included_text,message_thread_token,account_status,is_active,is_bind_phone,is_force_renamed,is_privacy_protected,is_blocking,is_blocked,is_following,is_followed,mutual_followees_count,vote_to_count,vote_from_count,thank_to_count,thank_from_count,thanked_count,description,hosted_live_count,participated_live_count,allow_message,recognized_count,widgets,industry_category,org_name,org_homepage,is_org_createpin_white_user,badge[?(type=best_answerer)].topics"
  url_params <- list(
    include = include_string
  )
  endpoint <- sprintf("members/%s", handle)
  .access_api(endpoint, url_params)
}
