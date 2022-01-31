source("zhihu_scraper.R")

test <- search_zhihu("动态清零")

test2 <- answers_zhihu(330180480)

test3 <- comment_zhihu(2327984980, "answer")

test4 <- child_comment_zhihu(test3$id[3])
