source("zhihu_scraper.R")

test <- search_zhihu("动态清零")

test2 <- zhihu_answer(330180480)

test3 <- zhihu_comment(2327984980, "answer")

test4 <- zhihu_child_comment(test3$id[3])
