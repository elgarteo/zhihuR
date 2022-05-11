# zhihuR

R Scraper for Zhihu, a popular Quora-like question-and-answer platform in China

## Using

* `search_zhihu()`: search posts by keyword
* `zhihu_answer()`: retrieve answers from a given question
* `zhihu_comment()`: retrieve comments from a given post
* `zhihu_child_comment()`: retrieve child comments from a given comment
* `zhihu_user()`: retrieve details of a given comment
* `zhihu_question_metadata()`: retrieve metadata of a given question
* `zhihu_answer_metadata()`: retrieve metadata of a given comment

## Note

The scrapper requires a Node.js API to sign a parameter in the request header. Run `app.js` in the directory `sign` before using the scrapper.
