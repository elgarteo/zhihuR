# zhihuR

R Scraper for Zhihu, a popular Quora-like question-and-answer platform in China

## Using

* `search_zhihu()`: search posts by keyword
* `answers_zhihu()`: retrieve answers from a given question
* `comment_zhihu()`: retrieve comments from a given post
* `child_comment_zhihu()`: retrieve child comments from a given comment

## Note

The scrapper requires a Node.js API to sign one of the parameters in the request header. Run `app.js` in the directory `sign` before using the scrapper.
