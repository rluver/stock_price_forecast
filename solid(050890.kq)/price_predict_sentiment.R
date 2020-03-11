require('data.table')
require('stringr')
require('lubridate')
require('dplyr')
require('httr')
require('XML')
require('keras')




# define user function

get_data = function(i){
  start = str_c('&start=', 100*(i - 1) + 1)
  
  GET(str_c(url, keyword, display, sort, start),
      add_headers('X-Naver-Client-Id' = 'TTVxhVoGSPjIQFdRVJdn',
                  'X-Naver-Client-Secret' = 'JGC2v63ocC')) %>% 
    xmlParse() %>% 
    return()
}

get_index = function(data){
  data %>% 
    xpathSApply(., '/rss/channel/item/title', xmlValue) %>% 
    str_detect('쏠리드') %>% 
    which() %>% 
    return()
}




# naver new api
## set var
url = 'https://openapi.naver.com/v1/search/news.xml?query='
keyword = iconv('쏠리드', to = 'UTF-8') %>% URLencode()
display = '&display=100'
sort = '&sort=date'
news = data.table()


## get data
for(i in 1:10){
  news = news %>% 
    bind_rows(
        data.table(date = (get_data(i) %>% xpathSApply(., '/rss/channel/item/pubDate', xmlValue) %>%
                     str_extract_all('\\d{2}.+\\d{4}\\s{1}', simplify = T) %>% 
                     dmy())[get_data(i) %>% get_index()],
                   title = (get_data(i) %>% 
                     xpathSApply(., '/rss/channel/item/title', xmlValue))[get_data(i) %>% get_index()] %>% 
                     str_replace_all('</?b>', ' ') %>% 
                     str_replace_all('^[:space:]{1}', '') %>% 
                     str_replace_all('[(?:쏠리드) (?:,)]', ''),
                   text = (get_data(i) %>% 
                     xpathSApply(., '/rss/channel/item/description', xmlValue))[get_data(i) %>% get_index()])
    )
  
}