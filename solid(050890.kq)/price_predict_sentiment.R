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




### lstm model using sentiment
stock_data_input = layer_input(batch_shape = c(batch_size, time_lag, 100),
                               name = 'stock_data_input') %>% 
  layer_lstm(units = 100,
             batch_size = batch_size,
             return_sequences = T,
             stateful = T,
             name = 'lstm_layer') %>% 
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 50,
             return_sequences = F,
             stateful = T,
             name = 'stock_data_process') %>%
  layer_dropout(rate = 0.5)

lstm_output = stock_data_input %>% 
  layer_dense(units = 1,
              name = 'lstm_output')
  
sentiment_input = layer_input(batch_shape = c(50, 1), 
                              name = 'sentiment_input')

bind_layer = layer_concatenate(inputs = c(lstm_output, sentiment_input),
                               name = 'bind_layer')
   

model_lstm_sentiment = keras_model(inputs = c(stock_data_input, sentiment_input),
                                   outputs = c(lstm_output, final_output)) %>% 
  compile(loss = 'mae', 
          optimizer = 'Nadam')
