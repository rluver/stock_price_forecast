require("data.table")
require("stringr")
require("lubridate")
require("dplyr")
require("plotly")
require("keras")




# set options
options(scipen = 10)




# get price
solid = bind_rows(list.files("solid(050890.kq)/solid_data", pattern = "*.csv", full.names = T) %>% 
                    purrr::map_df(~fread(., encoding = "UTF-8"))) %>% 
  setNames(c("Date", "Close", "Compare", "Volume", "Trading", "Open", "High", "Low", "Cap", "Stocks")) %>% 
  mutate(Date = ymd(Date),
         Close = str_replace_all(Close, ",", "") %>% as.numeric(),
         Volume = str_replace_all(Volume, ",", "") %>% as.numeric(),
         Trading = str_replace_all(Trading, ",", "") %>% as.numeric(),
         Open = str_replace_all(Open, ",", "") %>% as.numeric(),
         High = str_replace_all(High, ",", "") %>% as.numeric(),
         Low = str_replace_all(Low, ",", "") %>% as.numeric(),
         Cap = str_replace_all(Cap, ",", "") %>% as.numeric(),
         Stocks = str_replace_all(Stocks, ",", "") %>% as.numeric()) %>% 
  arrange(Date)

# min-max scaling
solid_scaled = solid %>% select(Date, Open, High, Low, Close, Volume) %>% 
  mutate(Open = (Open - min(Open)) / (max(Open) - min(Open)),
         High = (High - min(High)) / (max(High) - min(High)),
         Low = (Low - min(Low)) / (max(Low) - min(Low)),
         Close = (Close - min(Close)) / (max(Close) - min(Close)),
         Volume = (Volume - min(Volume)) / (max(Volume) - min(Volume)))




# ml
## nnar
model_nnar = nnetar(solid$Close)
model_nnar_fit = model_nnar %>% forecast(h = 20, PI= T)

plot_ly() %>% 
  add_lines(x = c(1:((solid %>% dim())[1])),
            y = solid$Close,
            name = "observed") %>% 
  add_ribbons(x = c((((solid %>% dim())[1]) + 1) : (((solid %>% dim())[1]) + 20)),
              ymin = model_nnar_fit$lower[, 2],
              ymax = model_nnar_fit$upper[, 2],
              color = "gray95",
              name = "95% confidence") %>% 
  add_ribbons(x = c((((solid %>% dim())[1]) + 1) : (((solid %>% dim())[1]) + 20)),
              ymin = model_nnar_fit$lower[, 1],
              ymax = model_nnar_fit$upper[, 1],
              color = "gray80",
              name = "80% confidence") %>% 
  add_lines(x = c((((solid %>% dim())[1]) + 1) : (((solid %>% dim())[1]) + 20)),
            y = model_nnar_fit$mean,
            color = "blue",
            name = "predict")



## keras
#### data split for train, test and set var
batch_size = 50
time_lag = 5

#### train
x_train = solid_scaled %>% slice(seq(900 + time_lag)) %>% 
  tibble::column_to_rownames("Date") %>% 
  mutate(Open = lag(Open, time_lag),
         High = lag(High, time_lag),
         Low = lag(Low, time_lag),
         Close = lag(Close, time_lag),
         Volume = lag(Volume, time_lag)) %>% 
  filter(!is.na(Open)) %>% 
  as.matrix() %>% 
  array(dim = c(900, time_lag, 5))
y_train = solid_scaled %>% select(Close) %>% 
  slice(seq(900 + time_lag + 1)) %>% 
  mutate(Close = lag(Close, time_lag + 1)) %>% 
  filter(!is.na(Close)) %>% 
  as.matrix() %>% 
  array(dim = c(900, 1))

#### test
x_test = solid_scaled %>% slice(seq(885 + time_lag + 1, nrow(solid))) %>% 
  tibble::column_to_rownames("Date") %>% 
  mutate(Open = lag(Open, time_lag),
         High = lag(High, time_lag),
         Low = lag(Low, time_lag),
         Close = lag(Close, time_lag),
         Volume = lag(Volume, time_lag)) %>% 
  filter(!is.na(Open)) %>% 
  as.matrix() %>% 
  array(dim = c(350, time_lag, 5))
y_test = solid_scaled %>% select(Close) %>% 
  slice(seq(885 + time_lag + 1, nrow(solid))) %>% 
  mutate(Close = lag(Close, time_lag + 1)) %>% 
  filter(!is.na(Close)) %>% 
  bind_rows(data.frame(Close = 0.6104294)) %>% 
  as.matrix() %>% 
  array(dim = c(350, 1))




### lstm
model_lstm = keras_model_sequential() %>% 
  layer_lstm(units = 100,
             input_shape = c(time_lag, 5),
             batch_size = batch_size,
             return_sequences = T,
             stateful = T) %>%
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 50,
             return_sequences = F,
             stateful = T) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1) %>% 
  compile(loss = 'mae', 
          optimizer = 'Nadam')

#### train
for(i in 1:1000){
  model_lstm %>% 
    fit(x = x_train,
        y = y_train,
        batch_size = batch_size,
        epochs = 1,
        verbose = 1,
        shuffle = FALSE)
  model_lstm %>% reset_states()
}
  



### gru
model_gru = keras_model_sequential() %>% 
  layer_gru(units = 100, 
            input_shape = c(time_lag, 5),
            batch_size = batch_size,
            return_sequences = T,
            stateful = T) %>% 
  layer_dropout(0.5) %>% 
  layer_gru(units = 50, 
            return_sequences = F,
            stateful = T) %>% 
  layer_dropout(0.5) %>% 
  layer_dense(units = 1) %>% 
  compile(loss = "mae",
          optimizer = "Nadam")


#### train
for(i in 1:1000){
  model_gru %>% 
    fit(x = x_train,
        y = y_train,
        batch_size = batch_size,
        epochs = 1,
        verbose = 1,
        shuffle = FALSE)
  model_gru %>% reset_states()
}




### compare
#### create result table
result_table = solid %>% 
  select(Date) %>% 
  tail(350) %>% 
  bind_cols(data_frame(real = y_test,
                       predict_lstm = model_lstm %>% predict(x_test, batch_size = batch_size) %>% as.numeric(),
                       predict_gru = model_gru %>% predict(x_test, batch_size = batch_size) %>% as.numeric()) %>% 
              mutate(real = real*(max(solid$Close) - min(solid$Close)) + min(solid$Close),
              predict_lstm = predict_lstm*(max(solid$Close) - min(solid$Close)) + min(solid$Close),
              predict_gru = predict_gru*(max(solid$Close) - min(solid$Close)) + min(solid$Close))
  )

#### visualization
result_table %>%
  plot_ly(x = ~ Date) %>% 
  add_lines(y = ~ real,
            name = "Actual",
            mode = "lines",
            line = list(width = 1.5)) %>% 
  add_lines(y = ~ predict_lstm,
            name = "LSTM",
            mode = "lines",
            line = list(width = 0.7)) %>% 
  add_lines(y = ~ predict_gru,
            name = "GRU",
            mode = "lines",
            line = list(width = 0.7)) %>% 
  layout(yaxis = list(title = "Price"))
