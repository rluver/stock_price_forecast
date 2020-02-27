require("data.table")
require("rugarch")
require("dplyr")
require("lubridate")
require("stringr")
require("quantmod")
require("lmtest")
require("forecast")
require("dynlm")
require("FinTS")
require("tseries")
require("ggplot2")
require("ggfortify")
require("plotly")




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


# as xts

solid_xts = solid %>% select(Date, Open, High, Low, Close, Volume) %>% 
  tibble::column_to_rownames("Date") %>% 
  as.matrix()




# candle chart

solid_xts %>% barChart()
addMACD()
addBBands()


# plotly candle chart

# create bollinger bands
solid_full = solid %>% select(Date, Open, High, Low, Close, Volume) %>% 
  bind_cols(
    solid %>% select(Date, High, Low, Close) %>% 
      tibble::column_to_rownames("Date") %>% setNames(c("High", "Low", "Close")) %>% 
      as.matrix() %>% BBands() %>% as.data.frame() %>% select(1:3)
  ) %>% filter(!is.na(up)) %>% 
  mutate(direction = ifelse(Close >= Open, "Increasing", "Decreasing"))

# candle chart with bband
solid_full %>% plot_ly(x = ~ Date,
                       type = "candlestick",
                       open = ~ Open,
                       close = ~ Close,
                       high = ~ High,
                       low = ~ Low) %>% 
  add_lines(x = ~ Date, y = ~ up , name = "B Bands",
            line = list(color = '#ccc', width = 0.5),
            legendgroup = "Bollinger Bands",
            hoverinfo = "none", inherit = F) %>% 
  add_lines(x = ~ Date, y = ~ dn, name = "B Bands",
            line = list(color = '#ccc', width = 0.5),
            legendgroup = "Bollinger Bands", inherit = F,
            showlegend = FALSE, hoverinfo = "none") %>% 
  add_lines(x = ~ Date, y = ~ mavg, name = "Mv Avg",
            line = list(color = '#E377C2', width = 0.5),
            hoverinfo = "none", inherit = F) %>% 
  layout(yaxis = list(title = "Price")) -> candlechart

# add volume bar chart
solid_full %>% 
  plot_ly(x = ~ Date,
          y = ~ Volume,
          type = "bar",
          name = "Volume",
          color = ~ direction,
          colors = c("#17BECF", "#7F7F7F")) %>%
  layout(yaxis = list(title = "Volume")) -> volumechart

# create rangeselector buttons
range_selector = list(visible = T, x = 0.5, y = -0.055,
                      xanchor = "center", yref = "paper",
                      font = list(size = 9),
                      buttons = list(
                        list(count = 1,
                             label = "Reset",
                             step = "all"),
                        list(count = 1,
                             label = "1 Year",
                             step = "year",
                             stepmode = "backward"),
                        list(count = 3,
                             label = "3 Month",
                             step = "month",
                             stepmode = "backward"),
                        list(count = 1,
                             label = "1 Month",
                             step = "month",
                             stepmode = "backward")
                      )
)

# subplot with shared x axis
subplot(candlechart, volumechart, heights = c(0.7, 0.2), nrows = 2,
        shareX = T, titleY = T) %>% 
  layout(title = str_c("Solid(050890.kq) Chart 2015-01-29 ~ ", Sys.Date()),
         xaxis = list(rangeselector = range_selector),
         legend = list(orientation = "h", x = 0.5, y = 1,
                       xanchor = "center", yref = "paper",
                       font = list(size = 10),
                       bgcolor = "transparent"))


# close graphic 

solid %>% plot_ly(x = ~ Date,
                  y = ~ Close,
                  mode = "lines")




# time series
# arima
# log
# check
solid$Close %>% log() %>% auto.arima() %>% summary()
solid$Close %>% log() %>% adf.test()
solid$Close %>% log() %>% auto.arima() %>% ggtsdiag()
solid$Close %>% log() %>% auto.arima() %>% checkresiduals()


# diff
# check
model_arima = solid$Close %>% auto.arima()
summary(model_arima)
# autocorrelation
model$residuals %>% adf.test()
# heteroskedasticity, autocorrelation, independence
model_arima %>% ggtsdiag(conf.init.fill = "#0000FF") 
model_arima %>% checkresiduals()


# predict by arima

fore = model_arima %>% forecast(20)

plot_ly() %>% 
  add_lines(x = c(1:((solid %>% dim())[1])),
            y = solid$Close,
            name = "observed") %>% 
  add_ribbons(x = c((((solid %>% dim())[1]) + 1) : (((solid %>% dim())[1]) + 20)),
              ymin = fore$lower[, 2],
              ymax = fore$upper[, 2],
              color = "gray95",
              name = "95% confidence") %>% 
  add_ribbons(x = c((((solid %>% dim())[1]) + 1) : (((solid %>% dim())[1]) + 20)),
              ymin = fore$lower[, 1],
              ymax = fore$upper[, 1],
              color = "gray80",
              name = "80% confidence") %>% 
  add_lines(x = c((((solid %>% dim())[1]) + 1) : (((solid %>% dim())[1]) + 20)),
            y = fore$mean,
            color = "blue",
            name = "predict")




# arch model

model_dynlm = dynlm(solid$Close ~ 1)
summary(model_dynlm)

e_sq = model_dynlm$residuals^2 %>% ts()
model_arch = dynlm(ehatsq ~ L(e_sq))
summary(model_arch)

# arch effect
ArchTest(solid$close ,lags = 1, demean = T)

# same as arch when c(0, 1)
model_garch = solid$Close %>% ts() %>% garch(c(0, 1))

summary(model_garch)

model_garch$fitted.values[-1, 1]^2 %>% ts() %>%  autoplot() %>% ggplotly()




# garch model
# garch
model_sgarch = ugarchspec(variance.model = list(model = "sGARCH",
                                                garchOrder = c(1, 1)),
                          mean.model = list(armaOrder = c(0, 0)),
                          distribution.model = "std")
# fit
model_sgarch_fit = ugarchfit(spec = model_sgarch, 
                             data = solid$Close %>% ts())

# check
coef(model_sgarch_fit)
model_sgarch_fit@fit$fitted.values %>% ts() %>% autoplot() %>% ggplotly()
model_sgarch_fit@fit$sigma^2 %>% ts() %>% autoplot() %>% ggplotly()
model_sgarch_fit@fit %>% checkresiduals()

# predict
model_sgarch_fit %>% ugarchforecast(n.ahead = 20)
# graph
plot_ly() %>% 
  add_lines(x = c(1:((solid %>% dim())[1])),
            y = solid$Close,
            name = "observed") %>% 
  add_lines(x = c((((solid %>% dim())[1]) + 1) : (((solid %>% dim())[1]) + 20)),
            y = (model_sgarch_fit %>% ugarchforecast(n.ahead = 20))@forecast$seriesFor,
            name = "predict")



# fgarch
model_fgarch = ugarchspec(variance.model = list(model = "fGARCH",
                                                garchOrder = c(1, 1),
                                                submodel = "TGARCH"),
                          mean.model=list(armaOrder = c(0, 0)),
                          distribution.model = "norm"
                          )

# fit
model_fgarch_fit = ugarchfit(spec = model_fgarch, 
                             data = solid$close %>% ts())

# check
coef(model_fgarch_fit)
model_fgarch_fit@fit$fitted.values %>% ts() %>% autoplot() %>% ggplotly()
model_fgarch_fit@fit$sigma^2 %>% ts() %>% autoplot() %>% ggplotly()
model_fgarch_fit@fit %>% checkresiduals()

# predict
model_fgarch_fit %>% ugarchforecast(n.ahead = 20)
# graph
plot_ly() %>% 
  add_lines(x = c(1:((solid %>% dim())[1])),
            y = solid$Close,
            name = "observed") %>% 
  add_lines(x = c((((solid %>% dim())[1]) + 1) : (((solid %>% dim())[1]) + 20)),
            y = (model_fgarch_fit %>% ugarchforecast(n.ahead = 20))@forecast$seriesFor,
            name = "predict")




# arch-m
model_fgarch = ugarchspec(variance.model = list(model = "fGARCH",
                                                garchOrder = c(1,1),
                                                submodel = "APARCH"),
                          mean.model = list(armaOrder = c(0, 0),
                                            include.mean=TRUE,
                                            archm = TRUE,
                                            archpow = 2),
                          distribution.model = "std"
                          )
model_fgarch_fit = ugarchfit(spec = model_fgarch, 
                             data = solid$Close %>% ts())

# check
coef(model_fgarch_fit)
model_fgarch_fit@fit$fitted.values %>% ts() %>% autoplot() %>% ggplotly()
model_fgarch_fit@fit$sigma^2 %>% ts() %>% autoplot() %>% ggplotly()
model_fgarch_fit@fit %>% checkresiduals()
