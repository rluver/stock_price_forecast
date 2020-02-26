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




# get values

solid = bind_rows(list.files("d:/solid", pattern = "*.csv", full.names = T) %>% 
                    purrr::map_df(~fread(., encoding = "UTF-8"))) %>% 
  setNames(c("date", "close", "compare", "volumn", "trading", "start", "high", "low", "cap", "stocks")) %>% 
  mutate(date = ymd(date),
         close = str_replace_all(close, ",", "") %>% as.numeric(),
         volumn = str_replace_all(volumn, ",", "") %>% as.numeric(),
         trading = str_replace_all(trading, ",", "") %>% as.numeric(),
         start = str_replace_all(start, ",", "") %>% as.numeric(),
         high = str_replace_all(high, ",", "") %>% as.numeric(),
         low = str_replace_all(low, ",", "") %>% as.numeric(),
         cap = str_replace_all(cap, ",", "") %>% as.numeric(),
         stocks = str_replace_all(stocks, ",", "") %>% as.numeric()) %>% 
  arrange(date)




# candle chart

candleChart(solid)


# close graphic 

solid %>% plot_ly(x = ~ date,
                  y = ~ close,
                  mode = "lines")




# time series
# arima
# log
# check
solid$close %>% log() %>% auto.arima() %>% summary()
solid$close %>% log() %>% adf.test()
solid$close %>% log() %>% auto.arima() %>% ggtsdiag()
solid$close %>% log() %>% auto.arima() %>% checkresiduals()


# diff
# check
model_arima = solid$close %>% auto.arima()
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
            y = solid$close,
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

model_dynlm = dynlm(solid$close ~ 1)
summary(model_dynlm)

e_sq = model_dynlm$residuals^2 %>% ts()
model_arch = dynlm(ehatsq ~ L(e_sq))
summary(model_arch)

ArchTest(solid$close ,lags = 1, demean = T)

# same as arch when c(0, 1)
model_garch = solid$close %>% ts() %>% garch(c(0, 1))

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
                             data = solid$close %>% ts())

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
            y = solid$close,
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
            y = solid$close,
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
                             data = solid$close %>% ts())

# check
coef(model_fgarch_fit)
model_fgarch_fit@fit$fitted.values %>% ts() %>% autoplot() %>% ggplotly()
model_fgarch_fit@fit$sigma^2 %>% ts() %>% autoplot() %>% ggplotly()
model_fgarch_fit@fit %>% checkresiduals()