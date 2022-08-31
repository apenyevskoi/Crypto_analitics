#----data preperation--------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)
library(lubridate)

rm(list = ls())
setwd("C:/Users/INSAGNIFICANT/Downloads/R")
df_btc <- read.csv(('crypto/btc.csv'))
str(df_btc)
btc = df[c('time','DiffLast','HashRate', "RevHashRateUSD", "BlkCnt", "RevUSD")]
btc$time = as.Date(levels(btc$time))[btc$time]


#x = ~DiffLast, y = ~RevUSD
#
#
plot_ly(x = ~btc$time, y = ~btc$RevUSD, type = 'scatter', mode = 'lines')
#prepare lm data for chart
fit_usd_difflast <- btc %>% 
  filter(!is.na(RevUSD)) %>% 
  lm(RevUSD ~ DiffLast,.) %>% 
  fitted.values()
#short data for chart
fit_usd_difflast_short <- btc[btc$time <= '2021-08-15',] %>% 
  filter(!is.na(RevUSD)) %>%
  lm(RevUSD ~ DiffLast,.) %>% 
  fitted.values()
btc_short_fitted <- btc[btc$time <= '2021-08-15',] %>% 
  filter(!is.na(RevUSD))
fit_usd_difflast_short2 <- btc[btc$time >= '2021-08-15',] %>% 
  filter(!is.na(RevUSD)) %>%
  lm(RevUSD ~ DiffLast,.) %>% 
  fitted.values()
btc_short_fitted2 <- btc[btc$time >= '2021-08-15',] %>% 
  filter(!is.na(RevUSD))
fit_usd_difflast_short3 <- btc[btc$time >= '2022-01-01',] %>% 
  filter(!is.na(RevUSD)) %>%
  lm(RevUSD ~ DiffLast,.) %>% 
  fitted.values()
btc_short_fitted3 <- btc[btc$time >= '2022-01-01',] %>% 
  filter(!is.na(RevUSD))
fit_usd_difflast_short4 <- btc[btc$time >= '2022-03-01',] %>% 
  filter(!is.na(RevUSD)) %>%
  lm(RevUSD ~ DiffLast,.) %>% 
  fitted.values()
btc_short_fitted4 <- btc[btc$time >= '2022-03-01',] %>% 
  filter(!is.na(RevUSD))
#chart of dependece of Miners' Revenue from Difficulty, by time periods
#
#зеленая линия показывает зависимость роста Выручки Майнеров от Сложности с 2015 по август 2021
#красная линия показывает зависимость падения Выручки Майнеров от Сложности с августа 2021 по сегодня
#оранжевая линия показывает зависимость Выручки Майнеров от Сложности на всей генеральной совокупности
#фиолетова линия показывает, как Выручка Майнеров стала выравниваться на одном уровне при максимальной 
#   исторической сложности с начала 2022 и 
#c 1 марта 2022 слегка начала расти - коричневая линия
# 
#
line.fmt = list(dash="solid", width = 1.5, color=NULL)
btc %>% filter(!is.na(RevUSD)) %>% 
  plot_ly(x = ~DiffLast, y = ~RevUSD, mode = 'markers') %>%
  add_markers(y = ~RevUSD, marker = list(size = 1,
                                         color = 'rgba(255, 182, 193, .9)',
                                         line = list(color = 'rgba(152, 0, 0, .8)',
                                                     width = 2)),
              name = 'relationship') %>%
  add_trace(x = ~DiffLast, y = fit_usd_difflast, line=line.fmt, type = 'scatter', 
            mode = 'lines',
            name = '2015-2022') %>%
  add_trace(x = ~eth_short_fitted$DiffLast, y = fit_usd_difflast_short, line=line.fmt,type = 'scatter', 
            mode = 'lines',
            name = '2015-2021_08') %>%
  add_trace(x = ~eth_short_fitted2$DiffLast, y = fit_usd_difflast_short2, line=line.fmt,type = 'scatter', 
            mode = 'lines',
            name = '2021_08-TODAY') %>%
  add_trace(x = ~eth_short_fitted3$DiffLast, y = fit_usd_difflast_short3, line=line.fmt,type = 'scatter', 
            mode = 'lines',
            name = '2022_01-TODAY') %>%
  add_trace(x = ~eth_short_fitted4$DiffLast, y = fit_usd_difflast_short4, line=line.fmt,type = 'scatter', 
            mode = 'lines',
            name = '2022_03-TODAY') %>%
  layout(
    title = list(
      text  = "Зависимость Выручки Майнеров от Сложности в сети btc, период 2015-2022",
      font = list(
        size = 12
      ),
      x = 0.05
    ),
    xaxis = list(
      title = list(
        text = "Difficulty",
        font = list(
          size = 11
        )
      )
    ),
    yaxis = list(
      title = list(
        text = "Miner revenue, USD",
        font = list(
          size = 11
        )
      )
    ),
    showlegend = T
  )
fig




#three charts: RevHashRateUSD, Difficulty, RevUSD (miners' revenue)
#fit_hashprice_hashrate <- lm(RevHashRateUSD ~ DiffLast, data = btc) %>% fitted.values()
btc %>% filter(!is.na(RevUSD)) %>% 
  plot_ly() %>%
  add_trace(x = ~btc$time, y = ~btc$RevHashRateUSD, type = 'scatter', mode = 'lines', name="HashPrice",
            line = list(color = 'rgba(3,169,254,.7)')) %>%
  add_trace(x = ~btc$time, y = ~btc$DiffLast/12^10, type = 'scatter', mode = 'lines', name="Difficulty",
            line = list(color = 'rgba(244,67,54,.7)')) %>%
  add_trace(x = ~btc$time, y = ~btc$RevUSD/10^3, type = 'scatter', mode = 'lines', name="Miners' Revenue") %>%
  layout(
    title = list(
      text  = "Сравнение отношений между HashPrice, Сложностью и Выручкой Майнеров",
      font = list(
        size = 10
      ),
      x = 0.05
    ),
    xaxis = list(
      title = list(
        text = "Date",
        font = list(
          size = 10
        )
      )
    ),
    yaxis = list(
      title = list(
        text = "HashPrice, Difficulty, Miners' Revenue",
        font = list(
          size = 11
        )
      )
    ),
    showlegend = T
  )
fig

#График частоты дневного изменения Сложности, разбитый по процентных периодам
#Graph of appearance of daily changes of Difficulty group by percentage periods
#
btc$diff_perc <- c(btc$DiffLast[2:nrow(btc)], btc$DiffLast[nrow(btc)])
btc$diff_perc <- c(0, ((btc$diff_perc[1:nrow(btc)-1] * 100)/btc$DiffLast[1:nrow(btc)-1])-100)
head(btc)
perc_intervals <- c(length(btc[btc$diff_perc < -15,]$diff_perc),
         length(btc[btc$diff_perc >= -15 & btc$diff_perc < -10,]$diff_perc),
         length(btc[btc$diff_perc >= -10 & btc$diff_perc < -5,]$diff_perc),
         length(btc[btc$diff_perc >= -5 & btc$diff_perc < 0,]$diff_perc),
         length(btc[btc$diff_perc >= 0 & btc$diff_perc < 5,]$diff_perc),
         length(btc[btc$diff_perc >= 5 & btc$diff_perc < 10,]$diff_perc),
         length(btc[btc$diff_perc >= 10 & btc$diff_perc < 15,]$diff_perc),
         length(btc[btc$diff_perc > 15,]$diff_perc))
intervals = c('< -15%', '-15-10%', '-10-5%', '-5-0%', '0-5%', '5-10%', '10-15%', '> 15%')
intervals = factor(intervals,levels = c('< -15%', '-15-10%', '-10-5%', '-5-0%', '0-5%', '5-10%', '10-15%', '> 15%'))
df_perc_intervals <- data.frame(intervals, perc_intervals)


df_perc_intervals %>%
  plot_ly(x = ~as.factor(intervals)) %>%
  add_trace(y = ~perc_intervals*100/sum(perc_intervals), type='bar', marker = list(color = 'rgba(244,67,54,.7)')) %>%
  layout(
    title = list(
      text  = "Изменение сложности сети Bitcoin",
      font = list(
        size = 12
      ),
      x = 0.05
    ),
    xaxis = list(
      title = list(
        text = "Daily Change, %",
        font = list(
          size = 11
        )
      )
    ),
    yaxis = list(
      title = list(
        text = "Percentage of Occurances",
        font = list(
          size = 11
        )
      )
    ),
    showlegend = F
  )


plot_ly(x = ~btc$time, y = ~btc$HashRate, type = 'scatter', mode = 'lines')
