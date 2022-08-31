#----data preperation--------------------------------------------------------------------------
library(tidyverse)
library(writexl)
library(readxl)
library(lobstr)
library(dplyr)
library(tidyr)
library(stringi)
library(plotly)
library(ggplot2)

rm(list = ls())
setwd("C:/Users/INSAGNIFICANT/Downloads/R")
df <- read.csv('crypto/eth.csv')
df_xlm <- read.csv(('crypto/xlm.csv'))
df_btc <- read.csv(('crypto/btc.csv'))
str(df)
str(eth)
tail(eth)
str(xlm)
colnames(df)
eth = df[c('time','DiffLast','HashRate', "RevHashRateUSD", "BlkCnt", "RevUSD")]
eth$time = as.Date(levels(eth$time))[eth$time]
xlm = df[c('time','DiffLast','HashRate', "RevHashRateUSD","BlkCnt", "RevUSD")]
xlm$time = as.Date(levels(xlm$time))[xlm$time]
btc = df[c('time','DiffLast','HashRate', "RevHashRateUSD", "BlkCnt", "RevUSD")]
btc$time = as.Date(levels(btc$time))[btc$time]


#x = ~DiffLast, y = ~RevUSD
#
#
plot_ly(x = ~eth$time, y = ~eth$RevUSD, type = 'scatter', mode = 'lines')
#prepare lm data for chart
  fit_usd_difflast <- eth %>% 
                      filter(!is.na(RevUSD)) %>% 
                      lm(RevUSD ~ DiffLast,.) %>% 
                      fitted.values()
#short data for chart
  fit_usd_difflast_short <- eth[eth$time <= '2021-08-15',] %>% 
                            filter(!is.na(RevUSD)) %>%
                            lm(RevUSD ~ DiffLast,.) %>% 
                            fitted.values()
  eth_short_fitted <- eth[eth$time <= '2021-08-15',] %>% 
         filter(!is.na(RevUSD))
  fit_usd_difflast_short2 <- eth[eth$time >= '2021-08-15',] %>% 
    filter(!is.na(RevUSD)) %>%
    lm(RevUSD ~ DiffLast,.) %>% 
    fitted.values()
  eth_short_fitted2 <- eth[eth$time >= '2021-08-15',] %>% 
    filter(!is.na(RevUSD))
  fit_usd_difflast_short3 <- eth[eth$time >= '2022-01-01',] %>% 
    filter(!is.na(RevUSD)) %>%
    lm(RevUSD ~ DiffLast,.) %>% 
    fitted.values()
  eth_short_fitted3 <- eth[eth$time >= '2022-01-01',] %>% 
    filter(!is.na(RevUSD))
  fit_usd_difflast_short4 <- eth[eth$time >= '2022-03-01',] %>% 
    filter(!is.na(RevUSD)) %>%
    lm(RevUSD ~ DiffLast,.) %>% 
    fitted.values()
  eth_short_fitted4 <- eth[eth$time >= '2022-03-01',] %>% 
    filter(!is.na(RevUSD))
#chart
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
    eth %>% filter(!is.na(RevUSD)) %>% 
      plot_ly(x = ~DiffLast, y = ~RevUSD, mode = 'markers') %>%
      add_markers(y = ~RevUSD, marker = list(size = 1,
                                             color = 'rgba(255, 182, 193, .9)',
                                             line = list(color = 'rgba(152, 0, 0, .8)',
                                                         width = 2))) %>%
      add_trace(x = ~DiffLast, y = fit_usd_difflast, line=line.fmt, type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~eth_short_fitted$DiffLast, y = fit_usd_difflast_short, line=line.fmt,type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~eth_short_fitted2$DiffLast, y = fit_usd_difflast_short2, line=line.fmt,type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~eth_short_fitted3$DiffLast, y = fit_usd_difflast_short3, line=line.fmt,type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~eth_short_fitted4$DiffLast, y = fit_usd_difflast_short4, line=line.fmt,type = 'scatter', mode = 'lines') %>%
      layout(
        title = list(
          text  = "Зависимость Выручки Майнеров от Сложности в сети Ethereum, период 2015-2022",
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
        showlegend = F
      )
fig
    
    
    
    
#three charts: RevHashRateUSD, Difficulty, RevUSD (miners' revenue)
#fit_hashprice_hashrate <- lm(RevHashRateUSD ~ DiffLast, data = eth) %>% fitted.values()
eth %>% filter(!is.na(RevUSD)) %>% 
  plot_ly() %>%
  add_trace(x = ~eth$time, y = ~eth$RevHashRateUSD, type = 'scatter', mode = 'lines', name="HashPrice",
            line = list(color = 'rgba(3,169,254,.7)')) %>%
  add_trace(x = ~eth$time, y = ~eth$DiffLast/12^10, type = 'scatter', mode = 'lines', name="Difficulty",
            line = list(color = 'rgba(244,67,54,.7)')) %>%
  add_trace(x = ~eth$time, y = ~eth$RevUSD/10^3, type = 'scatter', mode = 'lines', name="Miners' Revenue") %>%
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






#DiffLast ~ HashRate
fig <- plot_ly(x = ~eth$RevUSD, y = ~eth$HashRate)
fig
#RevUSD ~ HashRate
fig <- plot_ly(x = ~eth$RevUSD, y = ~eth$HashRate)
fit_usd_hashrate <- lm(RevUSD ~ HashRate, data = eth) %>% fitted.values()
eth %>% filter(!is.na(RevUSD)) %>% 
  plot_ly(x = ~HashRate, y = ~RevUse, mode = 'markers') %>%
  add_markers(y = ~RevUSD) %>%
  add_lines(x = ~HashRate, y = fit_usd_hashrate, mode = 'lines') %>%
  layout(
    title = list(
      text  = "Зависимость Выручки Майнеров от HashRate в сети Ethereum, период 2015-2022",
      font = list(
        size = 10
      ),
      x = 0.05
    ),
    xaxis = list(
      title = list(
        text = "HashRate",
        font = list(
          size = 10
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
    showlegend = F
  )

#График частоты дневного изменения Сложности, разбитый по процентных периодам
#
#
eth$diff_perc <- c(eth$DiffLast[2:nrow(eth)], eth$DiffLast[nrow(eth)])
eth$diff_perc <- c(0, ((eth$diff_perc[1:nrow(eth)-1] * 100)/eth$DiffLast[1:nrow(eth)-1])-100)

perc_intervals <- c(length(eth[eth$diff_perc < -15,]$diff_perc),
                    length(eth[eth$diff_perc >= -15 & eth$diff_perc < -10,]$diff_perc),
                    length(eth[eth$diff_perc >= -10 & eth$diff_perc < -5,]$diff_perc),
                    length(eth[eth$diff_perc >= -5 & eth$diff_perc < 0,]$diff_perc),
                    length(eth[eth$diff_perc >= 0 & eth$diff_perc < 5,]$diff_perc),
                    length(eth[eth$diff_perc >= 5 & eth$diff_perc < 10,]$diff_perc),
                    length(eth[eth$diff_perc >= 10 & eth$diff_perc < 15,]$diff_perc),
                    length(eth[eth$diff_perc > 15,]$diff_perc))
intervals = factor(intervals,levels = c('< -15%', '-15-10%', '-10-5%', '-5-0%', '0-5%', '5-10%', '10-15%', '> 15%'))
df_perc_intervals <- data.frame(intervals, perc_intervals)


df_perc_intervals %>%
  plot_ly(x = ~as.factor(intervals)) %>%
  add_trace(y = ~perc_intervals*100/sum(perc_intervals), 
            type='bar', 
            text = round(perc_intervals*100/sum(perc_intervals),2),
            textposition = 'auto',
            marker = list(color = 'rgba(244,67,54,.7)'),
                          line = list(color = 'black',
                                      width = 1.5),
            width = 0.5) %>%
  layout(
    title = list(
      text  = "Изменение сложности сети Etherium",
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


plot_ly(x = ~eth$time, y = ~eth$HashRate, type = 'scatter', mode = 'lines')









fig <- plot_ly(x = ~eth$time, y = ~eth$HashRate, type = 'scatter', mode = 'lines')
fig <- plot_ly(x = ~eth$time, y = ~eth$RevUSD, type = 'scatter', mode = 'lines')
fig
fig <- plot_ly(x = ~eth$time, y = ~eth$RevHashRateUSD, type = 'scatter', mode = 'lines', name = "HashPrice")
fig <- plot_ly(x = ~xlm$time, y = ~xlm$RevHashRateUSD, type = 'scatter', mode = 'lines')
fig
fig <- plot_ly(x = ~eth$time, y = ~eth$BlkCnt)
fig