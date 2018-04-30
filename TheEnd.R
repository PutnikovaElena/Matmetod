rm(list=ls())
setwd("C:/ZAN/Matmetod")
#установак пакетов
install.packages(tidyverse)
install.packages(tidyr)
install.packages(string)
install.packages(dplyr)
install.packages(tibble)
install.packages(readr)
install.packages(ggplot2)
install.packages(ggplot2)
# включаем библиотеки
#library("dplyr")
library("tidyverse")
library("tidyr")
library("stringr")
library("tibble")
library("readr") 
library("ggplot2")
library("car")
# читаем файл, вносим изменения
data= read_csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))
# удаляем первую строку
data = data[-1,]
data
# Информация по столбцам
glimpse(data)
# Удаляем ненужные переменные 
data = select(data, -(roll))
# Преобразуем повторяющиеся значения в факторы
data = data %>% mutate_if(is.character, factor)
# исправляем синтаксис переменных
names(data) =  str_replace_all(names(data), "[!]","_emph_")
names(data) = names(data) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_") 
glimpse(data)
# Оставляем только числовые данные 
data_numeric = data[,sapply(data,is.numeric) ]
#отборка по времени года
glimpse(br) 
sapply(br,is.numeric) 
br_numeric = br[,sapply(br,is.numeric) ] 
br_non_numeric = br[,!sapply(br,is.numeric) ] 
br$daytime = as.logical(br$daytime) 
br = subset(br, as.Date(date) >= as.Date("2013-09-05") & as.Date(date) <= as.Date("2013-11-05") & daytime == T) 
br
# Анализ
cor_td = cor(data_numeric)
cor_td
# убираем значение NA
cor_td = cor(drop_na(data_numeric))
cor_td = cor(drop_na(data_numeric)) %>% as.data.frame %>% select(h2o_flux)
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .2] %>% na.exclude
vars
# собираем формулу из вектора
formula = as.formula(paste("h2o_var~", paste(vars,collapse = "+"), sep=""))
formula
# Построение модели
fit = lm(h2o_flux ~ co2_flux + Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + 
           rand_err_LE + qc_h2o_flux + rand_err_h2o_flux + 
           h2o_time_lag + sonic_temperature + air_temperature + air_density + 
           air_molar_volume + es + RH + VPD + u_star_ + TKE + T_star_ + 
           un_Tau + un_H + un_LE + un_co2_flux + un_h2o_flux + u_var + 
           v_var + w_var + h2o_var + w_div_ts_cov + w_div_co2_cov + 
           w_div_h2o_cov + flowrate, data = teaching_data_unq)
anova(fit)

#взаимодействие второго порядка
fit1= lm(h2o_flux ~ (co2_flux+ Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + 
                       rand_err_LE + qc_h2o_flux + rand_err_h2o_flux + 
                       h2o_time_lag + sonic_temperature + air_temperature + air_density + 
                       air_molar_volume + es + RH + VPD + u_star_ + TKE + T_star_ + 
                       un_Tau + un_H + un_LE + un_co2_flux + un_h2o_flux + u_var + 
                       v_var + w_var + h2o_var + w_div_ts_cov + w_div_co2_cov + 
                       w_div_h2o_cov + flowrate)^2, data = teaching_data_unq)
anova(fit1)
#удаляем переменные и взаимодействия которые не значимы
fit2= lm(h2o_flux ~ (co2_flux + Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + 
                       rand_err_LE + qc_h2o_flux + rand_err_h2o_flux + 
                       h2o_time_lag + sonic_temperature + air_temperature + air_density + 
                       air_molar_volume + es + RH + VPD + u_star_ + TKE + T_star_ + 
                       un_Tau + un_H + un_LE + un_co2_flux + un_h2o_flux + u_var + 
                       v_var + w_var + h2o_var + w_div_ts_cov + w_div_co2_cov + 
                       w_div_h2o_cov + flowrate)^2-
           ( Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + 
               rand_err_LE + h2o_flux ), data = teaching_data_unq)
anova(fit2)
plot(fit2)

