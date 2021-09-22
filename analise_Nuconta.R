# analise conta nubank
# Luan Santos
# 22-09-2021

library(tidyverse)
library(ggplot2)
library(treemap)
library(ggpubr)

setwd("D:\\Desktop\\DataScience\\R\\faturaNubank")

nuconta <- 
  purrr::map(fs::dir_ls(path = paste0(getwd(), "/extrato"), glob = '*.csv'),
             readr::read_csv,
             col_types = cols(Valor = col_number()),
             locale = locale(encoding = 'UTF-8')) %>%
  purrr::reduce(bind_rows) %>%
  as.data.frame() %>%
  rename(Descricao = 'Descrição') %>%
  mutate(Data = as.Date(Data, format = "%d/%m/%Y"),
         nome.mes = format(Data, '%B'),
         mes = as.numeric(format(Data, "%m")),
         ano = as.numeric(format(Data, '%Y')))

str(nuconta)

#### Frequencia de ocorrencias em descricao

freq.descricao <- nuconta %>%
  filter(is.na(Descricao) == F) %>%
  count(Descricao, sort = T)

#### Receitas por Meses

nuconta %>%
  filter(Valor > 0) %>%
  group_by(nome.mes, mes) %>%
  summarise(Total = sum(Valor)) %>%
  ggplot(aes(x = reorder(nome.mes, mes), y = Total)) +
  geom_bar(stat = 'identity', fill = '#00cc00') +
  geom_text(aes(label = Total), vjust = -0.3, size = 3.5) +
  labs(x = 'Meses', y = 'Quantia', title = 'Receitas Totais da Nuconta por Mes') +
  theme_bw()

ggsave("Receitas Totais_0921.png")

#### Despesas por meses

nuconta %>%
  filter(Valor < 0) %>%
  group_by(nome.mes, mes) %>%
  summarise(Total = sum(Valor)) %>%
  ggplot(aes(x = reorder(nome.mes, mes), y = Total)) +
  geom_bar(stat = 'identity', fill = '#ff0000') +
  geom_text(aes(label = Total), vjust = -0.3, size = 3.5) +
  labs(x = 'Meses', y = 'Quantia', title = 'Despesas Totais da Nuconta por Mes') +
  theme_bw()

ggsave("Despesas Totais_0921.png")

#### Divididos por ano

nuconta %>%
  filter(Valor > 0) %>%
  group_by(nome.mes, mes, ano) %>%
  summarise(Total = sum(Valor)) %>%
  ggplot(aes(x = reorder(nome.mes, mes), y = Total)) +
  geom_bar(stat = 'identity', fill = '#00cc00') +
  geom_text(aes(label = Total), vjust = -0.3, size = 3.5) +
  labs(x = 'Meses', y = 'Quantia', title = 'Receitas Totais da Nuconta por Mes') +
  facet_grid(vars(ano)) +
  theme_bw()

ggsave("Receitas por Mes_0921.png")

nuconta %>%
  filter(Valor < 0) %>%
  group_by(nome.mes, mes, ano) %>%
  summarise(Total = sum(Valor)) %>%
  ggplot(aes(x = reorder(nome.mes, mes), y = Total)) +
  geom_bar(stat = 'identity', fill = '#ff0000') +
  geom_text(aes(label = Total), vjust = -0.3, size = 3.5) +
  labs(x = 'Meses', y = 'Quantia', title = 'Despesas Totais da Nuconta por Mes') +
  facet_grid(vars(ano)) +
  theme_bw()

ggsave("Despesas por Mes_0921.png")

#### Salvando em csv

write_csv(nuconta, 'Base_Nuconta.csv')
