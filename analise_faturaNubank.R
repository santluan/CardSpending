# Analise Gastos no Cart√£o Nubank
# Luan Santos
# 11-09-2021

#install.packages('fs')
#install.packages('treemap')
library(tidyverse)
library(ggplot2)
library(treemap)
library(ggpubr)

setwd('D:\\Desktop\\DataScience\\R\\faturaNubank')

#### modo Cava

nufatura <- 
  purrr::map(fs::dir_ls(path = getwd(), glob = '*.csv'),
             readr::read_csv,
             locale = locale(encoding = 'UTF-8')) %>%
  purrr::reduce(bind_rows) %>%
  as.data.frame() %>%
  mutate(month.name = format(date, '%B'),
         month = as.numeric(format(date, "%m")),
         year = as.numeric(format(date, '%Y')))

str(nufatura)

### Frequencia de cada estabelecimento e categoria
freq.category <- nufatura %>%
  filter(is.na(category) == F) %>%
  count(category, sort = T)

freq.title <- nufatura %>%
  count(title, sort = T)

# Convertendo juros e multa

nufatura <- nufatura %>%
  mutate(category = case_when(str_detect(title, "IOF de atraso") | str_detect(title, "Juros de atraso") |
                                str_detect(title, "Multa de atraso") | str_detect(title, "Juros e mora") |
                                str_detect(title, "Juros de rotativo") | str_detect(title, "IOF de rotativo")~ "juros",
                              TRUE ~ as.character(category)))

## Gastos Totais por MÍs ----

nufatura %>%
  filter(is.na(category) == FALSE) %>%
  group_by(category) %>%
  summarise(valor = sum(amount)) %>%
  mutate(media = mean(valor)) %>%
  ggplot(aes(x = reorder(category, -valor))) +
  geom_bar(aes(y = valor), stat = "identity", fill = '#612F74') +
  geom_text(aes(y = valor, label = valor), vjust = -0.3, size = 3.5) +
  geom_line(aes(y = media), linetype = "dashed", size = 1.5, color="#808080", group = 1) +
  labs(x = 'Categoria', y = 'Quantia', 
       title = 'Gastos Totais do Cart„o Nubank por Categoria') +
  theme_minimal()

ggsave("Gastos por Categoria_0921.png")

## Treemap ----
png("Treemap_0921.png")

nufatura %>%
  filter(is.na(category) == FALSE) %>%
  group_by(category) %>%
  summarise(valor = sum(amount)) %>%
  treemap(index = 'category',
          vSize = 'valor',
          vColor = 'category',
          title = 'Gastos Totais do Cart„o Nubank por Categoria')

dev.off()

## Meses com maiores gastos ----
nufatura %>%
  filter(is.na(category) == F) %>%
  group_by(month.name, month) %>%
  summarise(amount = sum(amount)) %>%
  ggplot(aes(x = reorder(month.name, month), y = amount)) +
  geom_bar(stat = 'identity', fill = '#612F74') +
  geom_text(aes(label = amount), vjust = -0.3, size = 3.5) +
  labs(x = 'Meses', y = 'Quantia', title = 'Gastos Totais do Cart„o Nubank por MÍs') +
  theme_bw()

ggsave("Gastos por Mes_0921.png")

## Meses com maiores gastos divididos por ano ----

nufatura %>%
  filter(is.na(category) == F) %>%
  group_by(month.name, month, year) %>%
  summarise(amount = sum(amount)) %>%
  ggplot(aes(x = reorder(month.name, month), y = amount)) +
    geom_bar(stat = 'identity', fill = '#612F74') +
    geom_text(aes(label = amount), vjust = -0.3, size = 3.5) +
    labs(x = 'Meses', y = 'Quantia', title = 'Gastos Totais do Cart„o Nubank por MÍs') +
    facet_grid(vars(year)) + 
    theme_bw()

ggsave("Gastos por Mes e Ano_0921.png")

## Gastos por Mes e Ano ----

nufatura %>%
  filter(is.na(category) == F,
         year == 2019) %>%
  group_by(month.name, month) %>%
  summarise(amount = sum(amount)) %>%
  ggplot(aes(x = reorder(month.name, month), y = amount)) +
  geom_bar(stat = 'identity', fill = '#612F74') +
  geom_text(aes(label = amount), vjust = -0.3, size = 3.5) +
  labs(x = 'Meses', y = 'Quantia', title = 'Gastos Totais do Cart„o Nubank por MÍs') +
  theme_bw()

ggsave("Gastos_2019.png")

nufatura %>%
  filter(is.na(category) == F,
         year == 2020) %>%
  group_by(month.name, month) %>%
  summarise(amount = sum(amount)) %>%
  ggplot(aes(x = reorder(month.name, month), y = amount)) +
  geom_bar(stat = 'identity', fill = '#612F74') +
  geom_text(aes(label = amount), vjust = -0.3, size = 3.5) +
  labs(x = 'Meses', y = 'Quantia', title = 'Gastos Totais do Cart„o Nubank por MÍs') +
  theme_bw()

ggsave("Gastos_2020.png")

nufatura %>%
  filter(is.na(category) == F,
         year == 2021) %>%
  group_by(month.name, month) %>%
  summarise(amount = sum(amount)) %>%
  ggplot(aes(x = reorder(month.name, month), y = amount)) +
  geom_bar(stat = 'identity', fill = '#612F74') +
  geom_text(aes(label = amount), vjust = -0.3, size = 3.5) +
  labs(x = 'Meses', y = 'Quantia', title = 'Gastos Totais do Cart„o Nubank por MÍs') +
  theme_bw()

ggsave("Gastos_2021.png")
