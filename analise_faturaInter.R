# Analise Gastos no Cartao Inter
# Luan Santos
# 16-09-2021

library(tidyverse)
library(ggplot2)
library(stringr)

dir <- 'D:\\Desktop\\DataScience\\R\\faturaInter'
setwd(dir)

faturainter <- 
  purrr::map(fs::dir_ls(path = dir, glob = '*.csv'),
             readr::read_csv2,
             skip = 7,
             col_types = cols(Valor = col_number()),
             locale = locale(encoding = 'UTF-8')) %>%
  purrr::reduce(bind_rows) %>%
  as.data.frame() %>%
  rename(data = 'Data da Transacao',
         estabelecimento = Estabelecimento,
         tipo_transacao = 'Tipo da Transacao',
         valor = Valor) %>%
  mutate(data = as.Date(data, format = "%d/%m/%Y"),
         nome_mes = format(data, '%B'),
         mes = as.numeric(format(data, "%m")),
         ano = as.numeric(format(data, "%Y")))

str(faturainter)

# Criando coluna para categoria de acordo com o Estabelecimento ----

faturainter <- faturainter %>%
  mutate(categoria = case_when(
    str_detect(estabelecimento, "ifood") | str_detect(estabelecimento, "Bar") | 
      str_detect(estabelecimento, 'Uber Eats') | str_detect(estabelecimento, "jamesdelivery") |
      str_detect(estabelecimento, "marialuciado") | str_detect(estabelecimento, "Rappi") |
      str_detect(estabelecimento, "Amor A Cake") | str_detect(estabelecimento, "Bob S") |
      str_detect(estabelecimento, "Panificadora") | str_detect(estabelecimento, "Maria E O Boi") |
      str_detect(estabelecimento, "Burger") | str_detect(estabelecimento, "Burguer") |
      str_detect(estabelecimento, "Sorveteria") | str_detect(estabelecimento, "Grill") ~ "restaurante",
    str_detect(estabelecimento, "Waze") | str_detect(estabelecimento, "Uber") | 
      str_detect(estabelecimento, "Riocardmais") | str_detect(estabelecimento, " Pop ") ~ "transporte",
    str_detect(estabelecimento, "Gift Card") | str_detect(estabelecimento, "Amazon") |
      str_detect(estabelecimento, "Lojas Americanas") | str_detect(estabelecimento, "oboticario") ~ "outros",
    str_detect(estabelecimento, "Gympass") | str_detect(estabelecimento, "Drog") | 
      str_detect(estabelecimento, "Raia") ~ "saude",
    str_detect(estabelecimento, "Grand Marche") | str_detect(estabelecimento, "extra") | 
      str_detect(estabelecimento, 'Supermercado') | str_detect(estabelecimento, "Superprix") ~ "supermercado",
    str_detect(estabelecimento, "Ame Dig") | str_detect(estabelecimento, "donr") ~ "servicos",
    str_detect(estabelecimento, "Guilherme Fajardo Ec") ~ "eletronicos",
    str_detect(estabelecimento, "Juros") | str_detect(estabelecimento, "Encargos") |
      str_detect(estabelecimento, "Projecao") ~ "juros",
    str_detect(estabelecimento, "Multa") ~ "multa"
  ))

# frequencia de cada estabelecimento ----
freq.estabelecimento <- faturainter %>%
  count(estabelecimento, sort = T)

# frequencia de cada categoria ----
freq.categoria <- faturainter %>%
  count(categoria, sort = T)

# Gastos por categoria ----

faturainter %>%
  filter(is.na(categoria) == FALSE) %>%
  group_by(categoria) %>%
  summarise(valor = sum(valor)) %>%
  mutate(media = mean(valor)) %>%
  ggplot(aes(x = reorder(categoria, -valor))) +
  geom_bar(aes(y = valor), stat = "identity", fill = '#ff8c00') +
  geom_text(aes(y = valor, label = valor), vjust = -0.3, size = 3.5) +
  geom_line(aes(y = media), linetype = "dashed", size = 1.5, color="#808080", group = 1) +
  labs(x = 'Categoria', y = 'Quantia', 
       title = 'Gastos Totais do Cartao Inter por Categoria') +
  theme_minimal()

ggsave("Gastos por Categoria_Inter0921.png")

# Meses com maiores gastos ----

faturainter %>%
  filter(valor >= 0,
         estabelecimento != 'Antecipacao Parcela',
         estabelecimento != 'Antecipacao Juros') %>%
  group_by(nome_mes, mes) %>%
  summarise(valor = sum(valor)) %>%
  ggplot(aes(x = reorder(nome_mes, mes), y = valor)) +
  geom_bar(stat = 'identity', fill = '#ff8c00') +
  geom_text(aes(label = valor), vjust = -0.3, size = 3.5) +
  labs(x = 'Meses', y = 'Quantia', title = 'Gastos Totais do Cartao Inter por Mes') +
  theme_bw()

ggsave("Gastos por Mes_Inter0921.png")

# Meses com maiores gastos divididos pelos anos ----

faturainter %>%
  filter(valor >= 0,
         estabelecimento != 'Antecipacao Parcela',
         estabelecimento != 'Antecipacao Juros') %>%
  group_by(nome_mes, mes, ano) %>%
  summarise(valor = sum(valor)) %>%
  ggplot(aes(x = reorder(nome_mes, mes), y = valor)) +
  geom_bar(stat = 'identity', fill = '#ff8c00') +
  geom_text(aes(label = valor), vjust = -0.3, size = 3.5) +
  labs(x = 'Meses', y = 'Quantia', title = 'Gastos Totais do Cartao Inter por Mes') +
  facet_grid(vars(ano)) +
  theme_bw()

ggsave("Gastos por Ano_Inter0921.png")

# Salvando num arquivo csv ----
write.csv(faturainter, "FaturaInter.csv")

writexl::write_xlsx(faturainter, "FaturaInter.xlsx")
