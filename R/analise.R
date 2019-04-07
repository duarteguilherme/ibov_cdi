# Loading packages
library(dplyr)
library(purrr)
library(ggplot2)

# Loading data
dados <- readRDS('data/dados.rds')


retorna_resultado <- function(dados, janela) {
  #' Funcao recebe uma janela em dias uteis,
  #' e retorna a porcentagem de vezes em que CDI venceu IBOV
  dados %>%
    mutate(cdi_diferenca = lead(cdi_index, janela)/cdi_index - 1,
           ibov_diferenca = lead(ibov_valor, janela)/ibov_valor - 1) %>%
    mutate(resultado = cdi_diferenca > ibov_diferenca) %>%
    summarise(resultado = mean(resultado, na.rm=T)) %>%
    pull(resultado)
    
}



resultados <- tibble(janelas = 1:10000)
resultados <- resultados %>%
  mutate(cdi_vitoria = map_dbl(janelas, ~ retorna_resultado(dados, .x))) 

resultados <- bind_rows(resultados %>% 
                          mutate(ativo = "CDI") %>%
                          rename(vitoria = cdi_vitoria),
                        resultados %>% 
                          mutate(ativo = "IBOVESPA", 
                                 vitoria = 1 - cdi_vitoria))


ggplot(resultados %>%
         filter(!is.na(vitoria))) +
  geom_line(aes(x = janelas, y = vitoria, color = ativo)) +
  theme_bw()