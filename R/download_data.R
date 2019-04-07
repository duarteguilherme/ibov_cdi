#' In order to replicate this script, please download
#' package 'ipeaData'
#' devtools::install_github('ipea/ipeaData')
#' tidyverse is also required, including glue
#' install.packages('tidyverse')
library(ipeaData)
library(dplyr)
library(purrr)
library(glue)
library(readxl)
library(xml2)
library(ggplot2)
library(tidyr)

download_data_ibovespa_file <- function() {
  #' Download data from Ibovespa
  #' Only data between 1968 and 1997
  download.file('http://bvmf.bmfbovespa.com.br/indices/download/IBOVDIA.zip',
                destfile = "data/IBOVDIA.zip")
  unzip("data/IBOVDIA.zip",exdir = "data")
}

read_ibovespa_year_file <- function(year) {
  #' First of, download data  with download_data_ibovespa
  #' Only data between 1968 and 1997
  readxl::read_excel('data/IBOVDIA.XLS',
                             sheet = as.character(year),
                     range = "A3:M33",
                     col_names = c("Dia", 1:12)
                                       ) %>%
    dplyr::slice(1:31) %>%
    dplyr::mutate(Ano = year) 
}




# Download IBOvespa data from B3 webpage
# 1968 to 1997
download_data_ibovespa_file()
ibov <- 1968:1997 %>%
  map(read_ibovespa_year_file) %>%
  bind_rows() %>%
  gather(mes, ibov_valor, -Dia, - Ano) %>%
  mutate(data = stringr::str_c(Dia,"/", mes, "/",Ano) %>%
           lubridate::dmy()) %>%
  filter(!is.na(data)) %>%
  select(-(Dia:mes))


# Now we bind ibov data with IPEA data
ibov_ipea <- ipeadata('GM366_IBVSP366') %>%
  mutate(data = stringr::str_c(DIA,"/", MES, "/",ANO) %>%
           lubridate::dmy()) %>%
  filter(!is.na(data)) %>%
  select(data, ibov_valor = VALVALOR) %>%
  filter(lubridate::year(data) >= 1998 )

ibov <- bind_rows(ibov, ibov_ipea)


# Plotting ibov data
ggplot(ibov, aes(x = data, y = ibov_valor)) +
  geom_line()


# Now we download CDI data
cdi <- ipeadata('GM366_TJOVER366')

cdi <- cdi %>% 
  mutate(data = stringr::str_c(DIA, "/", MES, "/", ANO) %>%
           lubridate::dmy()) %>%
  mutate(cdi_valor =  (1 + VALVALOR/100)^(1/252)-1) %>%
  select(data, cdi_valor) 



# Plotting ibov data
ggplot(cdi, aes(x = data, y = cdi_valor)) +
  geom_line()



dados <- inner_join(ibov, cdi)


#' Cleaning NA data
#' 1) We wipe rows with cdi_valor
#' or ibov_valor equal to NA
#' 2) Then, if ibov_valor is equal to NA, 
#' but cdi_valor is not, we fill ibov_valor with
#' the previous value
#' 3) If cdi_valor is equal to NA<
#' but ibov_valor is not, we assign 0
#' to cdi_valor
dados <- dados %>%
  filter(lubridate::year(data) >= 2003) %>%
  # 1)
  filter(!is.na(cdi_valor) | !is.na(ibov_valor)) %>%
  arrange(data) %>%
  # 2)
  tidyr::fill(ibov_valor) %>%
  # 3)
  mutate(cdi_valor = ifelse(is.na(cdi_valor), 0, cdi_valor))

dados <- dados %>%
  mutate(cdi_index = (1 + cdi_valor) %>%
           accumulate(`*`)) %>%
  mutate(first_ibov = first(ibov_valor)) %>%
  mutate(cdi_index = cdi_index * first(ibov_valor))
  

# Plotting cdi index x ibov index
ggplot(dados) +
  geom_line(aes(x = data, y = cdi_index)) + 
  geom_line(aes(x = data, y = ibov_valor))


saveRDS(object = dados, file = "data/dados.rds")
