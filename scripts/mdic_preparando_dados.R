library(tidyverse)
library(here)

# Este arquivo deve ser carregado uma vez por mês, toda vez que for atualizada a base de dados do Ministério da Economia

exp_files <- fs::dir_ls(path = here("input", "mdic", "exp"), glob = "*.csv")
imp_files <- fs::dir_ls(path = here("input", "mdic", "imp"), glob = "*.csv")

# Lendo os dados

exp <- vroom::vroom(exp_files, col_select = c(CO_ANO, CO_MES, CO_NCM, CO_PAIS, VL_FOB), 
                    col_types = c(CO_ANO = "i", CO_MES = "i", CO_NCM = "c", CO_PAIS = "c", VL_FOB = "i")) %>%
  mutate(trade_flow = "exp")
imp <- vroom::vroom(imp_files, col_select = c(CO_ANO, CO_MES, CO_NCM, CO_PAIS, VL_FOB), 
                    col_types = c(CO_ANO = "i", CO_MES = "i", CO_NCM = "c", CO_PAIS = "c", VL_FOB = "i")) %>%
  mutate(trade_flow = "imp")

exp_imp <- bind_rows(exp, imp)

rm(exp, imp)

# Fazendo os joins

ncm <- read_csv2(here("input", "mdic", "dicionários", "NCM.csv")) %>% 
  select(CO_NCM, CO_SH6, CO_FAT_AGREG, CO_ISIC_CLASSE)

ncm_sh <- read_csv2(here("input", "mdic", "dicionários", "NCM_SH.csv"), locale = locale(encoding = "ISO-8859-1")) %>% 
  select(CO_SH6, CO_SH4, NO_SH4_POR)

pais <- read_csv2(here("input", "mdic", "dicionários", "PAIS.csv"), locale = locale(encoding = "ISO-8859-1")) %>%
  select(CO_PAIS, NO_PAIS)

ncm_isic <- read_csv2(here("input", "mdic", "dicionários", "NCM_ISIC.csv"), locale = locale(encoding = "ISO-8859-1")) %>%
  select(CO_ISIC_CLASSE, NO_ISIC_SECAO)

exp_imp <- exp_imp %>%
  left_join(ncm) %>% 
  left_join(ncm_sh) %>% 
  left_join(pais) %>%
  left_join(ncm_isic) %>%
  mutate(CO_FAT_AGREG = case_when(str_detect(CO_FAT_AGREG, "01") ~ "Produtos Básicos",
                                  str_detect(CO_FAT_AGREG, "02") ~ "Produtos Semimanufaturados",
                                  str_detect(CO_FAT_AGREG, "03") ~ "Produtos Manufaturados",
                                  str_detect(CO_FAT_AGREG, "04") ~ "Transações Especiais",
                                  str_detect(CO_FAT_AGREG, "05") ~ "Consumo de Bordo",
                                  str_detect(CO_FAT_AGREG, "06") ~ "Reexportação",
                                  TRUE ~ "Não identificado")) %>%
  rename(fator_agregado = CO_FAT_AGREG)

# Divisão da base de dados em dois: dados mensais e dados anuais

# Tomando por base o último mês do ano corrente

ultimo_mes <- exp_imp %>%
  filter(CO_ANO == max(CO_ANO)) %>%
  select(CO_MES) %>%  
  filter(CO_MES == max(CO_MES)) %>%
  distinct() %>%
  pull(CO_MES)

# Base de dados que toma por base os meses

exp_imp_mes <- exp_imp %>%
  filter(!is.na(VL_FOB)) %>% 
  filter(CO_MES <= ultimo_mes) %>% 
  group_by(CO_ANO, NO_PAIS, trade_flow, NO_SH4_POR, ISIC, `Fator Agregado`) %>%
  summarise(value = sum(VL_FOB)) %>%
  ungroup()
# ATERAR A PARTIR DAQUI - EU NÃO PRECISO DESSAS COLUNAS ABAIXP
  # group_by(CO_ANO, trade_flow, NO_PAIS) %>%
  # mutate(paises_mundo_mes = sum(value)) %>%
  # group_by(CO_ANO, trade_flow, NO_SH4_POR) %>%
  # mutate(produtos_mundo_mes = sum(value)) %>%
  # ungroup()

# Base de dados exclusivamente anual

exp_imp <- exp_imp %>%
  filter(!is.na(VL_FOB)) %>% 
  filter(CO_ANO < max(CO_ANO)) %>% 
  group_by(CO_ANO, NO_PAIS, trade_flow, NO_SH4_POR, ISIC, `Fator Agregado`) %>%
  summarise(value = sum(VL_FOB)) %>%
  ungroup()

# ATERAR A PARTIR DAQUI - EU NÃO PRECISO DESSAS COLUNAS ABAIXO

  # group_by(CO_ANO, trade_flow, NO_PAIS) %>%
  # mutate(paises_mundo_ano = sum(value)) %>%
  # group_by(CO_ANO, trade_flow, NO_SH4_POR) %>%
  # mutate(produtos_mundo_ano = sum(value)) %>%
  # ungroup()

# Escrevendo os dados

vroom::vroom_write(exp_imp_mes, here("input", "mdic", "exp_imp_mes.csv"))
vroom::vroom_write(exp_imp, here("input", "mdic", "exp_imp.csv"))


#testes -----
## criando csv em sh6
# exp_imp <- exp_imp %>% 
#   filter(!is.na(VL_FOB)) %>% 
#   filter(CO_ANO < max(CO_ANO)) %>% 
#   group_by(CO_ANO, CO_PAIS, trade_flow, CO_SH6) %>%
#   summarise(value = sum(VL_FOB)) %>%
#   ungroup()
# 
# vroom::vroom_write(exp_imp, here("input", "mdic", "exp_imp_sh6.csv"))


## tentando ler tudo

