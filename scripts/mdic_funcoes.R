library(tidyverse)
library(here)
library(scales)
library(ggthemes)
library(vroom)
library(glue)
library(kableExtra)
library(janitor)


exp_imp <- vroom::vroom(here("input", "mdic", "exp_imp.csv"))
exp_imp_mes <- vroom::vroom(here("input", "mdic", "exp_imp_mes.csv"))


# funções básicas -------

## Função que seleciona o País, o Bloco ou o Brasil em geral -----

fun_inicial <- function(data, var, filtro) {
  
  # no caso, o parâmetro "var" serve para definir se devemos tomar por base um país (NO_PAIS)
  # ou um grupo de países (NO_BLOCO)
  # Caso o objetivo seja tomar as relações comerciais globais do Brasil, basta selecionar o filtro "Brasil"
  
  if (filtro == "Brasil") {
    data
  }  else {
    data <- data %>%
      filter({{ var }} == filtro)
  }
  data
}

## Função que seleciona quantos anos ----

# fun_anos <- function(data, anos) {
#   
#   if (anos == "último") {
#     data <- data %>%
#       filter(CO_ANO == max(CO_ANO))
#   }
#   
#   else if (anos == "quatro") {
#     data <- data %>%
#       filter(CO_ANO > max(CO_ANO)-4)
#   }
#   
#   else if (anos == "cinco") {
#     data <- data %>%
#       filter(CO_ANO > max(CO_ANO)-5)
#   }
#   data
# }


fun_anos <- function(data, anos) {

  data %>%
    filter(CO_ANO > max(CO_ANO)-anos)

}

# Funções Corrente de Comércio -----

fun_corrente_dados <- function(data, filtro) {
  

  data %>% 
    group_by(CO_ANO, trade_flow) %>%
    summarise(value = sum(value)) %>%
    pivot_wider(names_from = trade_flow, values_from = value) %>%
    mutate(Corrente = exp + imp,
           Saldo = exp - imp) %>%
    rename(Exportações = exp,
           Importações = imp) %>%
    pivot_longer(cols = Exportações:Saldo, names_to = "trade_flow", values_to = "value")

}

fun_corrente_grafico <- function(data, filtro) {

  data %>% 
    ggplot() +
    geom_col(aes(CO_ANO, value, fill = trade_flow)) +
    facet_wrap(~ factor(trade_flow, levels = c("Exportações", "Importações", "Corrente", "Saldo")),
               scales = "free_x") +
    scale_y_continuous(labels = label_number_si(accuracy = 0.01), breaks = pretty_breaks()) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    scale_fill_tableau() +
    theme(legend.position = "none", axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    labs(title = glue("Brasil-{filtro}, Corrente de Comércio"),
         caption = "Fonte: Ministério da Economia")
  
} 

fun_corrente_tabela <- function(data) {
  
  tabela_corrente = data %>%
    
    pivot_wider(names_from = "trade_flow", 
                values_from = value,
                values_fill = list(value = 0)) %>%
    ungroup() %>% 
    arrange(desc(CO_ANO)) %>% 
    mutate(variação_exp = (Exportações/lead(Exportações) - 1)*100, 
           variação_imp = (Importações/lead(Importações)- 1)*100,
           variação_cor = (Corrente/lead(Corrente)- 1)*100,
           variação_sal = case_when(lead(Saldo) > 0 ~ (Saldo/lead(Saldo) - 1)*100,
                                    lead(Saldo) < 0 ~ ((Saldo/lead(Saldo) - 1)*100)*-1)) %>%
    mutate(CO_ANO = as.character(CO_ANO)) %>% 
    mutate_if(is.numeric, label_number_si(accuracy = 0.01)) %>%
    mutate(Exportações = paste0(Exportações, " (", variação_exp, "%)"),
           Importações = paste0(Importações, " (", variação_imp, "%)"),
           Corrente = paste0(Corrente, " (", variação_cor, "%)"),
           Saldo = paste0(Saldo, " (", variação_sal, "%)")) %>% 
    arrange(desc(CO_ANO)) %>% 
    select(CO_ANO, Exportações, Importações, Saldo, Corrente) %>% 
    mutate(Exportações = cell_spec(Exportações, "latex",
                                   color = ifelse(str_detect(Exportações, "\\(-"), "#000000", "#000000"))) %>%
    mutate(Importações = cell_spec(Importações, "latex",
                                   color = ifelse(str_detect(Importações, "\\(-"), "#000000", "#000000"))) %>%
    mutate(Corrente = cell_spec(Corrente, "latex",
                                color = ifelse(str_detect(Corrente, "\\(-"), "#000000", "#000000"))) %>%
    mutate(Saldo = cell_spec(Saldo, "latex",
                             color = ifelse(str_detect(Saldo, "\\(-"), "#000000", "#000000")))
  
  tabela_corrente = as.data.frame(t(tabela_corrente)) %>%
    row_to_names(row_number = 1)
  
  tabela = split(1:(ncol(tabela_corrente)-1), sort(rep_len(1:2, ncol(tabela_corrente)-1))) %>%
    map(~select(tabela_corrente, .)) %>%
    map(kable, format = "latex", booktabs = TRUE, align = "r", escape = FALSE) %>%
    map(kable_styling, tabela_corrente, position = "center", full_width = FALSE) %>%
    walk(print)
  
  
}

# Funções dos principais produtos exportados e importados ----

fun_produtos_dados <- function(data, fluxo) {
  
  data %>%
    filter(trade_flow == fluxo) %>% 
    group_by(CO_ANO, NO_SH4_POR) %>%
    summarise(value = sum(value)) %>%
    ungroup()
  
  }

fun_produtos_grafico <- function(data) {
  
  data %>%
    group_by(CO_ANO) %>% 
    slice_max(value, n = 10) %>%
    ungroup() %>% 
    mutate(NO_SH4_POR = str_sub(NO_SH4_POR, 1, 30)) %>% 
    mutate(CO_ANO = as.character(CO_ANO)) %>% 
    ggplot() +
    geom_col(aes(reorder_within(NO_SH4_POR, value, CO_ANO), value, fill = CO_ANO)) +
    coord_flip() +
    facet_wrap(~ factor(CO_ANO), scales = "free_y") +
    scale_x_reordered() +
    scale_y_continuous(labels = label_number_si(accuracy = 0.01),
                       expand = c(0,0),
                       breaks = breaks_pretty(),
                       guide = guide_axis(n.dodge = 2)) +
    scale_fill_tableau() +
    theme(legend.position = "none", axis.title.x = element_blank(),
          axis.title.y = element_blank())
}

fun_produtos_tabela <- function(data, ano) {
  
  data %>% 
    group_by(CO_ANO) %>%
    mutate(total_ano = sum(value)) %>% 
    group_by(NO_SH4_POR) %>%
    arrange(desc(CO_ANO), .by_group = TRUE) %>% 
    mutate(Variação = round((value/lead(value) - 1)*100), 3) %>%
    mutate(Porcentagem = round((value/total_ano)*100),3) %>% 
    ungroup() %>% 
    fun_anos(4) %>%
    group_by(CO_ANO) %>% 
    slice_max(value, n = 10) %>%
    mutate(Posição = row_number()) %>%
    mutate(Variação = paste0(Variação, "\\%", sep = " ")) %>%
    mutate(Porcentagem = paste0(Porcentagem, "\\%", sep = " ")) %>%
    ungroup() %>%
    mutate(NO_SH4_POR = str_sub(NO_SH4_POR, 1, 71)) %>%
    mutate(CO_ANO = as.character(CO_ANO),
           Posição = as.character(Posição)) %>% 
    mutate_if(is.numeric, label_number_si(accuracy = 0.01)) %>% 
    select(Posição, NO_SH4_POR, value, CO_ANO, Variação, Porcentagem) %>%
    filter(CO_ANO == ano) %>%
    pivot_wider(names_from = CO_ANO, values_from = value) %>%
    # tomar cuidado, o Latex não aceita underlines (_) nas colunas
    rename(Produto = NO_SH4_POR) %>% 
    relocate(Variação, .after = last_col()) %>%
    relocate(Porcentagem, .after = last_col()) %>% 
    kable(format = "latex", booktabs = TRUE, align = "r", escape = FALSE) %>%
    kable_styling(latex_options = "striped", full_width = T, font_size = 9) %>%
    column_spec(1, width = "0.5cm") %>%
    column_spec(3, width = "1.5cm") %>% 
    column_spec(4, width = "1cm") %>%
    column_spec(5, width = "1cm")
  
  
}

# Funções dos principais países parceiros comerciais ----

fun_paises_dados <- function(data, fluxo, filtro) {
  
  data %>%
    filter(trade_flow == fluxo) %>%
    # retiramos o Brasil para não contar reimportações
    filter(NO_PAIS != "Brasil") %>% 
    group_by(CO_ANO, NO_PAIS) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    group_by(CO_ANO) %>%
    arrange(desc(value)) %>%
    mutate(rank = row_number()) %>%
    filter(rank < 11 | NO_PAIS == filtro) %>%
    ungroup() %>%
    mutate(fill = case_when(NO_PAIS == filtro ~ "fill",
                            TRUE ~ "NA"))

}

fun_paises_grafico <- function(data) {
  
  data %>%
    mutate(CO_ANO = as.character(CO_ANO)) %>%
    ggplot(aes(reorder_within(NO_PAIS, value, CO_ANO), value)) +
    geom_col(aes(fill = CO_ANO)) +
    geom_label(data=data %>% filter(fill == "fill"), aes(label = rank, y = max(data$value)-max(data$value)/8),
               size = 2.5) +
    coord_flip() +
    facet_wrap(~ factor(CO_ANO), scales = "free_y") +
    scale_x_reordered() +
    scale_y_continuous(labels = label_number_si(accuracy = 0.01),
                       expand = c(0,0),
                       breaks = breaks_pretty(),
                       guide = guide_axis(n.dodge = 2)) +
    scale_fill_tableau() +
    theme(legend.position = "none", axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
}

fun_paises_tabela <- function(data, ano) {
  
  data %>% 
    group_by(CO_ANO) %>%
    mutate(total_ano = sum(value)) %>% 
    group_by(NO_PAIS) %>%
    arrange(desc(CO_ANO), .by_group = TRUE) %>% 
    mutate(Variação = round((value/lead(value) - 1)*100), 3) %>%
    mutate(Porcentagem = round((value/total_ano)*100),3) %>% 
    ungroup() %>% 
    fun_anos(4) %>%
    group_by(CO_ANO) %>%
    arrange(desc(value)) %>% 
    rename(Posição = rank) %>%
    mutate(Variação = paste0(Variação, "\\%", sep = " ")) %>%
    mutate(Porcentagem = paste0(Porcentagem, "\\%", sep = " ")) %>%
    ungroup() %>% 
    mutate(NO_PAIS = str_sub(NO_PAIS, 1, 71)) %>%
    mutate(CO_ANO = as.character(CO_ANO),
           Posição = as.character(Posição)) %>% 
    mutate_if(is.numeric, label_number_si(accuracy = 0.01)) %>% 
    select(Posição, NO_PAIS, value, CO_ANO, Variação, Porcentagem) %>%
    filter(CO_ANO == ano) %>% 
    pivot_wider(names_from = CO_ANO, values_from = value) %>%
    # tomar cuidado, o Latex não aceita underlines (_) nas colunas
    rename(País = NO_PAIS) %>% 
    relocate(Variação, .after = last_col()) %>%
    relocate(Porcentagem, .after = last_col()) %>% 
    kable(format = "latex", booktabs = TRUE, align = "r", escape = FALSE) %>%
    kable_styling(latex_options = "striped", full_width = T, font_size = 9) %>%
    column_spec(1, width = "0.5cm") %>%
    column_spec(3, width = "2cm") %>% 
    column_spec(4, width = "1.5cm") %>%
    column_spec(5, width = "1.5cm")
  
}

# Funções de waffle ----

fun_waffle_produtos <- function(data, fluxo) {
  
  if(fluxo == "exp") {
    fluxo_frase <- "Exportações"
  } else if (fluxo == "imp") {
    fluxo_frase <- "Importações"
  }
  
  ano <- max(data$CO_ANO)
  
  data %>%
    mutate(Produto = str_sub (NO_SH4_POR, 1, 25)) %>% 
    select(-NO_SH4_POR) %>% 
    mutate(porcentagem=paste0(value/sum(value)*100)) %>%
    mutate(porcentagem = as.numeric(porcentagem)) %>% 
    slice_max(value, n = 10) %>% 
    select(Produto, porcentagem) %>%
    add_row(Produto = "Outros", porcentagem = (100 - sum(.$porcentagem))) %>%
    mutate(porcentagem = round(porcentagem, 2)) %>%
    mutate(Produto = glue("{Produto} ({porcentagem}) %")) %>% 
    mutate(Produto = factor(Produto, levels = Produto))  %>%
    ggplot() +
    geom_waffle(aes(fill = Produto, values = porcentagem), color = "white", size = 1.5, n_rows = 5, make_proportional = TRUE, flip = FALSE) +
    scale_fill_tableau(palette = "Tableau 20", name=NULL) +
    coord_equal() +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    labs(title = glue("Brasil-{filtro}, {fluxo_frase} em {ano}")) +
    theme(legend.position = "left")
}

fun_waffle_paises <- function(data, fluxo) {
  
  if (fluxo == "exp") {
    fluxo_frase <- "Destinos de Exportações"
  } else if (fluxo == "imp") {
    fluxo_frase <- "Origens de Importações"
  }
  
  ano <- max(data$CO_ANO)
  
  data %>% 
    mutate(porcentagem=paste0(value/sum(value)*100)) %>%
    mutate(porcentagem = as.numeric(porcentagem)) %>%
    arrange(desc(porcentagem)) %>%
    mutate(rank_produto_mdic = row_number()) %>%
    ungroup() %>% 
    filter(rank_produto_mdic < 11) %>%
    select(NO_PAIS, porcentagem) %>%
    add_row(NO_PAIS = "Outros", porcentagem = (100 - sum(.$porcentagem))) %>% 
    mutate(porcentagem = round(porcentagem, 2)) %>%
    mutate(NO_PAIS = paste(NO_PAIS, " (", porcentagem, ") %", sep = "")) %>%
    mutate(NO_PAIS = factor(NO_PAIS, levels = NO_PAIS))  %>%
    ggplot() +
    geom_waffle(aes(fill = NO_PAIS, values = porcentagem), color = "white", size = 1.5, n_rows = 5, make_proportional = TRUE, flip = FALSE) +
    scale_fill_tableau(palette = "Tableau 20", name=NULL) +
    coord_equal() +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    labs(title = glue("{fluxo_frase} do Brasil em {ano}")) +
    theme(legend.position = "left")
  
}

fun_isic_fat_dados <- function(data, fator) {
  
  data %>% 
    group_by({{ fator }}, trade_flow) %>%
    summarise(value = sum(value)) %>%
    group_by(trade_flow) %>% 
    mutate(porcentagem = round((value/sum(value))*100, 2)) %>%
    mutate(trade_flow = case_when(trade_flow == "exp" ~ "Exportações",
                                  trade_flow == "imp" ~ "Importações")) %>%
    ungroup()
  
}

fun_isic_fat_grafico <- function(data, fator) {

  fator_nome <- data %>%
    select({{ fator }}) %>%
    names()
  
  data %>% 
    ggplot() +
    geom_waffle(aes(fill = {{ fator }}, values = porcentagem),
                color = "white", size = 0.25, n_rows = 10,
                reverse = TRUE, make_proportional = TRUE) +
    facet_wrap(~ trade_flow) +
    scale_x_discrete() +
    scale_y_discrete() +
    scale_fill_tableau(name = NULL, drop = FALSE) +
    coord_equal() +
    labs(title = glue("Composição do Comércio por {fator_nome}")) +
    theme(legend.position = "bottom", axis.title = element_blank())
  
}

fun_isic_fat_tabela <- function(data, fator) {
  
  data %>% 
    # filter(trade_flow == sentido) %>%
    select(-value) %>% 
    group_by({{ fator }}, trade_flow) %>% 
    mutate(porcentagem = glue("{porcentagem} \\%")) %>%
    pivot_wider(names_from = {{ fator }}, values_from = porcentagem) %>%
    ungroup() %>% 
    rename(Sentido = trade_flow) %>%
    kable(format = "latex", booktabs = TRUE, align = "r", escape = FALSE) %>%
    kable_styling(position = "center", full_width = FALSE)
  
  
}
  
  

# TESTES -----
## Tentando resolver o erro da porcentagem quando há oscilação entre negativo e positivo

fun_inicial(exp_imp, NO_PAIS, filtro = "Canadá") %>%
  fun_corrente_dados(filtro = "Canadá") %>% 
  pivot_wider(names_from = "trade_flow", 
              values_from = value,
              values_fill = list(value = 0)) %>%
  ungroup() %>% 
  arrange(desc(CO_ANO)) %>% 
  mutate(variação_exp = (Exportações/lead(Exportações) - 1)*100, 
         variação_imp = (Importações/lead(Importações)- 1)*100,
         variação_cor = (Corrente/lead(Corrente)- 1)*100,
         variação_sal = case_when(lead(Saldo) > 0 ~ (Saldo/lead(Saldo) - 1)*100,
                                  lead(Saldo) < 0 ~ ((Saldo/lead(Saldo) - 1)*100)*-1),
         old_saldo = (Saldo/lead(Saldo)-1)*100) %>%
  mutate(CO_ANO = as.character(CO_ANO)) %>% 
  mutate_if(is.numeric, label_number_si(accuracy = 0.01)) %>%
  mutate(Exportações = paste0(Exportações, " (", variação_exp, "%)"),
         Importações = paste0(Importações, " (", variação_imp, "%)"),
         Corrente = paste0(Corrente, " (", variação_cor, "%)"),
         Saldo = paste0(Saldo, " (", variação_sal, "%)")) 



# fun_variavel(exp_imp_mes, NO_PAIS, filtro = "Brasil") %>%
#   group_by(trade_flow, CO_ANO, NO_PAIS) %>%
#   summarise(value = sum(value)) %>%
#   group_by(trade_flow, CO_ANO) %>%
#   mutate(total_ano = sum(value)) %>%
#   group_by(CO_ANO, trade_flow) %>%
#   mutate(n = n()) %>% 
#   ungroup() %>%
#   filter(trade_flow == "exp") %>% 
#   mutate(porcentagem = value/total_ano) %>%
#   mutate(HH = porcentagem^2) %>%
#   group_by(CO_ANO) %>% 
#   summarise(HH = sum(HH)) %>% 
#   ggplot() +
#   geom_col(aes(CO_ANO, HH, fill = HH), show.legend = F) +
#   scale_x_continuous(breaks = pretty_breaks(), name = NULL) +
#   scale_y_continuous(name = "Índice Herfindahl-Hirschman") +
#   labs(title = "Índice Herfindahl-Hirschman (HH)",
#        subtitle = "Destino das Exportações",
#        caption = "Fonte: Ministério da Economia")
#   
# 
# fun_variavel(exp_imp_mes, NO_PAIS, filtro = "Brasil") %>%
#   group_by(trade_flow, CO_ANO, NO_PAIS) %>%
#   summarise(value = sum(value)) %>%
#   group_by(trade_flow, CO_ANO) %>%
#   mutate(total_ano = sum(value)) %>%
#   group_by(CO_ANO, trade_flow) %>%
#   mutate(n = n()) %>% 
#   ungroup() %>%
#   filter(trade_flow == "imp") %>% 
#   mutate(porcentagem = value/total_ano) %>%
#   mutate(HH = porcentagem^2) %>%
#   group_by(CO_ANO) %>% 
#   summarise(HH = sum(HH)) %>% 
#   ggplot() +
#   geom_col(aes(CO_ANO, HH, fill = HH), show.legend = F) +
#   scale_x_continuous(breaks = pretty_breaks(), name = NULL) +
#   scale_y_continuous(name = "Índice Herfindahl-Hirschman") +
#   labs(title = "Índice Herfindahl-Hirschman (HH)",
#        subtitle = "Origem das Importações",
#        caption = "Fonte: Ministério da Economia")
# 
# 
# fun_variavel(exp_imp_mes, NO_PAIS, filtro = "Brasil") %>%
#   group_by(trade_flow, CO_ANO, NO_PAIS) %>% 
#   summarise(value = sum(value)) %>%
#   pivot_wider(names_from = trade_flow, values_from = value, values_fill = 0) %>%
#   mutate(corrente = exp + imp) %>% 
#   select(CO_ANO, NO_PAIS, corrente) %>% 
#   group_by(CO_ANO) %>%
#   mutate(total_ano = sum(corrente)) %>%
#   mutate(n = n()) %>% 
#   ungroup() %>% 
#   mutate(porcentagem = corrente/total_ano) %>%
#   mutate(HH = porcentagem^2) %>%
#   group_by(CO_ANO) %>% 
#   summarise(HH = sum(HH)) %>% 
#   ggplot() +
#   geom_col(aes(CO_ANO, HH, fill = HH), show.legend = F) +
#   scale_x_continuous(breaks = pretty_breaks(), name = NULL) +
#   scale_y_continuous(name = "Índice Herfindahl-Hirschman") +
#   labs(title = "Índice Herfindahl-Hirschman (HH)",
#        subtitle = "Corrente de Comércio",
#        caption = "Fonte: Ministério da Economia")


