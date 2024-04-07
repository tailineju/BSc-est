if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, knitr, openxlsx)

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, knitr, openxlsx)

# Extração ---

## Censo Escolar 2022
censo22 <- read.csv("dados/microdados(...).csv", encoding = "latin1")
kable(head(censo22))

## PDDE 2023
pdde_pagos <- read.csv("dados/pdde_pagos.csv", skip = 5)
pdde_naopagos <- read.csv("dados/pdde_naopagos.csv", skip = 5)

# Transformação ---

## Censo Escolar 2022 na região Centro-Oeste:
censo22_co <- censo22 %>%
    filter(NO_REGIAO == "Centro-Oeste") 
write.csv(censo22_co, "dados/censo22_co.csv", row.names = FALSE)
censo22_co <- read.csv("dados/censo22_co.csv")

## PDDE 2023 com pagos e não-pagos:
pdde_pagos$fonte <- "pdde_pagos"
pdde_naopagos$fonte <- "pdde_naopagos"
pdde <- rbind(pdde_pagos, pdde_naopagos)

## Cruzamento de informações Censo e PDDE utilizando o código da escola como chave:
censo22_co$CO_ENTIDADE <- as.character(censo22_co$CO_ENTIDADE)
pdde$Código.Escola <- as.character(pdde$Código.Escola)

dados <- censo22_co %>% left_join(pdde, by = c("CO_ENTIDADE" = "Código.Escola"))

# Loading/Exportação ---
write.csv(dados, "dados/base_completa.csv")

# ------------------------------------------------------------------------------------ #
# Leitura e filtragem dos dados  ---
dados <- read.csv("dados/base_completa.csv")
dados <- dados %>% 
        select(CO_ENTIDADE, NO_UF, TP_DEPENDENCIA, TP_SITUACAO_FUNCIONAMENTO, TP_LOCALIZACAO_DIFERENCIADA, fonte) %>% 
        distinct(CO_ENTIDADE, .keep_all = TRUE)

# Censo Escolar  ---

## Número de escolas públicas segundo a dependência administrativa e a UF – Centro-Oeste

tab1 <- dados %>% 
        group_by(NO_UF, TP_DEPENDENCIA) %>% 
        summarise(n = n(), .groups = 'drop') %>% 
        spread(TP_DEPENDENCIA, n, fill = 0) %>% 
        mutate_at(vars(`1`, `2`, `3`, `4`), as.numeric) %>% 
        select(NO_UF, `1`, `2`, `3`, `4`) %>% 
        rename(`Federal` = `1`, `Estadual` = `2`, `Municipal` = `3`, `Privada` = `4`) %>% 
        mutate(Total = rowSums(select(., -NO_UF))) %>% 
        add_row(NO_UF = "Total", Federal = sum(.$Federal, na.rm = TRUE), 
                Estadual = sum(.$Estadual, na.rm = TRUE), 
                Municipal = sum(.$Municipal, na.rm = TRUE), 
                Privada = sum(.$Privada, na.rm = TRUE), 
                Total = sum(.$Total, na.rm = TRUE))

kable(tab1, align = "l")

## Número de escolas públicas segundo a situação de funcionamento e a UF – Centro-Oeste

tab2 <- dados %>% 
        group_by(NO_UF, TP_SITUACAO_FUNCIONAMENTO) %>% 
        summarise(n = n(), .groups = 'drop') %>% 
        spread(TP_SITUACAO_FUNCIONAMENTO, n, fill = 0) %>% 
        mutate_at(vars(matches("^[12345]$")), as.numeric) %>% 
        rename_at(vars(matches("^[12345]$")), 
                  ~c("Em atividade", "Paralisada", "Extinta", "Em atividade de educação especial", "Paralisada de educação especial")[match(., c("1", "2", "3", "4", "5"))]) %>% 
        mutate(Total = rowSums(select(., -NO_UF), na.rm = TRUE)) %>% 
        add_row(NO_UF = "Total", !!!colSums(select(., -NO_UF), na.rm = TRUE))

kable(tab2, align = "l")

## Número de escolas públicas segundo a localização e a UF – Centro-Oeste (identificação das escolas em localização diferenciada)

tab3 <- dados %>% 
        group_by(NO_UF, TP_LOCALIZACAO_DIFERENCIADA) %>% 
        summarise(n = n(), .groups = 'drop') %>% 
        spread(TP_LOCALIZACAO_DIFERENCIADA, n, fill = 0) %>% 
        mutate_at(vars(matches("^[01234]$")), as.numeric) %>% 
        rename_at(vars(matches("^[01234]$")), 
                  ~c("Não diferenciada", "Urbana", "Rural", "Rural de difícil acesso", "Indígena")[match(., c("0","1", "2", "3", "4"))]) %>% 
        mutate(Total = rowSums(select(., -NO_UF), na.rm = TRUE)) %>% 
        add_row(NO_UF = "Total", !!!colSums(select(., -NO_UF), na.rm = TRUE))

kable(tab3, align = "l")

# PDDE 2023 (Consulta Escola/FNDE)  ---

## Número de escolas participantes do PDDE: Escolas PAGAS em 2023

tab4 <- dados %>% 
        filter(fonte == "pdde_pagos") %>% 
        group_by(NO_UF, TP_DEPENDENCIA) %>% 
        summarise(n = n(), .groups = 'drop') %>% 
        spread(TP_DEPENDENCIA, n, fill = 0) %>% 
        mutate_at(vars(matches("^[1234]$")), as.numeric) %>% 
        rename_at(vars(matches("^[1234]$")), 
                  ~c("Federal", "Estadual", "Municipal", "Privada")[match(., c("1", "2", "3", "4"))]) %>% 
        mutate(Total = rowSums(select(., -NO_UF), na.rm = TRUE)) %>% 
        add_row(NO_UF = "Total", !!!colSums(select(., -NO_UF), na.rm = TRUE))

kable(tab4, align = "l")

## Número de escolas participantes do PDDE: Escolas NÃO PAGAS em 2023

tab5 <- dados %>% 
        filter(fonte == "pdde_naopagos") %>% 
        group_by(NO_UF, TP_DEPENDENCIA) %>% 
        summarise(n = n(), .groups = 'drop') %>% 
        spread(TP_DEPENDENCIA, n, fill = 0) %>% 
        mutate_at(vars(matches("^[1234]$")), as.numeric) %>% 
        rename_at(vars(matches("^[1234]$")), 
                  ~c("Federal", "Estadual", "Municipal", "Privada")[match(., c("1", "2", "3", "4"))]) %>% 
        mutate(Total = rowSums(select(., -NO_UF), na.rm = TRUE)) %>% 
        add_row(NO_UF = "Total", !!!colSums(select(., -NO_UF), na.rm = TRUE))

kable(tab5, align = "l")

# Cruzamentos de Informações  ---

## Escolas participantes do PDDE 2023

tab6 <- dados %>% 
        filter(fonte %in% c("pdde_pagos","pdde_naopagos")) %>% 
        group_by(NO_UF, TP_DEPENDENCIA) %>% 
        summarise(n = n(), .groups = 'drop') %>% 
        spread(TP_DEPENDENCIA, n, fill = 0) %>% 
        mutate_at(vars(matches("^[1234]$")), as.numeric) %>% 
        rename_at(vars(matches("^[1234]$")), 
                  ~c("Federal", "Estadual", "Municipal", "Privada")[match(., c("1", "2", "3", "4"))]) %>% 
        mutate(Total = rowSums(select(., -NO_UF), na.rm = TRUE)) %>% 
        add_row(NO_UF = "Total", !!!colSums(select(., -NO_UF), na.rm = TRUE))

kable(tab6, align = "l")

## Escolas não participantes do PDDE 2023

tab7 <- dados %>% 
        filter(!fonte %in% c("pdde_pagos","pdde_naopagos")) %>% 
        group_by(NO_UF, TP_DEPENDENCIA) %>% 
        summarise(n = n(), .groups = 'drop') %>% 
        spread(TP_DEPENDENCIA, n, fill = 0) %>% 
        mutate_at(vars(matches("^[1234]$")), as.numeric) %>% 
        rename_at(vars(matches("^[1234]$")), 
                  ~c("Federal", "Estadual", "Municipal", "Privada")[match(., c("1", "2", "3", "4"))]) %>% 
        mutate(Total = rowSums(select(., -NO_UF), na.rm = TRUE)) %>% 
        add_row(NO_UF = "Total", !!!colSums(select(., -NO_UF), na.rm = TRUE))

kable(tab7, align = "l")

# Exportando as tabelas para um arquivo Excel  ---

wb <- createWorkbook()
addWorksheet(wb, "tab1")
writeData(wb, "tab1", tab1)
addWorksheet(wb, "tab2")
writeData(wb, "tab2", tab2)
addWorksheet(wb, "tab3")
writeData(wb, "tab3", tab3)
addWorksheet(wb, "tab4")
writeData(wb, "tab4", tab4)
addWorksheet(wb, "tab5")
writeData(wb, "tab5", tab5)
addWorksheet(wb, "tab6")
writeData(wb, "tab6", tab6)
addWorksheet(wb, "tab7")
writeData(wb, "tab7", tab7)
saveWorkbook(wb, "relatorio2.xlsx", overwrite = TRUE)
