# ---
# title: 'Primeira visita ao SEDAP'
# author: 'Tailine J. S. Nonato'
# date: '2024-05-22'
# ---

# -----------------------------
# Carregamento de pacotes

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(lubridate)

# -----------------------------
# PDDE 

# Lendo os arquivos
files <- list.files(path = "T:/2. Bases_Externas/pdde", pattern = ".csv", full.names = TRUE)

# Lendo os arquivos
pdde <- map_dfr(files, ~read_csv2(.x, skip=5) %>%
                select(Ano, `Código Escola`, everything()), .id = "id_ano")

pdde2 <- pdde %>%
        group_by(`Código Escola`, Ano) %>%
        summarise(`Valor Total` = sum(`Valor Total`),
                  `Valor Custeio` = sum(`Valor Custeio`),
                  `Valor Capital` = sum(`Valor Capital`))

pdde3 <- pdde %>%
  group_by(`Código Escola`) %>%
  summarise(`Valor Total` = sum(`Valor Total`),
            `Valor Custeio` = sum(`Valor Custeio`),
            `Valor Capital` = sum(`Valor Capital`))

# -----------------------------
# SAEB
saeb_2017 <- read.csv("1. Bases_Sedap/SAEB_ALUNO_9EF_2017.csv",sep=';')
saeb_2021 <- read.csv("1. Bases_Sedap/SAEB_ALUNO_9EF_2021.csv",sep=';')


# Filtro: alunos não presentes
saeb_2017 <- saeb_2017 %>% filter(IN_PRESENCA_LP==1)
saeb_2021 <- saeb_2021 %>% filter(IN_PRESENCA_LP==1)

# Criar variável de região
saeb_2017$ID_REGIAO <- substr(saeb_2017$ID_MUNICIPIO,start=1,stop=1)
saeb_2021$ID_REGIAO <- substr(saeb_2021$ID_MUNICIPIO,start=1,stop=1)

# Filtrar apenas escolas do Centro-Oeste
saeb_2017_co <- saeb_2017 %>% filter(ID_REGIAO == 5)  
saeb_2021_co <- saeb_2021 %>% filter(ID_REGIAO == 5) 

# Encontrar escolas que estão presentes em todos os datasets
escolas_2017 <- as.character(unique(saeb_2017_co$ID_ESCOLA))
escolas_2021 <- as.character(unique(saeb_2021_co$ID_ESCOLA))

escolas_comuns <-  intersect(escolas_2021,escolas_2017)
escolas_pdde <- pdde3$`Código Escola`

interesse <- intersect(escolas_pdde, escolas_comuns)

# PROEF MÉDIA POR ESCOLA

medias_2017 <- saeb_2017_co %>% 
  filter(ID_ESCOLA %in% interesse) %>% 
  group_by(ID_ESCOLA) %>%
  summarise(MEDIA_LP = mean(PROFICIENCIA_LP_SAEB),
            MEDIA_MT = mean(PROFICIENCIA_MT_SAEB))

# -----------------------------
# INTEGRAÇÃO

# Variavel chave
pdde3$`Código Escola` <- as.character(pdde3$`Código Escola`)
medias_2017$ID_ESCOLA <- as.character(medias_2017$ID_ESCOLA)

# Renomeando a variável chave
colnames(pdde3)[colnames(pdde3) == "Código Escola"] <- "ID_ESCOLA"

dados_2017_pdde <- inner_join(medias_2017,pdde3, by="ID_ESCOLA")

dados_2021 <- saeb_2021_co %>% filter(ID_ESCOLA %in% interesse)

# --------------------------
# DESIDENTIFICACAÇÃO 
cod_escolas <- unique(c(dados_2017_pdde$ID_ESCOLA,dados_2021$ID_ESCOLA))
des_cod_escolas <- seq_along(cod_escolas)
mapeamento <- setNames(des_cod_escolas,cod_escolas)

dados_2017_pdde$ID_ESCOLA <- mapeamento[as.character(dados_2017_pdde$ID_ESCOLA)]
dados_2021$ID_ESCOLA <- mapeamento[as.character(dados_2021$ID_ESCOLA)]

# -------------------
# Salvando os datasets
write_csv(dados_2017_pdde, "T:/3. Resultados/1ª Extração de Resultados/dados_2017_pdde.csv")
write_csv(dados_2021, "T:/3. Resultados/1ª Extração de Resultados/dados_2021.csv")
