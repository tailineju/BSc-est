# ---
# title: 'Primeira visita ao SEDAP'
# author: 'Tailine J. S. Nonato'
# date: '2024-05-14'
# ---

# -----------------------------
# Carregamento de pacotes
if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)

# -----------------------------
# PDDE 

# Lendo os arquivos
files <- list.files(path = "path/pdde", pattern = ".csv", full.names = TRUE)

# Lendo os arquivos
data1 <- map_dfr(files, ~read_csv(.x, skip=5) %>%
                select(Ano, `Código Escola`, everything()), .id = "id_ano")

# Removendo "." em variáveis de valor numérico
data2 <- data1 %>%
  mutate(`Valor Custeio` = as.numeric(str_replace_all(`Valor Custeio`, "\\.", "")),
         `Valor Capital` = as.numeric(str_replace_all(`Valor Capital`, "\\.", "")),
         `Valor Total` = `Valor Custeio` + `Valor Capital`)

# Salvando o dataset
write_csv(data2, "pdde.csv")

# -----------------------------
# SAEB
saeb_2017 <- read.csv("microdados_saeb_2017/DADOS/TS_ESCOLA.csv", encoding="latin1")
saeb_2021 <- read.csv("microdados_saeb_2021/DADOS/TS_ESCOLA.csv", sep = ";",encoding="latin1")


# Filtrar apenas escolas do Centro-Oeste
saeb_2017_co <- saeb_2017 %>% filter(ID_UF %in% c(50, 51, 52, 53))  
saeb_2021_co <- saeb_2021 %>% filter(ID_REGIAO == 5) 

saeb_2017_co$ID_REGIAO <- 5

# ID_PROVA_BRASIL em 2017 = ID_SAEB em 2021
saeb_2017_co <- saeb_2017_co %>% rename(ID_SAEB = ID_PROVA_BRASIL)

# MEDIA_3EM_LP e MEDIA_3EM_MT em 2017 = MEDIA_EM_LP e MEDIA_EM_MT em 2021
saeb_2017_co <- saeb_2017_co %>% rename(MEDIA_EM_LP = MEDIA_3EM_LP, MEDIA_EM_MT = MEDIA_3EM_MT)

# Selecionar apenas as colunas de interesse (até nível socioeconômico e depois apenas as médias)

saeb_2017_notas <- saeb_2017_co %>% select(ID_SAEB,ID_ESCOLA,ID_REGIAO,ID_MUNICIPIO,IN_PUBLICA,ID_LOCALIZACAO,NIVEL_SOCIO_ECONOMICO,MEDIA_5EF_LP,MEDIA_5EF_MT,MEDIA_9EF_LP,MEDIA_9EF_MT,MEDIA_EM_LP,MEDIA_EM_MT)

saeb_2021_notas <- saeb_2021_co %>% select(ID_SAEB,ID_ESCOLA,ID_REGIAO,ID_MUNICIPIO,IN_PUBLICA,ID_LOCALIZACAO,NIVEL_SOCIO_ECONOMICO,MEDIA_5EF_LP,MEDIA_5EF_MT,MEDIA_9EF_LP,MEDIA_9EF_MT,MEDIA_EM_LP,MEDIA_EM_MT)

# Encontrar escolas que estão presentes em ambos datasets
escolas_2017 <- saeb_2017_notas$ID_ESCOLA
escolas_2021 <- saeb_2021_notas$ID_ESCOLA

# independente da ordem, encontrar escolas que estão presentes em ambos datasets
escolas_comuns <- escolas_2017 %in% escolas_2021

# Integração dos datasets
saeb <- inner_join(saeb_2017_notas,saeb_2021_notas, by = c("ID_SAEB","ID_ESCOLA","ID_REGIAO","ID_MUNICIPIO","IN_PUBLICA","ID_LOCALIZACAO","NIVEL_SOCIO_ECONOMICO"), suffix = c("_2017", "_2021"))

# Salvar o dataset final
write.csv(saeb, "saeb.csv", row.names = FALSE)


# -----------------------------
# INTEGRAÇÃO

# Carregando os dados do PDDE
pdde <- read_csv("pdde_saldo_ac.csv")

# Carregando os dados do SAEB
saeb <- read_csv("saeb.csv")

# Variavel chave
pdde$`Código Escola` <- as.character(pdde$`Código Escola`)
saeb$ID_ESCOLA <- as.character(saeb$ID_ESCOLA)

# Renomeando a variável chave
colnames(pdde)[colnames(pdde) == "Código Escola"] <- "ID_ESCOLA"

# Merge
dados <- inner_join(pdde, saeb, by = "ID_ESCOLA")