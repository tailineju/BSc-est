# Titile: Extração de Resultados SEDAP
# Autor: Tailine Nonato
# Date: 15 Jan 2025
# Description: Esse script apresenta todos os códigos e resultados obtidos na análise dos dados dentro do ambiente do SEDAP.

# Packages ----
library(tidyverse)
library(data.table)
library(lme4)
library(stargazer)

# Extraction, Transformation and Loading ----

# Manipulação PDDE

# Lendo os arquivos
files <- list.files(path = "T:/2. Bases_Externas/pdde", pattern = ".csv", full.names = TRUE)
pdde <- map_dfr(files, ~fread(.x) %>%
                  select(Ano, `Código Escola`, everything()), .id = "id_ano")

# Renomeando variáveis

colnames(pdde)[colnames(pdde) == "Valor Total"] <- "VL_TOTAL"
colnames(pdde)[colnames(pdde) == "Valor Custeio"] <- "VL_CUSTEIO"
colnames(pdde)[colnames(pdde) == "Valor Capital"] <- "VL_CAPITAL"


# Padronizando variáveis numéricas
pdde$VL_CUSTEIO <- gsub("\\.","",pdde$VL_CUSTEIO)
pdde$VL_CUSTEIO <- gsub(",",".",pdde$VL_CUSTEIO)
pdde$VL_CUSTEIO <- as.numeric(pdde$VL_CUSTEIO)

pdde$VL_CAPITAL <- gsub("\\.","",pdde$VL_CAPITAL)
pdde$VL_CAPITAL <- gsub(",",".",pdde$VL_CAPITAL)
pdde$VL_CAPITAL <- as.numeric(pdde$VL_CAPITAL)

pdde$VL_TOTAL <- gsub("\\.","",pdde$VL_TOTAL)
pdde$VL_TOTAL <- gsub(",",".",pdde$VL_TOTAL)
pdde$VL_TOTAL <- as.numeric(pdde$VL_TOTAL)

# Gerando base final PDDE
pdde <- pdde %>%
  group_by(`Código Escola`) %>%
  summarise(VL_TOTAL = sum(VL_TOTAL),
            VL_CUSTEIO = sum(VL_CUSTEIO),
            VL_CAPITAL = sum(VL_CAPITAL))
# SAEB

# Leitura dos dados Nível Escola
saeb_2015 <- fread("T:/1. Bases_Sedap/EM USO/TS_SAEB_ESCOLA_2015.csv", encoding = "Latin-1")
saeb_2019 <- fread("T:/1. Bases_Sedap/EM USO/TS_SAEB_ESCOLA_2019.csv", encoding = "Latin-1")

# Leitura dos dados Nível Aluno
## Tem a informação do código real da escola e do nível socioeconômico do aluno
alunos_2019_2 <- fread("T:/1. Bases_Sedap/EM USO/SAEB_ALUNO_9EF_2019_2.csv", encoding = "Latin-1")

## Tem o questionário socioeconômico
alunos_2019_1 <- fread("T:/1. Bases_Sedap/EM USO/TS_ALUNO_9EF_2019.csv", encoding = "Latin-1")

# Gerando a base dos alunos 
alunos_2019 <- merge(alunos_2019_1, alunos_2019_2[,c('ID_ALUNO', 'VL_INSE_ALUNO','ID_ESCOLA')], by='ID_ALUNO', all.x = TRUE)

# Filtro: alunos não presentes
saeb_2015 <- saeb_2015 %>% filter(!is.na(NU_MATRICULADOS_CENSO_9EF))
saeb_2019 <- saeb_2019 %>% filter(!is.na(NU_MATRICULADOS_CENSO_9EF))
alunos_2019 <- alunos_2019 %>% filter(IN_PRESENCA_MT==1)

# Filtro: apenas escolas do Centro-Oeste
saeb_2015_co <- saeb_2015 %>% filter(ID_REGIAO == 5)  
saeb_2019_co <- saeb_2019 %>% filter(ID_REGIAO == 5)
alunos_2019_co <- alunos_2019 %>% filter(ID_REGIAO == 5)

# Encontrar escolas que estão presentes em todos os datasets
escolas_2015 <- as.character(unique(saeb_2015_co$ID_ESCOLA))
escolas_2019 <- as.character(unique(saeb_2019_co$ID_ESCOLA)) #2536
escolas_comuns <-  intersect(escolas_2019,escolas_2015) #1998
escolas_pdde <- pdde$`Código Escola`

escolas_comuns_pdde <- intersect(escolas_comuns,escolas_pdde)

# Extraindo as médias das escolas em 2015
medias_2015 <- saeb_2015_co %>% 
  filter(ID_ESCOLA %in% escolas_comuns, !is.na(MEDIA_MT_9EF)) %>% 
  group_by(ID_ESCOLA) %>%
  summarise(MEDIA_LP_2015 = MEDIA_LP_9EF,MEDIA_MT_2015 = MEDIA_MT_9EF) 

medias_2015$MEDIA_LP_2015 <- as.numeric(medias_2015$MEDIA_LP_2015)
medias_2015$MEDIA_MT_2015 <- as.numeric(medias_2015$MEDIA_MT_2015)

# Integração PDDE e SAEB 2015

# Variavel chave
pdde$`Código Escola` <- as.character(pdde$`Código Escola`)
medias_2015$ID_ESCOLA <- as.character(medias_2015$ID_ESCOLA)
colnames(pdde)[colnames(pdde) == "Código Escola"] <- "ID_ESCOLA"

dados_2015_pdde <- merge(medias_2015,pdde, by="ID_ESCOLA", all.x = TRUE)

# Dados finais do SAEB 2015 + PDDE Acumulado (nível escola)
dados_2015_pdde <- dados_2015_pdde %>%
  mutate(VL_TOTAL=replace_na(VL_TOTAL,0),
         VL_CUSTEIO=replace_na(VL_CUSTEIO,0),
         VL_CAPITAL=replace_na(VL_CAPITAL,0))

# Dados finais do SAEB 2019 (nível escola)
dados_2019 <- saeb_2019_co %>% filter(ID_ESCOLA %in% escolas_comuns)

# Dados finais do SAEB 2019 (nível aluno)
colnames(alunos_2019_co)[colnames(alunos_2019_co) == "ID_SAEB"] <- "NU_ANO_SAEB"
colnames(alunos_2019_co)[colnames(alunos_2019_co) == "ID_ESCOLA.y"] <- "ID_ESCOLA"
alunos_2019_co <- alunos_2019_co %>% select(-ID_ESCOLA.x)
alunos_2019_co <- alunos_2019_co %>% filter(ID_ESCOLA %in% escolas_comuns)

# Salvando os datasets
write_csv(dados_2015_pdde, "T:/3. Resultados/Bases geradas/dados_2015_pdde.csv")
write_csv(dados_2019, "T:/3. Resultados/Bases geradas/dados_2019.csv")
write_csv(alunos_2019_co, "T:/3. Resultados/Bases geradas/alunos_2019.csv")


# Leitura dos dados SAEB 2019
df_escola <- fread("T:/3. Resultados/Bases geradas/dados_2019.csv")
df_aluno <- fread("T:/3. Resultados/Bases geradas/alunos_2019.csv")

# Novas variáveis (nível aluno)
df_aluno <- df_aluno %>%
  mutate(PONT1 = case_when(#escolaridade da mae
    TX_RESP_Q004 == "A" ~ 0,
    TX_RESP_Q004 == "B" ~ 1,
    TX_RESP_Q004 == "C" ~ 2,
    TX_RESP_Q004 == "D" ~ 3,
    TX_RESP_Q004 == "E" ~ 4,
    TX_RESP_Q004 == "F" ~ 0,
    TRUE ~ NA_real_),
    PONT2 = case_when(#contrata limpeza
      TX_RESP_Q007 == "A" ~ 0,
      TX_RESP_Q007 == "B" ~ 1,
      TX_RESP_Q007 == "C" ~ 3, 
      TRUE ~ NA_real_),
    PONT3 = case_when(#rua pavimentada
      TX_RESP_Q008A == "A" ~ 0,
      TX_RESP_Q008A == "B" ~ 1,
      TRUE ~ NA_real_),
    PONT4 = case_when(#água encanada 
      TX_RESP_Q008B == "A" ~ 0,
      TX_RESP_Q008B == "B" ~ 1,
      TRUE ~ NA_real_),
    PONT5 = case_when(#iluminação na rua
      TX_RESP_Q008C == "A" ~ 0,
      TX_RESP_Q008C == "B" ~ 1,
      TRUE ~ NA_real_),
    PONT6 = case_when(#geladeira
      TX_RESP_Q009A == "A" ~ 0,
      TX_RESP_Q009A == "B" ~ 1,
      TX_RESP_Q009A == "C" ~ 2,
      TX_RESP_Q009A == "D" ~ 3,
      TRUE ~ NA_real_),
    PONT7 = case_when(#tablet
      TX_RESP_Q009B == "A" ~ 0,
      TX_RESP_Q009B == "B" ~ 1,
      TX_RESP_Q009B == "C" ~ 2,
      TX_RESP_Q009B == "D" ~ 3,
      TRUE ~ NA_real_),
    PONT8 = case_when(#computador
      TX_RESP_Q009C == "A" ~ 0,
      TX_RESP_Q009C == "B" ~ 1,
      TX_RESP_Q009C == "C" ~ 2,
      TX_RESP_Q009C == "D" ~ 3,
      TRUE ~ NA_real_),
    PONT9 = case_when(#quartos
      TX_RESP_Q009D == "A" ~ 0,
      TX_RESP_Q009D == "B" ~ 1,
      TX_RESP_Q009D == "C" ~ 2,
      TX_RESP_Q009D == "D" ~ 3,
      TRUE ~ NA_real_),
    PONT10 = case_when(#televisao
      TX_RESP_Q009E == "A" ~ 0,
      TX_RESP_Q009E == "B" ~ 1,
      TX_RESP_Q009E == "C" ~ 2,
      TX_RESP_Q009E == "D" ~ 3,
      TRUE ~ NA_real_),
    PONT11 = case_when( #banheiro
      TX_RESP_Q009F == "A" ~ 0,
      TX_RESP_Q009F == "B" ~ 1,
      TX_RESP_Q009F == "C" ~ 2,
      TX_RESP_Q009F == "D" ~ 3,
      TRUE ~ NA_real_),    
    PONT12 = case_when( #carro
      TX_RESP_Q009G == "A" ~ 0,
      TX_RESP_Q009G == "B" ~ 1,
      TX_RESP_Q009G == "C" ~ 2,
      TX_RESP_Q009G == "D" ~ 3,
      TRUE ~ NA_real_),
    PONT13 = case_when(#tv a cabo
      TX_RESP_Q010A == "A" ~ 0,
      TX_RESP_Q010A == "B" ~ 1,
      TRUE ~ NA_real_),
    PONT14 = case_when(#wifi
      TX_RESP_Q010B == "A" ~ 0,
      TX_RESP_Q010B == "B" ~ 1,
      TRUE ~ NA_real_),
    PONT15 = case_when(#quarto separado
      TX_RESP_Q010C == "A" ~ 0,
      TX_RESP_Q010C == "B" ~ 1,
      TRUE ~ NA_real_),
    PONT16 = case_when(#mesa de estudo
      TX_RESP_Q010D == "A" ~ 0,
      TX_RESP_Q010D == "B" ~ 1,
      TRUE ~ NA_real_),
    PONT17 = case_when(#garagem
      TX_RESP_Q010E == "A" ~ 0,
      TX_RESP_Q010E == "B" ~ 1,
      TRUE ~ NA_real_),
    PONT18 = case_when(#microoondas
      TX_RESP_Q010F == "A" ~ 0,
      TX_RESP_Q010F == "B" ~ 1,
      TRUE ~ NA_real_),
    PONT19 = case_when(#aspirador
      TX_RESP_Q010G == "A" ~ 0,
      TX_RESP_Q010G == "B" ~ 1,
      TRUE ~ NA_real_),
    PONT20 = case_when(#lava roupa
      TX_RESP_Q010H == "A" ~ 0,
      TX_RESP_Q010H == "B" ~ 1,
      TRUE ~ NA_real_),
    PONT21 = case_when(#freezer
      TX_RESP_Q010I == "A" ~ 0,
      TX_RESP_Q010I == "B" ~ 1,
      TRUE ~ NA_real_))

df_aluno$ESCOLARIDADE_MAE <- df_aluno$PONT1

df_aluno$REPROVACAO <- ifelse(df_aluno$TX_RESP_Q015 == "A", 0, ifelse(df_aluno$TX_RESP_Q015 == "B", 1, 2))

df_aluno$GRUPO_RACIAL <- ifelse(df_aluno$TX_RESP_Q002 == "A" | df_aluno$TX_RESP_Q002 == "D", 1, 0)

# Análise de Componentes Principais 
pca_data <- df_aluno %>%
  select(PONT1, PONT2, PONT3, PONT4, PONT5, PONT6, PONT7, PONT8, PONT9, PONT10, PONT11, 
         PONT12, PONT13, PONT14, PONT15, PONT16, PONT17, PONT18, PONT19, PONT20, PONT21) %>%
  mutate(across(everything(), as.numeric)) 
non_na_rows <- complete.cases(pca_data)  
pca_data <- pca_data %>% na.omit()
pca_data_norm <- scale(pca_data)

pca_result <- princomp(pca_data_norm)

df_aluno$NSE_ALUNO <- NA
df_aluno$NSE_ALUNO[non_na_rows] <- pca_result$scores[, 1]

# Novas variáveis (nível escola)
df_escola <- df_escola %>%
  mutate(NSE_ESCOLA = case_when(
    NIVEL_SOCIO_ECONOMICO == "Nível I" ~ 1,
    NIVEL_SOCIO_ECONOMICO == "Nível II" ~ 2,
    NIVEL_SOCIO_ECONOMICO == "Nível III" ~ 3,
    NIVEL_SOCIO_ECONOMICO == "Nível IV" ~ 4,
    NIVEL_SOCIO_ECONOMICO == "Nível V" ~ 5,
    NIVEL_SOCIO_ECONOMICO == "Nível VI" ~ 6,
    NIVEL_SOCIO_ECONOMICO == "Nível VII" ~ 7,
    TRUE ~ as.numeric(NIVEL_SOCIO_ECONOMICO)
  ))


# Integração dos dados
df <- merge(df_aluno,df_escola, by = c("ID_ESCOLA","NU_ANO_SAEB","ID_UF","ID_LOCALIZACAO","ID_AREA"))
var_df <- names(df)
write_csv(df, "T:/3. Resultados/Bases geradas/dados_tcc.csv")

# Leitura dos dados (seleção final)
df <- fread("T:/3. Resultados/Bases geradas/dados_tcc.csv")%>%
  select(ID_ALUNO,ID_ESCOLA,PROFICIENCIA_MT_SAEB,PROFICIENCIA_LP_SAEB, 
         ID_AREA, ID_LOCALIZACAO, NSE_ESCOLA, VL_INSE_ALUNO, NSE_ALUNO, GRUPO_RACIAL,  ESCOLARIDADE_MAE, REPROVACAO)

# Nova base sem NA
df_used <- na.omit(df)

df_used$ID_AREA <- ifelse(df_used$ID_AREA == 2, 0, 1)
df_used$ID_LOCALIZACAO <- ifelse(df_used$ID_LOCALIZACAO == 2, 0, 1)

# Leitura dos dados SAEB/PDDE 2015
df_escola_2015 <- fread("T:/3. Resultados/Bases geradas/dados_2015_pdde.csv")

# Integração completa
df_full <- merge(df_used, df_escola_2015)

# Centralização das variáveis explicativas em suas médias
df_center <- df_full %>%
  mutate(across(c(ID_AREA, ID_LOCALIZACAO, NSE_ESCOLA, VL_INSE_ALUNO, NSE_ALUNO, GRUPO_RACIAL,  ESCOLARIDADE_MAE, REPROVACAO,
                  MEDIA_LP_2015, MEDIA_MT_2015, VL_TOTAL, VL_CUSTEIO, VL_CAPITAL), ~ . - mean(.,na.rm=TRUE)))

# Salvando os datasets
write_csv(df_used, "T:/3. Resultados/Bases geradas/df_used.csv")
write_csv(df_center, "T:/3. Resultados/Bases geradas/base_final_c.csv")
write_csv(df_full,"T:/3. Resultados/Bases geradas/base_final.csv")

# Leitura final dos dados ----
df <- fread("T:/3. Resultados/Bases geradas/base_final.csv")
dfc <- fread("T:/3. Resultados/Bases geradas/base_final_c.csv")

# Criando uma amostra dos dados para faciltar algumas plotagens
set.seed(123)
dfs <- df %>% sample_n(5000)

# Exploratory Data Analysis LP ----

# Função tema dos gráficos
theme_tcc <- function() {
  theme_classic() +
    theme(
      axis.line = element_line(colour = "black"),
      panel.border = element_rect(colour = "black", fill=NA),
      panel.background = element_blank(),
      plot.background = element_blank(),
      axis.ticks = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(colour = "black", size=16),
      axis.title = element_text(colour="black", size=18)
    )
}

# Língua Portuguesa (ggplot histogram)
df %>%
  ggplot(aes(x = PROFICIENCIA_LP_SAEB)) +
  geom_histogram(binwidth = 10, fill = "white", color="black") +
  labs(x = "Proficiência em Língua Portuguesa", y = "Frequência") +
  xlim(100, 400) +
  ylim(0, 9000) +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/1_dist_lp.pdf", width = 5, height = 5, dpi = 300)

# Língua Portuguesa (qqplot)
dfs %>%
  ggplot(aes(sample = PROFICIENCIA_LP_SAEB)) +
  stat_qq() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Quantis teóricos", y = "Quantis amostrais") +
  ylim(100, 400) +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/2_qqplot_lp.pdf", width = 5, height = 5, dpi = 300)

# Língua Portuguesa (boxplot)
df %>%
  ggplot(aes(y = PROFICIENCIA_LP_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "Proficiência em Língua Portuguesa", y = "") +
  scale_x_discrete(labels = c("Proficiência em Língua Portuguesa")) +
  ylim(100, 400) +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/3_boxplot_lp.pdf", width = 5, height = 5, dpi = 300)

# Língua Portuguesa e NSE do Aluno
dfs %>%
  filter(!is.na(NSE_ALUNO)) %>%
  ggplot(aes(x = NSE_ALUNO, y = PROFICIENCIA_LP_SAEB)) +
  geom_point(alpha = 0.1) +
  ylim(100, 400) +
  labs(x = "Nível Socioeconômico do Aluno", y = "Proficiência em Língua Portuguesa") +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/4_lp_nse_aluno.pdf", width = 8, height = 5, dpi = 300)

dfs %>%
  filter(!is.na(VL_INSE_ALUNO)) %>%
  ggplot(aes(x = VL_INSE_ALUNO, y = PROFICIENCIA_LP_SAEB)) +
  ylim(100, 400) +
  geom_point(alpha = 0.1) +
  labs(x = "Nível Socioeconômico do Aluno", y = "Proficiência em Língua Portuguesa") +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/4_2_lp_inse_aluno.pdf", width = 8, height = 5, dpi = 300)

# Língua Portuguesa e NSE da Escola
df %>%
  filter(!is.na(NSE_ESCOLA)) %>%
  ggplot(aes(x = as.factor(NSE_ESCOLA), y = PROFICIENCIA_LP_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "Nível Socioeconômico da Escola", y = "Proficiência em Língua Portuguesa") +
  ylim(100, 400) +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/5_lp_nse_esc.pdf", width = 8, height = 5, dpi = 300)

# Língua Portuguesa e Grupo Racial
df %>%
  mutate(GRUPO_RACIAL = ifelse(GRUPO_RACIAL == 1, "Brancos e Amarelos", "PPI")) %>%
  ggplot(aes(x = as.factor(GRUPO_RACIAL), y = PROFICIENCIA_LP_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "Grupo Racial", y = "Proficiência em Língua Portuguesa") +
  ylim(100, 400) +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/6_lp_racial.pdf", width = 8, height = 5, dpi = 300)

# Língua Portuguesa e Escolaridade da Mãe
df %>%
  ggplot(aes(x = as.factor(ESCOLARIDADE_MAE), y = PROFICIENCIA_LP_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "Escolaridade da Mãe", y = "Proficiência em Língua Portuguesa") +
  ylim(100, 400) +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/7_lp_esc_mae.pdf", width = 8, height = 5, dpi = 300)

# Língua Portuguesa e Reprovação
df %>%
  mutate(REPROVACAO = ifelse(REPROVACAO == 0, "Nenhuma", ifelse(REPROVACAO == 1, "Uma", "Duas ou mais"))) %>%
  mutate(REPROVACAO = factor(REPROVACAO, levels = c("Nenhuma", "Uma", "Duas ou mais"))) %>%
  ggplot(aes(x = as.factor(REPROVACAO), y = PROFICIENCIA_LP_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "Reprovação", y = "Proficiência em Língua Portuguesa") +
  ylim(100, 400) +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/8_lp_reprovacao.pdf", width = 8, height = 5, dpi = 300)

# Língua Portuguesa e Área
df %>%
  mutate(ID_AREA = ifelse(ID_AREA == 1, "Capital", "Interior")) %>%
  ggplot(aes(x = as.factor(ID_AREA), y = PROFICIENCIA_LP_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "Área", y = "Proficiência em Língua Portuguesa") +
  ylim(100, 400) +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/9_lp_area.pdf", width = 8, height = 5, dpi = 300)

# Língua Portuguesa e Localização
df %>%
  mutate(ID_LOCALIZACAO = ifelse(ID_LOCALIZACAO == 1, "Urbana", "Rural")) %>%
  mutate(ID_LOCALIZACAO = factor(ID_LOCALIZACAO, levels = c("Urbana", "Rural"))) %>%
  ggplot(aes(x = as.factor(ID_LOCALIZACAO), y = PROFICIENCIA_LP_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "Localização", y = "Proficiência em Língua Portuguesa") +
  ylim(100, 400) +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/10_lp_loc.pdf", width = 8, height = 5, dpi = 300)

# Distribuição de Alunos por Escola
df %>%
  group_by(ID_ESCOLA) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 10, fill = "white", color="black") +
  labs(x = "Número de Alunos por Escola", y = "Frequência") +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/11_dist_alunos_escola.pdf", width = 8, height = 5, dpi = 300)

# Língua Portuguesa e Valor Total
dfs %>%
  ggplot(aes(x = VL_TOTAL, y = PROFICIENCIA_LP_SAEB)) +
  geom_point(alpha = 0.1) +
  ylim(100, 400) +
  labs(x = "Valor Total de Repasses", y = "Proficiência em Língua Portuguesa") +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/12_lp_vl_total.pdf", width = 8, height = 5, dpi = 300)

# Exploratory Data Analysis MT ----

# Matemática (ggplot histogram)
df %>%
  ggplot(aes(x = PROFICIENCIA_MT_SAEB)) +
  geom_histogram(binwidth = 10, fill = "white", color="black") +
  labs(x = "Proficiência em Matemática", y = "Frequência") +
  xlim(100, 400) +
  ylim(0, 9000) +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/1_dist_mt.pdf", width = 5, height = 5, dpi = 300)

# Matemática (qqplot)
dfs %>%
  ggplot(aes(sample = PROFICIENCIA_MT_SAEB)) +
  stat_qq() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Quantis teóricos", y = "Quantis amostrais") +
  ylim(100, 400) +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/2_qqplot_mt.pdf", width = 5, height = 5, dpi = 300)

# Matemática (boxplot)
df %>%
  ggplot(aes(y = PROFICIENCIA_MT_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "Proficiência em Matemática", y = "") +
  scale_x_discrete(labels = c("Proficiência em Matemática")) +
  ylim(100, 400) +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/3_boxplot_mt.pdf", width = 5, height = 5, dpi = 300)

# Matemática e NSE do Aluno
dfs %>%
  filter(!is.na(NSE_ALUNO)) %>%
  ggplot(aes(x = NSE_ALUNO, y = PROFICIENCIA_MT_SAEB)) +
  geom_point(alpha = 0.1) +
  labs(x = "Nível Socioeconômico do Aluno", y = "Proficiência em Matemática") +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/4_mt_nse_aluno.pdf", width = 8, height = 5, dpi = 300)

dfs %>%
  filter(!is.na(VL_INSE_ALUNO)) %>%
  ggplot(aes(x = VL_INSE_ALUNO, y = PROFICIENCIA_MT_SAEB)) +
  geom_point(alpha = 0.1) +
  labs(x = "Nível Socioeconômico do Aluno", y = "Proficiência em Matemática") +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/4_2_mt_inse_aluno.pdf", width = 8, height = 5, dpi = 300)

# Matemática e NSE da Escola
df %>%
  filter(!is.na(NSE_ESCOLA)) %>%
  ggplot(aes(x = as.factor(NSE_ESCOLA), y = PROFICIENCIA_MT_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "Nível Socioeconômico da Escola", y = "Proficiência em Matemática") +
  ylim(100, 400) +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/5_mt_nse_esc.pdf", width = 8, height = 5, dpi = 300)

# Matemática e Grupo Racial
df %>%
  mutate(GRUPO_RACIAL = ifelse(GRUPO_RACIAL == 1, "Brancos e Amarelos", "PPI")) %>%
  ggplot(aes(x = as.factor(GRUPO_RACIAL), y = PROFICIENCIA_MT_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "Grupo Racial", y = "Proficiência em Matemática") +
  ylim(100, 400) +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/6_mt_racial.pdf", width = 8, height = 5, dpi = 300)

# Matemática e Escolaridade da Mãe
df %>%
  ggplot(aes(x = as.factor(ESCOLARIDADE_MAE), y = PROFICIENCIA_MT_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "Escolaridade da Mãe", y = "Proficiência em Matemática") +
  ylim(100, 400) +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/7_mt_esc_mae.pdf", width = 8, height = 5, dpi = 300)

# Matemática e Reprovação
df %>%
  mutate(REPROVACAO = ifelse(REPROVACAO == 0, "Nenhuma", ifelse(REPROVACAO == 1, "Uma", "Duas ou mais"))) %>%
  mutate(REPROVACAO = factor(REPROVACAO, levels = c("Nenhuma", "Uma", "Duas ou mais"))) %>%
  ggplot(aes(x = as.factor(REPROVACAO), y = PROFICIENCIA_MT_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "Reprovação", y = "Proficiência em Matemática") +
  ylim(100, 400) +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/8_mt_reprovacao.pdf", width = 8, height = 5, dpi = 300)

# Matemática e Área
df %>%
  mutate(ID_AREA = ifelse(ID_AREA == 1, "Capital", "Interior")) %>%
  ggplot(aes(x = as.factor(ID_AREA), y = PROFICIENCIA_MT_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "Área", y = "Proficiência em Matemática") +
  ylim(100, 400) +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/9_mt_area.pdf", width = 8, height = 5, dpi = 300)

# Matemática e Localização
df %>%
  mutate(ID_LOCALIZACAO = ifelse(ID_LOCALIZACAO == 1, "Urbana", "Rural")) %>%
  mutate(ID_LOCALIZACAO = factor(ID_LOCALIZACAO, levels = c("Urbana", "Rural"))) %>%
  ggplot(aes(x = as.factor(ID_LOCALIZACAO), y = PROFICIENCIA_MT_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "Localização", y = "Proficiência em Matemática") +
  ylim(100, 400) +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/10_mt_loc.pdf", width = 8, height = 5, dpi = 300)

# Matemática e Valor Total
dfs %>%
  ggplot(aes(x = VL_TOTAL, y = PROFICIENCIA_MT_SAEB)) +
  geom_point(alpha = 0.1) +
  ylim(100, 400) +
  labs(x = "Valor Total de Repasses", y = "Proficiência em Matemática") +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/12_mt_vl_total.pdf", width = 8, height = 5, dpi = 300)


# Linear Mixed Effects Models LP ----

# Passo 1: Nulo
modelo1_lp <- lmer(PROFICIENCIA_LP_SAEB ~ 1 + (1|ID_ESCOLA), data = dfc)
# Passo 2: nível 1
modelo2_lp <- lmer(PROFICIENCIA_LP_SAEB ~ GRUPO_RACIAL + ESCOLARIDADE_MAE + REPROVACAO + VL_INSE_ALUNO + (1|ID_ESCOLA), data = dfc)
# Passo 3: nível 2
modelo3_lp <- lmer(PROFICIENCIA_LP_SAEB ~ GRUPO_RACIAL + ESCOLARIDADE_MAE + REPROVACAO + VL_INSE_ALUNO + NSE_ESCOLA + ID_AREA + ID_LOCALIZACAO + (1|ID_ESCOLA), data = dfc)
# Passo 4: efeitos aleatorios
modelo4_lp <- lmer(PROFICIENCIA_LP_SAEB ~ GRUPO_RACIAL + ESCOLARIDADE_MAE + REPROVACAO + VL_INSE_ALUNO + NSE_ESCOLA + ID_AREA + ID_LOCALIZACAO + (1 + VL_INSE_ALUNO + REPROVACAO + ESCOLARIDADE_MAE + GRUPO_RACIAL|ID_ESCOLA), data = dfc)
# Passo 5: interações
modelo5_lp <- lmer(PROFICIENCIA_LP_SAEB ~ GRUPO_RACIAL + ESCOLARIDADE_MAE + REPROVACAO + VL_INSE_ALUNO + NSE_ESCOLA + ID_AREA + ID_LOCALIZACAO + NSE_ESCOLA:REPROVACAO + NSE_ESCOLA:ESCOLARIDADE_MAE + (1 + VL_INSE_ALUNO + REPROVACAO + GRUPO_RACIAL|ID_ESCOLA), data = dfc)

# Etapa 2 - +SAEB 2015 E PDDE 2015 A 2018
modelo6_lp <- lmer(PROFICIENCIA_LP_SAEB ~  GRUPO_RACIAL + ESCOLARIDADE_MAE + REPROVACAO + VL_INSE_ALUNO + NSE_ESCOLA + ID_AREA + VL_TOTAL + MEDIA_LP_2015 + NSE_ESCOLA:REPROVACAO + NSE_ESCOLA:ESCOLARIDADE_MAE + (1 + VL_INSE_ALUNO + REPROVACAO + GRUPO_RACIAL|ID_ESCOLA),data = dfc)


# Linear Mixed Effects Models MT ----

# Etapa 1 - SAEB 2019 

# Passo 1: Nulo
modelo1_mt <- lmer(PROFICIENCIA_MT_SAEB ~ 1 + (1|ID_ESCOLA), data = dfc)
# Passo 2: nível 1
modelo2_mt <- lmer(PROFICIENCIA_MT_SAEB ~ GRUPO_RACIAL + ESCOLARIDADE_MAE + REPROVACAO + VL_INSE_ALUNO + (1|ID_ESCOLA), data = dfc)
# Passo 3: nível 2
modelo3_mt <- lmer(PROFICIENCIA_MT_SAEB ~ GRUPO_RACIAL + ESCOLARIDADE_MAE + REPROVACAO + VL_INSE_ALUNO + NSE_ESCOLA + ID_AREA + ID_LOCALIZACAO + (1|ID_ESCOLA), data = dfc)
# Passo 4: efeitos aleatorios
modelo4_mt <- lmer(PROFICIENCIA_MT_SAEB ~ GRUPO_RACIAL + ESCOLARIDADE_MAE + REPROVACAO + VL_INSE_ALUNO + NSE_ESCOLA + ID_AREA + ID_LOCALIZACAO + (1 + VL_INSE_ALUNO + REPROVACAO + ESCOLARIDADE_MAE + GRUPO_RACIAL|ID_ESCOLA), data = dfc)
# Passo 5: interações
modelo5_mt <- lmer(PROFICIENCIA_MT_SAEB ~ GRUPO_RACIAL + ESCOLARIDADE_MAE + REPROVACAO + VL_INSE_ALUNO + NSE_ESCOLA + ID_AREA + ID_LOCALIZACAO + NSE_ESCOLA:REPROVACAO + NSE_ESCOLA:ESCOLARIDADE_MAE + (1 + VL_INSE_ALUNO + REPROVACAO + GRUPO_RACIAL|ID_ESCOLA), data = dfc)

# Etapa 2 - +SAEB 2015 E PDDE 2015 A 2018
modelo6_mt <- lmer(PROFICIENCIA_MT_SAEB ~  GRUPO_RACIAL + ESCOLARIDADE_MAE + REPROVACAO + VL_INSE_ALUNO + NSE_ESCOLA + ID_AREA + VL_TOTAL + MEDIA_MT_2015 + NSE_ESCOLA:REPROVACAO + NSE_ESCOLA:ESCOLARIDADE_MAE + (1 + VL_INSE_ALUNO + REPROVACAO + GRUPO_RACIAL|ID_ESCOLA),data = dfc)


# Análise de Resíduos MT e LP ----

residuos_lp <- residuals(modelo6_lp, type = "response")
dfc$residuos_lp <- residuos_lp

residuos_mt <- residuals(modelo6_mt, type = "response")
dfc$residuos_mt <- residuos_mt

# Sample da base com resíduos
set.seed(123)
dfcs <- dfc %>% sample_n(5000)

# Língua Portuguesa
## Histograma
dfcs %>%
  ggplot(aes(x = residuos_lp)) +
  geom_histogram(binwidth = 10, fill = "white", color="black") +
  labs(x = "Proficiência em Língua Portuguesa", y = "Frequência") +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/13_hist_res_lp.pdf", width = 5, height = 5, dpi = 300)

## QQPlot
dfcs %>%
  ggplot(aes(sample = residuos_lp)) +
  stat_qq() +
  labs(x = "Quantis teóricos", y = "Quantis amostrais") +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/14_qqplot_res_lp.pdf", width = 5, height = 5, dpi = 300)

## Resíduos vs. Predições
dfc %>%
  ggplot(aes(x = fitted(modelo6_lp), y = residuos_lp)) +
  geom_point(alpha = 0.1) +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "Predições", y = "Resíduos") +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/15_pred_res_lp.pdf", width = 5, height = 5, dpi = 300)


# Matemática
## Histograma
dfcs %>%
  ggplot(aes(x = residuos_mt)) +
  geom_histogram(binwidth = 10, fill = "white", color="black") +
  labs(x = "Proficiência em Matemática", y = "Frequência") +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/13_hist_res_mt.pdf", width = 5, height = 5, dpi = 300)

## QQPlot
dfcs %>%
  ggplot(aes(sample = residuos_mt)) +
  stat_qq() +
  labs(x = "Quantis teóricos", y = "Quantis amostrais") +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/14_qqplot_res_mt.pdf", width = 5, height = 5, dpi = 300)

## Resíduos vs. Predições
dfc %>%
  ggplot(aes(x = fitted(modelo6_mt), y = residuos_mt)) +
  geom_point(alpha = 0.1) +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "Predições", y = "Resíduos") +
  theme_tcc()
ggsave("T:/3. Resultados/1ª Extração de Resultados/IMG/15_pred_res_mt.pdf", width = 5, height = 5, dpi = 300)

# Exportação dos Resultados Numéricos ----

lp_modelos <- list(modelo1_lp, modelo2_lp, modelo3_lp, modelo4_lp, modelo5_lp, modelo6_lp)
mt_modelos <- list(modelo1_mt, modelo2_mt, modelo3_mt, modelo4_mt, modelo5_mt, modelo6_mt)

mt_file_t <- 'T:/3. Resultados/1ª Extração de Resultados/MODELOS_MT_TEXT.txt'
lp_file_t <- 'T:/3. Resultados/1ª Extração de Resultados/MODELOS_LP_TEXT.txt'

mr_file <- 'T:/3. Resultados/1ª Extração de Resultados/MEDIDAS_TEXT.txt'

mt_file_l <- 'T:/3. Resultados/1ª Extração de Resultados/MODELOS_MT_LATEX.tex'
lp_file_l <- 'T:/3. Resultados/1ª Extração de Resultados/MODELOS_LP_LATEX.tex'

# Modelos Matemática

## Texto
sink(mt_file_t)
for (i in seq_along(mt_modelos)){
  cat(paste0("Modelo ", i, ": \n"))
  stargazer(mt_modelos[[i]], 
            type = 'text',
            report='vc*p',
            decimal.mark = ',',
            digit.separator = '.',
            style = 'default')
  cat('\n')
  print(performance::icc(mt_modelos[[i]]))
  cat('\n')
  print(performance::r2(mt_modelos[[i]]))
  cat('\n # Matriz de Variâncias \n')
  print(as.data.frame(VarCorr(mt_modelos[[i]])))
  if (i< length(mt_modelos)) cat('\n ********************************************* \n')
}
sink()

## Latex
sink(mt_file_l)
for (i in seq_along(mt_modelos)){
  stargazer(mt_modelos[[i]], 
            type = 'latex',
            report='vc*p',
            decimal.mark = ',',
            digit.separator = '.',
            style = 'default')
}
sink()


# Modelos Língua Portuguesa

## Texto
sink(lp_file_t)
for (i in seq_along(lp_modelos)){
  cat(paste0("Modelo ", i, ": \n"))
  stargazer(lp_modelos[[i]], 
            type = 'text',
            report='vc*p',
            decimal.mark = ',',
            digit.separator = '.',
            style = 'default')
  cat('\n')
  print(performance::icc(lp_modelos[[i]]))
  cat('\n')
  print(performance::r2(lp_modelos[[i]]))
  cat('\n # Matriz de Variâncias \n')
  print(as.data.frame(VarCorr(lp_modelos[[i]])))
  if (i< length(lp_modelos)) cat('\n ********************************************* \n')
}
sink()

## Latex
sink(lp_file_l)
for (i in seq_along(lp_modelos)){
  stargazer(lp_modelos[[i]], 
            type = 'latex',
            report='vc*p',
            decimal.mark = ',',
            digit.separator = '.',
            style = 'default')
}
sink()

# Medidas
sink(mr_file)
cat('\n ********************************************* \n # Medidas Resumo - Língua Portuguesa \n')
summary(df$PROFICIENCIA_LP_SAEB)
cat('\n',paste0('SD: ',sd(df$PROFICIENCIA_LP_SAEB)), '\n')

cat('\n # Medidas Resumo - Matemática \n')
summary(df$PROFICIENCIA_MT_SAEB)
cat('\n',paste0('SD: ',sd(df$PROFICIENCIA_MT_SAEB)), '\n')

cat('\n # Medidas Resumo - Valor Total \n')
summary(df$VL_TOTAL)
cat('\n',paste0('SD: ',sd(df$VL_TOTAL)), '\n')

cat('\n # Correlação - Língua Portuguesa \n')
cor(df$PROFICIENCIA_LP_SAEB,df$VL_TOTAL)

cat('\n # Correlação - Matemática \n')
cor(df$PROFICIENCIA_MT_SAEB,df$VL_TOTAL)

cat('\n # Resumo PCA \n')
summary(pca_result)

cat('\n # Base de dados: Alunos Presentes 2019 Brasil \n')
dim(alunos_2019_2)

cat('\n # Base de dados: Alunos Presentes 2019 Centro-Oeste \n')
dim(alunos_2019_co)

cat('\n # Base de dados: SAEB 2019 Brasil \n')
dim(saeb_2019)

cat('\n # Base de dados: SAEB 2019 Centro-Oeste \n')
dim(saeb_2019_co)

cat('\n # Base de dados: SAEB 2015 Brasil \n')
dim(saeb_2015)

cat('\n # Base de dados: SAEB 2015 Centro-Oeste \n')
dim(saeb_2015_co)

cat('\n # Base de dados: Escolas comuns SAEB 2015 e SAEB 2019 \n')
length(escolas_comuns)

cat('\n # Base de dados: PDDE 2015 a 2018 Centro-Oeste \n')
dim(pdde)

cat('\n # Base de dados: Escolas comuns SAEB e PDDE \n')
length(escolas_comuns_pdde)

cat('\n # Base de dados: Escolas avaliadas \n')
n_distinct(df$ID_ESCOLA)

cat('\n # Base de dados: Alunos avaliados \n')
n_distinct(df$ID_ALUNO)
sink()
