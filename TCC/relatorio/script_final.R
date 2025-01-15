# Titile: Extração de Resultados SEDAP
# Autor: Tailine Nonato
# Date: Jan 2025
# Description: Esse script apresenta todos os códigos e resultados obtidos na análise dos dados dentro do ambiente do SEDAP.

# Extraction, Transformation and Loading ----

library(tidyverse)
library(data.table)
library(lme4)
library(sjPlot)

df <- fread("df_used.csv")
dfc <- fread("df_center.csv")

set.seed(123)
dfs <- df %>% sample_n(5000)

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

## PCA
#| label: pca_social_points

pca_data <- df_aluno %>%
  select(PONT1, PONT2, PONT3, PONT4, PONT5, PONT6, PONT7, PONT8, PONT9, PONT10, PONT11, 
         PONT12, PONT13, PONT14, PONT15, PONT16, PONT17, PONT18, PONT19, PONT20, PONT21) %>%
  mutate(across(everything(), as.numeric)) 
non_na_rows <- complete.cases(pca_data)  
pca_data <- pca_data %>% na.omit()
pca_data_norm <- scale(pca_data)

pca_result <- princomp(pca_data_norm)
summary(pca_result)

df_aluno$NSE_ALUNO <- NA
df_aluno$NSE_ALUNO[non_na_rows] <- pca_result$scores[, 1]


## NSE e escolaridade

#| label: vars_escola

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


# Exploratory Data Analysis ----

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

# Matemática (ggplot histogram)
df %>%
  ggplot(aes(x = PROFICIENCIA_MT_SAEB)) +
  geom_histogram(binwidth = 10, fill = "white", color="black") +
  labs(x = "Proficiência em Matemática", y = "Frequência") +
  xlim(100, 400) +
  ylim(0, 13000) +
  theme_tcc()
ggsave("EDA/img/dist_mt.pdf", width = 5, height = 5, dpi = 300)

# Matemática (qqplot)
dfs %>%
  ggplot(aes(sample = PROFICIENCIA_MT_SAEB)) +
  stat_qq() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Quantis teóricos", y = "Quantis amostrais") +
  ylim(100, 400) +
  theme_tcc()
ggsave("EDA/img/qqplot_mt.pdf", width = 5, height = 5, dpi = 300)

# Matemática (boxplot)
df %>%
  ggplot(aes(y = PROFICIENCIA_MT_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "Proficiência em Matemática", y = "") +
  scale_x_discrete(labels = c("Proficiência em Matemática")) +
  ylim(100, 400) +
  theme_tcc()
ggsave("EDA/img/boxplot_mt.pdf", width = 5, height = 5, dpi = 300)

# Matemática e NSE do Aluno
df %>%
  filter(!is.na(NSE_ALUNO)) %>%
  ggplot(aes(x = NSE_ALUNO, y = PROFICIENCIA_MT_SAEB)) +
  geom_point(alpha = 0.1) +
  labs(x = "Nível Socioeconômico do Aluno", y = "Proficiência em Matemática") +
  theme_tcc()
ggsave("EDA/img/mt_nse_aluno.pdf", width = 8, height = 5, dpi = 300)

# Matemática e NSE da Escola
df %>%
  filter(!is.na(NSE_ESCOLA)) %>%
  ggplot(aes(x = as.factor(NSE_ESCOLA), y = PROFICIENCIA_MT_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "Nível Socioeconômico da Escola", y = "Proficiência em Matemática") +
  ylim(100, 400) +
  theme_tcc()
ggsave("EDA/img/mt_nse_esc.pdf", width = 8, height = 5, dpi = 300)

# Matemática e Grupo Racial
df %>%
  mutate(GRUPO_RACIAL = ifelse(GRUPO_RACIAL == 1, "Brancos e Amarelos", "PPI")) %>%
  ggplot(aes(x = as.factor(GRUPO_RACIAL), y = PROFICIENCIA_MT_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "Grupo Racial", y = "Proficiência em Matemática") +
  ylim(100, 400) +
  theme_tcc()
ggsave("EDA/img/mt_racial.pdf", width = 8, height = 5, dpi = 300)

# Matemática e Escolaridade da Mãe
df %>%
  ggplot(aes(x = as.factor(ESCOLARIDADE_MAE), y = PROFICIENCIA_MT_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "Escolaridade da Mãe", y = "Proficiência em Matemática") +
  ylim(100, 400) +
  theme_tcc()
ggsave("EDA/img/mt_esc_mae.pdf", width = 8, height = 5, dpi = 300)

# Matemática e Reprovação
df %>%
  mutate(REPROVACAO = ifelse(REPROVACAO == 0, "Nenhuma", ifelse(REPROVACAO == 1, "Uma", "Duas ou mais"))) %>%
  mutate(REPROVACAO = factor(REPROVACAO, levels = c("Nenhuma", "Uma", "Duas ou mais"))) %>%
  ggplot(aes(x = as.factor(REPROVACAO), y = PROFICIENCIA_MT_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "Reprovação", y = "Proficiência em Matemática") +
  ylim(100, 400) +
  theme_tcc()
ggsave("EDA/img/mt_reprovacao.pdf", width = 8, height = 5, dpi = 300)

# Matemática e Área
df %>%
  mutate(ID_AREA = ifelse(ID_AREA == 1, "Capital", "Interior")) %>%
  ggplot(aes(x = as.factor(ID_AREA), y = PROFICIENCIA_MT_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "Área", y = "Proficiência em Matemática") +
  ylim(100, 400) +
  theme_tcc()
ggsave("EDA/img/mt_area.pdf", width = 8, height = 5, dpi = 300)

# Matemática e Localização
df %>%
  mutate(ID_LOCALIZACAO = ifelse(ID_LOCALIZACAO == 1, "Urbana", "Rural")) %>%
  mutate(ID_LOCALIZACAO = factor(ID_LOCALIZACAO, levels = c("Urbana", "Rural"))) %>%
  ggplot(aes(x = as.factor(ID_LOCALIZACAO), y = PROFICIENCIA_MT_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "Localização", y = "Proficiência em Matemática") +
  ylim(100, 400) +
  theme_tcc()
ggsave("EDA/img/mt_loc.pdf", width = 8, height = 5, dpi = 300)

# Distribuição de Alunos por Escola
df %>%
  group_by(ID_ESCOLA) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 10, fill = "white", color="black") +
  labs(x = "Número de Alunos por Escola", y = "Frequência") +
  theme_tcc()
ggsave("EDA/img/dist_alunos_escola.pdf", width = 8, height = 5, dpi = 300)

# Linear Mixed Effects Models ----

modelo0_mt <- lmer(PROFICIENCIA_MT_SAEB ~ 1 + (1|ID_ESCOLA), data = dfc)

modelo1_mt <- lmer(PROFICIENCIA_MT_SAEB ~ GRUPO_RACIAL + ESCOLARIDADE_MAE + REPROVACAO + NSE_ALUNO + (1|ID_ESCOLA), data = dfc)

modelo2_mt <- lmer(PROFICIENCIA_MT_SAEB ~ GRUPO_RACIAL + ESCOLARIDADE_MAE + REPROVACAO + NSE_ALUNO + NSE_ESCOLA + ID_AREA + ID_LOCALIZACAO + (1|ID_ESCOLA), data = dfc)

modelo4_mt <- lmer(PROFICIENCIA_MT_SAEB ~ GRUPO_RACIAL + ESCOLARIDADE_MAE + REPROVACAO + NSE_ALUNO + NSE_ESCOLA + ID_AREA + ID_LOCALIZACAO + (1 + NSE_ALUNO + REPROVACAO + ESCOLARIDADE_MAE + GRUPO_RACIAL|ID_ESCOLA), data = dfc)

modelo6_mt <- lmer(PROFICIENCIA_MT_SAEB ~ GRUPO_RACIAL + ESCOLARIDADE_MAE + REPROVACAO + NSE_ALUNO + NSE_ESCOLA + ID_AREA + ID_LOCALIZACAO + NSE_ESCOLA:REPROVACAO + NSE_ESCOLA:ESCOLARIDADE_MAE + (1 + NSE_ALUNO + REPROVACAO + ESCOLARIDADE_MAE + GRUPO_RACIAL|ID_ESCOLA), data = dfc)

modelo7_mt <- lmer(PROFICIENCIA_MT_SAEB ~ GRUPO_RACIAL + ESCOLARIDADE_MAE + REPROVACAO + NSE_ALUNO + NSE_ESCOLA + ID_AREA + ID_LOCALIZACAO + NSE_ESCOLA:REPROVACAO + NSE_ESCOLA:ESCOLARIDADE_MAE + (1 + NSE_ALUNO + REPROVACAO + GRUPO_RACIAL|ID_ESCOLA), data = dfc)

# Análise de Resíduos ----

residuos <- residuals(modelo8_mt, type = "response")
dfc$residuos <- residuos

# Resíduos vs. Predições

dfc %>%
  ggplot(aes(x = fitted(modelo8_mt), y = residuos)) +
  geom_point(alpha = 0.1) +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "Predições", y = "Resíduos") +
  theme_tcc()

