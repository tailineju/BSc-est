## Carregamento de pacotes e dados
if (!require(pacman)) install.packages("pacman")
pacman::p_load(vroom,tidyverse, knitr, openxlsx, 
               kableExtra, lme4, gridExtra, clipr, 
               corrplot, ggplot2,sjPlot)
options(OutDec = ",")
setwd("C:/Users/User/Documents/GitHub/gradest-1/TCC/relatorio")

# Limpeza dos dados ----------------------------------------------

## Filtragem dos dados para região Centro-Oeste

df_aluno <- vroom("TS_ALUNO_9EF_2019.csv", locale = locale(encoding = "Latin1"), delim = ";") #2.388.931
df_aluno <- df_aluno %>% filter(IN_PREENCHIMENTO_LP ==1, IN_PREENCHIMENTO_MT ==1) #1.943.252, #1.942.375

df_escola <- vroom("TS_ESCOLA_2019.csv", locale = locale(encoding = "Latin1"), delim = ";") #70.606

df_aluno <- df_aluno %>% filter(ID_REGIAO == 5) #162.370
df_escola <- df_escola %>% filter(ID_REGIAO == 5) #4.794

write.csv(df_aluno, file = "TS_ALUNO_9EF_2019_co.csv")
write.csv(df_escola, file = "TS_ESCOLA_2019_co.csv")

## Leitura dos dados já filtrados

df_aluno <- vroom("TS_ALUNO_9EF_2019_co.csv", locale = locale(encoding = "UTF-8"))
df_escola <- vroom("TS_ESCOLA_2019_co.csv", locale = locale(encoding = "UTF-8"))

# Pontuação do questionário socioeconômico
df_aluno <- df_aluno %>%
  mutate(PONT1 = case_when( #Escolaridade da pessoa de referência 0 1 2 4 7
      TX_RESP_Q004 == "A" ~ 0,
      TX_RESP_Q004 == "B" ~ 1,
      TX_RESP_Q004 == "C" ~ 2,
      TX_RESP_Q004 == "D" ~ 3,
      TX_RESP_Q004 == "E" ~ 4,
      TX_RESP_Q004 == "F" ~ 0,
      TRUE ~ NA_real_),
    PONT2 = case_when( #Banheiros 0 3 7 10 14
      TX_RESP_Q009F == "A" ~ 0,
      TX_RESP_Q009F == "B" ~ 1,
      TX_RESP_Q009F == "C" ~ 2,
      TX_RESP_Q009F == "D" ~ 3,
      TRUE ~ NA_real_),
    PONT4 = case_when( #Automóveis 0 3 5 8 11
      TX_RESP_Q009G == "A" ~ 0,
      TX_RESP_Q009G == "B" ~ 1,
      TX_RESP_Q009G == "C" ~ 2,
      TX_RESP_Q009G == "D" ~ 3,
      TRUE ~ NA_real_),
    PONT5 = case_when(#Microcomputador 0 3 6 8 11 
      TX_RESP_Q009A == "A" ~ 0,
      TX_RESP_Q009A == "B" ~ 1,
      TX_RESP_Q009A == "C" ~ 2,
      TX_RESP_Q009A == "D" ~ 3,
      TRUE ~ NA_real_),
    PONT7 = case_when(#Geladeira 0 2 3 5 5
      TX_RESP_Q009A == "A" ~ 0,
      TX_RESP_Q009A == "B" ~ 1,
      TX_RESP_Q009A == "C" ~ 2,
      TX_RESP_Q009A == "D" ~ 3,
      TRUE ~ NA_real_),
    PONT8 = case_when(#Freezer 0 2 4 6 6
      TX_RESP_Q010I == "A" ~ 0,
      TX_RESP_Q010I == "B" ~ 1,
      TRUE ~ NA_real_),
    PONT9 = case_when(#Lava roupa 0 2 4 6 6
      TX_RESP_Q010H == "A" ~ 0,
      TX_RESP_Q010H == "B" ~ 1,
      TRUE ~ NA_real_),
    PONT11 = case_when(#Micro-ondas 0 2 4 4 4
      TX_RESP_Q010F == "A" ~ 0,
      TX_RESP_Q010F == "B" ~ 1,
      TRUE ~ NA_real_),
    PONT14 = case_when(#água encanada 
      TX_RESP_Q008B == "A" ~ 0,
      TX_RESP_Q008B == "B" ~ 1,
      TRUE ~ NA_real_),
    PONT15 = case_when(#rua pavimentada
      TX_RESP_Q008A == "A" ~ 0,
      TX_RESP_Q008A == "B" ~ 1,
      TRUE ~ NA_real_) )

# PCA para NSE aluno
pca_data <- df_aluno %>%
  select(PONT1, PONT2, PONT4, PONT5, PONT7, PONT8, PONT9, PONT11, PONT14, PONT15) %>%
  mutate(across(everything(), as.numeric)) 

non_na_rows <- complete.cases(pca_data)
  
pca_data <- pca_data %>% na.omit()

pca_data_norm <- scale(pca_data)

pca_result <- princomp(pca_data_norm)
summary(pca_result)

df_aluno$NSE_pca <- NA
df_aluno$NSE_pca[non_na_rows] <- pca_result$scores[, 1]

# Escolaridade da mãe

df_aluno <- df_aluno %>%
  mutate(ESCDADE = PONT1)

# NSE da escola

df_escola <- df_escola %>%
  mutate(NSE_esc = case_when(
    NIVEL_SOCIO_ECONOMICO == "Nível I" ~ 1,
    NIVEL_SOCIO_ECONOMICO == "Nível II" ~ 2,
    NIVEL_SOCIO_ECONOMICO == "Nível III" ~ 3,
    NIVEL_SOCIO_ECONOMICO == "Nível IV" ~ 4,
    NIVEL_SOCIO_ECONOMICO == "Nível V" ~ 5,
    NIVEL_SOCIO_ECONOMICO == "Nível VI" ~ 6,
    NIVEL_SOCIO_ECONOMICO == "Nível VII" ~ 7,
    TRUE ~ as.numeric(NIVEL_SOCIO_ECONOMICO)
  ))

# Merge dos dados ----------------------------------------------

df <- merge(df_aluno,df_escola, by = c("ID_ESCOLA", "ID_REGIAO","ID_SAEB", "ID_UF","ID_MUNICIPIO","ID_AREA","IN_PUBLICA", "ID_LOCALIZACAO"))
var_df <- names(df)

write.csv(df,"dados_tcc.csv")

# Dados para os modelos ----------------------------------------------

df_used <- vroom("dados_tcc.csv")%>%
  select(ID_ALUNO,ID_ESCOLA,PROFICIENCIA_MT_SAEB,PROFICIENCIA_LP_SAEB, NSE_esc, ID_AREA, ID_LOCALIZACAO, TX_RESP_Q002, TX_RESP_Q004, ESCDADE, TX_RESP_Q015, NSE_pca)

kable(head(df_used))

df_used <- df_used %>%
  filter(across(everything(), ~ !(. %in% c("*", ".")))) %>%
  na.omit(df_used)

write_csv(df_used, "df_used.csv")

## Leitura direta dos dados já filtrados
df_used <- vroom("df_used.csv")

# Recode área e localização da escola
table(df_used$ID_AREA)
table(df_used$ID_LOCALIZACAO)

df_used$ID_AREA <- ifelse(df_used$ID_AREA == 1, 0, 1)
df_used$ID_LOCALIZACAO <- ifelse(df_used$ID_LOCALIZACAO == 1, 0, 1)

table(df_used$ID_AREA)
table(df_used$ID_LOCALIZACAO)

# Theme para gráficos ----------------------------------------------
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
      axis.title = element_text(colour="black", size=18),
    )
}

# Exploração dos dados ----------------------------------------------


#  (var. numéricas)

plot(y = df_used$PROFICIENCIA_LP_SAEB, x = df_used$NSE_pca, main = "Proficiência em Língua Portuguesa x NSE Aluno", xlab = "NSE Aluno", ylab = "Proficiência")
plot(y = df_used$PROFICIENCIA_MT_SAEB, x = df_used$NSE_pca, main = "Proficiência em Matemática x NSE Aluno", xlab = "NSE Aluno", ylab = "Proficiência")

plot(y = df_used$PROFICIENCIA_LP_SAEB, x = df_used$NSE_esc, main = "Proficiência em Língua Portuguesa x NSE Escola", xlab = "NSE Escola", ylab = "Proficiência")
plot(y = df_used$PROFICIENCIA_MT_SAEB, x = df_used$NSE_esc, main = "Proficiência em Matemática x NSE Escola", xlab = "NSE Escola", ylab = "Proficiência")

plot(y = df_used$PROFICIENCIA_LP_SAEB, x = df_used$ESCDADE, main = "Proficiência em Língua Portuguesa x ESCDADE")
plot(y = df_used$PROFICIENCIA_MT_SAEB, x = df_used$ESCDADE, main = "Proficiência em Matemática x ESCDADE", xlab = "ESCDADE", ylab = "Proficiência")

# 
### Quantidade de Escolas
	 


df %>% 
    summarise(n = n_distinct(ID_ESCOLA))


### Quantidade de Alunos por Escola



df %>%
    group_by(ID_ESCOLA) %>%
    summarise(n = n()) %>%
    ggplot(aes(x = n)) +
    geom_histogram(binwidth = 10, fill = "white", color="black") +
    labs(x = "",
         y = "Frequência") +
  theme_tcc()
ggsave("img/dist_alunos_escola.pdf", width = 8, height = 5, dpi = 300)

# quadro resumo
df %>%
  group_by(ID_ESCOLA) %>%
  summarise(n = n()) %>%
  summarise(
    Média = mean(n),
    `Desvio Padrão` = sd(n),
    Mínimo = min(n),
    `Primeiro Quartil` = quantile(n, 0.25),
    Mediana = median(n),
    `Terceiro Quartil` = quantile(n, 0.75),
    Máximo = max(n)
  ) %>%
  mutate(across(everything(), ~format(round(., 2), nsmall = 2))) %>%
  pivot_longer(everything(), names_to = "Estatística", values_to = "Número de Alunos por Escola") %>%
  kable(booktabs = TRUE, caption = "Summary Statistics for Number of Students per School", format = "latex")
	

### Nível Socioecononômico (NSe) das Escolas

nse_freq <- df_escola %>%
    mutate(NIVEL_SOCIO_ECONOMICO = as.factor(NIVEL_SOCIO_ECONOMICO)) %>%
    filter(!is.na(NIVEL_SOCIO_ECONOMICO)) %>%
    count(NIVEL_SOCIO_ECONOMICO) %>%
    mutate(prop = round(n / sum(n) * 100,2)) %>%
    select(NIVEL_SOCIO_ECONOMICO, prop)  # Select only the desired columns

kable(nse_freq, caption = "Frequência do Nível Socioeconômico das Escolas", booktabs = TRUE, 
      col.names = c("Nível Socioeconômico", "Proporção (%)"), align = "c", format = "latex") %>%
  kable_styling()

# nse numerico



### Área das Escolas



area_freq <- df_escola %>%
    mutate(ID_AREA = ifelse(ID_AREA == 1, "Capital", "Interior")) %>%
    count(ID_AREA) %>%
    mutate(prop = round(n / sum(n) * 100,2)) %>%
    select(ID_AREA, prop)

kable(area_freq, caption = "Frequência da Área das Escolas", booktabs = TRUE, 
      col.names = c("Área", "Proporção (%)"), align = "c", format = "latex") 


### Localização das Escolas



loc_freq <- df_escola %>%
    mutate(ID_LOCALIZACAO = ifelse(ID_LOCALIZACAO == 1, "Urbana", "Rural")) %>%
    mutate(ID_LOCALIZACAO = factor(ID_LOCALIZACAO)) %>%
    count(ID_LOCALIZACAO) %>%
    mutate(prop = round(n / sum(n) * 100,2)) #%>%
    #select(ID_LOCALIZACAO, prop)

kable(loc_freq, caption = "Frequência da Administração das Escolas", booktabs = TRUE, align = "c", format = "latex")


### Distribuição da proficiência em Língua Portuguesa e Matemática



#lingua portuguesa (ggplot histogram)
df %>%
    ggplot(aes(x = PROFICIENCIA_LP_SAEB)) +
    geom_histogram(binwidth = 10,  fill = "white", color="black") +
    labs(x = "Língua Portuguesa",
         y = "Frequência") +
    xlim(100,400)+
    ylim(0,13000)+
    theme_tcc()
ggsave("img/dist_lp.pdf", width = 5, height = 5, dpi = 300)

#lingua portuguesa (qqplot)
df_sampled %>% 
  ggplot(aes(sample = PROFICIENCIA_LP_SAEB)) +
  stat_qq() +
  labs(x = "Quantis teóricos", y = "Quantis amostrais") +
  ylim(100,400)+
  theme_tcc()
ggsave("img/qqplot_lp.pdf", width = 5, height = 5, dpi = 300)

#lingua portuguesa (boxplot)
df %>%
    ggplot(aes(y = PROFICIENCIA_MT_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "Língua Portuguesa", y="") +
  scale_x_discrete(labels = c("Língua Portuguesa")) +
  ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_lp.pdf", width = 5, height = 5, dpi = 300)

#matemática (ggplot histogram)
df %>%
    ggplot(aes(x = PROFICIENCIA_MT_SAEB)) +
    geom_histogram(binwidth = 10,  fill = "white", color="black") +
    labs(x = "Matemática",
         y = "Frequência") +
    xlim(100,400)+
    ylim(0,13000)+
  theme_tcc()
ggsave("img/dist_mt.pdf", width = 5, height = 5, dpi = 300)

#matematica (qqplot)
df_sampled %>% 
  ggplot(aes(sample = PROFICIENCIA_MT_SAEB)) +
  stat_qq() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Quantis teóricos", y = "Quantis amostrais") +
  ylim(100,400)+
  theme_tcc()
ggsave("img/qqplot_mt.pdf", width = 5, height = 5, dpi = 300)

#matematica (boxplot)
df %>%
    ggplot(aes(y = PROFICIENCIA_MT_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "Matemática", y="") +
  scale_x_discrete(labels = c("Matemática")) +
  ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_mt.pdf", width = 5, height = 5, dpi = 300)


### Quadro Resumo  da proficiência em Língua Portuguesa e Matemática



lp_stats <- summarise(df,
  Média = mean(PROFICIENCIA_LP_SAEB, na.rm = TRUE),
  `Desvio Padrão` = sd(PROFICIENCIA_LP_SAEB, na.rm = TRUE),
  Mínimo = min(PROFICIENCIA_LP_SAEB, na.rm = TRUE),
  `Primeiro Quartil` = quantile(PROFICIENCIA_LP_SAEB, 0.25, na.rm = TRUE),
  Mediana = median(PROFICIENCIA_LP_SAEB, na.rm = TRUE),
  `Terceiro Quartil` = quantile(PROFICIENCIA_LP_SAEB, 0.75, na.rm = TRUE),
  Máximo = max(PROFICIENCIA_LP_SAEB, na.rm = TRUE)
) %>% pivot_longer(everything(), names_to = "Estatística", values_to = "Língua Portuguesa")

# Calculate summary statistics for Matemática
mt_stats <- summarise(df,
  Média = mean(PROFICIENCIA_MT_SAEB, na.rm = TRUE),
  `Desvio Padrão` = sd(PROFICIENCIA_MT_SAEB, na.rm = TRUE),
  Mínimo = min(PROFICIENCIA_MT_SAEB, na.rm = TRUE),
  `Primeiro Quartil` = quantile(PROFICIENCIA_MT_SAEB, 0.25, na.rm = TRUE),
  Mediana = median(PROFICIENCIA_MT_SAEB, na.rm = TRUE),
  `Terceiro Quartil` = quantile(PROFICIENCIA_MT_SAEB, 0.75, na.rm = TRUE),
  Máximo = max(PROFICIENCIA_MT_SAEB, na.rm = TRUE)
) %>% pivot_longer(everything(), names_to = "Estatística", values_to = "Matemática")

# Combine the statistics into one data frame
combined_stats <- full_join(lp_stats, mt_stats, by = "Estatística")

# round to 2 decimal places and decimal mark as comma
combined_stats <- combined_stats %>% mutate(across(where(is.numeric), ~format(round(., 2), nsmall = 2, decimal.mark = ",")))

combined_stats %>%
  kable(booktabs = TRUE, caption = "Summary Statistics for Língua Portuguesa and Matemática", format="latex") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F, font_size = 12) %>%
  scroll_box(width = "100%", height = "500px")



## Boxplots da proficiência em Língua Portuguesa e Matemática

### Por Nível Socioeconômico




df %>%
    filter(!is.na(NIVEL_SOCIO_ECONOMICO)) %>%
    ggplot(aes(x = as.factor(NIVEL_SOCIO_ECONOMICO), y = PROFICIENCIA_LP_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Língua Portuguesa") +
    ylim(100,400)+
    theme_tcc()
ggsave("img/boxplot_lp_nse.pdf", width = 8, height = 5, dpi = 300)

df %>%
    filter(!is.na(NIVEL_SOCIO_ECONOMICO)) %>%
    ggplot(aes(x = as.factor(NIVEL_SOCIO_ECONOMICO), y = PROFICIENCIA_MT_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Matemática") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_mt_nse.pdf", width = 8, height = 5, dpi = 300)


### Por Área




df %>%
    mutate(ID_AREA = ifelse(ID_AREA == 1, "Capital", "Interior")) %>%
    ggplot(aes(x = as.factor(ID_AREA), y = PROFICIENCIA_LP_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Língua Portuguesa") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_lp_area.pdf", width = 8, height = 5, dpi = 300)

df %>%
    mutate(ID_AREA = ifelse(ID_AREA == 1, "Capital", "Interior")) %>%
    ggplot(aes(x = as.factor(ID_AREA), y = PROFICIENCIA_MT_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Matemática") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_mt_area.pdf", width = 8, height = 5, dpi = 300)


### Por Administração




df %>%
    mutate(ID_LOCALIZACAO = ifelse(ID_LOCALIZACAO == 1, "Urbana", "Rural")) %>%
    ggplot(aes(x = as.factor(ID_LOCALIZACAO), y = PROFICIENCIA_LP_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Língua Portuguesa") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_lp_loc.pdf", width = 8, height = 5, dpi = 300)

df %>%
    mutate(ID_LOCALIZACAO = ifelse(ID_LOCALIZACAO == 1, "Urbana", "Rural")) %>%
    ggplot(aes(x = as.factor(ID_LOCALIZACAO), y = PROFICIENCIA_MT_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Matemática") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_mt_loc.pdf", width = 8, height = 5, dpi = 300)


### Por TX_RESP_Q002
unique(df$TX_RESP_Q002)

#tabela de frequencia
df %>% 
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q002 %in% c(".","*")) %>%
    mutate(TX_RESP_Q002 = case_when(
      TX_RESP_Q002 == "A" ~ "Branca",
      TX_RESP_Q002 == "B" ~ "Preta",
      TX_RESP_Q002 == "C" ~ "Parda",
      TX_RESP_Q002 == "D" ~ "Amarela",
      TX_RESP_Q002 == "E" ~ "Indígena",
      TX_RESP_Q002 == "F" ~ "ND",
      TRUE ~ TX_RESP_Q002
    )) %>% 
    mutate(TX_RESP_Q002 = factor(TX_RESP_Q002, levels = c("Branca", "Preta", "Parda", "Amarela", "Indígena", "ND")))%>%
    count(TX_RESP_Q002) %>%
    mutate(prop = round(n / sum(n) * 100,2)) %>%
    select(TX_RESP_Q002, prop) %>%
    kable(booktabs = TRUE, caption = "Frequência da Raça/Cor dos Responsáveis", format = "latex") %>%
    kable_styling()

df %>%
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q002 %in% c(".","*")) %>%
    mutate(TX_RESP_Q002 = case_when(
      TX_RESP_Q002 == "A" ~ "Branca",
      TX_RESP_Q002 == "B" ~ "Preta",
      TX_RESP_Q002 == "C" ~ "Parda",
      TX_RESP_Q002 == "D" ~ "Amarela",
      TX_RESP_Q002 == "E" ~ "Indígena",
      TX_RESP_Q002 == "F" ~ "ND",
      TRUE ~ TX_RESP_Q002
    )) %>%
    mutate(TX_RESP_Q002 = factor(TX_RESP_Q002, levels = c("Branca", "Preta", "Parda", "Amarela", "Indígena", "ND")))%>%
    ggplot(aes(x = TX_RESP_Q002, y = PROFICIENCIA_LP_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Língua Portuguesa") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_lp_raca.pdf", width = 8, height = 5, dpi = 300)

df %>%
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q002 %in% c(".","*")) %>%
    mutate(TX_RESP_Q002 = case_when(
      TX_RESP_Q002 == "A" ~ "Branca",
      TX_RESP_Q002 == "B" ~ "Preta",
      TX_RESP_Q002 == "C" ~ "Parda",
      TX_RESP_Q002 == "D" ~ "Amarela",
      TX_RESP_Q002 == "E" ~ "Indígena",
      TX_RESP_Q002 == "F" ~ "ND",
      TRUE ~ TX_RESP_Q002
    )) %>%
    mutate(TX_RESP_Q002 = factor(TX_RESP_Q002, levels = c("Branca", "Preta", "Parda", "Amarela", "Indígena", "ND"))) %>%
    ggplot(aes(x = TX_RESP_Q002, y = PROFICIENCIA_MT_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Matemática") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_mt_raca.pdf", width = 8, height = 5, dpi = 300)


### Por TX_RESP_Q004




unique(df$TX_RESP_Q004)

# tabela de frequencia

df %>% 
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q004 %in% c(".","*")) %>%
    count(TX_RESP_Q004) %>%
    mutate(prop = round(n / sum(n) * 100,2)) %>%
    select(TX_RESP_Q004, prop) %>%
    kable(booktabs = TRUE, caption = "Frequência do Nível de Escolaridade da Mãe", format = "latex") %>%
    kable_styling()

df %>%
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q004 %in% c(".","*")) %>%
    ggplot(aes(x = TX_RESP_Q004, y = PROFICIENCIA_LP_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Língua Portuguesa") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_lp_mae.pdf", width = 8, height = 5, dpi = 300)

df %>%
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q004 %in% c(".","*")) %>%
    ggplot(aes(x = TX_RESP_Q004, y = PROFICIENCIA_MT_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Matemática") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_mt_mae.pdf", width = 8, height = 5, dpi = 300)


### Por TX_RESP_Q011



#tabela de frequencia

df %>% 
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q011 %in% c(".","*")) %>%
    mutate(TX_RESP_Q011 = case_when(
    TX_RESP_Q011 == "A" ~ "Menos de 30min",
    TX_RESP_Q011 == "B" ~ "Entre 30min e 1h",
    TX_RESP_Q011 == "C" ~ "Mais de 1h",
    TRUE ~ TX_RESP_Q011
  )) %>%
  mutate(TX_RESP_Q011 = factor(TX_RESP_Q011, levels = c("Menos de 30min", "Entre 30min e 1h", "Mais de 1h"))) %>%
    count(TX_RESP_Q011) %>%
    mutate(prop = round(n / sum(n) * 100,2)) %>%
    select(TX_RESP_Q011, prop) %>%
    kable(booktabs = TRUE, caption = "Tempo para chegar a escola", format = "latex") %>%
    kable_styling()

df %>%
  filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q011 %in% c(".", "*")) %>%
  mutate(TX_RESP_Q011 = case_when(
    TX_RESP_Q011 == "A" ~ "Menos de 30min",
    TX_RESP_Q011 == "B" ~ "Entre 30min e 1h",
    TX_RESP_Q011 == "C" ~ "Mais de 1h",
    TRUE ~ TX_RESP_Q011
  )) %>%
  mutate(TX_RESP_Q011 = factor(TX_RESP_Q011, levels = c("Menos de 30min", "Entre 30min e 1h", "Mais de 1h"))) %>%
  ggplot(aes(x = TX_RESP_Q011, y = PROFICIENCIA_LP_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "", y = "Língua Portuguesa") +
  ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_lp_tempo.pdf", width = 8, height = 5, dpi = 300)

df %>%
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q011 %in% c(".","*")) %>%
    mutate(TX_RESP_Q011 = case_when(
      TX_RESP_Q011 == "A" ~ "Menos de 30min",
      TX_RESP_Q011 == "B" ~ "Entre 30min e 1h",
      TX_RESP_Q011 == "C" ~ "Mais de 1h",
      TRUE ~ TX_RESP_Q011
    )) %>%
    mutate(TX_RESP_Q011 = factor(TX_RESP_Q011, levels = c("Menos de 30min", "Entre 30min e 1h", "Mais de 1h"))) %>%
    ggplot(aes(x = as.factor(TX_RESP_Q011), y = PROFICIENCIA_MT_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Matemática") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_mt_tempo.pdf", width = 8, height = 5, dpi = 300)



### Por TX_RESP_Q015

#| 


#tabela de frequencia

df %>% 
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q015 %in% c(".","*")) %>%
    mutate(TX_RESP_Q015 = case_when(
      TX_RESP_Q015 == "A" ~ "Não",
      TX_RESP_Q015 == "B" ~ "1 vez",
      TX_RESP_Q015 == "C" ~ "2 ou mais",
      TRUE ~ TX_RESP_Q015
    )) %>%
    mutate(TX_RESP_Q015 = factor(TX_RESP_Q015, levels = c("Não", "1 vez", "2 ou mais"))) %>%
    count(TX_RESP_Q015) %>%
    mutate(prop = round(n / sum(n) * 100,2)) %>%
    select(TX_RESP_Q015, prop) %>%
    kable(booktabs = TRUE, caption = "Frequência da Reprovação dos Alunos", format = "latex") %>%
    kable_styling()


df %>%
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q015 %in% c(".","*")) %>%
    mutate(TX_RESP_Q015 = case_when(
      TX_RESP_Q015 == "A" ~ "Não",
      TX_RESP_Q015 == "B" ~ "1 vez",
      TX_RESP_Q015 == "C" ~ "2 ou mais",
      TRUE ~ TX_RESP_Q015
    )) %>%
    mutate(TX_RESP_Q015 = factor(TX_RESP_Q015, levels = c("Não", "1 vez", "2 ou mais"))) %>%
    ggplot(aes(x = TX_RESP_Q015, y = PROFICIENCIA_LP_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Língua Portuguesa") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_lp_reprovacao.pdf", width = 8, height = 5, dpi = 300)

df %>%
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q015 %in% c(".","*")) %>%
    mutate(TX_RESP_Q015 = case_when(
      TX_RESP_Q015 == "A" ~ "Não",
      TX_RESP_Q015 == "B" ~ "1 vez",
      TX_RESP_Q015 == "C" ~ "2 ou mais",
      TRUE ~ TX_RESP_Q015
    )) %>%
    mutate(TX_RESP_Q015 = factor(TX_RESP_Q015, levels = c("Não", "1 vez", "2 ou mais"))) %>%
    ggplot(aes(x = as.factor(TX_RESP_Q015), y = PROFICIENCIA_MT_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Matemática") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_mt_reprovacao.pdf", width = 8, height = 5, dpi = 300)


### Por TX_RESP_Q016



#tab de frequencia

df %>% 
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q016 %in% c(".","*")) %>%
    mutate(TX_RESP_Q016 = case_when(
      TX_RESP_Q016 == "A" ~ "Não",
      TX_RESP_Q016 == "B" ~ "1 vez",
      TX_RESP_Q016 == "C" ~ "2 ou mais",
      TRUE ~ TX_RESP_Q016
    )) %>%
    mutate(TX_RESP_Q016 = factor(TX_RESP_Q016, levels = c("Não", "1 vez", "2 ou mais"))) %>%
    count(TX_RESP_Q016) %>%
    mutate(prop = round(n / sum(n) * 100,2)) %>%
    select(TX_RESP_Q016, prop) %>%
    kable(booktabs = TRUE, caption = "Frequência do Abandono dos Alunos", format = "latex") %>%
    kable_styling()


df %>%
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q016 %in% c(".","*")) %>%
    mutate(TX_RESP_Q016 = case_when(
      TX_RESP_Q016 == "A" ~ "Não",
      TX_RESP_Q016 == "B" ~ "1 vez",
      TX_RESP_Q016 == "C" ~ "2 ou mais",
      TRUE ~ TX_RESP_Q016
    )) %>%
    mutate(TX_RESP_Q016 = factor(TX_RESP_Q016, levels = c("Não", "1 vez", "2 ou mais"))) %>%
    ggplot(aes(x = as.factor(TX_RESP_Q016), y = PROFICIENCIA_LP_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Língua Portuguesa") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_lp_abandono.pdf", width = 8, height = 5, dpi = 300)

df %>%
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q016 %in% c(".","*")) %>%
    mutate(TX_RESP_Q016 = case_when(
      TX_RESP_Q016 == "A" ~ "Não",
      TX_RESP_Q016 == "B" ~ "1 vez",
      TX_RESP_Q016 == "C" ~ "2 ou mais",
      TRUE ~ TX_RESP_Q016
    )) %>%
    mutate(TX_RESP_Q016 = factor(TX_RESP_Q016, levels = c("Não", "1 vez", "2 ou mais"))) %>%
    ggplot(aes(x = as.factor(TX_RESP_Q016), y = PROFICIENCIA_MT_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Matemática") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_mt_abandono.pdf", width = 8, height = 5, dpi = 300)

### Por TX_RESP_Q017A




unique(df$TX_RESP_Q017A)

#tab freq

df %>% 
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q017A %in% c(".","*")) %>%
    mutate(TX_RESP_Q017A = case_when(
      TX_RESP_Q017A == "A" ~ "Não tem",
      TX_RESP_Q017A == "B" ~ "Menos de 1h",
      TX_RESP_Q017A == "C" ~ "Entre 1 e 2h",
      TX_RESP_Q017A == "D" ~ "Mais de 2h",
      TRUE ~ TX_RESP_Q017A
    )) %>%
    mutate(TX_RESP_Q017A = factor(TX_RESP_Q017A, levels
    = c("Não tem", "Menos de 1h", "Entre 1 e 2h", "Mais de 2h"))) %>%
    count(TX_RESP_Q017A) %>%
    mutate(prop = round(n / sum(n) * 100,2)) %>%
    select(TX_RESP_Q017A, prop) %>%
    kable(booktabs = TRUE, caption = "Frequência do Tempo de Lazer dos Alunos", format = "latex")


df %>% 
  filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q017A %in% c(".","*")) %>%
  mutate(TX_RESP_Q017A = case_when(
    TX_RESP_Q017A == "A" ~ "Não tem",
    TX_RESP_Q017A == "B" ~ "Menos de 1h",
    TX_RESP_Q017A == "C" ~ "Entre 1 e 2h",
    TX_RESP_Q017A == "D" ~ "Mais de 2h",
    TRUE ~ TX_RESP_Q017A
  )) %>%
  mutate(TX_RESP_Q017A = factor(TX_RESP_Q017A, levels = c("Não tem", "Menos de 1h", "Entre 1 e 2h", "Mais de 2h"))) %>%
  ggplot(aes(x = TX_RESP_Q017A, y = PROFICIENCIA_LP_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "",
     y = "Língua Portuguesa") +
  ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_lp_lazer.pdf", width = 8, height = 5, dpi = 300)

df %>%
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q017A %in% c(".","*")) %>%
    mutate(TX_RESP_Q017A = case_when(
    TX_RESP_Q017A == "A" ~ "Não tem",
    TX_RESP_Q017A == "B" ~ "Menos de 1h",
    TX_RESP_Q017A == "C" ~ "Entre 1 e 2h",
    TX_RESP_Q017A == "D" ~ "Mais de 2h",
    TRUE ~ TX_RESP_Q017A
  )) %>%
    mutate(TX_RESP_Q017A = factor(TX_RESP_Q017A, levels = c("Não tem", "Menos de 1h", "Entre 1 e 2h", "Mais de 2h"))) %>%
    ggplot(aes(x = TX_RESP_Q017A, y = PROFICIENCIA_MT_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Matemática") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_mt_lazer.pdf", width = 8, height = 5, dpi = 300)


### Por TX_RESP_Q017B (Fazer cursos)




# descartar

unique(df$TX_RESP_Q017B)

df %>% 
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q017B %in% c(".","*")) %>%
    mutate(TX_RESP_Q017B = case_when(
      TX_RESP_Q017B == "A" ~ "Não tem",
      TX_RESP_Q017B == "B" ~ "Menos de 1h",
      TX_RESP_Q017B == "C" ~ "Entre 1 e 2h",
      TX_RESP_Q017B == "D" ~ "Mais de 2h",
      TRUE ~ TX_RESP_Q017B)) %>%
    mutate(TX_RESP_Q017B = factor(TX_RESP_Q017B, levels = c("Não tem", "Menos de 1h", "Entre 1 e 2h", "Mais de 2h"))) %>%
    ggplot(aes(x = TX_RESP_Q017B, y = PROFICIENCIA_LP_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Língua Portuguesa") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_lp_cursos.pdf", width = 8, height = 5, dpi = 300)

df %>% 
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q017B %in% c(".","*")) %>%
    mutate(TX_RESP_Q017B = case_when(
      TX_RESP_Q017B == "A" ~ "Não tem",
      TX_RESP_Q017B == "B" ~ "Menos de 1h",
      TX_RESP_Q017B == "C" ~ "Entre 1 e 2h",
      TX_RESP_Q017B == "D" ~ "Mais de 2h",
      TRUE ~ TX_RESP_Q017B)) %>%
    mutate(TX_RESP_Q017B = factor(TX_RESP_Q017B, levels = c("Não tem", "Menos de 1h", "Entre 1 e 2h", "Mais de 2h"))) %>%
    ggplot(aes(x = TX_RESP_Q017B, y = PROFICIENCIA_MT_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Matemática") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_mt_cursos.pdf", width = 8, height = 5, dpi = 300)


### Por TX_RESP_Q017C (Trabalhos domesticos)



# tabela de frequencia

df %>% 
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q017C %in% c(".","*")) %>%
    mutate(TX_RESP_Q017C = case_when(
      TX_RESP_Q017C == "A" ~ "Não tem",
      TX_RESP_Q017C == "B" ~ "Menos de 1h",
      TX_RESP_Q017C == "C" ~ "Entre 1 e 2h",
      TX_RESP_Q017C == "D" ~ "Mais de 2h",
      TRUE ~ TX_RESP_Q017C)) %>%
    mutate(TX_RESP_Q017C = factor(TX_RESP_Q017C, levels
    = c("Não tem", "Menos de 1h", "Entre 1 e 2h", "Mais de 2h"))) %>%
    count(TX_RESP_Q017C) %>%
    mutate(prop = round(n / sum(n) * 100,2)) %>%
    select(TX_RESP_Q017C, prop) %>%
    kable(booktabs = TRUE, caption = "Frequência do Tempo de Trabalho Doméstico dos Alunos", format = "latex") %>%
    kable_styling()


df %>% 
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q017C %in% c(".","*")) %>%
    mutate(TX_RESP_Q017C = case_when(
      TX_RESP_Q017C == "A" ~ "Não tem",
      TX_RESP_Q017C == "B" ~ "Menos de 1h",
      TX_RESP_Q017C == "C" ~ "Entre 1 e 2h",
      TX_RESP_Q017C == "D" ~ "Mais de 2h",
      TRUE ~ TX_RESP_Q017C)) %>%
    mutate(TX_RESP_Q017C= factor(TX_RESP_Q017C, levels = c("Não tem", "Menos de 1h", "Entre 1 e 2h", "Mais de 2h"))) %>%
    ggplot(aes(x = TX_RESP_Q017C, y = PROFICIENCIA_LP_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Língua Portuguesa") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_lp_domesticos.pdf", width = 8, height = 5, dpi = 300)

df %>%
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q017C %in% c(".","*")) %>%
    mutate(TX_RESP_Q017C = case_when(
      TX_RESP_Q017C == "A" ~ "Não tem",
      TX_RESP_Q017C == "B" ~ "Menos de 1h",
      TX_RESP_Q017C == "C" ~ "Entre 1 e 2h",
      TX_RESP_Q017C == "D" ~ "Mais de 2h",
      TRUE ~ TX_RESP_Q017C)) %>%
    mutate(TX_RESP_Q017C = factor(TX_RESP_Q017C, levels = c("Não tem", "Menos de 1h", "Entre 1 e 2h", "Mais de 2h"))) %>%
    ggplot(aes(x = TX_RESP_Q017C, y = PROFICIENCIA_MT_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Matemática") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_mt_domesticos.pdf", width = 8, height = 5, dpi = 300)


### Por TX_RESP_Q017D




#tab de frequencia

df %>% 
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q017D %in% c(".","*")) %>%
    mutate(TX_RESP_Q017D = case_when(
      TX_RESP_Q017D == "A" ~ "Não tem",
      TX_RESP_Q017D == "B" ~ "Menos de 1h",
      TX_RESP_Q017D == "C" ~ "Entre 1 e 2h",
      TX_RESP_Q017D == "D" ~ "Mais de 2h",
      TRUE ~ TX_RESP_Q017D)) %>%
    mutate(TX_RESP_Q017D = factor(TX_RESP_Q017D, levels
    = c("Não tem", "Menos de 1h", "Entre 1 e 2h", "Mais de 2h"))) %>%
    count(TX_RESP_Q017D) %>%
    mutate(prop = round(n / sum(n) * 100,2)) %>%
    select(TX_RESP_Q017D, prop) %>%
    kable(booktabs = TRUE, caption = "Frequência do Tempo de Estudo dos Alunos", format = "latex") %>%
    kable_styling()

df %>%
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q017D %in% c(".","*")) %>%
   mutate(TX_RESP_Q017D = case_when(
      TX_RESP_Q017D == "A" ~ "Não tem",
      TX_RESP_Q017D == "B" ~ "Menos de 1h",
      TX_RESP_Q017D == "C" ~ "Entre 1 e 2h",
      TX_RESP_Q017D == "D" ~ "Mais de 2h",
      TRUE ~ TX_RESP_Q017D)) %>%
    mutate(TX_RESP_Q017D = factor(TX_RESP_Q017D, levels = c("Não tem", "Menos de 1h", "Entre 1 e 2h", "Mais de 2h"))) %>%
    ggplot(aes(x = TX_RESP_Q017D, y = PROFICIENCIA_LP_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Língua Portuguesa") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_lp_estudo.pdf", width = 8, height = 5, dpi = 300)

df %>%
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q017D %in% c(".","*")) %>%
     mutate(TX_RESP_Q017D = case_when(
      TX_RESP_Q017D == "A" ~ "Não tem",
      TX_RESP_Q017D == "B" ~ "Menos de 1h",
      TX_RESP_Q017D == "C" ~ "Entre 1 e 2h",
      TX_RESP_Q017D == "D" ~ "Mais de 2h",
      TRUE ~ TX_RESP_Q017D)) %>%
    mutate(TX_RESP_Q017D = factor(TX_RESP_Q017D, levels = c("Não tem", "Menos de 1h", "Entre 1 e 2h", "Mais de 2h"))) %>%
    ggplot(aes(x = TX_RESP_Q017D, y = PROFICIENCIA_MT_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Matemática") +
    ylim(100,400)+
  theme_tcc() 
ggsave("img/boxplot_mt_estudo.pdf", width = 8, height = 5, dpi = 300)


### Por TX_RESP_Q017E
	


#tab de frequencia

df %>% 
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q017E %in% c(".","*")) %>%
    mutate(TX_RESP_Q017E = case_when(
      TX_RESP_Q017E == "A" ~ "Não tem",
      TX_RESP_Q017E == "B" ~ "Menos de 1h",
      TX_RESP_Q017E == "C" ~ "Entre 1 e 2h",
      TX_RESP_Q017E == "D" ~ "Mais de 2h",
      TRUE ~ TX_RESP_Q017E)) %>%
    mutate(TX_RESP_Q017E = factor(TX_RESP_Q017E, levels
    = c("Não tem", "Menos de 1h", "Entre 1 e 2h", "Mais de 2h"))) %>%
    count(TX_RESP_Q017E) %>%
    mutate(prop = round(n / sum(n) * 100,2)) %>%
    select(TX_RESP_Q017E, prop) %>%
    kable(booktabs = TRUE, caption = "Frequência do Tempo de Trabalho dos Alunos", format = "latex") %>%
    kable_styling()

df %>%
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q017E %in% c(".","*")) %>%
     mutate(TX_RESP_Q017E = case_when(
      TX_RESP_Q017E == "A" ~ "Não tem",
      TX_RESP_Q017E == "B" ~ "Menos de 1h",
      TX_RESP_Q017E == "C" ~ "Entre 1 e 2h",
      TX_RESP_Q017E == "D" ~ "Mais de 2h",
      TRUE ~ TX_RESP_Q017E)) %>%
    mutate(TX_RESP_Q017E = factor(TX_RESP_Q017E, levels = c("Não tem", "Menos de 1h", "Entre 1 e 2h", "Mais de 2h"))) %>%
    ggplot(aes(x = as.factor(TX_RESP_Q017E), y = PROFICIENCIA_LP_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Língua Portuguesa") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_lp_trabalho.pdf", width = 8, height = 5, dpi = 300)

df %>%
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q017E %in% c(".","*")) %>%
    mutate(TX_RESP_Q017E = case_when(
      TX_RESP_Q017E == "A" ~ "Não tem",
      TX_RESP_Q017E == "B" ~ "Menos de 1h",
      TX_RESP_Q017E == "C" ~ "Entre 1 e 2h",
      TX_RESP_Q017E == "D" ~ "Mais de 2h",
      TRUE ~ TX_RESP_Q017E)) %>%
    mutate(TX_RESP_Q017E = factor(TX_RESP_Q017E, levels = c("Não tem", "Menos de 1h", "Entre 1 e 2h", "Mais de 2h"))) %>%
    ggplot(aes(x = as.factor(TX_RESP_Q017E), y = PROFICIENCIA_MT_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Matemática") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_mt_trabalho.pdf", width = 8, height = 5, dpi = 300)


### Por TX_RESP_Q018A




unique(df$TX_RESP_Q018A)

#tab frequency()

df %>% 
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q018A %in% c(".","*")) %>%
    mutate(TX_RESP_Q018A = case_when(
      TX_RESP_Q018A == "A" ~ "Nunca ou quase nunca",
      TX_RESP_Q018A == "B" ~ "De vez em quando",
      TX_RESP_Q018A == "C" ~ "Sempre ou quase sempre",
      TRUE ~ TX_RESP_Q018A)) %>%
    mutate(TX_RESP_Q018A = factor(TX_RESP_Q018A, levels = c("Nunca ou quase nunca", "De vez em quando", "Sempre ou quase sempre"))) %>%
    count(TX_RESP_Q018A) %>%
    mutate(prop = round(n / sum(n) * 100,2)) %>%
    select(TX_RESP_Q018A, prop) %>%
    kable(booktabs = TRUE, caption = "Frequência do Uso de Notícias pelos Alunos", format = "latex") %>%
    kable_styling()

df %>%
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q018A %in% c(".","*")) %>%
    ggplot(aes(x = TX_RESP_Q018A, y = PROFICIENCIA_LP_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Língua Portuguesa") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_lp_noticias.pdf", width = 8, height = 5, dpi = 300)

df %>%
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q018A %in% c(".","*")) %>%
    ggplot(aes(x = TX_RESP_Q018A, y = PROFICIENCIA_MT_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Matemática") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_mt_noticias.pdf", width = 8, height = 5, dpi = 300)


### Por TX_RESP_Q018B (Livros)



#tab frequency(

df %>% 
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q018B %in% c(".","*")) %>%
    mutate(TX_RESP_Q018B = case_when(
      TX_RESP_Q018B == "A" ~ "Nunca ou quase nunca",
      TX_RESP_Q018B == "B" ~ "De vez em quando",
      TX_RESP_Q018B == "C" ~ "Sempre ou quase sempre",
      TRUE ~ TX_RESP_Q018B)) %>%
    mutate(TX_RESP_Q018B = factor(TX_RESP_Q018B, levels = c("Nunca ou quase nunca", "De vez em quando", "Sempre ou quase sempre"))) %>%
    count(TX_RESP_Q018B) %>%
    mutate(prop = round(n / sum(n) * 100,2)) %>%
    select(TX_RESP_Q018B, prop) %>%
    kable(booktabs = TRUE, caption = "Frequência do Uso de Livros pelos Alunos", format = "latex") %>%
    kable_styling()

df %>%
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q018B %in% c(".","*")) %>%
    ggplot(aes(x = TX_RESP_Q018B, y = PROFICIENCIA_LP_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Língua Portuguesa") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_lp_livros.pdf", width = 8, height = 5, dpi = 300)

df %>%
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q018B %in% c(".","*")) %>%
    ggplot(aes(x = TX_RESP_Q018B, y = PROFICIENCIA_MT_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Matemática") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_mt_livros.pdf", width = 8, height = 5, dpi = 300)


### Por TX_RESP_Q018C (Quadrinhos)



#descartar

df %>%
    filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q018C %in% c(".","*")) %>%
    ggplot(aes(x = TX_RESP_Q018C, y = PROFICIENCIA_LP_SAEB)) +
    geom_boxplot(fill = "white") +
    labs(x = "",
         y = "Língua Portuguesa") +
    ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_lp_quadrinhos.pdf", width = 8, height = 5, dpi = 300)

df %>%
  filter(IN_PREENCHIMENTO_QUESTIONARIO == 1, !TX_RESP_Q018C %in% c(".","*")) %>%
  ggplot(aes(x = TX_RESP_Q018C, y = PROFICIENCIA_MT_SAEB)) +
  geom_boxplot(fill = "white") +
  labs(x = "",
     y = "Matemática") +
  ylim(100,400)+
  theme_tcc()
ggsave("img/boxplot_mt_quadrinhos.pdf", width = 8, height = 5, dpi = 300)


### Ordinais para numericas


unique(df_used$NIVEL_SOCIO_ECONOMICO)
table(df_used$NIVEL_SOCIO_ECONOMICO)


table(df_used$NSE_pont)

unique(df_used$TX_RESP_Q004)
table(df_used$TX_RESP_Q004)

df_used <- df_used %>%
  mutate(escolaridade = case_when(
    TX_RESP_Q004 == "A" ~ 1,
    TX_RESP_Q004 == "B" ~ 2,
    TX_RESP_Q004 == "C" ~ 3,
    TX_RESP_Q004 == "D" ~ 4,
    TX_RESP_Q004 == "E" ~ 5,
    TX_RESP_Q004 == "F" ~ 0,
    TRUE ~ as.numeric(TX_RESP_Q004)
  ))
table(df_used$escolaridade)

write.csv(df_used, "df_used.csv", row.names = FALSE)

	

### Correlação entre as variáveis



# between pont and PROFICIENCIA_MT_SAEB
df_used %>%
  select(pont, PROFICIENCIA_MT_SAEB) %>%
  cor()

# between pont and PROFICIENCIA_LP_SAEB
df_used %>%
  select(pont, PROFICIENCIA_LP_SAEB) %>%
  cor()

# between NSE_pont and PROFICIENCIA_MT_SAEB
df_used %>%
  select(NSE_pont, PROFICIENCIA_MT_SAEB) %>%
  cor()

# between NSE_pont and PROFICIENCIA_LP_SAEB
df_used %>%
  select(NSE_pont, PROFICIENCIA_LP_SAEB) %>%
  cor()

# between NSE_pont and pont
df_used %>%
  select(NSE_pont, pont) %>%
  cor()

# escolaridade e PROFICIENCIA_MT_SAEB
df_used %>%
  select(escolaridade, PROFICIENCIA_MT_SAEB) %>%
  cor()

# escolaridade e PROFICIENCIA_LP_SAEB
df_used %>%
  select(escolaridade, PROFICIENCIA_LP_SAEB) %>%
  cor()

## correlation plot
correlation_matrix <- df_used %>%
  select(pont, NSE_pont, PROFICIENCIA_MT_SAEB, PROFICIENCIA_LP_SAEB, escolaridade) %>%
  cor()

correlation_matrix %>%
  corrplot::corrplot(method = "color", type = "upper", addCoef.col = "black", tl.col = "black", tl.srt = 45)







# Modelos

## Língua Portuguesa

# **Marginal R^2 (R^2m)**:  the proportion of variance explained by the fixed effects relative to the overall variance 

# **Conditional R^2 (R^2c)**: the proportion of variance explained by both fixed and random effects relative to the overall variance 

### Modelo 0

modelo_nulo_lp <- lmer(PROFICIENCIA_LP_SAEB ~ 1 + (1|ID_ESCOLA), data = df_used)
summary(modelo_nulo_lp)
tab_model(modelo_nulo_lp, show.icc = TRUE, title="Modelo Nulo")
	

### Modelo 1

modelo1_lp <- lmer(PROFICIENCIA_LP_SAEB ~ TX_RESP_Q002 + ESCDADE + TX_RESP_Q015 + NSE_pca + (1|ID_ESCOLA), data = df_used)
summary(modelo1_lp)
tab_model(modelo1_lp, show.icc = TRUE, title="Modelo com adição de variáveis do primeiro nível")


### Modelo 2

modelo2_lp <- lmer(PROFICIENCIA_LP_SAEB ~ TX_RESP_Q002 + ESCDADE + TX_RESP_Q015 + NSE_pca + NSE_esc + ID_AREA +  ID_LOCALIZACAO + (1|ID_ESCOLA), data = df_used)
summary(modelo2_lp)
tab_model(modelo2_lp, show.icc = TRUE, title="Modelo com adição de variáveis do segundo nível")


### Modelo 3


modelo3_lp <- lmer(PROFICIENCIA_LP_SAEB ~ TX_RESP_Q002 + ESCDADE + TX_RESP_Q015 + NSE_pca + NSE_esc + ID_AREA +  ID_LOCALIZACAO + (TX_RESP_Q015|ID_ESCOLA), data = df_used)
summary(modelo3_lp)
tab_model(modelo3_lp, show.icc = TRUE, title="Modelo com adição de efeitos aleatórios do primeiro nível")

modelo3_lp <- lmer(PROFICIENCIA_LP_SAEB ~ TX_RESP_Q002 + ESCDADE + TX_RESP_Q015 + NSE_pca + NSE_esc + ID_AREA +  ID_LOCALIZACAO + (TX_RESP_Q002|ID_ESCOLA), data = df_used)
summary(modelo3_lp)
tab_model(modelo3_lp, show.icc = TRUE, title="Modelo com adição de efeitos aleatórios do primeiro nível")

modelo3_lp <- lmer(PROFICIENCIA_LP_SAEB ~ TX_RESP_Q002 + ESCDADE + TX_RESP_Q015 + NSE_pca + NSE_esc  + ID_AREA +  ID_LOCALIZACAO + (NSE_pca|ID_ESCOLA), data = df_used)
summary(modelo3_lp)
tab_model(modelo3_lp, show.icc = TRUE, title="Modelo com adição de efeitos aleatórios do primeiro nível")


### Comparação entre modelos

kable(anova(modelo_nulo_lp, modelo1_lp))
kable(anova(modelo1_lp, modelo2_lp))
kable(anova(modelo2_lp, modelo3_lp))


### Análise de resíduos (Modelos com efeitos aleatórios)

residuos_lp <- resid(modelo3_lp)
qqnorm(residuos_lp)
qqline(residuos_lp)


## Matemática

### Modelo 0

modelo_nulo_mt <- lmer(PROFICIENCIA_MT_SAEB ~ 1 + (1|ID_ESCOLA), data = df_used)
summary(modelo_nulo_mt)
tab_model(modelo_nulo_mt, show.icc = TRUE)
	

### Modelo 1

modelo1_mt <- lmer(PROFICIENCIA_MT_SAEB ~  TX_RESP_Q002 + ESCDADE + TX_RESP_Q015 + (1|ID_ESCOLA), data = df_used)
summary(modelo1_mt)
tab_model(modelo1_mt, show.icc = TRUE, title="Modelo com adição de variáveis do primeiro nível")


### Modelo 2

modelo2_mt <- lmer(PROFICIENCIA_MT_SAEB ~  TX_RESP_Q002 + ESCDADE + TX_RESP_Q015 + NSE_pca + NSE_esc + ID_AREA +  ID_LOCALIZACAO + (1|ID_ESCOLA), data = df_used)
summary(modelo2_mt)
tab_model(modelo2_mt, show.icc = TRUE, title="Modelo com adição de variáveis do segundo nível")
	

### Modelo 3

modelo3_mt <- lmer(PROFICIENCIA_MT_SAEB ~  TX_RESP_Q002 + ESCDADE + TX_RESP_Q015 + NSE_pca + NSE_esc + ID_AREA +  ID_LOCALIZACAO + (TX_RESP_Q015|ID_ESCOLA), data = df_used)
summary(modelo3_mt)
tab_model(modelo3_mt, show.icc = TRUE, title="Modelo com adição de efeitos aleatórios do primeiro nível")
	

modelo3_mt <- lmer(PROFICIENCIA_MT_SAEB ~  TX_RESP_Q002 + ESCDADE + TX_RESP_Q015 + NSE_pca + NSE_esc + ID_AREA +  ID_LOCALIZACAO + (TX_RESP_Q002|ID_ESCOLA), data = df_used)
summary(modelo3_mt)
tab_model(modelo3_mt, show.icc = TRUE, title="Modelo com adição de efeitos aleatórios do primeiro nível")
	

modelo3_mt <- lmer(PROFICIENCIA_MT_SAEB ~  TX_RESP_Q002 + ESCDADE + TX_RESP_Q015 + NSE_pca + NSE_esc + ID_AREA +  ID_LOCALIZACAO + (NSE_pca|ID_ESCOLA), data = df_used)
summary(modelo3_mt)
tab_model(modelo3_mt, show.icc = TRUE, title="Modelo com adição de efeitos aleatórios do primeiro nível")
	

# Comparação entre modelos
kable(anova(modelo_nulo_lp, modelo1_lp))
kable(anova(modelo1_lp, modelo2_lp))
kable(anova(modelo2_mt, modelo3_mt))


## Análise de resíduos (Modelos com efeitos aleatórios)
residuos_mt <- resid(modelo3_mt)
qqnorm(residuos_mt)
qqline(residuos_mt)
