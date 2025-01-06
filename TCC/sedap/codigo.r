# ---
# title: 'SEDAP - Modelos'
# author: 'Tailine J. S. Nonato'
# date: '2025-01-06'
# ---

### label ### novas_variaveis

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
  

### label ### merge_SAEB

df <- merge(df_aluno,df_escola, by = c("ID_ESCOLA", "ID_REGIAO","ID_SAEB", "ID_UF","ID_MUNICIPIO","ID_AREA","IN_PUBLICA", "ID_LOCALIZACAO"))

# checagem das variaveis
var_df <- names(df)

df <- vroom("DADOS/dados_tcc.csv")%>%
  select(ID_ALUNO,ID_ESCOLA,PROFICIENCIA_MT_SAEB,PROFICIENCIA_LP_SAEB, 
         ID_AREA, ID_LOCALIZACAO, NSE_ESCOLA, NSE_ALUNO, GRUPO_RACIAL, 
         ESCOLARIDADE_MAE, REPROVACAO)

df_used <- df %>%
  filter(across(everything(), ~ !(. %in% c("*", ".")))) %>%
  na.omit(df_used)

df_used$ID_AREA <- ifelse(df_used$ID_AREA == 2, 0, 1)
df_used$ID_LOCALIZACAO <- ifelse(df_used$ID_LOCALIZACAO == 2, 0, 1)

df_center <- df_used %>%
  mutate(across(c(ID_AREA, ID_LOCALIZACAO, NSE_ESCOLA, NSE_ALUNO, GRUPO_RACIAL, ESCOLARIDADE_MAE, REPROVACAO), ~ . - mean(.)))

############################################

### label ### modelo0_mt
modelo0_mt <- lmer(PROFICIENCIA_MT_SAEB ~ 1 + (1|ID_ESCOLA), data = df_used)
tab_model(modelo0_mt, show.ci=FALSE, show.icc=TRUE, show.dev=TRUE)

### label ### modelo1_mt
modelo1_mt <- lmer(PROFICIENCIA_MT_SAEB ~  GRUPO_RACIAL + ESCOLARIDADE_MAE + REPROVACAO + NSE_ALUNO + (1|ID_ESCOLA), data = df_used)
tab_model(modelo1_mt, show.ci=FALSE, show.icc=TRUE, show.dev=TRUE)

### label ### modelo2_mt
modelo2_mt <- lmer(PROFICIENCIA_MT_SAEB ~  GRUPO_RACIAL + ESCOLARIDADE_MAE + REPROVACAO + NSE_ALUNO + NSE_ESCOLA + ID_AREA +  ID_LOCALIZACAO + (1|ID_ESCOLA), data = df_used)
tab_model(modelo2_mt, show.ci=FALSE, show.icc=TRUE, show.dev=TRUE)

### label ### modelo4_mt
modelo4_mt <- lmer(PROFICIENCIA_MT_SAEB ~  GRUPO_RACIAL + ESCOLARIDADE_MAE + REPROVACAO + NSE_ALUNO + NSE_ESCOLA + ID_AREA +  ID_LOCALIZACAO + (1 + NSE_ALUNO + REPROVACAO + ESCOLARIDADE_MAE + GRUPO_RACIAL|ID_ESCOLA), data = df_used)
tab_model(modelo4_mt, show.ci=FALSE, show.icc=TRUE, show.dev=TRUE)

### label ### modelo5_mt
modelo5_mt <- lmer(PROFICIENCIA_MT_SAEB ~  GRUPO_RACIAL + ESCOLARIDADE_MAE + REPROVACAO + NSE_ALUNO + NSE_ESCOLA + ID_AREA +  ID_LOCALIZACAO + NSE_ESCOLA:REPROVACAO + NSE_ESCOLA:GRUPO_RACIAL + NSE_ESCOLA:ESCOLARIDADE_MAE + (1 + NSE_ALUNO + REPROVACAO + ESCOLARIDADE_MAE + GRUPO_RACIAL|ID_ESCOLA), data = df_used)
tab_model(modelo5_mt, show.ci=FALSE, show.icc=TRUE, show.dev=TRUE)

### label ### modelo6_mt
modelo6_mt <- lmer(PROFICIENCIA_MT_SAEB ~  GRUPO_RACIAL + ESCOLARIDADE_MAE + REPROVACAO + NSE_ALUNO + NSE_ESCOLA + ID_AREA +  ID_LOCALIZACAO + NSE_ESCOLA:REPROVACAO + NSE_ESCOLA:ESCOLARIDADE_MAE +  (1 + NSE_ALUNO + REPROVACAO + ESCOLARIDADE_MAE + GRUPO_RACIAL|ID_ESCOLA), data = df_used)
tab_model(modelo6_mt, show.icc = TRUE, show.dev=TRUE)

### label ### modelo7_mt
modelo7_mt <- lmer(PROFICIENCIA_MT_SAEB ~  GRUPO_RACIAL + ESCOLARIDADE_MAE + REPROVACAO + NSE_ALUNO + NSE_ESCOLA + ID_AREA +  ID_LOCALIZACAO + NSE_ESCOLA:REPROVACAO + NSE_ESCOLA:ESCOLARIDADE_MAE +  (1 + NSE_ALUNO + REPROVACAO + GRUPO_RACIAL|ID_ESCOLA), data = df_used)
tab_model(modelo7_mt, show.icc = TRUE, show.dev=TRUE)