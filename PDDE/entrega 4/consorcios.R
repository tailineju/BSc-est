if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, readxl, openxlsx)

dados <- read_excel("situacao-atendimento-entidade.xlsx")

## Verificação de UEx consórcios
# cnpj seduc é diferente de cnpj uex E cnpj uex aparece mais de 2 vezes nos dados
names(dados)

unique(dados$Destinacao)

destinacao <- dados %>%
  filter(Destinacao == "PDDE Basico - 2 Parcela 2022" | Destinacao == "PDDE Basico - 1 Parcela 2022")

cnpjs <- dados %>%
  filter(CNPJ_SEDUC != CNPJ_Ex) %>%
  group_by(CNPJ_Ex) %>%
  summarise(n = n()) 
  
consorcios <- cnpjs %>% filter(n > 2) 

summary(consorcios$n)

# identificar escolas que estão em consórcios
escolas_consorcios <- dados %>%
  filter(CNPJ_Ex %in% consorcios$CNPJ_Ex)

#adicionar numero de vezes que o cnpj aparece
escolas_consorcios <- escolas_consorcios %>%
  left_join(consorcios, by = "CNPJ_Ex")


# Transformando os códigos para facilitar a visualização
unique_codes <- unique(escolas_consorcios$CNPJ_Ex)
new_codes <- seq_along(unique_codes)
code_mapping <- setNames(new_codes, unique_codes)
escolas_consorcios$cod<- code_mapping[as.character(escolas_consorcios$CNPJ_Ex)]

# Exportando a tabela
wb <- createWorkbook()
addWorksheet(wb, "Escolas Consórcios")
writeData(wb, "Escolas Consórcios", escolas_consorcios)
saveWorkbook(wb, "e5_tabelas_tailine_consorcios.xlsx", overwrite = TRUE)
