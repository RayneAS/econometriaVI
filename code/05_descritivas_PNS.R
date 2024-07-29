## Objetivo Executar a consulta SQL da Pesquisa Nacional de Saúde --------------
#E obter algumas estatisticas descritivas sobre afastamento 

rm(list = ls())
gc()

#PACKAGES USED
library(basedosdados)
library(tidyverse)
library(lubridate)
library(dplyr)
library(stargazer)


#Billing ID in Google Big Query
set_billing_id("stately-furnace-418521")

## Executar a consulta SQL PNS ------------------------------------------------
query_PNS_2019 <- "SELECT 
C001,
C004,
C006,
C008,
C009,
C01001,
D00901,
D014,
E001,
E005,
E006011,
E008,
E010010,
E010011,
E010012,
E010013,
E01402,
E01403,
E01501,
E01602,
E017,
J037,
J038,
J04001,
J04002
FROM `basedosdados.br_ms_pns.microdados_2019`;
"
PNS_2019 <- read_sql(query_PNS_2019)

names(PNS_2019) <- c("num_pes_dom", "cond_dom", "sexo","idade", "cor", "conj_mora",
                     "curso_mais_elev", "concluiu_curso", "trabalhou", "afast", 
                     "motivo_afast", "doenca_trab", "tempo_afast", "afast_1mes_1ano",
                     "afast_1ano_2ano", "afast_acima_2ano", "sevidor_pub", "carteira_assin",
                     "ativ_empresa", "rendimento_trab", "horas_trab", "internado", 
                     "qtas_internado" , "tempo_internado_meses", "tempo_internado_dias")

#Mudar para numeric
PNS_2019 <- PNS_2019 %>% 
  mutate_at(c('cond_dom', 'sexo','cor', 'afast','qtas_internado',
              'tempo_internado_meses', 'tempo_internado_dias' ), as.numeric)


##Filtros para analise especifica ----------------------------------------------
#Mantem as pessoas afastadas do trabalho 
PNS_sample <- PNS_2019 %>% 
  filter(afast == 1)

#Mantem apenas o chefe do domicilio e homem 
PNS_sample <- PNS_sample  %>% 
  filter(cond_dom == 1)

#Mantem apenas homem 
PNS_sample <- PNS_sample %>% 
  filter(sexo == 1)

#mantem apenas motivos de afastamento especificos 
PNS_sample <- PNS_sample  %>% 
  filter(motivo_afast == 3 | motivo_afast == 5)

freq <- table(PNS_sample$motivo_afast)
freq

#Mantem apenas idade entre 15 e 65 anos 
PNS_sample <- PNS_sample %>% 
  filter(idade >= 15 & idade <= 65)

freq <- table(addNA(PNS_sample$doenca_trab))
freq

freq <- table(addNA(PNS_sample$carteira_assin))
freq

freq <- table(addNA(PNS_sample$conj_mora))
freq

freq <- table(addNA(PNS_sample$cor))
freq

freq <- table(addNA(PNS_sample$tempo_afast))
freq

avg <- mean(PNS_sample$tempo_afast, na.rm = TRUE)
avg



freq <- table(addNA(PNS_sample$internado))
freq

freq <- table(addNA(PNS_sample$tempo_internado_dias))
freq

avg <- mean(PNS_sample$tempo_internado_dias, na.rm = TRUE)
avg

avg <- mean(PNS_sample$tempo_internado_meses, na.rm = TRUE)
avg

## Executar a consulta SQL PNS ------------------------------------------------
query_PNS_2019 <- "SELECT 
J001,
J00101,
J002,
J003,
J00402,
J00404,
J005,
J006,
J007
FROM `basedosdados.br_ms_pns.microdados_2019`;
"
PNS_2019 <- read_sql(query_PNS_2019)

names(PNS_2019) <- c("sit_saude", "sit_saude_2", "deixou_trab_saude",
                     "dias_deixou_trab_saude", "motivo_deixou_trab_saude", 
                     "rel_trab_deixou_trab_saude", "acamado", "dias_acamado",
                     "tem_doenca")



PNS_2019 <- PNS_2019 %>% 
  mutate_at(c('dias_deixou_trab_saude','dias_acamado'), as.numeric)


#Mantem as pessoas que deixaram de trabalhar por doença 
PNS_sample <- PNS_2019 %>% 
  filter(deixou_trab_saude == 'sim')

freq <- table(addNA(PNS_sample$motivo_deixou_trab_saude))
freq

freq <- table(addNA(PNS_sample$rel_trab_deixou_trab_saude))
freq

freq <- table(addNA(PNS_sample$dias_deixou_trab_saude))
freq

freq <- table(addNA(PNS_sample$acamado))
freq

freq <- table(addNA(PNS_sample$dias_acamado))
freq

freq <- table(addNA(PNS_sample$tem_doenca))
freq



#Mantem as pessoas que deixaram de trabalhar por motivo relacionado ao trabalho 
PNS_sample_2 <- PNS_sample %>% 
  filter(rel_trab_deixou_trab_saude == '1')

freq <- table(addNA(PNS_sample_2$motivo_deixou_trab_saude))
freq

freq <- table(addNA(PNS_sample_2$rel_trab_deixou_trab_saude))
freq

freq <- table(addNA(PNS_sample_2$dias_deixou_trab_saude))
freq

freq <- table(addNA(PNS_sample_2$acamado))
freq

freq <- table(addNA(PNS_sample_2$dias_acamado))
freq

freq <- table(addNA(PNS_sample_2$tem_doenca))
freq

freq <- table(addNA(PNS_sample_2$sit_saude))
freq

freq <- table(addNA(PNS_sample_2$sit_saude_2))
freq


## Executar a consulta SQL PNS ------------------------------------------------
query_PNS_2019 <- "SELECT
C001,
C004,
C008,
C009,
C01001,
D00901,
D014,
E001,
E005,
E006011,
E008,
E010010,
E010011,
E010012,
E010013,
E01402,
E01403,
E01501,
E01602,
E017,
J037,
J038,
J04001,
J04002,
UPA_PNS,
V0006_PNS,
C006,
C00701,
C00702,
C00703,
J001,
J00101,
J002,
J003,
J00402,
J00404,
J005,
J006,
J007,
E02801,
E02804,
E02806
FROM `basedosdados.br_ms_pns.microdados_2019`;
"
PNS_2019 <- read_sql(query_PNS_2019)



names(PNS_2019) <- c("num_pes_dom", "cond_dom","idade", "cor", "conj_mora",
                     "curso_mais_elev", "concluiu_curso", "trabalhou", "afast", 
                     "motivo_afast", "doenca_trab", "tempo_afast", "afast_1mes_1ano",
                     "afast_1ano_2ano", "afast_acima_2ano", "sevidor_pub", "carteira_assin",
                     "ativ_empresa", "rendimento_trab", "horas_trab", "internado", 
                     "qtas_internado" , "tempo_internado_meses", "tempo_internado_dias",
                     "UPA", "ordem_dom", "sexo","dia_nas", "mes_nas", "ano_nas",
                     "sit_saude", "sit_saude_2", "deixou_trab_saude",
                     "dias_deixou_trab_saude", "motivo_deixou_trab_saude", 
                     "rel_trab_deixou_trab_saude", "acamado", "dias_acamado",
                     "tem_doenca", "aux_cuidado_pess", "aux_companhia", "aux_outros")

PNS_2019 <- PNS_2019 %>%
  filter(!is.na(UPA) & !is.na(ordem_dom) &
    !is.na(sexo) & !is.na(dia_nas) & !is.na(mes_nas) & !is.na(ano_nas))


PNS_2019 <- PNS_2019 %>%
  mutate(
    dia_nas = str_pad(dia_nas, width = 2, pad = "0"),
    mes_nas = str_pad(mes_nas, width = 2, pad = "0")
  )

PNS_2019$iddom <- paste(PNS_2019$UPA, PNS_2019$ordem_dom, sep="")

PNS_2019$idind <- paste(PNS_2019$iddom, PNS_2019$sexo, PNS_2019$dia_nas, 
                        PNS_2019$mes_nas, PNS_2019$ano_nas, sep="")


#Mudar para numeric
PNS_2019 <- PNS_2019 %>% 
  mutate_at(c('cond_dom', 'sexo','cor', 'afast','qtas_internado',
              'tempo_internado_meses', 'tempo_internado_dias', 
              'dias_deixou_trab_saude','dias_acamado', 
              'motivo_deixou_trab_saude'), as.numeric)


# write.csv(PNS_2019, file = "D:/rayne/Documents/dados_econometria_VI/PNS_2019.csv",
#           row.names = FALSE)


#getting idind column as first 
PNS_2019 <- PNS_2019 %>% 
  select(iddom,idind, everything())

### PRODUZ TABELA COM MOTIVOS DE DEIXAR O TRABALHO POR DOENÇA--------------

##TABELA 1 -----------------------------------------------------------------

# Mantém as pessoas que deixaram de trabalhar por doença, são chefes e homens
PNS_sample <- PNS_2019 %>%
  filter(cond_dom == 1 & sexo == 1 & deixou_trab_saude == "sim")

freq <- table((PNS_sample$motivo_deixou_trab_saude))
freq


# Contar o número total de chefes do sexo masculino
total_chefes <- PNS_2019 %>%
  filter(cond_dom == 1 & sexo == 1) %>%
  nrow()

# Contar o número de chefes do sexo masculino que deixaram de trabalhar por problemas de saúde
total_chefes_doentes <- PNS_sample %>%
  nrow()

# Calcular a porcentagem de chefes que deixaram de trabalhar por problemas de saúde
porcentagem_chefes_doentes <- (total_chefes_doentes / total_chefes) * 100

# Contar o número de chefes do sexo masculino por tipo de motivo
motivos <- PNS_sample %>%
  count(motivo_deixou_trab_saude) %>%
  mutate(porcentagem = (n / total_chefes_doentes) * 100) %>%
  filter(motivo_deixou_trab_saude != 3) # Remove categoria 3

# Renomear os valores para rótulos mais amigáveis
motivos$motivo_deixou_trab_saude <- recode(motivos$motivo_deixou_trab_saude,
                                           `1` = "Problemas nos ossos e articulações",
                                           `2` = "Dor de cabeça ou enxaqueca",
                                           `4` = "Problema odontológico",
                                           `5` = "Problemas respiratórios",
                                           `6` = "Problemas gastrointestinais",
                                           `7` = "Dengue, Chikungunya, Zika ou Febre amarela",
                                           `8` = "Problemas cardiovasculares",
                                           `9` = "Diabetes",
                                           `10` = "Câncer",
                                           `11` = "Problemas neurológicos",
                                           `12` = "Saúde mental",
                                           `13` = "Lesões ou fraturas por acidentes ou violência",
                                           `14` = "Outro problema de saúde"
)

# Criar o data frame com as estatísticas dos chefes doentes
estatisticas <- data.frame(
  Estatística = c(
    "Total de Chefes (Sexo Masculino)", 
    "Total de Chefes (Sexo Masculino) que Deixaram de Trabalhar por Saúde", 
    "Porcentagem de Chefes (Sexo Masculino) que Deixaram de Trabalhar por Saúde (%)",
    "Problemas nos ossos e articulações (%)",
    "Dor de cabeça ou enxaqueca (%)",
    "Problema odontológico (%)",
    "Problemas respiratórios (%)",
    "Problemas gastrointestinais (%)",
    "Dengue, Chikungunya, Zika ou Febre amarela (%)",
    "Problemas cardiovasculares (%)",
    "Diabetes (%)",
    "Câncer (%)",
    "Problemas neurológicos (%)",
    "Saúde mental (%)",
    "Lesões ou fraturas por acidentes ou violência (%)",
    "Outro problema de saúde (%)"
  ),
  Valor = c(
    total_chefes,
    total_chefes_doentes,
    porcentagem_chefes_doentes,
    motivos$porcentagem[motivos$motivo_deixou_trab_saude == "Problemas nos ossos e articulações"],
    motivos$porcentagem[motivos$motivo_deixou_trab_saude == "Dor de cabeça ou enxaqueca"],
    motivos$porcentagem[motivos$motivo_deixou_trab_saude == "Problema odontológico"],
    motivos$porcentagem[motivos$motivo_deixou_trab_saude == "Problemas respiratórios"],
    motivos$porcentagem[motivos$motivo_deixou_trab_saude == "Problemas gastrointestinais"],
    motivos$porcentagem[motivos$motivo_deixou_trab_saude == "Dengue, Chikungunya, Zika ou Febre amarela"],
    motivos$porcentagem[motivos$motivo_deixou_trab_saude == "Problemas cardiovasculares"],
    motivos$porcentagem[motivos$motivo_deixou_trab_saude == "Diabetes"],
    motivos$porcentagem[motivos$motivo_deixou_trab_saude == "Câncer"],
    motivos$porcentagem[motivos$motivo_deixou_trab_saude == "Problemas neurológicos"],
    motivos$porcentagem[motivos$motivo_deixou_trab_saude == "Saúde mental"],
    motivos$porcentagem[motivos$motivo_deixou_trab_saude == "Lesões ou fraturas por acidentes ou violência"],
    motivos$porcentagem[motivos$motivo_deixou_trab_saude == "Outro problema de saúde"]
  )
)

# Usar o stargazer para criar uma tabela
stargazer(estatisticas, type = "text", summary = FALSE, rownames = FALSE, 
          title = "Principais Motivos de Saúde para Ausência em Atividades Diárias")

stargazer(estatisticas, type = "latex", summary = FALSE, rownames = FALSE, 
          title = "Principais Motivos de Saúde para Ausência em Atividades Diárias")

##TABELA 2 -----------------------------------------------------------------

freq <- table(addNA(PNS_sample$rel_trab_deixou_trab_saude))
freq

relacio_trabalho <- PNS_sample %>%
  count(rel_trab_deixou_trab_saude) %>%
  mutate(porcentagem = (n / total_chefes_doentes) * 100)


relacio_trabalho$rel_trab_deixou_trab_saude <- recode(relacio_trabalho$rel_trab_deixou_trab_saude,
                                           `1` = "Sim",
                                           `2` = "Não",
                                           `3` = "Não trabalhava/não trabalhou")


print(relacio_trabalho)

# Filtrar os dados para incluir apenas os casos onde 
#"rel_trab_deixou_trab_saude" e "acamado" são "Sim"
rel_trab_sim <- PNS_sample %>%
  filter(rel_trab_deixou_trab_saude == 1) %>%
  summarise(porcentagem = (n() / total_chefes_doentes) * 100) %>%
  mutate(Categoria = "O motivo de saúde estava relacionado ao trabalho (%)")

acamado_sim <- PNS_sample %>%
  filter(acamado == "sim") %>%
  summarise(porcentagem = (n() / total_chefes_doentes) * 100) %>%
  mutate(Categoria = "Esteve acamado recentemente")


doença_sim <- PNS_sample %>%
  filter(tem_doenca== "sim") %>%
  summarise(porcentagem = (n() / total_chefes_doentes) * 100) %>%
  mutate(Categoria = "Recebeu diagnóstico de alguma doença crônica, física ou mental, ou longa duração")

# Combine as tabelas em uma única tabela
tabela_final <- bind_rows(rel_trab_sim, acamado_sim, doença_sim) %>%
  select(Categoria, porcentagem)

# Exibir a tabela final
print(tabela_final)

stargazer(tabela_final, type = "latex", summary = FALSE, rownames = FALSE, 
          title = "Estatisticas da amostra ")


freq <- table(addNA(PNS_sample$acamado))
freq

freq <- table(addNA(PNS_sample$tem_doenca))
freq

freq <- table(addNA(PNS_sample$dias_deixou_trab_saude))
freq

avg <- mean(PNS_sample$dias_deixou_trab_saude, na.rm = TRUE)
avg

freq <- table(addNA(PNS_sample$dias_acamado))
freq

avg <- mean(PNS_sample$dias_acamado, na.rm = TRUE)
avg

#Mantem as pessoas que deixaram de trabalhar por motivo relacionado ao trabalho 
PNS_sample_2 <- PNS_sample %>% 
  filter(rel_trab_deixou_trab_saude == '1')

freq <- table(addNA(PNS_sample_2$motivo_deixou_trab_saude))
freq

freq <- table(addNA(PNS_sample_2$rel_trab_deixou_trab_saude))
freq

freq <- table(addNA(PNS_sample_2$dias_deixou_trab_saude))
freq

freq <- table(addNA(PNS_sample_2$acamado))
freq

freq <- table(addNA(PNS_sample_2$dias_acamado))
freq

freq <- table(addNA(PNS_sample_2$tem_doenca))
freq

freq <- table(addNA(PNS_sample_2$sit_saude))
freq

freq <- table(addNA(PNS_sample_2$sit_saude_2))
freq

#Analise por domicilio usando a var deixou de trabalhar por doença -----------------------------------------------------


# Filtrar para domicílios onde o chefe (sexo masculino) deixou de trabalhar por problemas de saúde
chefe_doente <- PNS_2019 %>%
  filter(cond_dom == 1 & sexo == 1 & deixou_trab_saude == "sim") %>%
  select(iddom)

# Contar o número total de chefes do sexo masculino
total_chefes <- PNS_2019 %>%
  filter(cond_dom == 1 & sexo == 1) %>%
  nrow()

# Contar o número de chefes do sexo masculino que deixaram de trabalhar por problemas de saúde
total_chefes_doentes <- chefe_doente %>%
  nrow()

# Calcular a porcentagem de chefes que deixaram de trabalhar por problemas de saúde
porcentagem_chefes_doentes <- (total_chefes_doentes / total_chefes) * 100

# Contar o número total de cônjuges do sexo feminino na base
total_conjuges_base <- PNS_2019 %>%
  filter(cond_dom == 2 & sexo == 2) %>%
  nrow()

# Filtrar para cônjuges do sexo feminino que residem nos domicílios dos chefes doentes
conjuges <- PNS_2019 %>%
  filter(iddom %in% chefe_doente$iddom & cond_dom == 2 & sexo == 2)

# Contar o número de cônjuges na amostra dos chefes doentes
total_conjuges_amostra <- conjuges %>% nrow()

# Filtrar para cônjuges do sexo feminino que auxiliaram em algo
conjuges_auxiliaram <- conjuges %>%
  filter(aux_cuidado_pess == "sim" | aux_companhia == "sim" | aux_outros == "sim")

# Contar o número de cônjuges que auxiliaram em algo
total_conjuges_auxiliaram <- conjuges_auxiliaram %>% nrow()

# Calcular a porcentagem de cônjuges que auxiliaram em algo
porcentagem_auxiliaram <- (total_conjuges_auxiliaram / total_conjuges_amostra) * 100

# Filtrar para cônjuges que trabalhavam e auxiliaram em algo
conjuges_auxiliaram_trabalhavam <- conjuges_auxiliaram %>%
  filter(trabalhou == "sim")

# Contar o número de cônjuges que auxiliaram e trabalhavam
total_auxiliaram_trabalhavam <- conjuges_auxiliaram_trabalhavam %>% nrow()

# Calcular a porcentagem de cônjuges que auxiliaram e trabalhavam
porcentagem_auxiliaram_trabalhavam <- (total_auxiliaram_trabalhavam / total_conjuges_auxiliaram) * 100

# Criar um data frame com as estatísticas
estatisticas <- data.frame(
  Estatística = c("Total de Chefes (Sexo Masculino)", "Total de Chefes (Sexo Masculino) que Deixaram de Trabalhar por Saúde", 
                  "Porcentagem de Chefes (Sexo Masculino) que Deixaram de Trabalhar por Saúde",
                  "Total de Cônjuges na Base", "Total de Cônjuges na Amostra dos Chefes doentes", 
                  "Total de Cônjuges que Auxiliaram", 
                  "Porcentagem de Cônjuges que Auxiliaram", 
                  "Total de Cônjuges que Auxiliaram e Trabalham", 
                  "Porcentagem de Cônjuges que Auxiliaram e Trabalham"),
  Valor = c(total_chefes, total_chefes_doentes, 
            porcentagem_chefes_doentes, total_conjuges_base, 
            total_conjuges_amostra, total_conjuges_auxiliaram, 
            porcentagem_auxiliaram, total_auxiliaram_trabalhavam, 
            porcentagem_auxiliaram_trabalhavam)
)


# Usar o stargazer para criar uma tabela
stargazer(estatisticas, type = "text", summary = FALSE, rownames = FALSE, title = "Estatísticas Descritivas")

# Usar o stargazer para criar uma tabela em LaTeX
stargazer(estatisticas, type = "latex", summary = FALSE, rownames = FALSE, title = "Estatísticas Descritivas")



#Analise por domicilio usando a var AFASTAMENTO -----------------------------------------------------

library(dplyr)
library(stargazer)

# Filtrar para domicílios onde o chefe (sexo masculino) deixou de trabalhar por problemas de saúde
chefe_doente <- PNS_2019 %>%
  filter(cond_dom == 1 & sexo == 1 & afast == 1 & motivo_afast ==3 | motivo_afast == 5 ) %>%
  select(iddom)

# Contar o número total de chefes do sexo masculino
total_chefes <- PNS_2019 %>%
  filter(cond_dom == 1 & sexo == 1) %>%
  nrow()

# Contar o número de chefes do sexo masculino que deixaram de trabalhar por problemas de saúde
total_chefes_doentes <- chefe_doente %>%
  nrow()

# Calcular a porcentagem de chefes que deixaram de trabalhar por problemas de saúde
porcentagem_chefes_doentes <- (total_chefes_doentes / total_chefes) * 100

# Contar o número total de cônjuges do sexo feminino na base
total_conjuges_base <- PNS_2019 %>%
  filter(cond_dom == 2 & sexo == 2) %>%
  nrow()

# Filtrar para cônjuges do sexo feminino que residem nos domicílios dos chefes doentes
conjuges <- PNS_2019 %>%
  filter(iddom %in% chefe_doente$iddom & cond_dom == 2 & sexo == 2)

# Contar o número de cônjuges na amostra dos chefes doentes
total_conjuges_amostra <- conjuges %>% nrow()

# Filtrar para cônjuges do sexo feminino que auxiliaram em algo
conjuges_auxiliaram <- conjuges %>%
  filter(aux_cuidado_pess == "sim" | aux_companhia == "sim" | aux_outros == "sim")

# Contar o número de cônjuges que auxiliaram em algo
total_conjuges_auxiliaram <- conjuges_auxiliaram %>% nrow()

# Calcular a porcentagem de cônjuges que auxiliaram em algo
porcentagem_auxiliaram <- (total_conjuges_auxiliaram / total_conjuges_amostra) * 100

# Filtrar para cônjuges que trabalhavam e auxiliaram em algo
conjuges_auxiliaram_trabalhavam <- conjuges_auxiliaram %>%
  filter(trabalhou == "sim")

# Contar o número de cônjuges que auxiliaram e trabalhavam
total_auxiliaram_trabalhavam <- conjuges_auxiliaram_trabalhavam %>% nrow()

# Calcular a porcentagem de cônjuges que auxiliaram e trabalhavam
porcentagem_auxiliaram_trabalhavam <- (total_auxiliaram_trabalhavam / total_conjuges_auxiliaram) * 100

# Criar um data frame com as estatísticas
estatisticas <- data.frame(
  Estatística = c("Total de Chefes (Sexo Masculino)", "Total de Chefes (Sexo Masculino) que Deixaram de Trabalhar por Saúde", 
                  "Porcentagem de Chefes (Sexo Masculino) Afastados por choque de saúde",
                  "Total de Cônjuges na Base", "Total de Cônjuges na Amostra dos Chefes Afastados", 
                  "Total de Cônjuges que Auxiliaram", 
                  "Porcentagem de Cônjuges que Auxiliaram", 
                  "Total de Cônjuges que Auxiliaram e Trabalham", 
                  "Porcentagem de Cônjuges que Auxiliaram e Trabalham"),
  Valor = c(total_chefes, total_chefes_doentes, 
            porcentagem_chefes_doentes, total_conjuges_base, 
            total_conjuges_amostra, total_conjuges_auxiliaram, 
            porcentagem_auxiliaram, total_auxiliaram_trabalhavam, 
            porcentagem_auxiliaram_trabalhavam)
)


# Usar o stargazer para criar uma tabela
stargazer(estatisticas, type = "text", summary = FALSE, rownames = FALSE, title = "Estatísticas Descritivas")

# Usar o stargazer para criar uma tabela em LaTeX
stargazer(estatisticas, type = "latex", summary = FALSE, rownames = FALSE, title = "Estatísticas Descritivas")
