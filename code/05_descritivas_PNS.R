## Objetivo Executar a consulta SQL da Pesquisa Nacional de Saúde --------------
#E obter algumas estatisticas descritivas sobre afastamento 

rm(list = ls())
gc()

#PACKAGES USED
library(basedosdados)
library(tidyverse)
library(lubridate)

#Billing ID in Google Big Query
set_billing_id("stately-furnace-418521")

# # Executar a consulta SQL CNPJ socios ----------------------------------------
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
              'tempo_internado_meses', 'tempo_internado_dias), as.numeric)


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

