
library(foreign)
library(dplyr)
library(openxlsx)

arquivo <- paste0(data_dir, "/Base_final_completa.dta")
data <- read.dta(arquivo)

# Deflacionando a renda

arquivo <- paste0(data_dir, "/ipca.xlsx")

ipca <- read.xlsx(arquivo)

data <- data %>%
    left_join(ipca, by = c("Trimestre", "Ano")) %>%
    mutate(renda_deflac = (VD4020/index)*100)

# Criando novas variáveis

  # Choque de saúde
    
    # Remunerado por instituto de previdência
    data <- data %>%
      mutate(V4006 = ifelse(is.na(V4006), 0, V4006),
             V4006A = ifelse(is.na(V4006A), 0, V4006A)) %>%
      mutate(motivos = V4006 + V4006A) %>%
      group_by(V1016, idind, Ano, Trimestre) %>%
      mutate(saude = ifelse(motivos == 3, 1, 0)) %>%
      mutate(choque = ifelse(V2005 == 1 & saude == 1, 1, 0)) %>%
      ungroup() %>%
      group_by(V1016, iddom) %>%
      mutate(choque_max_remun = max(choque)) %>%
      select(-saude, -choque)
  
    # Não remunerado por instituto de previdência
    data <- data %>%
      group_by(V1016, idind, Ano, Trimestre) %>%
      mutate(saude = ifelse(motivos == 5, 1, 0)) %>%
      mutate(choque = ifelse(V2005 == 1 & saude == 1, 1, 0)) %>%
      ungroup() %>%
      group_by(V1016, iddom) %>%
      mutate(choque_max_nremun = max(choque)) %>%
      select(-saude, -choque)

    # Total
    data <- data %>%
      group_by(V1016, idind, Ano, Trimestre) %>%
      mutate(saude = ifelse(motivos %in% c(3,5), 1, 0)) %>%
      mutate(choque = ifelse(V2005 == 1 & saude == 1, 1, 0)) %>%
      ungroup() %>%
      group_by(V1016, iddom) %>%
      mutate(choque_max_total = max(choque)) %>%
      select(-saude, -choque)

  # Rural
  data <- data %>%
    mutate(rural = ifelse(V1022 == 2, 1, 0))

  # Região metropolitana
  data <- data %>%
    mutate(metrop = ifelse(V1023 %in% c(1,2), 1, 0))

  # Dummies presença de outras mulheres e outros homens no domicílio
  
    # Presença mulher
    data <- data %>%
      group_by(V1016, idind) %>%
      mutate(mulher_nao_conjuge = ifelse(!(V2005 %in% c(1, 2, 3)) & V2007 == 2, 1, 0)) %>%
      ungroup() %>%
      group_by(V1016, iddom) %>%
      mutate(presenca_mulher = max(mulher_nao_conjuge)) %>%
      ungroup()

    # Presença homem
    data <- data %>%
      group_by(V1016, idind) %>%
      mutate(homem_nao_conjuge = ifelse(!(V2005 %in% c(1, 2, 3)) & V2007 == 1, 1, 0)) %>%
      ungroup() %>%
      group_by(V1016, iddom) %>%
      mutate(presenca_homem = max(homem_nao_conjuge)) %>%
      ungroup()

  # Informal
    data <- data %>%
      group_by(V1016, idind, Ano, Trimestre) %>%
      mutate(informal = ifelse(V4029 == 2 & V4032 == 2, 1, 0)) %>%
      ungroup()

    # Mantendo somente famílias que realizaram as 5 entrevistas
    data <- data %>%
      group_by(idind) %>%
      mutate(num_ent = ifelse(V2005==1,sum(V1016),0)) %>%
      ungroup() %>%
      group_by(iddom) %>%
      mutate(num_ent_max = max(num_ent)) %>%
      ungroup() %>%
      filter(num_ent_max == 15) %>%
      select(-num_ent, -num_ent_max) 
    
    # Mantendo somente famílias cujo chefe trabalhava na primeira entrevista
    data <- data %>%
      group_by(idind, V1016) %>%
      mutate(trabalha = ifelse(V2005 == 1 & V4001 == 1 & V1016 ==1, 1, 0)) %>%
      ungroup() %>%
      group_by(iddom) %>%
      mutate(trab_max = max(trabalha)) %>%
      ungroup() %>%
      filter(trab_max == 1) %>%
      select(-trabalha, -trab_max)

    # Mantendo somente famílias cujo chefe trabalhava na segunda entrevista
    data <- data %>%
      group_by(idind, V1016) %>%
      mutate(trabalha = ifelse(V2005 == 1 & V4001 == 1 & V1016 ==2, 1, 0)) %>%
      ungroup() %>%
      group_by(iddom) %>%
      mutate(trab_max = max(trabalha)) %>%
      ungroup() %>%
      filter(trab_max == 1) %>%
      select(-trabalha, -trab_max)
    
    # # Mantendo somente famílias cujo chefe trabalhava na quarta entrevista
    # data <- data %>%
    #   group_by(idind, V1016) %>%
    #   mutate(trabalha = ifelse(V2005 == 1 & V4001 == 1 & V1016 ==4, 1, 0)) %>%
    #   ungroup() %>%
    #   group_by(iddom) %>%
    #   mutate(trab_max = max(trabalha)) %>%
    #   ungroup() %>%
    #   filter(trab_max == 1) %>%
    #   select(-trabalha, -trab_max)
    # 
    # 
    # # Mantendo somente famílias cujo chefe trabalhava na quinta entrevista
    # data <- data %>%
    #   group_by(idind, V1016) %>%
    #   mutate(trabalha = ifelse(V2005 == 1 & V4001 == 1 & V1016 ==5, 1, 0)) %>%
    #   ungroup() %>%
    #   group_by(iddom) %>%
    #   mutate(trab_max = max(trabalha)) %>%
    #   ungroup() %>%
    #   filter(trab_max == 1) %>%
    #   select(-trabalha, -trab_max)

# Salvando a base de dados

    # Chefe
    
chefe <- data %>% filter(V2005 == 1)
  
write.dta(chefe, file = "D:/rayne/Documents/dados_econometria_VI/Base_final_chefe_todas_entrev_3.dta")
    
    

    # Cônjuge
    
conjuge <- data %>% filter(V2005 %in% c(2,3))
    
write.dta(conjuge, file = "D:/rayne/Documents/dados_econometria_VI/Base_final_conjuge_todas_entrev_3.dta")
    
  