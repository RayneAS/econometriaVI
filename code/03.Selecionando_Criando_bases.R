
data <- read.dta("Base_final_completa.dta")

# Selecionando variáveis de interesse

vars <- c("Ano", "Trimestre", "UF", "iddom", "idind", "V1016", "V1022", "V1023",
         "V1028", "V2001", "V2005", "V2007", "V2008", "V20081",
         "V20082", "V2009", "V2010", "V4001", "V4002", "V4003",
         "V4004", "V4005", "V4006", "V4006A", "V4007", "V4008",
         "V40081", "V40082", "V40083", "V4009", "V4010", "V4012",
         "V40121", "V4013", "V40132", "V40132A", "V4014",
         "V4025", "V4028", "V4029", "V4032", "V4033",
         "V40331", "V403311", "V403312", "V40332", "V403321", "V403322",
         "V40333", "V403331", "V4034", "V40341", "V403411", "V403412",
         "V40342", "V403421", "V403422", "V4039", "V4039C", "V4040",
         "V40401", "V40402", "V40403", "V4041", "V4043", "V40431",
         "V4044", "V4045", "V4046", "V4047", "V4048", "V4049",
         "V4050", "V40501", "V405011", "V405012", "V40502", "V405021",
         "V405022", "V40503", "V405031", "V4051", "V40511", "V405111",
         "V405112", "V40512", "V405121", "V405122", "V4056", "V4056C",
         "V4057", "V4058", "V40581", "V405811", "V405812", "V40582",
         "V405821", "V405822", "V40583", "V405831", "V40584", "V4059",
         "V40591", "V405911", "V405912", "V40592", "V405921", "V405922",
         "V4062", "V4062C", "V4063", "V4063A", "V4064", "V4064A", "V4071",
         "V4072", "V4072A", "V4073", "V4074", "V4074A",
         "V4076", "V40761", "V40762", "V40763", "V4077", "V4078", "V4078A", "V4082")

data <- data %>% 
  select(all_of(vars))

# Criando novas variáveis


#V4006: Na semana de ... a .... (semana de referência), por que motivo ... 
#estava afastado desse trabalho?
  
#V4006A: Na semana de ... a .... (semana de referência), por que motivo ... 
#estava afastado desse trabalho?
#V4006A: 5 Afastamento do próprio negócio/empresa por motivo de gestação, doença, acidente, etc., 
#sem ser remunerado por instituto de previdência

#V1016: numero de entrevista do domicilio
#V2005: condicao no domicilio (igual a 1 é o chefe)

# Choque de saúde
  data <- data %>%
    mutate(V4006 = ifelse(is.na(V4006), 0, V4006),
           V4006A = ifelse(is.na(V4006A), 0, V4006A)) %>%
    mutate(motivos = V4006 + V4006A) %>%
    group_by(V1016, idind, Ano, Trimestre) %>%
    mutate(saude = ifelse(motivos == 5, 1, 0)) %>%
    mutate(choque = ifelse(V2005 == 1 & saude == 1, 1, 0)) %>%
    ungroup() %>%
    group_by(V1016, iddom) %>%
    mutate(choque_max = max(choque))
  
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

# Salvando a base de dados
# write.dta(data, "Base_final.dta")
    
write.csv(data, file = "D:/rayne/Documents/dados_econometria_VI/Base_final.csv",
          row.names = FALSE)
