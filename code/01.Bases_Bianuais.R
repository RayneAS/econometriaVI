setwd("C:/Users/thiag/OneDrive/Mestrado FEA-USP/Eletivas/Econometria V/Artigo/Dados")
memory.limit(9999999999999)

#Carregando os pacotes necessários
library(foreign)
library(dplyr)
library(tidyr)
library(PNADcIBGE)
library(srvyr)

#Anos
Anos <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)

#Vari?veis a serem utilizadas
vars <- c("Ano", "Trimestre", "UF", "Capital", "RM_RIDE", "UPA", "Estrato", "V1008", "V1014",
          "V1016", "V1022", "V1023", "V1027", "V1028", "V1029", "posest", "V2001", "V2003",
          "V2005", "V2007", "V2008", "V20081", "V20082", "V2009", "V2010", "V3007", "V3008",
          "V3009", "V3009A", "V4001", "V4002", "V4003", "V4004", "V4005", "V4006", "V4006A",
          "V4007", "V4008", "V40081", "V40082", "V40083", "V4009", "V4010", "V4012", "V40121",
          "V4013", "V40132", "V40132A", "V4014", "V4025", "V4028", "V4029", "V4032", "V4033",
          "V40331", "V403311", "V403312", "V40332", "V403321", "V403322", "V40333", "V403331",
          "V4034", "V40341", "V403411", "V403412", "V40342", "V403421", "V403422", "V4039",
          "V4039C", "V4040", "V40401", "V40402", "V40403", "V4041", "V4043", "V40431", "V4044",
          "V4045", "V4046", "V4047", "V4048", "V4049", "V4050", "V40501", "V405011", "V405012",
          "V40502", "V405021", "V405022", "V40503", "V405031", "V4051", "V40511", "V405111",
          "V405112", "V40512", "V405121", "V405122", "V4056", "V4056C", "V4057", "V4058", "V40581",
          "V405811", "V405812", "V40582", "V405821", "V405822", "V40583", "V405831", "V40584",
          "V4059", "V40591", "V405911", "V405912", "V40592", "V405921", "V405922", "V4062",
          "V4062C", "V4063", "V4063A", "V4064", "V4064A", "V4071", "V4072", "V4072A", "V4073",
          "V4074", "V4074A", "V4076", "V40761", "V40762", "V40763", "V4077", "V4078", "V4078A",
          "V4082", "VD3004", "VD3005", "VD4001", "VD4002", "VD4003", "VD4004", "VD4004A", "VD4005",
          "VD4007", "VD4008", "VD4009", "VD4010", "VD4011", "VD4012", "VD4013", "VD4014", "VD4015",
          "VD4016", "VD4017", "VD4018", "VD4019", "VD4020", "VD4023", "VD4030", "VD4031", "VD4032",
          "VD4033", "VD4034", "VD4035", "VD4036", "VD4037")

#Baixa a base, transforma em bianual e faz as limpezas necess?rias

for(i in 1:length(Anos)) {
  
  Ano_t <- paste0(as.character(Anos[i]))
  
  Ano_t1 <- paste0(as.character(Anos[i]+1))
  
  #Carregando a base do Ano t
  base_t_01 <- read_pnadc(microdata = paste0("PNADC_01", Ano_t, ".txt"),
                             input_txt = paste0("Input_PNADC_trimestral.txt"))
  
  base_t_02 <- read_pnadc(microdata = paste0("PNADC_02", Ano_t, ".txt"),
                          input_txt = paste0("Input_PNADC_trimestral.txt"))
  
  base_t_03 <- read_pnadc(microdata = paste0("PNADC_03", Ano_t, ".txt"),
                          input_txt = paste0("Input_PNADC_trimestral.txt"))
  
  base_t_04 <- read_pnadc(microdata = paste0("PNADC_04", Ano_t, ".txt"),
                          input_txt = paste0("Input_PNADC_trimestral.txt"))
  
  base_t <- rbind(base_t_01, base_t_02, base_t_03, base_t_04)
  
  rm(base_t_01, base_t_02, base_t_03, base_t_04)
    
  base_t <- base_t %>%
    select(all_of(vars))
  
  base_t <- data.frame(lapply(base_t, function(x) as.numeric(as.character(x))))
  
  base_t$iddom <- paste(base_t$UPA, base_t$V1008, base_t$V1014, sep="")
  
  base_t$idind <- paste(base_t$iddom, base_t$V2007, base_t$V2008, base_t$V20081, base_t$V20082, sep="")
  
  #Carregando a base do Ano t+1
  base_t1_01 <- read_pnadc(microdata = paste0("PNADC_01", Ano_t1, ".txt"),
                          input_txt = paste0("Input_PNADC_trimestral.txt"))
  
  base_t1_02 <- read_pnadc(microdata = paste0("PNADC_02", Ano_t1, ".txt"),
                          input_txt = paste0("Input_PNADC_trimestral.txt"))
  
  base_t1_03 <- read_pnadc(microdata = paste0("PNADC_03", Ano_t1, ".txt"),
                          input_txt = paste0("Input_PNADC_trimestral.txt"))
  
  base_t1_04 <- read_pnadc(microdata = paste0("PNADC_04", Ano_t1, ".txt"),
                          input_txt = paste0("Input_PNADC_trimestral.txt"))
  
  base_t1 <- rbind(base_t1_01, base_t1_02, base_t1_03, base_t1_04)
  
  rm(base_t1_01, base_t1_02, base_t1_03, base_t1_04)
  
  base_t1 <- base_t1 %>%
    select(all_of(vars))
  
  base_t1 <- data.frame(lapply(base_t1, function(x) as.numeric(as.character(x))))
  
  base_t1$iddom <- paste(base_t1$UPA, base_t1$V1008, base_t1$V1014, sep="")
  
  base_t1$idind <- paste(base_t1$iddom, base_t1$V2007, base_t1$V2008, base_t1$V20081, base_t1$V20082, sep="")
  
  #Faz as limpezas necess?rias
    
  ##ANO t
    #Remove a 5? entrevista da base do ano t
  base_t_2 <- base_t %>%
    filter(!(V1016 == 5 & Ano == Ano_t))
  
    #Remove a 4? entrevista da base do ano t para os trimestres <4
  base_t_2 <- base_t_2 %>%
    filter(!(V1016 == 4 & Trimestre <4 & Ano == Ano_t))
  
    #Remove a 3? entrevista da base do ano t para os trimestres <3
  base_t_2 <- base_t_2 %>%
    filter(!(V1016 == 3 & Trimestre <3 & Ano == Ano_t))
  
    #Remove a 2? entrevista da base do ano t para os trimestres <2
  base_t_2 <- base_t_2 %>%
    filter(!(V1016 == 2 & Trimestre <2 & Ano == Ano_t))
    
  rm(base_t)
  
  ##ANO t+1
   #Remove a 1? entrevista da base do ano t+1
  base_t1_2 <- base_t1 %>%
    filter(!(V1016 == 1 & Trimestre == 1 & Ano == Ano_t1))
  
    #Remove a 2? entrevista da base do ano t+1
  base_t1_2 <- base_t1_2 %>%
    filter(!(V1016 == 2 & Trimestre == 2 & Ano == Ano_t1))
  
    #Remove a 3? entrevista da base do ano t+1
  base_t1_2 <- base_t1_2 %>%
    filter(!(V1016 == 3 & Trimestre == 3 & Ano == Ano_t1))
  
    #Remove a 4? entrevista da base do ano t+1
  base_t1_2 <- base_t1_2 %>%
    filter(!(V1016 == 4 & Trimestre == 4 & Ano == Ano_t1))
  
    #Remove a 1? entrevista da base do ano t+1 no 2? trimestre
  base_t1_2 <- base_t1_2 %>%
    filter(!(V1016 == 1 & Trimestre == 2 & Ano == Ano_t1))
  
    #Remove a 2? entrevista da base do ano t+1 no 3? trimestre
  base_t1_2 <- base_t1_2 %>%
    filter(!(V1016 == 2 & Trimestre == 3 & Ano == Ano_t1))
  
    #Remove a 3? entrevista da base do ano t+1 no 4? trimestre
  base_t1_2 <- base_t1_2 %>%
    filter(!(V1016 == 3 & Trimestre == 4 & Ano == Ano_t1))
  
   #Remove a 1? entrevista da base do ano t+1 no 3? trimestre
  base_t1_2 <- base_t1_2 %>%
    filter(!(V1016 == 1 & Trimestre == 3 & Ano == Ano_t1))
  
    #Remove a 2? entrevista da base do ano t+1 no 4? trimestre
  base_t1_2 <- base_t1_2 %>%
    filter(!(V1016 == 2 & Trimestre == 4 & Ano == Ano_t1))
  
   #Remove a 1? entrevista da base do ano t+1 no 4? trimestre
  base_t1_2 <- base_t1_2 %>%
    filter(!(V1016 == 1 & Trimestre == 4 & Ano == Ano_t1))
  
  rm(base_t1)
  
  #Une as bases
  base_unida <- rbind(base_t_2, base_t1_2)
  
  #Faz as limpezas necess?rias
  
  ##Remove os indiv?duos com c?digo identificador duplicado em um mesmo ponto do tempo
  base_limpa <- base_unida %>%
    group_by(V1016, idind) %>%
    filter(n()==1) %>%
    ungroup()

  #Remove os indiv?duos que n?o declararam dia, m?s e/ou ano de nascimento
  base_limpa <- base_limpa %>%
    filter(V2008 != 99 | V20081 != 99 | V20082 != 9999)

  #Remove as fam?lias com mais de um chefe
  base_limpa <- base_limpa %>%
    mutate(chefe = ifelse(V2005==1, 1, 0)) %>%
    group_by(iddom, V1016) %>%
    mutate(qt_chefe = sum(chefe)) %>%
    ungroup() %>%
    filter(qt_chefe == 1) %>%
    select(-chefe, -qt_chefe)
 
  #Remove os indiv?duos que tem mais de 5 observa??es na base
  base_limpa <- base_limpa %>%
    group_by(idind) %>%
    mutate(qt_obs = n()) %>%
    ungroup() %>%
    filter(qt_obs <= 5) %>%
    select(-qt_obs)
  
  #Remove indiv?duos que n?o contribuem para a renda domiciliar
  #(AGREGADOS, PENSIONISTAS, EMPREGADO(A) DOM?STICO(A), 
  #PARENTE DO(A) EMPREGADO(A) DOM?STICO(A))
  base_limpa <- base_limpa %>%
    filter(!(V2005 %in% c(15, 17, 18, 19)))
  
  #Remove as fam?lias que realizaram somente uma entrevista
  base_limpa <- base_limpa %>%
    group_by(idind) %>%
    mutate(num_ent = ifelse(V2005==1,sum(V1016),0)) %>%
    ungroup() %>% 
    group_by(iddom) %>%
    mutate(num_ent_max = max(num_ent)) %>%
    ungroup() %>%
    filter(num_ent_max != 1) %>%
    select(-num_ent, -num_ent_max)
  
  #Somente indivíduos em idade ativa
  base_limpa <- base_limpa %>%
    filter(V2009 >= 15 & V2009 <= 65)
      
  #Somente famílias com chefes homens
  base_limpa <- base_limpa %>%
    group_by(idind, V1016) %>%
    mutate(chefe_homem = ifelse(V2005 == 1 & V2007 == 1, 1, 0)) %>%
    ungroup() %>% 
    group_by(iddom) %>%
    mutate(max_chefe_homem = max(chefe_homem)) %>%
    ungroup() %>%
    filter(max_chefe_homem == 1) %>%
    select(-chefe_homem, -max_chefe_homem)

  #Somente famílias em que há a presença de um cônjuge
  base_limpa <- base_limpa %>%
    group_by(idind, V1016) %>%
    mutate(conjuge = ifelse((V2005 == 2 | V2005 == 3), 1, 0)) %>%
    ungroup() %>% 
    group_by(iddom) %>%
    mutate(max_conjuge = max(conjuge)) %>%
    ungroup() %>%
    filter(max_conjuge == 1) %>%
    select(-conjuge, -max_conjuge)
  
  #Salva a base sem limpezas em RDS
  saveRDS(base_unida, paste0("PNADC_Bianual_SL_", Ano_t,".RDS"))
  
  #Salva a base com limpezas em RDS
  saveRDS(base_limpa, paste0("PNADC_Bianual_L_", Ano_t,".RDS"))
  
  rm(base_limpa, base_unida, base_t, base_t_2, base_t1, base_t1_2)

}



