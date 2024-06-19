
# dt_pnad <- read.dta("Base_final.dta")
# 
# write.csv(dt_pnad, file = "D:/rayne/Documents/dados_econometria_VI/Base_final.csv",
#           row.names = FALSE)

arquivo <- paste0(data_dir, "/Base_final.csv")
dt_pnad <- fread(arquivo, colClasses = 'character')
colnames(dt_pnad)

dt_pnad <-dt_pnad %>% 
  rename(renda = V403312)

dt_pnad <-dt_pnad %>% 
  rename(horas_trab_pr = V4039C)


dt_pnad <-dt_pnad %>% 
  rename(tipo_area = V1023)

dt_pnad <-dt_pnad %>% 
  rename(cod_ativ = V4044)




#Dummmy variable
# dummy_var <- lm (renda ~ choque + factor(idind) + factor(UF), data = dt_pnad)
# summary(dummy_var)
install.packages("lmtest")
library(lmtest)
library(plm)
#Within estimator
within <- plm(renda ~ choque_max,
               model="within", data = dt_pnad)

summary(within )

coeftest(within, vcov. = vcovHC, type = "HC1")

within_1 <- plm(renda ~ choque_max, index =  c("idind","Trimestre","Ano"),
              model="within", data = dt_pnad)

summary(within_1)

within_2 <- plm(renda ~ choque_max, index =  c("UF", "idind"),
                model="within", data = dt_pnad)

summary(within_2)

within_3 <- plm(renda ~ choque_max, index =  c("UF", "idind","tipo_area"),
                model="within", data = dt_pnad)

summary(within_3)

within_4 <- plm(renda ~ choque_max, index =  c("UF", "idind","tipo_area","cod_ativ"),
                model="within", data = dt_pnad)

summary(within_4)


within_5 <- plm(renda ~ choque_max, index =  c("UF", "idind","tipo_area","cod_ativ","Trimestre", "Ano"),
                model="within", data = dt_pnad)

summary(within_5)


within_horas <- plm(horas_trab_pr ~ choque_max, index = "UF", 
              model="within", data = dt_pnad)

summary(within_horas)

library(fixest)