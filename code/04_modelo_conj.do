// Define o caminho para a pasta de dados
global data_folder "D:/rayne/Documents/dados_econometria_VI"

*keep if Ano == 2012 | Ano == 2013

log using "D:/rayne/Documents/dados_econometria_VI/model_conj.log", replace

// Abrir a base de dados 
*use "${data_folder}/Base_final.dta", clear

use "${data_folder}/Base_final_conjuge.dta", clear

ren VD4020 renda
ren V403312 renda_2 
ren VD4035 horas_trab_t
ren V4039C horas_trab_pr 
ren V1023 tipo_area 
ren V1016 num_entrev 
ren Ano ano
ren Trimestre tri
ren V2009 idade 
ren V2010 cor
*ren VD3006 educ
ren VD4010 grup_ativ

*trabalhadores em idade entre 50-60 sao mais frequentes no choque
sum idade if choque_max==1
sum idade if choque_max==0
tab grup_ativ

sum renda_deflac if choque_max==1 [aw=V1028]
sum renda_deflac if choque_max==0 [aw=V1028]


sum horas_trab_t if choque_max==1 [aw=V1028]
sum horas_trab_t if choque_max==0 [aw=V1028]



*torna indid numeric
destring idind, replace
destring iddom, replace


*Definindo o formato painel:
xtset idind 


*FE  
xtreg renda_deflac choque_max, fe vce(robust)
estimates store FE

xtreg renda_deflac choque_max i.ano, fe vce(robust)
estimates store FE

xtreg renda_deflac choque_max i.ano i.tri, fe vce(robust)
estimates store FE

xtreg renda_deflac choque_max rural i.grup_ativ i.ano i.tri, fe vce(robust)
estimates store FE

xtreg horas_trab_t choque_max i.grup_ativ i.ano i.tri, fe vce(robust)
estimates store FE

*xtreg renda_deflac choque i.grup_ativ i.cor i.ano i.tri, fe vce(robust)
*estimates store FE

log close

