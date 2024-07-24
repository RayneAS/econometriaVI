// Define o caminho para a pasta de dados
global data_folder "D:/rayne/Documents/dados_econometria_VI"

*log using "D:/rayne/Documents/dados_econometria_VI/model_chefe.log", replace

// Abrir a base de dados 
use "${data_folder}/Base_final_chefe.dta", clear

*NOSSA AMOSTRA DE INTERESSE ESTÁ NOS CENTROS URBANOS
*keep if rural ==0

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
ren VD3005 educ
ren VD4010 grup_ativ
ren V4008 temp_afast
ren V2007 sexo
ren V2001 num_pes_dom

tab num_entrev
describe

*630,603 observações

// Abrir a base de dados 
use "${data_folder}/Base_final_chefe_todas_entrev.dta", clear

describe

*NOSSA AMOSTRA DE INTERESSE ESTÁ NOS CENTROS URBANOS
*keep if rural ==0

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
ren VD3005 educ
ren VD4010 grup_ativ
ren V4008 temp_afast
ren V2007 sexo
ren V2001 num_pes_dom
ren V4001 trabalhou

tab num_entrev

tab temp_afast if num_entrev==1
tab temp_afast if num_entrev==2
tab temp_afast if num_entrev==3
tab temp_afast if num_entrev==4
tab temp_afast if num_entrev==5

tab V2005

gen trab_entrev_1 = 0
replace trab_entrev_1 =1 if trabalhou ==1 & num_entrev ==1

tab trab_entrev_1

gen trab_entrev_2 = 0
replace trab_entrev_2 =1 if trabalhou ==1 & num_entrev ==2

gen trab_entrev_3 = 0
replace trab_entrev_3 =1 if trabalhou ==1 & num_entrev ==3

gen trab_entrev_4 = 0
replace trab_entrev_4 =1 if trabalhou ==1 & num_entrev ==4

gen trab_entrev_5 = 0
replace trab_entrev_5 =1 if trabalhou ==1 & num_entrev ==5

*Cria as variaveis que serao utilizadas nos modelos
*did para choque nao remunerado
gen time = 0
replace time = 1 if num_entrev == 5

bysort idind:egen treated = max(choque_max_nremun)
gen did = time*treated

*did para choque remunerado
bysort idind:egen treated_2 = max(choque_max_remun)
gen did_2 = time*treated_2 

*did para choque remunerado e nao remunerado
bysort idind:egen treated_3 = max(choque_max_total)
gen did_3 = time*treated_3 

tab treated
tab treated_2
tab treated_3

*exclui da amostra se tiver individuos com choque de saúde na primeira entrevista
drop if choque_max_remun==1 & num_entrev == 1
drop if choque_max_nremun==1 & num_entrev == 1
drop if choque_max_total==1 & num_entrev == 1

************** estima para choque nao remunerado*******************************

*DID SEM PSM
diff renda_deflac [aw=V1028], t(treated) p(time)id(idind) ktype(gaussian) robust
