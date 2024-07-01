// Define o caminho para a pasta de dados
global data_folder "D:/rayne/Documents/dados_econometria_VI"

log using "D:/rayne/Documents/dados_econometria_VI/model_chefe.log", replace

// Abrir a base de dados 

use "${data_folder}/Base_final_chefe.dta", clear

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
*drop _ps
*ssc install diff
*Realiza o psm para depois rodar o diff in diff
logit treated idade grup_ativ UF educ 
predict _ps, pr

hist _ps, by(treated) bin(20) // Histograma dos propensity scores

summarize _ps if treated == 1
local min_treated = r(min)
local max_treated = r(max)

summarize _ps if treated == 0
local min_control = r(min)
local max_control = r(max)

local common_min = max(`min_treated', `min_control')
local common_max = min(`max_treated', `max_control')

keep if _ps >= `common_min' & _ps <= `common_max'

*ssc install psmatch2
psmatch2 treated, out(renda_deflac) pscore(_ps) bw(0.06)

pstest idade grup_ativ UF educ , graph

psgraph

*Modelo com diff in diff e propensity score matching
diff renda_deflac [aw=V1028], t(treated) p(time) kernel id(idind) ktype(gaussian) pscore(_ps)


*******************************************************************************
************** estima para choque remunerado*******************************
*drop _ps_2
*ssc install diff
*Realiza o psm para depois rodar o diff in diff
logit treated_2 idade grup_ativ UF educ 
predict _ps_2, pr

hist _ps_2, by(treated_2) bin(20) // Histograma dos propensity scores

summarize _ps_2 if treated_2 == 1
local min_treated_2 = r(min)
local max_treated_2 = r(max)

summarize _ps_2 if treated_2 == 0
local min_control_2 = r(min)
local max_control_2 = r(max)

local common_min_2 = max(`min_treated_2', `min_control_2')
local common_max_2 = min(`max_treated_2', `max_control_2')

keep if _ps_2 >= `common_min_2' & _ps <= `common_max_2'

*ssc install psmatch2
psmatch2 treated_2, out(renda_deflac) pscore(_ps_2) bw(0.06)

pstest idade grup_ativ UF educ , graph

psgraph

*Modelo com diff in diff e propensity score matching
diff renda_deflac [aw=V1028], t(treated_2) p(time) kernel id(idind) ktype(gaussian) pscore(_ps_2)


*******************************************************************************
************** estima para choque remunerado e nao remunerado *******************************
*drop _ps_3
*ssc install diff
*Realiza o psm para depois rodar o diff in diff
logit treated_3 idade grup_ativ UF educ 
predict _ps_3, pr

hist _ps_3, by(treated_3) bin(20) // Histograma dos propensity scores

summarize _ps_3 if treated_3 == 1
local min_treated_3 = r(min)
local max_treated_3 = r(max)

summarize _ps_3 if treated_3 == 0
local min_control_3 = r(min)
local max_control_3 = r(max)

local common_min_3 = max(`min_treated_3', `min_control_3')
local common_max_3 = min(`max_treated_3', `max_control_3')

keep if _ps_3 >= `common_min_3' & _ps <= `common_max_3'

*ssc install psmatch2
psmatch2 treated_3, out(renda_deflac) pscore(_ps_3) bw(0.06)

pstest idade grup_ativ UF educ , graph

psgraph

*Modelo com diff in diff e propensity score matching
diff renda_deflac [aw=V1028], t(treated_3) p(time) kernel id(idind) ktype(gaussian) pscore(_ps_3)


*******************************************************************************


log close



*****************************ANALISE PARA REGIAO RURAL ************************

global data_folder "D:/rayne/Documents/dados_econometria_VI"

log using "D:/rayne/Documents/dados_econometria_VI/model_chefe_RURAL.log", replace

// Abrir a base de dados 

use "${data_folder}/Base_final_chefe.dta", clear

keep if rural ==1

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
*drop _ps
*ssc install diff
*Realiza o psm para depois rodar o diff in diff
logit treated idade grup_ativ UF educ 
predict _ps, pr

hist _ps, by(treated) bin(20) // Histograma dos propensity scores

summarize _ps if treated == 1
local min_treated = r(min)
local max_treated = r(max)

summarize _ps if treated == 0
local min_control = r(min)
local max_control = r(max)

local common_min = max(`min_treated', `min_control')
local common_max = min(`max_treated', `max_control')

keep if _ps >= `common_min' & _ps <= `common_max'

*ssc install psmatch2
psmatch2 treated, out(renda_deflac) pscore(_ps) bw(0.06)

pstest idade grup_ativ UF educ , graph

psgraph

*Modelo com diff in diff e propensity score matching
diff renda_deflac [aw=V1028], t(treated) p(time) kernel id(idind) ktype(gaussian) pscore(_ps)


*******************************************************************************
************** estima para choque remunerado*******************************
*drop _ps_2
*ssc install diff
*Realiza o psm para depois rodar o diff in diff
logit treated_2 idade grup_ativ UF educ 
predict _ps_2, pr

hist _ps_2, by(treated_2) bin(20) // Histograma dos propensity scores

summarize _ps_2 if treated_2 == 1
local min_treated_2 = r(min)
local max_treated_2 = r(max)

summarize _ps_2 if treated_2 == 0
local min_control_2 = r(min)
local max_control_2 = r(max)

local common_min_2 = max(`min_treated_2', `min_control_2')
local common_max_2 = min(`max_treated_2', `max_control_2')

keep if _ps_2 >= `common_min_2' & _ps <= `common_max_2'

*ssc install psmatch2
psmatch2 treated_2, out(renda_deflac) pscore(_ps_2) bw(0.06)

pstest idade grup_ativ UF educ , graph

psgraph

*Modelo com diff in diff e propensity score matching
diff renda_deflac [aw=V1028], t(treated_2) p(time) kernel id(idind) ktype(gaussian) pscore(_ps_2)


*******************************************************************************
************** estima para choque remunerado e nao remunerado *******************************
*drop _ps_3
*ssc install diff
*Realiza o psm para depois rodar o diff in diff
logit treated_3 idade grup_ativ UF educ 
predict _ps_3, pr

hist _ps_3, by(treated_3) bin(20) // Histograma dos propensity scores

summarize _ps_3 if treated_3 == 1
local min_treated_3 = r(min)
local max_treated_3 = r(max)

summarize _ps_3 if treated_3 == 0
local min_control_3 = r(min)
local max_control_3 = r(max)

local common_min_3 = max(`min_treated_3', `min_control_3')
local common_max_3 = min(`max_treated_3', `max_control_3')

keep if _ps_3 >= `common_min_3' & _ps <= `common_max_3'

*ssc install psmatch2
psmatch2 treated_3, out(renda_deflac) pscore(_ps_3) bw(0.06)

pstest idade grup_ativ UF educ , graph

psgraph

*Modelo com diff in diff e propensity score matching
diff renda_deflac [aw=V1028], t(treated_3) p(time) kernel id(idind) ktype(gaussian) pscore(_ps_3)


*******************************************************************************


log close


*****************************ANALISE PARA REGIAO URBANA ************************

global data_folder "D:/rayne/Documents/dados_econometria_VI"

log using "D:/rayne/Documents/dados_econometria_VI/model_chefe_URBANA.log", replace

// Abrir a base de dados 

use "${data_folder}/Base_final_chefe.dta", clear

keep if rural ==0

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
*drop _ps
*ssc install diff
*Realiza o psm para depois rodar o diff in diff
logit treated idade grup_ativ UF educ 
predict _ps, pr

hist _ps, by(treated) bin(20) // Histograma dos propensity scores

summarize _ps if treated == 1
local min_treated = r(min)
local max_treated = r(max)

summarize _ps if treated == 0
local min_control = r(min)
local max_control = r(max)

local common_min = max(`min_treated', `min_control')
local common_max = min(`max_treated', `max_control')

keep if _ps >= `common_min' & _ps <= `common_max'

*ssc install psmatch2
psmatch2 treated, out(renda_deflac) pscore(_ps) bw(0.06)

pstest idade grup_ativ UF educ , graph

psgraph

*Modelo com diff in diff e propensity score matching
diff renda_deflac [aw=V1028], t(treated) p(time) kernel id(idind) ktype(gaussian) pscore(_ps)


*******************************************************************************
************** estima para choque remunerado*******************************
*drop _ps_2
*ssc install diff
*Realiza o psm para depois rodar o diff in diff
logit treated_2 idade grup_ativ UF educ 
predict _ps_2, pr

hist _ps_2, by(treated_2) bin(20) // Histograma dos propensity scores

summarize _ps_2 if treated_2 == 1
local min_treated_2 = r(min)
local max_treated_2 = r(max)

summarize _ps_2 if treated_2 == 0
local min_control_2 = r(min)
local max_control_2 = r(max)

local common_min_2 = max(`min_treated_2', `min_control_2')
local common_max_2 = min(`max_treated_2', `max_control_2')

keep if _ps_2 >= `common_min_2' & _ps <= `common_max_2'

*ssc install psmatch2
psmatch2 treated_2, out(renda_deflac) pscore(_ps_2) bw(0.06)

pstest idade grup_ativ UF educ , graph

psgraph

*Modelo com diff in diff e propensity score matching
diff renda_deflac [aw=V1028], t(treated_2) p(time) kernel id(idind) ktype(gaussian) pscore(_ps_2)


*******************************************************************************
************** estima para choque remunerado e nao remunerado *******************************
*drop _ps_3
*ssc install diff
*Realiza o psm para depois rodar o diff in diff
logit treated_3 idade grup_ativ UF educ 
predict _ps_3, pr

hist _ps_3, by(treated_3) bin(20) // Histograma dos propensity scores

summarize _ps_3 if treated_3 == 1
local min_treated_3 = r(min)
local max_treated_3 = r(max)

summarize _ps_3 if treated_3 == 0
local min_control_3 = r(min)
local max_control_3 = r(max)

local common_min_3 = max(`min_treated_3', `min_control_3')
local common_max_3 = min(`max_treated_3', `max_control_3')

keep if _ps_3 >= `common_min_3' & _ps <= `common_max_3'

*ssc install psmatch2
psmatch2 treated_3, out(renda_deflac) pscore(_ps_3) bw(0.06)

pstest idade grup_ativ UF educ , graph

psgraph

*Modelo com diff in diff e propensity score matching
diff renda_deflac [aw=V1028], t(treated_3) p(time) kernel id(idind) ktype(gaussian) pscore(_ps_3)


*******************************************************************************


log close
