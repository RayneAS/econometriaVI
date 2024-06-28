// Define o caminho para a pasta de dados
global data_folder "D:/rayne/Documents/dados_econometria_VI"

*log using "D:/rayne/Documents/dados_econometria_VI/model_chefe.log", replace

// Abrir a base de dados 
*use "${data_folder}/Base_final.dta", clear

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

*Algumas descritivas
sum idade if choque_max_nremun==1
sum educ if choque_max_nremun==1
tab cor if choque_max_nremun==1

sum idade if choque_max_nremun==0
sum educ if choque_max_nremun==0
tab cor if choque_max_nremun==0

tab grup_ativ
tab temp_afas
tab choque_max_remun ano
tab choque_max_nremun ano
tab choque_max_total ano

sum renda_deflac if choque_max_nremun==1 
sum renda_deflac if choque_max_nremun==0 

sum renda_deflac if choque_max_remun==1 
sum renda_deflac if choque_max_remun==0 

sum renda_deflac if choque_max_total==1 
sum renda_deflac if choque_max_total==0 


tab num_entrev ano

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

* contar o número de entrevistas por indiv
*by idind, sort: gen num_entrev_count = _N

* filtrar os indiv que fizeram todas as 5 entrevistas
*keep if num_entrev_count == 5

* remover a var aux 
*drop num_entrev_count


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





*modelo de diff in diff
reg renda_deflac time treated did i.ano [aw=V1028]
reg renda_deflac time treated did i.ano
*collapse (mean) renda_deflac, by(time treated)

reg horas_trab_t time treated did i.ano[aw=V1028]

*indid numeric
egen idind_num = group(idind)
list idind idind_num in 1/10

*did model
didregress (renda_deflac) (did), group(idind_num) time(time)
estat trendplots
estat ptrends

collapse (mean) renda_deflac, by(ano treated_2)

twoway (line renda_deflac ano if treated_2 == 1, lcolor(blue) lpattern(solid)) ///
       (line renda_deflac ano if treated_2 == 0, lcolor(red) lpattern(dash)), ///
       legend(label(1 "Tratado") label(2 "Controle")) ///
       title("Renda Deflacionada por Ano") ///
       ytitle("Renda Deflacionada") xtitle("Ano")

*graph
* Calcular a proporção de choques por ano
* Criar a variável combinada de ano e trimestre
gen ano_tri = yq(ano, tri)

* Calcular a proporção de choques por ano-tri
egen total_ano_tri = total(choque_max), by(ano_tri)
egen n_total_tri = total(1), by(ano_tri)
gen prop_choque_tri = total_ano_tri / n_total_tri

* Ajustar a variável ano_tri para ser exibida como data
format ano_tri %tq

* Plotar o gráfico
twoway (line prop_choque_tri ano_tri, lcolor(blue) lwidth(medium) lpattern(solid)) ///
       , title("Proporção de Choques de Saúde por Trimestre-Ano") ///
         ytitle("Proporção de Choques") xtitle("Trimestre-Ano") ///
         legend(off)

		 
* var combinada de ano e tri
gen ano_tri = yq(ano, tri)

* prop de choques por ano-tri
egen total_ano_tri = total(choque_max), by(ano_tri)
egen n_total_tri = total(1), by(ano_tri)
gen prop_choque_tri = total_ano_tri / n_total_tri

* Ajustar a var ano_tri como data
format ano_tri %tq

* bar graph
graph bar (mean) prop_choque_tri, over(ano) ///
       bar(1, color(blue)) ///
       title("Proporção de Choques de Saúde por Ano") ///
       ytitle("Proporção de Choques") ///
       legend(off)

graph bar (mean) prop_choque_tri, over(ano_tri) ///
       bar(1, color(blue)) ///
       title("Proporção de Choques de Saúde por Trimestre-Ano") ///
       ytitle("Proporção de Choques") ///
       legend(off)
	   	   
* Criar uma variável string legível para o eixo x
gen ano_tri_str = string(year(ano_tri)) + "-Q" + string(quarter(ano_tri))

* Plotar o gráfico de barras usando twoway bar
twoway (bar prop_choque_tri ano_tri, barwidth(0.5) lcolor(blue)) ///
       , title("Proporção de Choques de Saúde por Trimestre-Ano") ///
         ytitle("Proporção de Choques") ///
         xtitle("Trimestre-Ano") ///
         xlabel(1(1)12, valuelabel labsize(small) angle(45)) ///
         legend(off)
		   
		   
	* Criar uma variável string legível para o eixo x
gen ano_tri_str = string(year(ano_tri)) + "-Q" + string(quarter(ano_tri))

* Plotar o gráfico de barras usando twoway bar
twoway (bar prop_choque_tri ano_tri, barwidth(0.8) lcolor(blue)) ///
       , title("Proporção de Choques de Saúde por Trimestre-Ano") ///
         ytitle("Proporção de Choques") ///
         xtitle("Trimestre-Ano") ///
         xlabel(, angle(45) labsize(small)) ///
         legend(off)	   
		 
	   
local anos 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023

foreach ano in `anos' {
    display "Summary statistics for year `ano' with choque_fraco==1:"
    quietly sum renda_deflac if ano==`ano' & choque_fraco==1 [aw=V1028]
    display r(mean)
    display r(sd)
    display r(min)
    display r(max)
}
local anos 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023

foreach ano in `anos' {
    display "Summary statistics for year `ano' with choque_forte==0:"
    quietly sum renda_deflac if ano==`ano' & choque_forte==0 [aw=V1028]
    display r(mean)
    display r(sd)
    display r(min)
    display r(max)
}

sum renda_deflac if choque_max==1 [aw=V1028]
sum renda_deflac if choque_max==0 [aw=V1028]



********************************************************************************
*Se a ideia fosse estimar um modelo com efeitos fixos
*******************************************************************************
*Definindo o formato painel:
xtset idind 

*Descritivas: desvio-padrão, máximo e mínimo intra e inter unidades de análises:
xtsum renda choque_max


*FE  
xtreg renda_deflac choque_max, fe vce(robust)
estimates store FE

xtreg renda_deflac choque_max i.ano, fe vce(robust)
estimates store FE

xtreg renda_deflac choque_max i.ano i.tri, fe vce(robust)
estimates store FE

xtreg renda_deflac choque i.grup_ativ i.cor i.ano i.tri, fe vce(robust)
estimates store FE
 

*log close
