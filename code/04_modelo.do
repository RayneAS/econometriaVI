// Define o caminho para a pasta de dados
global data_folder "D:/rayne/Documents/dados_econometria_VI"

*keep if Ano == 2012 | Ano == 2013

log using "D:/rayne/Documents/dados_econometria_VI/model_chefe.log", replace

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
*ren VD3006 educ
ren VD4010 grup_ativ
ren V4008 temp_afast


gen choque_fraco = 1 if temp_afast==1
replace choque_fraco=0 if temp_afast==.

gen choque_forte = 1 if temp_afast==2
replace choque_forte=0 if temp_afast==.

tab choque_fraco
tab choque_forte



*trabalhadores em idade entre 50-60 sao mais frequentes no choque
sum idade if choque_max==1
sum idade if choque_max==0

tab cor if choque_max==1
tab cor if choque_max==0



tab grup_ativ
tab temp_afas
tab choque_max ano

sum renda_deflac if choque_max==1 [aw=V1028]
sum renda_deflac if choque_max==0 [aw=V1028]

sum renda_deflac if choque_fraco==1
sum renda_deflac if choque_fraco==0


sum renda_deflac if choque_forte==1
sum renda_deflac if choque_forte==0


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


* contar o número de entrevistas por indiv
*by idind, sort: gen num_entrev_count = _N

* filtrar os indiv que fizeram todas as 5 entrevistas
*keep if num_entrev_count == 5

* remover a var aux 
*drop num_entrev_count

*torna indid numeric
destring idind, replace
destring iddom, replace

*duplicates report idind iddom

*Definindo o formato painel:
xtset idind 
*Overall: Estatísticas considerando todas as observações sem distinção entre unidades ou tempo.
*Between: Estatísticas considerando a variação entre diferentes unidades (indivíduos).
*Within: Estatísticas considerando a variação dentro das unidades ao longo do tempo.

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
 

 *testes
 *idade não é um bom controle
 
 
log close
