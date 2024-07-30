// Define o caminho para a pasta de dados
global data_folder "D:/rayne/Documents/dados_econometria_VI"

log using "D:/rayne/Documents/dados_econometria_VI/model_chefe_event_study.log", replace

// Abrir a base de dados 
use "${data_folder}/Base_final_chefe_todas_entrev_3.dta", clear

describe

*NOSSA AMOSTRA DE INTERESSE ESTÁ NOS CENTROS URBANOS
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
ren V4001 trabalhou

tab num_entrev

tab temp_afast if num_entrev==1
tab temp_afast if num_entrev==2
tab temp_afast if num_entrev==3
tab temp_afast if num_entrev==4
tab temp_afast if num_entrev==5

tab V2005


destring idind, replace
gen qdate= yq(ano, tri)
assert missing(qdate) == missing(ano, tri)
format qdate %tq
tab qdate

xtset idind qdate


* Definir períodos relativos ao choque
gen time_to_shock = num_entrev - 3

* Criar variáveis dummies para os períodos de interesse
gen pre_shock1 = (time_to_shock == -2)
gen pre_shock2 = (time_to_shock == -1)
gen post_shock1 = (time_to_shock == 1)
gen post_shock2 = (time_to_shock == 2)
gen post_shock3 = (time_to_shock == 3)


	
* Estimar o modelo de event study com efeitos fixos
xtreg renda_deflac pre_shock1 pre_shock2 post_shock1 post_shock2 post_shock3 i.choque_max_nremun i.cor i.idade i.educ i.ano i.tri, fe cluster(idind)
			
* Coletar os coeficientes e erros padrão
matrix list e(b)
matrix list e(V)

* Criar variáveis para armazenar os coeficientes e intervalos de confiança
gen coef = .
gen lower = .
gen upper = .

* Períodos relativos ao choque
local periods "pre_shock1 pre_shock2 post_shock1 post_shock2 post_shock3"

* Coletar coeficientes e intervalos de confiança
local i = 1
foreach var of local periods {
    replace coef = _b[`var'] if _n == `i'
    replace lower = _b[`var'] - 1.96 * _se[`var'] if _n == `i'
    replace upper = _b[`var'] + 1.96 * _se[`var'] if _n == `i'
    local i = `i' + 1
}

* Criar variável de período
gen period = .
replace period = -2 if _n == 1
replace period = -1 if _n == 2
replace period = 1 if _n == 3
replace period = 2 if _n == 4
replace period = 3 if _n == 5

* Plotar o gráfico -mais clean
twoway (rarea lower upper period, color(gs12)) ///
       (line coef period, lcolor(navy) lwidth(medium)), ///
    xlabel(-2(1)3) ylabel(, angle(0)) ///
    xtitle("Período em relação ao choque") ytitle("Coeficiente de Renda") ///
    legend(off) ///
    title("") ///
    graphregion(color(white)) ///
    plotregion(style(none))

* Plotar o gráfico
twoway (rarea lower upper period, color(gs12)) (line coef period, lcolor(navy) lwidth(medium)), ///
    xlabel(-2(1)3) ylabel(, angle(0)) ///
    xtitle("Período em relação ao choque") ytitle("Coeficiente de Renda")
		
* Plotar o gráfico
twoway (rarea lower upper period, color(gs12)) (line coef period, lcolor(navy) lwidth(medium)), ///
    xlabel(-2(1)3) ylabel(, angle(0)) ///
    title("Event Study: Impacto do Choque de Saúde na Renda") ///
    xtitle("Período em relação ao choque") ytitle("Coeficiente de Renda")
	
log close	
	