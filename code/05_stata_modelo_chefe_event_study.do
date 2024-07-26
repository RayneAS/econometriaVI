// Define o caminho para a pasta de dados
global data_folder "D:/rayne/Documents/dados_econometria_VI"

*log using "D:/rayne/Documents/dados_econometria_VI/model_chefe.log", replace

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
xtreg renda_deflac pre_shock1 pre_shock2 post_shock1 post_shock2 post_shock3 i.choque_max_nremun, fe cluster(idind)

* Coletar os coeficientes e erros padrão
matrix coef = e(b)
matrix se = e(V)

* Preparar dados para o gráfico
local n = rowsof(coef)
gen time = -2

forval i = 1/`n' {
    local coef`i' = coef[1,`i']
    local se`i' = sqrt(se[`i',`i'])
    gen coef`i' = .
    replace coef`i' = `coef`i'' if _n == `i'
    gen se`i' = .
    replace se`i' = `se`i'' if _n == `i'
    replace time = time + 1 if _n == `i'
}

* Criar dataset para o gráfico
gen lower = .
gen upper = .
gen period = .
forval i = 1/`n' {
    replace lower = coef`i' - 1.96*se`i'
    replace upper = coef`i' + 1.96*se`i'
    replace period = time[`i']
}

* Plotar o gráfico
twoway (rarea lower upper period, color(gs12)) (line coef1 period, lcolor(navy) lwidth(medium)), ///
    xlabel(-2(1)3) ylabel(, angle(0)) ///
    title("Event Study: Impacto do Choque de Saúde na Renda") ///
    xtitle("Período em relação ao choque") ytitle("Coeficiente de Renda")
	
	
	