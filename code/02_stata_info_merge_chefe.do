// Define o caminho para a pasta de dados
global data_folder "D:/rayne/Documents/dados_econometria_VI"

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

keep idind iddom num_entrev ano tri idade grup_ativ UF educ

egen new_id = concat(iddom num_entrev ano tri)

ren idade idade_chefe
ren grup_ativ grup_ativ_chefe
ren educ educ_chefe

keep new_id idade_chefe grup_ativ_chefe educ_chefe

save "${data_folder}/chefe_merge_info.dta", replace
