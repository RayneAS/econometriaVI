
base_2012 <- readRDS("PNADC_Bianual_L_2012.RDS")

base_2013 <- readRDS("PNADC_Bianual_L_2013.RDS")

base_2014 <- readRDS("PNADC_Bianual_L_2014.RDS")

base_2015 <- readRDS("PNADC_Bianual_L_2015.RDS")

base_2016 <- readRDS("PNADC_Bianual_L_2016.RDS")

base_2017 <- readRDS("PNADC_Bianual_L_2017.RDS")

base_2018 <- readRDS("PNADC_Bianual_L_2018.RDS")

base_2019 <- readRDS("PNADC_Bianual_L_2019.RDS")

base_2020 <- readRDS("PNADC_Bianual_L_2020.RDS")

base_2021 <- readRDS("PNADC_Bianual_L_2021.RDS")

base_2022 <- readRDS("PNADC_Bianual_L_2022.RDS")

base_unida <- rbind(base_2012, base_2013, base_2014, base_2015, base_2016,
                    base_2017, base_2018, base_2019, base_2020, base_2021,
                    base_2022)

write.dta(base_unida, "Base_final_completa.dta")
