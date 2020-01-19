#Household Income Quantiles


# Household income constant prices

income_constant_prices <- function(data = df,
                                   base.month = 6,
                                   base.year = 2018,
                                   mes = mes,
                                   ht11 = ht11,
                                   ysvl = ysvl,
                                   ht13 = ht13,
                                   ht19 = ht19){

  varname <- paste0("ipc", base.year)
  load("~/MEGA/R/My packages/ech/R/sysdata.rda")
  mes_base <- ipc_base2010 %>% filter(month == base.month) %>% select(paste0("ipc", base.year)) %>% as.numeric
}

# Calculamos el deflactor QUEDÉ ACÁ
deflate <- ipc_base2010 %>% mutate(deflate = mes_base/varname,
                                   mes = 1:12)  %>% select(deflate, mes)

load("/home/calcita/MEGA/R/JOBS/Cursos y presentaciones/Curso rECH/curso-rech/Clase3/data/ech_h2018.Rdata")
load("/home/calcita/MEGA/R/JOBS/Cursos y presentaciones/Curso rECH/curso-rech/Clase3/data/ech_p2018.Rdata")

h2018 <- h2018 %>% mutate(Mes = as.integer(haven::zap_labels(mes)))

h2018 <- left_join(h2018, def, by = c("Mes" = "mes"))

h2018 %>% select(Mes, ht11, deflactor) %>% head()

## Ingresos promedio per cápita a precios constantes de junio 2018

# Ingresos deflactados
h2018 %<>% mutate(ht11_pc = ht11/ht19,
                  ht11_def = ht11 * deflactor,
                  ht13_def = ht13 * deflactor,
                  ht11_svl_def = YSVL * deflactor,
                  ht11_svl_pc_def = YSVL/ht19 * deflactor,
                  ht11_pc_def = ht11/ht19 * deflactor)
h2018 %>% select(mes, ht11,  ht11_def, YSVL, ht11_svl_def, ht11_pc, ht11_pc_def, YHOG) %>% head()

# Estimación de ingresos promedio per cápita a pesos constantes de jun/18

#Agrego las variables de ingreso calculadas a la base de personas

p2018 <- h2018 %>% select(numero, ht11,  ht11_def, YSVL, ht11_svl_def, ht11_pc, ht11_pc_def) %>% left_join(p2018, ., by = "numero")
p2018 %>% select(numero, ht11:ht11_pc_def) %>% head()



# Estimación de ingresos promedio per cápita a pesos constantes (jun/18)


# Defino el diseño
library(srvyr)
dp <- p2018 %>% as_survey_design(ids = numero,
                                 strata = estred13,
                                 weights = pesoano)

# Estimo promedio
dp %>% summarise(ypc = survey_mean(ht11_pc_def,
                                   vartype = "ci"))



## quintiles
quintiles = svyquantile(~yconstante_pc, dhog, c(.20,.40,.60,.80,1))

## deciles
deciles = svyquantile(~yconstante_pc, dhog, seq(0.1,1,0.1))
