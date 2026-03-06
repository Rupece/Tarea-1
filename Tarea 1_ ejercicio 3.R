# Librerias #

#install.packages("readxl")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("stringr")

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

## 3. Contraste un modelo trivial de la determinación del salario con los datos por medio de los siguientes pasos: **[2 horas, 0.5 puntos cada inciso]**.

# Por favor documente su trabajo para que se pueda replicar.

#### 1. Obtenga una serie del **PIB** $Y_t$ de la economía.

## Carga de base de datos y limpieza

ruta <- setwd("C:/Users/Rupec/OneDrive/Escritorio/ME/Segundo semestre/Macroeconomia II/1. Mercado laboral/Tarea 1") # directorio de la computadora

pib_raw <- read_excel("PIB_desest_2018.xlsx",
                       col_names = F,
                       col_types = "text") # base del PIB a precios de 2018 del INEGI

pib_raw <- pib_raw[,colSums(!is.na(pib_raw)) > 0] # borramos NAs

fila_años <- 4 # Fila donde estan los años
fila_trim <- 5 # Fila donde estan los trimestres
fila_pib <- 6 # Fila donde estan los datos de PIB

años <- unlist(pib_raw[fila_años,-1])[2:133] # Extraemos años, trimestres y PIB 
trim <- unlist(pib_raw[fila_trim,-1])[2:133]
pib_val <- unlist(pib_raw[fila_pib,-1])[2:133]


trim_num <- recode(trim,
                   "I" = "1",
                   "II" = "2",
                   "III" = "3",
                   "IV" = "4") # Cambiamos los trimestres de string a numericos

pib_val <- as.numeric(pib_val) # Cambiamos el PIB de string a numerico
fecha <- paste0(años, "T", trim_num) # Creamos la variable fecha para la serie de tiempo deL PIB

año_inicio <- as.numeric(años[1]) # año de inicio 
trim_inicio <- as.numeric(trim_num[1]) # trimestre de inicio 

PIB_ts <- ts(pib_val/1000,
             start = c(año_inicio, trim_inicio),
             frequency =  4) # Serie de tiempo PIB 1993T1 - 2025T4

source("C:/Users/Rupec/OneDrive/Escritorio/R/graf_excel.R")

grafica_excel(PIB_ts,mostrar_leyenda = F, y_break_by = 2000, date_breaks = "4 years",, grosor_linea = 0.6, titulo = "Grafica 1: PIB a precios corrientes de 2018", subtitulo = "Miles de millones de pesos", fuente = "Fuente: elaboracion propia con datos del INEGI")

#### 2. Obtenga una serie del **capital** $K_t$ de la economía ("Índice de Volumen físico acumulado").

cap_raw <- read_excel("Capital_base_2018.xlsx",
                      col_names = F,
                      col_types = "text") # base del Indice de capital fisico base 2018 del INEGI

cap_raw <- cap_raw[,colSums(!is.na(cap_raw))>0] # borramos NAs

fila_años_c <- 5 # Fila donde estan los años
fila_mes <- 6 # Fila donde estan los trimestres
fila_cap <- 78 # Fila donde estan los datos del Indice de volumen fisico acumulado
                  # La serie de Por tipo de bien y origen y por tipo de bien y comprador son exactamente las mismas. Por tanto, se pueden usar indistintamente

años_c <- unlist(cap_raw[fila_años_c,-1])[2:396] # Extraemos años, meses y Capital 
meses <- unlist(cap_raw[fila_mes,-1])[2:396]
cap_val <- unlist(cap_raw[fila_cap,-1])[2:396]

cap_val <- as.numeric(cap_val) # Cambiamos el Capital de string a numerico 

mes_num <- recode(meses,
                  "Enero" = 1,
                  "Febrero" = 2,
                  "Marzo" = 3,
                  "Abril" = 4,
                  "Mayo" = 5,
                  "Junio" = 6,
                  "Julio" = 7,
                  "Agosto" = 8,
                  "Septiembre" = 9,
                  "Octubre" = 10,
                  "Noviembre" = 11,
                  "Diciembre" = 12) # Convertir meses a número
fecha_mensual <- as.yearmon(paste(años_c, mes_num), "%Y %m") # Convertir meses a variable de fecha
cap_mensual <- zoo(cap_val, order.by = fecha_mensual) # Serie mensual del Capital

cap_trim <- aggregate(cap_mensual,
                                as.yearqtr,
                                mean) # Promedio por mes trimestre del Capital

fechas_q <- format(index(cap_trim), "%YQ%q") # Formato de trimestre
fechas_q <- gsub("Q", "T", fechas_q)

año_inicio <- as.numeric(substr(fechas_q[1], 1, 4)) # año de inicio
trim_inicio <- as.numeric(substr(fechas_q[1], 6, 6)) # trimestre de inicio

Capital_ts <- ts(as.numeric(cap_trim),
                 start = c(año_inicio, trim_inicio),
                 frequency = 4) # Creamos la serie de tiempo del Indice de volumen de capital fisico acumulado base 2018 1993T1-2025T4

grafica_excel(Capital_ts,mostrar_leyenda = F,y_break_by = 11, date_breaks = "4 years", grosor_linea = 0.6, titulo = "Grafica 2: Volumen del capital fisico acumulado", subtitulo = "Indice base 2018", fuente = "Fuente: elaboracion propia con datos del INEGI")

#### 3. Obtenga una serie del **empleo** $L_t$ de la economía.

emp_raw <- read_excel("Pob_ocupada.xls",
                      col_names = F,
                      col_types = "text") # base de la poblacion ocupada del INEGI

emp_raw <- emp_raw[,colSums(!is.na(emp_raw)) > 0] # borramos NAs

fila_años_e <- 6 # Fila donde estan los años
fila_trim_e <- 7 # Fila donde estan los trimestres
fila_empleo <- 8 # Fila donde estan los datos del empleo

años_e <- unlist(emp_raw[fila_años_e,-1])[2:85] # Extraemos años, trimestres y empleo 
trim_e <- unlist(emp_raw[fila_trim_e,-1])[2:85]
emp_val <- unlist(emp_raw[fila_empleo,-1])[2:85]

trim_num_e <- recode(trim_e,
                   "I" = "1",
                   "II" = "2",
                   "III" = "3",
                   "IV" = "4") # Cambiamos los trimestres de string a numericos

emp_val <- as.numeric(emp_val) # Cambiamos el empleo de string a numerico
fecha_e <- paste0(años_e, "T", trim_num_e) # Creamos la variable fecha para la serie de tiempo deL PIB

año_inicio_e <- as.numeric(años_e[1]) # año de inicio 
trim_inicio_e <- as.numeric(trim_num_e[1]) # trimestre de inicio 

Empleo_ts <- ts(emp_val/1000,
             start = c(año_inicio_e, trim_inicio_e),
             frequency =  4) # Serie de tiempo de la poblacion economicamente activa ocupada 2005T1 - 2025T4

grafica_excel(Empleo_ts,mostrar_leyenda = F,y_break_by = 2000,date_breaks = "2 years", grosor_linea = 0.6, titulo = "Grafica 3: Poblacion ocupada", subtitulo = "Miles de personas", fuente = "Fuente: elaboracion propia con datos del INEGI")

#### 4. Cree una serie de la **productividad** $A_t$ de la economía a partir de asumir una función de producción

# $Y_t = A_t F(K,L)$, con $F(K,L)=K^{0.7}L^{0.3}$.

# Las tres series tienen una medicion diferente.El PIB esta medido en millones de pesos constantes del 2018,mientras que las cifras de la poblacion ocupada estan medidas en millones (de personas).Por su parte, la serie del capital, como su nombre lo indica, es un indice a 2018.
# Para homogeneizar las tres series y que sean comparables para realizar estimaciones, convertimos la serie del PIB y el empleo a indices al primer trimestre del 2018. 
# En cuanto a su periodicidad,la serie del empleo es la serie mas corta, yendo desde el primer trimestre del 2005 hasta el cuarto trimeste del 2025. Por tanto, las estimaciones iran desde esa periodicidad.

base_pib    <- window(PIB_ts, start = c(2018,1), end = c(2018,1))
base_empleo <- window(Empleo_ts, start = c(2018,1), end = c(2018,1))
base_capital <- window(Capital_ts, start = c(2018,1), end = c(2018,1)) # Escogemos el valor base de cada serie del primer trimestre del 2018

pib_indice <- (PIB_ts / as.numeric(base_pib)) * 100
empleo_indice <- (Empleo_ts / as.numeric(base_empleo)) * 100
capital_indice <- (Capital_ts / as.numeric(base_capital)) * 100 # Creamos las series de tiempo de cada variable en indices

series_indice <- cbind(
  PIB      = pib_indice,
  Empleo   = empleo_indice,
  Capital  = capital_indice,
) %>% na.omit(series_indice) # Creamos el dataframe con las tres series en indices 

grafica_excel(series_indice[,1:3],mostrar_leyenda = T, colores = c("red","blue","#2CA02C"),y_break_by = 5,date_breaks = "2 years", grosor_linea = 0.6, titulo = "Grafica 4: PIB, empleo y capital",subtitulo = "Indice 2018-1T = 100", fuente = "Fuente: elaboracion propia con datos del INEGI")

# Suponiendo que el proceso generador de datos del PIB es Y_t = A_t F(K,L), con F(K,L)=K^{0.7}L^{0.3}
# Entonces la serie de la productividad A_t es A_t = Y_t/K^{0.7}L^{0.3}

series_indice <- cbind(
  PIB     = series_indice[, "PIB"],
  Empleo  = series_indice[, "Empleo"],
  Capital = series_indice[, "Capital"],
  TFP     = series_indice[, "PIB"] /
    (series_indice[, "Capital"]^0.7 *
       series_indice[, "Empleo"]^0.3)*100
) # Estimacion deterministica de la productividad total de los factores 

grafica_excel(series_indice[,4],mostrar_leyenda = F,date_breaks = "2 years", grosor_linea = 0.6, titulo = "Grafica 5: Productividad total de los factores",subtitulo = "Indice 2018-1T = 100", fuente = "Fuente: estimacion propia con datos del INEGI")

#### 5. Cree una serie **contrafactual** del salario que se debió de haber observado si el salario fuera el **ingreso marginal del trabajo** $A_t F_L(K_t, L_t)$.

# En un modelo simple de determinacion de salarios en el mercado laboral, en donde la oferta de trabajo es igual a la demanda de trabajo, el salario esta determinado por la productividad marginal del trabajo
# En este caso, la productivdad marginal del trabajo es igual a PMGL = 0.3A_tK_t^{0.7}L_t^{-0,7} = w_t

series_indice <- cbind(
  PIB     = series_indice[, "PIB"],
  Empleo  = series_indice[, "Empleo"],
  Capital = series_indice[, "Capital"],
  TFP = series_indice[, "TFP"],
  "Salario walrasiano" = 0.3*series_indice[,"TFP"]*
    series_indice[, "Capital"]^(0.7)*
    series_indice[, "Empleo"]^(-0.7),
  ) # Estimacion deterministica del salario walrasiano 

grafica_excel(series_indice[,5],mostrar_leyenda = F,y_break_by = 1,date_breaks = "2 years", grosor_linea = 0.6, titulo = "Grafica 6: Salario walrasiano de equilibrio", fuente = "Fuente: estimacion propia con datos del INEGI")

#### 6. Compare el salario observado con el salario contrafactual a la luz de los hechos estilizados y las teorías descritas en clase.

# La comparacion del salario observado con el salario contrafactual se hace usando el Indice global de remuneraciones medias reales de los sectores economicos, usado en el ejercicio 2.

salario_raw <- read_excel("Salario_real_base_2018.xlsx",
                      col_names = F,
                      col_types = "text")[7:223,] # base del Indice de remuneraciones reales del INEGI

salario_raw <- salario_raw[,colSums(!is.na(salario_raw))>0]

col_años_s <- 1 # Columna donde estan los años
col_meses_s <- 2 # Columna donde estan los meses
col_salario <- 3 # Columna donde estan los datos del Indice de volumen fisico acumulado

años_s <- unlist(salario_raw[-1,col_años_s])[1:216] # Extraemos años, meses y salario real
meses_s <- unlist(salario_raw[-1,col_meses_s])[1:216]
salario_val <- unlist(salario_raw[-1,col_salario])[1:216]

salario_val <- as.numeric(salario_val) # Cambiamos el Salario de string a numerico

mes_num_s <- recode(meses_s,
                  "Enero" = 1,
                  "Febrero" = 2,
                  "Marzo" = 3,
                  "Abril" = 4,
                  "Mayo" = 5,
                  "Junio" = 6,
                  "Julio" = 7,
                  "Agosto" = 8,
                  "Septiembre" = 9,
                  "Octubre" = 10,
                  "Noviembre" = 11,
                  "Diciembre" = 12) # Convertir meses a número

fecha_mensual_s <- as.yearmon(paste(años_s, mes_num_s), "%Y %m") # Convertir meses a variable de fecha
salario_mensual <- zoo(salario_val, order.by = fecha_mensual_s) # Serie mensual del Salario real

salario_trim <- aggregate(salario_mensual,
                      as.yearqtr,
                      mean) # Promedio por mes trimestre del Salario real

fechas_q_s <- format(index(salario_trim), "%YQ%q") # Formato de trimestre
fechas_q_s <- gsub("Q", "T", fechas_q_s)

año_inicio_s <- as.numeric(substr(fechas_q_s[1], 1, 4)) # año de inicio
trim_inicio_s <- as.numeric(substr(fechas_q_s[1], 6, 6)) # trimestre de inicio

Salario_ts <- ts(as.numeric(salario_trim),
                 start = c(año_inicio_s, trim_inicio_s),
                 frequency = 4) # Creamos la serie de tiempo del salario real base 2018 1993T1-2025T4

grafica_excel(Salario_ts,mostrar_leyenda = F,y_break_by = 4,date_breaks = "2.5 years", grosor_linea = 0.6, titulo = "Grafica 7: Salario real", subtitulo = "Indice base 2018", fuente = "Fuente: elaboracion propia con datos del INEGI")

# Ahora, para comparaciones, calculamos el indice al primer trimestre del 2018 del salario real

base_salario <- window(Salario_ts, start = c(2018,1), end = c(2018,1)) # Escogemos el valor base de cada serie del primer trimestre del 2018
salario_indice <- (Salario_ts / as.numeric(base_salario)) * 100

series_indice <- cbind(
  PIB     = series_indice[, "PIB"],
  Empleo  = series_indice[, "Empleo"],
  Capital = series_indice[, "Capital"],
  TFP = series_indice[, "TFP"],
  `Salario walrasiano` = series_indice[,"Salario walrasiano"],
  `Salario observado` = salario_indice
  )%>% na.omit(series_indice) # Juntamos el salario real observado con las demas series de tiempo

grafica_excel(series_indice[,5:6], colores = c("red","blue"),mostrar_leyenda = T, grosor_linea = 0.6,y_break_by = 10,date_breaks = "2 years", titulo = "Grafica 8: Salario real observado y salario real walrasiano estimado", subtitulo = "Indice 2018-1T = 100", fuente = "Fuente: elaboracion propia con datos del INEGI")

#### 7. Compare el salario promedio según el **IMSS** con el salario promedio según la **ENOE** del INEGI.

salario_IMSS_raw <- read_excel("Salario_IMSS.xlsx",
                          col_names = F,
                          col_types = "text") # base del Salario diario asociado a los trabajadores asegurados en el IMSS

salario_ENOE_raw <- read_excel("Salario_ENOE.xlsx",
                               col_names = F,
                               col_types = "text") # base del Salario promedio diario de la ENOE 

salario_IMSS_raw <- salario_IMSS_raw[,colSums(!is.na(salario_IMSS_raw))>0]
salario_ENOE_raw <- salario_ENOE_raw[,colSums(!is.na(salario_ENOE_raw)) > 0] # borramos NAs

col_años_IMSS <- 1 # Columna donde estan los años
col_meses_IMSS <- 2 # Columna donde estan los meses
col_salario_IMSS <- 3 # Columna donde estan los datos del salario del IMSS
fila_años_ENOE <- 5 # Fila donde estan los años
fila_trim_ENOE <- 6 # Fila donde estan los trimestres
fila_salario_ENOE <- 20 # Fila donde estan los datos del salario de la ENOE 


años_s_IMSS <- unlist(salario_IMSS_raw[-1,col_años_IMSS]) # Extraemos años, meses y salario del IMSS
meses_s_IMSS <- unlist(salario_IMSS_raw[-1,col_meses_IMSS])
salario_IMSS_val <- unlist(salario_IMSS_raw[-1,col_salario_IMSS])
salario_IMSS_val <- as.numeric(salario_IMSS_val) # Cambiamos el Salario de string a numerico

años_s_ENOE <- unlist(salario_ENOE_raw[fila_años_ENOE,-1]) # Extraemos años, trimestres y salario 
trim_s_ENOE <- unlist(salario_ENOE_raw[fila_trim_ENOE,-1])
salario_ENOE_val <- unlist(salario_ENOE_raw[fila_salario_ENOE,-1])
salario_ENOE_val <- as.numeric(salario_ENOE_val) # Cambiamos el salario de string a numerico

mes_num_s_IMSS <- recode(meses_s_IMSS,
                    "Enero" = 1,
                    "Febrero" = 2,
                    "Marzo" = 3,
                    "Abril" = 4,
                    "Mayo" = 5,
                    "Junio" = 6,
                    "Julio" = 7,
                    "Agosto" = 8,
                    "Septiembre" = 9,
                    "Octubre" = 10,
                    "Noviembre" = 11,
                    "Diciembre" = 12) # Convertir meses a número

trim_num_ENOE <- recode(trim_s_ENOE,
                   "I" = "1",
                   "II" = "2",
                   "III" = "3",
                   "IV" = "4") # Cambiamos los trimestres de string a numericos

fecha_mensual_s_IMSS <- as.yearmon(paste(años_s_IMSS, mes_num_s_IMSS), "%Y %m") # Convertir meses a variable de fecha
salario_mensual_IMSS <- zoo(salario_IMSS_val, order.by = fecha_mensual_s_IMSS) # Serie mensual del Salario del IMSS
salario_trim_IMSS <- aggregate(salario_mensual_IMSS,
                          as.yearqtr,
                          mean) # Promedio por mes trimestre del Salario del IMSS
fechas_q_s_IMSS <- format(index(salario_trim_IMSS), "%YQ%q") # Formato de trimestre
fechas_q_s_IMSS <- gsub("Q", "T", fechas_q_s_IMSS)

fecha_s_ENOE <- paste0(años_s_ENOE, "T", trim_num_ENOE) # Creamos la variable fecha para la serie de tiempo del Salario de la ENOE

año_inicio_IMSS <- as.numeric(substr(fechas_q_s_IMSS[1], 1, 4)) # año de inicio
trim_inicio_IMSS <- as.numeric(substr(fechas_q_s_IMSS[1], 6, 6)) # trimestre de inicio

Salario_IMSS_ts <- ts(as.numeric(salario_trim_IMSS),
                 start = c(año_inicio_IMSS, trim_inicio_IMSS),
                 frequency = 4) # Creamos la serie de tiempo del salario del IMSS 2016T3-2025T4

año_inicio_ENOE <- as.numeric(años_s_ENOE[1]) # año de inicio 
trim_inicio_ENOE <- as.numeric(trim_num_ENOE[1]) # trimestre de inicio 

Salario_ENOE_ts <- ts(salario_ENOE_val,
             start = c(año_inicio_ENOE, trim_inicio_ENOE),
             frequency =  4) # Serie de tiempo del salario de la ENOE 2016T1 - 2025T4

Salarios_ts <- cbind(
  IMSS = Salario_IMSS_ts,
  ENOE = Salario_ENOE_ts
) %>% na.omit(Salarios_ts)

grafica_excel(Salarios_ts, colores = c("red","blue"),mostrar_leyenda = T, y_break_by = 50,date_breaks = "1 years", grosor_linea = 0.6, titulo = "Grafica 9: Salario diario nominal promedio de los asegurados del IMSS y de la ENOE", subtitulo = "Pesos por dia", nota = "Nota: para el dato del salario promedio del segundo trimestre de 2020, se uso la ETOE.", fuente = "Fuente: elaboracion propia con datos del IMSS y la ENOE (INEGI)")

# De igual forma, para que podamos comparar estos salarios con el indice de salarios del INEGI y el salario contrafactual, convertimos estas dos series en un indice base primer trimestre del 2018
# Dado que estamos trabajando con salarios reales, es necesario que el salario del IMSS y el salario de la ENOE sean salarios reales. para este fin, usamos el INPC base 2018 para deflactar los salarios.

INPC_raw <- read_excel("INPC.xlsx", 
                       sheet = "INPCT",
                               col_names = F,
                               col_types = "text") # base del INPC  

col_años_P <- 1 # Columna donde estan los años
col_trim_P <- 2 # Columna donde estan los trimestres
col_P <- 3 # Columna donde estan los datos del INPC

años_P <- unlist(INPC_raw[-1,col_años_P]) # Extraemos años, meses y el INPC
trim_P <- unlist(INPC_raw[-1,col_trim_P])
P_val <- unlist(INPC_raw[-1,col_P])
P_val <- as.numeric(P_val) # Cambiamos el INPC de string a numerico

fecha_P <- paste0(años_P, "T", trim_P)

año_inicio_P <- as.numeric(años_P[1]) # año de inicio 
trim_inicio_P <- as.numeric(trim_P[1]) # trimestre de inicio 

INPC_ts <- ts(P_val,
              start = c(año_inicio_P, trim_inicio_P),
              frequency =  4) # Serie de tiempo del salario del 2008T1 - 2025T4

Salarios_ts <- cbind(
  IMSS = Salario_IMSS_ts,
  ENOE = Salario_ENOE_ts,
  INPC = INPC_ts
) %>% na.omit(Salarios_ts)

Salarios_ts <- cbind(
  IMSS = Salario_IMSS_ts,
  ENOE = Salario_ENOE_ts,
  INPC = INPC_ts,
  `IMSS real` = Salario_IMSS_ts/INPC_ts,
  `ENOE real` = Salario_ENOE_ts/INPC_ts
) %>% na.omit(Salarios_ts)

grafica_excel(Salarios_ts[,4:5], colores = c("red","blue"),mostrar_leyenda = T, y_break_by = 0.5,date_breaks = "1 years", grosor_linea = 0.6, titulo = "Grafica 9: Salario diario real promedio de los asegurados del IMSS y de la ENOE", nota = "Nota: para el dato del salario promedio del segundo trimestre de 2020, se uso la ETOE.", fuente = "Fuente: elaboracion propia con datos del IMSS y la ENOE (INEGI)")

# Calculamos los indices al primer trimestre de 2018

base_IMSSr <- window(Salarios_ts[,4], start = c(2018,1), end = c(2018,1)) # Escogemos el valor base de cada serie del primer trimestre del 2018
base_ENOEr <- window(Salarios_ts[,5], start = c(2018,1), end = c(2018,1))
IMSSr_indice <- (Salarios_ts[,4]/ as.numeric(base_IMSSr)) * 100
ENOEr_indice <- (Salarios_ts[,5]/ as.numeric(base_ENOEr)) * 100

series_indice <- cbind(
  PIB     = series_indice[, "PIB"],
  Empleo  = series_indice[, "Empleo"],
  Capital = series_indice[, "Capital"],
  TFP = series_indice[, "TFP"],
  `Salario walrasiano` = series_indice[,"Salario walrasiano"],
  `Salario observado` = series_indice[,"Salario observado"],
  `IMSS real` = IMSSr_indice,
  `ENOE real` = ENOEr_indice
)%>% na.omit(series_indice) # Juntamos los indices

grafica_excel(series_indice[,5:8], colores = c("red","blue","#2CA02C","#FF7F0E"),mostrar_leyenda = T, grosor_linea = 0.6,y_break_by = 10,date_breaks = "1 years", titulo = "Grafica 10: Salario real observado y salario real walrasiano estimado", subtitulo = "Indice 2018-1T = 100", fuente = "Fuente: elaboracion propia con datos del INEGI")

#### 8. Interprete.








