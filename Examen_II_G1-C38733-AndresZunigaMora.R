# Paquetes

library(tidyverse)   
library(magrittr)   

# 1) Cargar datos
moras <- read_delim("moras.txt",
                    delim = ",")

moras %>% glimpse()
# 2) Resumen de 5 números + promedio 

resumen_numericas <-
  moras %>%
  summarise(
    salario_min   = min(SALARIO, na.rm = TRUE),
    salario_q1    = quantile(SALARIO, 0.25, na.rm = TRUE),
    salario_med   = median(SALARIO, na.rm = TRUE),
    salario_q3    = quantile(SALARIO, 0.75, na.rm = TRUE),
    salario_max   = max(SALARIO, na.rm = TRUE),
    salario_mean  = mean(SALARIO, na.rm = TRUE),
    
    edad_min      = min(EDAD, na.rm = TRUE),
    edad_q1       = quantile(EDAD, 0.25, na.rm = TRUE),
    edad_med      = median(EDAD, na.rm = TRUE),
    edad_q3       = quantile(EDAD, 0.75, na.rm = TRUE),
    edad_max      = max(EDAD, na.rm = TRUE),
    edad_mean     = mean(EDAD, na.rm = TRUE)
  )

resumen_numericas

