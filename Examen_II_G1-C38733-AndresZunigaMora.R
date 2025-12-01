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

# 3) Z-score 

moras <-
  moras %>%
  mutate(
    z_salario = (SALARIO - mean(SALARIO, na.rm = TRUE)) /
      sd(SALARIO, na.rm = TRUE),
    z_edad    = (EDAD    - mean(EDAD,    na.rm = TRUE)) /
      sd(EDAD,    na.rm = TRUE),
    
    atipico_salario = case_when(
      is.na(z_salario)           ~ NA_character_,
      abs(z_salario) > 1.96      ~ "Si",
      TRUE                       ~ "No"
    ),
    atipico_edad = case_when(
      is.na(z_edad)              ~ NA_character_,
      abs(z_edad) > 1.96         ~ "Si",
      TRUE                       ~ "No"
    )
  )
# 4) Porcentaje de Na's

faltantes_por_col <-
  moras %>%
  summarise(
    across(
      everything(),
      ~ mean(is.na(.x)) * 100,
      .names = "pct_na_{.col}"
    )
  )

faltantes_por_col
# 5) Imputación de Na

imputar_media <- function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
}

moda <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

imputar_moda <- function(x) {
  ifelse(is.na(x), moda(x), x)
}

moras_imp <-
  moras %>%
  mutate(

    SALARIO_imp = imputar_media(SALARIO),
    EDAD_imp    = imputar_media(EDAD),

    SEXO_imp                 = imputar_moda(SEXO),
    TIPO_ASEGURAMIENTO_imp   = imputar_moda(TIPO_ASEGURAMIENTO),
    SECTOR_imp               = imputar_moda(SECTOR),
    INDICADOR_ACTIVO_imp     = imputar_moda(INDICADOR_ACTIVO),
    INDICADOR_EXTRANJERO_imp = imputar_moda(INDICADOR.EXTRANJERO),
    INDICADOR_MOROSO_imp     = imputar_moda(INDICADOR_MOROSO)
  )
# 6) Gráficos


moras_imp %>%
  ggplot(aes(x = SEXO_imp)) +
  geom_bar() +
  labs(
    title = "Cantidad de asegurados por sexo",
    x = "Sexo",
    y = "Frecuencia"
  )

moras_imp %>%
  ggplot(aes(x = TIPO_ASEGURAMIENTO_imp)) +
  geom_bar() +
  coord_flip() +
  labs(
    title = "Cantidad por tipo de aseguramiento",
    x = "Tipo de aseguramiento",
    y = "Frecuencia"
  )
# 7) Porcentaje de morosos categóricas

porc_moroso_sexo <-
  moras_imp %>%
  group_by(SEXO_imp, INDICADOR_MOROSO_imp) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(porc = 100 * n / sum(n))

porc_moroso_sexo

porc_moroso_sexo %>%
  ggplot(aes(x = SEXO_imp,
             y = porc,
             fill = INDICADOR_MOROSO_imp)) +
  geom_col(position = "dodge") +
  labs(
    title = "Porcentaje de morosos por sexo",
    x = "Sexo",
    y = "Porcentaje",
    fill = "Moroso"
  )
# 8) -

promedios_num <-
  moras_imp %>%
  group_by(INDICADOR_MOROSO_imp) %>%
  summarise(
    salario_mean = mean(SALARIO_imp, na.rm = TRUE),
    edad_mean    = mean(EDAD_imp,    na.rm = TRUE),
    .groups = "drop"
  )


promedios_num %>%
  ggplot(aes(x = INDICADOR_MOROSO_imp,
             y = salario_mean,
             fill = INDICADOR_MOROSO_imp)) +
  geom_col() +
  labs(
    title = "Salario promedio según morosidad",
    x = "Moroso",
    y = "Salario promedio"
  ) +
  guides(fill = "none")

promedios_num %>%
  ggplot(aes(x = INDICADOR_MOROSO_imp,
             y = edad_mean,
             fill = INDICADOR_MOROSO_imp)) +
  geom_col() +
  labs(
    title = "Edad promedio según morosidad",
    x = "Moroso",
    y = "Edad promedio"
  ) +
  guides(fill = "none")