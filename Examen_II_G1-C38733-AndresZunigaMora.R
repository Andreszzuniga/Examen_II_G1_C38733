# Paquetes

library(tidyverse)   
library(magrittr)   

# 1) Cargar datos
moras <- read_delim("moras.txt",
                    delim = ";")

moras %>% glimpse()