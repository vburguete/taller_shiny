
# Cargar librerias
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}
if(!require(readstata13)){
  install.packages("readstata13")
  library(readstata13)
}
if(!require(readxl)){
  install.packages("readxl")
  library(readxl)
}

###### 0. Cargar funciones
source('src/utils.R')

###### 1. Preparacion de la base

## a. Importar los datos
path <- file.path('data', 'externas')
files <- c("p_2011_terceros.dta",           # 2011                                       # archivos .dta 
           "p_2012_terceros.dta",           # 2012                                       
           "p_2013_terceros.dta",           # 2013                                       
           "p_2014_terceros.dta",           # 2014                                       
           "P_2015_Terceros.dat",           # 2015                                       # archivos .dat
           "Personas_2016_terceros.dat",    # 2016                                       
           "P_2017_Terceros.dat",           # 2017                                       # actualización 14/08/2018
           "P_2018_TERCEROS.dat")           # 2018                                       # actualización 02/04/2019

base <- sapply(str_c("df", 11:18), function(x) NULL)                                     # Crea lista con 8 elementos nulos nombrados

for(i in 1:8) {
  
  ifelse(i == 1,                                                                         # año: 2011
         base[[i]] <- file.path(path, files[i]) %>%                                      # La base 2011, tiene dos variables para el ciiu 
           read.dta13 %>%                                                                # (f72_2 y f72_ciiu4_4), f72_ciiu4_4 es la adecuada,
           tomar_vars %>%                                                                # lo mismo sucede para el ciuo
           select(-f71_2, -f72_2) %>% 
           rename(f71_2 = f71_ciuo_08_4, 
                  f72_2 = f72_ciiu4_4),
         
         ifelse(i <= 4,                                                                  # años: 2012, 2013, 2014
                base[[i]] <- file.path(path, files[i]) %>% 
                  read.dta13 %>% 
                  tomar_vars,
                
                ifelse(i == 7,                                                           # año: 2017
                       base[[i]] <- file.path(path, files[i]) %>%
                         read_tsv %>% 
                         tomar_vars %>% 
                         as.data.frame,
                       
                       base[[i]] <- file.path(path, files[i]) %>%                        # años: 2015, 2016, 2018
                         read.delim %>%
                         tomar_vars)
         ))
}


## b. Codificacion de variables: 

# Se codifican los niveles de la variables para las ECH 2011 a 2014, para homogeneizar las bases.
datos <- rbind(base[["df11"]], 
               base[["df12"]], 
               base[["df13"]], 
               base[["df14"]]) %>%
  codif


## c. Creacion de nuevas variables:
## incorporacion bases y cambios en variables
datos <- datos %>% 
  rbind(base[["df15"]], 
        base[["df16"]], 
        base[["df17"]], 
        base[["df18"]]) %>% 
  nuevas_var %>%
  asignar_seccion_ciiuRev4 %>%
  asignar_nivel_educativo_alcanzado


###### 2. Correccion lectura ciuo y ciiu
datos <- datos %>%
  correc

###### 3. Incorporacion de informacion sobre el ciiu y secciones
path <- file.path('data', 'internas')

datos <- datos %>% 
  left_join(file.path(path, "ciiu.csv") %>%
              read_tsv %>%
              transmute(ciiu = as.character(CIIU), 
                        clasificacion = Clasificacion),
            by = "ciiu") %>% 
  
  left_join( file.path(path, "ciiu_seccion_tildes.csv") %>%
               read.csv(sep = ";") %>% 
               mutate(seccion = as.character(seccion)) %>% 
               select(seccion, descripcion) %>% 
               filter(seccion != "Fuente:"), 
            by = "seccion")

#saveRDS(datos, 'app/data/internas/datos.rds')

# Generar rds para taller
bd <- datos %>% 
  mutate(genero = case_when(genero == 1 ~ "hombre", TRUE ~ "mujer"),
         nomdpto = case_when(nomdpto == "TREINTA Y T" ~ "TREINTA Y TRES",
                             TRUE ~ nomdpto),
         dpto = str_to_title(nomdpto)) %>% 
  select(anio, numero, ponderador = pesoano, 
         departamento = dpto, 
         genero, 
         edad, 
         edad_categoria = cat_edad_2) %>% 
  group_by(numero) %>% 
  do(mutate(., nro = seq_along(numero))) %>% 
  mutate(nro = str_pad(nro, 3, "left", "0")) %>% 
  unite("id", numero, nro, sep = "") %>% 
  select(id, everything())

#saveRDS(bd, 'data/internas/bd.rds')

