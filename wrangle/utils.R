haz.cero.na=function(x){
  ifelse(is.na(x),0,x)
}

## Seleccion de variables
tomar_vars <- function(df) {
  df %>% select(anio, numero, dpto, nomdpto, nper, region_4, e26, e27,                             # region_3 y region_4 traen la misma información, en region_4 está más desagregada
                e49, starts_with("e51_"), e193,
                e197_1, e201_1, e212_1, e215_1, e218_1, e221_1, e224, e224_1,                      # variables referidas a educación, no estan en la base 2010, si en las restantes bases
                f66, f70, starts_with("f71"), starts_with("f72"), 
                f73, f74, f75, f76_2, f77, f91_2, f85, f98, 
                starts_with("g126"), starts_with("g128"), starts_with("g129"), 
                starts_with("g130"), starts_with("g131"), starts_with("g133"),
                starts_with("g144"), starts_with("g134"), g142, g143, pobpcoac, pesoano)
}

## Codificacion de variables existentes
codif <- function(df) {
  df %>% 
    mutate(region_4 = if_else(((region_4 == "montevideo") | (region_4 == "montevideo ")) , 1,
                              if_else( ((region_4 == "interior urbano. localidades menores de 5mil habitantes") |
                                        (region_4 == "interior < 5000")),  3,
                                       if_else(((region_4 == "interior rural") | (region_4 == "rural")), 4,
                                               2))),
           e26 = case_when(e26 == "hombre " ~ 1, e26 == "mujer" ~ 2),
           e49 = case_when(e49 == "sí " ~ 1, e49 == "no" ~ 2),
           pobpcoac = if_else(((pobpcoac == "ocupados") | (pobpcoac == "ocupados ")), 2,
                      if_else(((pobpcoac == "desocupados") | (pobpcoac == "desocupados bt1v") | (pobpcoac == "desocupados buscan trabajo por primera vez ")), 3,
                      if_else(((pobpcoac == "desocupados propiamente dichos") | (pobpcoac == "desocupados propiamente dichos ")), 4,
                      if_else(((pobpcoac == "desocupados en seguro de paro ") | (pobpcoac == "desocupados en seguro de paro")), 5,
                      if_else(((pobpcoac == "inactivo, realiza quehaceres del hogar ") | (pobpcoac == "inactivo, realiza los quehaceres del hogar")), 6,
                      if_else(((pobpcoac == "inactivo, estudiante") | (pobpcoac == "inactivo, estudiante ")), 7,
                      if_else(((pobpcoac == "inactivo, rentista") | (pobpcoac == "inactivo, rentista ")), 8,
                      if_else(((pobpcoac == "inactivo, pensionista") | (pobpcoac == "inactivo, pensionista ")), 9,
                      if_else(((pobpcoac == "inactivo, jubilado") | (pobpcoac == "inactivo, jubilado ")), 10,
                      if_else((pobpcoac == "inactivo, otro"), 11, 
                      1)))))))))))
}

## Seleccion de nuevas variables y codificacion
nuevas_var <- function(df) {
  df %>%
    transmute(anio, numero, dpto, nomdpto, nper, 
              region = region_4,
              genero = e26,
              edad = e27, 
              cat_edad = case_when(edad < 14 ~ 0,                                                  # "menor a 14"
                                   edad >= 14 & edad <= 18 ~ 1,                                    # "entre 14 y 18"
                                   edad > 18 & edad <= 30 ~ 2,                                     # "entre 18 y 30"
                                   edad > 30 & edad <= 45 ~ 3,                                     # "entre 30 y 45"
                                   edad > 45 & edad <= 60 ~ 4,                                     # "entre 45 y 60"
                                   edad > 60 ~ 5),                                                 # "mayor a 60"
              cat_edad_2 = case_when(edad <= 13 ~ "menor 14",                                      # "menor a 14"
                                     edad >= 14 & edad <= 29 ~ "14 a 29",                          # "entre 14 y 29"
                                     edad >= 30 & edad <= 49 ~ "30 a 49",                          # "entre 30 y 49"
                                     edad >= 50 & edad <= 64 ~ "50 a 64",                          # "entre 50 y 64"
                                     edad >= 65 ~ "65 y mas"),                                     # "mayor a 65"
              en_edad_trabajar = case_when(edad >= 14 ~ 1, 
                                           edad < 14 ~ 0),                                         # personas en edad de trabajar
              concurrencia = e49, preescolar = e193, concurrencia_postgrado = e224,
              
              anios_primaria_c = e51_2, anios_primaria_e = e51_3,                                  # primaria - común/especial
              anios_ciclo_basico = e51_4, anios_bachill = e51_5, anios_bachill_tec = e51_6,        # media - ciclo básico/bachillerato/bachill. tecnológico
              anios_tecnica = e51_7, exigencia = e51_7_1, anios_mag = e51_8,                       # técnica, exigencia, magisterio o profesorado
              anios_univ = e51_9, anios_terciario_no_univ = e51_10, anios_postgrado = e51_11,      # universidad o similar, terciario no universitario, postgrado
              
              educ_primaria = e197_1, educ_media = e201_1, educ_tecnica = e212_1, educ_mag = e215_1,   # Finalizo Nivel Educativo:
              educ_univ = e218_1, educ_terciario_no_univ = e221_1, educ_postgrado = e224_1,            # 1 = "Si", 2 = "No"
         
         # Corroborar que sea sobre el nivel y no sobre el año:
              nivel_educ_alcanzado = if_else((educ_postgrado == 1), 7, if_else((educ_terciario_no_univ == 1), 6,
                                     if_else((educ_univ == 1), 5, if_else((educ_mag == 1), 4,
                                     if_else((educ_tecnica == 1), 3, if_else((educ_media == 1), 2, 
                                     if_else((educ_primaria == 1), 1, if_else((educ_primaria == 2), 9, 0)))))))),
         
         ## De acuerdo a información proporcionada por el INE (mail de Gabriel Paolillo con fecha 20/02/2018, guardado en carpeta OP - compartida),
         ## se confirma que las variables e197_1, e201_1, e212_1, e215_1, e218_1, e221_1, e224_1, hacen referencia al nivel educativo (primaria, media, técnica, etc.)
         
              trabaja = f66, cant_trabajos = f70, 
              ciuo = f71_2, ciuo_gg = if_else(nchar(ciuo) == 4, substring(ciuo, 1, 2), 
                                              if_else(nchar(ciuo) == 3, substring(ciuo, 1, 1), "NA")),
              ciiu = f72_2, division = if_else(nchar(ciiu) == 4, substring(ciiu, 1, 2), substring(ciiu, 1, 1)),
              calificados = if_else(ciuo_gg == "NA", 0,
                                    if_else((substring(ciuo_gg, 1, 1) >= 0) & (substring(ciuo_gg, 1, 1) != 9), 1, 0)),
              categoria_ocup_ppal = f73, sector_publico_ppal = f74, establecimiento_ppal = f75, 
              actividad_emp_ppal = f76_2, nro_empleados = f77, ciiu_sec = f91_2, horas_semana = f85, horas_otras = f98,
              sueldo_liq_ppal_dep = (g126_1 + g126_2 + g126_3 + g126_4 + g126_5 + g126_6 + g126_7 + g126_8 +                           # componentes de ingreso por actividad principal: 
                                       g128_1 + g129_2 + g130_1 + g131_1 + g133_1),                                                    # hasta g133 son referidos a trabajadores dependientes,
              sueldo_liq_ppal_nodep = round((g142 + g143/12 + g144_1 + g144_2_1 + g144_2_2 + g144_2_3 + g144_2_4 + g144_2_5),0),       # las restantes son para trabajadores no dependientes.                   
              pobpcoac, pesoano,
              nro_ocupados = ifelse(pobpcoac == 2, 1, 0),
              dependientes = if_else(((pobpcoac == 2) & (sueldo_liq_ppal_dep > 0)), 1, 0))
}

asignar_seccion_ciiuRev4 <- function(df){
  df %>%
    mutate(seccion = case_when((as.numeric(division) %in% 1:3) ~ "A", (as.numeric(division) %in% 5:9) ~ "B", 
                               (as.numeric(division) %in% 10:33) ~ "C", (as.numeric(division) == 35) ~ "D", 
                               (as.numeric(division) %in% 36:39) ~ "E", (as.numeric(division) %in% 41:43) ~ "F", 
                               (as.numeric(division) %in% 45:47) ~ "G", (as.numeric(division) %in% 49:53) ~ "H", 
                               (as.numeric(division) %in% 55:56) ~ "I", (as.numeric(division) %in% 58:63) ~ "J", 
                               (as.numeric(division) %in% 64:66) ~ "K", (as.numeric(division) == 68) ~ "L",
                               (as.numeric(division) %in% 69:75) ~ "M", (as.numeric(division) %in% 77:82) ~ "N",
                               (as.numeric(division) == 84) ~ "O", (as.numeric(division) == 85) ~ "P", 
                               (as.numeric(division) %in% 86:88) ~ "Q", (as.numeric(division) %in% 90:93) ~ "R", 
                               (as.numeric(division) %in% 94:96) ~ "S", (as.numeric(division) %in% 97:98) ~ "T", 
                               (as.numeric(division) == 99) ~ "U"))
}

### considerando clasificación OTU
asignar_nivel_educativo_alcanzado <- function(df) {
  df %>%
    mutate(
      nivel_educativo_alcanzado = (
        # UNIVERSIDAD O SIMILAR
        if_else(((anios_univ > 0 & anios_univ < 9) | (anios_postgrado > 0 & anios_postgrado < 9)), "6. universidad o similar",
                # MAGISTERIO O PROFESORADO
                if_else((anios_mag > 0 & anios_mag < 9), "5. magisterio o profesorado",
                        # TERCIARIO NO UNIVERSITARIO - SIN FORMACIÓN DOCENTE   
                        if_else(((anios_terciario_no_univ > 0 & anios_terciario_no_univ < 9) | 
                                   ((anios_tecnica > 0 & anios_tecnica < 9) & exigencia == 1)), "4. terciario no universitario",
                                # SECUNDARIA
                                if_else(((anios_ciclo_basico > 0 & anios_ciclo_basico < 9) | 
                                           ((anios_tecnica > 0 & anios_tecnica < 9) & exigencia %in% c(2:3))), "3. secundaria",
                                        # PRIMARIA
                                        if_else((((anios_primaria_c > 0 & anios_primaria_c < 9) | (anios_primaria_e > 0 & anios_primaria_e < 9) | 
                                                    ((anios_tecnica > 0 & anios_tecnica < 9) & exigencia == 4))), "2. primaria",
                                                "1. sin instruccion")))))),
      finalizo_postgrado = if_else(concurrencia_postgrado == 2 & educ_postgrado == 1 & (anios_postgrado > 0 & anios_postgrado < 9), 1, 0))
}

## Correccion de variable "ciuo" y "ciiu"
correc <- function(df) {
  df %>%
    mutate(ciuo = if_else(substring(ciuo, 1, 1) == 0, substring(ciuo, 2, 4), ciuo),
           ciiu = if_else(substring(ciiu, 1, 1) == 0, substring(ciiu, 2, 4), ciiu))
}