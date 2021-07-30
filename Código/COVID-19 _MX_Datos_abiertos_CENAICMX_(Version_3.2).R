### Limpiar ambiente ----
remove(list = ls())


### Setup general ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen = 9999)

### Paquetes ----
library(pacman)
p_load(av, cowplot, ggforce, ggmap, ggrepel, gifski, glue, gpclib, ggtext, Hmisc, janitor, lubridate, openxlsx, RColorBrewer, rcartocolor, readxl, rtweet, rworldmap, rworldxtra, scales, sf, shadowtext, tesseract, tidyverse, tm, treemapify, viridis, wesanderson, zoo, plotly)


### Definir cortes de datos ----
texto_corte_cifras <-   str_c("Última actualización: ", format(Sys.Date(), '%d de %B de %Y.'))

texto_corte_cifras

elaboracion <-  Sys.Date()

fecha_corte <-  Sys.Date() - 5
fecha_corte

fecha_inicio <-  as.Date("2020-01-18")
fecha_inicio

fecha_inicio_defunciones <- as.Date("2020-02-23")
fecha_inicio_defunciones


### Importar y procesar datos abiertos ----
# Datos diarios
mx_datos <- 
  read_csv(file.choose()) %>% 
  clean_names()

## Convertir valores numéricos en texto ----
mx_datos <- 
  mx_datos %>% 
  mutate(entidad_res = case_when(entidad_res == "01" ~ "Aguascalientes",
                                 entidad_res == "02" ~ "Baja California",
                                 entidad_res == "03" ~ "Baja California Sur",
                                 entidad_res == "04" ~ "Campeche",
                                 entidad_res == "05" ~ "Coahuila",
                                 entidad_res == "06" ~ "Colima",
                                 entidad_res == "07" ~ "Chiapas",
                                 entidad_res == "08" ~ "Chihuahua",
                                 entidad_res == "09" ~ "Ciudad de México",
                                 entidad_res == "10" ~ "Durango",
                                 entidad_res == "11" ~ "Guanajuato",
                                 entidad_res == "12" ~ "Guerrero",
                                 entidad_res == "13" ~ "Hidalgo",
                                 entidad_res == "14" ~ "Jalisco",
                                 entidad_res == "15" ~ "México",
                                 entidad_res == "16" ~ "Michoacán",
                                 entidad_res == "17" ~ "Morelos",
                                 entidad_res == "18" ~ "Nayarit",
                                 entidad_res == "19" ~ "Nuevo León",
                                 entidad_res == "20" ~ "Oaxaca",
                                 entidad_res == "21" ~ "Puebla",
                                 entidad_res == "22" ~ "Querétaro",
                                 entidad_res == "23" ~ "Quintana Roo",
                                 entidad_res == "24" ~ "San Luis Potisí",
                                 entidad_res == "25" ~ "Sinaloa",
                                 entidad_res == "26" ~ "Sonora",
                                 entidad_res == "27" ~ "Tabasco",
                                 entidad_res == "28" ~ "Tamaulipas",
                                 entidad_res == "29" ~ "Tlaxcala",
                                 entidad_res == "30" ~ "Veracruz",
                                 entidad_res == "31" ~ "Yucatán",
                                 entidad_res == "32" ~ "Zacatecas",
                                 entidad_res == "36" ~ "Estados Unidos Mexicanos",
                                 entidad_res == "97" ~ "No aplica",
                                 entidad_res == "98" ~ "Se ignora",
                                 entidad_res == "99" ~ "No especificado"),
         fallecio = ifelse(is.na(fecha_def), "No", "Sí"), 
         rango_edad = case_when(edad <= 6 ~ "< 6 años",
                                edad > 6 & edad <= 11 ~ "7-11 años",
                                edad > 11 & edad <= 17 ~ "12-17 años",
                                edad > 17 & edad <= 29 ~ "18-29 años",
                                edad > 29 & edad <= 44 ~ "30-44 años",
                                edad > 44 & edad <= 59 ~ "45-59 años",
                                edad > 59 ~ "> 60 años"))


###Clasificacion de casos positivos nacional----
clasificacion_casos_nac <- 
  mx_datos %>% 
  group_by(fecha_ingreso) %>% 
  summarise(casos_positivos = sum(clasificacion_final == 1, 
                                  clasificacion_final == 2, 
                                  clasificacion_final == 3),
            casos_negativos = sum(clasificacion_final == 7),
            casos_sospechosos = sum(clasificacion_final == 6))%>%
  mutate(positivos_acumulados = cumsum(casos_positivos),
         negativos_acumulados = cumsum(casos_negativos),
         sospechosos_acumulados = cumsum (casos_sospechosos),
         media_movil_casos_positivos = round(rollmean(casos_positivos, k = 7, align = 'right', fill = NA), 3),
         media_movil_casos_negativos = round(rollmean(casos_negativos, k = 7, align = 'right', fill = NA), 3),
         media_movil_casos_sospechosos = round(rollmean(casos_sospechosos, k = 7, align = 'right', fill = NA), 3))%>%
  rename(fecha = fecha_ingreso) 


corte_final_clasificacion_casos_nac <- 
  clasificacion_casos_nac%>% 
  filter(fecha == fecha_corte)

corte_final_clasificacion_casos_nac_2 <- 
  clasificacion_casos_nac%>% 
  filter(fecha == max(fecha))

###Clasificacion de defunciones nacional----
clasificacion_defunciones_nac <- 
  mx_datos %>% 
  group_by(fecha_def) %>% 
  filter(fallecio == "Sí") %>%
  summarise(defunciones = sum(clasificacion_final == 1, 
                              clasificacion_final == 2, 
                              clasificacion_final == 3),
            defunciones_sospechosas = sum(clasificacion_final == 6),
            defunciones_negativas = sum(clasificacion_final == 7))%>%
  mutate(defunciones_acumuladas = cumsum(defunciones),
         defunciones_sospechosas_acumuladas = cumsum(defunciones_sospechosas),
         defunciones_negativas_acumuladas = cumsum(defunciones_negativas),
         media_movil_defunciones = round(rollmean(defunciones, k = 7, align = 'right', fill = NA), 3),
         media_movil_defunciones_sospechosas = round(rollmean(defunciones_sospechosas, k = 7, align = 'right', fill = NA), 3),
         media_movil_defunciones_negativas = round(rollmean(defunciones_negativas, k = 7, align = 'right', fill = NA), 3))%>%
  rename(fecha = fecha_def) 


corte_final_clasificacion_defunciones_nac <- 
  clasificacion_defunciones_nac%>% 
  filter(fecha == fecha_corte)


corte_final_clasificacion_defunciones_nac_2 <- 
  clasificacion_defunciones_nac%>% 
  filter(fecha == max(fecha))

###Casos positivos nacional----
casos_positivos_nac <- 
  mx_datos %>% 
  group_by(fecha_ingreso) %>% 
  summarise(casos_positivos = sum(clasificacion_final == 1, 
                                  clasificacion_final == 2, 
                                  clasificacion_final == 3))%>%
  rename(fecha = "fecha_ingreso") 

corte_final_positivos_nac <- 
  casos_positivos_nac%>% 
  filter(fecha == fecha_corte)

###Defunciones nacional----
defunciones_nac <- 
  mx_datos%>%
  group_by(fecha_def) %>% 
  filter(fallecio == "Sí") %>%
  summarise(defunciones = sum(clasificacion_final == 1, 
                              clasificacion_final == 2, 
                              clasificacion_final == 3))%>%
  rename(fecha = "fecha_def") 

corte_final_defunciones_nac <- 
  defunciones_nac%>% 
  filter(fecha == fecha_corte)


###Positivos y defunciones----
positivos_defunciones_nac <- 
  casos_positivos_nac%>%
  left_join(defunciones_nac, by = c("fecha"))

positivos_defunciones_nac$defunciones[is.na(positivos_defunciones_nac$defunciones)] <-  0

positivos_defunciones_nac$casos_positivos[is.na(positivos_defunciones_nac$casos_positivos)] <-  0

positivos_defunciones_nac <- 
  positivos_defunciones_nac%>% 
  mutate(positivos_acumulados = cumsum(casos_positivos),
         defunciones_acumuladas = cumsum(defunciones),
         casos_activos =rollsum(casos_positivos, k = 14, align = 'right', fill = NA))

corte_final_positivos_defunciones_nac <- 
  positivos_defunciones_nac%>% 
  filter(fecha == fecha_corte)


###Letalidad nacional----
letalidad_nac <- 
  positivos_defunciones_nac%>%   
  mutate(casos_positivos_sem = rollsum(casos_positivos, k = 7, align = 'right', fill = NA),
         defunciones_sem = rollsum(defunciones, k = 7, align = 'right', fill = NA),
         letalidad_acumulada = round((defunciones_acumuladas / positivos_acumulados)*100,3),
         letalidad_sem = round((defunciones_sem / casos_positivos_sem)*100,3))

corte_final_letalidad_nac <- 
  letalidad_nac%>% 
  filter(fecha == max(fecha))

###Pruebas nacional----
pruebas_nac <- 
  mx_datos%>%
  group_by(fecha_ingreso) %>% 
  summarise(positivos_laboratorio = sum(resultado_lab == 1),
            negativos_laboratorio = sum(resultado_lab == 2),
            positivos_antigeno = sum(resultado_antigeno == 1),
            negativos_antigeno = sum(resultado_antigeno == 2),
            resultado_pendiente_laboratorio = sum(resultado_lab == 3),
            total_antigeno = positivos_antigeno + negativos_antigeno,
            total_pruebas = positivos_laboratorio + negativos_laboratorio + total_antigeno,
            positividad = round(((positivos_laboratorio + positivos_antigeno) / total_pruebas)*100, 2))%>%
  mutate(positivos_laboratorio_acumulados = cumsum(positivos_laboratorio),
         negativos_laboratorio_acumulados = cumsum(negativos_laboratorio),
         pruebas_acumuladas = cumsum(total_pruebas),
         positivos_antigeno_acumulados = cumsum(positivos_antigeno),
         negativos_antigeno_acumulados = cumsum(negativos_antigeno),
         total_antigeno_acumulados = cumsum(total_antigeno),
         total_pruebas_acumuladas = cumsum(total_pruebas),
         media_movil_positivos_laboratorio = round(rollmean(positivos_laboratorio, k = 7, align = 'right', fill = NA), 3),
         media_movil_negativos_laboratorio = round(rollmean(negativos_laboratorio, k = 7, align = 'right', fill = NA), 3),
         media_movil_positividad = round(rollmean(positividad, k = 7, align = 'right', fill = NA), 2),
         media_movil_total_pruebas = round(rollmean(total_pruebas, k = 7, align = 'right', fill = NA), 2),
         positivos_laboratorio_sem = rollsum(positivos_laboratorio, k = 7, align = 'right', fill = NA),
         negativos_laboratorio_sem = rollsum(negativos_laboratorio, k = 7, align = 'right', fill = NA),
         positivos_antigeno_sem= rollsum(positivos_antigeno, k = 7, align = 'right', fill = NA),
         negativos_antigeno_sem = rollsum(negativos_antigeno, k = 7, align = 'right', fill = NA),
         total_antigeno_sem = positivos_antigeno_sem + negativos_antigeno_sem,
         total_pruebas_sem = positivos_laboratorio_sem + negativos_laboratorio_sem + total_antigeno_sem,
         positividad_sem = round(((positivos_laboratorio_sem + positivos_antigeno_sem) / total_pruebas_sem)*100, 2),
         positividad_acumulada = round(((positivos_laboratorio_acumulados+positivos_antigeno_acumulados)/ total_pruebas_acumuladas)*100,2))%>%
  rename(fecha = fecha_ingreso) 

corte_final_pruebas_nac <- 
  pruebas_nac%>% 
  filter(fecha == fecha_corte)

corte_final_pruebas_nac_2 <- 
  pruebas_nac%>% 
  filter(fecha == max(fecha))


#####Tablas estatales--------  
###Clasificacion de casos positivos estatal----
clasificacion_casos_est <- 
  mx_datos %>% 
  group_by(entidad_res, fecha_ingreso) %>%
  summarise(casos_positivos = sum(clasificacion_final == 1, 
                                clasificacion_final == 2, 
                                clasificacion_final == 3),
          casos_negativos = sum(clasificacion_final == 7),
          casos_sospechosos = sum(clasificacion_final == 6))%>%
  mutate(positivos_acumulados = cumsum(casos_positivos),
         negativos_acumulados = cumsum(casos_negativos),
         sospechosos_acumulados = cumsum (casos_sospechosos),
         media_movil_casos_positivos = round(rollmean(casos_positivos, k = 7, align = 'right', fill = NA), 3),
         media_movil_casos_negativos = round(rollmean(casos_negativos, k = 7, align = 'right', fill = NA), 3),
         media_movil_casos_sospechosos = round(rollmean(casos_sospechosos, k = 7, align = 'right', fill = NA), 3))%>%
  rename(fecha = fecha_ingreso) 


corte_final_clasificacion_casos_est <- 
  clasificacion_casos_est%>% 
  filter(fecha == fecha_corte)


###Clasificacion de defunciones estatal----
clasificacion_defunciones_est <- 
  mx_datos %>% 
  group_by(entidad_res, fecha_def) %>% 
  filter(fallecio == "Sí") %>%
  summarise(defunciones = sum(clasificacion_final == 1, 
                              clasificacion_final == 2, 
                              clasificacion_final == 3),
            defunciones_sospechosas = sum(clasificacion_final == 6),
            defunciones_negativas = sum(clasificacion_final == 7))%>%
  mutate(defunciones_acumuladas = cumsum(defunciones),
         defunciones_sospechosas_acumuladas = cumsum(defunciones_sospechosas),
         defunciones_negativas_acumuladas = cumsum(defunciones_negativas),
         media_movil_defunciones = round(rollmean(defunciones, k = 7, align = 'right', fill = NA), 3),
         media_movil_defunciones_sospechosas = round(rollmean(defunciones_sospechosas, k = 7, align = 'right', fill = NA), 3),
         media_movil_defunciones_negativas = round(rollmean(defunciones_negativas, k = 7, align = 'right', fill = NA), 3))%>%
  rename(fecha = fecha_def) 

corte_final_clasificacion_defunciones_est <- 
  clasificacion_defunciones_est%>% 
  filter(fecha == fecha_corte)


###Casos positivos estatal----
casos_positivos_est <- 
  mx_datos %>% 
  group_by(entidad_res, fecha_ingreso) %>% 
  summarise(casos_positivos = sum(clasificacion_final == 1, 
                                  clasificacion_final == 2, 
                                  clasificacion_final == 3))%>%
  rename(fecha = "fecha_ingreso") 

corte_final_casos_positivos_est <- 
  casos_positivos_est%>% 
  filter(fecha == fecha_corte)

###Defunciones estatal----
defunciones_est <- 
  mx_datos%>%
  group_by(entidad_res, fecha_def) %>% 
  filter(fallecio == "Sí") %>%
  summarise(defunciones = sum(clasificacion_final == 1, 
                              clasificacion_final == 2, 
                              clasificacion_final == 3)) %>%
  rename(fecha = "fecha_def") 

corte_final_defunciones_est <- 
  defunciones_est%>% 
  filter(fecha == fecha_corte)



###Positivos y defunciones ----
positivos_defunciones_est <- 
  casos_positivos_est%>%
  left_join(defunciones_est, by = c("entidad_res", "fecha"))

positivos_defunciones_est$defunciones[is.na(positivos_defunciones_est$defunciones)] <-  0

positivos_defunciones_est$casos_positivos[is.na(positivos_defunciones_est$casos_positivos)] <-  0

positivos_defunciones_est <- 
  positivos_defunciones_est%>% 
  mutate(positivos_acumulados = cumsum(casos_positivos),
         defunciones_acumuladas = cumsum(defunciones),
         casos_activos =rollsum(casos_positivos, k = 14, align = 'right', fill = NA)) 


corte_final_positivos_defunciones_est <- 
  positivos_defunciones_est%>% 
  filter(fecha == fecha_corte)


corte_final_positivos_defunciones_est_2 <- 
  positivos_defunciones_est%>% 
  filter(fecha == max(fecha))

###Letalidad estatal----
letalidad_est <- 
  positivos_defunciones_est%>%   
  mutate(casos_positivos_sem = rollsum(casos_positivos, k = 7, align = 'right', fill = NA),
         defunciones_sem = rollsum(defunciones, k = 7, align = 'right', fill = NA),
         letalidad_acumulada = round((defunciones_acumuladas / positivos_acumulados)*100,3),
         letalidad_sem = round((defunciones_sem / casos_positivos_sem)*100,3),
         media_movil_casos_positivos = round(rollmean(casos_positivos, k = 7, align = 'right', fill = NA), 3),
         media_movil_defunciones = round(rollmean(defunciones, k = 7, align = 'right', fill = NA), 3)) 

corte_final_letalidad_est <- 
  letalidad_est%>% 
  filter(fecha == max(fecha))



corte_final_letalidad_est_2 <- 
  letalidad_est%>% 
  filter(fecha == fecha_corte)


###Pruebas estatal----
pruebas_est <- 
  mx_datos%>%
  group_by(entidad_res, fecha_ingreso) %>% 
  summarise(positivos_laboratorio = sum(resultado_lab == 1),
            negativos_laboratorio = sum(resultado_lab == 2),
            positivos_antigeno = sum(resultado_antigeno == 1),
            negativos_antigeno = sum(resultado_antigeno == 2),
            resultado_pendiente_laboratorio = sum(resultado_lab == 3),
            total_antigeno = positivos_antigeno + negativos_antigeno,
            total_pruebas = positivos_laboratorio + negativos_laboratorio + total_antigeno,
            positividad = round(((positivos_laboratorio + positivos_antigeno) / total_pruebas)*100, 2))%>%
  mutate(positivos_laboratorio_acumulados = cumsum(positivos_laboratorio),
         negativos_laboratorio_acumulados = cumsum(negativos_laboratorio),
         pruebas_acumuladas = cumsum(total_pruebas),
         positivos_antigeno_acumulados = cumsum(positivos_antigeno),
         negativos_antigeno_acumulados = cumsum(negativos_antigeno),
         total_antigeno_acumulados = cumsum(total_antigeno),
         total_pruebas_acumuladas = cumsum(total_pruebas),
         media_movil_positivos_laboratorio = round(rollmean(positivos_laboratorio, k = 7, align = 'right', fill = NA), 3),
         media_movil_negativos_laboratorio = round(rollmean(negativos_laboratorio, k = 7, align = 'right', fill = NA), 3),
         media_movil_positividad = round(rollmean(positividad, k = 7, align = 'right', fill = NA), 2),
         media_movil_total_pruebas = round(rollmean(total_pruebas, k = 7, align = 'right', fill = NA), 2),
         positivos_laboratorio_sem = rollsum(positivos_laboratorio, k = 7, align = 'right', fill = NA),
         negativos_laboratorio_sem = rollsum(negativos_laboratorio, k = 7, align = 'right', fill = NA),
         positivos_antigeno_sem= rollsum(positivos_antigeno, k = 7, align = 'right', fill = NA),
         negativos_antigeno_sem = rollsum(negativos_antigeno, k = 7, align = 'right', fill = NA),
         total_antigeno_sem = positivos_antigeno_sem + negativos_antigeno_sem,
         total_pruebas_sem = positivos_laboratorio_sem + negativos_laboratorio_sem + total_antigeno_sem,
         positividad_sem = round(((positivos_laboratorio_sem + positivos_antigeno_sem) / total_pruebas_sem)*100, 2),
         positividad_acumulada = round(((positivos_laboratorio_acumulados+positivos_antigeno_acumulados)/ total_pruebas_acumuladas)*100,2))%>%
  rename(fecha = fecha_ingreso) 

corte_final_pruebas_est <- 
  pruebas_est%>% 
  filter(fecha == max(fecha))

corte_final_pruebas_est_2 <- 
  pruebas_est%>% 
  filter(fecha == fecha_corte)

casos_positivos_edad_nac <- 
  mx_datos %>%
  group_by(rango_edad, fecha_ingreso) %>%
  filter(fallecio == "No") %>%
  summarise(casos_positivos = sum(clasificacion_final == 1, 
                                  clasificacion_final == 2, 
                                  clasificacion_final == 3)) %>%
  mutate(media_movil_casos_positivos = round(rollmean(casos_positivos, k = 14, align = 'right', fill = NA), 3)) %>%
  rename(fecha = fecha_ingreso)


casos_positivos_edad_nac$rango_edad <- factor(casos_positivos_edad_nac$rango_edad, levels = c("< 6 años", 
                                                                                              "7-11 años",
                                                                                              "12-17 años", 
                                                                                              "18-29 años", 
                                                                                              "30-44 años", 
                                                                                              "45-59 años", 
                                                                                              "> 60 años"))


defunciones_edad_nac <- 
  mx_datos %>%
  group_by(rango_edad, fecha_def) %>%
  filter(fallecio == "Sí") %>%
  summarise(casos_positivos = sum(clasificacion_final == 1, 
                                  clasificacion_final == 2, 
                                  clasificacion_final == 3)) %>%
  mutate(media_movil_casos_positivos = round(rollmean(casos_positivos, k = 14, align = 'right', fill = NA), 3)) %>%
  rename(fecha = fecha_def)


defunciones_edad_nac$rango_edad <- factor(defunciones_edad_nac$rango_edad, levels = c("< 6 años", 
                                                                                      "7-11 años",
                                                                                      "12-17 años", 
                                                                                      "18-29 años", 
                                                                                      "30-44 años", 
                                                                                      "45-59 años", 
                                                                                      "> 60 años"))

# Grafica_16: Positivos acumulados estados----
ggplot(clasificacion_casos_nac, aes(x=fecha)) +
  geom_line(aes(y = positivos_acumulados), size=2, colour="#2A5783") +
  geom_point(data = corte_final_clasificacion_casos_nac_2, aes(x = max(fecha), y = positivos_acumulados), color ="#2A5783", size = 4) +
  geom_label(data = corte_final_clasificacion_casos_nac_2, aes(x = max(fecha), y = positivos_acumulados, label =  comma(positivos_acumulados, accuracy = 1)),
             size = 6.5, 
             family = "Source Sans Pro",
             fontface = "bold",
             color = "#2A5783",
             hjust = 0.5,
             vjust = 1) +
  labs(title="Casos positivos a SARS-CoV-2 acumulados* | Nacional", 
       subtitle="La línea azul representa los casos positivos acumulados a nivel nacional.", 
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
       x="Fecha", 
       y="Positivos acumulados") +
  scale_y_continuous(labels = label_comma(accuracy = 1)) +
  scale_x_date(date_breaks = "10 day", 
               limits = c(fecha_inicio, NA)) +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_line(linetype = 3, color = "white"), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 12, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 1, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) 
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/AA1Positivos_acumulados_nacional_", ".png"), dpi = 150, width = 18, height = 9)



# Grafica_17: Defunciones acumuladas estados----
ggplot(clasificacion_defunciones_nac, aes(x=fecha)) +
  geom_line(aes(y = defunciones_acumuladas), size=2, colour="gray35") +
  geom_point(data = corte_final_clasificacion_defunciones_nac_2, aes(x = max(fecha), y = defunciones_acumuladas), color ="gray35", size = 4) +
  geom_label(data = corte_final_clasificacion_defunciones_nac_2, aes(x = max(fecha), y = defunciones_acumuladas, label =  comma(defunciones_acumuladas, accuracy = 1)),
             size = 6.5, 
             family = "Source Sans Pro",
             fontface = "bold",
             color = "gray35",
             hjust = 0.5,
             vjust = 1) +
  labs(title="Defunciones asociadas a SARS-CoV-2 acumuladas* | Nacional", 
       subtitle="La línea gris representa las defunciones acumuladas a nivel nacional.", 
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
       x="Fecha", 
       y="Defunciones acumuladas") +
  scale_y_continuous(labels = label_comma(accuracy = 1)) +
  scale_x_date(date_breaks = "10 day", 
               limits = c(fecha_inicio, NA)) +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_line(linetype = 3, color = "white"), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 12, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 1, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) +
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/AA2Defunciones_acumuladas_nacional_", ".png"), dpi = 150, width = 18, height = 9)



# Grafica 5_Positivos nacional-----
ggplot(clasificacion_casos_nac, aes(x=fecha)) +
  geom_bar(stat="identity", aes(y=casos_positivos), fill="#2A5783") +
  geom_line( aes(y=media_movil_casos_positivos), size=2, colour="red") +
  geom_point(data = corte_final_clasificacion_casos_nac, aes(x = fecha, y = media_movil_casos_positivos), color = "red", size = 3) +
  geom_label(data = corte_final_clasificacion_casos_nac, aes(x = fecha, y = media_movil_casos_positivos, label = comma(media_movil_casos_positivos)),
             size = 8, 
             family = "Source Sans Pro",
             fontface = "bold",
             color = "red",
             hjust = 1) +
  labs(title="Casos positivos a SARS-CoV-2 | Nacional", 
       subtitle="Las barras de color azul indican la incidencia diaria de casos positivos a SARS-CoV-2.\nLa línea roja representa el ajuste de media móvil de 7 días que suaviza los datos para ese periodo. Debido al retraso de datos, se omiten los últimos 5 días.",
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
       x="Fecha", 
       y="Casos positivos por día") +
  scale_y_continuous(limits = c(0,NA),
                     labels = label_comma(accuracy = 1)) +
  scale_x_date(date_breaks = "13 day", 
               limits = c(fecha_inicio,fecha_corte), expand = c(0, 0)) +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid= element_line(linetype = 3, color = "white"), 
        panel.grid.major.y = element_line(linetype = 3, color = "grey80"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 12, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 0, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) +
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/AA3Positivos_nacional_", ".png"), dpi = 150, width = 18, height = 9)


# Grafica 4_Defunciones nacional -----
ggplot(clasificacion_defunciones_nac, aes(x=fecha)) +
  geom_bar(stat="identity", aes(y=defunciones), fill="grey35") +
  geom_line( aes(y=media_movil_defunciones), size=2, colour="red") +
  geom_point(data = corte_final_clasificacion_defunciones_nac, aes(x = fecha, y = media_movil_defunciones), color = "red", size = 3) +
  geom_label(data = corte_final_clasificacion_defunciones_nac, aes(x = fecha, y = media_movil_defunciones, label = comma(media_movil_defunciones)),
             size = 8, 
             family = "Source Sans Pro",
             fontface = "bold",
             color = "red",
             hjust = 1) +
  labs(title="Defunciones asociadas a SARS-CoV-2 | Nacional", 
       subtitle="Las barras de color gris indican la incidencia diaria de defunciones asociadas a SARS-CoV-2.\nLa línea roja representa el ajuste de media móvil de 7 días que suaviza los datos para ese periodo. Debido al retraso de datos, se omiten los últimos 5 días.",
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)),
       x="Fecha", 
       y="Defunciones por día") +
  scale_y_continuous(limits = c(0,NA),
                     labels = label_comma(accuracy = 1)) +
  scale_x_date(date_breaks = "13 day", 
               limits = c(fecha_inicio_defunciones, fecha_corte), expand = c(0,0)) +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid= element_line(linetype = 3, color = "white"), 
        panel.grid.major.y = element_line(linetype = 3, color = "grey80"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 12, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 0, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) +
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/AA4Defunciones_nacional", ".png"), dpi = 150, width = 18, height = 9)


# Grafica 5_Casos activos nacional-----
ggplot(positivos_defunciones_nac, aes(x=fecha)) +
  geom_line( aes(y=casos_activos), size=2, colour="royalblue4") +
  geom_point(data = corte_final_positivos_defunciones_nac, aes(x = fecha, y = casos_activos), color = "royalblue4", size = 3) +
  geom_label(data = corte_final_positivos_defunciones_nac, aes(x = fecha, y = casos_activos, label = comma(casos_activos)),
             size = 8, 
             family = "Source Sans Pro",
             fontface = "bold",
             color = "royalblue4",
             hjust = 1) +
  labs(title="Casos activos estimados | Nacional", 
       subtitle="La línea de color azul indica los casos activos estimados. Se consideran casos activos a aquellos casos que cuyo resultado positivo\na SARS-CoV-2 ocurrió durante los últimos 14 días. Debido al retraso de datos, se omiten los últimos 5 días.",
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
       x="Fecha", 
       y="Casos activos") +
  scale_y_continuous(limits = c(0,NA),
                     labels = label_comma(accuracy = 1)) +
  scale_x_date(date_breaks = "13 day", 
               limits = c(fecha_inicio,fecha_corte), expand = c(0, 5)) +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid= element_line(linetype = 3, color = "white"), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 12, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 0, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) +
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/AA5Activos_nacional", ".png"), dpi = 150, width = 18, height = 9)


# Grafica 14: Letalidad acumulada y por semana nacional----
ggplot(letalidad_nac, aes(x=fecha)) +
  geom_line(aes(y = letalidad_sem), size=2, colour="gray66") +
  geom_line(aes(y = letalidad_acumulada), size=2, colour="gray15") +
  geom_point(data = corte_final_letalidad_nac, aes(x = fecha, y = letalidad_sem), color = "gray66", size = 3) +
  geom_point(data = corte_final_letalidad_nac, aes(x = fecha, y = letalidad_acumulada), color = "gray15", size = 3) +
  geom_label(data = corte_final_letalidad_nac, aes(x = max(fecha), y = letalidad_sem, label =  str_c(round(letalidad_sem,2), " %")),
             size = 8, 
             family = "Source Sans Pro",
             fontface = "bold",
             color = "gray66",
             hjust = 1) +
  geom_label(data = corte_final_letalidad_nac, aes(x = max(fecha), y = letalidad_acumulada, label =  str_c(round(letalidad_acumulada,2), " %")),
             size = 8, 
             family = "Source Sans Pro",
             fontface = "bold",
             color = "gray15",
             hjust = 1) +
  labs(title="Tasa de letalidad absoluta semanal y acumulada* | Nacional", 
       subtitle="La línea de color gris obscuro representa la tasa de letalidad nacional acumulada y la línea gris clara la tasa de letalidad por semana.\nLos datos se indican a partir del 27 de febrero de 2020.",
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
       x="Fecha", 
       y="Tasa de letalidad") +
  scale_y_continuous(limits = c(0,NA),
                     labels = label_comma(accuracy = 1, suffix = " %")) +
  scale_x_date(date_breaks = "13 day", 
               limits = c(fecha_inicio,NA), expand = c(0,10)) +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid= element_line(linetype = 3, color = "white"), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 12, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 0, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) +
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/AA6Letalidad_nacional_sem", ".png"), dpi = 150, width = 18, height = 9) 


# Grafica 6_Pruebas nacional-----
ggplot(pruebas_nac, aes(x=fecha)) +
  geom_bar(stat="identity", aes(y=total_pruebas), size=1, fill="#006847") +
  geom_line(aes(y=media_movil_total_pruebas), size=2, colour="red") +
  geom_point(data = corte_final_pruebas_nac, aes(x = fecha, y = media_movil_total_pruebas), color = "red", size = 3) +
  geom_label(data = corte_final_pruebas_nac, aes(x = fecha, y = media_movil_total_pruebas, label = comma(media_movil_total_pruebas)),
             size = 8, 
             family = "Source Sans Pro",
             fontface = "bold",
             color = "red",
             hjust = 1) +
  labs(title="Número total de pruebas realizadas por día | Nacional", 
       subtitle="Las barras de color verde obscuro representan el número total de pruebas por día con resultado positivo y negativo y que incluyen\ntanto pruebas por PCR como antigénicas. La línea roja representa el ajuste de media móvil de 7 días que suaviza\nlos datos para ese periodo. Debido al retraso de datos, se omiten los últimos 5 días.",
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
       x="Fecha", 
       y="Pruebas por día") +
  scale_y_continuous(limits = c(0,NA),
                     labels = label_comma(accuracy = 1)) +
  scale_x_date(date_breaks = "13 day", 
               limits = c(fecha_inicio,fecha_corte), expand = c(0, 0)) +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid= element_line(linetype = 3, color = "white"), 
        panel.grid.major.y = element_line(linetype = 3, color = "grey80"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 12, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 0, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) +
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/AB7Pruebas_nacional", ".png"), dpi = 150, width = 18, height = 9)


# Grafica 12: Positividad nacional acumulada y por semana-----
ggplot(pruebas_nac, aes(x=fecha)) +
  geom_line(aes(y = positividad_acumulada), size=2, colour="gray66") +
  geom_point(data = corte_final_pruebas_nac_2, aes(x = fecha, y = positividad_acumulada), color = "gray66", size = 3) +
  geom_line(aes(y = positividad_sem), size=2, colour="firebrick") +
  geom_point(data = corte_final_pruebas_nac_2, aes(x = fecha, y = positividad_sem), color = "firebrick", size = 3) +
  geom_label(data = corte_final_pruebas_nac_2, aes(x = max(fecha), y = positividad_sem, label =  str_c(round(positividad_sem,2), " %")),
             size = 8, 
             family = "Source Sans Pro",
             fontface = "bold",
             color = "firebrick",
             hjust = 1) +
  geom_label(data = corte_final_pruebas_nac_2, aes(x = max(fecha), y = positividad_acumulada, label =  str_c(round(positividad_acumulada,2), " %")),
             size = 8, 
             family = "Source Sans Pro",
             fontface = "bold",
             color = "gray66",
             hjust = 1) +
  labs(title="Porcentaje de positividad semanal y acumulada* | Nacional", 
       subtitle="La línea roja representa el porcentaje de positividad por semana y la línea gris el porcentaje de positividad acumulada.",
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
       x="Fecha", 
       y="Positividad") +
  scale_y_continuous(limits = c(0,60), breaks = c(5,10,25, 35, 40,  50,  60),
                     labels = label_comma(accuracy = 1, suffix = "%")) +
  scale_x_date(date_breaks = "13 day", 
               limits = c(fecha_inicio, NA)) +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid= element_line(linetype = 3, color = "white"), 
        panel.grid.major.y = element_line(linetype = 3, color = "grey60"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 12, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 0, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) +
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/AB8Positividad_nacional_sem", ".png"), dpi = 150, width = 18, height = 9)


# Graficas de tendencia estatal----
# Grafica_16: Positivos acumulados estados----
ggplot(positivos_defunciones_est, aes(x=fecha, group=entidad_res)) +
  geom_line(aes(y = positivos_acumulados), size=1, colour="#2A5783") +
  geom_point(data = corte_final_positivos_defunciones_est_2, aes(x = max(fecha), y = positivos_acumulados), color ="#2A5783", size = 3) +
  geom_label(data = corte_final_positivos_defunciones_est_2, aes(x = max(fecha), y = positivos_acumulados, label =  comma(positivos_acumulados, accuracy = 1)),
             size = 4, 
             family = "Source Sans Pro",
             fontface = "bold",
             color = "#2A5783",
             hjust = 1,
             vjust = 1) +
  facet_wrap(~entidad_res, scales="free_y", ncol=8) +
  labs(title="Casos positivos a SARS-CoV-2 acumulados* | Estatal", 
       subtitle="La línea azul representa los casos positivos acumulados por estado.", 
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
       x="Fecha", 
       y="Positivos acumulados") +
  scale_y_continuous(labels = label_comma(accuracy = 1)) +
  scale_x_date(date_breaks = "45 day", 
               limits = c(fecha_inicio, NA)) +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_line(linetype = 3, color = "white"), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 12, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 1, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) +
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/AC9Positivos_acumulados_esatal", ".png"), dpi = 150, width = 18, height = 9)

# Grafica_17: Defunciones acumuladas estados----
ggplot(positivos_defunciones_est, aes(x=fecha, group=entidad_res)) +
  geom_line(aes(y = defunciones_acumuladas), size=1, colour="gray35") +
  geom_point(data = corte_final_positivos_defunciones_est_2, aes(x = max(fecha), y = defunciones_acumuladas), color ="gray35", size = 3) +
  geom_label(data = corte_final_positivos_defunciones_est_2, aes(x = max(fecha), y = defunciones_acumuladas, label =  comma(defunciones_acumuladas, accuracy = 1)),
             size = 4, 
             family = "Source Sans Pro",
             fontface = "bold",
             color = "gray35",
             hjust = 1,
             vjust = 1) +
  facet_wrap(~entidad_res, scales="free_y", ncol=8) +
  labs(title="Defunciones asociadas a SARS-CoV-2 acumuladas* | Estatal", 
       subtitle="La línea gris representa las defunciones acumuladas por estado.", 
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
       x="Fecha", 
       y="Defunciones acumuladas") +
  scale_y_continuous(labels = label_comma(accuracy = 1)) +
  scale_x_date(date_breaks = "45 day", 
               limits = c(fecha_inicio, NA)) +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_line(linetype = 3, color = "white"), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 12, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 1, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) +
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/AC10Defunciones_acumuladas_esatal", ".png"), dpi = 150, width = 18, height = 9)


# Grafica 15: Letalidad acumulada y por semana estados----
ggplot(letalidad_est, aes(x=fecha, group = entidad_res)) +
  geom_line(aes(y = letalidad_sem), size=1, colour="gray65") +
  geom_line(aes(y = letalidad_acumulada), size=1, colour="gray15") +
  geom_point(data = corte_final_letalidad_est, aes(x = max(fecha), y = letalidad_sem), color = "gray65", size = 3) +
  geom_point(data = corte_final_letalidad_est, aes(x = max(fecha), y = letalidad_acumulada), color = "gray15", size = 3) +
  geom_label(data = corte_final_letalidad_est, aes(x = max(fecha), y = letalidad_sem, label =  str_c(round(letalidad_sem,1), " %")),
             size = 4, 
             family = "Source Sans Pro",
             fontface = "bold",
             color = "gray65",
             hjust = 1) +
  facet_wrap(~entidad_res, scales="free_y", ncol=8) +
  labs(title="Tasa de letalidad absoluta semanal y acumulada* | Estatal", 
       subtitle="La línea de color gris obscuro representa la tasa de letalidad nacional acumulada y la línea gris clara la tasa de letalidad por semana.\nSe muestran los datos de los últimos 185 días.",
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
       x="Fecha", 
       y="Letalidad") +
  scale_y_continuous(labels = label_comma(accuracy = 1, suffix = "%")) +
  scale_x_date(date_breaks = "45 day", 
               limits = c(fecha_inicio_defunciones, NA), expand = c(0,10)) +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_line(linetype = 3, color = "white"), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 12, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 1, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) +
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/AD11Letalidad_estatal_sem", ".png"), dpi = 150, width = 18, height = 9) 



# Gráfica_11: Casos activos estados----
ggplot(positivos_defunciones_est, aes(x=fecha, group=entidad_res)) +
  geom_line( aes(y=casos_activos), size=1, colour="royalblue4") +
  geom_point(data = corte_final_positivos_defunciones_est, aes(x = fecha, y = casos_activos), color = "royalblue4", size = 2) +
  geom_label(data = corte_final_positivos_defunciones_est, aes(x = fecha, y = casos_activos, label = comma(casos_activos, accuracy = 1)),
             size = 4, 
             family = "Source Sans Pro",
             fontface = "bold",
             color = "royalblue4",
             hjust = 1) +
  facet_wrap(~entidad_res, scales="free_y", ncol=8) +
  labs(title="Casos activos estimados* | Estatal", 
       subtitle="La línea de color azul indica los casos activos estimados. Se consideran casos activos a aquellos casos que cuyo resultado positivo\na SARS-CoV-2 ocurrió durante los últimos 14 días. Debido al retraso de datos, se omiten los últimos 5 días.",
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
       x="Fecha", 
       y="Casos activos") +
  scale_y_continuous(limits = c(0,NA),
                     labels = label_comma(accuracy = 1)) +
  scale_x_date(date_breaks = "42 day", 
               limits = c(fecha_inicio, fecha_corte), expand = c(0,10)) +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_line(linetype = 3, color = "white"), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 12, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 1, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) +
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/AD12Casos_activos_estadoH", ".png"), dpi = 150, width = 18, height = 9)

# Gráfica_08: Casos positivos por dia estados----
ggplot(letalidad_est, aes(x=fecha, group=entidad_res)) +
  geom_bar(stat="identity", aes(y=casos_positivos), fill="#2A5783") + 
  geom_line( aes(y=media_movil_casos_positivos), size=1, colour="red") +
  geom_point(data = corte_final_letalidad_est_2, aes(x = fecha, y = media_movil_casos_positivos), color = "red", size = 3) +
  geom_label(data = corte_final_letalidad_est_2, aes(x = fecha, y = media_movil_casos_positivos, label = comma(media_movil_casos_positivos, accuracy = 1)),
             size = 4, 
             family = "Source Sans Pro",
             fontface = "bold",
             color = "red",
             hjust = 1) +
  facet_wrap(~entidad_res, scales="free_y", ncol=8) +
  labs(title="Casos positivos a SARS-CoV-2* | Estatal", 
       subtitle="Las barras azules representan la incidencia mostrada en los últimos 45 días a partir de la actualización de datos.\nLa línea roja representa el ajuste de media móvil que suaviza los datos en ese periodo. Debido al retraso de datos, se omiten los últimos 5 días.", 
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
       x="Fecha", 
       y="Casos positivos") +
  scale_y_continuous(limits = c(0,NA),
                     labels = label_comma(accuracy = 1)) +
  scale_x_date(date_breaks = "5 day", 
               limits = c(fecha_corte - 45, fecha_corte), expand = c(0, 0)) +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_line(linetype = 3, color = "white"), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 12, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 1, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) +
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/AE13Incidencia_positivos_estadoH", ".png"), dpi = 150, width = 18, height = 9)


# Grafica_09: Defunciones por dia estados ----
ggplot(letalidad_est, aes(x=fecha, group=entidad_res)) +
  geom_bar(stat="identity", aes(y=defunciones), fill="grey35") + 
  geom_line( aes(y=media_movil_defunciones), size=1, colour="red") +
  geom_point(data = corte_final_letalidad_est_2, aes(x = fecha, y = media_movil_defunciones), color = "red", size = 3) +
  geom_label(data = corte_final_letalidad_est_2, aes(x = fecha, y = media_movil_defunciones, label = comma(media_movil_defunciones, accuracy = 1)),
             size = 4, 
             family = "Source Sans Pro",
             fontface = "bold",
             color = "red",
             hjust = 1) +
  facet_wrap(~entidad_res, scales="free_y", ncol=8) +
  labs(title="Defunciones asociadas a SARS-CoV-2* | Estatal", 
       subtitle="Las barras grises representan la incidencia mostrada en los últimos 45 días a partir de la actualización de datos.\nLa línea roja representa el ajuste de media móvil que suaviza los datos en ese periodo. Debido al retraso de datos, se omiten los últimos 5 días.", 
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)),
       x="Fecha", 
       y="Defunciones") +
  scale_y_continuous(limits = c(0,NA),
                     labels = label_comma(accuracy = 1)) +
  scale_x_date(date_breaks = "5 day", 
               limits = c(fecha_corte - 45, fecha_corte), expand = c(0, 0)) +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_line(linetype = 3, color = "white"), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 12, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 1, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) +
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/AE14Incidencia_defunciones_estadoH", ".png"), dpi = 150, width = 18, height = 9)


# Gráfica_10: Pruebas por estado----
ggplot(pruebas_est, aes(x=fecha, group=entidad_res)) +
  geom_bar(stat="identity", aes(y=total_pruebas), fill="#006847") + 
  geom_line( aes(y=media_movil_total_pruebas), size=1, colour="red") +
  geom_point(data = corte_final_pruebas_est_2, aes(x = fecha, y = media_movil_total_pruebas), color = "red", size = 3) +
  geom_label(data = corte_final_pruebas_est_2, aes(x = fecha, y = media_movil_total_pruebas, label = comma(media_movil_total_pruebas, accuracy = 1)),
             size = 4, 
             family = "Source Sans Pro",
             fontface = "bold",
             color = "red",
             hjust = 1) +
  facet_wrap(~entidad_res, scales="free_y", ncol=8) +
  labs(title="Número total de pruebas realizadas por día* | Estatal", 
       subtitle="Las barras de color verde obscuro representan el número total de pruebas por día con resultado positivo y negativo y que incluyen\ntanto pruebas por PCR como antigénicas. La línea roja representa el ajuste de media móvil de 7 días que suaviza\nlos datos para ese periodo. Debido al retraso de datos, se omiten los últimos 5 días.",
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
       x="Fecha", 
       y="Pruebas por día") +
  scale_y_continuous(limits = c(0,NA),
                     labels = label_comma(accuracy = 1)) +
  scale_x_date(date_breaks = "25 day", 
               limits = c(fecha_inicio+225, fecha_corte), expand = c(0, 0)) +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_line(linetype = 3, color = "white"), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 12, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 1, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) +
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/AF15Pruebas_estadoH", ".png"), dpi = 150, width = 18, height = 9)



# Grafica_13: Positividad acumulada y por semana estados----
ggplot(pruebas_est, aes(x=fecha, group=entidad_res)) +
  geom_line(aes(y = positividad_acumulada), size=1, colour="gray66") +
  geom_point(data = corte_final_pruebas_est, aes(x = max(fecha), y = positividad_acumulada), color = "gray66", size = 3) +
  geom_point(data = corte_final_pruebas_est, aes(x = max(fecha), y = positividad_sem), color = "firebrick", size = 3) +
  geom_line(aes(y = positividad_sem ), size=1, colour="firebrick") +
  geom_label(data = corte_final_pruebas_est, aes(x = max(fecha), y = positividad_sem, label =  str_c(round(positividad_sem,2), " %")),
             size = 4, 
             family = "Source Sans Pro",
             fontface = "bold",
             color = "firebrick",
             hjust = 1) +
  facet_wrap(~entidad_res, scales="free_y", ncol=8) +
  labs(title="Porcentaje de positividad semanal y acumulada* | Estatal", 
       subtitle="La línea roja representa el porcentaje de positividad por semana y la línea gris el porcentaje de positividad acumulada.\nLa positividad óptima para el control epidémico es del 5% de acuerdo con la OMS.", 
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
       x="Fecha", 
       y="Positividad") +
  scale_y_continuous(labels = label_comma(accuracy = 1, suffix = "%")) +
  scale_x_date(date_breaks = "45 day", 
               limits = c(fecha_inicio, NA)) +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_line(linetype = 3, color = "white"), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 12, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 1, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) +
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/AF16Positividad_estatal_sem", ".png"), dpi = 150, width = 18, height = 9)


ggplot(letalidad_est, aes(x=fecha, y=casos_positivos, group=entidad_res)) +
  geom_bar(stat="identity", aes(y=casos_positivos), fill="#2A5783") + 
  geom_smooth(color =  "red", span = 0.5) +
  facet_wrap(~entidad_res, scales="free_y", ncol = 8) +
  labs(title="Tendencia de casos positivos a SARS-CoV-2* | Estatal", 
       subtitle="Las barras azules representan la incidencia mostrada en los últimos 50 días a partir de la actualización de datos.\nLa línea roja representa la tendencia mostrada de los datos para ese periodo. Debido al retraso de datos, se omiten los últimos 5 días.", 
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
       x="Fecha", 
       y="Casos positivos") +
  scale_y_continuous(limits = c(0,NA),
                     labels = label_comma(accuracy = 1)) +
  scale_x_date(date_breaks = "15 day", 
               limits = c(fecha_corte - 50, fecha_corte), expand = c(0, 0)) +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_line(linetype = 3, color = "white"), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 12, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 1, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) +
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/AG17Casos_positivos_tendencia_est", ".png"), dpi = 150, width = 18, height = 9)


ggplot(letalidad_est, aes(x=fecha, y=defunciones, group=entidad_res)) +
  geom_bar(stat="identity", fill="grey35") + 
  geom_smooth(color =  "red", span = 0.5) +
  facet_wrap(~entidad_res, scales="free_y", ncol=8) +
  labs(title="Tendencia de defunciones asociadas a SARS-CoV-2* | Estatal", 
       subtitle="Las barras grises representan la incidencia mostrada en los últimos 50 días a partir de la actualización de datos.\nLa línea roja representa la tendencia mostrada de los datos para ese periodo. Debido al retraso de datos, se omiten los últimos 5 días.", 
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)),
       x="Fecha", 
       y="Defunciones") +
  scale_y_continuous(limits = c(0,NA),
                     labels = label_comma(accuracy = 1)) +
  scale_x_date(date_breaks = "15 day", 
               limits = c(fecha_corte - 50, fecha_corte), expand = c(0, 0)) +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_line(linetype = 3, color = "white"), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 12, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 1, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) +
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/AG18Defunciones_tendencia_est", ".png"), dpi = 150, width = 18, height = 9)

# Grafica 5_Positivos nacional-----
ggplot(clasificacion_casos_nac, aes(x=fecha, y=casos_positivos)) +
  geom_bar(stat="identity", fill="#2A5783") +
  geom_smooth(color="red", method = "gam", formula = y ~ s(x)) +
  labs(title="Tendencia de casos positivos a SARS-CoV-2 | Nacional", 
       subtitle="Las barras de color azul indican la incidencia diaria de casos positivos a SARS-CoV-2.\nLa línea roja representa la tendencia mostrada de los datos para ese periodo. Debido al retraso de datos, se omiten los últimos 5 días.",
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
       x="Fecha", 
       y="Casos positivos por día") +
  scale_y_continuous(limits = c(0,NA),
                     labels = label_comma(accuracy = 1)) +
  scale_x_date(date_breaks = "13 day", 
               limits = c(fecha_inicio,fecha_corte), expand = c(0, 0)) +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid= element_line(linetype = 3, color = "white"), 
        panel.grid.major.y = element_line(linetype = 3, color = "grey80"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 12, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 0, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) +
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/AG19GTendencia_positivos_nacional", ".png"), dpi = 150, width = 18, height = 9)


# Grafica 4_Defunciones nacional -----
ggplot(clasificacion_defunciones_nac, aes(x=fecha, y=defunciones)) +
  geom_bar(stat="identity", fill="grey35") +
  geom_smooth(color="red", method = "gam", formula = y ~ s(x)) +
  labs(title="Tendencia de defunciones asociadas a SARS-CoV-2 | Nacional", 
       subtitle="Las barras de color gris indican la incidencia diaria de defunciones asociadas a SARS-CoV-2.\nLa línea roja representa la tendencia mostrada de los datos para ese periodo. Debido al retraso de datos, se omiten los últimos 5 días.",
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)),
       x="Fecha", 
       y="Defunciones por día") +
  scale_y_continuous(limits = c(0,NA),
                     labels = label_comma(accuracy = 1)) +
  scale_x_date(date_breaks = "13 day", 
               limits = c(fecha_inicio_defunciones, fecha_corte), expand = c(0,0)) +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid= element_line(linetype = 3, color = "white"), 
        panel.grid.major.y = element_line(linetype = 3, color = "grey80"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 12, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 0, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) +
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/AG20Tendencia_defunciones_nacional", ".png"), dpi = 150, width = 18, height = 9)



# Grafica 5_Casos activos nacional-----
ggplot(positivos_defunciones_nac, aes(x=fecha, y = casos_activos)) +
  geom_point(size=2, color="royalblue4") +
  geom_smooth(color="red", method = "gam", formula = y ~ s(x)) +
  labs(title="Tendencia de casos activos estimados | Nacional", 
       subtitle="Los puntos de color azul indica los casos activos estimados y la línea roja representa la tendencia mostrada en ese periodo. Se consideran casos activos\na aquellos casos que cuyo resultado positivo a SARS-CoV-2 ocurrió durante los últimos 14 días. Debido al retraso de datos, se omiten los últimos 5 días.",
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
       x="Fecha", 
       y="Casos activos") +
  scale_y_continuous(limits = c(0,NA),
                     labels = label_comma(accuracy = 1)) +
  scale_x_date(date_breaks = "13 day", 
               limits = c(fecha_inicio,fecha_corte), expand = c(0, 5)) +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid= element_line(linetype = 3, color = "white"), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 12, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 0, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) +
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/AG21Tendencia_activos_nacional", ".png"), dpi = 150, width = 18, height = 9)


# Gráfica_11: Casos activos estados----
ggplot(positivos_defunciones_est, aes(x=fecha, y=casos_activos, group=entidad_res)) +
  geom_point(size=1, colour="royalblue4") +
  geom_smooth(color="red", method = "gam", formula = y ~ s(x)) +
  facet_wrap(~entidad_res, scales="free_y", ncol=8) +
  labs(title="Tendencia de casos activos estimados* | Estatal", 
       subtitle="Los puntos de color azul indica los casos activos estimados y la línea roja representa la tendencia mostrada en ese periodo. Se consideran casos activos\na aquellos casos que cuyo resultado positivo a SARS-CoV-2 ocurrió durante los últimos 14 días. Debido al retraso de datos, se omiten los últimos 5 días.",
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
       x="Fecha", 
       y="Casos activos") +
  scale_y_continuous(limits = c(0,NA),
                     labels = label_comma(accuracy = 1)) +
  scale_x_date(date_breaks = "15 day", 
               limits = c(fecha_corte - 150, fecha_corte), expand = c(0,10)) +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_line(linetype = 3, color = "white"), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 12, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 1, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) +
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/AG22Tendencia_casos_activos_estadoH", ".png"), dpi = 150, width = 18, height = 9)


ggplot(casos_positivos_edad_nac, aes(y = media_movil_casos_positivos, x = fecha, group = rango_edad, colour = rango_edad)) +
  geom_line(size = 1) +
  geom_point(size = 1) +
  labs(title="Casos positivos a SARS-CoV-2 por rango de edad | Nacional", 
       subtitle="Se agrupan los casos positivos conforme a la clasificación final en la base de datos abiertos fue 'Positivo a SARS-CoV-2'.\nDebido al retraso de datos, se omiten los últimos 5 días.",
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
       x="Fecha", 
       y="Casos positivos por día",
       color = "Grupos de edad") +
  scale_y_continuous(limits = c(0,NA),
                     labels = label_comma(accuracy = 1)) +
  scale_x_date(date_breaks = "13 day", 
               limits = c(fecha_inicio+30,fecha_corte), expand = c(0, 5)) +
  scale_colour_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid= element_line(linetype = 3, color = "white"), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        legend.background = element_rect(fill = "grey98", colour = "grey80"),
        #legend.justification = c(0.9,0.75),
        legend.position=c(0.9,0.75),
        axis.title = element_text(size = 15, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 14, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 0, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) +
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/Positivos_edad_nacional", ".png"), dpi = 150, width = 18, height = 9)


ggplot(defunciones_edad_nac, aes(y = media_movil_casos_positivos, x = fecha, group = rango_edad, colour = rango_edad)) +
  geom_line(size = 1) + 
  geom_point(size = 1) +
  labs(title="Defunciones asociadas a COVID-19 por rango de edad | Nacional", 
       subtitle="Se agrupan aquellas defunciones cuya clasificación final en la base de datos abiertos fue 'Positivo a SARS-CoV-2'.\nDebido al retraso de datos, se omiten los últimos 5 días.",
       caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de defunción. | ", str_c(texto_corte_cifras)), 
       x="Fecha", 
       y="Defunciones por día",
       color = "Grupos de edad") +
  scale_y_continuous(limits = c(0,NA),
                     labels = label_comma(accuracy = 1)) +
  scale_x_date(date_breaks = "13 day", 
               limits = c(fecha_inicio+30,fecha_corte), expand = c(0, 5)) +
  scale_colour_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
        plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid= element_line(linetype = 3, color = "white"), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
        legend.text = element_text(size = 12, family = "Source Sans Pro"),
        legend.title.align = 0.5,
        legend.background = element_rect(fill = "grey98", colour = "grey80"),
        #legend.justification = c(0.9,0.75),
        legend.position=c(0.9,0.75),
        axis.title = element_text(size = 15, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
        axis.text = element_text(size = 14, family = "Source Sans Pro"),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        axis.text.x = element_text(size= 12, angle = 90, hjust = 0, vjust = 0.5),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(face = "bold", color = "grey10", size = 11)) +
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/00_Resumen/Defuciones_edad_nacional", ".png"), dpi = 150, width = 18, height = 9)


# Loop casos positivos

for (i in unique(positivos_defunciones_est$entidad_res)) {
  ggplot(positivos_defunciones_est %>%filter(entidad_res == i), aes(x=fecha)) +
    geom_line(aes(y = positivos_acumulados), size=2, colour="#2A5783") +
    geom_point(data = corte_final_positivos_defunciones_est_2 %>% filter(entidad_res == i), aes(x = max(fecha), y = positivos_acumulados), color ="#2A5783", size = 3) +
    geom_label(data = corte_final_positivos_defunciones_est_2 %>% filter(entidad_res == i), aes(x = max(fecha), y = positivos_acumulados, label =  comma(positivos_acumulados, accuracy = 1)),
               size = 8,
               family = "Source Sans Pro",
               fontface = "bold",
               color = "#2A5783",
               hjust = 1,
               vjust = 1) +
    labs(title=str_c("Casos positivos a SARS-CoV-2 acumulados* | ", i),
         subtitle="La línea azul representa los casos positivos acumulados por estado.", 
         caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
         x="Fecha", 
         y="Positivos acumulados") +
    scale_y_continuous(labels = label_comma(accuracy = 1)) +
    scale_x_date(date_breaks = "13 day", 
                 limits = c(fecha_inicio, NA)) +
    theme_minimal() +
    theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
          plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
          plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
          plot.caption = element_text(hjust = 0, size = 15),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.grid = element_line(linetype = 3, color = "white"), 
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
          legend.text = element_text(size = 12, family = "Source Sans Pro"),
          legend.title.align = 0.5,
          axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
          axis.text = element_text(size = 12, family = "Source Sans Pro"),
          panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
          axis.text.x = element_text(size= 12, angle = 90, hjust = 1, vjust = 0.5),
          strip.background = element_rect(fill = "grey70", color  = "grey70"),
          strip.text = element_text(face = "bold", color = "grey10", size = 11)) 
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/01_Casos_acumulados/01_Casos_positivos_acumulados_", i, ".png"), dpi = 150, width = 18, height = 9)
}

# Loop defunciones

for (i in unique(positivos_defunciones_est$entidad_res)) {
  ggplot(positivos_defunciones_est %>% filter(entidad_res == i), aes(x=fecha)) +
    geom_line(aes(y = defunciones_acumuladas), size=2, colour="gray35") +
    geom_point(data = corte_final_positivos_defunciones_est_2 %>% filter(entidad_res == i), aes(x = max(fecha), y = defunciones_acumuladas), color ="gray35", size = 3) +
    geom_label(data = corte_final_positivos_defunciones_est_2 %>% filter(entidad_res == i), aes(x = max(fecha), y = defunciones_acumuladas, label =  comma(defunciones_acumuladas, accuracy = 1)),
               size = 8, 
               family = "Source Sans Pro",
               fontface = "bold",
               color = "gray35",
               hjust = 1,
               vjust = 1) +
    labs(title=str_c("Defunciones asociadas a SARS-CoV-2 acumuladas* | ", i),
         subtitle="La línea gris representa las defunciones acumuladas por estado.", 
         caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
         x="Fecha", 
         y="Defunciones acumuladas") +
    scale_y_continuous(labels = label_comma(accuracy = 1)) +
    scale_x_date(date_breaks = "13 day", 
                 limits = c(fecha_inicio, NA)) +
    theme_minimal() +
    theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
          plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
          plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
          plot.caption = element_text(hjust = 0, size = 15),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.grid = element_line(linetype = 3, color = "white"), 
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
          legend.text = element_text(size = 12, family = "Source Sans Pro"),
          legend.title.align = 0.5,
          axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
          axis.text = element_text(size = 12, family = "Source Sans Pro"),
          panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
          axis.text.x = element_text(size= 12, angle = 90, hjust = 1, vjust = 0.5),
          strip.background = element_rect(fill = "grey70", color  = "grey70"),
          strip.text = element_text(face = "bold", color = "grey10", size = 11)) 
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/02_Defunciones_acumuladas/02_Defunciones_acumuladas_", i, ".png"), dpi = 150, width = 18, height = 9)
}

# Loop letalidad

for (i in unique(letalidad_est$entidad_res)) {
  ggplot(letalidad_est  %>% filter(entidad_res == i), aes(x=fecha)) +
    geom_line(aes(y = letalidad_sem), size=2, colour="gray65") +
    geom_line(aes(y = letalidad_acumulada), size=2, colour="gray15") +
    geom_point(data = corte_final_letalidad_est %>% filter(entidad_res == i), aes(x = max(fecha), y = letalidad_sem), color = "gray65", size = 4) +
    geom_point(data = corte_final_letalidad_est %>% filter(entidad_res == i), aes(x = max(fecha), y = letalidad_acumulada), color = "gray15", size = 4) +
    geom_label(data = corte_final_letalidad_est %>% filter(entidad_res == i), aes(x = max(fecha), y = letalidad_sem, label =  str_c(round(letalidad_sem,1), " %")),
               size = 8, 
               family = "Source Sans Pro",
               fontface = "bold",
               color = "gray65",
               hjust = 1) +
    labs(title=str_c("Tasa de letalidad absoluta semanal y acumulada* | ", i),
         subtitle="La línea de color gris obscuro representa la tasa de letalidad nacional acumulada y la línea gris clara la tasa de letalidad por semana.\nSe muestran los datos de los últimos 185 días.",
         caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
         x="Fecha", 
         y="Letalidad") +
    scale_y_continuous(labels = label_comma(accuracy = 1, suffix = "%")) +
    scale_x_date(date_breaks = "13 day", 
                 limits = c(fecha_inicio_defunciones, NA), expand = c(0,10)) +
    theme_minimal() +
    theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
          plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
          plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
          plot.caption = element_text(hjust = 0, size = 15),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.grid = element_line(linetype = 3, color = "white"), 
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
          legend.text = element_text(size = 12, family = "Source Sans Pro"),
          legend.title.align = 0.5,
          axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
          axis.text = element_text(size = 12, family = "Source Sans Pro"),
          panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
          axis.text.x = element_text(size= 12, angle = 90, hjust = 1, vjust = 0.5),
          strip.background = element_rect(fill = "grey70", color  = "grey70"),
          strip.text = element_text(face = "bold", color = "grey10", size = 11)) 
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/03_Letalidad/03_Letalidad_sem_", i, ".png"), dpi = 150, width = 18, height = 9) 
}

# Loop casos activos

for (i in unique(positivos_defunciones_est$entidad_res)) {
  ggplot(positivos_defunciones_est %>% filter(entidad_res == i), aes(x=fecha)) +
    geom_line( aes(y=casos_activos), size=2, colour="royalblue4") +
    geom_point(data = corte_final_positivos_defunciones_est %>% filter(entidad_res == i), aes(x = fecha, y = casos_activos), color = "royalblue4", size = 3) +
    geom_label(data = corte_final_positivos_defunciones_est %>% filter(entidad_res == i), aes(x = fecha, y = casos_activos, label = comma(casos_activos, accuracy = 1)),
               size = 8, 
               family = "Source Sans Pro",
               fontface = "bold",
               color = "royalblue4",
               hjust = 1) +
    labs(title=str_c("Casos activos estimados* | ", i),
         subtitle="La línea de color azul indica los casos activos estimados. Se consideran casos activos a aquellos casos que cuyo resultado positivo\na SARS-CoV-2 ocurrió durante los últimos 14 días. Debido al retraso de datos, se omiten los últimos 5 días.",
         caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
         x="Fecha", 
         y="Casos activos") +
    scale_y_continuous(limits = c(0,NA),
                       labels = label_comma(accuracy = 1)) +
    scale_x_date(date_breaks = "13 day", 
                 limits = c(fecha_inicio, fecha_corte), expand = c(0,10)) +
    theme_minimal() +
    theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
          plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
          plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
          plot.caption = element_text(hjust = 0, size = 15),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.grid = element_line(linetype = 3, color = "white"), 
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
          legend.text = element_text(size = 12, family = "Source Sans Pro"),
          legend.title.align = 0.5,
          axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
          axis.text = element_text(size = 12, family = "Source Sans Pro"),
          panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
          axis.text.x = element_text(size= 12, angle = 90, hjust = 1, vjust = 0.5),
          strip.background = element_rect(fill = "grey70", color  = "grey70"),
          strip.text = element_text(face = "bold", color = "grey10", size = 11)) 
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/04_Casos_activos/04_Casos_activos_", i, ".png"), dpi = 150, width = 18, height = 9) 
}


# Loop casos positivos

for (i in unique(letalidad_est$entidad_res)) {
  ggplot(letalidad_est %>% filter(entidad_res == i), aes(x=fecha)) +
    geom_bar(stat="identity", aes(y=casos_positivos), fill="#2A5783") + 
    geom_line( aes(y=media_movil_casos_positivos), size=2, colour="red") +
    geom_point(data = corte_final_letalidad_est_2  %>% filter(entidad_res == i), aes(x = fecha, y = media_movil_casos_positivos), color = "red", size = 4) +
    geom_label(data = corte_final_letalidad_est_2  %>% filter(entidad_res == i), aes(x = fecha, y = media_movil_casos_positivos, label = comma(media_movil_casos_positivos, accuracy = 1)),
               size = 8, 
               family = "Source Sans Pro",
               fontface = "bold",
               color = "red",
               hjust = 1) +
    labs(title=str_c("Casos positivos a SARS-CoV-2* | ", i), 
         subtitle="Las barras azules representan la incidencia mostrada desde el inicio de la epidemia.\nLa línea roja representa el ajuste de media móvil que suaviza los datos en ese periodo. Debido al retraso de datos, se omiten los últimos 5 días.", 
         caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
         x="Fecha", 
         y="Casos positivos") +
    scale_y_continuous(limits = c(0,NA),
                       labels = label_comma(accuracy = 1)) +
    scale_x_date(date_breaks = "13 day", 
                 limits = c(fecha_inicio, fecha_corte), expand = c(0, 10)) +
    theme_minimal() +
    theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
          plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
          plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
          plot.caption = element_text(hjust = 0, size = 15),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.grid = element_line(linetype = 3, color = "white"), 
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
          legend.text = element_text(size = 12, family = "Source Sans Pro"),
          legend.title.align = 0.5,
          axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
          axis.text = element_text(size = 12, family = "Source Sans Pro"),
          panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
          axis.text.x = element_text(size= 12, angle = 90, hjust = 1, vjust = 0.5),
          strip.background = element_rect(fill = "grey70", color  = "grey70"),
          strip.text = element_text(face = "bold", color = "grey10", size = 11)) 
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/05_Casos_diarios/05_Incidencia_positivos_",  i, ".png"), dpi = 150, width = 18, height = 9) 
}


# Loop defunciones

for (i in unique(letalidad_est$entidad_res)) {
  ggplot(letalidad_est %>% filter(entidad_res == i), aes(x=fecha)) +
    geom_bar(stat="identity", aes(y=defunciones), fill="grey35") + 
    geom_line( aes(y=media_movil_defunciones), size=2, colour="red") +
    geom_point(data = corte_final_letalidad_est_2 %>% filter(entidad_res == i), aes(x = fecha, y = media_movil_defunciones), color = "red", size = 4) +
    geom_label(data = corte_final_letalidad_est_2 %>% filter(entidad_res == i), aes(x = fecha, y = media_movil_defunciones, label = comma(media_movil_defunciones, accuracy = 1)),
               size = 8, 
               family = "Source Sans Pro",
               fontface = "bold",
               color = "red",
               hjust = 1) +
    labs(title=str_c("Defunciones asociadas a SARS-CoV-2* | ", i),
         subtitle="Las barras grises representan la incidencia mostrada desde el inicio de las defunciones.\nLa línea roja representa el ajuste de media móvil que suaviza los datos en ese periodo. Debido al retraso de datos, se omiten los últimos 5 días.", 
         caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)),
         x="Fecha", 
         y="Defunciones") +
    scale_y_continuous(limits = c(0,NA),
                       labels = label_comma(accuracy = 1)) +
    scale_x_date(date_breaks = "13 day", 
                 limits = c(fecha_inicio, fecha_corte), expand = c(0, 10)) +
    theme_minimal() +
    theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
          plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
          plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
          plot.caption = element_text(hjust = 0, size = 15),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.grid = element_line(linetype = 3, color = "white"), 
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
          legend.text = element_text(size = 12, family = "Source Sans Pro"),
          legend.title.align = 0.5,
          axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
          axis.text = element_text(size = 12, family = "Source Sans Pro"),
          panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
          axis.text.x = element_text(size= 12, angle = 90, hjust = 1, vjust = 0.5),
          strip.background = element_rect(fill = "grey70", color  = "grey70"),
          strip.text = element_text(face = "bold", color = "grey10", size = 11)) 
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/06_Defunciones_diarias/06_Incidencia_defunciones_", i, ".png"), dpi = 150, width = 18, height = 9) 
}

# Loop pruebas

for (i in unique(pruebas_est$entidad_res)) {
  ggplot(pruebas_est %>% filter(entidad_res == i), aes(x=fecha)) +
    geom_bar(stat="identity", aes(y=total_pruebas), fill="#006847") + 
    geom_line( aes(y=media_movil_total_pruebas), size=2, colour="red") +
    geom_point(data = corte_final_pruebas_est_2 %>% filter(entidad_res == i), aes(x = fecha, y = media_movil_total_pruebas), color = "red", size = 4) +
    geom_label(data = corte_final_pruebas_est_2 %>% filter(entidad_res == i), aes(x = fecha, y = media_movil_total_pruebas, label = comma(media_movil_total_pruebas, accuracy = 1)),
               size = 8, 
               family = "Source Sans Pro",
               fontface = "bold",
               color = "red",
               hjust = 1) +
    labs(title=str_c("Número total de pruebas realizadas por día* | ", i), 
         subtitle="Las barras de color verde obscuro representan el número total de pruebas por día con resultado positivo y negativo y que incluyen\ntanto pruebas por PCR como antigénicas. La línea roja representa el ajuste de media móvil de 7 días que suaviza\nlos datos para ese periodo. Debido al retraso de datos, se omiten los últimos 5 días.",
         caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
         x="Fecha", 
         y="Pruebas por día") +
    scale_y_continuous(limits = c(0,NA),
                       labels = label_comma(accuracy = 1)) +
    scale_x_date(date_breaks = "13 day", 
                 limits = c(fecha_inicio, fecha_corte), expand = c(0, 10)) +
    theme_minimal() +
    theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
          plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
          plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
          plot.caption = element_text(hjust = 0, size = 15),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.grid = element_line(linetype = 3, color = "white"), 
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
          legend.text = element_text(size = 12, family = "Source Sans Pro"),
          legend.title.align = 0.5,
          axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
          axis.text = element_text(size = 12, family = "Source Sans Pro"),
          panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
          axis.text.x = element_text(size= 12, angle = 90, hjust = 1, vjust = 0.5),
          strip.background = element_rect(fill = "grey70", color  = "grey70"),
          strip.text = element_text(face = "bold", color = "grey10", size = 11)) 
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/07_Pruebas/07_Pruebas_", i, ".png"), dpi = 150, width = 18, height = 9) 
}


# Loop positividad

for (i in unique(pruebas_est$entidad_res)) {
  ggplot(pruebas_est %>% filter(entidad_res == i), aes(x=fecha)) +
    geom_line(aes(y = positividad_acumulada), size=2, colour="gray66") +
    geom_point(data = corte_final_pruebas_est %>% filter(entidad_res == i), aes(x = max(fecha), y = positividad_acumulada), color = "gray66", size = 4) +
    geom_point(data = corte_final_pruebas_est %>% filter(entidad_res == i), aes(x = max(fecha), y = positividad_sem), color = "firebrick", size = 4) +
    geom_line(aes(y = positividad_sem ), size=2, colour="firebrick") +
    geom_label(data = corte_final_pruebas_est %>% filter(entidad_res == i), aes(x = max(fecha), y = positividad_sem, label =  str_c(round(positividad_sem,2), " %")),
               size = 8, 
               family = "Source Sans Pro",
               fontface = "bold",
               color = "firebrick",
               hjust = 1) +
    labs(title=str_c("Porcentaje de positividad semanal y acumulada* | ", i), 
         subtitle="La línea roja representa el porcentaje de positividad por semana y la línea gris el porcentaje de positividad acumulada.\nLa positividad óptima para el control epidémico es del 5% de acuerdo con la OMS.", 
         caption=str_c("Elaborado por: CENAIC México Advanced Research®, con datos de la SSA/DGE, en colaboración con Comunicólogos MX®.","\n","* Datos ordenados por fecha de ingreso. | ", str_c(texto_corte_cifras)), 
         x="Fecha", 
         y="Positividad") +
    scale_y_continuous(labels = label_comma(accuracy = 1, suffix = "%")) +
    scale_x_date(date_breaks = "13 day", 
                 limits = c(fecha_inicio, NA), expand = c(0, 10)) +
    theme_minimal() +
    theme(text = element_text(family = "Source Sans Pro", color = "grey35"),
          plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,10,0), family = "Source Sans Pro", color = "grey25"),
          plot.subtitle = element_text(size = 16, colour = "#666666", margin = margin(0, 0, 20, 0), family = "Source Sans Pro"),
          plot.caption = element_text(hjust = 0, size = 15),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.grid = element_line(linetype = 3, color = "white"), 
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.title = element_text(size = 12, face = "bold", family = "Source Sans Pro"),
          legend.text = element_text(size = 12, family = "Source Sans Pro"),
          legend.title.align = 0.5,
          axis.title = element_text(size = 12, face = "bold", margin = margin(100,0,20,0), family = "Source Sans Pro"),
          axis.text = element_text(size = 12, family = "Source Sans Pro"),
          panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
          axis.text.x = element_text(size= 12, angle = 90, hjust = 1, vjust = 0.5),
          strip.background = element_rect(fill = "grey70", color  = "grey70"),
          strip.text = element_text(face = "bold", color = "grey10", size = 11)) 
  ggsave(str_c("COVID-19-Mexico-Time-Series-Plots/Graficas/08_Positividad/08_Positividad_", i, ".png"), dpi = 150, width = 18, height = 9) 
}
