## Efectos de subsidiar la calcomanía vehicular en Colima

## ¿A quién beneficiaría dejar de cobrar el holograma vehicular?
## Supongamos que sigue una lógica de subsidio
## Como es un impuesto, ahora todos debemos cubrir lo que ya no están aportando especificamente los hogares con un auto o más
## Por cuestiones de simplicidad, vamos a asumir que solamente se subsidiará un holograma vehicular por hogar, si los hogares tienen más de un vehículo
## deberan cubrir el holograma del excedente

## Cargamos librerias de trabajo
library(tidyverse) ## Hacer la tidymagia        
library(readr) ## Leer la base de datos
library(wesanderson) ## Los colores están geniales

## Cargamos bases de datos
viviendas_int_15 <- read_csv("https://raw.githubusercontent.com/Zatara13/sub_holo_vehi_col/main/TR_VIVIENDA06.CSV?token=ARB6FRQH67PRFU6TSJIW653AAIGDM")
## Seleccionamos las variables de Sexo de Jefe del Hogar, Ingreso de Trabajo, Tenencia de automovil en el hogar y el factor de expansión
viv_15 <- viviendas_int_15 %>%
      select(JEFE_SEXO, INGTRHOG, AUTOPROP, FACTOR)

## Creamos base de datos para trabajar el análisis
viv_15_s <- viv_15 %>%
      drop_na() %>% ## Se eliminan las observaciones donde no hay datos en ingreso, sexo de jefe de familia o en tenecia de auto
      filter(AUTOPROP < 9) %>% ## Este filtro se hace para solo seleccionar los hogares donde hay certeza que hay o no hay auto
      mutate(auto = if_else(AUTOPROP == 7, 1, 0)) %>% ## Esta transformación permite codificar tener auto como variable dicotómica
      rename(jefe_sexo = JEFE_SEXO, ingtrhog = INGTRHOG, factor = FACTOR) %>% ## Renombramos a minúsculas para trabajar con más comodidad
      select(jefe_sexo, ingtrhog, auto, factor)  %>% ## seleccionamos variables de interés
      filter(ingtrhog < 999999) %>% ## filtramos ingreso < 999999, porque ese es el código de no se sabe
      mutate(jefe_sexo = if_else(jefe_sexo == 1, "hombre", "mujer")) ## Transformamos de los códigos a los nombres de categoría

## Calculamos rangos de deciles con ingreso de Colima
viv_15_ing <- viv_15 %>%
      drop_na(INGTRHOG) %>% ## Eliminamos las observaciones donde no hay datos en ingreso
      filter(INGTRHOG < 999999) ## 999,999 es el código para no especificó, por lo que seleccionamos las respuestas menores a esta
quantile(viv_15_ing$INGTRHOG,  prob = seq(0, 1, length = 11), type = 5)
rm(viviendas_int_15, viv_15_ing)

## Calculamos ingreso medio por decil
fing_md <- function(ingmin, ingmax, nombre_decil){
      viv_15 %>%
            filter(between(INGTRHOG, ingmin, ingmax)) %>% ## Seleccionamos ingresos en el rango
            summarise(ingreso_medio = mean(INGTRHOG)) %>% ## Calculamos el ingreso medio
            mutate(decil = nombre_decil) ## Le agregamos el numerito al decil
}
## Ingreso medio por decil
# I [0, 3000]
dec_1 <- fing_md(0, 3000, 1)
# II [3001, 4000]
dec_2 <- fing_md(3001, 4000, 2)
# III [4001, 5143]
dec_3 <- fing_md(4001, 5143, 3)
# IV [5144, 6200]
dec_4 <- fing_md(5144, 6171, 4)
# V [6201, 7714]
dec_5 <- fing_md(6172, 7714, 5)
# VI [7715, 9000]
dec_6 <- fing_md(7715, 9000, 6)
# VII [9001, 10857]
dec_7 <- fing_md(9001, 10800, 7)
# VIII [10858, 14000]
dec_8 <- fing_md(10801, 14000, 8)
# IX [14001, 19714]
dec_9 <- fing_md(14001, 19714, 9)
# X [19715, 999998] # El 999998 tiene que ver con la codificación del ingreso en la encuesta intercensal, que implica que ganan mucha lana
dec_10 <- fing_md(19715, 999998, 10)

ingmed <-  bind_rows(dec_1, dec_2,dec_3,dec_4,dec_5,dec_6,dec_7,dec_8,dec_9,dec_10) ## Aquí podemos ver el ingreso medio por decil
rm(dec_1, dec_2,dec_3,dec_4,dec_5,dec_6,dec_7,dec_8,dec_9,dec_10, fing_md)

## Con esta función calculamos el costo promedio de subsidiar por decil
## Usamos la fórmula Costo del holograma vehicular * probabilidad de tener carro en el decil * suma de factores de expansión del decil
fhog_ing <- function(ingmin, ingmax, nombre_decil){
      viv_15_s %>%
            group_by(jefe_sexo) %>% ## agrupamos por variable de sexo para ver efecto diferenciado en hogares con jefe de familia o con jefa de familia
            filter(between(ingtrhog, ingmin, ingmax)) %>% ## Filtramos al rango del decil
            mutate(p = auto*factor/sum(factor)) %>%
            summarise(p_auto = sum(p), hogares = sum(factor)) %>% ##Calculamos la probabilidad por decil de que un hogar tenga auto
            mutate(subsidio_mdp = p_auto * hogares * 1043 / 1000000) %>% ## Dividimos entre 1,000,000 para tener el subsidio por millones de pesos
            mutate(decil = nombre_decil) %>% ## Agregamos codificador de decil
            mutate(hog_auto = p_auto * hogares) ## Nos permite saber el número de hogares con auto por decil
}

## Deciles
# I [0, 3000]
dec_1 <- fhog_ing(0, 3000, 1)
# II [3001, 4000]
dec_2 <- fhog_ing(3001, 4000, 2)
# III [4001, 5143]
dec_3 <- fhog_ing(4001, 5143, 3)
# IV [5144, 6200]
dec_4 <- fhog_ing(5144, 6200, 4)
# V [6201, 7714]
dec_5 <- fhog_ing(6201, 7714, 5)
# VI [7715, 9000]
dec_6 <- fhog_ing(7715, 9000, 6)
# VII [9001, 10857]
dec_7 <- fhog_ing(9001, 10857, 7)
# VIII [10858, 14000]
dec_8 <- fhog_ing(10858, 14000, 8)
# IX [14001, 19714]
dec_9 <- fhog_ing(14001, 19714, 9)
# X [19715, 999998] # El 999998 tiene que ver con la codificación del ingreso en la encuesta intercensal, que implica que ganan mucha lana
dec_10 <- fhog_ing(19715, 999998, 10)

## Unimos los datos de los deciles, y agregamos la distribución del subsidio por cada 100 pesos (aka porcentaje)
sub_decil_au <- bind_rows(dec_1, dec_2,dec_3,dec_4,dec_5,dec_6,dec_7,dec_8,dec_9,dec_10) %>% ## Unimos los objetos creados previamente
      select(jefe_sexo, decil, subsidio_mdp, hogares, hog_auto) %>% ## Seleccionamos variable de interés
      mutate(pct_subs = subsidio_mdp / sum(subsidio_mdp) * 100) ## Calculamos distribución porcentual del apoyo
rm(dec_1, dec_2,dec_3,dec_4,dec_5,dec_6,dec_7,dec_8,dec_9,dec_10)

sub_decil_au %>% summarise(sum(subsidio_mdp * 1000000)) ## Vemos la suma del subsidio
## Comprobamos que operación sirva. 
viv_15_s %>% summarise(sum(auto * factor * 1043)) == sub_decil_au %>% summarise(sum(subsidio_mdp * 1000000)) ## Si resulta TRUE, no hay errores de código

## Graficas

## Dinero en mdp que recibe cada decil del subsidio propuesto
ggplot(sub_decil_au, aes(x = decil, y = subsidio_mdp, fill = jefe_sexo)) +
      theme_bw()+
      geom_bar(stat="identity") +
      geom_vline(xintercept = 1.5, ## Seleccionamos 1.5 para representar la línea de pobreza urbana (CA + CNA) de dic-15
                 linetype = "dashed", color = "red")+
      scale_fill_manual(values=wes_palette(n=2, name="GrandBudapest2"))+
      scale_x_continuous(breaks = c(1:10), labels = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"))+
      labs(title = "Subsidio de un holograma vehicular por hogar",
           subtitle = "Hogares de Colima por decil de ingresos, 2015",
           x = "Decil de ingresos",
           y = "Subsidio en millones pesos",
           caption = "Fuente: Encuesta Intercensal 2015, INEGI
           Nota: A la derecha de la línea roja están los deciles con ingresos medios superiores a la línea de pobreza  urbana (CA + CNA) de diciembre de 2015")

## Destino de cada 100 pesos del subsidio, por decil
ggplot(sub_decil_au, aes(x = decil, y = pct_subs, fill = jefe_sexo)) +
      theme_bw()+
      geom_bar(stat="identity") +
      geom_vline(xintercept = 1.5, ## Seleccionamos 1.5 para representar la línea de pobreza urbana (CA + CNA) de dic-15
                 linetype = "dashed", color = "red")+
      scale_fill_manual(values=wes_palette(n=2, name="GrandBudapest2"))+
      scale_x_continuous(breaks = c(1:10), labels = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"))+
      labs(title = "Destino de cada 100 pesos subsidiados en holograma vehicular por hogar",
           subtitle = "Hogares de Colima por decil de ingresos, 2015",
           x = "Decil de ingresos",
           y = "Pesos",
           caption = "Fuente: Encuesta Intercensal 2015, INEGI
           Nota: A la derecha de la línea roja están los deciles con ingresos medios superiores a la línea de pobreza  urbana (CA + CNA) de diciembre de 2015")