#' Practica Modelo de Marketing Mix.

 

# Librerias que vamos a utilizar a lo largo de la practica 
library(janitor)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

## Ejercicio 1, leemos el archivo y creamos el data frame

read_csv("mktmix.csv")
mi_df <- read_csv("mktmix.csv")
mi_df <- clean_names(mi_df)

## Ejercicio 2, averiguamos el numero de filas,columnas y explicamos acerca del data frame

dim(mi_df) 

#' El data frame tiene 104 filas y 9 columnas

str(mi_df) 

#' Como podemos ver, la clase de la tabla base_price y discount es numeric, y nos estan indicando el precio base de un producto sin cargas añadidas y el descuento que se le aplica a cada uno.


## Ejercicio 3, cambiamos los valores de newspaper_inserts, ya que es de tipo character y queremos que sea de tipo numerico. 

mi_df$newspaper_inserts[is.na(mi_df$newspaper_inserts)] <- 0
mi_df$newspaper_inserts

mi_df$newspaper_inserts[19] = 1
mi_df$newspaper_inserts[39] = 1
mi_df$newspaper_inserts[45] = 1
mi_df$newspaper_inserts[54] = 1
mi_df$newspaper_inserts[66] = 1
mi_df$newspaper_inserts[98] = 1

mi_df$newspaper_inserts <- as.numeric(mi_df$newspaper_inserts)
mi_df$newspaper_inserts

class(mi_df$newspaper_inserts)


## Ejercicio 4, creamos nuevas columnas para cada categoria, pero antes, le voy a dar un valor auxiliar(el 0, por ejemplo) a los NA.

mi_df$website_campaign[is.na(mi_df$website_campaign)] <- 0
mi_df$website_campaign

# Ahora creamos nuevas columnas, dando el 1 y el 0 como valores numericos para la varible Twitter, Facebook y Website Campaign.

mi_df <- mi_df %>%
  mutate(Twitter = if_else(website_campaign == "Twitter",1,0))

mi_df$Twitter

mi_df <- mi_df %>%
  mutate(WebsiteCompaign = if_else(website_campaign == "Website Campaign",1,0))

mi_df$WebsiteCompaign

mi_df <- mi_df %>%
  mutate(Facebook = if_else(website_campaign == "Facebook",1,0))

mi_df$Facebook

## Ejercicio 5, calculamos cuantas semanas hubo de campaña para Facebook y Twitter. 

mi_df %>%
  group_by(Twitter) %>%
  summarise(conteo_semanas_Twitter = n())

mi_df %>%
  group_by(Facebook) %>%
  summarise(conteo_semanas_Facebook = n())

#' Como podemos ver, en los dos casos, ha habido 4 semanas de campaña tanto en el caso de Facebook como de Twitter. 
  

## Ejercicio 6, calculamos cuantas semanas se hizo una inversion de menos de 50 grp (entendemos grp por audiencia televisiva)


mi_df %>%
  group_by(tv) %>%
  filter( tv < 50) %>%
  summarise(semanas = n()) #' como podemos ver, solamente hubo 3 semanas donde se realizo una inversion menor a 50 grp. 

## Ejercicio 7, calculamos la media tanto para las semanas que hubo inversion en radio como para las que no. 

mi_df %>%
  select(tv, radio) %>%
   mutate(inversion_en_radio = if_else(radio == "NaN"| radio == 0,"Sin inversion en radio", "Inversion en radio")) %>%
     group_by(inversion_en_radio) %>%
       summarise(mean(tv))

#' Podemos ver que la media para las semanas que hubo inversion en radio fue de 137 y para las semanas
#' sin inversion fue de 171, es evidente que la media fue mayor en las semanas que no hubo inversion en radio. 
     

## Ejercicio 8, creamos un grafico de lineas para las ventas del producto, ademas de crear el eje x

fechas_eje_x <- c(1:104)

mi_df %>%
  ggplot() + 
  geom_line(aes(x = fechas_eje_x, y= new_vol_sales), color = "darkgreen")

## Ejercicio 9, ahora toca crear un histograma y un boxplot para las ventas tambien, y.. ¿ la media es mayor o menor ?

hist(x = mi_df$new_vol_sales , main = "Histograma de Volumen de ventas", 
     xlab = "Volumen de Ventas", ylab = "Frecuencia")

boxplot(x = mi_df$new_vol_sales, col = rgb(0, 0.5, 1, alpha = 0.5))

#' Como vemos en el boxplot creado, el 50% de las ventas estara entre 19000 y 21000 ya que es la diferencia
#' de distancia entre el primer y el segundo cuartil, y en cuanto a la mediana, vemos que se situa en el centro
#' de la caja, lo que nos esta indicando que la distribucion es simetrica y que moda, media y mediana coinciden. 


## Ejercicio 10, creamos un nuevo data.frame con solo las columnas de tv, radio y stout, y despues usamos facet_wrap para unir los 3 graficos de lineas. 


mi_df2 <- data.frame (mi_df$tv, mi_df$radio, mi_df$stout)

mi_df2 <- mi_df2 %>%
  pivot_longer(everything())

mi_df2 %>%
ggplot() +
  geom_line(aes(x = 1:312, y= value)) + 
  facet_wrap(~ name, ncol = 1, strip.position = "right",  scales = "free")

#' Lo mas significativo a comentar sin duda alguna son las fluctuaciones que sufre la inversion en radio 
#' ademas de la gran variabilidad que tienen los otros dos datos publicitarios como son la television o los 
#' stouts.  


## Ejercicio 11, creamos un grafico de dispersion con new_vol_sales y con in_store

  ggplot(mi_df, aes(x = in_store, y= new_vol_sales)) + 
  geom_point() + 
    geom_smooth(method = "lm")
  
#' La relacion entre estas dos variables es muy clara, el volumen de ventas dependera claramente del stock 
#' disponible que haya en cada tienda y de la gestion de los propios stocks, para una empresa es funtamental
#' controlar los niveles de stocks para poder adquirir o producir sus propios productos a un ritmo totalmente
#' distinto a las ventas, si quiere ser eficiente en el desarrollo de su actividad economica claro. 

## Ejercicio 12, repetimos el grafico anterior pero sin el geom_smooth y con dos variantes. 
  
  ggplot(mi_df, 
         aes(x = in_store, y = new_vol_sales, color = as.factor(newspaper_inserts))) +
    geom_point()
  
  ggplot(mi_df, 
         aes(x = in_store, y = new_vol_sales, color = tv)) +
    geom_point()
  
  
## Ejercicio 13, creamos nueva columna indicando si se ha aplicado descuento o no 
  
   mi_df %>%
    select(base_price, discount) %>% 
    mutate(discount_yesno = if_else( discount == 0 , FALSE, TRUE)) %>%
    group_by(discount_yesno) %>%                            
    summarise(media_ambos = mean(base_price)) %>%
     ggplot() + 
     geom_col(aes(x= discount_yesno, y = media_ambos))
   
    
#' Una cosa importante, la columna discount_yesno la he creado con 0 y 1 para el grafico, aunque en un principio
#' iba a hacer discount == 0, "No hay descuento", "Si hay descuento", quedaria mucho mejor, pero es de tipo character,
#' y aparte de eso, como podemos ver en el grafico y de manera numerica, las medias son practicamente iguales, 
#' varian solamente en 0.2. 

  
## Ejercicio 14, creamos una funcion que ajuste un modelo de regresion en los datos
  
vector_entrada <- c("tv", "radio")

mi_funcion <- function(vector_entrada){
    df_auxiliar <- mi_df [, c(vector_entrada, "new_vol_sales")]
    mi_modelo <- lm(new_vol_sales ~ ., data = df_auxiliar)
    V <- summary(mi_modelo)$adj.r.squared
    
    return(V)
}

mi_funcion(vector_entrada) # El valor de R cuadrado ajustado. 

## Ejercicio 15, por ultimo, creamos una lista con los 3 vectores y analizamos que modelo es el mejor. 

vector1 <- c("base_price", "radio", "tv", "stout")
vector2 <- c("base_price", "in_store", "discount", "radio", "tv", "stout")
vector3 <- c("in_store", "discount")

mi_lista <- list(vector1, vector2, vector3)

mi_lista

resultado_final <- map_dbl(mi_lista, mi_funcion)

resultado_final

#' Como podemos ver, el mejor modelo es el que sus valores mas se acercan a 1, es decir, en este caso es el segundo, 
#' con un R cuadrado ajustado de 0,7836765, esto nos indica que el segundo modelo es un modelo fiable para posibles
#' previsiones futuras y que a pesar de no tener un ajuste perfecto, es un modelo solido junto con el primero. 
