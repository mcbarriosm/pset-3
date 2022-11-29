## Mayra Catalina Barrios Mesa
## Codigo: 201911563
## Version de R:
R.version.string
## [1] "R version 4.2.2 (2022-10-31)"

## ------- PROBLEM-SET 3 -------

## configuracion inicial 
rm(list = ls()) # limpia el entorno de R

## Llamar y/o instalar las librerias a usar:
 
require(pacman)
p_load(tidyverse, rio, 
       arrow, ## read parque files
       broom, # tidy-coefficients
       mfx, # marginal effects
       margins,  # marginal effects
       estimatr, # robust standard errors
       lmtest, # HAC (Newey-West) standard errors
       fixest, # hdfe regressions (feols)
       modelsummary, # Coefplot with modelplot
       stargazer # export tables to latex 
) 

## PUNTO 1:

## 1.1. Estimaciones:

## Importar los datos de las regresiones
datos <- read_rds("input/data_regresiones.rds", refhook = NULL)
head(datos)

## Modelo econometrico 1:

# lm function

lm(formula = price ~ rooms + bathrooms , data = datos) 
lm(formula = price ~ rooms + bathrooms - 1, data = datos) 

# Linear regression
ols_1 = lm(price ~ rooms + bathrooms , data = datos)
ols_1 %>% summary() 
summary(ols_1)$r.squared # R^2
summary(ols_1)$adj.r.squared # R^2 ajustado

# What is ols object?
View(ols_1)
ols_1$call # model
ols_1$coefficients # get coefficients
ols_1$na.action # rows's NA
ols_1$residuals # get residuals
summary(ols_1$residuals)
hist(ols_1$residuals)

# get predict values
ols_1 %>% predict()
datos$predict_ols = predict(object = ols_1 , newdata = datos )

## Modelo econometrico 2:

# lm function

lm(formula = price ~ property_type + surface_total , data = datos) 
lm(formula = price ~ property_type + surface_total - 1, data = datos) 

# Linear regression
ols_2 = lm(price ~ property_type + surface_total , data = datos)
ols_2 %>% summary() 
summary(ols_2)$r.squared # R^2
summary(ols_2)$adj.r.squared # R^2 ajustado

# What is ols object?
View(ols_2)
ols_2$call # model
ols_2$coefficients # get coefficients
ols_2$na.action # rows's NA
ols_2$residuals # get residuals
summary(ols_2$residuals)
hist(ols_2$residuals)

# get predict values
ols_2 %>% predict()
datos$predict_ols = predict(object = ols_2 , newdata = datos )

## Modelo econometrico 3:

# lm function

lm(formula = price ~ dist_cbd + dist_cole , data = datos) 
lm(formula = price ~ dist_cbd + dist_cole - 1, data = datos) 

# Linear regression
ols_3 = lm(price ~ dist_cbd + dist_cole , data = datos)
ols_3 %>% summary() 
summary(ols_3)$r.squared # R^2
summary(ols_3)$adj.r.squared # R^2 ajustado

# What is ols object?
View(ols_3)
ols_3$call # model
ols_3$coefficients # get coefficients
ols_3$na.action # rows's NA
ols_3$residuals # get residuals
summary(ols_3$residuals)
hist(ols_3$residuals)

# get predict values
ols_3 %>% predict()
datos$predict_ols = predict(object = ols_3 , newdata = datos )

## 1.2. Presentar resultados:

# joint models (modelsummary)
coeficientes <- list(ols_1, ols_2, ols_3)
msummary(coeficientes)

# coefplot
mods = list('Modelo 1' = ols_1 , 'Modelo 2' = ols_2 , 'Modelo 3' = ols_3)

modelplot(mods) + coord_flip() + 
  labs(title = "Modelos econometricos con el precio de la vivienda como variable dependiente" , 
       subtitle = "Comparacion de modelos")

## 1.3. Exportar resultados:

# export table
stargazer(ols_1, ols_2,ols_3, 
          type= 'text',
          dep.var.labels = c('','Price',''), 
          df = FALSE,
          digits = 3, 
          out = paste0('output/resultados_regresiones.xlsx'))


