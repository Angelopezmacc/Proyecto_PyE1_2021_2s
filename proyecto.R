#**********************************************
#Universidad del Rosario                      *
# Proyecto final                              *
# Probailidad y EstadÃ­stica 1                 *
# 2021 - 2s                                   *
#                                             *
# Juan Obando                                 *
# Ãngel LÃ³pez                                 *
#**********************************************

# Para mÃ¡s informaciÃ³n, por favor remitirse a nuestro repositorio de GitHub. AhÃ­ encontrarÃ¡ ademÃ¡s de este cÃ³digo, 
# el archivo de datos analizado en formato xlsx o ods (depende de su preferencia y aclaramos que el cÃ³digo a continuaciÃ³n solo contemple
#archivo con el formato .xlsx). TambiÃ©n encontrarÃ¡ un archivo Readme.md en el cual podrÃ¡ obtener una mejor descripciÃ³n del proyecto
# junto con algunas indicaciones generales. Gracias

#Link al repositorio: https://github.com/Angelopezmacc/Proyecto_PyE1_2021_2s

#--------------------------------------------------------------------------------------------
# Se importa el dataset
library(readxl)
datos <- read_excel("C:/Users/juand/Desktop/Proyecto estaidstica/dataset_engineering_graduate_salary.xlsx",
                                                  col_types = c("numeric", "text", "text",
                                                                "numeric", "text", "numeric", "numeric",
                                                                "numeric", "numeric", "numeric", "text", "text",
                                                                "numeric", "numeric", "numeric", "text", "numeric",
                                                                "numeric", "numeric", "numeric", "numeric",
                                                                "numeric", "numeric", "numeric", "numeric",
                                                                "numeric", "numeric", "numeric",
                                                                "numeric", "numeric", "numeric", "numeric", "numeric",
                                                                "numeric", "text", "text", "text",
                                                                "text"))

#--------------------------------------------------------------------------------------------
# Definimos algunas variables globales Ãºtiles a lo largo de todo el cÃ³digo

# Columnas
col_1 = datos[,1] # ID (IdentificaciÃ³n)
col_2 = datos[,2] # Gender (GÃ©nero)
col_3 = datos[,3] # DOB (fecha de nacimiento)
col_4 = datos[,4] # 10percentage (Promedio al graduarse en grado 10)
col_5 = datos[,5] # 10 board
col_6 = datos[,6] # 12 grad
col_7 = datos[,7] # 12%
col_8 = datos[,8] # 12 board
col_9 = datos[,9] # CollegeID
col_10 = datos[,10] # College tier
col_12 = datos[,12] # Titulo
col_13 = datos[,13] # Especializacion
col_14 = datos[,14] # College GPA
col_15 = datos[,15] #
col_16 = datos[,16] # 
col_17 = datos[,17] # 
col_18 = datos[,18] # 
col_19 = datos[,19] # 
col_20 = datos[,20] # 
col_21 = datos[,21] # 
col_22 = datos[,22] # 
col_23 = datos[,23] # 
col_24 = datos[,24] # 
col_25 = datos[,25] # 
col_26 = datos[,26] # 
col_27 = datos[,27] # 
col_28 = datos[,28] #
col_29 = datos[,29] # 
col_30 = datos[,30] # 
col_31 = datos[,31] # 
col_32 = datos[,32] # 
col_33 = datos[,33] #
col_34 = datos[,34] # Salario 

#hasta 34 cols
#--------------------------------------------------------------------------------------------
# Obtenemos un breve y bÃ¡sico de los datos que tenemos
summary(datos)

#--------------------------------------------------------------------------------------------
# Analizando la cantidad de hombres y mujeres en nuestro dataset
tabla_sexo = table(col_2); tabla_sexo
pie(tabla_sexo)

#--------------------------------------------------------------------------------------------
#Analizamos aqui los intervalos de GPA (de 6 a 99, en intervalos de 10, exceptuando el inicial, el cual ira de 6 a 19.9 por conveniencia) con el salario promedio que obtienen
promedio_gpa1 = mean(col_34[col_14 >= 6 & col_14 <= 19.9])
promedio_gpa2 = mean(col_34[col_14 > 19.9 & col_14 <= 29.9])
promedio_gpa3 = mean(col_34[col_14 > 29.9 & col_14 <= 39.9])
promedio_gpa4 = mean(col_34[col_14 > 39.9 & col_14 <= 49.9])
promedio_gpa5 = mean(col_34[col_14 > 49.9 & col_14 <= 59.9])
promedio_gpa6 = mean(col_34[col_14 > 59.9 & col_14 <= 69.9])
promedio_gpa7 = mean(col_34[col_14 > 69.9 & col_14 <= 79.9])
promedio_gpa8 = mean(col_34[col_14 > 79.9 & col_14 <= 89.9])
promedio_gpa9 = mean(col_34[col_14 > 89.9 & col_14 <= 99.9])

#--------------------------------------------------------------------------------------------
#Analizamos el salario promedio de cada Tier de universidades
promedio_tier1 = mean(col_34[col_10 == 1])
promedio_tier2 = mean(col_34[col_10 == 2])

##############################################################
#Analizamos el salario promedio por especializacion
promedio_IT = mean(col_34[col_12 == "information technology"])
promedio_CE = mean(col_34[col_12 == "computer engineering"])
promedio_E = mean(col_34[col_12 == "electronics and communication engineering"])
promedio_CS = mean(col_34[col_12 == "computer science & engineering"])

##################################################################
print(promedio_gpa1)
print(promedio_gpa9)
print(promedio_IT)
print(promedio_tier1)


