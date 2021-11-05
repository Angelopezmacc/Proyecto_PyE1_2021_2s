#**********************************************
#Universidad del Rosario                      *
# Proyecto final                              *
# Probailidad y Estadística 1                 *
# 2021 - 2s                                   *
#                                             *
# Juan Obando                                 *
# Ángel López                                 *
#**********************************************

# Para más información, por favor remitirse a nuestro repositorio de GitHub. Ahí encontrará además de este código, 
# el archivo de datos analizado en formato xlsx o ods (depende de su preferencia y aclaramos que el código a continuación solo contemple
#archivo con el formato .xlsx). También encontrará un archivo Readme.md en el cual podrá obtener una mejor descripción del proyecto
# junto con algunas indicaciones generales. Gracias

#Link al repositorio: https://github.com/Angelopezmacc/Proyecto_PyE1_2021_2s

#--------------------------------------------------------------------------------------------
# Se importa el dataset
library(readxl)
data <- read_excel("Documentos/UR 2021-2/Git/Proyecto_PyE1_2021_2s/dataset_engineering_graduate_salary.xlsx",
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
# Definimos algunas variables globales útiles a lo largo de todo el código

# Columnas
col_1 = datos[,1] # ID (Identificación)
col_2 = datos[,2] # Gender (Género)
col_3 = datos[,3] # DOB (fecha de nacimiento)
col_4 = datos[,4] #
col_5 = datos[,5] # 
col_6 = datos[,6] # 
col_7 = datos[,7] # 
col_8 = datos[,8] # 
col_9 = datos[,9] # 
col_10 = datos[,10] # 
col_11 = datos[,11] # 
col_12 = datos[,12] # 
col_13 = datos[,13] # 
col_14 = datos[,14] # 
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
col_34 = datos[,34] # 

#hasta 34 cols
#--------------------------------------------------------------------------------------------
# Obtenemos un breve y básico de los datos que tenemos
summary(datos)

#--------------------------------------------------------------------------------------------
# Analizando la cantidad de hombres y mujeres en nuestro dataset
tabla_sexo = table(col_2); tabla_sexo
pie(tabla_sexo)

#--------------------------------------------------------------------------------------------









































