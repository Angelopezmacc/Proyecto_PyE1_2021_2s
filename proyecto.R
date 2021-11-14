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

# En Linux
# library(readxl)
# datos <- read_excel("Documentos/UR 2021-2/Git/Proyecto_PyE1_2021_2s/dataset_engineering_graduate_salary.xlsx",
#                     col_types = c("numeric", "text", "text",
#                                   "numeric", "text", "numeric", "numeric",
#                                   "numeric", "numeric", "numeric", "text", "text",
#                                   "numeric", "numeric", "numeric", "text", "numeric",
#                                   "numeric", "numeric", "numeric", "numeric",
#                                   "numeric", "numeric", "numeric", "numeric",
#                                   "numeric", "numeric", "numeric",
#                                   "numeric", "numeric", "numeric", "numeric", "numeric",
#                                   "numeric", "text", "text", "text",
#                                  "text"))

# En windows
# library(readxl)
# datos <- read_excel("ur_git/Proyecto_PyE1_2021_2s/dataset_engineering_graduate_salary.xlsx")
# col_types = c("numeric", "text", "text",
#               "numeric", "text", "numeric", "numeric",
#               "numeric", "numeric", "numeric", "text", "text",
#               "numeric", "numeric", "numeric", "text", "numeric",
#               "numeric", "numeric", "numeric", "numeric",
#               "numeric", "numeric", "numeric", "numeric",
#               "numeric", "numeric", "numeric",
#               "numeric", "numeric", "numeric", "numeric", "numeric",
#               "numeric", "text", "text", "text",
#               "text")
# View(datos)
library(readxl)
library(readxl)
datos <- read_excel("ur_git/Proyecto_PyE1_2021_2s/dataset_engineering_graduate_salary.xlsx")
                                                  col_types = c("numeric", "text", "text", 
                                                                "numeric", "text", "numeric", "numeric", 
                                                                "text", "numeric", "numeric", "text", 
                                                                "text", "numeric", "numeric", "numeric", 
                                                                "text", "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "skip", "skip", "skip", "skip")
View(datos)

#--------------------------------------------------------------------------------------------
# Definimos algunas variables globales útiles a lo largo de todo el código

# Columnas

col_1 = datos[,1] # ID (Identificación)
col_2 = datos[,2] # Gender (Género)
col_3 = datos[,3] # DOB (fecha de nacimiento)
col_4 = datos[,4] # 10percentage
col_5 = datos[,5] # 10board
col_6 = datos[,6] # 12graduation
col_7 = datos[,7] # 12percentage
col_8 = datos[,8] # 12board
col_9 = datos[,9] # CollegeID
col_10 = datos[,10] # CollegeTier
col_11 = datos[,11] # Degree
col_12 = datos[,12] # Specialization
col_13 = datos[,13] # CollegeGPA
col_14 = datos[,14] # COllegeCityID
col_15 = datos[,15] # CollegeCityTier
col_16 = datos[,16] # CollegeState
col_17 = datos[,17] # GraduationYear
col_18 = datos[,18] # English
col_19 = datos[,19] # Logical
col_20 = datos[,20] # Quant
col_21 = datos[,21] # Domain
col_22 = datos[,22] # ComputerProgramming
col_23 = datos[,23] # ElectronicsAndSemicon
col_24 = datos[,24] # ComputerScience
col_25 = datos[,25] # MechanicalEngg
col_26 = datos[,26] # ElectricalEngg
col_27 = datos[,27] # TelecomEngg
col_28 = datos[,28] # CivilEngg
col_29 = datos[,29] # Conscientiouness
col_30 = datos[,30] # Agreeableness
col_31 = datos[,31] # Extraversion
col_32 = datos[,32] # Neuroticism
col_33 = datos[,33] # Openess_to_experience
col_34 = datos[,34] # Salary

#hasta 34 cols

# Definimos listas vacias
rango_1_col_4 = list()
rango_2_col_4 = list()
rango_3_col_4 = list()
rango_4_col_4 = list()
rango_5_col_4 = list()
rango_6_col_4 = list()

# ---

rango_1_col_7 = list()
rango_2_col_7 = list()
rango_3_col_7 = list()
rango_4_col_7 = list()
rango_5_col_7 = list()
rango_6_col_7 = list()

#--------------------------------------------------------------------------------------------
# Obtenemos un breve y básico de los datos que tenemos
summary(datos)

#--------------------------------------------------------------------------------------------
# Analizando la cantidad de hombres y mujeres - col_3
tabla_sexo = table(col_2); tabla_sexo
pie(tabla_sexo)
#--------------------------------------------------------------------------------------------
# Analizando 10percentage - col_4

for (i in 1:2998) {
  if (col_4[i,1] >= 40.00 & col_4[i,1] <= 50.00){
    rango_1_col_4 = c(rango_1_col_4,col_4[i,1])
  }
  if (col_4[i,1] >= 50.00 & col_4[i,1] <= 60.00){
    rango_2_col_4 = c(rango_2_col_4,col_4[i,1])
  }
  if (col_4[i,1] >= 60.00 & col_4[i,1] <= 70.00){
    rango_3_col_4 = c(rango_3_col_4,col_4[i,1])
  }
  if (col_4[i,1] >= 70.00 & col_4[i,1] <= 80.00){
    rango_4_col_4 = c(rango_4_col_4,col_4[i,1])
  }
  if (col_4[i,1] >= 80.00 & col_4[i,1] <= 90.00){
    rango_5_col_4 = c(rango_5_col_4,col_4[i,1])
  }
  if (col_4[i,1] >= 90.00 & col_4[i,1] <= 100.00){
    rango_6_col_4 = c(rango_6_col_4,col_4[i,1])
  }
}
hist(as.numeric(rango_1_col_4))
hist(as.numeric(rango_2_col_4))
hist(as.numeric(rango_3_col_4))
hist(as.numeric(rango_4_col_4))
hist(as.numeric(rango_5_col_4))
hist(as.numeric(rango_6_col_4))

#--------------------------------------------------------------------------------------------
# Analizando 10board - col_5

#--------------------------------------------------------------------------------------------
# Analizando 12graduation - col_6
table(col_6)
#--------------------------------------------------------------------------------------------
# Analizando 12percentage - col_7
for (i in 1:2998) {
  if (col_7[i,1] >= 40.00 & col_7[i,1] <= 50.00){
    rango_1_col_7 = c(rango_1_col_7,col_7[i,1])
  }
  if (col_7[i,1] >= 50.00 & col_7[i,1] <= 60.00){
    rango_2_col_7 = c(rango_2_col_7,col_7[i,1])
  }
  if (col_7[i,1] >= 60.00 & col_7[i,1] <= 70.00){
    rango_3_col_7 = c(rango_3_col_7,col_7[i,1])
  }
  if (col_7[i,1] >= 70.00 & col_7[i,1] <= 80.00){
    rango_4_col_7 = c(rango_4_col_4,col_7[i,1])
  }
  if (col_7[i,1] >= 80.00 & col_7[i,1] <= 90.00){
    rango_5_col_7 = c(rango_5_col_7,col_7[i,1])
  }
  if (col_7[i,1] >= 90.00 & col_7[i,1] <= 100.00){
    rango_6_col_7 = c(rango_6_col_7,col_7[i,1])
  }
}
hist(as.numeric(rango_1_col_7))
hist(as.numeric(rango_2_col_7))
hist(as.numeric(rango_3_col_7))
hist(as.numeric(rango_4_col_7))
hist(as.numeric(rango_5_col_7))
hist(as.numeric(rango_6_col_7))
#--------------------------------------------------------------------------------------------
# Analizando 12board - col_8
table(col_8)

#--------------------------------------------------------------------------------------------
# Analizando CollegeTier - col_10
table(col_10)

#--------------------------------------------------------------------------------------------
# Fin del documento



































