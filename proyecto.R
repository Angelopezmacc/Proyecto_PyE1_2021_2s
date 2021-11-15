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
# Definimos algunas variables globales Ãºtiles a lo largo de todo el cÃ³digo

# Columnas

col_1 = datos[,1] # ID (IdentificaciÃ³n)
col_2 = datos[,2] # Gender (GÃ©nero)
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

# ---
rango_1_col_34 = list()
rango_2_col_34 = list()
rango_3_col_34 = list()
rango_4_col_34 = list()
rango_5_col_34 = list()


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
hist(as.numeric(rango_1_col_4), xlab = "calificación",ylab = "Frencuencia")
hist(as.numeric(rango_2_col_4), xlab = "calificación",ylab = "Frencuencia")
hist(as.numeric(rango_3_col_4), xlab = "calificación",ylab = "Frencuencia")
hist(as.numeric(rango_4_col_4), xlab = "calificación",ylab = "Frencuencia")
hist(as.numeric(rango_5_col_4), xlab = "calificación",ylab = "Frencuencia")
hist(as.numeric(rango_6_col_4), xlab = "calificación",ylab = "Frencuencia")

percentage_10 = cbind(rango_1_col_4,rango_2_col_4,rango_3_col_4,rango_4_col_4,rango_5_col_4,rango_6_col_4)
percentage_10
hist(as.numeric(percentage_10), xlab = "Calificación",ylab = "Frencuencia")

#--------------------------------------------------------------------------------------------
# Analizando 12graduation - col_6
table(col_6)


#--------------------------------------------------------------------------------------------
# Analizando 12percentage - col_7 
for (i in 1:2941) {
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
hist(as.numeric(rango_1_col_7), xlab = "calificación",ylab = "Frencuencia")
hist(as.numeric(rango_2_col_7), xlab = "calificación",ylab = "Frencuencia")
hist(as.numeric(rango_3_col_7), xlab = "calificación",ylab = "Frencuencia")
hist(as.numeric(rango_4_col_7), xlab = "calificación",ylab = "Frencuencia")
hist(as.numeric(rango_5_col_7), xlab = "calificación",ylab = "Frencuencia")
hist(as.numeric(rango_6_col_7), xlab = "calificación",ylab = "Frencuencia")

percentage_12 = cbind(rango_1_col_7,rango_2_col_7,rango_3_col_7,rango_4_col_7,rango_5_col_7,rango_6_col_7)
percentage_12
hist(as.numeric(percentage_12), xlab = "Calificación",ylab = "Frencuencia")
#--------------------------------------------------------------------------------------------
# Analizando  - CollegeTier
table(col_10)
dotchart(table(col_10))


#--------------------------------------------------------------------------------------------
# Analizando  - CollegeCityTier
table(col_15)
dotchart(table(col_15))


#--------------------------------------------------------------------------------------------
# Analizando el tipo de grado
table(col_11)
barplot(table(col_11))


#--------------------------------------------------------------------------------------------
# Analizando el tipo de especialización
table(col_12)


#--------------------------------------------------------------------------------------------
# Puntaje de inglés

table(col_18)
barplot(table(col_18))


#--------------------------------------------------------------------------------------------
# Puntaje de lógica

table(col_19)
barplot(table(col_19))

#--------------------------------------------------------------------------------------------
# Puntaje de quant

table(col_20)
barplot(table(col_20))

#--------------------------------------------------------------------------------------------
# Puntaje de Computer Programming

table(col_22)
barplot(table(col_22))

#--------------------------------------------------------------------------------------------
# Puntaje de ElectronicsAndSemicon

table(col_23)
barplot(table(col_23))

#--------------------------------------------------------------------------------------------
# Puntaje de ComputerScience

table(col_24)
barplot(table(col_24))

#--------------------------------------------------------------------------------------------
# Puntaje de MechanicalEngg

table(col_25)
barplot(table(col_25))

#--------------------------------------------------------------------------------------------
#Nota: El comportamiento de Electrical, telecom y civil es similar a las presentadas anteriormente
#--------------------------------------------------------------------------------------------
# Análisis salarios:
for (i in 1:2941) {
  if (col_34[i,1] >= 1000 & col_34[i,1] <= 250000){
    rango_1_col_34 = c(rango_1_col_34,col_34[i,1])
  }
  if (col_34[i,1] >= 250000 & col_34[i,1] <= 500000){
    rango_2_col_34 = c(rango_2_col_34,col_34[i,1])
  }
  if (col_34[i,1] >= 500000 & col_34[i,1] <= 1000000){
    rango_3_col_34 = c(rango_3_col_34,col_34[i,1])
  }
  if (col_34[i,1] >= 1000000 & col_34[i,1] <= 2500000){
    rango_4_col_34 = c(rango_4_col_34,col_34[i,1])
  }
  if (col_34[i,1] >= 2500000 & col_34[i,1] <= 5000000){
    rango_5_col_34 = c(rango_5_col_34,col_34[i,1])
  }
}
hist(as.numeric(rango_1_col_34), xlab = "Salario", ylab = "Frecuencia")
hist(as.numeric(rango_2_col_34), xlab = "Salario", ylab = "Frecuencia")
hist(as.numeric(rango_3_col_34), xlab = "Salario", ylab = "Frecuencia")
hist(as.numeric(rango_4_col_34), xlab = "Salario", ylab = "Frecuencia")
hist(as.numeric(rango_5_col_34), xlab = "Salario", ylab = "Frecuencia")

salary = cbind(rango_1_col_34,rango_2_col_34,rango_3_col_34,rango_4_col_34,rango_5_col_34)
salary
hist(as.numeric(salary), xlab = "Salario",ylab = "Frencuencia")
#--------------------------------------------------------------------------------------------
# Analizando el salario a lo largo de CollegeTier - col_10
promedio_tier1 = mean(col_34[col_10 == 1])
promedio_tier2 = mean(col_34[col_10 == 2])


#--------------------------------------------------------------------------------------------
# Analizando el salario en las diferentes especializacion - col_12
promedio_IT = mean(col_34[col_12 == "information technology"])
promedio_CE = mean(col_34[col_12 == "computer engineering"])
promedio_E = mean(col_34[col_12 == "electronics and communication engineering"])
promedio_CS = mean(col_34[col_12 == "computer science & engineering"])


#--------------------------------------------------------------------------------------------
# Analizando el salario en los promedios academicos universitarios - col_14
gpa1 = mean(col_34[col_14 >= 6 & col_14 <= 19.9])
gpa2 = mean(col_34[col_14 > 19.9 & col_14 <= 29.9])
gpa3 = mean(col_34[col_14 > 29.9 & col_14 <= 39.9])
gpa4 = mean(col_34[col_14 > 39.9 & col_14 <= 49.9])
gpa5 = mean(col_34[col_14 > 49.9 & col_14 <= 59.9])
gpa6 = mean(col_34[col_14 > 59.9 & col_14 <= 69.9])
gpa7 = mean(col_34[col_14 > 69.9 & col_14 <= 79.9])
gpa8 = mean(col_34[col_14 > 79.9 & col_14 <= 89.9])
gpa9 = mean(col_34[col_14 > 89.9 & col_14 <= 99.9])



#--------------------------------------------------------------------------------------------
#Realizamos diferentes graficas de los análisis previos

#Grafico de College Tier
Tiers <- c(promedio_tier2, promedio_tier1)
barplot(Tiers, main="Salario", xlab="College Tier")

#Grafico salario por especializacion
Esp <- c(promedio_IT, promedio_CE, promedio_E, promedio_CS)
barplot(Esp, main="Salario", xlab="Especializacion del graduado")


#--------------------------------------------------------------------------------------------
#Grafico salario
GPAs <- c(gpa1, gpa2, gpa3, gpa4, gpa5, gpa6, gpa7, gpa8, gpa9)
barplot(GPAs, main="Salario", xlab="Promedios de clases universitarias")



#--------------------------------------------------------------------------------------------
# Fin del documento
#--------------------------------------------------------------------------------------------


































