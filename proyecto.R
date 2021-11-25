#**********************************************
#Universidad del Rosario                      *
# Proyecto final                              *
# Probailidad y Estad铆stica 1                 *
# 2021 - 2s                                   *
#                                             *
# Juan Obando                                 *
# 脕ngel L贸pez                                 *
#**********************************************

# Para m谩s informaci贸n, por favor remitirse a nuestro repositorio de GitHub. Ah铆 encontrar谩 adem谩s de este c贸digo, 
# el archivo de datos analizado en formato xlsx o ods (depende de su preferencia y aclaramos que el c贸digo a continuaci贸n solo contemple
#archivo con el formato .xlsx). Tambi茅n encontrar谩 un archivo Readme.md en el cual podr谩 obtener una mejor descripci贸n del proyecto
# junto con algunas indicaciones generales. Gracias

#Link al repositorio: https://github.com/Angelopezmacc/Proyecto_PyE1_2021_2s


#--------------------------------------------------------------------------------------------
# Se importa el dataset
rm(list = ls()) # borrar los datos iniciales


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


library(readxl)
dataset_prueba_2 <- read_excel("ur_git/Proyecto_PyE1_2021_2s/dataset_prueba_2.xlsx", 
                               col_types = c("numeric", "text", "text", 
                                             "numeric", "text", "numeric", "numeric", 
                                             "text", "numeric", "numeric", "text", 
                                             "text", "numeric", "numeric", "numeric", 
                                             "text", "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric"))
View(dataset_prueba_2)

#--------------------------------------------------------------------------------------------
# Definimos algunas variables globales 脙潞tiles a lo largo de todo el c脙鲁digo

# Columnas

col_1 = datos[,1] # ID (Identificaci脙鲁n)
col_2 = datos[,2] # Gender (G脙漏nero)
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
# Obtenemos un breve y b谩sico de los datos que tenemos
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
hist(as.numeric(rango_1_col_4), xlab = "calificaci贸n",ylab = "Frencuencia")
hist(as.numeric(rango_2_col_4), xlab = "calificaci贸n",ylab = "Frencuencia")
hist(as.numeric(rango_3_col_4), xlab = "calificaci贸n",ylab = "Frencuencia")
hist(as.numeric(rango_4_col_4), xlab = "calificaci贸n",ylab = "Frencuencia")
hist(as.numeric(rango_5_col_4), xlab = "calificaci贸n",ylab = "Frencuencia")
hist(as.numeric(rango_6_col_4), xlab = "calificaci贸n",ylab = "Frencuencia")

percentage_10 = cbind(rango_1_col_4,rango_2_col_4,rango_3_col_4,rango_4_col_4,rango_5_col_4,rango_6_col_4)
percentage_10
hist(as.numeric(percentage_10), xlab = "Calificaci贸n",ylab = "Frencuencia")

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
hist(as.numeric(rango_1_col_7), xlab = "calificaci贸n",ylab = "Frencuencia")
hist(as.numeric(rango_2_col_7), xlab = "calificaci贸n",ylab = "Frencuencia")
hist(as.numeric(rango_3_col_7), xlab = "calificaci贸n",ylab = "Frencuencia")
hist(as.numeric(rango_4_col_7), xlab = "calificaci贸n",ylab = "Frencuencia")
hist(as.numeric(rango_5_col_7), xlab = "calificaci贸n",ylab = "Frencuencia")
hist(as.numeric(rango_6_col_7), xlab = "calificaci贸n",ylab = "Frencuencia")

percentage_12 = cbind(rango_1_col_7,rango_2_col_7,rango_3_col_7,rango_4_col_7,rango_5_col_7,rango_6_col_7)
percentage_12
hist(as.numeric(percentage_12), xlab = "Calificaci贸n",ylab = "Frencuencia")
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
# Analizando el tipo de especializaci贸n
table(col_12)


#--------------------------------------------------------------------------------------------
# Puntaje de ingl茅s

table(col_18)
barplot(table(col_18))


#--------------------------------------------------------------------------------------------
# Puntaje de l贸gica

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
# An谩lisis salarios:
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
#Realizamos diferentes graficas de los an谩lisis previos

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
#Modelos lineales

#Scatter plot GPA contra salario
# f <- grep(datos, datos$CollegeGPA >= 0
scatter.smooth(x=datos$CollegeGPA, y=datos$Salary, main="GPA~Salario")

#Scatter plot de calificacion en clases contra salario
Scatter.smooth(x=datos$Logic, y=datos$Salary, main="Calificacion Logica~Salario")

#--------------------------------------------------------------------------------------------
#pruebas
a = unlist(col_13, use.names = FALSE)
b = unlist(col_34, use.names = FALSE)

x = c(a)
y = c(b)

plot(x,y,main="funciona") 
alpha=0.05 
n=length(x) 
gl=n-2 

mod = lm(y~x) 
summary(mod) 
abline(mod) 
abline(mod,col=2,lwd=3)

a = unlist(col_18, use.names = FALSE)
b = unlist(col_34, use.names = FALSE)

#--------------------------------------------------------------------------------------------
# Comparativa entre columnas

# Ingl閟 vs salario
a = unlist(col_18, use.names = FALSE)
b = unlist(col_34, use.names = FALSE)
x = c(a)
y = c(b)

plot(x,y,main="縇os que saben ingl閟, ganana m醩?",xlab = "Puntaje ingl閟" , ylab = "Salario")

# logica vs salario
c = unlist(col_19, use.names = FALSE)
d = unlist(col_34, use.names = FALSE)
x = c(c)
y = c(d)

plot(x,y,main="L骻ica vs salario",xlab = "Puntaje l骻ica" , ylab = "Salario")


#--------------------------------------------------------------------------------------------
# Pruebas de hip髏esis
# prueba 1:
summary(datos) # consultamos el promedio de salarios
salario = unlist(col_34, use.names = FALSE)
media = mean(salario)
sd = sd(salario)

qnorm(0.025)

r1 = media - 30605
r2 = sd^2

z = r1 / (sqrt(r2)/sqrt(2941))
z

qnorm(z, lower.tail = F)


# para comprobar resultados, utilizamos la librer韆 BSDA que nos permite hacer el 
# proceso de manera m醩 directa

z.test(salario, alternative = "two.sided", mu=30605, sigma.x = sqrt(r2))


#prueba 2:
# En este caso, la media y desviaci髇 estandar fueron calculados directamente en excel
# debido a la facilidad para filtrar lo datos
summary(dataset_prueba_2) # consultamos el promedio de salarios
salario = unlist(dataset_prueba_2[,34], use.names = FALSE)
media = mean(salario)
sd = sd(salario)

qnorm(0.025)

r1 = media - 50848
r2 = sd^2

z = r1 / (sqrt(r2)/sqrt(1156))
z

pnorm(z, lower.tail = F)

z.test(dataset_prueba_2[,34], alternative = "two.sided", mu=50848, sigma.x = sqrt(r2))


#-------------------------------------------------------------------------------------------
#Modelos lineales

#GPA ~ Salario
a = unlist(col_13, use.names = FALSE)
a1 = c(mean(col_34[col_13 >= 6 & col_13 <= 10]), mean(col_34[col_13 >= 40 & col_13 <= 59.9]), mean(col_34[col_13 >= 60 & col_13 <= 79.9]), mean(col_34[col_13 >= 80 & col_13 <= 100]))
b = unlist(col_34, use.names = FALSE)
b1 = c(mean(col_13[col_13 >= 6 & col_13 <= 10]), mean(col_13[col_13 >= 40 & col_13 <= 59.9]), mean(col_13[col_13 >= 60 & col_13 <= 79.9]), mean(col_13[col_13 >= 80 & col_13 <= 100]))

x = c(b1)
y = c(a1)

plot(x,y,main="GPA ~Salario",xlim=c(0,100)) 
alpha=0.05 
n=length(x) 
gl=n-2 

mod = lm(y~x) 
summary(mod) 
abline(mod) 
abline(mod,col=2,lwd=3)
#print(a1)
#print(b1)


#Ingles ~ Salario
a = unlist(col_19, use.names = FALSE)
a1 = c(mean(col_34[col_19 >= 180 & col_19 <= 353]), mean(col_34[col_19 >= 354 & col_19 <= 527]), mean(col_34[col_19 >= 528 & col_19 <= 701]), mean(col_34[col_19 >= 702 & col_19 <= 876]))
b = unlist(col_34, use.names = FALSE)
b1 = c(mean(col_19[col_19 >= 180 & col_19 <= 353]), mean(col_19[col_19 >= 354 & col_19 <= 527]), mean(col_19[col_19 >= 528 & col_19 <= 701]), mean(col_19[col_19 >= 702 & col_19 <= 876]))


x = c(b1)
y = c(a1)

plot(x,y,main="Ingles ~ Salario") 
alpha=0.05 
n=length(x) 
gl=n-2 

mod = lm(y~x) 
summary(mod) 
abline(mod) 
abline(mod,col=2,lwd=3)
#print(a1)
#print(b1)


#programacion ~ Salario
a = unlist(col_19, use.names = FALSE)
a1 = c(mean(col_34[col_23 >= 160 & col_23 <= 300]), mean(col_34[col_23 >= 301 & col_23 <= 441]), mean(col_34[col_23 >= 442 & col_23 <= 582]), mean(col_34[col_23 >= 583 & col_23 <= 723]))
b = unlist(col_34, use.names = FALSE)
b1 = c(mean(col_23[col_23 >= 180 & col_23 <= 353]), mean(col_23[col_23 >= 354 & col_23 <= 527]), mean(col_23[col_23 >= 528 & col_23 <= 701]), mean(col_23[col_23 >= 702 & col_23 <= 876]))


x = c(b1)
y = c(a1)

plot(x,y,main="Programacion ~ Salario") 
alpha=0.05 
n=length(x) 
gl=n-2 

mod = lm(y~x) 
summary(mod) 
abline(mod) 
abline(mod,col=2,lwd=3)
#print(b1)
#print(a1)


#mecanica ~ Salario
a = unlist(col_19, use.names = FALSE)
a1 = c(mean(col_34[col_26 >= 180 & col_26 <= 290]), mean(col_34[col_26 >= 291 & col_26 <= 401]), mean(col_34[col_26 >= 402 & col_26 <= 512]), mean(col_34[col_26 >= 513 & col_26 <= 623]))
b = unlist(col_34, use.names = FALSE)
b1 = c(mean(col_26[col_26 >= 180 & col_26 <= 353]), mean(col_26[col_26 >= 354 & col_26 <= 527]), mean(col_26[col_26 >= 528 & col_26 <= 701]), mean(col_26[col_26 >= 702 & col_26 <= 876]))


x = c(b1)
y = c(a1)

plot(x,y,main="Mecanica ~ Salario") 
alpha=0.05 
n=length(x) 
gl=n-2 

mod = lm(y~x) 
summary(mod) 
abline(mod) 
abline(mod,col=2,lwd=3)
#print(a1)
#print(b1)


#Extraversion ~ Salario
a = unlist(col_19, use.names = FALSE)
a1 = c(mean(col_34[col_32 >= -4.6 & col_32 <= -2.8]), mean(col_34[col_32 >= -2.9 & col_32 <= -1.1]), mean(col_34[col_32 >= -1.2 & col_32 <= 0.4]), mean(col_34[col_32 >= 0.5 & col_32 <= 2.2]))
b = unlist(col_34, use.names = FALSE)
b1 = c(mean(col_32[col_32 >= -4.6 & col_32 <= -2.8]), mean(col_32[col_32 >= -2.9 & col_32 <= -1.1]), mean(col_32[col_32 >= -1.2 & col_32 <= 0.4]), mean(col_32[col_32 >= 0.5 & col_32 <= 2.2]))


x = c(b1)
y = c(a1)

plot(x,y,main="Extraversion ~ Salario",xlim=c(-5,3)) 
alpha=0.05 
n=length(x) 
gl=n-2 


#-------------------------------------------------------------------------------------------
#Modelos lineales

#GPA ~ Salario
a = unlist(col_13, use.names = FALSE)
a1 = c(mean(col_34[col_13 >= 6 & col_13 <= 10]), mean(col_34[col_13 >= 40 & col_13 <= 59.9]), mean(col_34[col_13 >= 60 & col_13 <= 79.9]), mean(col_34[col_13 >= 80 & col_13 <= 100]))
b = unlist(col_34, use.names = FALSE)
b1 = c(mean(col_13[col_13 >= 6 & col_13 <= 10]), mean(col_13[col_13 >= 40 & col_13 <= 59.9]), mean(col_13[col_13 >= 60 & col_13 <= 79.9]), mean(col_13[col_13 >= 80 & col_13 <= 100]))

x = c(b1)
y = c(a1)

plot(x,y,main="GPA ~Salario",xlim=c(0,100)) 
alpha=0.05 
n=length(x) 
gl=n-2 


mod = lm(y~x) 
summary(mod) 
abline(mod) 
abline(mod,col=2,lwd=3)
#print(a1)
#print(b1)


#Ingles ~ Salario
a = unlist(col_19, use.names = FALSE)
a1 = c(mean(col_34[col_19 >= 180 & col_19 <= 353]), mean(col_34[col_19 >= 354 & col_19 <= 527]), mean(col_34[col_19 >= 528 & col_19 <= 701]), mean(col_34[col_19 >= 702 & col_19 <= 876]))
b = unlist(col_34, use.names = FALSE)
b1 = c(mean(col_19[col_19 >= 180 & col_19 <= 353]), mean(col_19[col_19 >= 354 & col_19 <= 527]), mean(col_19[col_19 >= 528 & col_19 <= 701]), mean(col_19[col_19 >= 702 & col_19 <= 876]))


x = c(b1)
y = c(a1)

plot(x,y,main="Ingles ~ Salario") 
alpha=0.05 
n=length(x) 
gl=n-2 

mod = lm(y~x) 
summary(mod) 
abline(mod) 
abline(mod,col=2,lwd=3)
#print(a1)
#print(b1)


#programacion ~ Salario
a = unlist(col_19, use.names = FALSE)
a1 = c(mean(col_34[col_23 >= 160 & col_23 <= 300]), mean(col_34[col_23 >= 301 & col_23 <= 441]), mean(col_34[col_23 >= 442 & col_23 <= 582]), mean(col_34[col_23 >= 583 & col_23 <= 723]))
b = unlist(col_34, use.names = FALSE)
b1 = c(mean(col_23[col_23 >= 180 & col_23 <= 353]), mean(col_23[col_23 >= 354 & col_23 <= 527]), mean(col_23[col_23 >= 528 & col_23 <= 701]), mean(col_23[col_23 >= 702 & col_23 <= 876]))


x = c(b1)
y = c(a1)

plot(x,y,main="Programacion ~ Salario") 
alpha=0.05 
n=length(x) 
gl=n-2 

mod = lm(y~x) 
summary(mod) 
abline(mod) 
abline(mod,col=2,lwd=3)
#print(b1)
#print(a1)


#mecanica ~ Salario
a = unlist(col_19, use.names = FALSE)
a1 = c(mean(col_34[col_26 >= 180 & col_26 <= 290]), mean(col_34[col_26 >= 291 & col_26 <= 401]), mean(col_34[col_26 >= 402 & col_26 <= 512]), mean(col_34[col_26 >= 513 & col_26 <= 623]))
b = unlist(col_34, use.names = FALSE)
b1 = c(mean(col_26[col_26 >= 180 & col_26 <= 353]), mean(col_26[col_26 >= 354 & col_26 <= 527]), mean(col_26[col_26 >= 528 & col_26 <= 701]), mean(col_26[col_26 >= 702 & col_26 <= 876]))


x = c(b1)
y = c(a1)

plot(x,y,main="Mecanica ~ Salario") 
alpha=0.05 
n=length(x) 
gl=n-2 

mod = lm(y~x) 
summary(mod) 
abline(mod) 
abline(mod,col=2,lwd=3)
#print(a1)
#print(b1)


#Extraversion ~ Salario
a = unlist(col_19, use.names = FALSE)
a1 = c(mean(col_34[col_32 >= -4.6 & col_32 <= -2.8]), mean(col_34[col_32 >= -2.9 & col_32 <= -1.1]), mean(col_34[col_32 >= -1.2 & col_32 <= 0.4]), mean(col_34[col_32 >= 0.5 & col_32 <= 2.2]))
b = unlist(col_34, use.names = FALSE)
b1 = c(mean(col_32[col_32 >= -4.6 & col_32 <= -2.8]), mean(col_32[col_32 >= -2.9 & col_32 <= -1.1]), mean(col_32[col_32 >= -1.2 & col_32 <= 0.4]), mean(col_32[col_32 >= 0.5 & col_32 <= 2.2]))


x = c(b1)
y = c(a1)

plot(x,y,main="Extraversion ~ Salario",xlim=c(-5,3)) 
alpha=0.05 
n=length(x) 
gl=n-2 

mod = lm(y~x) 
summary(mod) 
abline(mod) 
abline(mod,col=2,lwd=3)
#print(a1)
#print(b1)


#--------------------------------------------------------------------------------------------
# Fin del documento
#--------------------------------------------------------------------------------------------


































