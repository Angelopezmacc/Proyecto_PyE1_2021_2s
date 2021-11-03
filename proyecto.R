#**********************************************
#Universidad del Rosario                      *
# Proyecto final                              *
# Probailidad y Estadística 1                 *
# 2021 - 2s                                   *
#                                             *
# Juan Obando                                 *
# Ángel López                                 *
#**********************************************

#--------------------------------------------------------------------------------------------
# Se importa el dataset
library(readxl)
dataset_engineering_graduate_salary <- read_excel("Documentos/UR 2021-2/git/Proyecto_PyE1_2021_2s/dataset_engineering_graduate_salary.xlsx", 
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
#View(dataset_engineering_graduate_salary)

#--------------------------------------------------------------------------------------------
summary(dataset_engineering_graduate_salary)

#--------------------------------------------------------------------------------------------


















