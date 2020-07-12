# Estimación y diagnóstico del Modelo de Regresión Lineal Normal
# @author Gonzalo Chacaltana Buleje

# Carga del dataset
sales <- read.csv(file="dataset/data.csv")

#-------------------------------------------------------------------------
# Revisión rápida de la estructura del dataset
#-------------------------------------------------------------------------

# Primeros registros (observaciones)
head(sales)
#tejados gastos clientes marcas potencial
#    79.3    5.5       31     10         8
#   200.1    2.5       55      8         6
#   163.2    8.0       67     12         9
#   200.1    3.0       50      7        16
#   146.0    3.0       38      8        15
#   177.7    2.9       71     12        17

# Tipo de variables
str(sales)

# Resumen del dataset
summary(sales)
# tejados          gastos         clientes         marcas         potencial     
# Min.   : 30.9   Min.   :2.500   Min.   :26.00   Min.   : 4.000   Min.   : 3.000  
# 1st Qu.:102.0   1st Qu.:4.000   1st Qu.:40.50   1st Qu.: 8.000   1st Qu.: 6.000  
# Median :159.8   Median :5.500   Median :51.50   Median : 9.000   Median : 9.000  
# Mean   :170.2   Mean   :5.408   Mean   :51.85   Mean   : 9.115   Mean   : 9.885  
# 3rd Qu.:217.5   3rd Qu.:6.650   3rd Qu.:61.50   3rd Qu.:11.000   3rd Qu.:13.750  
# Max.   :339.4   Max.   :9.000   Max.   :75.00   Max.   :13.000   Max.   :19.000 

