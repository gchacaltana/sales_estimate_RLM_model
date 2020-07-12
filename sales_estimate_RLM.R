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

#-------------------------------------------------------------------------
# Análisis descriptivo del dataset
#-------------------------------------------------------------------------

# Analizando la distribución (por frecuencia) de las ventas de m2 de la variable tejados.
hist(sales$tejados, main="Distribución de las ventas de m2 de tejado",
     xlab="m2 en miles", ylab="Frecuencia", col=c("bisque"))

# Distribución - de las ventas de la variable tejados

hist(sales$tejados, probability = TRUE,col=c("darkolivegreen1"), 
     main="Distribución de la variable tejados",xlab="m2 en miles")
lines(density(sales$tejados),col=2,lwd=2)

# Analizamos si existe relación de las ventas de m2 de los tejados con sus posibles predictores
# mediante diagramas de dispersión

par(mfrow=c(2,2))

plot(sales$gastos,sales$tejados,
     main="Gráfico de Diagrama de Dispersión: Tejados vendidos vs Gastos en promociones",
     ylab="M2 tejados vendidos",
     xlab="Gastos en promociones (en miles $)")

plot(sales$clientes,sales$tejados,
     main="Gráfico de Diagrama de Dispersión: Tejados vendidos vs número de clientes registrados",
     ylab="M2 tejados vendidos",
     xlab="Número de clientes por filial (en miles)")

plot(sales$marcas,sales$tejados,
     main="Gráfico de Diagrama de Dispersión: Tejados vendidos vs número de marcas competidoras",
     ylab="M2 tejados vendidos",
     xlab="Número de marcas competidoras")

plot(sales$potencial,sales$tejados,
     main="Gráfico de Diagrama de Dispersión: Tejidos vendidos vs Potencial de la filial",
     ylab="M2 tejados vendidos",
     xlab="Potencial de filial")

par(mfrow=c(1,1))

#-------------------------------------------------------------------------
# Construcción del modelo
#-------------------------------------------------------------------------

model_1 <- tejados~gastos+clientes+marcas+potencial
sales.lm0 <- lm(formula = model_1, data=sales)
summary(sales.lm0)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -19.0906  -5.9796   0.8968   6.5667  14.7985 

# Estimación de los coeficientes
coef(sales.lm0)

# (Intercept)      gastos    clientes      marcas   potencial 
# 178.3203403   1.8070643   3.3178334 -21.1849842   0.3245124

#-------------------------------------------------------------------------
# Evaluación del modelo: Prueba de significancia de la regresión
#-------------------------------------------------------------------------

library(sigr)
wrapFTest(sales.lm0)

#"F Test summary: (R2=0.9892, F(4,21)=479.1, p<1e-05)."

#-------------------------------------------------------------------------
# Selección de variables
#-------------------------------------------------------------------------

#Selección Stepwise

library(MASS)
sales.lm0<-lm(tejados~gastos+clientes+marcas+potencial, data=sales)
lm.s2<-stepAIC(sales.lm0, trace = TRUE)

fmla2 <- tejados~gastos+clientes+marcas
sales.rln <- lm(formula = fmla2,data=sales)

summary(sales.rln)
sum(sales.rln$residuals^2)/(26-4)

#-------------------------------------------------------------------------
# Diagnóstico del modelo
#-------------------------------------------------------------------------

# Gráfico Q-Q
qqnorm(rstudent(sales.rln),ylab="Cuantiles de los Residuos Estudentizados")
qqline(rstudent(sales.rln))

# Envelopes
source("http://www.poleto.com/funcoes/envel.norm.txt")
envel.norm(sales.rln)  

# Test de normalidad
shapiro.test(rstudent(sales.rln))
# W = 0.9792, p-value = 0.8566

envel.norm(sales.rln)
# (2) Homocedasticidad              
#---------------------


# Gráfico de residuos estudentizados 

rs2<-rstudent(sales.rln) # Residuos studentizados
pred2 <- predict(sales.rln)  # Valores ajustados

# límites para detectar outliers: quantiles de la distribución t-distribution (corrección de Bonferroni)

n <- dim(sales)[1];p<-length(sales.rln$coefficients);alfa <- 0.01
t2 <- qt(alfa/(n*2),df=n-p-1)
par(mfrow=c(2,2))
plot(pred2,rs2,ylab="residuos estudentizados",xlab="m2 tejados vendidos",
     main="residuos estudentizados versus m2 tejados vendidos",ylim=c(-5,5)) 
abline(-t,0)
abline(t,0)
abline(0,0)

plot(sales$gastos,rs2,ylab="residuos estudentizados",xlab="Gastos de promoción (en miles)",
     main="residuos estudentizados versus gastos de promoción",ylim=c(-5,5)) 
abline(-t,0)
abline(t,0)
abline(0,0)

plot(sales$clientes,rs2, ylab="residuos estudentizados",xlab="clientes registrados (en miles)",
     main="residuos estudentizados versus clientes registrados",ylim=c(-5,5)) 
abline(-t,0)
abline(t,0)
abline(0,0)

plot(sales$marcas,rs2, ylab="residuos estudentizados",xlab="Número de marcas competidoras",
     main="residuos estudentizados versus marcas en competencia",ylim=c(-5,5)) 
abline(-t,0)
abline(t,0)
abline(0,0)
par(mfrow=c(1,1))

#Test de Heterocedasticidad (Test de Breusch-Pagan)

#install.packages("rlang")

library(car)
ncvTest(sales.rln)

# Variance formula: ~ fitted.values 
# Chisquare = 0.3323654, Df = 1, p = 0.56427

# (3) Multicolinealidad    
#----------------------
car::vif(sales.rln)

# gastos clientes   marcas 
# 1.031060 1.150545 1.117925
