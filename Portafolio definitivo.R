options(digits = 4, scipen = 999) # datos sin fomarto cientifico, y solo con cuatro decimales


# Jorge Andres Sanchez 
#librerias necesarias
library(quantmod)
library(scales)
library(ggplot2)
library(corrplot)
library(readxl)
library(PerformanceAnalytics)
library(tidyverse)
library(knitr)


datos = read_excel("C:/Users/jorge/Downloads/datos_finanzas.xlsx")

v0 = 10000000 # plata a invertir( simplente multiplique por los pesos, y le dara que tant plata invertir por accion)

d = 252 # Numero de dias que opera la bolsa
r = colMeans(datos*100)*d # vector de rendimientos en escala log, y tasa anual, y en porcentaje
(Sigma = cov(datos*100)) # matriz de varianzas y covarianzas en porcentaje

# Elegimos una paleta de colores (de rojo a azul, pasando por blanco)
colores = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200)

corrplot(cor(datos), 
         method = "color",       # Usa cuadros de color completos
         col = colores,          # Aplicamos nuestra paleta
         type = "upper",         # Solo la parte superior (evita repetición)
         order = "hclust",       # Agrupa variables similares automáticamente
         addCoef.col = "black",  # Pone el número del coeficiente en negro
         tl.col = "black",       # Color de las etiquetas (texto)
         tl.srt = 45,            # Rotación de las etiquetas para que se lean bien
)

Sigma_inv = solve(Sigma)# matriz de varianzas y covarianzas inversa
vec_1 = rep(1, ncol(datos)) # vector de unos


#######################################################################
################## 1. Portafolio pesos iguales #########################
#######################################################################
n = ncol(Sigma_inv) # numero de activos
pesos2 = rep(1/n, n) # se debe ajustar, dependiendo de la
plata2 = v0*pesos2
(ri = t(pesos2)%*%(r)) #rendimiento esperados Anual
(var1 =(t(pesos2)%*%Sigma%*%pesos2)) # varianza de pesos iguales

#########################################################################
################# 2. Portafolio minima varianza #######################
#########################################################################

# Hallamos el delta/varianza minima
delta = as.numeric(solve(t(vec_1)%*%Sigma_inv%*%vec_1))
(pesos1 = delta*Sigma_inv%*%vec_1) # Pesos
(plata1 = v0*pesos1)
(rc1= t(pesos1)%*%(r))#Rendimiento esperados Anual
(var2 =(t(pesos1)%*%Sigma%*%pesos1)) # Varianza minima

#######################################################
###### 3. Portafolio con rentabilidad deseada #########
#######################################################
v = cbind(r, rep(1, n))

#matriz omega 
omega = solve(Sigma)%*%v

#matriz de merton
merton = t(omega)%*%Sigma%*%omega

A =merton[1, 2]
C = merton[2, 2]
B = merton[1, 1]
D = det(merton)

rd = 25 # rentabilidad deseada(en este caso, un 20% anual)
E = c(rd, 1)
(pesos3 = omega%*%solve(merton)%*%E) # pesos
t(pesos3)%*%(r) # rentabilidad
(var3 = (t(pesos3)%*%Sigma%*%pesos3)) # varianza


##################################################################
# 4. Para hallar el portafolio optimo del radio de sharpe
##################################################################

#Nota. Si se desea introducir un activo libre de riesgo, simplente cambie el valor 0, por su quivalente a rendimiento anual.

rf_d = 4 # rendimento de activo libre de riesg0 en escala normal, y rendimineto anual
# rentabilidad anual activo libre de riesgo, en escala log, expresada en porcentajes

p =solve(Sigma)%*%((r-rf_d*rep(1, n)))
(pesos4= p/abs(A-C*rf_d)) # pesos con el activo libre de riesgo

rf1 = rf_d# rendimientos diarios activoo libre de riesgo 
(rf2 = t(pesos4)%*%(r)) # rendimientos anuales del portafolio tangente

var4 = (t(pesos4)%*%Sigma%*%(pesos4)) # varianza del portafolio

#Ratio de sharpe
rz_sharp = ((rf2)-(rf1))/(sqrt(var4))
rz_sharp/100

###################################################################
#######################Grafico ####################################
###################################################################


sigma_min   = as.numeric(sqrt(delta))
E_min_tilde = as.numeric(rc1)         
D_val       = as.numeric(D) 

#rango de retornos 
E_range <- seq(0, 35, length.out = 500)

sigma =  sigma_min * sqrt(1 + (1/D_val) * ((E_range - E_min_tilde) / sigma_min^2)^2)

df = data.frame(E = E_range, sigma = sigma)

#Graficamos


ggplot(df, aes(x = sigma, y = E)) +
  
  # 1. Hipérbola completa
  geom_path(color = "grey80", size = 0.8, linetype = "solid") + 
  
  # 2. FRONTERA EFICIENTE 
  geom_path(data = subset(df, E >= E_min_tilde), 
            aes(color = "Frontera Eficiente"), size = 1.5) +
  
  # 6. CML 

  geom_abline(aes(intercept = rf_d, slope = rz_sharp, color = "CML"), size = 1.5) +
  
  # Puntos de anotación 
  annotate("point", x = sigma_min, y = E_min_tilde, color = "red", size = 3) +
  annotate("text", x = sigma_min, y = E_min_tilde, label = " Minima Varianza", hjust = -0.2, color = "red") +
  annotate("point", x = sqrt(var1), y = ri, color = "#5cb85c", size = 3) +
  annotate("text", x = sqrt(var1), y = ri, label = " Pesos iguales", hjust = -0.2, color = "#5cb85c") +
  annotate("point", x = sqrt(var3), y = rd, color = "black", size = 3) +
  annotate("text", x = sqrt(var3), y = rd, label = " Rendimiento Deseado", hjust = -0.2, color = "black") +
  annotate("point", x = sqrt(var4), y = rf2, color = "#5bc0de", size = 3) +
  annotate("text", x = sqrt(var4), y = rf2, label = " Ratio de sharpe", hjust = 1.1, color = "#5bc0de") +
  
  # descripcion
  scale_color_manual(name = "", values = c("Frontera Eficiente" = "#2b5797", "CML" = "grey50")) +
  
  # Formato estético
  scale_x_continuous(limits = c(0, max(df$sigma) -3), expand = c(0,0), labels = label_percent(scale = 1)) +
  scale_y_continuous(limits = c(20, 30), expand = c(0,0), labels = label_percent(scale = 1)) +
  labs(
    title = "Frontera Eficiente de Markowitz",
    x = expression(Riesgo ~ (sigma)),
    y = expression(Retorno ~ Anual ~ E(R))
  ) + 
  theme_classic() +
  theme(legend.position = "bottom") 

#################################################################################################
##############################Var y Cvar ########################################################
#################################################################################################

x = as.vector(datos[,1]*pesos4[1] +datos[, 2]*pesos4[2] +datos[,3]*pesos4[3]+ datos[, 4]*pesos4[4])
a = unlist(x) 

rf2 = rf2/100
dr = sqrt(var4) # desviacion rendimientos
vrc = c(rf2/252, rf2/252, rf2/252, rf2/252)
alphas = c(0.9, 0.95, 0.975, 0.99)

#clasico
var_clasico = vrc+c(dr)*qnorm(alphas)
var_clasico/100
# historico
quantile(a, probs = c(0.9, 0.95, 0.975,0.99 ))

#skew epdf
b1 = skewness(a) # Asimetría
#Kurtosis epdf
b2 = kurtosis(a)-3

# cornis fisher
c_fisher = function(alpha, b1, b2) {
  Z_alpha = qnorm(alpha)
  
  term1 = Z_alpha
  term2 = (b1*(Z_alpha^2 - 1)) / 6
  term3 = ((b2 - 3) * (Z_alpha^3 - 3 * Z_alpha)) / 24
  term4 = -((2 * Z_alpha^3 - 5 * Z_alpha) * b1^2) / 36
  term5 = -((Z_alpha^4 - 5 * Z_alpha^2 + 2) * (b2 - 3)*b1) / 24
  W_alpha = term1 + term2 + term3 + term4 + term5
}


(var_fisher= abs(vrc-c_fisher(alphas, b1, b2)*c(dr)))/100


## Vamos con cvar

#clasico
(cvar_clasico = vrc+(dnorm(qnorm(alphas))*c(dr))/(1-alphas))/100

#cvar fisher
c_fisher2 = function(alpha, b1, b2) {
  Z_alpha = qnorm(alpha)
  
  term1 = 1/6*(Z_alpha)*b1
  term2 = ((b2 - 3) * (Z_alpha^2 - 1)) / 24
  term3 = -((2 * Z_alpha^2 - 1) * b1^2) / 36
  term4 = -((Z_alpha^3 - 2 * Z_alpha) * (b2 - 3)*b1) / 24
  
  W_alpha = (1 + term1 + term2 + term3 + term4)*dnorm(qnorm(alphas))
}

(cvarf = abs(vrc - (1/(1-alphas))*c_fisher2(alphas, b1, b2)*c(dr)))/100


#####################################################################################
##########################Modelo Camp################################################
#####################################################################################


activos <- c("MSFT", "PPL", "V")
mercado <- "^GSPC"
getSymbols(c(activos, mercado), from = "2023-10-17", to = "2025-10-24", src = "yahoo")
precios_activos <- merge(Ad(MSFT), Ad(PPL), Ad(V))
precios_mercado <- Ad(GSPC)

colnames(precios_activos) <- activos
colnames(precios_mercado) <- "Mercado"
ret_activos <- na.omit(Return.calculate(precios_activos, method = "log"))
ret_mercado <- na.omit(Return.calculate(precios_mercado, method = "log"))
ret_activos_anual <- ret_activos * 252
ret_mercado_anual <- ret_mercado * 252
betas <- sapply(colnames(ret_activos_anual), function(x) {
  cov(ret_activos_anual[, x], ret_mercado_anual) / var(ret_mercado_anual)
})

tabla_betas <- data.frame(
  Activo = names(betas),
  Beta = as.numeric(betas)
)

tabla_betas
kable(tabla_betas, format = "latex", caption = "Tabla de betas")

Rf <- 0.04
Rm <- 0.25

ret_esperado <- Rf + betas * (Rm - Rf)

tabla_capm <- data.frame(
  Activo = names(betas),
  Beta = betas,
  Retorno_Esperado = ret_esperado
)

tabla_capm
tabla_capm$Prima_Riesgo <- tabla_capm$Retorno_Esperado - Rf
tabla_capm
kable(tabla_capm, format = "latex", caption = "Tabla CAPM")

ggplot(tabla_capm, aes(x = Beta, y = Prima_Riesgo)) +
  geom_point(size = 3) +
  geom_text(aes(label = Activo), vjust = -1) +
  geom_abline(intercept = 0, slope = (Rm - Rf)) +
  labs(
    title = "Relación Beta vs Prima de Riesgo",
    x = expression(beta),
    y = "Prima de Riesgo"
  ) +
  theme_minimal()

