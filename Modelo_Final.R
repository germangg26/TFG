#################################################
##### MODELO ARIMA + GARCH PARA EL S&P 500 ######
#################################################



rm(list = ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(quantmod, rugarch, forecast, tseries, ggplot2, gridExtra)


# Descargar datos 
getSymbols("^GSPC", from = "2021-04-01", to = "2025-04-01", src = "yahoo")
sp500 <- na.omit(GSPC[, "GSPC.Close"])

###########################
### Log-Retornos Diarios ##
###########################

log_returns <- na.omit(diff(log(sp500)))


# Graficos
PClose <- ggplot(data = fortify.zoo(sp500), aes(Index, GSPC.Close)) +
  geom_line(color = "steelblue") + 
  ggtitle("S&P 500 - Precio de cierre") + 
  xlab("Fecha") + ylab("Precio") +
  theme_minimal()

LogReturn <- ggplot(data = fortify.zoo(log_returns), aes(Index, GSPC.Close)) +
  geom_line(color = "darkred") + 
  ggtitle("Log-retornos diarios del S&P 500") + 
  xlab("Fecha") + ylab("Retorno logarítmico") +
  theme_minimal()
PClose
LogReturn

#########################
###### STATIONARY #######
#########################
set.seed(1234)
adf.test(log_returns) # estacionario

#####################
###### ARIMA ########
#####################


# Parametros ARIMA para media
acf(log_returns,main="ACF para Log Retunrs") # Lag 0
pacf(log_returns,main="PACF para Log Retunrs") # Lag 0
set.seed(1234)
t.test(log_returns)
arima_fit <- arima(log_returns,order = c(0,0,0))
t.test(log_returns)
summary(arima_fit)
auto.arima(log_returns)
res_arima<-residuals(arima_fit)

## ANALIZAR RESIDUOS

acf(res_arima,main="ACF de residuos")
pacf(res_arima,main="PACF de residuos")
set.seed(1234)
# HO: there are no ARCH effects in the time series
Box.test(res_arima, type="Ljung-Box")

acf(res_arima^2)
pacf(res_arima^2)
set.seed(1234)
Box.test(res_arima^2, type="Ljung-Box")
acf(residuals(arima_fit)^2, main="ACF de residuos al cuadrado")
pacf(residuals(arima_fit)^2, main="PACF de residuos al cuadrado")

#########################
### MODELO ARIMA+GARCH ##
#########################

garch_spec<-ugarchspec(mean.model = list(armaOrder = c(0, 0),include.mean=FALSE),
           variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
           distribution.model = "std")
fit_garch<-ugarchfit(data = log_returns, spec = garch_spec)
# 1,3,8,9 # graficos importantes
fit_garch
plot(fit_garch,which=3)

# Residuos estandarizados
z <- residuals(fit_garch, standardize = TRUE)

# ACF y PACF de los residuos estandarizados
# Debería parecer ruido blanco
acf(z,main="ACF Residuos estandarizados")     
pacf(z,main="PACF Residuos estandarizados") 
# Detecta autocorrelación en la varianza
acf(z^2,main="ACF Residuos estandarizados al cuadrado")
pacf(z^2,main="PACF Residuos estandarizados al cuadrado")

# QQ plot para comparar con la distribución teórica
qqnorm(z)
qqline(z, col = "red")

# Test de Ljung-Box sobre residuos estandarizados y su cuadrado
set.seed(1234)
Box.test(z, lag = 10, type = "Ljung-Box")
set.seed(1234)
Box.test(z^2, lag = 10, type = "Ljung-Box")

