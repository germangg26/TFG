# Instalar si no están
if (!require(quantmod)) install.packages("quantmod")
if (!require(rugarch)) install.packages("rugarch")
if (!require(tseries)) install.packages("tseries")
if (!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(zoo)) install.packages("zoo")
if (!require(xts)) install.packages("xts")
if (!require(forecast)) install.packages("forecast")
if (!require(tidyverse)) install.packages("tidyverse")

library(quantmod)
library(rugarch)
library(tseries)
library(PerformanceAnalytics)
library(ggplot2)
library(zoo)
library(xts)
library(forecast)
library(tidyverse)

rm(list=ls())

# Descargar precios
getSymbols("^GSPC", from = "2021-04-01", to = "2025-04-01", src = "yahoo")
sp500 <- GSPC$GSPC.Close

# Calcular log-returns (series a modelar)
log_returns <- diff(log(sp500))
log_returns <- na.omit(log_returns)  # eliminar NA

# Función modificada del TFG de Suecia (mencionada en la parte escrita)
expanding_ARIMA_GARCH_forecast <- function(ts_returns, ts_prices, initial_window_size, forecast_days) {
  
  garch_forecasted_returns_list <- list()
  garch_forecasted_volatility_list <- list()
  forecasted_prices_list <- list()
  execution_time <- list()
  
  for (i in seq(initial_window_size, length(ts_returns) - forecast_days)) {
    start_time <- Sys.time()
    
    current_window <- ts_returns[1:i]  
    # ARIMA sobre log-returns
    arima_fit <- auto.arima(current_window)
    residuals <- residuals(arima_fit)
    
    # GARCH sobre residuos
    garch_spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                             variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                             distribution.model = "nig")
    garch_fit <- ugarchfit(data = residuals, spec = garch_spec)
    
    # Predecir volatilidad futura
    forecast_volatility <- ugarchforecast(garch_fit, n.ahead = forecast_days)
    forecasted_volatility <- forecast_volatility@forecast$sigmaFor
    
    # Predecir retornos futuros con ARIMA
    arima_forecast <- forecast(arima_fit, h = forecast_days)
    arima_point_forecast <- as.numeric(arima_forecast$mean)
    
    # Combinar pronóstico ARIMA + volatilidad GARCH para retorno simulado
    set.seed(123)
    random_shocks <- forecasted_volatility * rnorm(forecast_days)
    final_forecasted_returns <- arima_point_forecast + random_shocks
    
    # Convertir retorno pronosticado a precio pronosticado:
    last_price <- as.numeric(ts_prices[i + 1])  
    
    # Precio pronosticado: P_{t+1} = P_t * exp(retorno)
    price_forecast <- last_price * exp(final_forecasted_returns[1])
    
    garch_forecasted_returns_list[[length(garch_forecasted_returns_list) + 1]] <- final_forecasted_returns[1]
    garch_forecasted_volatility_list[[length(garch_forecasted_volatility_list) + 1]] <- forecasted_volatility[1]
    forecasted_prices_list[[length(forecasted_prices_list) + 1]] <- price_forecast
    
    end_time <- Sys.time()
    execution_time[[length(execution_time) + 1]] <- as.numeric(difftime(end_time, start_time, units = "secs"))
  }
  
  # Vectores de salida
  forecasted_returns_vec <- unlist(garch_forecasted_returns_list)
  forecasted_vol_vec <- unlist(garch_forecasted_volatility_list)
  forecasted_prices_vec <- unlist(forecasted_prices_list)
  
  # Observados: precios reales (desde after initial window + 1 para que coincida con pronósticos)
  observed_prices_vec <- coredata(ts_prices[(initial_window_size + 2):(length(ts_prices) - forecast_days + 1)])
  
  exec_time_vec <- unlist(execution_time)
  
  forecasted_prices_df <- data.frame(
    Date = as.Date(index(ts_prices)[(initial_window_size + 2):(length(ts_prices) - forecast_days + 1)]),
    GSPC.Close = observed_prices_vec,
    GARCH_Forecast = forecasted_prices_vec,
    GARCH_Volatility = forecasted_vol_vec,
    Exec_Time_Sec = exec_time_vec
  )
  
  return(list(forecasted_prices_df = forecasted_prices_df))
}

# Ejecutar pronóstico expanding window sobre log-returns
prueba <- expanding_ARIMA_GARCH_forecast(log_returns, sp500, initial_window_size = 955, forecast_days = 1)
forecasted_prices_df <- prueba$forecasted_prices_df

# Visualización con días previos
n_prev_days <- 90
start_idx <- (955 + 2) - n_prev_days  

dates_prev <- index(sp500)[start_idx:(955 + 1)]
prices_prev <- coredata(sp500[start_idx:(955 + 1)])

prev_df <- data.frame(
  Date = as.Date(dates_prev),
  GSPC.Close = prices_prev,
  GARCH_Forecast = NA,
  GARCH_Volatility = NA,
  Exec_Time_Sec = NA
)

forecasted_prices_df_extended <- rbind(prev_df, forecasted_prices_df)

Forecast_ARIMA <- ggplot(data = forecasted_prices_df_extended, aes(x = Date)) +
  geom_line(aes(y = GSPC.Close, color = "Precio Real"), linewidth = 1.2) +
  geom_line(aes(y = GARCH_Forecast, color = "Pronóstico ARIMA+GARCH"), linewidth = 1.2, linetype = "dashed") +
  geom_vline(xintercept = as.numeric(forecasted_prices_df$Date[1]), 
             linetype = "dashed", color = "gray40", linewidth = 1) +
  labs(
    title = "Comparación de Precios Reales vs. Predichos (ARIMA+GARCH sobre log-returns)",
    x = "Fecha",
    y = "Precio de cierre",
    color = ""
  ) +
  scale_color_manual(values = c("Precio Real" = "blue", "Pronóstico ARIMA+GARCH" = "orange")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13)
  )

print(Forecast_ARIMA)

# Guardar en PDF
pdf(file="Forecast_ARIMA_log_returns.pdf")
print(Forecast_ARIMA)
dev.off()

# Tiempo ejecución
ggplot(forecasted_prices_df, aes(x = Date, y = Exec_Time_Sec)) +
  geom_line(color = "#2ecc71", linewidth = 1.2) +
  labs(
    title = "Tiempo de Ejecución por Iteración",
    x = "Fecha",
    y = "Tiempo (segundos)"
  ) +
  theme_minimal(base_size = 14)

sum(forecasted_prices_df$Exec_Time_Sec)
mean(forecasted_prices_df$Exec_Time_Sec)

# Guardar métricas
ARIMA_GARCH_metrics <- forecasted_prices_df %>%
  select(Date, Actual = GSPC.Close, Predicted = GARCH_Forecast)
direct <- paste0(getwd(), "/ARIMA_GARCH_metrics.csv")
write.csv(ARIMA_GARCH_metrics, file = "ARIMA_GARCH_metrics_log_returns.csv", row.names = FALSE)
