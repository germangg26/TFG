

rm(list=ls())
pacman::p_load(tidyverse,tseries,np,boot,dplyr,ggplot2,nortest)

#########################
#### LECTURA DE DATOS ###
#########################

ARIMA_metrics<-read.csv("ARIMA_GARCH_metrics_log_returns.csv",header=TRUE,sep=",")
head(ARIMA_metrics)
tail(ARIMA_metrics)
LSTM_metrics <- read.csv("LSTM_metrics.csv")
LSTM_metrics<-LSTM_metrics%>%
  filter(Date%in%ARIMA_metrics$Date)
head(LSTM_metrics)
tail(LSTM_metrics)
dim(ARIMA_metrics)
dim(LSTM_metrics)

#####################################
##### Creacion de  metricas #########
#####################################

calculate_metrics <- function(data) {
  data %>%
    mutate(
      index=row_number(),
      error = Actual - Predicted,
      APE = (abs(error / Actual))*100
    )
}


ARIMA_metrics<-calculate_metrics(ARIMA_metrics)
LSTM_metrics<-calculate_metrics(LSTM_metrics)
quantile(ARIMA_metrics$APE-LSTM_metrics$APE,c(0.025,0.975))


##############################
#### DEPENDENCIA TEMPORAL ####
##############################

### Errores
acf(ARIMA_metrics$error)
pacf(ARIMA_metrics$error)
Box.test(ARIMA_metrics$error, type="Ljung-Box")



acf(LSTM_metrics$error)
pacf(LSTM_metrics$error)
Box.test(LSTM_metrics$error, type="Ljung-Box")

## APE
acf(ARIMA_metrics$APE,main="ACF Métrica APE Modelo ARIMA")
pacf(ARIMA_metrics$APE,main="PACF Métrica APE Modelo ARIMA")
Box.test(ARIMA_metrics$APE, type="Ljung-Box")



acf(LSTM_metrics$APE,main="ACF Métrica APE Modelo LSTM")
pacf(LSTM_metrics$APE,main="PACF Métrica APE Modelo LSTM")
Box.test(LSTM_metrics$APE, type="Ljung-Box")

### DIFERENCIA ERROR
acf(ARIMA_metrics$error-LSTM_metrics$error)
pacf(ARIMA_metrics$error-LSTM_metrics$error)
Box.test(ARIMA_metrics$error-LSTM_metrics$error, type="Ljung-Box")

### DIFERENCIA APE
acf(ARIMA_metrics$APE-LSTM_metrics$APE)
pacf(ARIMA_metrics$APE-LSTM_metrics$APE)
Box.test(ARIMA_metrics$APE-LSTM_metrics$APE, type="Ljung-Box")


# Crear data frame con todos los modelos
ARIMA_metrics$model <- "ARIMA"
LSTM_metrics$model <- "LSTM"

combined_data <- bind_rows(
  ARIMA_metrics %>% select(APE, model),
  LSTM_metrics %>% select(APE, model)
)


#################
#### BOXPLOT ####
#################

# Calcular la media de APE por modelo
media_ape <- combined_data %>%
  group_by(model) %>%
  summarise(mean_ape = mean(APE, na.rm = TRUE))

# Crear el gráfico
Violin_Metrics <- ggplot(combined_data, aes(x = model, y = APE, fill = model)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white") +
  geom_jitter(width = 0.1, alpha = 0.3, color = "black", size = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  geom_label(
    data = media_ape,
    aes(
      x = model,
      y = max(combined_data$APE) * 0.95,  # Ajusta altura según tus datos
      label = paste0("Media: ", formatC(mean_ape, format = "f", digits = 2))
    ),
    fill = "white",
    color = "black",
    label.size = 0.8,
    size = 7,
    fontface = "bold",
    label.padding = unit(0.3, "lines")
  ) +
  scale_fill_manual(values = c("ARIMA" = "#00BFC4", "LSTM" = "#F8766D")) +
  labs(
    title = "Comparación de APE entre ARIMA y LSTM",
    x = "Modelo",
    y = "APE (Absolute Percentage Error)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


Violin_Metrics

###################################
##### BOOSTRAP INDICES ############
###################################

optimal_l_arima <- b.star(ARIMA_metrics$APE)
l_elegida_arima <- round(mean(optimal_l_arima))
optimal_l_lstm <- b.star(LSTM_metrics$APE)
l_elegida_lstm <- round(mean(optimal_l_lstm))
l_elegida_indice<- max(l_elegida_arima,l_elegida_lstm)
set.seed(1234)
boot_index<-tsboot(
  tseries = LSTM_metrics$index,  R = 10000,
  statistic = function(x, i) x[i],
  l = l_elegida_indice,
  sim = "geom"
)

## Extraer metrica MAPE por cada muestra boostrap
extract_MAPE<-function(boot,data){
  n_simul<-boot$R
  MAPE<-numeric(n_simul)
  for (i in 1:boot$R){
  order_boot<-boot_index$t[i,]
  MAPE[i]<-mean(data$APE[order_boot])
  }
  return(MAPE)
}

### ARIMA
boot_ARIMA <- extract_MAPE(boot_index, ARIMA_metrics) %>%
  as.data.frame() %>%
  mutate(Model = "ARIMA") 
colnames(boot_ARIMA)<-c("MAPE","model")

### LSTM
boot_LSTM<-extract_MAPE(boot_index,LSTM_metrics)%>%
  as.data.frame()%>%
  mutate(Model="LSTM")
colnames(boot_LSTM)<-c("MAPE","model")

### Combinar ambos modelos
boot_combined<-rbind(boot_ARIMA,boot_LSTM)

##############################
#### DEPENDENCIA TEMPORAL ####
##############################

### ARIMA
acf(boot_ARIMA$MAPE,lag.max = 20,main="ACF Métrica MAPE Muestra Bootstrap (ARIMA)")
pacf(boot_ARIMA$MAPE,lag.max = 20,main="PACF Métrica MAPE Muestra Bootstrap (ARIMA)")
set.seed(1234)
Box.test(boot_ARIMA$MAPE, type="Ljung-Box")
## LSTM
acf(boot_LSTM$MAPE,main="ACF Métrica MAPE Muestra Bootstrap (LSTM)")
pacf(boot_LSTM$MAPE,main="PACF Métrica MAPE Muestra Bootstrap (LSTM)")
set.seed(1234)
Box.test(boot_LSTM$MAPE, type="Ljung-Box")


###############
### BOXPLOT ###
###############

# Calcular la media de APE por modelo
media_mape <- boot_combined %>%
  group_by(model) %>%
  summarise(mean_mape = mean(MAPE, na.rm = TRUE))
Violin_MAPE <- ggplot(boot_combined, aes(x = model, y = MAPE, fill = model)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  geom_label(
    data = media_mape,
    aes(
      x = model,
      y = max(boot_combined$MAPE) * 0.95,  # Ajusta altura según tus datos
      label = paste0("Media: ", formatC(mean_mape, format = "f", digits = 2))
    ),
    fill = "white",
    color = "black",
    label.size = 0.8,
    size = 7,
    fontface = "bold",
    label.padding = unit(0.3, "lines")
  ) +
  scale_fill_manual(values = c("ARIMA" = "#00BFC4", "LSTM" = "#F8766D")) +
  labs(
    title = "Comparación de MAPE entre ARIMA y LSTM",
    x = "Modelo",
    y = "MAPE (Absolute Percentage Error)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

Violin_MAPE



##########################
## ANÁLISIS DIFERENCIAS ##
##########################


boot_Diferences <- boot_ARIMA$MAPE - boot_LSTM$MAPE
df_boot_diff <- data.frame(diff = boot_Diferences)

#### Histograma ####

# Calcular media y altura del histograma
mean_diff <- mean(df_boot_diff$diff)
hist_data <- ggplot_build(
  ggplot(df_boot_diff, aes(x = diff)) + geom_histogram(bins = 30))$data[[1]]
y_pos <- max(hist_data$count) * 0.95

# Gráfico 
Hist_diff<-ggplot(df_boot_diff, aes(x = diff)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "white", alpha = 0.8) +
  geom_vline(xintercept = 0, color = "black", linetype = "dotted", linewidth = 0.8) +
  labs(
    title = "Distribución Bootstrap de la Diferencia de MAPEs",
    x = "Diferencia de MAPE (ARIMA - LSTM)",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray30")
  )
Hist_diff

###########################
### Test Estádisticos ####
###########################

# Crear vector de diferencias (estan pareadas)
boot_Diferences <- boot_ARIMA$MAPE - boot_LSTM$MAPE

# Media observada
obs_diff <- mean(boot_Diferences)

# Permutaciones de signos
set.seed(123)  
n_perm <- 1000
perm_diffs <- replicate(n_perm, {
  signs <- sample(c(-1, 1), length(boot_Diferences), replace = TRUE)
  mean(signs * boot_Diferences)
})

# p-valor bilateral
p_value <- mean(abs(perm_diffs) >= abs(obs_diff))

# Resultados
cat("Diferencia media observada de MAPE (ARIMA - LSTM):", round(obs_diff, 4), "\n")
cat("p-valor (perm. emparejada):", round(p_value, 4), "\n")

# Estimar el intervalo de confianza al 95%
ic_diff_mape_boot <- quantile(boot_Diferences, probs = c(0.025, 0.975))


##########################
## Comprobar normalidad ##
##########################

# set.seed(1234)
# lillie.test(boot_ARIMA$MAPE) # Sino fuera apareado
# set.seed(1234)
# lillie.test(boot_LSTM$MAPE) # Sino fuera apareado
set.seed(1234)
lillie.test(boot_Diferences)


acf(boot_Diferences,main="ACF de las Diferencias de la métrica MAPE de los modelos")
pacf(boot_Diferences,main="PACF de las Diferencias de la métrica MAPE de los modelos")
set.seed(1234)
Box.test(boot_Diferences, type="Ljung-Box")


#t.test(boot_Diferences) Si fuera Normal
wilcox.test(mape_diff)




