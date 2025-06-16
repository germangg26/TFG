# Trabajo de Fin de Grado (TFG) – Estadística Aplicada (UAB)

**Autor:** Germán Gallego Garcia  
**Grado en Estadística Aplicada – Universitat Autònoma de Barcelona (UAB)**  
**Curso académico:** 2024–2025

Este repositorio contiene los distintos archivos de código utilizados en el desarrollo de mi **Trabajo de Fin de Grado en Estadística Aplicada**. El trabajo se centra en la **validación cruzada en series temporales** y la **comparación de modelos predictivos clásicos y de aprendizaje profundo**.

El archivo `ARIMA_GARCH_Expanding_Window.R` está basado en el proyecto universitario sueco _"Time Series Forecasting and Dynamic Asset Allocation: ARIMA and GARCH Models in Portfolio Management"_. El resto de los scripts han sido desarrollados de forma original durante este TFG.

## Archivos incluidos

- **`ARIMA_GARCH_Expanding_Window.R`**  
  Código en **R** que implementa un esquema de **validación cruzada para series temporales** utilizando el modelo **ARIMA+GARCH**. Basado en el TFG sueco anteriormente mencionado. Ejecutado en **RStudio**.

- **`LSTM_Expanding_Window.ipynb`**  
  Código en **Python**, ejecutado en **Google Colab**, que aplica validación cruzada para series temporales utilizando una **Red Neuronal Recurrente (RNN)** con **celdas LSTM**.

- **`Evaluation_Metrics.R`**  
  Script en **R** para la evaluación de métricas obtenidas con los modelos anteriores. Contiene los pasos, figuras y tablas presentados en el trabajo.

- **`Modelo_Final.R`**  
  Código en **R** que realiza un análisis más exhaustivo del modelo seleccionado como más eficaz, incluyendo una evaluación detallada y visualizaciones. Ejecutado en **RStudio**.
