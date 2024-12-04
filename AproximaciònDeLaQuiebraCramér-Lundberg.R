# -------------------------------
# Modelo de Cramér-Lundberg
# -------------------------------

# Parámetros del modelo
capital_inicial <- 1    # Capital inicial
tasa_primas <- 5       # Tasa de entrada de primas (fija)
tiempo_simulacion <- 100 # Tiempo total de simulación
num_trayectorias <- 1000 # Número de trayectorias simuladas
numero_muestral <- 10000 # Número de muestras para aplicar la ley de los grandes números


# -------------------------------
# Simulación para múltiples muestras
# -------------------------------
for (i in 1:numero_muestral) {
  # Generar parámetros aleatorios bajo las restricciones del modelo
  tasa_poisson <- runif(1, min = 0.1, max = 0.8) # Aseguramos que nunca sea 0
  beta_min <- max(1 / (tasa_poisson * tasa_primas), 0.01) # Evita valores negativos o NA
  beta_max <- 0.9
  
  # Verifica que los valores de beta sean válidos
  if (beta_min >= beta_max) {
    next # Saltar esta iteración si no se puede generar un rango válido
  }
  
  param_exponencial <- runif(1, min = beta_min, max = beta_max)
  
  # Validación explícita de la condición de ganancia neta
  if (tasa_poisson / param_exponencial >= tasa_primas) {
    next # Saltar esta iteración si no se cumple la condición
  }
  
  # Contadores para ruinas en las trayectorias simuladas
  contador_ruina <- 0
  
  # -------------------------------
  # Simulación de trayectorias
  # -------------------------------
  for (simulacion in 1:num_trayectorias) {
    # Generar tiempos de las reclamaciones
    intervalos_tiempo <- rexp(n = tiempo_simulacion * tasa_poisson, rate = tasa_poisson)
    tiempos_reclamos <- cumsum(intervalos_tiempo)
    tiempos_reclamos <- tiempos_reclamos[tiempos_reclamos <= tiempo_simulacion]
    
    # Generar montos de las reclamaciones
    montos_reclamos <- rexp(length(tiempos_reclamos), rate = param_exponencial)
    
    # Calcular la trayectoria del capital
    primas_acumuladas <- tasa_primas * tiempos_reclamos
    reclamos_acumulados <- cumsum(montos_reclamos)
    trayectoria_capital <- capital_inicial + primas_acumuladas - reclamos_acumulados
    
    # Revisar si hay ruina
    if (any(trayectoria_capital < 0)) {
      contador_ruina <- contador_ruina + 1
    }
  }
  
  # Calcular y almacenar la probabilidad de ruina para esta muestra
  proba_ruina <- contador_ruina / num_trayectorias
  vector_ruina[i] <- proba_ruina
}

# -------------------------------
# Resultados de la simulación
# -------------------------------
promedio_ruina <- mean(vector_ruina, na.rm = TRUE) # Ignorar valores NA si ocurren
cat("Promedio de la probabilidad estimada de ruina: ", promedio_ruina, "\n")

