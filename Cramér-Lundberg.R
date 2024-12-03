# -------------------------------
# Modelo de Cramér-Lundberg
# -------------------------------

# Parámetros del modelo
capital_inicial <- 0.5     # Capital inicial
tasa_primas <- 0.2         # Tasa de entrada de primas
tasa_poisson <- 0.1        # Tasa del proceso de Poisson (lambda)
param_exponencial <- 0.04  # Parámetro de la distribución exponencial (beta)
tiempo_simulacion <- 100   # Tiempo total de simulación
num_trayectorias <- 1000   # Número de trayectorias simuladas

# Verificar la condición de ganancia neta
if (tasa_poisson / param_exponencial >= tasa_primas) {
  stop("La condición de ganancia neta no se cumple: tasa_poisson / param_exponencial debe ser menor que tasa_primas.")
}

# Contadores para los resultados
contador_ruina <- 0
contador_exitos <- 0

# Graficar el espacio vacío para las trayectorias
plot(NULL, 
     xlim = c(0, tiempo_simulacion), 
     ylim = c(0, capital_inicial + tasa_primas * tiempo_simulacion), 
     xlab = "Tiempo", ylab = "Capital", 
     main = "Modelo de Cramér-Lundberg")
abline(h = 0, col = "red") # Línea de referencia en 0

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
  
  # Graficar la trayectoria (opcional)
  lines(c(0, tiempos_reclamos), c(capital_inicial, trayectoria_capital), col = rgb(0, 0, 1, 0.1))
}

# -------------------------------
# Resultados de la simulación
# -------------------------------
proba_ruina <- contador_ruina / num_trayectorias
proba_no_quiebra <- 1 - proba_ruina

cat("Probabilidad estimada de ruina: ", proba_ruina, "\n")
cat("Número de trayectorias con ruina: ", contador_ruina, "\n")
cat("Número de trayectorias sin ruina: ", num_trayectorias - contador_ruina, "\n")
cat("Probabilidad estimada de no quiebra: ", proba_no_quiebra, "\n")


