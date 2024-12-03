# Variables fijas
  u <- 0.5             # Capital inicial
  p <- 0.2          # Tasa de entrada de primas
  lambda_param <- 0.1   # Tasa del proceso de Poisson
  beta <- 0.04           # Parámetro de la distribución exponencial para reclamaciones
  T <- 100    # Tiempo de simulación
  numTray <- 1000 # Número de trayectorias



# Condición de ganancia neta
#(lambda_param / beta < p) 


#Contadores
ruina <- 0
exitos <- 0

# Genera la grafica vacia
plot(NULL, xlim = c(0, T), ylim = c(0, u + p * T), 
     xlab = "Tiempo", ylab = "Capital", 
     main = "Modelo de Cramér-Lundberg")
abline(h = 0, col = "red")

#Simulaciòn
for (simul in 1:numTray) {
  # Generar tiempos entre reclamaciones
  interarrival_times <- rexp(n = T * lambda_param, rate = lambda_param)
  claim_times <- cumsum(interarrival_times)
  claim_times <- claim_times[claim_times <= T]
  
  # Generar montos de las reclamaciones
  claims <- rexp(length(claim_times), rate = beta)
  
  # Calcular la trayectoria del capital
  premiums <- p * claim_times
  total_claims <- cumsum(claims)
  capital <- u + premiums - total_claims
  
  # Revisar si hay ruina
  if (any(capital < 0)) {
    ruina <- ruina + 1
  }
  # Graficar una trayectoria (opcional)
  lines(c(0, claim_times), c(u, capital), col = rgb(0, 0, 1, 0.1))
}





#############################
#Resultados de la simulaciòn#
#############################
probaFrac <- ruina / numTray
probaExit <- (numTray - ruina) / numTray
cat("Probabilidad estimada de ruina: ", probaFrac, "\n")
cat("Numero de ruinas:", ruina, "\n")
cat("Numero de exitos:", numTray-ruina, "\n")
cat("Probabilidad estimada de no quebrar: ", probaExit, "\n")
