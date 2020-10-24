# ----------------- Obteniendo datos -----------------
# Cogiendo y procesando los datos
household <- read.csv('household.csv')
household <- na.omit(household)

global_active_power <- as.numeric(as.character(household$Global_active_power))
global_reactive_power <- as.numeric(as.character(household$Global_reactive_power))
intensity <- as.numeric(as.character(household$Global_intensity))
voltage <-  as.numeric(as.character(household$Voltage))
submetering1 <-  as.numeric(as.character(household$Sub_metering_1))
submetering2 <- as.numeric(as.character(household$Sub_metering_2))
submetering3 <- as.numeric(as.character(household$Sub_metering_3))

# Formando la nueva matriz de datos con las componentes elegidas
data <- data.frame(global_active_power, global_reactive_power, intensity, voltage, submetering1, submetering2, submetering3)

# ------------- Estandarizando los datos -------------
# Estandarizando restando la media y dividiendo por la desviacion tipica
data.matrix <- data.matrix(data)
data.std <-(data - mean(data.matrix)) / sd(data.matrix)
# Dividiendo por el rango
# r <- range(data)      # obteniendo el rango
# Rango -1 a 1 (como el minimo es 0, entonces el rango es de 0 a 1)
#data.std <- data / r # no es factible porque el minimo es 0
#min <- r[1]
#max <- r[2]
# Magnitud maxima de 1
#data.std <- data / max
# Media de 1
#data.std <- data / mean(data.matrix)
# Desviacion tipica 1 
#data.std <- data / sd(data.matrix)
data.std

# Diagramas de dispersion
plot(data$global_active_power, data$global_reactive_power)
plot(data.std$global_active_power, data.std$global_reactive_power)
# ------------ Cluster Jerarquico completo ------------
#sample <- sample(1:nrow(data), size=10000, replace = FALSE)
d <- dist(data.std[0:5000,], method="euclidean") # matriz de distancias con la distancia euclideana
fit <- hclust(d, method = "complete") # ajuste completo
d2 <- as.dendrogram(fit)

plot(fit)

# Dibuja rectangulos rojos alrededor de los clusters
rect.hclust(fit, k=4, border="red")

# ------------------ KMeans ------------------

fit.k1 <- kmeans(data.std, 8)
fit.k1
#plot(data.std, col = fit.k1$cluster)x
plot(data.std$global_active_power, data.std$global_reactive_power, col=fit.k1$cluster, lwd=2)
points(fit.k1$centers, col=1:12, pch=6, lwd=2)

