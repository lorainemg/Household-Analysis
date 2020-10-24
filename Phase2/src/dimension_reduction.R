require(stats)

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
datos <- data.frame(global_active_power, global_reactive_power, intensity, voltage, submetering1, submetering2, submetering3)

# Calculando la matriz de correlacion
cm <- cor(datos)
round(cm, 4)
symnum(cm)

# Graficando la matriz de correlacion
library(corrplot)
corrplot(cm, type="upper", order="hclust", tl.col="black", tl.srt=45)

# Calculando los componentes principales
acp <- prcomp(datos, scale = TRUE)
summary(acp)
plot(acp)
round(acp$rotation,4)
