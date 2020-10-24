# ------------------ Cogiendo los datos ------------------
household <- read.csv('file:///media/loly/02485E43485E359F/_Escuela/__UH/4to/2do Semestre/Estadística/Proyecto/Household-Analysis/Phase2/src/household.csv')
household <- na.omit(household)

# Probando distintas maneras de obtener muestras
#sample_examples <- sample(1:nrow(household), size=50000, replace = FALSE)
#household <- household[sample_examples,]
#household$Date <- as.Date(household$Date, format="%d/%m/%Y")
#household <- subset(household, Date >= "2007-02-01" & Date <= "2007-03-01")

global_active_power <- as.numeric(as.character(household$Global_active_power))
global_reactive_power <- as.numeric(as.character(household$Global_reactive_power))
global_intensity <- as.numeric(as.character(household$Global_intensity))
voltage <-  as.numeric(as.character(household$Voltage))
submetering1 <-  as.numeric(as.character(household$Sub_metering_1))
submetering2 <- as.numeric(as.character(household$Sub_metering_2))
submetering3 <- as.numeric(as.character(household$Sub_metering_3))

# Formando la nueva matriz de datos con las componentes elegidas
data <- data.frame(global_active_power, global_reactive_power, global_intensity, voltage, submetering1, submetering2, submetering3)

# Obteniendo la matriz de correlacion
household.cor <- cor(data.matrix(data))
household.cor

library(corrplot)
corrplot(household.cor, type="upper", order="hclust", tl.col="black", tl.srt=45)

# Grafico de dispersion de las dos variables
plot(global_active_power, global_intensity)

# -------------------------- Linear Regression -----------
model.fit <- lm(global_active_power~global_intensity, data=data)
summary(model.fit)

# model.fit <- lm(global_active_power~global_intensity+global_reactive_power, data=household)
# summary(model.fit)

# ------------------------- Supuestos -------------------------
library(lmtest)
# La media de los errores es cero y la suma de los errores es cero
mean(model.fit$residuals)
sum(model.fit$residuals)

# Independencia de los residuos
dwtest(model.fit)

# Homocedasticidad
bptest(model.fit)

# Distribucion Normal con media 0 y varianza constante
shapiro.test(sample(model.fit$residuals, 5000))

# ---------------- Graficando los supuestos ----------------
# Histograma
hist(model.fit$residuals, main='Histograma de Residuos')

# QQ Plot
qqnorm(model.fit$residuals)
qqline(model.fit$residuals)

# Residuos estandarizados
plot(model.fit$fitted.values, rstudent(model.fit),
     main="Anova Studentized Residuals",
     xlab="Predictions", ylab="Studentized Resid")
abline(h=0, lty=2)

#plot(model.fit)