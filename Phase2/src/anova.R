# Eligiendo los datos
household <- read.csv('household.csv')
household <- na.omit(household)
# sample_examples <- sample(1:nrow(household), size=50000, replace = FALSE)
# household <- household[sample_examples,]
#household$Date <- as.Date(household$Date, format="%d/%m/%Y")
#household <- subset(household, Date >= "2007-02-01" & Date <= "2007-08-02")

household$Global_active_power <- as.numeric(as.character(household$Global_active_power))
household$Global_reactive_power <- as.numeric(as.character(household$Global_reactive_power))
household$Global_intensity <- as.numeric(as.character(household$Global_intensity))
household$Sub_metering_1 <-  as.numeric(as.character(household$Sub_metering_1))
household$Sub_metering_2 <-  as.numeric(as.character(household$Sub_metering_2))
household$Sub_metering_3 <- as.numeric(as.character(household$Sub_metering_3))


# ------------- Graficando los boxplots ---------------
# boxplot(date~global_active_power)
# boxplot(time~global_active_power)

# --------------- Calculando anova -----------------
data.anova <- aov(household$Sub_metering_3~household$Global_active_power+household$Global_intensity, data=household)
summary(data.anova)

# Obteniendo los residuals
res <- data.anova$residuals

# -------------------- Supuestos -----------------
# Supuesto de normalidad
shapiro.test(sample(res, 5000))

# Supuesto de independencia
library(lmtest)
dwtest(data.anova)

# Supuesto de homogeniedad
# bartlett.test(res, global_reactive_power)

# ------------ Graficas de los supuestos -------------
# Histograma
hist(res, main='Histograma de Residuos')

# QQ Plot
qqnorm(res)
qqline(res)

# Residuos estandarizados
plot(data.anova$fitted.values, rstudent(data.anova),
     main="Anova Studentized Residuals",
     xlab="Predictions", ylab="Studentized Resid")
# ylim=c(-2.5, 2.5))
abline(h=0, lty=2)
