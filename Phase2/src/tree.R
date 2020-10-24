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

# ------------------ Desicion Tree using Cart ------------------
library(rpart)
# eliminamos la variable que vamos a predecir (si no seria muy facil ;p)
# Prediciendo submetering3
# newdata <- data[,-7]
# newdata$predict = data$submetering3 > mean(data$submetering3)
newdata <- data[,-1]
newdata$predict = data$global_active_power > mean(data$global_active_power)

# l = cardinalidad de la poblacion
l <- length(newdata[,1])
l
# sub = se escoge al azar las 2/3 de la poblacion
#       como conjunto entrenante para crear el arbol
sub <- sample(1:l, 4*l/5)
sub
# Conjunto entrenante 
newdata[sub,]
# Conjunto de prueba
newdata[-sub,]
# Construyendo el arbol con el conjunto entrenante
newdata.tree <- rpart(predict~., data=newdata[sub,], cp=0.01, maxdepth=10) 

# Sumario del arbol
summary(newdata.tree)

# Grafico del arbol cart
plot(newdata.tree)
text(newdata.tree, use.n=TRUE, all=TRUE, pretty=0, xpd=TRUE)
plotcp(newdata.tree)
rpart.plot(newdata.tree)
printcp(newdata.tree)

# Hanciendo la prediccion con el conjunto de prueba
# teniendo en cuenta el arbol cart obteniendo
newdata.pred <- predict(newdata.tree, newdata=newdata[-sub,], type="vector")
newdata.pred

##### matriz de confucion
tb <- table(newdata.pred, newdata[-sub,]$predict)

#### Calculo del error del cart ####
error.rpart <- 1-(sum(diag(tb))/sum(tb))
tb
error.rpart
