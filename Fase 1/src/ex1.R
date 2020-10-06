household <- read.csv('household.csv')
# household <- read.csv('/media/loly/02485E43485E359F/_Escuela/__UH/4to/2do Semestre/Estadística/Proyecto/Proyecto Estadistica/Equipo 2 - Hosehold/Code/household.csv')
global_active_power <- na.omit(household$Global_active_power) 
global_active_power <- as.numeric(levels(global_active_power))[global_active_power]

global_reactive_power <- na.omit(household$Global_reactive_power) 
global_reactive_power <- as.numeric(levels(global_reactive_power))[global_reactive_power]

intensity <- na.omit(household$Global_intensity) 
intensity <- as.numeric(levels(intensity))[intensity]

descriptor <- function(x) {
    mean_x <- mean(x, na.rm=TRUE)
    cat('Mean:', mean_x, '\n')
    median_x <- median(x, na.rm=TRUE)
    cat('Median:', median_x, '\n')
    var_x <- var(x, na.rm=TRUE)
    cat('Variance:', var_x, '\n')
    sd_x <- sd(x, na.rm=TRUE)
    cat('Standard Deviation:', sd_x, '\n')
    cv_x <- sd_x / abs(mean_x)
    cat('CV:', cv_x, '\n')
    quantiles <- quantile(x, na.rm=TRUE)
    print('Quantiles:')
    print(quantiles)
}


print('Printing statistics descriptors of Global Active Power')
descriptor(global_active_power)

print('-----------------------------------------------------')
print('Printing statistics descriptors of Global Reactive Power')
descriptor(global_reactive_power)

print('-----------------------------------------------------')
print('Printing statistics descriptors of Intensity')
descriptor(intensity)

# Plotting the boxes
jpeg('Plots/ex1/BoxPlots/BoxPlots_GAP.jpeg')
    boxplot(global_active_power, main="Boxplot de la Corriente Global Activa", ylab="Corriente Global Activa")
dev.off()
jpeg('Plots/ex1/BoxPlots/BoxPlots_GRP.jpeg')
    boxplot(global_reactive_power, main="Boxplot de Corriente Global Rectiva", ylab="Corriente Global Reactiva")
dev.off()
jpeg('Plots/ex1/BoxPlots/BoxPlots_Intensity.jpeg')
    boxplot(intensity, main="Boxplot de la Intensidad", ylab="Intensity")
dev.off()

jpeg('Plots/ex1/Box Plots.jpeg')
    boxplot(global_active_power, main="Boxplot de la Corriente Global Activa", ylab="Corriente Global Activa")
    boxplot(global_reactive_power, main="Boxplot de la Corriente Global Rectiva", ylab="Corriente Global Reactiva")
    boxplot(intensity, main="Boxplot de la Intensidad", ylab="Intensity")
dev.off()

# Ploting the histograms
plotting <- function(x, name) {
    hist(x,
        freq=FALSE,
        col="red",
        xlab=name,
        main=name)
    # rug(jitter(x))
    lines(density(x, na.rm=TRUE), col="blue", lwd=2)
    box()
}

jpeg('Plots/ex1/Powers.jpeg')
    p1 <- hist(global_active_power, breaks=50, plot=FALSE) 
    p2 <- hist(global_reactive_power, breaks=11, plot=FALSE)
    c1 <- rgb(0,0,1, 1/4)
    c2 <- rgb(1,0,0, 1/4)
    plot(p1, col=c1, main="Diferencias entre la corriente global activa y reactiva", xlab="", xlim=c(0, 7))
    plot(p2, col=c2, xlim=c(0, 7), add=T)
    legend("topright", c("Corriente Global Activa", "Corriente Global Reactiva"), fill=c(c1, c2))
dev.off()


jpeg('Plots/ex1/Histograms.jpeg')
    plotting(global_active_power, 'Corriente Global Activa')
    plotting(global_reactive_power, 'Corriente Global Reactiva')
    plotting(intensity, 'Intensidad')
dev.off()


jpeg('Plots/ex1/Histograms/Histograms_GAP.jpeg')
    plotting(global_active_power, 'Corriente Global Activa')
dev.off()
jpeg('Plots/ex1/Histograms/Histograms_GRP.jpeg')
    plotting(global_reactive_power, 'Corriente Global Reactiva')
dev.off()
jpeg('Plots/ex1/Histograms/Histograms_Intensity.jpeg')
    plotting(intensity, 'Intensidad')
dev.off()