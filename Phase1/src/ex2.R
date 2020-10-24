p_norm = rnorm(500)

s1 = sample(p_norm, 20, replace = TRUE)
s2 = sample(p_norm, 20, replace = FALSE)
s3 = sample(p_norm, 30, replace = TRUE)
s4 = sample(p_norm, 30, replace = FALSE)
s5 = sample(p_norm, 60, replace = TRUE)
s6 = sample(p_norm, 60, replace = FALSE)
s7 = sample(p_norm, 250, replace = TRUE)
s8 = sample(p_norm, 250, replace = FALSE)

# ----------------------------------------------------------------
#                           Descriptors
# ----------------------------------------------------------------
descriptor <- function(x) {
    cat('\nSample of size:', length(x), '\n')
    mean_x <- mean(x)
    cat('Mean:', mean_x, '\n')
    median_x <- median(x)
    cat('Median:', median_x, '\n')
    var_x <- var(x)
    cat('Variance:', var_x, '\n')
    sd_x <- sd(x)
    cat('Standard Deviation:', sd_x, '\n')
    cv_x <- sd_x / abs(mean_x)
    cat('CV:', cv_x, '\n')    
    quantiles <- quantile(x)
    print('Quantiles:')
    print(quantiles)
}

descriptor(s1)
descriptor(s2)
descriptor(s3)
descriptor(s4)
descriptor(s5)
descriptor(s6)
descriptor(s7)
descriptor(s8)

descriptor(p_norm)


# ----------------------------------------------------------------
#                           Graphics
# ----------------------------------------------------------------
jpeg('Plots/ex2/Boxplots.jpeg')
    boxplot(s1, main="Boxplot de la Muestra 1 - 20R", ylab="Muestra 1")
    boxplot(s2, main="Boxplot de la Muestra 2 - 20", ylab="Muestra 2")
    boxplot(s3, main="Boxplot de la Muestra 3 - 30R", ylab="Muestra 3")
    boxplot(s4, main="Boxplot de la Muestra 4 - 30", ylab="Muestra 4")
    boxplot(s5, main="Boxplot de la Muestra 5 - 60", ylab="Muestra 5")
    boxplot(s6, main="Boxplot de la Muestra 6 - 60R", ylab="Muestra 6")
    boxplot(s7, main="Boxplot de la Muestra 7 - 250R", ylab="Muestra 7")
    boxplot(s8, main="Boxplot de la Muestra 8 - 250", ylab="Muestra 8")
    boxplot(p_norm, main="Boxplot de la Población", ylab="Población")
dev.off()

# Plotting the boxes
jpeg('Plots/ex2/BoxPlots/BoxPlots1.jpeg')
    boxplot(s1, main="Boxplot de la Muestra 1 - 20R", ylab="Muestra 1")
dev.off()
jpeg('Plots/ex2/BoxPlots/BoxPlots2.jpeg')
    boxplot(s2, main="Boxplot de la Muestra 2 - 20", ylab="Muestra 2")
dev.off()
jpeg('Plots/ex2/BoxPlots/BoxPlots3.jpeg')
    boxplot(s3, main="Boxplot de la Muestra 3 - 30R", ylab="Muestra 3")
dev.off()
jpeg('Plots/ex2/BoxPlots/BoxPlots4.jpeg')
    boxplot(s4, main="Boxplot de la Muestra 4 - 30", ylab="Muestra 4")
dev.off()
jpeg('Plots/ex2/BoxPlots/BoxPlots5.jpeg')
    boxplot(s5, main="Boxplot de la Muestra 5 - 60", ylab="Muestra 5")
dev.off()
jpeg('Plots/ex2/BoxPlots/BoxPlots6.jpeg')
    boxplot(s6, main="Boxplot de la Muestra 6 - 60R", ylab="Muestra 6")
dev.off()
jpeg('Plots/ex2/BoxPlots/BoxPlots7.jpeg')
    boxplot(s7, main="Boxplot de la Muestra 7 - 250R", ylab="Muestra 7")
dev.off()
jpeg('Plots/ex2/BoxPlots/BoxPlots8.jpeg')
    boxplot(s8, main="Boxplot de la Muestra 8 - 250", ylab="Muestra 8")
dev.off()
jpeg('Plots/ex2/BoxPlots/BoxPlotsPop.jpeg')
    boxplot(p_norm, main="Boxplot de la Población", ylab="Población")
dev.off()

# Ploting the histograms
plotting <- function(x, name) {
    hist(x,
        freq=FALSE,
        col="red",
        xlab=name,
        main=name)
    # rug(jitter(x))
    lines(density(x), col="blue", lwd=2)
    box()
}


jpeg('Plots/ex2/Histograms.jpeg')
    plotting(s1, 'Muestra 1 - 20R')
    plotting(s2, 'Muestra 2 - 20')
    plotting(s3, 'Muestra 3 - 30R')
    plotting(s4, 'Muestra 4 - 30')
    plotting(s5, 'Muestra 5 - 60R')
    plotting(s6, 'Muestra 6 - 60')
    plotting(s7, 'Muestra 7 - 250R')
    plotting(s8, 'Muestra 8 - 250')
    plotting(p_norm, 'Población')
dev.off()


jpeg('Plots/ex2/Histograms/Histogram1.jpeg')
    plotting(s1, 'Muestra 1 - 20R')
dev.off()
jpeg('Plots/ex2/Histograms/Histogram2.jpeg')
    plotting(s2, 'Muestra 2 - 20')
dev.off()
jpeg('Plots/ex2/Histograms/Histogram3.jpeg')
    plotting(s3, 'Muestra 3 - 30R')
dev.off()
jpeg('Plots/ex2/Histograms/Histogram4.jpeg')
    plotting(s4, 'Muestra 4 - 30')
dev.off()
jpeg('Plots/ex2/Histograms/Histogram5.jpeg')
    plotting(s5, 'Muestra 5 - 60R')
dev.off()
jpeg('Plots/ex2/Histograms/Histogram6.jpeg')
    plotting(s6, 'Muestra 6 - 60')
dev.off()
jpeg('Plots/ex2/Histograms/Histogram7.jpeg')
    plotting(s7, 'Muestra 7 - 250R')
dev.off()
jpeg('Plots/ex2/Histograms/Histogram8.jpeg')
    plotting(s8, 'Muestra 8 - 250')
dev.off()
jpeg('Plots/ex2/Histograms/HistogramPop.jpeg')
    plotting(p_norm, 'Población')
dev.off()

# ----------------------------------------------------------------
#                   Confidence Intervals
# ----------------------------------------------------------------
confidence_interval_mean_known <- function(x, alpha) {
    sqrt_n <- sqrt(length(x))
    mean_x <- mean(x) 
    percentile <- qnorm(1-alpha/2)
    var_x <- var(x)
    i_0 <- mean_x - percentile * var_x / sqrt_n
    i_1 <- mean_x + percentile * var_x / sqrt_n
    return(c(i_0, i_1))
}


# Intervalo de confianza para estimar la media con varianza desconocida
confidence_interval_mean <- function(x, alpha) {
    n <- length(x)
    mean_x <- mean(x) 
    var_x <- var(x)
    if (n > 30) {
        percentile <- qnorm(1-alpha/2)  
    } else {
        percentile <- qt(1-alpha/2, n-1)
    }
    i_0 <- mean_x - percentile * var_x / sqrt(n)
    i_1 <- mean_x + percentile * var_x / sqrt(n)
    return(c(i_0, i_1))
}


# Intervalo de confianza para estimar la varianza
confidence_interval_var <- function(x, alpha) {
    n <- length(x)
    var_x <- var(x)
    i_0 <- (n-1)*var_x**2 / qchisq(1-alpha/2, n-1) 
    i_1 <- (n-1)*var_x**2 / qchisq(alpha/2, n-1)
    return(c(i_0, i_1))
}

cat('\n')
cat('\n')
print('Mean')
print(confidence_interval_mean(s1, 0.01))
print(confidence_interval_mean(s2, 0.01))
print(confidence_interval_mean(s3, 0.01))
print(confidence_interval_mean(s4, 0.01))
print(confidence_interval_mean(s5, 0.01))
print(confidence_interval_mean(s6, 0.01))
print(confidence_interval_mean(s7, 0.01))
print(confidence_interval_mean(s8, 0.01))
print(confidence_interval_mean(p_norm, 0.01))

cat('\n')
print('Variance')
print(confidence_interval_var(s1, 0.01))
print(confidence_interval_var(s2, 0.01))
print(confidence_interval_var(s3, 0.01))
print(confidence_interval_var(s4, 0.01))
print(confidence_interval_var(s5, 0.01))
print(confidence_interval_var(s6, 0.01))
print(confidence_interval_var(s7, 0.01))
print(confidence_interval_var(s8, 0.01))
print(confidence_interval_var(p_norm, 0.01))