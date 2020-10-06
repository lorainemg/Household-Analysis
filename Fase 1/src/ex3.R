# household <- read.csv('/media/loly/02485E43485E359F/_Escuela/__UH/4to/2do Semestre/Estadística/Proyecto/Proyecto Estadistica/Equipo 2 - Hosehold/Code/household.csv')
household <- read.csv('household.csv')
global_active_power <- na.omit(household$Global_active_power) 
global_active_power <- as.numeric(levels(global_active_power))[global_active_power]

global_reactive_power <- na.omit(household$Global_reactive_power) 
global_reactive_power <- as.numeric(levels(global_reactive_power))[global_reactive_power]

hiphothesis <- function(x, y, alpha) {
    n1 <- length(x)
    n2 <- length(y)
    f = var(x, na.rm=TRUE) / var(y, na.rm=TRUE)
    print(f)
    print(qf(alpha/2, n1-1, n2-1))
    print(qf(1-alpha/2, n1-1, n2-1))
    if (f < qf(alpha/2, n1-1, n2-1) | f > qf(1-alpha/2, n1-1, n2-1))
        return(TRUE)
    else
        return(FALSE)
}
res = hiphothesis(global_active_power, global_reactive_power, 0.01)

if (res) {
    print("Las varianzas son diferentes")
} else {
    print("No se puede decir que las varianzas sean diferentes")
}

test <- var.test(global_reactive_power, global_active_power, alternative="t")
print(test)