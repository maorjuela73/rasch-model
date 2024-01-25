# Implementacion de modelo de Rasch para datos binarios

# Cargamos librerias
library(ggplot2)
library(haven)
library(RM.weights)

# Cargamos datos
data <- read_dta("data/inseguridad alimentaria.dta")

# Convertir en NA valores superiores a 2
data[, 2:9][data[, 2:9] > 2] <- NA

# Suma de respuestas por fila
data$sum <- rowSums(data[, 2:9])

# Distribucion de respuestas
table(data$sum)

# Gráfico de barras de distribucion de respuestas
ggplot(data, aes(x = sum)) +
  geom_bar(fill = "steelblue") +
  labs(x = "Suma de respuestas", y = "Frecuencia") +
  theme_bw()

# Distribucion de respuestas en proporciones
prop.table(table(data$sum)) * 100

# Crear una nueva variable donde si la suma está entre 0 y 2 es 
# "IA nula o leve en el hogar", si es entre 3 o 5 
# "IA moderada en el hogar" y si es entre 6 y 8 "IA severa en el hogar"
data$ia <- NA
data$ia[data$sum >= 0 & data$sum <= 2] <- "IA nula o leve en el hogar"
data$ia[data$sum >= 3 & data$sum <= 5] <- "IA moderada en el hogar"
data$ia[data$sum >= 6 & data$sum <= 8] <- "IA severa en el hogar"

# Distribucion de respuestas por categoria
table(data$ia)

# Gráfico de barras de distribucion de respuestas por categoria
ggplot(data, aes(x = ia)) +
  geom_bar(fill = "steelblue") +
  labs(x = "Inseguridad alimentaria", y = "Frecuencia") +
  theme_bw()

# Distribucion de respuestas por categoria en proporciones
prop.table(table(data$ia)) * 100

# Modelo de Rasch con la matriz de valores binarios
res <- RM.w(as.matrix(data[, 2:9]))

# Distribucion de 

# Calculo de la desviacion estandar de los residuos
sd(res$b)

prcomp_result <- prcomp(res$mat.res)

prcomp_result$rotation

prcomp_result$sdev^2

screeplot(prcomp(res$mat.res), type = "lines", main = "Screen plot")

res$infit[which(res$infit > 1.3)]

ad_res <- list()

ad_res$b <- (res$b - mean(res$b)) / sd(res$b) * sd(res$b)
ad_res$se.b <- res$se.b / sd(res$b) * sd(res$b)
ad_res$a <- (res$a - mean(res$b)) / sd(res$b) * sd(res$b)
ad_res$se.a <- res$se.a / sd(res$b) * sd(res$b)

example <- data.frame(
    RawScore = c(0:8),
    Severity = res$a,
    WeightedRelativCases = res$wt.rel.rs
)

ggplot(example, aes(x = Severity, y = WeightedRelativCases)) +
    geom_bar(stat = "identity")

mean(res$infit.person, na.rm = TRUE)
mean(res$outfit.person, na.rm = TRUE)
