
# Cargar datos desde un archivo CSV
datos <- read.csv("train.csv", header = TRUE, encoding = "UTF-8")

View(datos$MiscVal)

summary(datos)

View(datos)

datos_para_clustering <- datos[, c("LotFrontage", "LotArea", "OverallQual", "OverallCond", "YearBuilt",
                                   "YearRemodAdd", "TotalBsmtSF", "BsmtFullBath", "BsmtHalfBath", "FullBath", "HalfBath",
                                    "TotRmsAbvGrd", "Fireplaces", "GarageYrBlt", "GarageCars", "GarageArea",
                                   "PoolArea", "MiscVal", "MoSold", "YrSold", "SalePrice")]

datos_para_clustering <- na.omit(datos_para_clustering) 

normalized_data <- scale(datos_para_clustering)

View(normalized_data)

#Obtener cantidad de clusteres
set.seed(123)
k_values <- 1:10
iner <- numeric(length(k_values))

for (k in k_values) {
  model <- kmeans(normalized_data, centers = k)
  iner[k] <- model$tot.withinss
}

plot(k_values, iner, type = "b", main = "Método del Codo", xlab = "Número de Clústeres (k)", ylab = "Inercia")
abline(v = which.min(diff(iner) > 10) + 1, col = "red", lty = 2)

#Al obtener la cantidad de clusters, realizamos K-Means para encontrar los grupos.

set.seed(123)
num_clusters <- 2  # Número de clústeres determinado anteriormente

# Aplicar el algoritmo de k-means
kmeans_model <- kmeans(normalized_data, centers = num_clusters)

# Añadir las etiquetas de clúster al conjunto de datos
normalized_data$kmeans_cluster <- as.factor(kmeans_model$cluster)

# Visualizar el resultado del clustering
table(normalized_data$kmeans_cluster)
