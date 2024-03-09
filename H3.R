
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

#
# División de grupos train y test para modelos de regresión lineal
#

porcentaje <- 0.52 #Porcentaje con el que se calcularán los grupos de train y test.
datos_cuantitativos<- datos_para_clustering
corte <- sample(nrow(datos_cuantitativos),nrow(datos_cuantitativos)*porcentaje)
train<-datos_cuantitativos[corte,] #Corte para el grupo entrenamiento
test<-datos_cuantitativos[-corte,] #Corte para el grupo prueba

head(train)
head(test)

#Creación del modelo de regresión lineal.
single_linear_model<- lm(SalePrice~OverallQual, data = train) #Modelo lineal singular para SalePrice y OverallQual
summary(single_linear_model)

#Análisis de residuos

head(single_linear_model$residuals)

boxplot(single_linear_model$residuals)

# Análisis de predicción en conjunto prueba.

predSLM<-predict(single_linear_model, newdata = test)
head(predSLM)
length(predSLM)

# Gráfico del Modelo de Regresión Lineal Simple

library(ggplot2)
ggplot(data = train, mapping = aes(x = OverallQual, y = SalePrice)) +
  geom_point(color = "lightgreen", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Calidad Promedio del Material x Precio de Venta", x = "Calidad Promedio", y = "Precio de Venta") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))


#Modelo lineal múltiple para SalePrice.
multiple_linear_model<-lm(SalePrice~.,data = train)

summary(multiple_linear_model)


# Analisis de la correlacción

var_independients <- train[, -which(names(train) == "SalePrice")]

correlation_matrix <- cor(var_independients)

print(correlation_matrix)







#HOJA DE TRABAJO 4

# Cargar datos desde un archivo CSV
datos <- read.csv("train.csv", header = TRUE, encoding = "UTF-8")
datos <- datos[, -1]

View(datos)

porcentaje4 <- 0.70
trainRowsNumber<-sample(1:nrow(datos),porcentaje4*nrow(datos))
train<-datos[trainRowsNumber,]
test<-datos[-trainRowsNumber,]

data_tree <- datos
summary(data_tree)
library(rpart)

#Instalar paquetes con rplot
#install.packages("rpart.plot")
library(rpart.plot)

#Crear nuestro modelo_arbol
modelo_arbol <- rpart(SalePrice~.,data=data_tree)

summary(modelo_arbol)

#Mostrar el arbol con la data de SalePrice
rpart.plot(modelo_arbol, digits = 3, fallen.leaves = TRUE)

#Predicción y análisis del resultado.

modelo_train <- rpart(SalePrice~.,data=train)
rpart.plot(modelo_train, digits = 3, fallen.leaves = TRUE)

prediccion <- predict(modelo_arbol, newdata = test)

head(prediccion)

mse <- mean((test$SalePrice - prediccion)^2)
print(paste("Error Cuadrático Medio (MSE):", mse))

r_cuadrado <- 1 - mse / var(test$SalePrice)
print(paste("Coeficiente de Determinación (R^2):", r_cuadrado))

#Añadir 3 modelos más:

# Crear una lista de modelos con diferentes profundidades
modelos <- list()
for (depth in 1:3) {
  modelos[[depth]] <- rpart(SalePrice~., data = data_tree, control = rpart.control(maxdepth = depth))
}
# Evaluar los modelos
for (i in 1:length(modelos)) {
  predicciones <- predict(modelos[[i]], newdata = test)
  mse <- mean((test$SalePrice - predicciones)^2)
  correlacion <- cor(test$SalePrice, predicciones)
  print(paste("Profundidad:", i, "MSE:", mse, "Coeficiente de correlación:", correlacion))
}

#Inciso 6. Clasificar las casas en Económicas, Intermedias y Caras.

# Definir cuartiles
cuartiles <- quantile(datos$SalePrice, probs = c(0.25, 0.5, 0.75))

# Crear variable respuesta
datos$Clasificacion <- cut(datos$SalePrice, breaks = c(0, cuartiles[2], cuartiles[3], max(datos$SalePrice)), labels = c("Económicas", "Intermedias", "Caras"))

View(datos)

# Contar el número de casas en cada categoría
num_casas <- table(datos$Clasificacion)
print(num_casas)

#Añadir Clasificacion a los conjuntos train y test.

trainRowsNumber<-sample(1:nrow(datos),porcentaje4*nrow(datos))
train<-datos[trainRowsNumber,]
test<-datos[-trainRowsNumber,]

# Inciso 7 - Creación del árbol

# Crear modelo de árbol de clasificación

modelo_arbol_clasificacion <- rpart(Clasificacion ~ . - SalePrice, data = datos, method = "class")
# Visualizar el árbol
rpart.plot(modelo_arbol_clasificacion, digits = 3, fallen.leaves = TRUE)

# Inciso 8.
# Predecir con el conjunto de prueba
predicciones_clasificacion <- predict(modelo_arbol_clasificacion, newdata = test, type = "class")

head(predicciones_clasificacion)

# Calcular la precisión
precision <- sum(predicciones_clasificacion == test$Clasificacion) / length(test$Clasificacion)
print(paste("Precisión del árbol de clasificación:", precision))

