# Cargar datos desde un archivo CSV
datos <- read.csv("train.csv", header = TRUE, encoding = "UTF-8")

View(datos)

summary(datos)

# Preprocesamiento de datos para clustering y análisis de grupos.

datos_para_clustering <- datos[, c("MSSubClass", "LotFrontage", "LotArea", "OverallQual", "OverallCond", "YearBuilt",
                                   "YearRemodAdd", "TotalBsmtSF", "BsmtFullBath", "BsmtHalfBath", "FullBath", "HalfBath",
                                   "KitchenQual", "TotRmsAbvGrd", "Fireplaces", "GarageYrBlt", "GarageCars", "GarageArea",
                                   "PoolArea", "MiscVal", "MoSold", "YrSold", "SalePrice")]

datos_para_clustering <- na.omit(datos_para_clustering) # Eliminar filas con valores faltantes

