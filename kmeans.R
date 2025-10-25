library(readxl)
library(fastDummies)

install.packages("arules")
library(arules)

install.packages("ggplot2")
library(ggplot2)

info<- read.csv("C:/Users/rodri/OneDrive/Documentos/Maestria/Cuarto_trimestre/Mineria de datos/kmeans/violencia_intrafamiliar.csv")
summary(info)

info_k<- info[, c("HEC_MES", "QUIEN_REPORTA", "INST_DENUN_HECHO",  "CONDUCENTE", "VIC_EDAD", "HEC_TIPAGRE", "HEC_RECUR_DENUN", "AGR_SEXO", "VIC_EST_CIV", "VIC_GRUPET",  "AGR_EDAD")]
info_k[is.na(info_k)]<- -1

cat_vars <- c("HEC_MES", "QUIEN_REPORTA", "INST_DENUN_HECHO",  "CONDUCENTE", "HEC_TIPAGRE", "HEC_RECUR_DENUN", "AGR_SEXO", "VIC_EST_CIV", "VIC_GRUPET")

info_k_dummy <- dummy_cols(
  info_k,
  select_columns = cat_vars,
  remove_first_dummy = TRUE,   
  remove_selected_columns = TRUE  
)

names(info_k_dummy) <- gsub("__", "_", names(info_k_dummy))
names(info_k_dummy) <- gsub("_([A-Za-z0-9]+)$", ".\\1", names(info_k_dummy))
names(info_k_dummy) <- gsub("_", ".", names(info_k_dummy))

info_scaled <- scale(info_k_dummy)

cluster<- kmeans(info_scaled, centers = 4)

pca <- prcomp(info_scaled, scale. = FALSE)

######################

loadings <- pca$rotation[, 1:2] 
top_pc1 <- names(sort(abs(loadings[,1]), decreasing = TRUE))[1:3]
top_pc2 <- names(sort(abs(loadings[,2]), decreasing = TRUE))[1:3]

x_lab <- paste0("PC1 (", paste(top_pc1, collapse = ", "), ")")
y_lab <- paste0("PC2 (", paste(top_pc2, collapse = ", "), ")")

pca_data <- data.frame(
  PC1 = pca$x[,1],
  PC2 = pca$x[,2],
  cluster = as.factor(cluster$cluster)
)

centroids_pca <- as.data.frame(
  predict(pca, newdata = cluster$centers)[,1:2]
)
centroids_pca$cluster <- factor(1:nrow(centroids_pca))

ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_point(data = centroids_pca, aes(x = PC1, y = PC2),
             color = "black", shape = 8, size = 4) +
  geom_text(data = centroids_pca, aes(label = paste("Cluster", cluster)),
            color = "black", vjust = -1) +
  labs(
    title = "K-means visualizado con PCA",
    x = x_lab,
    y = y_lab
  ) +
  theme_minimal()


