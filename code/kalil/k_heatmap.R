# Instalar e carregar os pacotes necessários
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("reshape2")) install.packages("reshape2")
if (!require("scales")) install.packages("scales")
library(ggplot2)
library(reshape2)
library(scales)

# Definir o diretório de trabalho (ajuste conforme necessário)
setwd("/home/kalil/Documents/Graduacao/FGV/IC/joint-models-in-stan")

# Ler os dados do arquivo txt
data <- readLines("area_comparisons_2.txt")

# Função para extrair a ordem de grandeza de uma string em notação científica
extract_order_of_magnitude <- function(x) {
  match <- regexpr("e[+-]?[0-9]+", x)
  if (match == -1) {
    scientific_notation <- tryCatch(as.numeric(x), error = function(e) NA)
    if (!is.na(scientific_notation)) {
      order_of_magnitude <- as.numeric(substr(x, match + 1, nchar(x)))
      return(order_of_magnitude)
    }
    return(0)
  }
  order_of_magnitude <- as.numeric(substr(x, match + 1, nchar(x)))
  return(order_of_magnitude)
}

# Processar os dados
data_list <- lapply(data, function(line) {
  if (nchar(line) > 0) {
    parts <- strsplit(line, " ")[[1]]
    params <- strsplit(parts[1], "_")[[1]]
    alfa <- as.numeric(params[2])
    beta <- as.numeric(params[4])
    gamma <- as.numeric(params[6])
    
    # Padronizar a diferença para notação científica
    difference_str <- parts[3]
    difference <- as.numeric(gsub(",", "", difference_str))
    order_of_magnitude <- extract_order_of_magnitude(difference_str)
    if (order_of_magnitude < 0) difference <- 0
    difference <- order_of_magnitude
    if (difference < 0) difference <- 0
    
    return(c(alfa, beta, gamma, difference))
  }
})
#print(data_list)
# Transformar a lista de dados em uma matriz e depois em um data frame
data_matrix <- do.call(rbind, data_list)
data_df <- data.frame(alfa = data_matrix[,1], beta = data_matrix[,2], gamma = data_matrix[,3], difference = data_matrix[,4])
print(data_df)

# Função para criar um heatmap para cada valor de alfa
plot_heatmap <- function(df, gamma_value) {
  subset_df <- df[df$gamma == gamma_value, ]
  if (nrow(subset_df) == 0) return(NULL)
  
  # Definir os valores únicos de beta e alfa que desejamos na matriz
  unique_beta <- seq(-10, 10, length.out = 10)  # Ajuste conforme necessário
  unique_alfa <- seq(-10, 10, length.out = 10)  # Ajuste conforme necessário
  
  # Criar uma matriz com zeros e definir os nomes das linhas e colunas
  heatmap_matrix <- matrix(0, nrow = length(unique_beta), ncol = length(unique_alfa), 
                           dimnames = list(unique_beta, unique_alfa))
  
  # Preencher a matriz com os valores reais
  for (i in 1:nrow(subset_df)) {
    beta_val <- subset_df$beta[i]
    #print(beta_val)
    #print(as.character(beta_val))
    alfa_val <- subset_df$alfa[i]
    #if (beta_val %in% unique_beta && alfa_val %in% unique_alfa) {
    #print("Ahá: ", paste(i))
    heatmap_matrix[beta_val, alfa_val] <- subset_df$difference[i]
    print(subset_df$difference[i])
    print(paste(i))
    #}
    #print(subset_df$difference[i])
  }
  #print(heatmap_matrix)
  #print(subset_df$difference)
  # Plotar o heatmap
  ggplot(melt(heatmap_matrix), aes(x = Var2, y = Var1, fill = value)) +
    geom_tile() +
    geom_text(aes(label = value), color = "black", size = 3) +  # Add this line
    scale_fill_gradientn(colors = c("white", "blue", "red"), na.value = "grey50", name = "Difference") +
    labs(title = paste("Heatmap for Gamma =", gamma_value), x = "Alpha", y = "Beta") +
    theme_minimal()
}

# Criar heatmaps para cada valor de alfa
gamma_values <- unique(data_df$gamma)
heatmaps <- lapply(gamma_values, function(gamma) plot_heatmap(data_df, gamma))

# Printar os heatmaps
for (heatmap in heatmaps) {
  if (!is.null(heatmap)) print(heatmap)
}

