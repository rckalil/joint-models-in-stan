library(ggplot2)

set.seed(25072023)

setwd(paste("/home/kalil/Documents/Graduacao/FGV/IC/",
            "joint-models-in-stan", sep = "/"))

# Carregar os dados simulados
load("data/event_data.RData")
cens_time <- 4

# Criar um dataframe para plotagem
event_df <- data.frame(
  times = event_data$times,
  status = as.factor(ifelse(joint_data$times < cens_time, 1, 0))
)

# Plotar o histograma
ggplot(event_df, aes(x = times, fill = status)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Histograma dos Tempos de Sobrevivência", x = "Tempo", y = "Contagem") +
  scale_fill_manual(values = c("red", "blue"), labels = c("Censurado", "Observado")) +
  theme_minimal()
