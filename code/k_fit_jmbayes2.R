# Carregar pacotes necessários
library(JMbayes2)
library(nlme)
library(survival)

set.seed(25072023)

# Carregar os dados simulados
load("data/joint_data.RData")

# Organizando os dados longitudinais
long_data <- data.frame(id = joint_data$id, 
                        time = joint_data$obs_times, 
                        y = joint_data$y, 
                        x = joint_data$x[joint_data$id])

# Organizando os dados de sobrevivência
status <- rep(0, length(joint_data$times))
status[joint_data$ind_unc_times] <- 1
surv_data <- data.frame(id = 1:joint_data$N, 
                        time = joint_data$times, 
                        status = status, 
                        x = joint_data$x)

# Ajustar o modelo longitudinal usando lme() do nlme
lmeFit <- lme(fixed = y ~ time + x, 
              data = long_data, 
              random = ~ time | id)

# Ajustar o modelo de sobrevivência (Cox)
coxFit <- coxph(Surv(time, status) ~ x, data = surv_data, x = TRUE)

# Ajustar o modelo conjunto usando JMbayes2
jointFit <- jm(coxFit, lmeFit, time_var = "time", n_iter = 10000, cores=1)

# Resumo dos resultados
summary(jointFit)

# Verificar os dados para os ids 120, 134, 241
ids_to_check <- c(1, 2, 120, 134, 241, 243, 244)

# Dados longitudinais para os ids selecionados
long_data_subset <- subset(joint_data, id %in% ids_to_check)
print("Dados longitudinais para os ids 120, 134, 241:")
print(long_data_subset)

# Dados de sobrevivência para os ids selecionados
surv_data_subset <- subset(joint_data, id %in% ids_to_check)
print("Dados de sobrevivência para os ids 120, 134, 241:")
print(surv_data_subset)
