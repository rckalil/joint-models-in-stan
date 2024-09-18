library(JMbayes)
library(nlme)
library(survival)

set.seed(25072023)

setwd(paste("/home/kalil/Documents/Graduacao/FGV/IC/",
            "joint-models-in-stan", sep = "/"))

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

# Ajustar o modelo longitudinal
lmeFit <- lme(fixed = y ~ time + x, 
              data = long_data, 
              random = ~ time | id)

# Ajustar o modelo de sobrevivência
coxFit <- coxph(Surv(time, status) ~ x, data = surv_data, x = TRUE)

# Ajustar o modelo conjunto
jointFit <- jointModelBayes(lmeFit, coxFit, timeVar = "time", n.iter = 10000)

# Resumo dos resultados
summary(jointFit, include.baseHazCoefs = TRUE)
