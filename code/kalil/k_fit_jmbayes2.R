# Carregar pacotes necessários
library(JMbayes2)
library(nlme)
library(survival)
library(coxme)

set.seed(25072023)
setwd(paste("/home/kalil/Documents/", "joint-models-in-stan", sep="/"))

# Carregar os dados simulados
load("data/joint_data.RData")

# Organizar dados longitudinais
long_data <- data.frame(id = joint_data$id,
                        time = joint_data$obs_times,
                        y = joint_data$y,
                        x = joint_data$x[joint_data$id])

# Organizar dados de sobrevivência
status <- rep(0, length(joint_data$times))
status[joint_data$ind_unc_times] <- 1
surv_data <- data.frame(id = 1:joint_data$N, 
                        time = joint_data$times, 
                        status = status, 
                        x = joint_data$x)

# Ajuste do modelo longitudinal usando lme() do nlme
lmeFit <- lme(fixed = y ~ time + x, 
              data = long_data, 
              random = ~ time | id)

# Ajuste do modelo de sobrevivência (Cox)
coxFit <- coxph(Surv(time, status) ~ x, data = surv_data, x = TRUE)

# Ajuste do modelo conjunto usando JMbayes2
#jointFit <- jm(coxFit, lmeFit, time_var = "time", n_iter = 10000, cores = 1)
jointFit <- jm(
  coxFit, 
  lmeFit, 
  time_var = "time", 
  assoc = list(
    value = ~ u1 + u2 * time,   # Incluir intercepto e inclinação no risco
    slope = ~ u2,               # Incluir o efeito da inclinação
    interaction = ~ u1 + u2     # Incluir interação
  ),
  assoc_type = "AR",
  n_iter = 10000,
  cores = 1
)

# Extração dos parâmetros estimados
joint_summary <- summary(jointFit)
long_params <- fixef(jointFit)       # Parâmetros longitudinais beta_1
surv_params <- coef(jointFit)        # Parâmetro beta_21 e gamma
ranef_params <- ranef(jointFit)      # Efeitos aleatórios u_1, u_2 e variância associada

# Exibir estimativas dos parâmetros
print("Resumo do Modelo Conjunto:")
print(joint_summary)

print("Parâmetros Longitudinais:")
print(long_params)

print("Parâmetros do Modelo de Sobrevivência:")
print(surv_params)

print("Efeitos Aleatórios:")
print(ranef_params)
# Calcular a média de cada coluna (u_1 e u_2)
mean_u <- colMeans(ranef_params)
# Calcular o desvio padrão de cada coluna (u_1 e u_2)
sd_u <- apply(ranef_params, 2, sd)
# Exibir resultados
print("Média dos efeitos aleatórios:")
print(mean_u)
print("Desvio padrão dos efeitos aleatórios:")
print(sd_u)

# Obter variâncias e correlações estimadas
var_estimates <- joint_summary$sigma    # Variância de u_1 e u_2 (var_u)
print("Variância e Correlação dos Efeitos Aleatórios:")
print(var_estimates)

traceplot(jointFit)


