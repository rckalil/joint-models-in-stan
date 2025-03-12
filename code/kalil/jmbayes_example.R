library("JMbayes")
library("lattice")
pbc2$status2 <- as.numeric(pbc2$status != "alive")
pbc2.id$status2 <- as.numeric(pbc2.id$status != "alive")

sfit <- survfit(Surv(years, status2) ~ drug, data = pbc2.id)
plot(sfit, lty = 1:2, lwd = 2, col = 1:2, mark.time = FALSE, 
     xlab = "Time (years)", ylab = "Transplantation-free Survival")
legend("topright", levels(pbc2.id$drug), lty = 1:2, col = 1:2, lwd = 2, cex = 1.3, bty = "n")

pbc2$status2f <- factor(pbc2$status2, levels = 0:1, 
                        labels = c("alive", "transplanted/dead"))
xyplot(log(serBilir) ~ year | status2f, group = id, data = pbc2, 
       panel = function(x, y, ...) {
         panel.xyplot(x, y, type = "l", col = 1, ...)
         panel.loess(x, y, col = 2, lwd = 2)
       }, xlab = "Time (years)", ylab = "log(serum Bilirubin)")

library('splines')

lmeFit.pbc1 <- lme(log(serBilir) ~ ns(year, 2), data = pbc2, 
                   random = ~ ns(year, 2) | id)
coxFit.pbc1 <- coxph(Surv(years, status2) ~ drug * age, data = pbc2.id,
                     x = TRUE)
jointFit.pbc1 <- jointModelBayes(lmeFit.pbc1, coxFit.pbc1, 
                                 timeVar = "year", n.iter = 30000)
summary(jointFit.pbc1, include.baseHazCoefs = TRUE)
