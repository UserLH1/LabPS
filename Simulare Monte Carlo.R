test = read.delim("C:/Users/UserL/Downloads/rujeola.txt", header=TRUE)

names(test)

model1 = lm(Incidenta ~ Zile, data = test)
summary(model1)

plot(test$Zile, test$Incidenta,
     main = "Trendul Incidentei Rujeolei in UE",
     xlab = "Zile", ylab = "Incidenta",
     pch  = 20, col = "blue")
abline(model1, col = "red")

test$d1 = log(1.2 + sin((test$Zile + 110) * pi / 112))

model2 = lm(Incidenta ~ Zile + d1, data = test)
summary(model2)

news    = seq(0, max(test$Zile), by = 10)
pred_nl = predict(model2,
                  newdata = data.frame(Zile = news,
                                       d1   = log(1.2 + sin((news + 110) * pi / 112))))
plot(test$Zile, test$Incidenta,
     main = "Regresie neliniara cu ciclicitate",
     xlab = "Zile", ylab = "Incidenta",
     pch  = 20, col = "blue")
abline(model1, col = "red")
lines(news, pred_nl, col = "green", lwd = 2)

# 7. Simulare Monte Carlo
set.seed(123)
n        = nrow(test)
sd_obs   = sd(test$Incidenta)
sim_count= 100
mc       = matrix(NA, nrow = sim_count, ncol = n)

for(i in 1:sim_count) {
  eps      = rnorm(n, mean = 0, sd = sd_obs)
  mc[i, ]  = predict(model2, newdata = test) + eps
}

# 8. Media simulărilor
sMC = colMeans(mc)

# 9. Plot media simulărilor vs. trend liniar
plot(test$Zile, sMC,
     main = "Simulare MC: media predictiilor",
     xlab = "Zile", ylab = "Incidenta simulata",
     pch  = 17, col = "blue")
lines(test$Zile, sMC, col = "red", lwd = 2)
abline(model1, col = "yellow", lwd = 2)

# 10. Interval de încredere 95% pe fiecare zi
getCI    = function(v) t.test(v)$conf.int
conf_int = apply(mc, 2, getCI)

plot(range(conf_int), c(1,n), type="n",
     xlab="Incidenta", ylab="Ziua")
for(j in 1:n) {
  lines(conf_int[, j], rep(j,2), lwd=1, col="grey")
}
points(sMC, 1:n, pch=16, col="blue")

