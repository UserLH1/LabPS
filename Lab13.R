#Lab 13 - Corelatia Liniara
#calculam in scris suma de x patrat, (suma de x) totul la patrat si suma de y patrat si 
#(suma de y) totul la patrat si aplicam formula din lab

x = c(2, 3, 1, 4, 2, 1, 3, 1, 1, 2, 4, 4)
y = c(3, 4, 2, 7, 2, 2, 4, 2, 2, 3, 6, 5)
z = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 4)

#corelatia intre x si y
cor_xy = cor(x, y)
cor_xy
#0.93 >0.75 inseamna ca exista corelatie pozitiva intre x si y
plot(x, y, main = "Norul de puncte între x și y",
     xlab = "Variabila x", ylab = "Variabila y", col = "blue")
abline(lm(y ~ x), col = "red")
  
#corelatia intre x si z
cor_xz = cor(x, z)
cor_xz
#0.16 < 0.25 nu exista corelatie intre cele doua

plot(x, z, main = "Norul de puncte între x și z",
     xlab = "Variabila x", ylab = "Variabila z", pch = 19, col = "green")
# Adăugarea unei linii de regresie pentru vizualizare
abline(lm(z ~ x), col = "purple")

cor_yz = cor(y, z)
cor_yz
#nu exista corelatie intre y si z

#tesetam coeficientul de corelatie la nivelul populatiei intre x si y

#h0: rho=0 (nu exista corelatie la nivelul populatiei)
#ha: rho>0 (exista corelatie pozitiva la nivelul populatiei)
aplha=0.05
cor.test(x,y,alternative="greater",conf.level = 1-alpha, method = "pearson")#greater pt ca e poziva
#daca era corelatia negativa era lesser
#daca p-valoarea < alpha => exista corelatie la nivelul populatiei
#daca p-valoarea > alpha => nu avem suficiente informatii pt a infirma existenta corelatiei la niv. pop
#la noi p-valoarea = 3.228e-06 < alpha => exista corelatiei la nivelul populatiei

#t statistic/ calculat
t_calculat=cor(x,y)/sqrt(1-cor(x,y)^2)*sqrt(length(x)-2)
t_calculat

#ex2
iris
names(iris)
#norul de puncte
plot(iris$Sepal.Width, iris$Sepal.Length)
plot(iris)
plot(iris[1:4]) #pentru ca speciile nu sunt cantitative
#trebuie sa calculcam pentru toate combinatiile posibile pana obintem una pozitiva sau negativa
#daca gasim una aplicam testul cu coeficient 92%

#pentru lungime si latime sepale
length_width_sep = cor(iris$Sepal.Length, iris$Sepal.Width)
length_width_sep
#-0.11 deci nu exista corelatie
plot(iris$Sepal.Width, iris$Petal.Length)


#pentru lungime si latime petale
length_width = cor(iris$Petal.Length, iris$Petal.Width)
length_width
#0.96 deci exista corelatie pozitiva
plot(iris$Sepal.Width, iris$Petal.Length)
#h0: rho=0 (nu exista corelatie )
#ha: rho>0 (exista corelatie)
alpha=1-0.92
alpha
test_corelatie_iris = cor.test(iris$Sepal.Width, iris$Sepal.Length,
                                alternative = "greater",
                                conf.level = 0.92,
                                method = "pearson")
test_corelatie_iris
#p valoare mai mare decat alpha deci nu avem suficiente date