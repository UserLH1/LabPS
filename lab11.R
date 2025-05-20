#set date:
#facem tabelul de frecventa x:()
#x cu linie deasupra - media aritmetica = 28/12 = 7/3
7/3

#dispersia
#s^2 = suma(i=1;n;(x[i]-media aritmetica)^2)/suma(frecvente[1...n])
((1-2.33)^2*4 + (2-2.33)^2*3+ (3-2.33)^2*2 + + (4-2.33)^2*3)/12

#abaterea standard
s=sqrt(1.38)
s

#modul: nr care apare de cele mai multe ori: 1

#mediana - nr din mijloc - nr par => facem media la x6,x7
me=mean(2,2)
me

#quartile ordin 4 => 4 grupe egale => 3 quartile
#q1=p25, q2=p50=mem, q3=p75
#qn = pk
#det pozitia i pt (n*k)100
#daca (n*k)/100 intreg, unde n nr total val, => i=(n*k)
#daca (n*k)/100 nu e intreg => i ia o valoarea z
#pk se det astfel: daca i intreg, ia valoarea de la acea pozitie
                  #daca i nu e intreg, ia valmedia aritemtica dintre (n*k)/100 si (n*k)/100+1

#q1=p25
#(n*k)/100 
(12*25)/100
#=> i = 3.5 nu apartine z => (x3+x4)/2 = 2/1 =>q1 = 1

#q2=p50
#(n*k)/100 
(12*50)/100
#=> i = 6.5 nu apartine z  => (x6 + x7) / 2 = 4/2 = 2

#q2=p75
#(n*k)/100 
(12*75)/100
#=> i = 9.5 nu apartine z  =>( x9 + x10) / 2 = 7/2 =3.5

#problema 3 - in scris
media=(450*50+500*150+600*350+700*300+800*100+850*50)/1000
media

#intevalul modal
#550 - 560
#mediana
# in intervalul 550 - 650


#ex 1 in R
x=c(2,3,1,4,2,1,3,1,1,2,4,4)
length(x)  
m=mean(x)
#dispersia
var(x)
disp = sum((x-m)^2)/length(x)
# in R se implementeaza estimator nedeplasat de aceea avem solutii diferite
#abaterea 
sd(x)
sqrt(disp)
#valoare modala
t = table(x)
t
#la test scriem in cuvinte: modul este 1 pt ca are cea mai mare freventa

#mediana
median(x)

#quantile de ordin n se impart in n parti egale. Daca avem quantile de ordin n => n-1 qunatile pe care sa le gasim
summary(x)
quantile(x)
#prima quantile
Q1 = quantile(x, probs=c(0.25))  
Q1
Q2 = quantile(x, probs=0.50)
Q2
Q3 = quantile(x, probs=0.75)
Q3

#daca vrem sa schimbam nr quantile
quantile(x, probs = seq(0,1,0.1)) #decile

#ex 2
airquality
names(airquality)
ozon = na.omit(airquality$Ozone)
mean(ozon)
var(ozon)
sd(ozon)
table(ozon)
median(ozon)
#modul: 23
Q1 = quantile(ozon, probs=0.25)  
Q1
Q2 = quantile(ozon, probs=0.50)
Q2
Q3 = quantile(ozon, probs=0.75)
Q3
order(table(ozon))

#ex4
dataset = read.table("C:\\Users\\UserL\\Downloads\\ex1.txt")
data=dataset$V1
data
mean(data)
var(data)
sd(data)
table(data)
median(data)

#ex5
install.packages("moments")s

library(moments)
kurtosis(data)
skewness(test)
 
