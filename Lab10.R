#ex1
note=c(10, 7, 8, 9, 4, 5, 9, 7, 8, 6, 9, 10, 6, 7, 8, 7, 6, 7, 8, 9, 4, 4, 6, 8, 9, 7, 9, 10, 7, 6)
note
sort(note)
len=length(note)

#frecventa abs
frec_abs=table(note)
frec_abs

#frecventa rel
frec_rel=frec_abs/len
format(frec_rel,digits=1)

#frec_cum_abs
frec_cum_abs=cumsum(frec_abs)
frec_cum_abs

#frec_cum_rel
frec_cum_rel=frec_cum_abs/len
format(frec_cum_rel, digits=2)

#frec_proc
frec_proc=frec_abs/sum(frec_abs)
frec_proc*100
frec_proc

#diagrama cerc
library(plotrix)
pie(frec_abs)
label=c('nota 4','nota 5','nota 6','nota 7','nota 8','nota 9','nota 10')
pie(frec_abs,labels=label, clockwise = TRUE)
#3d
pie3D(frec_abs,labels=label,radius=2, explode=0.2)

#histograma
hist(note,breaks=2)
hist(note,col='red',border='black',main="Histograma note",xlab="Note studenti", ylab="Frecvente note")

#gruparea datelor in clase 
min(note)
max(note)
nrclase=5
#se stabilesc marginile pentru clase
margini=seq(min(note),max(note),by=(max(note)-min(note))/nrclase)
margini
#impartirea in clase
cut(note,margini,include.lowest = TRUE)
cut(note,margini,include.lowest = TRUE,right = FALSE)
#afisearea claselor
table(cut(note,margini,include.lowest = TRUE))
#histograma
hist(table(cut(note,margini,include.lowest = TRUE)))
#
     
#ex2
cars
#info
help(cars)
#vizualizare date
names(cars)
#tip de date
typeof(cars)
#accesarea unei variabile din set: setdate$denumirevariabile
cars$speed[20]

#faithful
faithful
help(faithful)
names(faithful)
typeof(faithful)

#b
hist(cars$speed,col='red',border='black',main="Histograma Histograma vitezelor",xlab="Viteza", ylab="Distanta")

frec_abs=table(cars$speed)
frec_abs

pie(cars$speed[1:10], clockwise = TRUE)

frec_rel=frec_abs/len
frec_rel
hist(frec_rel,col='red',border='black',main="Histograma Histograma vitezelor relative",xlab="Viteza", ylab="Distanta")

#c
faithful$eruptions
frec_rel=table(faithful$eruptions)
frec_abs=frec_rel/len
frec_abs
#frec_cum_abs
frec_cum_abs=cumsum(frec_abs)
frec_cum_abs

#frec_cum_rel
frec_cum_rel=frec_cum_abs/len
format(frec_cum_rel, digits=2)

min(faithful$eruptions)
max(faithful$eruptions)
nrclase=5
#se stabilesc marginile pentru clase
margini=seq(min(faithful$eruptions
),max(faithful$eruptions
),by=(max(faithful$eruptions
)-min(faithful$eruptions
))/nrclase)
margini
#impartirea in clase
cut(faithful$eruptions
,margini,include.lowest = TRUE)
cut(faithful$eruptions
,margini,include.lowest = TRUE,right = FALSE)
#afisearea claselor
table(cut(faithful$eruptions,margini,include.lowest = TRUE))

#ex3
dataset = read.table("C:\\Users\\UserL\\Downloads\\note.txt")
dataset
names(dataset)
frec_abs=table(dataset$V1)
frec_abs
len=length(dataset$V1)
len
frec_rel=frec_abs/len
frec_rel
min(dataset$V1)
max(dataset$V1)
nrclase=6
margini=seq(min(dataset$V1),max(dataset$V1),by=(max(dataset$V1)-min(dataset))/nrclase)
margini
#impartirea in clase
cut(dataset$V1,margini,include.lowest = TRUE)
cut(dataset$V1,margini,include.lowest = TRUE,right = FALSE)
#afisearea claselor
table(cut(dataset$V1,margini,include.lowest = TRUE))
#histograma
hist(table(cut(dataset$V1,margini,include.lowest = TRUE)))


dataset2 = read.csv("C:\\Users\\UserL\\Downloads\\note.csv")
dataset2
n1=dataset2$n1
n1
names(dataset)
frec_abs=table(n1)
frec_abs
len=length(n1)
len
frec_rel=frec_abs/len
frec_rel
min(n1)
max(n1)
nrclase=6
margini=seq(min(n1),max(n1),by=(max(n1)-min(n1))/nrclase)
margini
#impartirea in clase
cut(n1,margini,include.lowest = TRUE)
cut(n1,margini,include.lowest = TRUE,right = FALSE)
#afisearea claselor
table(cut(n1,margini,include.lowest = TRUE))
#histograma
hist(table(cut(n1,margini,include.lowest = TRUE)))

library(readxl)
dataset3 = read_excel("C:\\Users\\UserL\\Downloads\\note.xlsx")
dataset3
names(dataset3)
note=dataset3$Note
note
frec_abs=table(note)
frec_abs
len=length(note)
len
frec_rel=frec_abs/len
frec_rel
min(note)
max(note)
nrclase=6
margini=seq(min(note),max(note),by=(max(note)-min(note))/nrclase)
margini
#impartirea in clase
cut(note,margini,include.lowest = TRUE)
cut(note,margini,include.lowest = TRUE,right = FALSE)
#afisearea claselor
table(cut(note,margini,include.lowest = TRUE))
#histograma
hist(table(cut(note,margini,include.lowest = TRUE)))

#4
set.seed(123); n = 10000
# Distributia binomiala B(20, 0.3)
binom = rbinom(n, size = 20, prob = 0.3); 
hist(binom, prob=TRUE, col='blue', border='black', main="Binomiala B(20,0.3)", xlab="k", ylab="Frecventa relativa")

# Distributia Poisson lambda = 4
pois = rpois(n, lambda = 4); 
hist(pois, prob=TRUE, col='green', border='black', main="Poisson lambda=4", xlab="k", ylab="Frecventa relativa")

# Distributia geometrica p = 0.2 (nr. incercari pana la primul succes)
geom = rgeom(n, prob = 0.2) + 1; 
hist(geom, prob=TRUE, col='pink', border='black', main="Geometrica p=0.2", xlab="k", ylab="Frecventa relativa")

# Distributia uniforma U(0,1)
unif = runif(n, min = 0, max = 1); 
hist(unif, prob=TRUE, col='orange', border='black', main="Uniforma U(0,1)", xlab="x", ylab="Densitate")

# Distributia normala standard N(0,1)
norm = rnorm(n, mean = 0, sd = 1); 
hist(norm, prob=TRUE, col='violet', border='black', main="Normala N(0,1)", xlab="x", ylab="Densitate")

# Distributia exponentiala Exp(rate=1)
exp = rexp(n, rate = 1); 
hist(exp, prob=TRUE, col='red', border='black', main="Exponentiala Exp(1)", xlab="x", ylab="Densitate")




