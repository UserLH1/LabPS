#testarea ipotezelor - incercam sa aflam daca o idee despre un grrup mare este adevarat bazat pe un esnation
#pasi:1. stabileste ce vrei sa testezi: (H0 - ipoteza nula)
#2. stabileste ce s-ar putea intampla: (Ha - ipoteza alternativa) - opusul primei ip
#stabilesti un prag de risc alfa: cat de mult esti dispus sa gresesti - ex 5% sau 1%
#definesti regiunea critica: daca rezutlatul caculcat cade in aceasta regiunea inseamna ca ideea ta e gresita
#avem 3 tipuri de teste: 



#pb1
#datele problemei 1:
iq_scores = c(88, 92, 94, 94, 96, 97, 97, 97, 99, 99, 105, 109, 109, 109, 110, 112, 112, 113, 114, 115)
iq_scores
m=100 #media
sigma=15 #abaterea standard
lungime = length(iq_scores)
lungime
#nivelul de incerdere: 95%: 1-0.95 = 0.05
alpha=0.05
medie_esantion=mean(iq_scores)
medie_esantion

#testul z: il folosim pt ca stim sigma. daca nu am sti, am folosi testul t. ne spune cat de departe este media esantionului
#nostru fata de media populatiei
#calcuclam diferenta intre media obitnuta si cea data si o impartim la raportul dintre sigma si radical din lungime
testz = (medie_esantion - m)/(sigma/sqrt(lungime))
testz
#daca testz e aproape de 0 inseamna ca medicalentul nu are efect

#calculcam p valoarea pentru un test bilateral
#luam testul z si aplicam distributie normala standard si inmultim cu 2 pt ca e test biltaral
val_p = 2 * (1 - pnorm(abs(testz)))
val_p #0.36
val_p > alpha #true =< nu respingem ipoteza nula

#valori critice
valoare_critica_1 = qnorm(1 - alpha/2) #bilateral => impartim riscul in 2; qnorm p: prob ca o val sa fie mai mica decat p
valoare_critica_1
valoare_critica_2 = qnorm(alpha/2)
valoare_critica_2
testz #nu cade in regiunea critica (in afara intervalului) deci nu respingem ipoteza nula
#raspuns: Nu, pe baza acestor date și a acestui test, nu putem spune că medicamentul provoacă o diferență semnificativă.



#pb3
set.seed(123)
m=50
dev=2
nr_pers=10
greutati=round(rnorm(nr_pers, mean=m, sd=dev))#rnorm: generare 10 numare in jurul mediei 50 cu deviatie max de 2
greutati
df = data.frame(Greutate=greutati)
df

med_ip=47
certitudine=0.95
alpha = 1-certitudine
alpha
med_t = mean(greutati)
test_t_rezultat = t.test(df$Greutate, mu=med_ip, alternative = "two.sided", conf.level = certitudine)
test_t_rezultat
format(test_t_rezultat$p.value, digits=4)
#deoarece p-valoarea obtinuta prin testul t este mai mica decat apha, respingem ipoteza nula
##inseamna ca exista suficiente dovezi pentru a demonstra ca greutateae medie a pop. nu este 47


#pb4
date_chi=read.table("C:/Users/UserL/Downloads/date_chi.txt", header=TRUE)
date_chi
names(date_chi)
diametre=date_chi$DIAMETER

var_diam=0.00003969
alpha=0.05
nr_mas=length(diametre)
nr_mas
var_esant=var(diametre)
var_esant
