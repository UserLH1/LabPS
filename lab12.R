#testarea ipotezelor - incercam sa aflam daca o idee despre un grrup mare este adevarat bazat pe un esnation
#pasi:1. stabileste ce vrei sa testezi: (H0 - ipoteza nula)
#2. stabileste ce s-ar putea intampla: (Ha - ipoteza alternativa) - opusul primei ip
#stabilesti un prag de risc alfa: cat de mult esti dispus sa gresesti - ex 5% sau 1%
#definesti regiunea critica: daca rezutlatul caculcat cade in aceasta regiunea inseamna ca ideea ta e gresita
#avem 3 tipuri de teste: 
#la test la R sa punem comentariu cu cele doua ipoteze, daca nu punem nu primim pct
#daca avem cresterea sau scadere, e ipoteza unilat, altfel bilaterala
#testul z - esantion mare >30, este cunoascuta varianta
#daca nu stim varianta folosim testul t, in general se foloseste pe seturi mici date
#testul chi patrat atunci cnad nu avem date concrete despre esantion
#verificam daca se afla sau nu in interval
#daca p-valoarea e mai mica decat alfa, respinge, altfel acceptam

#formula test z: media aritemtica - medie presupunere / abaterea standard /radical lungime esantion
#ex 1 vs ex 2. crestere = unilateral la dreapta, diferenta = bilateral


#pb1
#datele problemei 1:
#H0 mu este 100
#Ha mu este diferita de 100
#Se aplica testul Z pentru ca se cunoaste abaterea standard a populatiei
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
z_statistic = (medie_esantion - m)/(sigma/sqrt(lungime))
z_statistic
#daca testz e aproape de 0 inseamna ca medicalentul nu are efect

#calculcam p valoarea pentru un test bilateral
#luam testul z si aplicam distributie normala standard si inmultim cu 2 pt ca e test biltaral
val_p = 2 * (1 - pnorm(abs(z_statistic))) #p valoare
val_p #0.36
val_p > alpha #true =< nu respingem ipoteza nula

#valori critice
valoare_critica_1 = qnorm(1 - alpha/2) #bilateral => impartim riscul in 2; qnorm p: prob ca o val sa fie mai mica decat p
valoare_critica_1
valoare_critica_2 = qnorm(alpha/2)
valoare_critica_2
z_statistic<valoare_critica_1 #H0 nu sunt respinge
#nu cade in regiunea critica (in afara intervalului) deci nu respingem ipoteza nula
#raspuns: Nu, pe baza acestor date și a acestui test, nu putem spune că medicamentul provoacă o diferență semnificativă.
z.test(iq_scores, mu=100, sigma.x=15, alternative='two.sided', conf.level=0.95)
z.test #folosim pentru a ne testa, nu e acceptat la test


#pb2
#H0: mu este 100
#Ha: mu este mai mare decat 100
date=c(97, 97, 99, 99, 99, 100, 102, 103, 103, 104, 107, 109, 109, 110, 114, 114, 116, 118, 118, 120)
lungime=length(date)
sigma=15
nivel_incredere=0.9
alpha=1-nivel_incredere
alpha
#avem abaterea standard a populatiei de 15 deci folosim testul z
medie_esantion=mean(date)
medie_esantion
z_statistic = (medie_esantion - m)/(sigma/sqrt(lungime))
z_statistic

#p-valoarea
p_value= 2 * (1 - pnorm(abs(z_statistic))) #p valoare
val_p > alpha #true =< nu respingem ipoteza nula


#pb3
m=50
sigma=2
nr_pers=10
greutati=round(rnorm(nr_pers, mean=m, sd=dev))#rnorm: generare 10 numare in jurul mediei 50 cu deviatie max de 2
greutati
df = data.frame(Greutate=greutati)
df
#H0: mu este 47
#Ha: diferit de 47
#Cunoastem abaterea standard a esantionului deci folosim testul t (nu stim abeterea std a populatiei)
med_ip=47
certitudine=0.98
alpha = 1-certitudine
alpha
medie_esantion = mean(greutati)
medie_esantion
lungime=length(greutati)
t_statistic = (medie_esantion - med_ip)/(sigma/sqrt(lungime))
t_statistic

t_critic=qt(alpha/2, df=length(greutati)-1,lower.tail = F)
t_critic
#concluzie
t_statistic<t_critic
#H0 se respinge deci greutatea e difrita de 47

#p-valoarea
p_val=pt(t_statistic, df=9, lower.tail = F)
p_val
p_val<alpha #inca un argument sa respingem ipoteza nula

#verificare:
test_t_rezultat = t.test(df$Greutate, mu=med_ip, alternative = "two.sided", conf.level = certitudine)
test_t_rezultat
format(test_t_rezultat$p.value, digits=4)
#deoarece p-valoarea obtinuta prin testul t este mai mica decat apha, respingem ipoteza nula
##inseamna ca exista suficiente dovezi pentru a demonstra ca greutateae medie a pop. nu este 47

    
#pb4
#nu avem medie, doar variatna sigma^2
#H0: sigma^2 este 0.00003969
#Ha: sigma^2 < 0.00003969
#se aplica testul chi patrat pt ca cunoastem variantele
date_chi=read.table("C:/Users/UserL/Downloads/date_chi.txt", header=TRUE)
date_chi
names(date_chi)
diametre=date_chi$DIAMETER

sigmna1=0.00003969 #varianta popultaiei
alpha=0.05
nr_mas=length(diametre)
nr_mas
sigma2=var(diametre) #varianta esantion
var_esant

grade_liberate = nr_mas - 1
grade_liberate
#statistica:
statistica_chi_patrat = (grade_liberate * sigma2) / sigmna1

chi_critic=qchisq(alpha, df=grade_liberate)
chi_critic
#concluzie
statistica_chi_patrat<chi_critic
#FALSE deci se respinge H0, deci varianta scade

p_valoare_var = pchisq(statistica_chi_patrat, df = nr_mas, lower.tail = TRUE)
p_valoare_var

