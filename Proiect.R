#1. Statistica descriptiva generala: Pentru fiecare dintre cele 3 tari si pt fiecare variabila  (Datorie Publică, Rata Dobânzii, Inflație, M1, PIB/capita)
#Calculeaza indicatorii statistici de baza:  medie, mediană, deviație standard, minim, maxim, coeficient de variație, asimetrie (skewness), aplatizare (kurtosis)
install.packages("readxl")
install.packages("dplyr") # For data manipulation
install.packages("tidyr") # For data reshaping
install.packages("moments") # For skewness and kurtosis

library(readxl)
library(dplyr)
library(tidyr)
library(moments) 

file_path = "./NationalDebt.xlsx"

data_ro=read_xlsx(file_path, sheet=1, col_names = FALSE)
data_hu=read_xlsx(file_path, sheet=2, col_names = FALSE)
data_sw=read_xlsx(file_path, sheet=5, col_names = FALSE)

data_ro
names(data_ro)
 
data_hu
data_sw
months = data_ro[[1]] 
months

years = data_ro[[2]]
years

debt_index = data_ro[[3]]
debt_index

prev_fluctuation = data_ro[[4]] 
prev_fluctuation

#2.Prezintă grafic valorile comparative: Grafice de tip serie de timp, Box plots



#3.Teste de normalitate:Testul Jarque-Bera, Shapiro-Wilk


#4.Tabel rezultate teste si interpretare




#5.Pentru fiecare țară în parte, calculează matricea de corelație Pearson între toate variabilele, rezintă aceste matrici tabelar si Interpretează corelațiile




#6. Model de Regresie: Var. Dep: Datoria Publică, Var. Indep:  Rata Dobânzii, Inflația, M1, PIB/capita
#Datoria_Publica = β₀ + β₁*Rata_Dobanzii + β₂*Inflatie + β₃*M1 + β₄*PIB_capita + ε


#Testul F, Testul t, Histograma reziduurilor, Q-Q plot, Jarque-Bera sau Shapiro-Wilk aplicat pe reziduuri.