#Brakujące obserwacje - liczby z konkretnych kolumn

czynniki <- read.csv("czynniki.csv")
View(czynniki)

install.packages("naniar")
library(naniar)


#Kolumna Sleep_Hours
n_miss(czynniki$Sleep_Hours)
#w kolumnie Sleep Hours występuje 350 braków danych

#Kolumna Family Income 
n_miss(czynniki$Family_Income)
#w kolumnie Family Income występuje 350 braków danych

#Kolumna Teacher Quality
n_miss(czynniki$Teacher_Quality)
#w kolumnie Teacher Quality występuje 78 braków danych

#Kolumna Parental Education Level
n_miss(czynniki$Parental_Education_Level)
#w kolumnie Parental Education Level występuje 90 braków danych

#Kolumna Distance from Home
n_miss(czynniki$Distance_from_Home) 
#w kolumnie Distance from Home występuje 67 braków danych

#Kolumna Exam Score
n_miss(czynniki$Exam_Score)
#w kolumnie Exam Score występuje 300 braków danych 

vis_miss
View(vis_miss)

install.packages("mice")
install.packages("devtools")
devtools::install_github(repo = "amices/mice")


gg_miss_upset(czynniki)

#Kombinacje braków danych wg wierszy 
library(mice)
install.packages("Amelia")
library(Amelia) 
missmap(czynniki)
md.pattern(czynniki)
