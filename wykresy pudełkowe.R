czynniki <- read.csv("czynniki.csv")
View(czynniki)
install.packages("ggplot2")
library("ggplot2")


my_theme <- theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 11))

#Wykres pudełkowy godzin nauki

ggplot(czynniki, aes(y = Hours_Studied)) +
  geom_boxplot() +
  labs(title = "Wykres pudełkowy godzin nauki",
       y = "Godziny nauki") +
  my_theme

#Wykres pudełkowy wyników z egzaminu

ggplot(czynniki, aes(y = Exam_Score)) +
  geom_boxplot() +
  labs(title = "Wykres pudełkowy wyników z egzaminu",
       y = "Wynik z egzaminu [%]") +
  my_theme

#Wykres pudełkowy obecności

ggplot(czynniki, aes(y = Attendance)) +
  geom_boxplot() +
  labs(title = "Wykres pudełkowy obecności",
       y = "Obecność [%]") +
  my_theme

#Wykres pudełkowy średniej liczby godzin snu w ciągu nocy

ggplot(czynniki, aes(y = Sleep_Hours)) +
  geom_boxplot() +
  labs(title = "Wykres pudełkowy średniej liczby godzin snu w ciągu nocy",
       y = "Średnia liczba godzin snu w ciągu nocy") +
  my_theme

#Wykres pudełkowy wyników z poprzednich egzaminów

ggplot(czynniki, aes(y = Previous_Scores)) +
  geom_boxplot() +
  labs(title = "Wykres pudełkowy wyników z poprzednich egzaminów",
       y = "Wyniki z poprzednich egzaminów [%]") +
  my_theme

#Wykres pudełkowy liczby sesji korepetycji w miesiącu

ggplot(czynniki, aes(y = Tutoring_Sessions)) +
  geom_boxplot() +
  labs(title = "Wykres pudełkowy liczby sesji korepetycji w miesiącu",
       y = "Liczba sesji korepetycji w miesiącu") +
  my_theme

#Wykres pudełkowy średniej liczby godzin aktywności fizycznej w tygodniu

ggplot(czynniki, aes(y = Physical_Activity)) +
  geom_boxplot() +
  labs(title = "Wykres pudełkowy średniej liczby godzin aktywności fizycznej w tygodniu",
       y = "Średnia liczba godzin aktywności fizycznej w tygodniu") +
  my_theme


library(validate)
library(tidyverse)
library(Rcmdr)
library(RcmdrMisc)
library(corrplot)
library(vcd)

install.packages("RcmdrMisc")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("vcd")

czynniki <- read.csv("czynniki-imputowane.csv")


#STATYSTYKI OPISOWE

numSummary(czynniki[, c("Hours_Studied", "Attendance", "Sleep_Hours", 
                        "Previous_Scores", "Tutoring_Sessions", 
                        "Physical_Activity", "Exam_Score")])

#HISTOGRAMY

#Histogram godzin uczenia się

hist(czynniki$Hours_Studied,
     main = "Histogram godzin nauki w tygodniu",   
     xlab = "Liczba godzin nauki",         
     ylab = "Ilość osób",     
     col = "blue",               
     border = "black") 

#Histogram obecności na zajęciach

hist(czynniki$Attendance,
     main = "Histogram obecności na zajęciach",   
     xlab = "Obecność [%]",         
     ylab = "Ilość osób",     
     col = "red",               
     border = "black") 

#Histogram średniej liczby godzin snu w ciągu nocy

hist(czynniki$Sleep_Hours,
     main = "Histogram średniej liczby godzin snu w ciągu nocy",   
     xlab = "Liczba godzin snu",         
     ylab = "Ilość osób",     
     col = "pink",               
     border = "black") 

#Histogram wyników z poprzednich egzaminów

hist(czynniki$Previous_Scores,
     main = "Histogram wyników z poprzednich egzaminów",   
     xlab = "Wyniki [%]",         
     ylab = "Ilość osób",     
     col = "green",               
     border = "black") 

#Histogram liczby sesji korepetycji w miesiącu

hist(czynniki$Tutoring_Sessions,
     main = "Histogram liczby sesji korepetycji w miesiącu",   
     xlab = "Liczba sesji",         
     ylab = "Ilość osób",     
     col = "purple",               
     border = "black") 

#Histogram średniej liczby godzin aktywności fizycznej w tygodniu

hist(czynniki$Physical_Activity,
     main = "Histogram średniej liczby godzin aktywności fizycznej w tygodniu",   
     xlab = "Liczba godzin",         
     ylab = "Ilość osób",     
     col = "blue",               
     border = "black") 

#Histogram wyników z egzaminu

hist(czynniki$Exam_Score,
     main = "Histogram wyników z egzaminu",   
     xlab = "Wynik [%]",         
     ylab = "Ilość osób",     
     col = "brown",               
     border = "black") 


#WYKRESY SŁUPKOWE

#Wykres słupkowy wpływu rodziców

freq <- table(czynniki$Parental_Involvement)
barplot(freq,
        main = "Wykres słupkowy wpływu rodziców", 
        xlab = "Stopień wpływu", 
        ylab = "Ilość osób", 
        col = "green", 
        border = "black") 

#Wykres słupkowy dostępności zasobów edukacyjnych

freq <- table(czynniki$Access_to_Resources)
barplot(freq,
        main = "Wykres słupkowy dostępności zasobów edukacyjnych", 
        xlab = "Stopień dostępności", 
        ylab = "Ilość osób", 
        col = "red", 
        border = "black") 

#Wykres słupkowy zajęć dodatkowych

freq <- table(czynniki$Extracurricular_Activities)
barplot(freq,
        main = "Wykres słupkowy zajęć dodatkowych", 
        xlab = "Uczestnictwo", 
        ylab = "Ilość osób", 
        col = "purple", 
        border = "black") 

#Wykres słupkowy poziomu motywacji

freq <- table(czynniki$Motivation_Level)
barplot(freq,
        main = "Wykres słupkowy poziomu motywacji", 
        xlab = "Poziom", 
        ylab = "Ilość osób", 
        col = "pink", 
        border = "black") 

#Wykres słupkowy dostępu do Internetu

freq <- table(czynniki$Internet_Access)
barplot(freq,
        main = "Wykres słupkowy dostępu do Internetu", 
        xlab = "Dostępność", 
        ylab = "Ilość osób", 
        col = "blue", 
        border = "black") 

#Wykres słupkowy odległości z domu do szkoły

freq <- table(czynniki$Distance_from_Home)
barplot(freq,
        main = "Wykres słupkowy odległości z domu do szkoły", 
        xlab = "Odległość", 
        ylab = "Ilość osób", 
        col = "gray", 
        border = "black") 

#Wykres słupkowy płci

freq <- table(czynniki$Gender)
barplot(freq,
        main = "Wykres słupkowy płci", 
        xlab = "Płeć", 
        ylab = "Ilość osób", 
        col = "yellow", 
        border = "black") 

#Wykres słupkowy trudności w uczeniu się

freq <- table(czynniki$Learning_Disabilities)
barplot(freq,
        main = "Wykres słupkowy trudności w uczeniu się", 
        xlab = "Występowanie trudności", 
        ylab = "Ilość osób", 
        col = "orange", 
        border = "black") 

#Wykres słupkowy wpływu rówieśników

freq <- table(czynniki$Peer_Influence)
barplot(freq,
        main = "Wykres słupkowy wpływu rówieśników", 
        xlab = "Wpływ rówieśników", 
        ylab = "Ilość osób", 
        col = "brown", 
        border = "black") 

#Wykres słupkowy jakości nauczycieli

freq <- table(czynniki$Teacher_Quality)
barplot(freq,
        main = "Wykres słupkowy jakości nauczycieli", 
        xlab = "Stopień jakości", 
        ylab = "Ilość osób", 
        col = "gold", 
        border = "black") 

#Wykres słupkowy rodzaju szkoły

freq <- table(czynniki$School_Type)
barplot(freq,
        main = "Wykres słupkowy rodzaju szkoły", 
        xlab = "Rodzaj szkoły", 
        ylab = "Ilość osób", 
        col = "maroon", 
        border = "black") 

#Wykres słupkowy poziomu dochodów rodziny

freq <- table(czynniki$Family_Income)
barplot(freq,
        main = "Wykres słupkowy poziomu dochodów rodziny", 
        xlab = "Poziom dochodów", 
        ylab = "Ilość osób", 
        col = "salmon", 
        border = "black") 

#Wykres słupkowy poziomu wykształcenia rodziców

freq <- table(czynniki$Parental_Education_Level)
barplot(freq,
        main = "Wykres słupkowy poziomu wykształcenia rodziców", 
        xlab = "Poziom wykształcenia", 
        ylab = "Ilość osób", 
        col = "plum", 
        border = "black") 


#KORELACJE zmienne ilościowe

czynniki %>%
  select(where(is.numeric))

cor(czynniki[, c("Hours_Studied", "Attendance", "Sleep_Hours", 
                 "Previous_Scores", "Tutoring_Sessions", 
                 "Physical_Activity", "Exam_Score")], method = "pearson", use = "pairwise.complete.obs")


#MACIERZE KORELACJI - zmienne ilościowe

cor_matrix <- cor(czynniki[, c("Hours_Studied", "Attendance", "Sleep_Hours", 
                               "Previous_Scores", "Tutoring_Sessions", 
                               "Physical_Activity", "Exam_Score")], 
                  use = "pairwise.complete.obs")

corrplot(cor_matrix, method = "color", tl.cex = 0.8, addCoef.col = "black")



#MACIERZE KORELACJI - zmienne jakościowe

data <- data.frame(
  Parental_Involvement = sample(c("Low", "Medium", "High"), 100, replace = TRUE),
  Access_to_Resources = sample(c("Low", "Medium", "High"), 100, replace = TRUE),
  Extracurricular_Activities = sample(c("Yes", "No"), 100, replace = TRUE),
  Motivation_Level = sample(c("Low", "Medium", "High"), 100, replace = TRUE),
  Internet_Access = sample(c("No", "Yes"), 100, replace = TRUE),
  Gender = sample(c("Male", "Female"), 100, replace = TRUE),
  Distance_from_Home = sample(c("Near", "Moderate", "Far"), 100, replace = TRUE),
  Learning_Disabilities = sample(c("Yes", "No"), 100, replace = TRUE),
  Peer_Influence = sample(c("Positive", "Neutral", "Negative"), 100, replace = TRUE),
  School_Type = sample(c("Private", "Public"), 100, replace = TRUE),
  Teacher_Quality = sample(c("Low", "Medium", "High"), 100, replace = TRUE),
  Family_Income = sample(c("Low", "Medium", "High"), 100, replace = TRUE),
  Parental_Education_Level = sample(c("College", "High School", "Postgraduate"), 100, replace = TRUE)
)


categorical_vars <- c("Parental_Involvement", "Access_to_Resources", "Extracurricular_Activities", 
                      "Motivation_Level", "Internet_Access", "Distance_from_Home", "Gender", 
                      "Learning_Disabilities", "Peer_Influence", "School_Type", "Teacher_Quality", 
                      "Family_Income", "Parental_Education_Level")

calculate_cramer_v <- function(var1, var2) {
  contingency_table <- table(var1, var2)
  cramer_v_result <- assocstats(contingency_table)$cramer
  return(cramer_v_result)
}

cramer_matrix <- matrix(NA, nrow = length(categorical_vars), ncol = length(categorical_vars), 
                        dimnames = list(categorical_vars, categorical_vars))

#Obliczanie Cramera dla każdej pary zmiennych


install.packages("ggplot2")
library(ggplot2)

#Histogram godzin nauki w tygodniu

ggplot(czynniki, aes(x = Hours_Studied)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram godzin nauki w tygodniu", 
       x = "Liczba godzin nauki", 
       y = "Ilość osób") +
  theme_minimal()


#Histogram obecności na zajęciach

ggplot(czynniki, aes(x = Attendance)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram obecności na zajęciach", 
       x = "Obecność [%]", 
       y = "Ilość osób") +
  theme_minimal()

#Histogram średniej liczby godzin snu w ciągu nocy

ggplot(czynniki, aes(x = Sleep_Hours)) +
  geom_histogram(binwidth = 1, fill = "pink", color = "black", alpha = 0.7) +
  labs(title = "Histogram średniej liczby godzin snu w ciągu nocy", 
       x = "Liczba godzin snu", 
       y = "Ilość osób") +
  theme_minimal()

#Histogram wyników z poprzednich egzaminów

ggplot(czynniki, aes(x = Previous_Scores)) +
  geom_histogram(binwidth = 5, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Histogram wyników z poprzednich egzaminów", 
       x = "Wyniki [%]", 
       y = "Ilość osób") +
  theme_minimal()

#Histogram liczby sesji korepetycji w miesiącu

ggplot(czynniki, aes(x = Tutoring_Sessions)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black", alpha = 0.7) +
  labs(title = "Histogram liczby sesji korepetycji w miesiącu", 
       x = "Liczba sesji", 
       y = "Ilość osób") +
  theme_minimal()

#Histogram średniej liczby godzin aktywności fizycznej w tygodniu

ggplot(czynniki, aes(x = Physical_Activity)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram średniej liczby godzin aktywności fizycznej w tygodniu", 
       x = "Liczba godzin", 
       y = "Ilość osób") +
  theme_minimal()

#Histogram wyników z egzaminu

ggplot(czynniki, aes(x = Exam_Score)) +
  geom_histogram(binwidth = 5, fill = "brown", color = "black", alpha = 0.7) +
  labs(title = "Histogram wyników z egzaminu", 
       x = "Wynik [%]", 
       y = "Ilość osób") +
  theme_minimal()

for (i in 1:(length(categorical_vars) - 1)) {
  for (j in (i + 1):length(categorical_vars)) {
    var1 <- categorical_vars[i]
    var2 <- categorical_vars[j]
    cramer_matrix[i, j] <- calculate_cramer_v(data[[var1]], data[[var2]])
    cramer_matrix[j, i] <- cramer_matrix[i, j]  #Uzupełniamy symetrycznie
  }
}

#Wizualizacja macierzy Cramera
corrplot(cramer_matrix, method = "color", tl.cex = 0.8, addCoef.col = "black")


