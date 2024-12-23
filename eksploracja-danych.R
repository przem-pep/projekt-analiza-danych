# Importowanie potrzebnych bibilotek

library(tidyverse)

# Importowanie danych

dane <- read_csv("czynniki.csv")

# Zbadanie struktury danych

dane
str(dane)

for(i in 1:ncol(dane)) {
  print(table(dane[ , i]))
  cat("\n")
}

# Widoczne jest, że wiele zmiennych należy zmienić na typ factor

# Liczenie braków danych

na_count <- sum(!complete.cases(dane))
na_count

na_count / nrow(dane)

# Nieco ponad 17% obserwacji posiada braki danych

# Zbadanie liczby braków danych w poszczególnych kolumnach

for(i in 1:ncol(dane)) {
  cat("Liczba braków danych w kolumnie", names(dane[, i]), ":", sum(is.na(dane[ , i])), "\n")
}

# Kolumny zawierające braki danych: Sleep_Hours, Family_Income, Teacher_Quality, Parental_Education_Level,
# Distance_from_Home, Exam_Score

dane$Parental_Involvement <- ifelse(dane$Parental_Involvement == "Low", 1,
                                    ifelse(dane$Parental_Involvement == "Medium", 2, 3))
dane$Parental_Involvement <- factor(dane$Parental_Involvement, levels = c(1, 2, 3),
                                    labels = c("Low", "Medium", "High"), ordered = TRUE)

dane$Access_to_Resources <- ifelse(dane$Access_to_Resources == "Low", 1,
                                   ifelse(dane$Access_to_Resources == "Medium", 2, 3))
dane$Access_to_Resources <- factor(dane$Access_to_Resources, levels = c(1, 2, 3),
                                   labels = c("Low", "Medium", "High"), ordered = TRUE)

dane$Extracurricular_Activities <- ifelse(dane$Extracurricular_Activities == "No", 0, 1)
dane$Extracurricular_Activities <- factor(dane$Extracurricular_Activities, levels = c(0, 1),
                                          labels = c("No", "Yes"))

dane$Motivation_Level <- ifelse(dane$Motivation_Level == "Low", 1,
                                ifelse(dane$Motivation_Level == "Medium", 2, 3))
dane$Motivation_Level <- factor(dane$Motivation_Level, levels = c(1, 2, 3),
                                labels = c("Low", "Medium", "High"), ordered = TRUE)

dane$Internet_Access <- ifelse(dane$Internet_Access == "No", 0, 1)
dane$Internet_Access <- factor(dane$Internet_Access, levels = c(0, 1), labels = c("No", "Yes"))
