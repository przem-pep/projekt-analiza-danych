# Importowanie potrzebnych bibilotek

library(tidyverse)

# Importowanie danych

dane <- read_csv("czynniki.csv")

# Zbadanie struktury danych

dane
str(dane)

checkout <- function(x) {
  for(i in 1:ncol(x)) {
    print(table(x[ , i]))
    cat("\n")
  }
}

checkout(dane)

# Widoczne jest, że wiele zmiennych należy zmienić na typ factor

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

dane$Family_Income <- ifelse(is.na(dane$Family_Income), NA,
                             ifelse(dane$Family_Income == "Low", 1,
                                    ifelse(dane$Family_Income == "Medium", 2, 3)))
dane$Family_Income <- factor(dane$Family_Income, levels = c(1, 2, 3),
                             labels = c("Low", "Medium", "High"), ordered = TRUE)

dane$Teacher_Quality <- ifelse(is.na(dane$Teacher_Quality), NA,
                               ifelse(dane$Teacher_Quality == "Low", 1,
                                      ifelse(dane$Teacher_Quality == "Medium", 2, 3)))
dane$Teacher_Quality <- factor(dane$Teacher_Quality, levels = c(1, 2, 3),
                               labels = c("Low", "Medium", "High"), ordered = TRUE)

dane$School_Type <- ifelse(dane$School_Type == "Public", 1, 2)
dane$School_Type <- factor(dane$School_Type, levels = c(1, 2), labels = c("Public", "Private"))

dane$Peer_Influence <- ifelse(dane$Peer_Influence == "Negative", 1,
                              ifelse(dane$Peer_Influence == "Neutral", 2, 3))
dane$Peer_Influence <- factor(dane$Peer_Influence, levels = c(1, 2, 3),
                              labels = c("Negative", "Neutral", "Positive"), ordered = TRUE)

dane$Learning_Disabilities <- ifelse(dane$Learning_Disabilities == "No", 0, 1)
dane$Learning_Disabilities <- factor(dane$Learning_Disabilities, levels = c(0, 1),
                                     labels = c("No", "Yes"))

dane$Parental_Education_Level <- ifelse(is.na(dane$Parental_Education_Level), NA,
                                        ifelse(dane$Parental_Education_Level == "High School", 1,
                                        ifelse(dane$Parental_Education_Level == "College", 2, 3)))
dane$Parental_Education_Level <- factor(dane$Parental_Education_Level, levels = c(1, 2, 3),
                                        labels = c("High School", "College", "Postgraduate"),
                                        ordered = TRUE)

dane$Distance_from_Home <- ifelse(is.na(dane$Distance_from_Home), NA,
                                  ifelse(dane$Distance_from_Home == "Far", 1,
                                         ifelse(dane$Distance_from_Home == "Moderate", 2, 3)))
dane$Distance_from_Home <- factor(dane$Distance_from_Home, levels = c(1, 2, 3),
                                  labels = c("Far", "Moderate", "Near"), ordered = TRUE)

dane$Gender <- ifelse(dane$Gender == "Female", 1, 2)
dane$Gender <- factor(dane$Gender, levels = c(1, 2), labels = c("Female", "Male"))

checkout(dane)

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