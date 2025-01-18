library(tidyverse)

dane <- read_csv("czynniki.csv")

# Należy najpierw zawrzeć przekodowanie zmiennych

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


# Dodanie własnego niestandardowego stylu dla wykresów

my_theme <- theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 11))

# Wykres punktowy wyniku z egzaminu w zależności od liczby godzin spędzonych na nauce

ggplot(dane, aes(x = Hours_Studied, y = Exam_Score)) +
  geom_point(size = 3, alpha = 0.1, colour = "black") +
  labs(title = "Wynik egzaminu w zależności od liczby godzin spędzonych na nauce",
       x = "Liczba godzin nauki [h]",
       y = "Wynik z egzaminu [%]") +
  my_theme

# Wykres punktowy wyniku z egzaminu w zależności od obecności

ggplot(dane, aes(x = Attendance, y = Exam_Score)) +
  geom_point(size = 3, alpha = 0.1, colour = "black") +
  labs(title = "Wynik egzaminu w zależności od obecności",
       x = "Obecność [%]",
       y = "Wynik z egzaminu [%]") +
  my_theme

# Rozkład ocen z egzaminów

ggplot(dane, aes(x = Exam_Score)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Rozkład ocen z egzaminu",
       x = "Ocena z egzaminu",
       y = "Liczebność") +
  my_theme

# Wykres pudełkowy wyników z egzaminu

ggplot(dane, aes(y = Exam_Score)) +
  geom_boxplot() +
  labs(title = "Wykres pudełkowy wyników z egzaminu",
       y = "Wynik z egzaminu [%]") +
  my_theme
