library(tidyverse)
dane <- read_csv("czynniki.csv")

# przekodowanie zmiennych na faktorowe

dane$Parental_Involvement <- ifelse(dane$Parental_Involvement == "Low", 1,
                                    ifelse(dane$Parental_Involvement == "Medium", 2, 3))
dane$Parental_Involvement <- factor(dane$Parental_Involvement, levels = c(1, 2, 3),
                                    labels = c("Niskie", "Średnie", "Wysokie"), ordered = TRUE)

dane$Access_to_Resources <- ifelse(dane$Access_to_Resources == "Low", 1,
                                   ifelse(dane$Access_to_Resources == "Medium", 2, 3))
dane$Access_to_Resources <- factor(dane$Access_to_Resources, levels = c(1, 2, 3),
                                   labels = c("Niski", "Średni", "Wysoki"), ordered = TRUE)

dane$Extracurricular_Activities <- ifelse(dane$Extracurricular_Activities == "No", 0, 1)
dane$Extracurricular_Activities <- factor(dane$Extracurricular_Activities, levels = c(0, 1),
                                          labels = c("Nie", "Tak"))

dane$Motivation_Level <- ifelse(dane$Motivation_Level == "Low", 1,
                                ifelse(dane$Motivation_Level == "Medium", 2, 3))
dane$Motivation_Level <- factor(dane$Motivation_Level, levels = c(1, 2, 3),
                                labels = c("Niski", "Średni", "Wysoki"), ordered = TRUE)

dane$Internet_Access <- ifelse(dane$Internet_Access == "No", 0, 1)
dane$Internet_Access <- factor(dane$Internet_Access, levels = c(0, 1), labels = c("Nie", "Tak"))

dane$Family_Income <- ifelse(is.na(dane$Family_Income), NA,
                             ifelse(dane$Family_Income == "Low", 1,
                                    ifelse(dane$Family_Income == "Medium", 2, 3)))
dane$Family_Income <- factor(dane$Family_Income, levels = c(1, 2, 3),
                             labels = c("Niski", "Średni", "Wysoki"), ordered = TRUE)

dane$Teacher_Quality <- ifelse(is.na(dane$Teacher_Quality), NA,
                               ifelse(dane$Teacher_Quality == "Low", 1,
                                      ifelse(dane$Teacher_Quality == "Medium", 2, 3)))
dane$Teacher_Quality <- factor(dane$Teacher_Quality, levels = c(1, 2, 3),
                               labels = c("Niska", "Średnia", "Wysoka"), ordered = TRUE)

dane$School_Type <- ifelse(dane$School_Type == "Public", 1, 2)
dane$School_Type <- factor(dane$School_Type, levels = c(1, 2), labels = c("Publiczna", "Prywatna"))

dane$Peer_Influence <- ifelse(dane$Peer_Influence == "Negative", 1,
                              ifelse(dane$Peer_Influence == "Neutral", 2, 3))
dane$Peer_Influence <- factor(dane$Peer_Influence, levels = c(1, 2, 3),
                              labels = c("Negatywny", "Neutralny", "Pozytywny"), ordered = TRUE)

dane$Learning_Disabilities <- ifelse(dane$Learning_Disabilities == "No", 0, 1)
dane$Learning_Disabilities <- factor(dane$Learning_Disabilities, levels = c(0, 1),
                                     labels = c("Nie", "Tak"))

dane$Parental_Education_Level <- ifelse(is.na(dane$Parental_Education_Level), NA,
                                        ifelse(dane$Parental_Education_Level == "High School", 1,
                                               ifelse(dane$Parental_Education_Level == "College", 2, 3)))
dane$Parental_Education_Level <- factor(dane$Parental_Education_Level, levels = c(1, 2, 3),
                                        labels = c("Średnie", "Wyższe", "Podyplomowe"),
                                        ordered = TRUE)

dane$Distance_from_Home <- ifelse(is.na(dane$Distance_from_Home), NA,
                                  ifelse(dane$Distance_from_Home == "Far", 1,
                                         ifelse(dane$Distance_from_Home == "Moderate", 2, 3)))
dane$Distance_from_Home <- factor(dane$Distance_from_Home, levels = c(1, 2, 3),
                                  labels = c("Daleka", "Umiarkowana", "Bliska"), ordered = TRUE)

dane$Gender <- ifelse(dane$Gender == "Female", 1, 2)
dane$Gender <- factor(dane$Gender, levels = c(1, 2), labels = c("Kobieta", "Mężczyzna"))

# ustawienie stylu wykresow

my_theme <- theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 11))

# parental involvement - wykres

ggplot(dane, aes(x = Parental_Involvement, fill = Parental_Involvement)) +
  geom_bar(color = "black") +
  labs(title = "Zaangażowanie rodziców", x = NULL, y = "Liczba wystąpień") +
  my_theme + theme(legend.position = "none")

# access to resources - wykres

ggplot(dane, aes(x = Access_to_Resources, fill = Access_to_Resources)) +
  geom_bar(color = "black") +
  labs(title = "Dostęp do zasobów", x = NULL, y = "Liczba wystąpień") +
  my_theme + theme(legend.position = "none")

# extracurricular activities - wykres

ggplot(dane, aes(x = Extracurricular_Activities, fill = Extracurricular_Activities)) +
  geom_bar(color = "black") +
  labs(title = "Udział w zajęciach pozalekcyjnych", x = NULL, y = "Liczba wystąpień") +
  my_theme + theme(legend.position = "none")

# motivation level - wykres

ggplot(dane, aes(x = Motivation_Level, fill = Motivation_Level)) +
  geom_bar(color = "black") +
  labs(title = "Poziom motywacji", x = NULL, y = "Liczba wystąpień") +
  my_theme + theme(legend.position = "none")

# internet access - wykres

ggplot(dane, aes(x = Internet_Access, fill = Internet_Access)) +
  geom_bar(color = "black") +
  labs(title = "Dostęp do internetu", x = NULL, y = "Liczba wystąpień") +
  my_theme + theme(legend.position = "none")

# family income - wykres

ggplot(dane, aes(x = Family_Income, fill = Family_Income)) +
  geom_bar(color = "black") +
  labs(title = "Dochód rodziny", x = NULL, y = "Liczba wystąpień") +
  my_theme + theme(legend.position = "none")

# teacher quality - wykres

ggplot(dane, aes(x = Teacher_Quality, fill = Teacher_Quality)) +
  geom_bar(color = "black") +
  labs(title = "Jakość nauczania", x = NULL, y = "Liczba wystąpień") +
  my_theme + theme(legend.position = "none")

# school type - wykres

ggplot(dane, aes(x = School_Type, fill = School_Type)) +
  geom_bar(color = "black") +
  labs(title = "Rodzaj szkoły", x = NULL, y = "Liczba wystąpień") +
  my_theme + theme(legend.position = "none")

# peer influence - wykres

ggplot(dane, aes(x = Peer_Influence, fill = Peer_Influence)) +
  geom_bar(color = "black") +
  labs(title = "Wpływ rówieśników", x = NULL, y = "Liczba wystąpień") +
  my_theme + theme(legend.position = "none")

# learning disabilities - wykres

ggplot(dane, aes(x = Learning_Disabilities, fill = Learning_Disabilities)) +
  geom_bar(color = "black") +
  labs(title = "Występowanie trudności w uczeniu się", x = NULL, y = "Liczba wystąpień") +
  my_theme + theme(legend.position = "none")

# parental education level - wykres

ggplot(dane, aes(x = Parental_Education_Level, fill = Parental_Education_Level)) +
  geom_bar(color = "black") +
  labs(title = "Poziom wykształcenia rodziców", x = NULL, y = "Liczba wystąpień") +
  my_theme + theme(legend.position = "none")

# distance from home - wykres

ggplot(dane, aes(x = Distance_from_Home, fill = Distance_from_Home)) +
  geom_bar(color = "black") +
  labs(title = "Odległość od domu", x = NULL, y = "Liczba wystąpień") +
  my_theme + theme(legend.position = "none")

# gender - wykres

ggplot(dane, aes(x = Gender, fill = Gender)) +
  geom_bar(color = "black") +
  labs(title = "Płeć", x = NULL, y = "Liczba wystąpień") +
  my_theme + theme(legend.position = "none")




