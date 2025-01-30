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
