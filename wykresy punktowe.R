library(tidyverse)

dane <- read_csv("czynniki.csv")

my_theme <- theme_classic() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 11))

ggplot(dane, aes(x = Hours_Studied, y = Exam_Score)) +
  geom_point(size = 3, alpha = 0.1, colour = "red") +
  labs(title = "Wynik egzaminu w zależności od liczby godzin spędzonych na nauce",
       x = "Liczba godzin nauki [h]",
       y = "Wynik z egzaminu [%]") +
  my_theme

ggplot(dane, aes(x = Attendance, y = Exam_Score)) +
  geom_point(size = 3, alpha = 0.1, colour = "black") +
  labs(title = "Wynik egzaminu w zależności od obecności",
       x = "Obecność [%]",
       y = "Wynik z egzaminu [%]") +
  my_theme

ggplot(dane, aes(x = Parental_Involvement, y = Exam_Score)) +
  geom_point(size = 3, alpha = 0.1, colour = "purple") +
  labs(title = "Wynik egzaminu w zależności od zaangażowania rodziców",
       x = "Zaangażowanie rodziców",
       y = "Wynik z egzaminu [%]") +
  my_theme

ggplot(dane, aes(x = Access_to_Resources, y = Exam_Score)) +
  geom_point(size = 3, alpha = 0.1, colour = "grey") +
  labs(title = "Wynik egzaminu w zależności od od dostępu do zasobów naukowych",
       x = "Dostęp do zasobów",
       y = "Wynik z egzaminu [%]") +
  my_theme

ggplot(dane, aes(x = Extracurricular_Activities, y = Exam_Score)) +
  geom_point(size = 3, alpha = 0.1, colour = "pink") +
  labs(title = "Wynik egzaminu w zależności od liczby zajęć pozalekcyjnych",
       x = "Zajęcia pozalekcyjne",
       y = "Wynik z egzaminu [%]") +
  my_theme

ggplot(dane, aes(x = Sleep_Hours, y = Exam_Score)) +
  geom_point(size = 3, alpha = 0.1, colour = "blue") +
  labs(title = "Wynik egzaminu w zależności od liczby godzin snu",
       x = "Liczba godzin snu [h]",
       y = "Wynik z egzaminu [%]") +
  my_theme

ggplot(dane, aes(x = Previous_Scores, y = Exam_Score)) +
  geom_point(size = 3, alpha = 0.1, colour = "green") +
  labs(title = "Wynik egzaminu w zależności od poprzednich wyników z egzaminu",
       x = "Poprzednie wyniki z egzaminu [%]",
       y = "Wynik z egzaminu [%]") +
  my_theme

ggplot(dane, aes(x = Motivation_Level, y = Exam_Score)) +
  geom_point(size = 3, alpha = 0.1, colour = "yellow") +
  labs(title = "Wynik egzaminu w zależności od poziomu motywacji",
       x = "Poziom motywacji",
       y = "Wynik z egzaminu [%]") +
  my_theme

ggplot(dane, aes(x = Internet_Access, y = Exam_Score)) +
  geom_point(size = 3, alpha = 0.1, colour = "brown") +
  labs(title = "Wynik egzaminu w zależności od dostępu do internetu",
       x = "Dostęp do internetu",
       y = "Wynik z egzaminu [%]") +
  my_theme

ggplot(dane, aes(x = Tutoring_Sessions, y = Exam_Score)) +
  geom_point(size = 3, alpha = 0.1, colour = "orange") +
  labs(title = "Wynik egzaminu w zależności od ilości korepetycji",
       x = "Ilość korepetycji",
       y = "Wynik z egzaminu [%]") +
  my_theme

ggplot(dane, aes(x = Family_Income, y = Exam_Score)) +
  geom_point(size = 3, alpha = 0.1, colour = "lightblue") +
  labs(title = "Wynik egzaminu w zależności od zarobku rodziny",
       x = "Zarobek rodziny",
       y = "Wynik z egzaminu [%]") +
  my_theme

ggplot(dane, aes(x = Teacher_Quality, y = Exam_Score)) +
  geom_point(size = 3, alpha = 0.1, colour = "lightgreen") +
  labs(title = "Wynik egzaminu w zależności od jakości kształcenia",
       x = "Jakość kształcenia",
       y = "Wynik z egzaminu [%]") +
  my_theme

ggplot(dane, aes(x = School_Type, y = Exam_Score)) +
  geom_point(size = 3, alpha = 0.1, colour = "darkgreen") +
  labs(title = "Wynik egzaminu w zależności od wybranego rodzaju szkoły",
       x = "Rodzaj szkoły",
       y = "Wynik z egzaminu [%]") +
  my_theme

ggplot(dane, aes(x = Peer_Influence, y = Exam_Score)) +
  geom_point(size = 3, alpha = 0.1, colour = "darkred") +
  labs(title = "Wynik egzaminu w zależności od wpływu rówieśników",
       x = "Wpływ rówieśników",
       y = "Wynik z egzaminu [%]") +
  my_theme

ggplot(dane, aes(x = Physical_Activity, y = Exam_Score)) +
  geom_point(size = 3, alpha = 0.1, colour = "darkblue") +
  labs(title = "Wynik egzaminu w zależności od liczby godzin spędzonych na aktywności fizycznej",
       x = "Aktywność fizyczna [h]",
       y = "Wynik z egzaminu [%]") +
  my_theme

ggplot(dane, aes(x = Learning_Disabilities, y = Exam_Score)) +
  geom_point(size = 3, alpha = 0.1, colour = "darkorange") +
  labs(title = "Wynik egzaminu w zależności od niepełnosprawności",
       x = "Niepełnosprawność",
       y = "Wynik z egzaminu [%]") +
  my_theme

ggplot(dane, aes(x = Parental_Education_Level, y = Exam_Score)) +
  geom_point(size = 3, alpha = 0.1, colour = "lightpink") +
  labs(title = "Wynik egzaminu w zależności od wykształcenia rodziców",
       x = "Wykształcenie rodziców",
       y = "Wynik z egzaminu [%]") +
  my_theme

ggplot(dane, aes(x = Distance_from_Home, y = Exam_Score)) +
  geom_point(size = 3, alpha = 0.1, colour = "lightgrey") +
  labs(title = "Wynik egzaminu w zależności od odległości od domu",
       x = "Odległość od domu",
       y = "Wynik z egzaminu [%]") +
  my_theme

ggplot(dane, aes(x = Gender, y = Exam_Score)) +
  geom_point(size = 3, alpha = 0.1, colour = "darkgrey") +
  labs(title = "Wynik egzaminu w zależności od płci",
       x = "Płeć",
       y = "Wynik z egzaminu [%]") +
  my_theme