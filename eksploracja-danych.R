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