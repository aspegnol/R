library(dplyr)
library(ggplot2)

#Wczytanie plików; jeśli nie w tym samym katalogu, to podać ze ścieżką
Klienci <- read.csv("Klienci.csv", sep=";")
Przesylki <- read.csv("Przesylki.csv", sep=";")

#Formatowanie kolumny as Date
Przesylki$Data_przesylki <- as.Date(Przesylki$Data_przesylki,"%d.%m.%Y")

#Nowa kolumna - pierwszy dzień miesiąca z daty
Przesylki$Month <- as.Date(cut.Date(Przesylki$Data_przesylki,breaks='month'))

#Dołączenie klienta do przesyłki - z drugiego pliku
razem <- Przesylki %>% inner_join(Klienci,by='ID_Przesylki')

#Sumowanie klientów w ramach miesiąca
unikalni <- razem %>% select(Month,ID_Klienta) %>% group_by(Month) %>% count(ID_Klienta)
unikalni_liczba <- unikalni %>% select(Month,ID_Klienta) %>% group_by(Month) %>% count(Month)

#Zapisanie wyniku do pliku
write.csv2(unikalni_liczba, 'Zad3.csv', row.names = T, na = '', fileEncoding = 'Windows-1250');

#Wykres słupkowy
obiekt <- ggplot(data = unikalni_liczba, mapping = aes(unikalni_liczba$Month,unikalni_liczba$n))
wykres <- obiekt + geom_bar(stat='identity') +
  labs(title='Liczba unikalnych klientów w poszczególnych miesiącach',x='Miesiące/lata',y='Liczba unikalnych klientów')

#Zapisanie wykresu do pliku
ggsave("Zad3.png",wykres,width=14,height=9)