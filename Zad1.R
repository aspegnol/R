library(dplyr)
library(ggplot2)

#Wczytanie plików; jeśli nie w tym samym katalogu, to podać ze ścieżką
Klienci <- read.csv("Klienci.csv", sep=";")
Przesylki <- read.csv("Przesylki.csv", sep=";")

#Formatowanie kolumny as Date
Przesylki$Data_przesylki <- as.Date(Przesylki$Data_przesylki,"%d.%m.%Y")

#Nowa kolumna - pierwszy dzień miesiąca z daty
Przesylki$Month <- as.Date(cut.Date(Przesylki$Data_przesylki,breaks='month'))

#Grupowanie po miesiącu w nowej kolumnie
nowa <- Przesylki %>% 
    filter(!is.na(Month)) %>% 
       group_by(Month) %>% 
            count(Month)

#Zapisanie wyniku do pliku
write.csv2(nowa, 'Zad1.csv', row.names = T, na = '', fileEncoding = 'Windows-1250');

#Wykres słupkowy
obiekt <- ggplot(data = nowa, mapping = aes(nowa$Month,nowa$n))
wykres <- obiekt + geom_bar(stat='identity') +
    labs(title='Liczba przesyłek w poszczególnych miesiącach',x='Miesiące/lata',y='Liczba przesyłek')

#zapisanie wykresu do pliku
ggsave("Zad1.png",wykres,width=14,height=9)