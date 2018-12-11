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

#Podzielenie liczby przesyłek przez liczbę unikalnych klientów
unikalni <- razem %>% select(Month,ID_Klienta) %>% group_by(Month) %>% count(ID_Klienta)
unikalni_suma <- unikalni %>% group_by(Month) %>% summarise(sum(n))
names(unikalni_suma)[2] <- "Suma"
unikalni_liczba <- unikalni %>% select(Month,ID_Klienta) %>% group_by(Month) %>% count(Month)


srednio <- unikalni_suma %>% inner_join(unikalni_liczba,by='Month')
srednio$Mean <- srednio$Suma / srednio$n

#Wybranie tylko tych kolumn, które nas interesują
srednio_2 <- srednio %>% select(Month,Mean)

#Zapisanie wyników do pliku
write.csv2(srednio_2, 'Zad4.csv', row.names = T, na = '', fileEncoding = 'Windows-1250');

#Wykres punktowy
obiekt <- ggplot(data = srednio_2, mapping = aes(srednio_2$Month,srednio_2$Mean))
wykres <- obiekt + geom_point(stat='identity') +
  labs(title='Średnia liczba przesyłek na 1 klienta (w miesiącu)',x='Miesiące/lata',y='Średnia liczba przesyłek')  

#Zapisanie wyniku do pliku
ggsave("Zad4.png",wykres,width=14,height=9)