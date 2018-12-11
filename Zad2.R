library(dplyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)


#Wczytanie plików; jeśli nie w tym samym katalogu, to podać ze ścieżką
Klienci <- read.csv("Klienci.csv", sep=";")
Przesylki <- read.csv("Przesylki.csv", sep=";")

#Formatowanie kolumny as Date
Przesylki$Data_przesylki <- as.Date(Przesylki$Data_przesylki,"%d.%m.%Y")

#Nowa kolumna - pierwszy dzień miesiąca z daty
Przesylki$Month <- as.Date(cut.Date(Przesylki$Data_przesylki,breaks='month'))

#Dołączenie klienta do przesyłki - z drugiego pliku
razem <- Przesylki %>% inner_join(Klienci,by='ID_Przesylki')

#ID Klienta posiadające duplikaty
razem$duplikaty <- duplicated(razem$ID_Klienta) 

#Oznakowanie duplikatów i unikatów False/True, policzenie
pierwsi <- razem %>% filter(duplikaty == FALSE) 
pierwsi <- pierwsi  %>% arrange(Month) %>% group_by(Month) %>% count(duplikaty)
pierwsi <- pierwsi %>% select(Month,n)

drudzy <- razem %>% filter(razem$duplikaty ==TRUE)
drudzy <- drudzy  %>% arrange(Month) %>% group_by(Month) %>% count(duplikaty)
drudzy <- drudzy %>% select(Month,n)

#Wyniki zapisanie do dwóch plików
write.csv2(pierwsi, 'Zad2_a.csv', row.names = T, na = '', fileEncoding = 'Windows-1250')
write.csv2(drudzy, 'Zad2_b.csv', row.names = T, na = '', fileEncoding = 'Windows-1250')

#Wykres liniowy, w formacie HTML, interaktywny
wykres <- plot_ly(data = pierwsi, x = ~pierwsi$Month, y = ~pierwsi$n, type = 'scatter', mode = 'line', 
                  name = 'Pierwsze') %>%
  add_trace(data = drudzy, x = ~drudzy$Month, y = ~drudzy$n,  name = 'Kolejne') %>% 
    layout(title = ' Liczba przesyłek pierwszych i kolejnych', xaxis = list(title = 'Lata'),
           yaxis =  list(title='Liczba przesyłek'))

#Zapisanie wyniku w formacie HTML
saveWidget(wykres, file='Zad2.html')

