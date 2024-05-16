#' Zmiana typu czasu.
#' 
#' Zmienia format czasu z x na y.
#' @param df Nasza ramka danych.
#' @param namesFrom Kolumna, w której posiadamy date. Domyślnie "Date".
#' @param namesTo Nazwa kolumny wyniku. Domyślnie nie zmieniamy nazwy.
#' @param from Początkowy format daty.
#' @param to Końcowy format daty.
#' @return Zwraca ramkę ze zmienionym formatem danych.
#' @export

czas <- function(df,nameFrom="Date",nameTo=nameFrom, from ="%Y-%m-%d %H:%M:%S" , to ="%d-%m-%Y %H:%M:%S" ){
df1 <- df[,nameFrom]
suppressWarnings(
df1<- as.POSIXct(df1,from)
)
suppressWarnings(
df1<- format(df1,to)
)
df[,nameFrom] <- df1
colnames(df)[colnames(df)==nameFrom] <- nameTo 
df
}
#' Normalizacja danych numerycznych.
#'
#' Zmienia wartości wszystkich lub podanych kolumn do przedziału <0,1>.
#' @param df Nasza ramka danych.
#' @param which Numery kolumn, w których mamy znormalizować dane.
#' @param check Czy sprawdzamy każdą kolumnę. Jeśli podajemy jakie kolumny zmienić na FALSE!. W przeciwnym wypadku funkcja sama znormalizuje tylko kolumny numeryczne.
#' @param decimals Ile cyfr po przecnku ma mieć wynik w kolumnach.
#' @return Zwraca ramkę z znormalizowany wartościami liczbowymi.
#' @export

unormuj <- function(df, which = c(2, length(colnames(df))), check = TRUE, decimals = 3) {
  if (!check) {
    for (i in which) {
      largest <- max(df[, i], na.rm = TRUE)
      df[, i] <- round(df[, i] / largest, decimals)
    }
  } else {
    for (i in 1:length(colnames(df))) {
      if (is.numeric(df[, i])) {
        largest <- max(df[, i], na.rm = TRUE)
        df[, i] <- round(df[, i] / largest, decimals)
      }
    }
  }
  return(df)
}
#' Wyodrebnienie czesci daty.
#'
#' Wyodrebnia podana czesc daty i tworzy badz nie nowa kolumne.
#' @param df Nasza ramka danych.
#' @param name Nazwa kolumny z datą. Domyślnie "Date".
#' @param what Którą część daty chcemy wyodrębnić.
#' @param from Format daty w  kolumnie poczatkowej.
#' @param new Czy tworzymy nową kolumne, czy nadpisujemy starą.
#' @param newName Nazwa stworzonej kolumny.
#' @return Zwraca ramkę z kolumną z wyodrębnioną częścią daty.
#' @export

wyodrebnij <- function(df,name = "Date", what= c("%H"), from="%d-%m-%Y %H:%M:%S",  new = TRUE, newName =c("Hour")){
  if (!new){
    for (x in 1:length(what)){
    suppressWarnings(
    df [,name] <- sprintf("%02d", as.numeric(format(as.POSIXct(df[,name],from),what[x])))
    )
    
    }
    
  }
  else{
    for (x in 1:length(what)){
    suppressWarnings(
    kol <- sprintf("%02d", as.numeric(format(as.POSIXct(df[,name],from),what[x])))
    )
    df<-cbind(df,kol)
    colnames(df)[length(colnames(df))] <- newName[x]
    }
    df    
  }
}

#' Sprawdzenie korelacji x kolumn względem y kolumn
#'
#' Sprawdza korelacje pomiędzy podanymi kolumnanmi
#' @param df Nasza ramka danych.
#' @param whichx Numery kolumn  
#' @param whichy Numery 2 typu kolumn
#' @param method Metoda liczenia korelacji. Tak samo jak w funkcji bazowej cor.
#' @param high wartość współczynnika od jakiej mamy wypisywać pary na konsole
#' @return Zwraca współczynnik korelacji dla podanych par kolumn
#' @export
correlation <- function(df,whichx,whichy, method="spearman", high=0.01){
  for (x in 1:length(whichx)){
    for (y in 1:length(whichy)){
      suppressWarnings(
      z<-cor(df[,whichx[x]],df[,whichy[y]], method = method)
      )
      if (!is.na(z) &abs(z) >= high){
      cat("Sprawdzam", whichx[x], "i", whichy[y], "\n")
      print(z)
      }
    }
  }
}
