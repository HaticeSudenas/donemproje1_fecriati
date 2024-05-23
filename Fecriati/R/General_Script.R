# Kütüphaneleri yükle

library(tidyverse)
library(magrittr)
library(dplyr)

#' Veritabanı bağlantısını kurar ve bağlantı nesnesini döndürür
#'
#' @param db_path Veritabanı dosyasının yolunu belirtir.
#' @return Veritabanı bağlantısı nesnesi.
#' @export
connect_to_database <- function(db_path) {
  library(RSQLite)
  con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

}

#' CSV dosyasını yükler ve veri çerçevesi döndürür
#'
#' @param file_path CSV dosyasının yolu.
#' @param sep CSV dosyasındaki ayırıcı karakter (varsayılan ";").
#' @return Veri çerçevesi.
#' @export
load_data <- function(file_path, sep = ";") {
  data <- read.csv(file_path, sep = sep, stringsAsFactors = FALSE)
  return(data)
}

#' Veriye genel bakış ve eksik verileri kontrol eder
#'
#' @param data Veri çerçevesi.
#' @export
summarize_data <- function(data) {

}

#' Eksik verileri doldurma veya çıkarma işlemlerini yapar
#'
#' @param data Veri çerçevesi.
#' @return Düzenlenmiş veri çerçevesi.
#' @export
handle_missing_data <- function(data) {
  # Boş karakter dizilerini NA ile değiştir
  data <- data %>%
    mutate(across(where(is.character), ~ifelse(. == "", NA, .)))

  # Sayısal verilerde NA değerlerini sütun ortalaması ile değiştir
  data <- data %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

  return(data)
}

#' Gereksiz kolonları kaldırır
#'
#' @param data Veri çerçevesi.
#' @param columns Kaldırılacak kolonların isimleri.
#' @return Düzenlenmiş veri çerçevesi.
#' @export
remove_unnecessary_columns <- function(data, columns) {
  data <- select(data, -all_of(columns))
  return(data)
}

#' Veri çerçevesini CSV dosyasına kaydeder
#'
#' @param data Veri çerçevesi.
#' @param file_path Kaydedilecek dosyanın yolu.
#' @export
save_data <- function(data, file_path) {
  write.csv(data, file_path, row.names = FALSE)

  return(head(data))
}

# Ana işlem fonksiyonu
#' Veriyi yükler, eksik verileri işler, gereksiz kolonları kaldırır ve sonuçları kaydeder
#'
#' @param input_path Giriş CSV dosyasının yolu.
#' @param output_path Çıkış CSV dosyasının yolu.
#' @param sep CSV dosyasındaki ayırıcı karakter (varsayılan ";").
#' @param remove_cols Kaldırılacak kolonların isimleri.
#' @export
process_data <- function(input_path, output_path, sep = ";", remove_cols = c("latitude", "longitude", "X")) {
  data <- load_data(input_path, sep)
  summarize_data(data)
  data <- handle_missing_data(data)
  data <- remove_unnecessary_columns(data, remove_cols)
  summarize_data(data)
  save_data(data, output_path)
  return(data)
}
#' Grafik oluşturma ve istatistiksel analizler yapma
#'
#' @param data Veri çerçevesi.
#' @export
create_graphics <- function(data) {
  # Yaş dağılımını gösteren histogram
  p <- ggplot(data, aes(x = age)) +
    geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = "Kullanıcıların Yaş Dağılımı", x = "Yaş", y = "Kullanıcı Sayısı") +
    theme_minimal()
  print(p)


}





