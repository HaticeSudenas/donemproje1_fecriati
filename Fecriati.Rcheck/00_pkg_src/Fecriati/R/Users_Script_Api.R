library(httr)
library(jsonlite)
library(readr)
library(dplyr)

# WooCommerce API bilgileri
url <- "http://your-woocommerce-site.com/wp-json/wc/v3/customers"
consumer_key <- "your_consumer_key"
consumer_secret <- "your_consumer_secret"

# Users CSV dosyasını okuma ve sütunları ayırma
users <- read_delim('usersyeni.csv', delim = ';', locale = locale(encoding = 'latin1'))
colnames(users) <- tolower(gsub(' ', '_', gsub('[/"]', '', colnames(users))))

# Sütun adlarını kontrol etme
print("Users Columns:")
print(colnames(users))

# Verileri JSON formatına dönüştürme ve API üzerinden gönderme
for (i in 1:nrow(users)) {
  customer <- list(
    email = users$email[i],
    first_name = users$first_name[i],
    last_name = users$last_name[i],
    billing = list(
      first_name = users$first_name[i],
      last_name = users$last_name[i],
      address_1 = ifelse(!is.na(users$street_address[i]), users$street_address[i], ""),
      city = ifelse(!is.na(users$city[i]), users$city[i], ""),
      state = ifelse(!is.na(users$state[i]), users$state[i], ""),
      postcode = ifelse(!is.na(users$postal_code[i]), users$postal_code[i], ""),
      country = ifelse(!is.na(users$country[i]), users$country[i], ""),
      email = users$email[i],
      phone = ifelse(!is.na(users$phone[i]), users$phone[i], "")
    ),
    shipping = list(
      first_name = users$first_name[i],
      last_name = users$last_name[i],
      address_1 = ifelse(!is.na(users$street_address[i]), users$street_address[i], ""),
      city = ifelse(!is.na(users$city[i]), users$city[i], ""),
      state = ifelse(!is.na(users$state[i]), users$state[i], ""),
      postcode = ifelse(!is.na(users$postal_code[i]), users$postal_code[i], ""),
      country = ifelse(!is.na(users$country[i]), users$country[i], "")
    ),
    meta_data = list(
      list(key = "gender", value = ifelse(!is.na(users$gender[i]), users$gender[i], "")),
      list(key = "age", value = ifelse(!is.na(users$age[i]), users$age[i], "")),
      list(key = "latitude", value = ifelse(!is.na(users$latitude[i]), users$latitude[i], "")),
      list(key = "longitude", value = ifelse(!is.na(users$longitude[i]), users$longitude[i], "")),
      list(key = "traffic_source", value = ifelse(!is.na(users$traffic_source[i]), users$traffic_source[i], ""))
    )
  )

  response <- POST(
    url,
    authenticate(consumer_key, consumer_secret, type = "basic"),
    body = toJSON(customer, auto_unbox = TRUE),
    encode = "json"
  )

  # Ham yanıtı görüntüle
  print(paste("Status Code:", status_code(response)))
  print(paste("Response Text:", content(response, "text", encoding = "UTF-8")))

  # JSON olarak parse etmeyi deneyin
  try({
    response_json <- fromJSON(content(response, "text", encoding = "UTF-8"))
    print(toJSON(response_json, pretty = TRUE))
  }, silent = TRUE)
}
