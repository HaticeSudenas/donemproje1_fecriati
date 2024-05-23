library(httr)
library(jsonlite)
library(readr)
library(dplyr)

# WooCommerce API bilgileri
base_url <- "http://your-woocommerce-site.com/wp-json/wc/v3"
customers_url <- paste0(base_url, "/customers")
orders_url <- paste0(base_url, "/orders")
consumer_key <- "your_consumer_key"
consumer_secret <- "your_consumer_secret"

# Users CSV dosyasını okuma ve sütunları ayırma
users <- read_delim('usersyeni.csv', delim = ';', locale = locale(encoding = 'latin1'))
colnames(users) <- tolower(gsub(' ', '_', gsub('[/"]', '', colnames(users))))

# Orders CSV dosyasını okuma
orders <- read_delim('ordersyeni.csv', delim = ';')
colnames(orders) <- tolower(gsub(' ', '_', gsub('[/"]', '', colnames(orders))))

# Order Items CSV dosyasını okuma
order_items <- read_delim('order_items_yeni.csv', delim = ';')
colnames(order_items) <- tolower(gsub(' ', '_', gsub('[/"]', '', colnames(order_items))))

# Sütun adlarını kontrol etme
print("Users Columns:")
print(colnames(users))
print("Orders Columns:")
print(colnames(orders))
print("Order Items Columns:")
print(colnames(order_items))

# Kullanıcı ID'lerini maplemek için bir sözlük oluşturma
custom_id_map <- list()

# Müşteri ID'lerini almak için API çağrısı
for (i in 1:nrow(users)) {
  custom_id <- users$id[i]
  response <- GET(customers_url, authenticate(consumer_key, consumer_secret, type = "basic"), query = list(search = custom_id, meta_key = "custom_id", meta_value = custom_id))
  customers <- fromJSON(content(response, "text", encoding = "UTF-8"))
  if (length(customers) > 0) {
    customer_id <- customers[[1]]$id
    custom_id_map[[as.character(custom_id)]] <- customer_id
  }
}

# Siparişleri eklemek için müşterilerin mevcut ID'lerini kullanma
merged_orders <- merge(orders, order_items, by = "order_id", suffixes = c("", "_item"))

for (order_id in unique(merged_orders$order_id)) {
  order_group <- merged_orders[merged_orders$order_id == order_id, ]
  order_row <- order_group[1, ]
  customer_custom_id <- order_row$user_id
  customer_id <- custom_id_map[[as.character(customer_custom_id)]]

  if (is.null(customer_id)) {
    print(paste("Müşteri bulunamadı:", customer_custom_id))
    next
  }

  line_items <- lapply(1:nrow(order_group), function(j) {
    item <- order_group[j, ]
    list(
      product_id = as.integer(item$product_id),
      quantity = 1
    )
  })

  order <- list(
    customer_id = customer_id,
    payment_method = "bacs",
    payment_method_title = "Direct Bank Transfer",
    set_paid = TRUE,
    billing = list(
      first_name = order_row$first_name,
      last_name = order_row$last_name,
      address_1 = ifelse(!is.na(order_row$street_address), order_row$street_address, ""),
      city = ifelse(!is.na(order_row$city), order_row$city, ""),
      state = ifelse(!is.na(order_row$state), order_row$state, ""),
      postcode = ifelse(!is.na(order_row$postal_code), order_row$postal_code, ""),
      country = ifelse(!is.na(order_row$country), order_row$country, ""),
      email = order_row$email,
      phone = ifelse(!is.na(order_row$phone), order_row$phone, "")
    ),
    shipping = list(
      first_name = order_row$first_name,
      last_name = order_row$last_name,
      address_1 = ifelse(!is.na(order_row$street_address), order_row$street_address, ""),
      city = ifelse(!is.na(order_row$city), order_row$city, ""),
      state = ifelse(!is.na(order_row$state), order_row$state, ""),
      postcode = ifelse(!is.na(order_row$postal_code), order_row$postal_code, ""),
      country = ifelse(!is.na(order_row$country), order_row$country, "")
    ),
    line_items = line_items
  )

  response <- POST(
    orders_url,
    authenticate(consumer_key, consumer_secret, type = "basic"),
    body = toJSON(order, auto_unbox = TRUE),
    encode = "json"
  )

  print(paste("Status Code:", status_code(response)))
  print(paste("Response Text:", content(response, "text", encoding = "UTF-8")))

  tryCatch({
    response_json <- fromJSON(content(response, "text", encoding = "UTF-8"))
    print(toJSON(response_json, pretty = TRUE))
  }, error = function(e) {
    print("JSONDecodeError: Yanıt JSON formatında değil.")
  })
}
