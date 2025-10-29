library(dplyr)
library(lubridate)

baru_data        <- readRDS("baru_data.rds")
baru_data          <- baru_data %>% mutate(locality = "Baru")

baru_data <- baru_data %>%
  filter(year(collection_date_start) != 2006 & year(collection_date_end) != 2006)

# Definir rango de fechas
fecha_inicio <- as.Date("2019-04-09")
fecha_fin    <- as.Date("2020-04-09")

# Filtrar los registros de Baru que caen dentro de este rango
baru_data <- baru_data %>%
  filter(locality == "Baru" &
           collection_date_start >= fecha_inicio &
           collection_date_end   <= fecha_fin)

saveRDS(baru_data, "baru_data_2019_2020.rds")

guanacaste_data  <- readRDS("guanacaste_data.rds")
guanacaste_data    <- guanacaste_data %>% mutate(locality = "Guanacaste")

# Definir rango de fechas
fecha_inicio <- as.Date("2013-11-14")
fecha_fin    <- as.Date("2014-11-14")

# Filtrar los registros de Baru que caen dentro de este rango
guanacaste_data <- guanacaste_data %>%
  filter(locality == "Guanacaste" &
           collection_date_start >= fecha_inicio &
           collection_date_end   <= fecha_fin)

saveRDS(guanacaste_data, "guanacaste_data_2013_2014.rds")






