library(sf)
library(readxl)
library(countrycode)
library(tidygeocoder)
library(tidyverse)
library(rollama)

directories <- "C:/Users/Xiangyong Zhang/OneDrive/桌面/Sendai-20250428T203057Z-001/Sendai_Xlsx_Clean_No_Shp"

Sendai_Countries <- list.files(directories, pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)

gaul_1 <- st_read("C:/Users/Xiangyong Zhang/Box/SevereWeather_RefugeeOutflows/GAUL/gaul_1_2015_fixed/gaul_1_2015_fixed.shp")

gaul_1$iso3_code <- countrycode(gaul_1$ADM0_NAME,"country.name", "iso3c")

ok_list <- list()
miss_list <- list()
i <- 0

for (Sendai_df in Sendai_Countries) {
  
  Sendai_Country <- read_xlsx(Sendai_df)
  
  unique_gaul_name <- Sendai_Country[, 4] %>% distinct()
  colnames(unique_gaul_name)[1] <- "gaul_1_name"
  
  country <- sub("^Sendai_(.*)_Cleaned\\.xlsx$", "\\1", basename(Sendai_df))
  
  unique_gaul_name <- unique_gaul_name %>%
    mutate(country = country) %>%
    geocode(state = gaul_1_name, country = country, method = "osm",
            lat = latitude, long = longitude)
  
  unique_gaul_name$iso <- countrycode(unique_gaul_name$country, "country.name", "iso3c")
  
  unique_gaul_name_missing <- unique_gaul_name %>% filter(is.na(latitude) | is.na(longitude))
  unique_gaul_name_ok      <- unique_gaul_name %>% filter(!is.na(latitude) & !is.na(longitude))

  if (nrow(unique_gaul_name_ok) > 0) {
    unique_gaul_name_ok <- st_as_sf(unique_gaul_name_ok, coords = c("longitude", "latitude"), crs = 4326)
    
    this_iso <- unique(unique_gaul_name_ok$iso) %>% dplyr::first()
    gaul_1_country <- gaul_1 %>% filter(iso3_code == this_iso)
    
    if (nrow(gaul_1_country) > 0) {
      unique_gaul_name_ok <- unique_gaul_name_ok %>% st_join(gaul_1_country, left = FALSE)
    }
    
    unique_gaul_name_ok <- unique_gaul_name_ok %>% mutate(source_file = basename(Sendai_df))
  }
  
  if (nrow(unique_gaul_name_missing) > 0) {
    unique_gaul_name_missing <- unique_gaul_name_missing %>%
      mutate(source_file = basename(Sendai_df))
  }
  
  i <- i + 1
  ok_list[[i]]   <- unique_gaul_name_ok
  miss_list[[i]] <- unique_gaul_name_missing
}

Sendai_ok <- bind_rows(ok_list, .id = "source")

Sendai_ok <- Sendai_ok %>%
  select(gaul_1_name, iso, ADM1_NAME) %>%
  st_drop_geometry()

Sendai_ok$ADM1_NAME[Sendai_ok$gaul_1_name == "Luanda"] <- "Luanda"

Sendai_missing <- bind_rows(miss_list, .id = "source")

official_by_country <- split(
  data.frame(adm1_name = gaul_1$ADM1_NAME,
             adm1_code = gaul_1$ADM1_CODE),
  gaul_1$iso3_code
)

Sendai_missing$`qwen2.5:14b` <- vapply(seq_len(nrow(Sendai_missing)), function(i) {
  ISO3C <- Sendai_missing$iso[i]
  input <- Sendai_missing$gaul_1_name[i]
  
  official_df <- official_by_country[[ISO3C]]
  
  instruction <- paste0(
    "You are normalizing each country's first-level administrative unit names.\n",
    "Country: ", ISO3C, "\n",
    "Here is the list of official units (name and code):\n",
    paste0("- ", official_df$adm1_name, " | ", official_df$adm1_code, collapse = "\n"),
    "\n\nFor the input name, match it ONLY to the list above.\n",
    "Reply ONLY with the *code* of the matching unit.\n",
    "If nothing matches, reply exactly: NO MATCH."
  )
  
  q <- make_query(
    text   = input,
    prompt = instruction,
    system = "Return exactly one administrative code, or NO MATCH, no extra text."
  )
  
  out <- query(q, output = "text", model_params = list(temperature = 0), model = "Qwen2.5:14B")
  out <- trimws(as.character(out))
  
  as.numeric(out)
  
}, FUN.VALUE = numeric(1))

Sendai_missing$`llama3.1:8b` <- vapply(seq_len(nrow(Sendai_missing)), function(i) {
  ISO3C <- Sendai_missing$iso[i]
  input <- Sendai_missing$gaul_1_name[i]
  
  official_df <- official_by_country[[ISO3C]]
  
  instruction <- paste0(
    "You are normalizing each country's first-level administrative unit names.\n",
    "Country: ", ISO3C, "\n",
    "Here is the list of official units (name and code):\n",
    paste0("- ", official_df$adm1_name, " | ", official_df$adm1_code, collapse = "\n"),
    "\n\nFor the input name, match it ONLY to the list above.\n",
    "Reply ONLY with the *code* of the matching unit.\n",
    "If nothing matches, reply exactly: NO MATCH."
  )
  
  q <- make_query(
    text   = input,
    prompt = instruction,
    system = "Return exactly one administrative code, or NO MATCH, no extra text."
  )
  
  out <- query(q, output = "text", model_params = list(temperature = 0), model = "llama3.1:8b")
  out <- trimws(as.character(out))
  
  as.numeric(out)
  
}, FUN.VALUE = numeric(1))

Sendai_missing$`gemma3:27b` <- vapply(seq_len(nrow(Sendai_missing)), function(i) {
  ISO3C <- Sendai_missing$iso[i]
  input <- Sendai_missing$gaul_1_name[i]
  
  official_df <- official_by_country[[ISO3C]]
  
  instruction <- paste0(
    "You are normalizing each country's first-level administrative unit names.\n",
    "Country: ", ISO3C, "\n",
    "Here is the list of official units (name and code):\n",
    paste0("- ", official_df$adm1_name, " | ", official_df$adm1_code, collapse = "\n"),
    "\n\nFor the input name, match it ONLY to the list above.\n",
    "Reply ONLY with the *code* of the matching unit.\n",
    "If nothing matches, reply exactly: NO MATCH."
  )
  
  q <- make_query(
    text   = input,
    prompt = instruction,
    system = "Return exactly one administrative code, or NO MATCH, no extra text."
  )
  
  out <- query(q, output = "text", model_params = list(temperature = 0), model = "gemma3:27b")
  out <- trimws(as.character(out))
  
  as.numeric(out)
  
}, FUN.VALUE = numeric(1))

Sendai_missing$`gpt-oss:20b` <- vapply(seq_len(nrow(Sendai_missing)), function(i) {
  ISO3C <- Sendai_missing$iso[i]
  input <- Sendai_missing$gaul_1_name[i]
  
  official_df <- official_by_country[[ISO3C]]
  
  instruction <- paste0(
    "You are normalizing each country's first-level administrative unit names.\n",
    "Country: ", ISO3C, "\n",
    "Here is the list of official units (name and code):\n",
    paste0("- ", official_df$adm1_name, " | ", official_df$adm1_code, collapse = "\n"),
    "\n\nFor the input name, match it ONLY to the list above.\n",
    "Reply ONLY with the *code* of the matching unit.\n",
    "If nothing matches, reply exactly: NO MATCH."
  )
  
  q <- make_query(
    text   = input,
    prompt = instruction,
    system = "Return exactly one administrative code, or NO MATCH, no extra text."
  )
  
  out <- query(q, output = "text", model_params = list(temperature = 0), model = "gpt-oss:20b")
  out <- trimws(as.character(out))
  
  as.numeric(out)
  
}, FUN.VALUE = numeric(1))


model_cols <- c("qwen2.5:14b", "llama3.1:8b", "gemma3:27b", "gpt-oss:20b")

failed_rows <- subset(
  Sendai_missing,
  apply(Sendai_missing[model_cols], 1, function(x) {
    x_no_na <- x[!is.na(x)]
    length(x_no_na) < 3 || length(unique(x_no_na)) > 1
  })
)

passed_rows <- Sendai_missing %>%
  filter(!gaul_1_name %in% failed_rows$gaul_1_name) %>%
  select(gaul_1_name, iso, all_of(model_cols)) %>%
  pivot_longer(
    cols = all_of(model_cols),
    names_to = "model",
    values_to = "adm1_name"
  ) %>%
  group_by(gaul_1_name, iso, adm1_name) %>%
  summarize(count = n(), .groups = "drop") %>%
  group_by(gaul_1_name, iso) %>%  
  slice_max(order_by = count, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(gaul_1, by = c("adm1_name" = "ADM1_CODE")) %>%
  filter(iso == iso3_code) %>%
  select(gaul_1_name,iso,ADM1_NAME)
  
failed_rows <- failed_rows %>%
  select(gaul_1_name,iso)

failed_rows$ADM1_NAME[failed_rows$gaul_1_name == "Sweda"] <- "As_Suweida"

failed_rows$ADM1_NAME[failed_rows$gaul_1_name == "Qunitera"] <- "Al_Qunaytirah"

failed_rows$ADM1_NAME[failed_rows$gaul_1_name == "Edlib"] <- "Idleb"

Sendai_missing_matched <- rbind(failed_rows,passed_rows,Sendai_ok)

Sendai_missing_matched <- Sendai_missing_matched %>%
  arrange(iso,gaul_1_name)

saveRDS(Sendai_missing_matched, "C:/Users/Xiangyong Zhang/OneDrive/R_code_PVL/Sendai_missing_matched.rds")


