library(readxl)
library(tidyverse)
library(writexl)
library(lubridate)

input_dir  <- "C:/Users/Xiangyong Zhang/iCloudDrive/Desktop/Sendai_Gaul_Matched_Xlsx_Separation/Sendai_Gaul_Matched_Xlsx_Separation_FLOOD/Sendai_Gaul_Matched_Xlsx_ADM2_sep_FLOOD"
output_dir <- "/Users/zhangxiangyong/Desktop/Sendai_Plot/No_Shp_Only_1"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

files <- list.files(input_dir, pattern = "\\.xlsx$", full.names = TRUE)

for (f in files) {
  
  df <- read_xlsx(f, col_types = "text")
  
  fname <- basename(f)
  fname <- sub("\\.xlsx$", "", fname)
  
  parts <- strsplit(fname, "_")[[1]]
  disaster <- parts[2]
  iso3c    <- parts[3]
  
  title_txt <- paste0(iso3c, "_", disaster)
  out_file  <- file.path(output_dir, paste0(fname, ".png"))
  
  raw_date <- trimws(df$`Date (YMD)`)
  raw_date[raw_date == ""] <- NA
  
  parsed_date <- parse_date_time(
    raw_date,
    orders = c("Y/m/d","m/d/Y","d/m/Y","y-m-d"),
    quiet = TRUE
  )
  
  df$`Date (YMD)` <- as.Date(parsed_date)
  
  if (all(is.na(df$`Date (YMD)`))) {
    message("SKIP (no valid dates): ", fname)
    next
  }
  
  dmin <- min(df$`Date (YMD)`, na.rm = TRUE)
  dmax <- max(df$`Date (YMD)`, na.rm = TRUE)
  
  all_dates <- data.frame(`Date (YMD)` = seq(dmin, dmax, by = "day"))
  
  colnames(all_dates) <- "Date (YMD)"
  
  daily_counts <- df %>%
    filter(!is.na(`Date (YMD)`)) %>%
    count(`Date (YMD)`, name = "number")
  
  full_daily_counts <- all_dates %>%
    left_join(daily_counts, by = "Date (YMD)") %>%
    mutate(number = replace_na(number, 0))
  
  p <- ggplot(full_daily_counts, aes(x = `Date (YMD)`, y = number)) +
    geom_line(aes(group = 1)) +
    geom_point() +
    theme_bw() +
    labs(title = title_txt, x = "Date", y = "Count")
  
  png(out_file, width = 1200, height = 800)
  print(p)
  dev.off()
}











