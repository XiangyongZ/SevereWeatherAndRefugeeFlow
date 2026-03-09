input_dir  <- "/Users/zhangxiangyong/Desktop/Sendai_Gaul_Matched_Xlsx_IND_Only"
output_dir <- "/Users/zhangxiangyong/Desktop/Sendai_Gaul_Matched_Xlsx_Separation/Sendai_Gaul_Matched_Xlsx_IND_Only_sep"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

files <- list.files(input_dir, pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)

for (f in files) {
  
  df <- read_xlsx(f, col_types = "text") %>%
    select(-Serial)   
  
  df <- df %>%
    separate(
      Event,
      into = c("Event", "Associated_Event_One", "Associated_Event_Two"),
      sep = "/",
      fill = "right"
    ) %>%
    filter(!is.na(Event), !is.na(ISO3C))
  
  split_list <- split(df, list(df$Event, df$ISO3C, df$mtchd_1), drop = TRUE)
  
  for (nm in names(split_list)) {
    
    df_out <- split_list[[nm]]
    
    event <- unique(df_out$Event)[1]
    iso   <- unique(df_out$ISO3C)[1]
    mtchd   <- unique(df_out$mtchd_1)[1]
    
    out_file <- file.path(output_dir, paste0("Sendai_", event, "_", iso, "_", mtchd, ".xlsx"))
    
    write_xlsx(df_out, out_file)
  }
}