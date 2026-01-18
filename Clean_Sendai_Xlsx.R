###################################################################
# To Filter the Desinventer Excel File to Keep the Needed Columns #
###################################################################

##########################################
# Install and load the required packages #
##########################################

library(readxl)
library(writexl)
library(dplyr)

#################################################################
# Set Up the Working Directory, and Specify the Desired Columns #
#################################################################

input_folder  <- "C:/Users/Xiangyong Zhang/OneDrive/桌面/Sendai-20250428T203057Z-001/Sendai_mex"      
output_folder <- "C:/Users/xiangyong Zhang/OneDrive/桌面/Sendai-20250428T203057Z-001/Sendai_mex"    
columns_to_keep <- c("Serial", "Event", "ADM1_CODE", "ADM1_NAME", "ADM2_CODE", "ADM2_NAME", "Date (YMD)", "Comments", "Cause", "Description of Cause", "Source", "Magnitude", "Deaths", "Injured", "Directly affected", "Indirectly Affected", "Evacuated", "Losses $USD", "Losses $Local", "Duration (d)")   
suffix <- "_Cleaned"                      

##################################################
# Set Up the Loop to Do the Filter to All Files  #
##################################################

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}


files <- list.files(input_folder, pattern = "\\.xlsx$", full.names = TRUE)

for (file in files) {
  

  df <- read_excel(file)
  

  cols_present <- intersect(columns_to_keep, colnames(df))
  df_filtered <- df |> select(all_of(cols_present))
  

  base_name <- tools::file_path_sans_ext(basename(file))
  new_name <- paste0(base_name, suffix, ".xlsx")
  output_path <- file.path(output_folder, new_name)
  

  write_xlsx(df_filtered, output_path)
}


