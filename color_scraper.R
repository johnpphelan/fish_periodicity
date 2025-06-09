
library(xlsx)
library(tidyverse)

data_path <- "//sfp.idir.bcgov/s140/S40203/WFC AEB/General/2 Stewardship Mgt Climate Change/Drought/Communications/Periodicity tables/"

excel_files <- list.files(path = data_path, pattern = "\\.xlsx$", full.names = TRUE)


for (file in excel_files) {
  cat("Processing file:", basename(file), "\n")
  
  wb <- loadWorkbook(file)
  
  
  #wb <- loadWorkbook(paste0(data_path, "KBR periodicty tables_updated May 2025.xlsx"))
  
  sheets<- getSheets(wb)
  for (sheet_name in names(sheets)){
    cat("Processing sheet:", sheet_name, "\n")
    
    #sheet <- getSheets(wb)[["Creston Water Management Precin"]]
    sheet<- sheets[[sheet_name]]
    rows <- getRows(sheet)
    cells <- getCells(rows)
    styles <- sapply(cells, getCellStyle)
    
    # Extract fill color
    cellColor <- function(style) {
      fg <- tryCatch(style$getFillForegroundXSSFColor(), error = function(e) NULL)
      rgb <- tryCatch(fg$getRgb(), error = function(e) NULL)
      if (!is.null(rgb)) paste(rgb, collapse = "") else NA
    }
    
    # Extract text + color
    cell_texts <- sapply(cells, getCellValue)
    styles <- sapply(cells, getCellStyle)
    
    cell_colors <- sapply(styles, cellColor)
    
    cell_info <- data.frame(
      address = names(cells),
      text = unname(cell_texts),
      color = unname(cell_colors),
      stringsAsFactors = FALSE
    )
    
    color_mapping <- list(
      "adadad" = "Present",       
      "dae9f8" = "Drought", 
      "595959" = "Species"
    )
    
    cell_info$stage <- sapply(cell_info$color, function(x) {
      if (is.na(x)) {
        return(NA) # No color, or white background (no specific stage indicated)
      } else if (x %in% names(color_mapping)) {
        return(color_mapping[[x]])
      } else {
        return("Other/Unmapped Color") # In case "595959" appears as a fill in data cells
      }
    })
    
    
    col_to_excel_letter <- function(col_index) {
      if (is.na(col_index) || col_index <= 0) return(NA)
      # Max column in your image is Z (26), so LETTERS is sufficient.
      # If you go beyond Z, you'd need to extend this.
      letters <- c(LETTERS)
      return(letters[col_index])
    }
    
    cell_info <- cell_info |> 
      separate(address, into = c("row", "col_num"), sep = "\\.", convert = TRUE) |> 
      mutate(
        col_letter = sapply(col_num, col_to_excel_letter),
        row = as.numeric(row)
      )
    
    
    month_names<-tolower(month.name) 
    
    
    month_cols <- cell_info |> 
      filter(!is.na(text)) |> 
      mutate(text_lower = tolower(text)) |> 
      filter(text_lower %in% month_names) |> 
      select(row, col_num, col_letter, text)
    
    month_lookup <- month_cols |> 
      select(month = text, col_num)
    
    species_with_months <- cell_info |>
      full_join(month_cols, by = "col_num")
    
    species_with_months <- species_with_months |> 
      rename(
        row = row.x,
        text = text.x,
        month = text.y
      ) |>
      select(row, col_num, text, month, color, stage)
    
    # Create a combined column as a list with both text and stage
    species_filtered <- species_with_months |>
      filter(!(is.na(text) & is.na(stage)))
    
    # Combine text and stage into a list-column
    combined_data <- species_filtered |>
      mutate(cell = map2(text, stage, ~list(text = .x, stage = .y))) |>
      select(row, col_num, cell)
    
    # Pivot wider
    reshaped_df <- combined_data |>
      pivot_wider(names_from = col_num, values_from = cell, names_prefix = "col_") |>
      arrange(row)
    
    life_stages_df <- reshaped_df |>
      mutate(row_index = row_number()) |>
      transmute(row_index, life_stage = map_chr(col_1, ~ .x$text %||% NA_character_))
    
    
    
    data_cols <- names(reshaped_df)[-1]
    
    
    cell_cols <- grep("^col_\\d+$", names(reshaped_df), value = TRUE)
    reshaped_df <- reshaped_df |>
      mutate(row_index = row,
             col1_text = map_chr(col_1, ~ .x$text %||% NA_character_),
             col1_stage = map_chr(col_1, ~ .x$stage %||% NA_character_))
    
    reshaped_df <- reshaped_df |>
      mutate(species = if_else(col1_stage == "Species", col1_text, NA_character_)) |>
      fill(species)
    
    reshaped_df <- reshaped_df |>
      mutate(life_stage = if_else(col1_stage != "Species", col1_text, NA_character_))
    
    long_df <- reshaped_df |>
      pivot_longer(cols = all_of(cell_cols), names_to = "col_name", values_to = "cell") |>
      mutate(
        stage = map_chr(cell, ~ .x$stage %||% NA_character_),
        text  = map_chr(cell, ~ .x$text %||% NA_character_)
      )
    
    
    # Find the row that contains "Typical drought months"
    drought_row <- cell_info |> 
      filter(!is.na(text)) |> 
      filter(str_detect(tolower(text), "typical drought months")) |> 
      pull(row) |> 
      unique()
    
    # Now extract month names from that row
    month_names <- tolower(month.name)
    
    month_cols <- cell_info |> 
      filter(!is.na(text)) |> 
      mutate(text_lower = tolower(text)) |> 
      filter(text_lower %in% month_names) |> 
      select(row, col_num, col_letter, text) |> 
      distinct(col_num, .keep_all = TRUE)  # Keep only one entry per column
    
    
    # Step 1: Identify missing adjacent columns
    missing_month_cols <- month_cols |> 
      mutate(col_num = col_num + 1) |> 
      anti_join(month_cols, by = "col_num") |> 
      mutate(
        col_letter = NA,  # You can fill this in later if needed
        text = text,      # Copy the month name
        row = row         # Keep the same row
      )
    
    # Step 2: Bind the new rows to the original
    month_cols_filled <- bind_rows(month_cols, missing_month_cols) |> 
      arrange(col_num)
    
    # Ensure month_cols_filled has col_name
    month_cols_filled <- month_cols_filled |> 
      mutate(col_name = paste0("col_", col_num))
    
    # Join just the 'text' column from month_cols_filled onto long_df
    long_df_joined <- long_df |> 
      left_join(month_cols_filled |>  select(col_name, month_text = text), by = "col_name")
    
    location<-long_df_joined$text[1]
    
    final_df<- long_df_joined |>
      filter(!is.na(stage) & !is.na(species)) |> 
      filter(stage == "Present") |> 
      select(species, stage, month_text, col1_text) |> 
      mutate(loc = location)
    
    
    
    drought_header_row <- long_df |>
      filter(text == "Typical drought months") |>
      distinct(row_index) |>
      pull()
    
    drought_months <- long_df |>
      filter(row_index == drought_header_row + 1, stage == "Drought", !is.na(text)) |>
      pull(text)
    
    
    final_df <- final_df |> 
      mutate(is_drought_month = month_text %in% drought_months) |> 
      distinct() |> 
      select(-stage) |> 
      rename(month = month_text, stage = col1_text)
    
    if (!dir.exists("output/")) {
      dir.create("output/")
    }
    write.csv(final_df, 
              file = paste0("output/species_life_stages_", tolower(gsub(" ", "_", sheet_name)), ".csv"), 
              row.names = FALSE)
  }
}


