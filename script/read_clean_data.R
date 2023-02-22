


# Define a function to read and clean data for one region
read_clean_data <- function(region_name) {
  # Define new column names
  new_names <-   c("phone_number", "gender", "region",  "district", "age", "education",
                   "is_working",  "is_official",  "q_1", "q_2", "q_3", "income", 
                   "q_4", "q_5", "q_6", "q_7", "q_8", "q_9")
  
  # Load data
  data <- readxl::read_xlsx(here("data", paste0(region_name, ".xlsx")), skip = 1, col_names = new_names) %>% 
    select(-3)
  
  # Clean data
  data <- data %>% 
    relocate(income, .before = q_1) %>% 
    mutate(across(where(is.character), as_factor)) %>% 
    distinct(across(everything())) %>% 
    drop_na(district) 
  
  return(data)
}

