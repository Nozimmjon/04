#libraries

library(readxl)
library(lubridate)
library(stringr)
library(gtsummary)
library(janitor)
library(skimr)
library(here)
library(gt)
library(knitr)
library(writexl)
library(tidyverse)
library(rlang)
library(openxlsx)


#read and cleaning data 

jizzax_input <- read_clean_data("jizzax") %>% 
  clean_names()

#list of positive and negative labels for each column
pos_labels_q1 <- c("Яхшиланади", "Ёмонлашади")
pos_labels_q2 <- c("Ошди", "Пасайди")
pos_labels_q3 <- c("Кўпаяди", "Қисқаради")
pos_labels_q4 <- c("Кўпайди", "Камайди")
pos_labels_q5 <- c("Кўпаяди", "Камаяди")
pos_labels_q6 <- c("Ҳа","Йўқ")



# Define a function to calculate balance score with custom labels
balance_score <- function(x, pos_labels) {
  # Get positive and negative responses
  positive <- sum(x[pos_labels[1]])
  negative <- sum(x[pos_labels[2]])
  
  # Calculate balance score
  bs_score <- (positive - negative) / sum(x) * 100 + 100
  
  return(bs_score)
}


# Apply the function to each question column by district with custom labels
jizzax_output <- jizzax_input %>% 
  group_by(district) %>% 
  summarise(q_1 = balance_score(table(q_1), pos_labels_q1),
            q_2 = balance_score(table(q_2), pos_labels_q2),
            q_3 = balance_score(table(q_3), pos_labels_q3),
            q_4 = balance_score(table(q_4), pos_labels_q4),
            q_5 = balance_score(table(q_5), pos_labels_q5),
            q_6 = balance_score(table(q_6), pos_labels_q6)) 


# Create a new data frame with values for whole region
whole_region <- data.frame(district = "Whole Region",
                           q_1 = balance_score(table(jizzax_input$q_1), pos_labels_q1),
                           q_2 = balance_score(table(jizzax_input$q_2), pos_labels_q2),
                           q_3 = balance_score(table(jizzax_input$q_3), pos_labels_q3),
                           q_4 = balance_score(table(jizzax_input$q_4), pos_labels_q4),
                           q_5 = balance_score(table(jizzax_input$q_5), pos_labels_q5),
                           q_6 = balance_score(table(jizzax_input$q_6), pos_labels_q6))

# Append it to jizzax_output using rbind()
jizzax_output <- rbind(jizzax_output, whole_region) 
  
  
# Create new columns using mutate()

jizzax_output <- jizzax_output %>%
  mutate(bs_score_cur = (q_2 + q_4 + q_6)/3,
         bs_score_fut = (q_1 + q_3 + q_5)/3,
         bs_gen = (bs_score_cur + bs_score_fut) / 2) %>% 
  mutate_if(is.numeric, round, digits=0)

#frequency table for q_7

q7 <- reg_table(jizzax_input, q_7) %>% 
  mutate_at(vars(-district), as.double) %>% 
  mutate_if(is.numeric, round, digits =0)



# frequency table for q_8
q_8 <- jizzax_input %>%
  mutate(q_8 = str_replace_all(q_8, "\\(.*\\)", "")) %>% 
  separate_rows(q_8, sep = ",") %>%
  mutate(q_8 = str_trim(q_8)) %>% 
  mutate(q_8 = recode(q_8, 
                      "Соғлиқни сақлаш хизматлари сифатини ошириш  борасидаги ишлар" = "Соғлиқни сақлашда",
                      "Мактабгача таълим тизими билан қамров даражасини ошириш борасидаги ишлар" = "Мактабгача таълим тизими қамровида",
                      "Мактабда таълим сифатини ошириш борасидаги ишлар" = "Мактаб таълим сифатида",
                      "Аҳолини уй-жой билан таъминлаш ишларини" = "Уй-жой билан таъминлаш",
                      "Маҳаллабай ишлаш ва ҳоким ёрдамчилари тизимидаги ишларни" = "Маҳаллабай ишлаш ва ҳоким ёрдамчилари",
                      "Ўқитувчиларни қўллаб-қувватлаш ишларини" = "Ўқитувчиларни қўллаб-қувватлаш",
                      "Шифокорларни қўллаб-қувватлаш ишларини" = "Шифокорларни қўллаб-қувватлаш",
                      "Коррупцияга қарши курашишлар" = "Коррупцияга қарши курашиш",
                      "Камбағаликка қарши кураш ишларини" = "Камбағаликка қарши кураш",
                      "Олий таълим тизимидаги ислоҳотларни" = "Олий таълимдаги ислоҳотлар",
                      "Тадбиркорларни қўллаб-қувватлаш ишларини" = "Тадбиркорларни қўллаб-қувватлаш",
                      "Автомобил сотиб олиш жараёнларини" = "Автомобил сотиб олиш жараёнларида",
                      "Газ таъминоти соҳаси" = "Газ таъминотида",
                      "Иш билан таъминлаш соҳаси" = "Бандликда",
                      "Электр таъминоти соҳасида" = "Электр таъминотида",
                      "Ахолини ижтимоий куллаб кувватлаш" = "Ижтимоий қўллаб-қувватлаш",
                      "Йул курилиши тизимини ривожлантириш" = "Йўл қуриш ишларида",
                      .default = "Бошқа")) %>% 
  count(district, q_8) %>%
  group_by(district) %>%
  mutate(freq = n/sum(n)) %>%
  ungroup() %>%
  select(-n) %>% 
  pivot_wider(names_from = q_8, values_from = freq, values_fill = 0) %>%
  mutate(across(-district, as.double)) %>%
  adorn_totals() %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(rounding = "half up", digits = 0, affix_sign = FALSE)



list_of_dataframes <- list("index" =jizzax_output,  "problems_district" = q7, "islohotlar_tezlashtirish" = q_8)  

wb <- createWorkbook()
lapply(seq_along(list_of_dataframes), function(i){
  addWorksheet(wb=wb, sheetName = names(list_of_dataframes[i]))
  writeData(wb, sheet = i, list_of_dataframes[[i]])
})
#Save Workbook
saveWorkbook(wb, "jizzax_indeks_2023_01.xlsx", overwrite = TRUE) 

