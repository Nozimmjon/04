dir.create(here("results", "tables", "buxoro"), recursive = TRUE, showWarnings = FALSE)
#Table 1 

buxoro_input_02 %>%
  tabyl(district, q_1) %>%
  adorn_percentages() %>% 
  select(district, "Ёмонлашади", "Ўзгармайди", "Яхшиланади") %>% 
  mutate_at(vars(-district), as.double) %>% 
  arrange(desc(across(starts_with("Ёмонлашади")))) %>% 
  gt(rowname_col = "district") %>% 
  tab_header(title = md("*1-савол.* **3 ойдан сўнг Ўзбекистонда <span style='color:red'>иқтисодий ҳолат</span> қандай ўзгаради?**"),
             subtitle = md("(*Респондентларнинг жавоблари*)")) %>% 
  my_theme_gt() %>% 
  cols_width(everything() ~ px(180)) %>% 
  gtsave('1_savol.png', path = here("results", "tables", "buxoro"))

#table 3
buxoro_input_02 %>%
  tabyl(district, q_3) %>%
  adorn_percentages() %>% 
  select(district, "Қисқаради", "Ўзгармайди", "Кўпаяди") %>% 
  mutate_at(vars(-district), as.double) %>% 
  arrange(desc(across(starts_with("Қисқаради")))) %>% 
  gt(rowname_col = "district") %>% 
  tab_header(title = md("*3-савол*. **Кейинги 3 ой давомида <span style='color:red'>оилавий даромадингиз</span> қандай кутиляпти?**"),
             subtitle = md("*(Респондентларнинг жавоблари)*")) %>% 
  my_theme_gt() %>% 
  cols_width(everything() ~ px(180)) %>% 
  gtsave('3_savol.png', path = here("results", "tables", "buxoro"))

#table 5
buxoro_input_02 %>%
  tabyl(district, q_5) %>%
  adorn_percentages() %>% 
  select(district, "Камаяди", "Ўзгармайди", "Кўпаяди") %>% 
  mutate_at(vars(-district), as.double) %>% 
  arrange(desc(across(starts_with("Камаяди")))) %>% 
  gt(rowname_col = "district") %>% 
  tab_header(title = md("*5-савол*. **3 ойдан сўнг  <span style='color:red'>янги иш ўринлари сони</span> қандай ўзгаради?**"),
             subtitle = md("*(Респондентларнинг жавоблари)*")) %>% 
  my_theme_gt() %>% 
  cols_width(everything() ~ px(180)) %>% 
  gtsave('5_savol.png', path = here("results", "tables", "buxoro"))

#table 2
buxoro_input_02 %>%
  tabyl(district, q_2) %>%
  adorn_percentages() %>% 
  select(district, "Пасайди", "Ўзгармади", "Ошди") %>% 
  mutate_at(vars(-district), as.double) %>% 
  arrange(desc(across(starts_with("Пасайди")))) %>% 
  gt(rowname_col = "district") %>% 
  tab_header(title = md("*2-савол*. **Ўтган 3 ойга нисбатан <span style='color:red'>оилавий даромадингиз </span> қандай ўзгарди?**"),
             subtitle = md("*(Респондентларнинг жавоблари)*")) %>% 
  my_theme_gt() %>% 
  cols_width(everything() ~ px(180)) %>% 
  gtsave('2_savol.png', path = here("results", "tables", "buxoro"))

#table 4
buxoro_input_02 %>%
  tabyl(district, q_4) %>%
  adorn_percentages() %>% 
  select(district, "Камайди", "Ўзгармади", "Кўпайди") %>% 
  mutate_at(vars(-district), as.double) %>% 
  arrange(desc(across(starts_with("Камайди")))) %>% 
  gt(rowname_col = "district") %>% 
  tab_header(title = md("*4-савол*. **Ўтган 3 ойга нисбатан <span style='color:red'>янги иш ўринлари  </span> қандай ўзгарди?**"),
             subtitle = md("*(Респондентларнинг жавоблари)*")) %>% 
  my_theme_gt() %>% 
  cols_width(everything() ~ px(180)) %>% 
  gtsave('4_savol.png', path = here("results", "tables", "buxoro"))


#table 6
buxoro_input_02 %>%
  tabyl(district, q_6) %>%
  adorn_percentages() %>% 
  select(district, "Қулай фурсат эмас" = "Йўқ", "Билмайман",   "Қулай фурсат" = "Ҳа") %>% 
  mutate_at(vars(-district), as.double) %>% 
  arrange(desc(across(starts_with("Қулай фурсат эмас")))) %>% 
  gt(rowname_col = "district") %>% 
  tab_header(title = md("*6-савол*. **Ҳозир узоқ муддатли <span style='color:red'> истеъмол товарларини харид қилиш  </span> учун қулай фурсатми?**"),
             subtitle = md("*(Респондентларнинг жавоблари)*")) %>% 
  my_theme_gt() %>% 
  cols_width(everything() ~ px(180)) %>% 
  gtsave('6_savol.png', path = here("results", "tables", "buxoro"))


#table 11 mahalliy organlarni baholash 

buxoro_input_02 %>%
  tabyl(district, q_9) %>%
  adorn_percentages() %>% 
  select(district, "Жуда ёмон", "Ёмон", "Ўртача", "Яхши", "Жуда яхши") %>% 
  mutate_at(vars(-district), as.double) %>% 
  arrange(desc(across(starts_with("Жуда ёмон")))) %>% 
  gt(rowname_col = "district") %>% 
  tab_header(title = md("**Маҳаллий ҳокимият органи фаолиятини қандай баҳолайсиз?**"),
             subtitle = md("(*Респондентларнинг жавоблари)*")) %>% 
  cols_width(everything() ~ px(120)) %>% 
  my_theme_gt() %>% 
  gtsave('mahalliy_organ.png', path = here("results", "tables", "buxoro"))




#table ishsizlik

buxoro_input_02 %>% 
  filter(is_working != "Пенсиядаман") %>% 
  tabyl(district, is_working) %>%
  adorn_percentages() %>% 
  select(district, "Ишламайди"="Йўқ", "Ишлайди"="Ҳа") %>% 
  mutate_at(vars(-district), as.double) %>% 
  arrange(desc(across(starts_with("Ишламайди")))) %>% 
  gt(rowname_col = "district") %>% 
  tab_header(title = md("**Туманлар (шаҳарлар) кесимида меҳнат ресурсларнинг <span style='color:red'>иш билан бандлик</span>  ҳолати**"),
             subtitle = md("*(Респондентларнинг жавоблари)*")) %>% 
  cols_width(everything() ~ px(190)) %>% 
  my_theme_gt() %>% 
  gtsave('ishsizlik.png', path = here("results", "tables", "buxoro"))  

#gender
buxoro_input_02 %>% 
  filter(is_working != "Пенсиядаман") %>% 
  tabyl(gender, is_working) %>% 
  adorn_percentages()

#formality

buxoro_input_02 %>% 
  filter(is_working == "Ҳа") %>% 
  tabyl(district, is_official) %>%
  adorn_percentages() %>% 
  select(district, "Расмийлаштирилмаган" = "Йуқ", "Расмийлаштирилган" = "Ҳа") %>% 
  mutate_at(vars(-district), as.double) %>% 
  arrange(desc(across(starts_with("Расмийлаштирилмаган")))) %>% 
  gt(rowname_col = "district") %>% 
  tab_header(title = md("**Туманлар (шаҳарлар) кесимида расмий меҳнат фаолияти билан банд бўлганлар ҳолати**")) %>% 
  cols_width(everything() ~ px(200)) %>%
  my_theme_gt() %>% 
  gtsave('formality.png', path = here("results", "tables", "buxoro"))    


#income groups
income <- buxoro_input_02 %>% filter(income != "Жавоб беришдан бош тортди") %>% 
  mutate(income = str_replace_all(income, " ", "")) %>%  
  mutate(income = as.double(income)) %>% 
  mutate(income_group = case_when(income == 0 ~ "Даромади мавжуд эмас",
                                  income >= 0 & income <= 1000000 ~ "1 млн сўмгача",
                                  income >= 1000001 & income <= 3000000 ~ "1-3 млн",
                                  income >= 3000001 ~ "3 млн сўмдан баланд")) %>% 
  select(district, income, income_group) %>% 
  tabyl(district, income_group) %>%
  adorn_percentages() %>% 
  mutate_at(vars(-district), as.double) %>% 
  select(district, "Даромади мавжуд эмас", "1 млн сўмгача", "1-3 млн", "3 млн сўмдан баланд") %>% 
  arrange(desc(across(starts_with("Даромади мавжуд эмас")))) %>% 
  gt(rowname_col = "district") %>% 
  tab_header(title = md("**Аҳоли ойлик даромадининг тақсимланиши**")) %>% 
  cols_width(everything() ~ px(150)) %>% 
  my_theme_gt() %>% 
  gtsave('income.png', path = here("results", "tables", "buxoro"))     