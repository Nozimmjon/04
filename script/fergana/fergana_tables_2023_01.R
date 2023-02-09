
#Table 1 

fergana_input_02 %>%
  tabyl(district, q_1) %>%
  adorn_percentages() %>% 
  select(district, "Ёмонлашади", "Ўзгармайди", "Яхшиланади") %>% 
  mutate_at(vars(-district), as.numeric) %>% 
  arrange(desc(across(starts_with("Ёмонлашади")))) %>% 
  gt(rowname_col = "district") %>% 
  tab_header(title = md("*1-савол.* **3 ойдан сўнг Ўзбекистонда <span style='color:red'>иқтисодий ҳолат</span> қандай ўзгаради?**"),
             subtitle = md("(*Респондентларнинг жавоблари*)")) %>% 
  fmt_percent(columns = c(2:4), decimals = 0) %>% 
  my_theme_gt() %>% 
  cols_width(everything() ~ px(180)) %>% 
  gtsave('1_savol.png', path = here("results", "tables", "fergana"))

#table 3
fergana_input_02 %>%
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
  gtsave('3_savol.png', path = here("results", "tables", "fergana"))

#table 5
fergana_input_02 %>%
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
  gtsave('5_savol.png', path = here::here("results", "tables", "fergana"))

#table 2
fergana_input_02 %>%
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
  gtsave('2_savol.png', path = here("results", "tables", "fergana"))

#table 4
fergana_input_02 %>%
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
  gtsave('4_savol.png', path = here("results", "tables", "fergana"))


#table 6
fergana_input_02 %>%
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
  gtsave('6_savol.png', path = here("results", "tables", "fergana"))


#table 12 mahalliy organlarni baholash 

fergana_input_02 %>%
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
  gtsave('mahalliy_organ.png', path = here("results", "tables", "fergana"))

# table 8 uy isitish manbalari

fergana_input_02 %>%
  mutate(q_8 = str_replace_all(q_8, "(тез тиббий ёрдам, поликлиника, шифохоналар ва дорихоналар мавжудлиги)", "")) %>% 
  add_count(district) %>% 
  separate_rows(q_8, sep = ",") %>%
  mutate(q_8 = str_trim(q_8)) %>% 
  mutate(q_8 = recode(q_8, 
                      "Соғлиқни сақлаш хизматлари сифатини ошириш борасидаги ишлар" = "Соғлиқни сақлаш сифатини ошириш",
                      "Мактабгача таълим тизими билан қамров даражасини ошириш борасидаги ишлар" = "Мактабгача таълим тизими билан қамровни ошириш",
                      "Мактабда таълим сифатини ошириш борасидаги ишлар" = "Мактабда таълим сифатини ошириш",
                      "Аҳолини уй-жой билан таъминлаш ишларини" = "Аҳолини уй-жой билан таъминлаш",
                      "Маҳаллабай ишлаш ва ҳоким ёрдамчилари тизимидаги ишларни" = "Маҳаллабай ишлаш ва ҳоким ёрдамчилари ",
                      "Ўқитувчиларни қўллаб-қувватлаш ишларини" = "Ўқитувчиларни қўллаб-қувватлаш",
                      "Шифокорларни қўллаб-қувватлаш ишларини" = "Шифокорларни қўллаб-қувватлаш",
                      "Коррупцияга қарши курашишлар" = "Коррупцияга қарши курашиш",
                      "Камбағаликка қарши кураш ишларини" = "Камбағаликка қарши кураш",
                      "Олий таълим тизимидаги ислоҳотларни" = "Олий таълим тизимидаги ислоҳотларни",
                      "Тадбиркорларни қўллаб-қувватлаш ишларини" = "Тадбиркорларни қўллаб-қувватлаш",
                      "Автомобил сотиб олиш жараёнларини" = "Автомобил сотиб олиш жараёнларини",
                      "Газ таъминоти соҳаси" = "Газ таъминоти",
                      "Иш билан таъминлаш соҳаси" = "Бандликни таъминлаш",
                      "Электр таъминоти соҳаси" = "Электр таъминоти",
                      .default = "Бошқа")) %>% 
  count(district, n, q_8) %>% 
  mutate(freq = nn/n) %>% 
  select(-n, -nn) %>% 
  pivot_wider(names_from = q_8, values_from = freq, values_fill = 0) %>% 
  mutate_at(vars(-district), as.double) %>%
  #arrange(desc(across(starts_with("Жуда ёмон")))) %>%
  gt(rowname_col = "district") %>%
  tab_header(title = md("**8.	Қайси соҳадаги ислоҳотларни тезлаштириш керак деб ўйлайсиз?**"),
             subtitle = md("(*Респондентларнинг жавоблари)*")) %>%
  cols_width(everything() ~ px(120)) %>%
  my_theme_gt() %>%
  gtsave('islohotlar_tezlash.png', path = here("results", "tables", "fergana"), vwidth = 1500, vheight = 1000)

#table 10 qishga tayyorlik


#table ishsizlik

fergana_input_02 %>% 
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
  gtsave('ishsizlik.png', path = here("results", "tables", "fergana"))  

#gender
fergana_input_02 %>% 
  filter(is_working != "Пенсиядаман") %>% 
  tabyl(gender, is_working) %>% 
  adorn_percentages()

#formality

fergana_input_02 %>% 
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
  gtsave('formality.png', path = here("results", "tables", "fergana"))    


#income groups
fergana_input_02 %>% filter(income != "Жавоб беришдан бош тортди") %>% 
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
  gtsave('income.png', path = here("results", "tables", "fergana"))    
