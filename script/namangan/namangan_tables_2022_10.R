
#Table 1 

namangan_input_02 %>%
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
  gtsave('1_savol.png', path = here("results", "tables", "namangan"))

#table 3
namangan_input_02 %>%
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
  gtsave('3_savol.png', path = here("results", "tables", "namangan"))

#table 5
namangan_input_02 %>%
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
  gtsave('5_savol.png', path = here("results", "tables", "namangan"))

#table 2
namangan_input_02 %>%
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
  gtsave('2_savol.png', path = here("results", "tables", "namangan"))

#table 4
namangan_input_02 %>%
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
  gtsave('4_savol.png', path = here("results", "tables", "namangan"))


#table 6
namangan_input_02 %>%
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
  gtsave('6_savol.png', path = here("results", "tables", "namangan"))

#table 7

# table_7 <- namangan_input_02 %>% add_count(district) %>% 
#   separate_rows(q_7, sep = ",") %>%
#   mutate(q_7 = str_trim(q_7)) %>% 
#   count(district, n,  q_7) %>% 
#   mutate(freq = 100*nn/n) %>% 
#   select(-n, -nn) %>% 
#   pivot_wider(names_from = q_7, values_from = freq) 
# 
# write_xlsx(table_7, "muammolar_namangan.xlsx")

#table 8

#table 11 mahalliy organlarni baholash 

namangan_input_02 %>%
  tabyl(district, q_12) %>%
  adorn_percentages() %>% 
  select(district, "Жуда ёмон", "Ёмон", "Ўртача", "Яхши", "Жуда яхши") %>% 
  mutate_at(vars(-district), as.double) %>% 
  arrange(desc(across(starts_with("Жуда ёмон")))) %>% 
  gt(rowname_col = "district") %>% 
  tab_header(title = md("**Маҳаллий ҳокимият органи фаолиятини қандай баҳолайсиз?**"),
             subtitle = md("(*Респондентларнинг жавоблари)*")) %>% 
  cols_width(everything() ~ px(120)) %>% 
  my_theme_gt() %>% 
  gtsave('mahalliy_organ.png', path = here("results", "tables", "namangan"))

# table 8 uy isitish manbalari

namangan_input_02 %>%
  mutate(q_8 = str_replace_all(q_8, "(электропечка, пушка, кондиционер ва х.к.)", "")) %>% 
  mutate(q_8 = str_replace_all(q_8, "(тёплый пол, АГВ ёки котёл)", "")) %>% 
  mutate(q_8 = str_replace_all(q_8, "Апилка билан", "опилка")) %>% 
  mutate(q_8 = str_replace_all(q_8, "ОПИЛЬКА", "опилка")) %>% 
  add_count(district) %>% 
  separate_rows(q_8, sep = ",") %>%
  mutate(q_8 = str_trim(q_8)) %>% 
  mutate(q_8 = recode(q_8, 
                      "Кўмир ва кўмир брикетлари" = "Кўмир ва кўмир брикетлари",
                      "Суюлтирилган газ (баллон)" = "Суюлтирилган газ (баллон)",
                      "Марказлашган иссиқлик тизими (Центральное отопление)" = "Марказлашган иссиқлик тизими",
                      "Ўтин" = "Ўтин",
                      "Электр таъминоти ()" = "Электр таъминоти",
                      "Табиий газ таъминоти ( )" = "Табиий газ таъминоти",
                      .default = "Бошқа")) %>% 
  count(district, n, q_8) %>% 
  mutate(freq = nn/n) %>% 
  select(-n, -nn) %>% 
  pivot_wider(names_from = q_8, values_from = freq, values_fill = 0) %>% 
  mutate_at(vars(-district), as.double) %>%
  #arrange(desc(across(starts_with("Жуда ёмон")))) %>%
  gt(rowname_col = "district") %>%
  tab_header(title = md("**Уйингизни иситиш учун асосан қандай манбалардан фойдаланасиз?**"),
             subtitle = md("(*Респондентларнинг жавоблари)*")) %>%
  cols_width(everything() ~ px(120)) %>%
  my_theme_gt() %>%
  gtsave('иситиш_манбалари.png', path = here("results", "tables", "namangan"), vwidth = 1500, vheight = 1000)

#table 10 qishga tayyorlik

namangan_input_02 %>%
  tabyl(district, q_10) %>%
  adorn_percentages() %>%
  select(district, "Ҳа, тўлиқ тайёр",  
         "Тайёр эмас",
         "Қисман"
  ) %>%
  mutate_at(vars(-district), as.double) %>%
  arrange(desc(across(starts_with("Ҳа, тўлиқ тайёр")))) %>%
  gt(rowname_col = "district") %>%
  tab_header(title = md("**Уй хўжалигингиз куз-қиш мавсумига тайёрми?**"),
             subtitle = md("(*Респондентларнинг жавоблари)*")) %>%
  cols_width(everything() ~ px(120)) %>%
  my_theme_gt() %>%
  gtsave('winter_readiness.png', path = here("results", "tables", "namangan"), vwidth = 1500, vheight = 1000)

# isitish bilan bogliq muammolar 

namangan_input_02 %>%
  tabyl(district, q_11) %>%
  adorn_percentages() %>%
  mutate_at(vars(-district), as.double) %>%
  gt(rowname_col = "district") %>%
  tab_header(title = md("**Куз-қиш мавсумида иситиш билан боғлиқ энг катта муаммо**"),
             subtitle = md("(*Респондентлар жавоблари)*")) %>%
  cols_width(everything() ~ px(125)) %>%
  my_theme_gt() %>%
  gtsave('winter_readiness_2.png', path = here("results", "tables", "namangan"), vwidth = 1500, vheight = 1000)

#table 8 hokim yordamchisi

#namangan_input_02 %>%
#  tabyl(district, q_8) %>%
# adorn_percentages() %>%
#  select(district, "Танимайман",  
#        "Ҳеч қандай ёрдам бергани йўқ", 
#       "Имтиёзли кредит олишда кўмаклашди", 
#        "Ишга жойлаштиришга ёрдам берди",
#         "Субсидия, грант ва моддий ёрдам тақдим этилди",
#         "Ер ажратишда ёрдам берди", 
#         "Танийман, лекин ёрдамга зарурият йўқ") %>%
#  mutate_at(vars(-district), as.double) %>%
#  #arrange(desc(across(starts_with("Жуда ёмон")))) %>%
#  gt(rowname_col = "district") %>%
#  tab_header(title = md("**Ҳоким ёрдамчиси томонидан кўрсатилган ёрдам ҳолати**"),
#             subtitle = md("(*Респондентларнинг жавоблари)*")) %>%
#  cols_width(everything() ~ px(120)) %>%
#  my_theme_gt() %>%
#  gtsave('hokim_yordamchisi.png', path = here("results", "tables", "namangan"))

#table 9 yoshlar yetakchisi faoli

#namangan_input_02 %>%
# filter(age <= "30") %>% 
#  tabyl(district, q_9) %>%
#  adorn_percentages() %>%
#  select(district, "Танимайман",  
#         "Ёрдам олганман",
#         "Фаолиятидан хабардорман, лекин ёрдам олмаганман", 
#         "Танийман, лекин ёрдамга зарурият йўқ") %>%
#  mutate_at(vars(-district), as.double) %>%
#  #arrange(desc(across(starts_with("Жуда ёмон")))) %>%
#  gt(rowname_col = "district") %>%
#  tab_header(title = md("**Ёшлар етакчиси фаолияти билан танишмисиз?**"),
#             subtitle = md("(*Респондентларнинг жавоблари)*")) %>%
#  cols_width(everything() ~ px(120)) %>%
#  my_theme_gt() %>%
#  gtsave('yoshlar_yetakchisi.png', path = here("results", "tables", "namangan"))


#table 10 ayollar faoli

#namangan_input_02 %>%
#  filter(gender == "Аёл") %>% 
#  tabyl(district, q_10) %>%
#  adorn_percentages() %>%
#  select(district, "Танимайман",  
#         "Ёрдам олганман",
#         "Фаолиятидан хабардорман, лекин ёрдам олмаганман", 
#         "Танийман, лекин ёрдамга зарурият йўқ") %>%
#  mutate_at(vars(-district), as.double) %>%
#  #arrange(desc(across(starts_with("Жуда ёмон")))) %>%
#  gt(rowname_col = "district") %>%
#  tab_header(title = md("**Хотин-қизлар фаоли билан танишмисиз?**"),
#             subtitle = md("(*Респондентларнинг жавоблари)*")) %>%
#  cols_width(everything() ~ px(120)) %>%
#  my_theme_gt() %>%
#  gtsave('ayollar_yetakchisi.png', path = here("results", "tables", "namangan"))

#table ishsizlik

namangan_input_02 %>% 
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
  gtsave('ishsizlik.png', path = here("results", "tables", "namangan"))  

#gender
namangan_input_02 %>% 
  filter(is_working != "Пенсиядаман") %>% 
  tabyl(gender, is_working) %>% 
  adorn_percentages()

#formality

namangan_input_02 %>% 
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
  gtsave('formality.png', path = here("results", "tables", "namangan"))    


#income groups
namangan_input_02 %>% filter(income != "Жавоб беришдан бош тортди") %>% 
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
  gtsave('income.png', path = here("results", "tables", "namangan"))     

