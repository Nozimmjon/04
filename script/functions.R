
  #gt_tables
  my_theme_gt <- function(data, ...) {
    data %>% 
  tab_stubhead(label = md("**Туманлар**")) %>% 
  #mutate_at(vars(everything()), as.numeric) %>% 
  fmt_percent(columns = everything()[-1], decimals = 0) %>% 
  grand_summary_rows(columns = everything(),
                     fns = list("Вилоят бўйича ўртача" = ~ mean(., na.rm = TRUE)), 
                     formatter = fmt_percent,  decimals = 0) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  ) %>% 
  cols_align(align = "center", columns = everything()) %>% 
  tab_options(
    heading.subtitle.font.size = 13,
    heading.align = "center",
    table.border.top.color = "white",
    column_labels.border.bottom.width= px(3),
  ) %>% 
  tab_style(
    style = cell_borders(sides = "top", 
                         color = "black", weight = px(3)),
    locations = cells_column_labels(everything())
  ) %>% 
  tab_style(
    style = cell_borders(sides = "top", 
                         color = "black", weight = px(3)),
    locations = cells_stubhead()) %>% 
  tab_style(
    style = cell_borders(sides = "bottom", 
                         color = "black", weight = px(3)),
    locations = cells_grand_summary()) %>% 
  tab_style(
    style = cell_borders(sides = "bottom", 
                         color = "black", weight = px(3)),
    locations = cells_stub_grand_summary()) %>% 
  tab_style(
    style = cell_text(weight = "bold", align = "left"),
    locations = cells_stub_grand_summary()
  ) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_grand_summary()
  ) 
  }
  
  
  #freq_tables
  reg_table <- function(data, column) {
    data %>% 
      tabyl(district, {{column}}) %>%
      adorn_totals() %>% 
      adorn_percentages("row") %>% 
      adorn_pct_formatting(rounding = "half up", digits = 0, affix_sign = FALSE)
  }
  
  
  