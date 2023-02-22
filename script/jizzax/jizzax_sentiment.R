
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







