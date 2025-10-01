source("0 data.R")

fig6 <- read_excel("data/Figure 6 - Russia dependencies on China/dependencies.xlsx",
                       range = "a1:o5") %>% 
  rename(type = 1) %>%
  pivot_longer(-type) %>%
  mutate(name = as.numeric(name),
         type = str_to_sentence(type)) %>%
  print()

# check -------------------------------------------------------------------

fig6 %>% 
  ggplot() +
  aes(name, value) +
  geom_line(aes(col = type)) 

# write -------------------------------------------------------------------

fig6 %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  write_csv("data/fig6.csv")
