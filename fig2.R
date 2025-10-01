source("0 data.R")

fig2 <- read_excel("data/Figure 2 - China_s export of cars to Russia/Cars_ export.xlsx",
                           range = "c1:bd2") %>% 
  pivot_longer(1:54,
               names_to = "date") %>%
  euiss_date_from_excel() %>% 
  print()

# check -------------------------------------------------------------------

fig2 %>% 
  ggplot() +
  aes(date, value) +
  geom_line()

# write -------------------------------------------------------------------

fig2 %>% 
  mutate(value = value / 1000) %>% 
  write_csv("data/fig2.csv")
