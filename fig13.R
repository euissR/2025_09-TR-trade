source("0 data.R")

fig13 <- read_excel("data/Figure 13 - UN Agreement/UNGA Agreement score.xlsx",
                    range = "a1:aa2") %>% 
  select(-1) %>%
  pivot_longer(1:26, values_to = "UNGA") %>%
  mutate(name = as.numeric(name)) %>% 
  left_join(read_excel("data/Figure 13 - UN Agreement/UNSC Agreement score.xlsx",
                       range = "a1:aa2") %>% 
              select(-1) %>% 
              pivot_longer(1:26, values_to = "UNSC") %>%
              mutate(name = as.numeric(name)),
                     by = "name") %>% 
  print()

# check -------------------------------------------------------------------

fig13 %>% 
  ggplot() +
  aes(name, value) +
  geom_line(aes(col = type))

# write -------------------------------------------------------------------

fig13 %>% 
  # print()
  # pivot_wider(names_from = type, values_from = value) %>%
  write_csv("data/fig13.csv")
