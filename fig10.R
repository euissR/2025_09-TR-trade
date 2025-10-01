source("0 data.R")

fig10 <- read_excel("data/Figure 10 - EU share of certain goods/EU share of selected goods.xlsx",
                    range = "a2:h8") %>% 
  rename(type = 1) %>%
  pivot_longer(-type) %>%
  mutate(name = as.numeric(name)) %>%
  print()

# check -------------------------------------------------------------------

fig10 %>% 
  ggplot() +
  aes(name, value) +
  geom_col(aes(fill = name), position = "dodge") +
  facet_wrap(~type, nrow = 1)

# write -------------------------------------------------------------------

fig10 %>% 
  pivot_wider(names_from = type, values_from = value) %>%
  write_csv("data/fig10.csv")
