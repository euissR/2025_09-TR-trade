source("0 data.R")

fig11 <- read_excel("data/Figure 11 - EU share on Russia_s export/EU imp on Russia exp.xlsx",
                   range = "a1:h7") %>% 
  rename(type = 1) %>%
  pivot_longer(-type) %>%
  mutate(name = as.numeric(name)) %>%
  print()

fig11_types <- fig11 %>% 
  distinct(type) %>% pull()

# check -------------------------------------------------------------------

fig11 %>% 
  ggplot() +
  aes(name, value) +
  geom_col(aes(fill = name), position = "dodge") +
  facet_wrap(~type, nrow = 1)

# write -------------------------------------------------------------------

fig11 %>% 
  pivot_wider(names_from = type, values_from = value) %>%
  write_csv("data/fig11.csv")

fig1011 <- fig10 %>% 
  mutate(dir = "EU share") %>% 
  bind_rows(fig11 %>% 
              mutate(dir = "China's export to Russia as share of EU import")) %>% 
  # print()
  write_csv("data/fig1011.csv")
