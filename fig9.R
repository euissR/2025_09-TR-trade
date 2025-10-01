source("0 data.R")

fig9 <- read_excel("data/Figure 9 - Export of dual use goods/Export of dual use goods.xlsx",
                         range = "a1:l7") %>% 
  rename(type = 1) %>%
  pivot_longer(-type) %>%
  mutate(name = as.numeric(name)) %>%
  arrange(desc(value)) %>% 
  print()

# check -------------------------------------------------------------------

fig9 %>% 
  ggplot() +
  aes(name, value) +
  geom_area(aes(fill = reorder(type, desc(value)))) +
  facet_wrap(~type)

# write -------------------------------------------------------------------

fig9 %>% 
  pivot_wider(names_from = type, values_from = value) %>%
  # print()
  # mutate(level = ifelse(is.na(level), lag(level), level))
  write_csv("data/fig9.csv", na = "")
