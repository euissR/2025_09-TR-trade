source("0 data.R")

fig12 <- read_excel("data/Figure 12 - Secondary Sanctions/Secondary sanctions.xlsx",
                    range = "a1:c4") %>% 
  rename(date = 1) %>%
  pivot_longer(-date) %>%
  print()

# check -------------------------------------------------------------------

fig12 %>% 
  ggplot() +
  aes(name, ymax = value) +
  geom_linerange(aes(ymin = 0, col = name), 
                 position = "dodge") +
  facet_wrap(~date, nrow = 1)

# write -------------------------------------------------------------------

fig12 %>% 
  # pivot_wider(names_from = name, values_from = value) %>%
  write_csv("data/fig12.csv")
