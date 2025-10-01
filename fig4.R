source("0 data.R")

fig4_oil <- read_excel("data/Figure 4 - Energy diversification/Oil diversification.xlsx",
                   range = "a1:l10") %>% 
  rename(country = 1) %>% 
  pivot_longer(-country) %>%
  mutate(name = as.numeric(name),
         type = "Oil") %>% 
  print()

fig4_gas <- read_excel("data/Figure 4 - Energy diversification/Gas diversification.xlsx",
                   range = "a1:l9") %>% 
  rename(country = 1) %>% 
  pivot_longer(-country) %>%
  mutate(name = as.numeric(name),
         type = "Gas") %>% 
  print()

fig4_coal <- read_excel("data/Figure 4 - Energy diversification/Coal diversification.xlsx",
                   range = "a1:l7") %>% 
  rename(country = 1) %>% 
  pivot_longer(-country) %>%
  mutate(name = as.numeric(name),
         type = "Coal") %>% 
  print()

fig4 <- bind_rows(fig4_coal, fig4_gas, fig4_oil) %>% 
  filter(!str_detect(country, "HHI")) %>% 
  mutate(country = ifelse(str_detect(country, "ther"), "Other", country))

fig4_hhi <- bind_rows(fig4_coal, fig4_gas, fig4_oil) %>% 
  filter(str_detect(country, "HHI")) %>% 
  mutate(country = ifelse(str_detect(country, "ther"), "Other", country)) %>% 
  mutate(country = ifelse(str_detect(country, "HHI"), "Diversification index (HHI)", country))

# check -------------------------------------------------------------------

fig4 %>% 
  ggplot() +
  aes(name, value) +
  geom_area(aes(fill = reorder(country, value),
                alpha = ifelse(country == "Russia", 1, .33)), 
            col = "#fff",
            position = "fill") +
  scale_alpha_identity() +
  facet_wrap(~type, nrow = 1)

# write -------------------------------------------------------------------

fig4 %>% 
  complete(country, name, type, fill = list(value = 0)) %>% 
  # mutate(value = ifelse(value == 0, "", value)) %>% 
  pivot_wider(names_from = country, values_from = value) %>% 
  # print()
  write_csv("data/fig4.csv")

fig4_hhi %>% 
  pivot_wider(names_from = country, values_from = value) %>% 
  # print()
  write_csv("data/fig4_hhi.csv")
