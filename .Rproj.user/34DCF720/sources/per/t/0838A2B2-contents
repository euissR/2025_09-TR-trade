source("0 data.R")

fig8_value <- read_excel("data/Figure 7 & 8 - Typology and volume of imported weapons/typology and volume of imported weapons.xlsx",
                   range = "a1:z7") %>% 
  rename(type = 1) %>%
  pivot_longer(-type) %>%
  mutate(name = as.numeric(name),
         type = str_to_sentence(type)) %>%
  drop_na(value) %>% 
  mutate(type = case_match(
    type,
    "Fga aircraft" ~ "✈️ FGA aircraft",
    "Sam" ~ "🚀 SAM",
    "Sam system" ~ "🚀 SAM",
    "Submarine" ~ "⚓ Submarine",
    "Transport helicopter" ~ "🚁 Transport helicopter",
    "Turbofan" ~ "🛞 Turbofan",
    .default = type
  )) %>% 
  print()

fig8_years <- read_excel("data/Figure 7 & 8 - Typology and volume of imported weapons/Sophistication of imported weapons.xlsx",
                   range = "a19:z31") %>% 
  rename(type = 1) %>%
  pivot_longer(-type, values_to = "model") %>%
  mutate(name = as.numeric(name),
         type = str_to_sentence(type)) %>%
  drop_na(model) %>% 
  mutate(type = case_match(
    type,
    "Fga aircraft" ~ "✈️ FGA aircraft",
    "Sam" ~ "🚀 SAM",
    "Sam system" ~ "🚀 SAM",
    "Submarine" ~ "⚓ Submarine",
    "Transport helicopter" ~ "🚁 Transport helicopter",
    "Turbofan" ~ "🛞 Turbofan",
    .default = type
  )) %>% 
  print()

fig8_soph <- read_excel("data/Figure 7 & 8 - Typology and volume of imported weapons/Sophistication of imported weapons.xlsx",
                   range = "ab19:ac37") %>% 
  rename(model = 1, level = 2) %>% 
  print()

fig8 <- fig8_years %>% 
  left_join(fig8_soph, by = "model") %>% 
  left_join(fig8_value, by = c("type", "name")) %>% 
  # filter(str_detect(model, "Treu")) %>% print()
  mutate(type = case_match(
    type,
    "Fga aircraft" ~ "✈️ FGA aircraft",
    "Sam" ~ "🚀 SAM",
    "Sam system" ~ "🚀 SAM",
    "Submarine" ~ "⚓ Submarine",
    "Transport helicopter" ~ "🚁 Transport helicopter",
    "Turbofan" ~ "🛞 Turbofan",
    .default = type
  ),
  level = as.character(level)) %>% 
  # level = paste0(level, " ")) %>% 
  print()
  
# check -------------------------------------------------------------------

fig8 %>% 
  ggplot() +
  aes(name, reorder(model, type)) +
  geom_point(aes(size = value, col = level)) +
  facet_wrap(~type, ncol = 1, scales = "free_y")

# write -------------------------------------------------------------------

fig8_value %>% 
  group_by(name) %>% 
  summarize(value = sum(value)) %>%
  write_csv("data/fig8-1total.csv")
  
fig8_value %>% 
  write_csv("data/fig8-2.csv")

fig8 %>% 
  arrange(level) %>% 
  filter(type == "🛞 Turbofan") %>% 
  mutate(type = "Turbofan",
         level = case_match(
           level, 
           "1" ~ "★☆☆☆☆",
           "2" ~ "★★☆☆☆",
           "3" ~ "★★★☆☆",
           "4" ~ "★★★★☆",
           "5" ~ "★★★★★"
         )) %>% 
  # pull(level) %>% range()
  # print()
  # pivot_wider(names_from = model, values_from = value) %>%
  # print()
  # mutate(level = ifelse(is.na(level), lag(level), level))
  write_csv("data/fig8.csv", na = "")
