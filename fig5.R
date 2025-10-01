source("0 data.R")

fig5_ex <- read_excel("data/Figure 5 - Reciprocal share/China_s export - reciprocal share.xlsx",
                   range = "b5:r7") %>% 
  rename(dir = 1) %>%
  pivot_longer(-dir) %>%
  mutate(type = "Total exports") %>% 
  bind_rows(read_excel("data/Figure 5 - Reciprocal share/China_s export - reciprocal share.xlsx",
                       range = "b26:r28") %>% 
              rename(dir = 1) %>%
              pivot_longer(-dir) %>% 
              mutate(type = "Machines")) %>% 
  bind_rows(read_excel("data/Figure 5 - Reciprocal share/China_s export - reciprocal share.xlsx",
                       range = "b30:r32") %>% 
              rename(dir = 1) %>%
              pivot_longer(-dir) %>% 
              mutate(type = "Vehicles")) %>% 
  bind_rows(read_excel("data/Figure 5 - Reciprocal share/China_s export - reciprocal share.xlsx",
                       range = "b34:r36") %>% 
              rename(dir = 1) %>%
              pivot_longer(-dir) %>% 
              mutate(type = "Electric Machines")) %>% 
  mutate(name = as.numeric(name),
         type = str_to_sentence(type),
         cat = "Export") %>%
  print()

fig5_im <- read_excel("data/Figure 5 - Reciprocal share/China_s import - reciprocal share.xlsx",
                   range = "b1:r3") %>% 
  rename(dir = 1) %>%
  pivot_longer(-dir) %>%
  mutate(type = "Total imports") %>% 
  bind_rows(read_excel("data/Figure 5 - Reciprocal share/China_s import - reciprocal share.xlsx",
                       range = "b21:r23") %>% 
              rename(dir = 1) %>%
              pivot_longer(-dir) %>% 
              mutate(type = "Mineral fuels")) %>% 
  bind_rows(read_excel("data/Figure 5 - Reciprocal share/China_s import - reciprocal share.xlsx",
                       range = "b25:r27") %>% 
              rename(dir = 1) %>%
              pivot_longer(-dir) %>% 
              mutate(type = "Aluminium and articles thereof")) %>% 
  bind_rows(read_excel("data/Figure 5 - Reciprocal share/China_s import - reciprocal share.xlsx",
                       range = "b29:r31") %>% 
              rename(dir = 1) %>%
              pivot_longer(-dir) %>% 
              mutate(type = "Wood and articles")) %>% 
  bind_rows(read_excel("data/Figure 5 - Reciprocal share/China_s import - reciprocal share.xlsx",
                       range = "b33:r35") %>% 
              rename(dir = 1) %>%
              pivot_longer(-dir) %>% 
              mutate(type = "Copper and articles thereof")) %>% 
  bind_rows(read_excel("data/Figure 5 - Reciprocal share/China_s import - reciprocal share.xlsx",
                       range = "b37:r39") %>% 
              rename(dir = 1) %>%
              pivot_longer(-dir) %>% 
              mutate(type = "Raw precious metals")) %>% 
  mutate(name = as.numeric(name),
         type = str_to_sentence(type),
         cat = "Import") %>%
  print()

# check -------------------------------------------------------------------

fig5 <- bind_rows(fig5_im, fig5_ex)

fig5 %>% 
  # distinct(type) %>% 
  filter(str_detect(type, "Total")) %>%
  # print()
  ggplot() +
  aes(name, value) +
  geom_col(aes(fill = dir)) +
  facet_wrap(~cat)

# write -------------------------------------------------------------------

fig5 %>% 
  filter(cat == "Export") %>% 
  pivot_wider(names_from = dir, values_from = value) %>% 
  # mutate(label = ifelse(value < 0, -value, value)) %>%
  write_csv("data/fig5_exp.csv")

fig5 %>% 
  filter(cat == "Import") %>% 
  # mutate(label = ifelse(value < 0, -value, value)) %>% 
  pivot_wider(names_from = dir, values_from = value) %>% 
  write_csv("data/fig5_img.csv")
