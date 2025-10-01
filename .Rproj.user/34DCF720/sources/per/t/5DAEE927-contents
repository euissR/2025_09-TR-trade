source("0 data.R")

fig1_balance <- read_excel("data/Figure 1 & 3 - General trade/New Microsoft Excel Worksheet.xlsx",
                         sheet = "general trade ",
                         range = "a1:s12") %>% 
  pivot_longer(`2009`:`2025`) %>% 
  rename(dir = flux,
         cat = Products) %>%
  mutate(cat = str_trim(cat),
         dir = str_to_sentence(dir)) %>% 
  filter(dir == "Trade balance") %>% 
  print()

fig1_total <- read_excel("data/Figure 1 & 3 - General trade/New Microsoft Excel Worksheet.xlsx",
                         sheet = "general trade ",
                         range = "a1:s12") %>% 
  pivot_longer(`2009`:`2025`) %>% 
  rename(dir = flux,
         cat = Products) %>%
  mutate(cat = str_trim(cat),
         dir = str_to_sentence(dir),
         # AC turned imports negative and made it bn
         value = ifelse(value < 0, -value, value)) %>% 
  filter(dir != "Trade balance") %>% 
  print()

fig1_export <- read_excel("data/Figure 1 & 3 - General trade/New Microsoft Excel Worksheet.xlsx",
                          sheet = "export",
                          range = "a1:r12") %>% 
  pivot_longer(`2009`:`2025`) %>% 
  rename(subcat = 1) %>% 
  mutate(cat = "Machinery & mechanical appliances") %>% 
  bind_rows(read_excel("data/Figure 1 & 3 - General trade/New Microsoft Excel Worksheet.xlsx",
                       sheet = "export",
                       range = "a17:r25") %>% 
              pivot_longer(`2009`:`2025`) %>% 
              rename(subcat = 1) %>% 
              mutate(cat = "Electrical machinery & equipment")) %>% 
  bind_rows(read_excel("data/Figure 1 & 3 - General trade/New Microsoft Excel Worksheet.xlsx",
                       sheet = "export",
                       range = "a31:r35") %>% 
              pivot_longer(`2009`:`2025`) %>% 
              rename(subcat = 1) %>% 
              mutate(cat = "Vehicles")) %>% 
  mutate(dir = "Export",
         cat = str_trim(cat),
         value = value / 1000) %>% 
  # rename(Export = value) %>% 
  print()

fig1_import <- read_excel("data/Figure 1 & 3 - General trade/New Microsoft Excel Worksheet.xlsx",
                          sheet = "import",
                          range = "a1:r5") %>% 
  pivot_longer(`2009`:`2025`) %>% 
  rename(subcat = 1) %>% 
  mutate(cat = "Mineral fuels & oil ",
         value = value * 1000) %>% # this one is in billion, randomly
  bind_rows(read_excel("data/Figure 1 & 3 - General trade/New Microsoft Excel Worksheet.xlsx",
                       sheet = "import",
                       range = "a15:r20") %>% 
              pivot_longer(`2009`:`2025`) %>% 
              rename(subcat = 1) %>% 
              mutate(cat = "Raw metals")) %>% 
  bind_rows(read_excel("data/Figure 1 & 3 - General trade/New Microsoft Excel Worksheet.xlsx",
                       sheet = "import",
                       range = "a30:r33") %>% 
              pivot_longer(`2009`:`2025`) %>% 
              rename(subcat = 1) %>% 
              mutate(cat = "Copper & articles thereof")) %>% 
  bind_rows(read_excel("data/Figure 1 & 3 - General trade/New Microsoft Excel Worksheet.xlsx",
                       sheet = "import",
                       range = "a46:r48") %>% 
              pivot_longer(`2009`:`2025`) %>% 
              rename(subcat = 1) %>% 
              mutate(cat = "Aluminium & articles thereof")) %>% 
  bind_rows(read_excel("data/Figure 1 & 3 - General trade/New Microsoft Excel Worksheet.xlsx",
                       sheet = "import",
                       range = "a61:r65") %>% 
              pivot_longer(`2009`:`2025`) %>% 
              rename(subcat = 1) %>% 
              mutate(cat = "Wood & articles of wood; charcoal")) %>% 
  mutate(dir = "Import",
         cat = str_trim(cat),
         value = value / 1000) %>% 
  # rename(Import = value) %>% 
  print()

fig1 <- fig1_export %>% 
  bind_rows(fig1_import) %>%  
  bind_rows(fig1_total %>% 
              filter(cat == "Other") %>% 
              mutate(subcat = "Other")) %>%  
  mutate(name = as.numeric(name)) %>% 
  print()

# check -------------------------------------------------------------------

fig1 %>% 
  # group_by(dir, name) %>% 
  # summarize(sum = sum(value, na.rm = TRUE)) %>%
  # ungroup() %>%
  # mutate(y = ifelse(str_detect(dir, "Export"), sum, -sum)) %>%
  ggplot() +
  aes(name, ifelse(str_detect(dir, "Export"), value, -value)) +
  geom_col(aes(fill = cat)) 
  # facet_wrap(~dir)

fig1_total %>% 
  # group_by(dir, name) %>% summarize(sum = sum(value)) %>%
  # ungroup() %>%
  # mutate(y = ) %>% 
  # print()
  ggplot() +
  aes(x = name, y = ifelse(str_detect(dir, "Export"), value, -value)) +
  # aes(x = name, y = sum) +
  geom_col(aes(fill = dir)) 
  # facet_wrap(~dir)


# write -------------------------------------------------------------------

# full by direction (flourish)
fig1 %>%
  #   mutate(value = ifelse(str_detect(dir, "Export"), value, -value)) %>% 
  #   mutate(value = ifelse(is.na(value), "", as.character(value))) %>% 
  #   pivot_wider(names_from = cat, 
  #               values_from = value,
  #               values_fill = "") %>% 
  group_by(dir, cat, name) %>% 
  summarize(value = sum(value)) %>%
  ungroup() %>% 
  # mutate(value = ifelse(str_detect(dir, "Export"), value, -value)) %>%
  pivot_wider(
    id_cols = c(name, dir),  # keep subcat, year, and direction as identifiers
    names_from = cat,                # categories become column names
    values_from = value,
    values_fill = 0,
    values_fn = sum
  ) %>% 
  # print()
  write_csv("data/fig1.csv")


fig1_balance %>% 
  mutate(col = value > 0) %>% 
  write_csv("data/fig1_balance.csv")
fig1 %>% 
  write_csv("data/fig1_breakdown.csv")
fig1_total %>% 
  write_csv("data/fig1_total.csv")
# for flourish
fig1_total %>% 
  group_by(dir, name) %>% 
  summarize(value = sum(value)) %>%
  ungroup() %>%
  pivot_wider(names_from = dir, 
              values_from = value,
              values_fill = 0) %>% 
  mutate(Export = -Export) %>% 
  # print()
  write_csv("data/fig1_total_dir.csv")
fig1_total %>% 
  mutate(value = replace_na(value, 0)) %>%
  pivot_wider(names_from = cat, 
              values_from = value,
              values_fill = 0) %>% 
  write_csv("data/fig1_total_wide.csv")
fig1_total %>% 
  filter(dir == "Import") %>% 
  pivot_wider(names_from = cat, values_from = value) %>% 
  write_csv("data/fig1_total_import.csv")
fig1_total %>% 
  filter(dir == "Export") %>% 
  pivot_wider(names_from = cat, values_from = value) %>% 
  # print()
  write_csv("data/fig1_total_export.csv")