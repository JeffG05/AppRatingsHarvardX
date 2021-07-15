

train_set %>%
  group_by(Downloads) %>%
  summarise(count = n(), num = parse_number(str_replace(Downloads, "\\+", ""))) %>%
  ggplot(aes(reorder(Downloads, num), count)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

train_set %>%
  mutate(Downloads_Num = parse_number(str_replace(Downloads, "\\+", ""))) %>%
  ggplot(aes(Rating, Downloads_Num)) +
  geom_point() +
  geom_smooth() +
  scale_y_log10()

train_set %>%
  mutate(Downloads_Num = parse_number(str_replace(Downloads, "\\+", ""))) %>%
  ggplot(aes(Reviews, Downloads_Num)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_log10() +
  scale_x_log10()

train_set %>%
  mutate(Downloads_Num = parse_number(str_replace(Downloads, "\\+", "")),
         Price = parse_number(ifelse(Price == "Free", 0, Price))) %>%
  ggplot(aes(Price, Downloads_Num)) +
  geom_point() +
  geom_smooth() +
  scale_y_log10() +
  scale_x_log10()

train_set %>%
  mutate(Downloads_Num = parse_number(str_replace(Downloads, "\\+", "")),
         Price = parse_number(ifelse(Price == "Free", 0, Price))) %>%
  filter(str_starts(Downloads, "1")) %>%
  ggplot(aes(Reviews, Price, color = Downloads)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10()

train_set %>%
  mutate(Downloads_Num = parse_number(str_replace(Downloads, "\\+", ""))) %>%
  ggplot(aes(OS_Version_Required, Downloads_Num)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

train_set %>%
  mutate(Downloads_Num = parse_number(str_replace(Downloads, "\\+", ""))) %>%
  ggplot(aes(Content_Rating, Downloads_Num)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

train_set %>%
  mutate(Downloads_Num = parse_number(str_replace(Downloads, "\\+", ""))) %>%
  ggplot(aes(Category, Downloads_Num)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))