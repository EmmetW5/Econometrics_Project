# OLD CODE I MIGHT NEED LATER
permit_values = read_xlsx("building_permits.xlsx") %>%
  rename_with(make.names, unique = TRUE) %>%
  select(State.Name, Survey.Date, ends_with("rep")) %>%
  rename_with(\(n) str_extract(n, ".+[^(\\.rep)]")) %>%
  group_by(Survey.Date) %>%
  summarize(units = sum(Single.family.units),
            value = sum(Single.family.value)) %>%
  mutate(average_value = value / units,
         date_string = str_c("01/01/", Survey.Date),
         date = mdy(date_string))