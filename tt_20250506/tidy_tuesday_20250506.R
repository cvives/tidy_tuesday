
library(tidyverse)
library(sf)
library(spData)

nsf_terminations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-06/nsf_terminations.csv')
states_code <- readr::read_csv('us-state-ansi-fips.csv')

spending_bystate <- nsf_terminations |>
  group_by(org_state) |> 
  summarize(
    usaspending_obligated = sum(usaspending_obligated, na.rm = TRUE)
  ) |> 
  mutate(
    percentage = (usaspending_obligated / sum(usaspending_obligated, na.rm = TRUE)) * 100
  )

data_plot <- spending_bystate |>
  left_join(states_code,join_by(org_state == stusps))

data_plot <- us_states |>
  left_join(data_plot,join_by(GEOID == st)) 
  

  ggplot(data_plot) +
  geom_sf(aes(fill = percentage)) +
  geom_sf_text(aes(label = org_state), size=3,color="white")+
  theme_void() +
  scale_fill_gradient(
    low ="#56B1F7",
    high =  "#132B43",
    na.value = "grey50",
  )+
  labs(
    title = "What states are likely more affected by the termination of the NSF grants?",
    subtitle = "In April 2025, hundreds of grants were abruptly terminated.",
    caption = "Data: Grant Watch",
    fill = "% of funding"
  )

ggsave(
  filename = "termination_grants.png",
  height = 4.5,
  width = 8,
  bg="white",
  units = "in",
  dpi = 300
)
  