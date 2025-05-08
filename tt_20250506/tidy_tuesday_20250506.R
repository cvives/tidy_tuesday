
library(tidyverse)
library(sf)
library(rnaturalearth)
library(cowplot)

nsf_terminations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-06/nsf_terminations.csv')
usa_sf = ne_states(country = "United States of America", returnclass = "sf")

spending_bystate <- nsf_terminations |>
  group_by(org_state) |> 
  summarize(
    usaspending_obligated = sum(usaspending_obligated, na.rm = TRUE)
  ) |> 
  mutate(
    percentage = (usaspending_obligated / sum(usaspending_obligated, na.rm = TRUE)) * 100
  )

data_plot <- usa_sf |>
  left_join(spending_bystate,join_by(postal == org_state)) 

data_plot1 <- data_plot |> 
  filter(name != "Hawaii")  |>  
  filter(name != "Alaska")

data_plot2 <- data_plot |> 
  filter(name == "Alaska")

data_plot3 <- data_plot |> 
  filter(name == "Hawaii")

plot1 <- ggplot(data_plot1) +
  geom_sf(aes(fill = percentage)) +
  geom_sf_text(aes(label = postal), size=3, color="white") + 
  coord_sf(
    crs = st_crs(9311), 
    xlim = c(-2500000, 2500000), 
    ylim = c(-2300000, 730000)
  ) +
  theme_void() +
  scale_fill_gradient(
    low ="#56B1F7",
    high =  "#132B43",
    na.value = "grey50"
  ) +
  labs(
    title = "What states are likely more affected by the termination of the NSF grants?",
    subtitle = "In April 2025, hundreds of grants were abruptly terminated.",
    caption = "Data: Grant Watch",
    fill = "% of funding"
  )
 
plot2 <- ggplot(data_plot2) + 
  geom_sf(aes(fill = percentage)) +
  geom_sf_text(aes(label = postal), size=3, color="white") + 
  coord_sf(
    crs = st_crs(3467), 
    xlim = c(-2400000, 1600000), 
    ylim = c(200000, 2500000), 
    expand = FALSE
  ) +
  theme_void() + 
  scale_fill_gradient( 
    low ="#56B1F7",
    high =  "#132B43",
    na.value = "grey50"
  ) + 
  theme(legend.position = "none", 
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
  )
 
plot3 <- ggplot(data_plot3) + 
  geom_sf(aes(fill = percentage)) +
  geom_sf_text(aes(label = postal), size=3, color="white") +
  coord_sf(crs = st_crs(4135), 
    xlim = c(-161, -154), 
    ylim = c(18, 23), 
    expand = FALSE
  ) +
  theme_void() +
  scale_fill_gradient(
    low ="#56B1F7",
    high =  "#132B43",
    na.value = "grey50"
  ) +
  theme(legend.position = "none", 
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
  )
 
ratioAlaska <- (2500000 - 200000) / (1600000 - (-2400000))
ratioHawaii  <- (23 - 18) / (-154 - (-161))
 
ggdraw(plot1) +
  draw_plot(plot2, width = 0.18, height = 0.18 * 10/6 * ratioAlaska, x = 0.10, y = 0.05) +
  draw_plot(plot3, width = 0.11, height = 0.11 * 10/6 * ratioHawaii, x = 0.28, y = 0.05)

ggsave(
  filename = "termination_grants.png",
  height = 4.5,
  width = 8,
  bg="white",
  units = "in",
  dpi = 300
)
