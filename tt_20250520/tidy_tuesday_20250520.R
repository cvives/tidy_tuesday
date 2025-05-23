
library(tidyverse)
library(ggtext)

water_quality <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-20/water_quality.csv')
weather <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-20/weather.csv')

meanbacteria <- water_quality |> 
  group_by(date)|> 
  summarise(
    bacteria = mean(enterococci_cfu_100ml,na.rm=T)
  )

mergeddf <- left_join(meanbacteria, weather)

mergeddf2 <- mergeddf |>
  filter(date > "2018-12-31")|>
  filter(date < "2025-01-01")|>
  mutate(
    month = floor_date(date, "month")
  ) |>
  group_by(month)|>
  summarize(
    rain=sum(precipitation_mm,na.rm=T),
    bacteria=mean(bacteria,na.rm=T)
  )

mean_rain  <- mean(mergeddf2$rain,na.rm=T)

mergeddf2 <- mergeddf2 |>
  mutate(
    rainy=if_else(rain > mean_rain,T,F),
    year = floor_date(month, "year"),
    year=year(year)
  ) |>
  group_by(year, rainy) |>
  summarize(
    bacteria_rainy = mean(bacteria,na.rm=T),
    rain_rainy = mean(rain,na.rm=T)
  )
  

plot1 <- mergeddf2|> 
  ggplot(aes(x=factor(year), y=bacteria_rainy, fill=rainy)) +
  geom_col(position = "dodge")+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values = c("#FC8D62","#8DA0CB"))+
  theme_minimal()+
  labs(
    title = "Water Quality at Sidney Beaches on <span style = 'color:#8DA0CB;'>rainy months</span> and <span style = 'color:#FC8D62;'>non-rainy months</span>",
    subtitle = "Quality measured as Enterococci bacteria levels. Rainy months are those with rain levels above the 2019-2024 average.",
    y="Enterococci (cfu/100ml)",
    caption = "Source: Beachwatch and Open Meteo"
  )+
  theme(
    plot.title = element_markdown(
      size=14,
      face="bold",
      margin=margin(5,5,2,0)
    ),
    plot.subtitle = element_textbox_simple(
      size=12,
      margin=margin(2,0,10,0)
    ),
    axis.title.x = element_blank(),
    plot.caption = element_textbox_simple(
      size=11, 
      margin=margin(10,5,0,0)
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(0.2, 0.2, 0.5, 0.2, "cm"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

png(filename="Sidney_beaches.png", type="cairo", antialias="gray", width = 500, height = 350)
plot1
dev.off()
  







