library(tidyverse)
library(ggwordcloud)
library(ggtext)
library(sysfonts)
library(showtext)
library(glue)


sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = "fontawesome-free-6.7.2-desktop/fontawesome-free-6.7.2-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf")
font_add("Gabriola", "Gabriola.ttf")
showtext::showtext_auto()

mastodon_icon <- "&#xf4f6"
mastodon_username <- "ceciliavives"
github_icon <- "&#xf09b"
github_username <- "cvives"


caption_text <- glue(
  "Font: Project Gutenberg.<br><span style='font-family:\"Font Awesome 6 Brands\";'>{mastodon_icon};</span>
  <span style='color: #000000'>{mastodon_username}</span> · <span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
  <span style='color: #000000'>{github_username}</span>"
)

gutenberg_metadata <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-03/gutenberg_metadata.csv')

gutenberg_cat <- gutenberg_metadata |> 
  filter(str_detect(language, "ca")) |>
  count(author) |> 
    mutate(
      author = fct(author)
    ) |>
  mutate(
    author = fct_recode(author,
                        "Jerome, Jerome K."    = "Jerome, Jerome K. (Jerome Klapka)",
                        "Andersen, H. C."      = "Andersen, H. C. (Hans Christian)",
                        "Givanel Mas, J." = "Givanel Mas, J. (Juan)",
                        "Plutarch, " = "Plutarch",
                        "Xenophon, " = "Xenophon",
                        "Erckmann-Chatrian, " = "Erckmann-Chatrian"
                       )
  )

gutenberg_cat2 <- gutenberg_cat |> 
  separate_wider_delim(
    author,
    delim = ",",
    names = c("cognom", "nom")
  ) |> 
mutate(etiqueta = str_c(nom, " ", cognom))

plot1 <- ggplot(gutenberg_cat2, aes(label = etiqueta, size = n,  color = factor(sample.int(10, nrow(gutenberg_cat2), replace = TRUE)))) +
  geom_text_wordcloud() +
  scale_radius(range = c(2, 6), limits = c(0, NA)) +
  theme_minimal()+
  labs(
    title = "<span style = 'font-family:Gabriola;'>Autors que es poden llegir en català al <br>Projecte Gutenberg</span>",
    caption = caption_text
  )+
  theme(plot.title = element_textbox_simple(size=18, halign=0.5, colour="#000080",  face="bold",
                                            margin=margin(10,0,0,0)),
        plot.caption = element_textbox_simple(size=10),
        plot.caption.position = "plot")


png(filename="gutenberg_cat.png", type="cairo", antialias="gray", width = 500, height = 400)
plot1
dev.off()

