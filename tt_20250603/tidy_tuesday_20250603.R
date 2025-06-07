library(tidyverse)
library(ggtext)
library(sysfonts)
library(showtext)
library(glue)
library(wordcloud2)
library(webshot2)
library(htmlwidgets)
library(cowplot)
library(magick)


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

gutenberg_cat2 <- gutenberg_cat2 |> 
  select(
    word = etiqueta,
    freq= n)|> 
  filter(!is.na(word))

#I transform the frequency to just 2 values, otherwise, the words with higher
#freq are huge and the ones with less frequency too small. I have not found a 
#way to properly rescaling the sizes, rescaling the frequencies does not work.

gutenberg_cat2 <- gutenberg_cat2 |> 
  mutate(
    freq=if_else(freq>1,2,1)
  )

plot_word <- wordcloud2(gutenberg_cat2, color = "random-light", fontWeight="normal", size=0.15, rotateRatio=0, ellipticity = 0.5)

saveWidget(plot_word,"tmp.html",selfcontained = F)

webshot("tmp.html","fig_1.png", delay =5, vwidth = 550, vheight=330)

#after learning to place the image down in the figure, cutting the bottom
#is not necessary anymore, but I have kept it like that
img <- image_read("fig_1.png")
img2 <- image_crop(img, "550x310+0+0")
image_write(img2, path = "fig_t.png", format = "png")

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


plotgg <- ggplot() +
  theme_void() +
  labs(
    title = "Autors que es poden llegir en català al <br>Projecte Gutenberg<br> ",
    caption = caption_text,
    subtitle = " " 
  )+
  theme(plot.title = element_textbox_simple(size=22, halign=0.5, colour="#000080",  face="bold",
                                            margin=margin(20,0,10,0), family = "Gabriola"),
        plot.caption = element_textbox_simple(size=10, margin=margin(0,0,10,10)),
        plot.caption.position = "plot")

img <- "fig_t.png"

plotf <- ggdraw() +
  draw_image(img, scale=0.9, valign = 0.4)+
  draw_plot(plotgg)


png(filename="gutenberg_cat9.png", type="cairo", antialias="gray", width = 550, height = 400)
plotf
dev.off()


