library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(patchwork)
library(packcircles)
library(viridis)
library(showtext)

# font_add_google("Inter", "Inter")
# showtext_begin()

data <- read.csv("./data/Patient_Perspectives.csv")

packing <- circleProgressiveLayout(data$Counts, sizetype='area')
data <- cbind(data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)


wrap_text <- function(text, width = 10) {
  sapply(text, function(x) {
    paste(strwrap(x, width = width), collapse = "\n")
  }, USE.NAMES = FALSE)
}

data$theme_wrapped <- wrap_text(data$Theme, width = 30)


# Make the plot
Circles <- ggplot() + 
  
  # Make the bubbles
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), alpha = 0.6) +
  scale_fill_viridis(discrete = TRUE,
                     option = "viridis",
                     begin = 0.5 )+
  # Add text in the center of each bubble + control its size
  geom_text(data = data, aes(x, y,label = theme_wrapped)) +
  scale_size_continuous(range = c(1,4)) +
  
  # labs(title = bquote('Patient perspectives on'~ bold('challenges faced when understanding research and how ')))+

  # General theme:
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()

# showtext_end()
Circles
# ggsave("Figure5.1.png", Circles, width = 6, height = 7, dpi = 300)
