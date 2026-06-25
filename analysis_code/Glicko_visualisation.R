library(tidyverse)
library(directlabels)
library(gganimate)
library(ggimage)
# call in images for AFL teams
# call in images using png package
filenames <- list.files(file.path(here::here(), "images"), pattern = "*.png", full.names = TRUE)
myJSON <- lapply(filenames, function(x) readPNG(x))
teams <- str_split_fixed(filenames, '/', 8)[,8]
teams <- str_split_fixed(teams, '.png', 2)[,1]
names(myJSON) <- teams

link_to_img <- function(x, width = 50) {
  glue::glue("<img src='{x}' width='{width}'/>")
}

image.ref <- tibble(label = link_to_img(filenames),
       Team = teams)




p <- match %>% 
  arrange(Date) %>%
  filter(Date > as.Date("2021-01-01")) %>% 
  left_join(image.ref, by = 'Team') %>% 
  ggplot(aes(x = Date, y = value, color = Team)) +
  geom_line() +
  ggimage::geom_image(aes(x = Date, y = value, image = label))+
  geom_dl(aes(label = Team), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  scale_x_date() +
  scale_colour_manual(values = cols)+
  #facet_wrap(~Season, nrow = 1) +
  theme_AFL()+
  transition_time(Date) +
  labs(title = 'AFL team ratings at date: {frame_along}') +
  geom_point(aes(group = seq_along(Date))) +
  transition_reveal(Date) +
  ease_aes('linear')+
  view_follow()
  #view_zoom_manual(1,1, xmin = as.Date('2010-03-28'):as.Date('2010-10-10'), xmax = as.Date('2020-12-13'):as.Date('2021-06-27'), ymin = min(match$value), ymax = max(match$value), wrap = TRUE)

animate(p, renderer = ffmpeg_renderer(), height = 500, width = 800, 
        #fps = 10, 
        nframes = 200,
        duration = 50,
        end_pause = 60, res = 100)

anim_save("AFL_glicko.mp4")
