## Are your cats indoor, outdoor, or indoor/outdoor?
## Image by Gary Ross on Pixabay https://pixabay.com/images/id-659426/

library(dplyr)
library(magrittr)
library(ggplot2)
library(ggthemes)
library(magick)
library(ggtextures)
library(ggimage)

## Get data
indoor_outdoor <-
  data.frame(response = c("indoor cats", "indoor/outdoor cats", "outdoor cats"),
             votes = c(23, 1, 4)) %>%
  dplyr::mutate(prop = votes / sum(votes, na.rm = TRUE)) %>%
  dplyr::mutate(perc = paste0(round(x = prop * 100, digits = 0), "%")) %>%
  dplyr::mutate(label = paste0(response, ":\n", perc),
                x = c(0.5, 6.1, 6.5),
                y = c(1, 2.5, 2),
                colour = c("white", "black", "black"))

response_rate <- 
  data.frame(resp = sum(indoor_outdoor$votes, na.rm = TRUE),
             total = 286) %>%
  dplyr::mutate(resp_rate = resp / total) %>%
  dplyr::mutate(resp_perc = paste0(round(x = resp_rate * 100, digits = 2), "%"))

image <- "img/window_cat.jpg"


## Make plot
g <- 
  ggplot2::ggplot(data = indoor_outdoor, 
                 mapping = ggplot2::aes(x = x, 
                                        y = y,
                                        label = label,
                                        colour = label)) +
  ggplot2::geom_text(size = 3) +
  ggplot2::coord_cartesian(xlim = c(-1, 7), ylim = c(0.5, 3)) + 
  ggplot2::labs(title = "Are Your Cats Indoors Or Outdoors?",
                caption = paste0("Response rate: ",
                                 response_rate$resp[1],
                                 " / ",
                                 response_rate$total[1],
                                 " = ",
                                 response_rate$resp_perc)) +
  ggplot2::scale_colour_manual(values = test) +
  ggplot2::theme_void() +
  ggplot2::theme(title = ggplot2::element_text(colour = "white", size = 8),
                 legend.position = "none",
                 plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"))

g <- ggbackground(g, image)

ggsave(filename = "graphs/2_indoors_or_outdoors.png")
