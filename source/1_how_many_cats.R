## How many cats do you have?
## 0, 1, 2, 3, 4, or 5+?
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggthemes)
library(magick)
library(ggtextures)

## Get data -----
how_many <-
  data.frame(cats_in_home = c(0, 1, 2, 3, 4, 5),
             votes = c(4, 31, 17, 3, 0, 2)) %>%
  dplyr::mutate(cats_factor = ifelse(test = cats_in_home != 1,
                                     yes = ifelse(test = cats_in_home != 5,
                                                  yes = paste0(cats_in_home, 
                                                               " cats"),
                                                  no = "5+ cats"),
                                     no = paste0(cats_in_home, " cat"))) %>%
  dplyr::mutate(cats_factor = factor(cats_factor,
                                     levels = c("0 cats",
                                                "1 cat",
                                                "2 cats",
                                                "3 cats",
                                                "4 cats",
                                                "5+ cats"))) %>%
  dplyr::mutate(perc = paste0(round(x = votes / sum(votes, na.rm = TRUE) * 100,
                                    digits = 0),
                              "%")) %>%
  dplyr::as_tibble()

images <- tibble(image = c(list(magick::image_read("img/_no_cats.jpg")),
                           list(magick::image_read("img/_one_cat.jpg")),
                           list(magick::image_read("img/_two_cats.jpg")),
                           list(magick::image_read("img/_three_cats.jpg")),
                           list(magick::image_read("img/_four_cats.jpg")),
                           list(magick::image_read("img/_five_cats.jpg"))),
                 cats_in_home = c(0, 1, 2, 3, 4, 5))

how_many <- dplyr::left_join(how_many, images, by = c("cats_in_home"))

response_rate <- 
  data.frame(resp = sum(how_many$votes, na.rm = TRUE),
             total = c(287)) %>%
  dplyr::mutate(resp_rate = resp / total) %>%
  dplyr::mutate(resp_perc = paste0(round(x = resp_rate * 100, digits = 2), "%"))


## Graph data -----
how_many_graph <-
  ggplot2::ggplot(data = how_many, 
                  mapping = ggplot2::aes(x = cats_factor, 
                                         y = votes,
                                         image = image)) +
  ggtextures::geom_textured_bar(stat = "identity") +
  geom_text(mapping = ggplot2::aes(label = perc, 
                                   x = cats_factor, 
                                   y = votes), 
            position = position_dodge(width = 0.8), vjust = -0.6) +
  ggplot2::labs(title = "How Many Cats Do You Have In Your Home?",
                caption = paste0("Poll response rate: ",
                                 response_rate$resp[1],
                                 " / ",
                                 response_rate$total[1],
                                 " cat lovers = ",
                                 response_rate$resp_perc)) + 
  ggthemes::theme_hc() + 
  ggplot2::theme(axis.title.x = ggplot2::element_blank())
  

ggplot2::ggsave(filename = "graphs/1_how_many_cats.png",
                plot = how_many_graph,
                height = 9.2)
