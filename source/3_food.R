## Image credit:
## Graph 1:
## Background: Kris Chen at 
## https://pixabay.com/photos/cat-eat-looking-kitten-kitty-4135062/
## Dry food icon: 
## kibble by Jieting Tina Chen from the Noun Project
## https://thenounproject.com/term/kibble/1643226/
## Wet food icon:
## Meat by sandra from the Noun Project
## https://thenounproject.com/search/?q=meat&i=1263289
## Graph 2: 
## Image by Didi S on Pixabay 
## https://pixabay.com/photos/cat-white-angora-turkish-cafe-3593012/

library(dplyr)
library(stringi)
library(ggimage)


## Get data -----
food_data_raw <- read.csv("source/3_food_data/poll3-data.csv")
colnames(food_data_raw) <- c("food_type", "num_votes", "voter", "voter_id")
food_data_raw <- 
  food_data_raw %>% 
  dplyr::select(food_type, num_votes, voter)

hard_food <-
  food_data_raw$voter[1] %>%
  stringr::str_split(pattern = ";")

soft_food <-
  food_data_raw$voter[2] %>%
  stringr::str_split(pattern = ";")

both_hard_and_soft <- base::intersect(hard_food[[1]], soft_food[[1]])
hard_only <- base::setdiff(hard_food[[1]], soft_food[[1]])
soft_only <- base::setdiff(soft_food[[1]], hard_food[[1]]) ## None!
total_length <- length(hard_only) + length(both_hard_and_soft)

food_data <-
  data.frame(type = c("Hard Food Only", "Hard & Soft Food", "Soft Food Only"),
             count = c(length(hard_only), 
                       length(both_hard_and_soft),
                       length(soft_only))) %>%
  dplyr::mutate(prop = count / sum(count)) %>%
  dplyr::mutate(perc_num = round(prop * 100, 0)) %>%
  dplyr::mutate(perc = paste0(perc_num, "%")) %>%
  dplyr::mutate(label = paste0(type, ": ", perc))

response_rate <-
  data.frame(votes = sum(food_data$count),
             total = 286) %>%
  dplyr::mutate(prop = votes / total) %>%
  dplyr::mutate(perc = paste0(round(x = prop * 100, digits = 0), "%"))

## To create a random scatter plot that avoids the center where the label is
food_data_for_graphing <- 
  data.frame(type = c(rep("Hard Food Only", length(hard_only)), 
                      rep("Hard & Soft Food", length(both_hard_and_soft)))) %>%
  dplyr::mutate(x = runif(n = total_length,
                          min = 0, max = 40),
                y = runif(n = total_length,
                          min = 0, max = 40)) %>%
  dplyr::mutate(flip_x = 
                  sample(x = c(0, 1), size = total_length, replace = TRUE),
                flip_y = 
                  sample(x = c(0, 1), size = total_length, replace = TRUE)) %>%
  dplyr::mutate(offset = 
                  ifelse(test = type == "Hard Food Only",
                         yes = 0,
                         no = ifelse(test = type == "Hard & Soft Food",
                                     yes = 150,
                                     no = 300))) %>%
  dplyr::mutate(x = abs(flip_x * 100 - x),
                y = abs(flip_y * 100 - y) + offset) %>%
  dplyr::mutate(img = ifelse(test = type == "Hard Food Only",
                             yes = "img/kibble_icon.png",
                             no = ifelse(test = type == "Hard & Soft Food",
                                         yes = "img/kibble_and_meat_icon.png",
                                         no = "img/meat_icon.png")))


## Make graph #1 except it turned out ugly -----
cat_graph <-
  ggplot2::ggplot(data = food_data_for_graphing,
                  mapping = ggplot2::aes(x = x, 
                                         y = y, 
                                         image = img)) +
  ggimage::geom_image(aes(size = I(0.04))) +
  ggplot2::coord_cartesian(xlim = c(0, 300), ylim = c(0, 400)) + 
  ggplot2::theme_void() +
  ggplot2::geom_text(mapping = aes(x = 50, y = 50),
                      label = 
                       food_data$label[food_data$type == "Hard Food Only"],
                     size = 2) +
  ggplot2::geom_text(mapping = aes(x = 50, y = 200),
                     label = 
                       food_data$label[food_data$type == "Hard & Soft Food"],
                     size = 1.9) +
  ggplot2::geom_text(mapping = aes(x = 50, y = 300),
                     label = 
                       food_data$label[food_data$type == "Soft Food Only"],
                     size = 2)

g <- ggbackground(cat_graph, "img/short-coated-white-cat-1287518.jpg")


## Make graph #2 -----
bar_graph <-
  ggplot2::ggplot(data = food_data,
                  mapping = ggplot2::aes(x = 1, 
                                         y = perc_num,
                                         fill = type)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::geom_text(aes(x=1, y=perc_num, label=label),
                     vjust = 6,
                     size = 1.7,
                     color = "black") +
  ggplot2::labs(title = "What Type Of Food Does Your Cat Prefer?",
                 caption = paste0("Response Rate: ",
                                  response_rate$votes,
                                  " / ",
                                  response_rate$total,
                                  " = ",
                                  response_rate$perc)) +
  ggplot2::coord_cartesian(ylim = c(-20, 100), xlim = c(0, 6)) +
  ggplot2::scale_fill_manual(values = c("cadetblue", "darkseagreen", "black")) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position =  "none",
                 plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"))

bar_graph_img <-
  ggimage::ggbackground(gg = bar_graph,
                        background = "img/short-coated-white-cat-1287518.jpg")

ggsave(filename = "graphs/3_what_type_of_food.png", 
       plot = bar_graph_img,
       width = 6,
       height = 4)
