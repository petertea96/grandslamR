#devtools::create("grandslamR")

library(devtools)
library(roxygen2)
library(testthat)

# -- Check that all R packages are loaded
load_all()
#test()
check()
document()


library(grandslamR)
library(ggplot2)
library(dplyr)
load_available_matches() %>% head()

thiem_nadal <- load_complete_match_data(match_id = 'atp_roland_garros_2019_SM001')

thiem_nadal %>%
  group_by(point_ID) %>%
  summarise(cc = sum(position == 'bounce')) %>%
  filter(cc <= 5) %>%
  arrange(desc(cc))

thiem_nadal %>%
  dplyr::filter(point_ID == '1_7_12_1') %>%
  dplyr::filter(position == 'bounce') %>%
  ggplot(aes(x = x,
             y = y)) +
  gg_fulltenniscourt() +
  geom_point(alpha = 0.75, fill = 'green', shape = 21,
             size = 2.5) +
  geom_path(color = 'white', linetype = 2)

load_complete_player_data(player_name = 'D.SHAPOVALOV')
