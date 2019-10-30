
library(tidyverse)
library(glue)
library(ggrepel)
library(pec)
library(magrittr)

outcome <- 'dead'
model <- 'xgb'

.relevel <- function(data){
  data %>% 
    mutate(
      time = factor(
        x = time, 
        levels = c('1','3','6','12'),
        labels = glue('{c(1,3,6,12)} Month \nPost-op')
      ),
      name = factor(
        x = name, 
        levels = c('w00','w01','w04'),
        labels = c(
          "Pre-op \nRP Equation", 
          "1 Week \nPost-op \nRP Equation", 
          "4 Weeks \nPost-op \nRP Equation"
          )
      )
    )
}

relabel_vec = c(
  "Pre-op \nRP Equation" = 'w00_txt', 
  "1 Week \nPost-op \nRP Equation" = 'w01_txt', 
  "4 Weeks \nPost-op \nRP Equation" = 'w04_txt'
)

cstats <- read_rds(glue('results/cstats/{outcome}_{model}.rds'))

gg_dat <- cstats %>% 
  dplyr::filter(mdl == 'full') %>% 
  dplyr::select(name, time, cstat) 

segment_dat <- gg_dat %>% 
  spread(name, cstat) %>% 
  mutate(
    y = pmin(w00, w01, w04, na.rm = TRUE),
    yend = pmax(w00, w01, w04, na.rm = TRUE),
    name = 'filler_column'
  )

make_perc_val <- function(num_vec){
  
  signs <- dplyr::if_else(num_vec > 0, "+", "-")
  chars <- tibbleOne::adapt_round(num_vec)
  output <- glue::glue("{signs}{chars}%")
  
  if(any(is.na(num_vec))){
    output[is.na(num_vec)] <- NA_character_
  }
  
  output
  
}

label_repel_dat <- gg_dat %>% 
  filter(time == '12')

gg_dat %<>% 
  arrange(time) %>% 
  group_by(time) %>% 
  mutate(
    cstat_txt_right = 100 * (cstat / cstat[name=='w00'] - 1),
    cstat_txt_right = make_perc_val(cstat_txt_right),
    cstat_txt_left = tibbleOne::adapt_round(cstat)
  ) %>% 
  ungroup() %>% 
  mutate(
    cstat_txt_right = recode(cstat_txt_right, '-0.00%' = '(reference)')
  ) %>% 
  .relevel()

label_repel_dat %<>% .relevel()
segment_dat %<>% .relevel()

cols <- RColorBrewer::brewer.pal(n=3, name = 'Dark2')[c(3,1,2)]
  
plt <- ggplot(gg_dat, aes(x = time, y = cstat, fill = name)) +
  geom_segment(
    data = segment_dat, 
    mapping = aes(xend = time, y=y, yend = yend), 
    color = 'black'
  ) +
  geom_line(aes(group = name), linetype = 2, col = 'grey') +
  geom_text(
    mapping = aes(label = cstat_txt_right, col = name),
    nudge_x = c(1/3, 1/4, 1/4)
  ) +
  geom_text(
    mapping = aes(label = cstat_txt_left, col = name),
    nudge_x = -1/6
  ) +
  geom_label_repel(
    data = label_repel_dat, 
    mapping = aes(label = name),
    nudge_x = 1,
    nudge_y = c(0.02, 0.04, 0.07),
    force = 1.5,
    segment.color = 'grey'
  ) +
  geom_point(shape = 21, color = 'black', size = 5) + 
  labs(
    x = '\nTime since mechanical circulatory support was applied',
    y = 'C-statistic for mortality events\n'
  ) + 
  theme_bw() +
  theme(
    legend.position = '', 
    text = element_text(size = 16, face = 'bold'), 
    panel.grid = element_blank()
  ) +
  scale_fill_brewer(type = 'qual', palette = 'Dark2') +
  scale_color_manual(values = cols)+
  expand_limits(x=5.5, y = c(0.65, 0.87))

write_rds(plt, glue('docs/figs/{outcome}_{model}_cstats.rds'))
