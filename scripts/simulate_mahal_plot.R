library(tidyverse)
theme_set(theme_bw() +
            theme(panel.grid = element_blank()))

n = 1000

df <- tibble(x = rnorm(n), y = x + rnorm(n, sd = 2)) %>%
  mutate(x = x + abs(min(x)),
         y = y + abs(min(y)))

x_thresh <- df$x %>% quantile(0.9)
y_thresh <- df$y %>% quantile(0.9)

df <- df %>%
  mutate(Reference = x > x_thresh & y > y_thresh)

ggplot(df) +
  geom_point(aes(x = x, y = y, col = Reference), alpha = 0.5) +
  labs(x = NULL,
       y = NULL) +
  theme(axis.text = element_blank()) +
  stat_ellipse(data = df %>% filter(Reference),
               aes(x = x, y = y), col = "#00b4c0", type = "euclid", linewidth = 1) +
  theme(legend.position = "none")

ggsave(here::here("figures", "mahal_fake_plot.png"), height = 3, width = 3, dpi = 300)
