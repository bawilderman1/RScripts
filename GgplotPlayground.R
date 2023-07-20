library(tidyverse)
library(lubridate, warn.conflicts = false)
library(tidyquant)
library(cowplot)

t <- tribble(~x, ~y,
             1, 1,
             2, 1,
             3, 1,
             4, 1,
             5, 1,
             8, 2,
             9, 2,
             10, 2,
             11, 2,
             12, 2,
             15, 3,
             16, 3,
             17, 3,
             18, 3,
             19, 3,
             22, 4,
             23, 4,
             24, 4,
             25, 4,
             26, 4,
             29, 5,
             30, 5,
             31, 5,
             32, 5,
             33, 5)
table(t)
head(t)

t %>% 
  ggplot(aes(x = factor(x), y = y)) + 
  geom_point() +
  scale_x_discrete(
    name = "discrete", 
    breaks = c(1,3,8,10,15,17,22,24,29,31),
    labels = c("one","","two","","three","","four","","five",""))

d <- tribble(~x, ~y,
             ymd_hm("2023-06-05T09:30"), 1,
             ymd_hm("2023-06-05T10:00"), 1.1,
             ymd_hm("2023-06-05T10:30"), 1.2,
             ymd_hm("2023-06-05T11:00"), 1.3,
             ymd_hm("2023-06-05T11:30"), 1.4,
             ymd_hm("2023-06-05T12:00"), 1.5,
             ymd_hm("2023-06-05T12:30"), 1.6,
             ymd_hm("2023-06-05T13:00"), 1.5,
             ymd_hm("2023-06-05T13:30"), 1.4,
             ymd_hm("2023-06-05T14:00"), 1.3,
             ymd_hm("2023-06-05T14:30"), 1.2,
             ymd_hm("2023-06-05T15:00"), 1.1,
             ymd_hm("2023-06-05T15:30"), 1,
             ymd_hm("2023-06-06T09:30"), 2,
             ymd_hm("2023-06-06T10:00"), 2.1,
             ymd_hm("2023-06-06T10:30"), 2.2,
             ymd_hm("2023-06-06T11:00"), 2.3,
             ymd_hm("2023-06-06T11:30"), 2.4,
             ymd_hm("2023-06-06T12:00"), 2.5,
             ymd_hm("2023-06-06T12:30"), 2.6,
             ymd_hm("2023-06-06T13:00"), 2.5,
             ymd_hm("2023-06-06T13:30"), 2.4,
             ymd_hm("2023-06-06T14:00"), 2.3,
             ymd_hm("2023-06-06T14:30"), 2.2,
             ymd_hm("2023-06-06T15:00"), 2.1,
             ymd_hm("2023-06-06T15:30"), 2,
             ymd_hm("2023-06-07T09:30"), 3,
             ymd_hm("2023-06-07T10:00"), 3.1,
             ymd_hm("2023-06-07T10:30"), 3.2,
             ymd_hm("2023-06-07T11:00"), 3.3,
             ymd_hm("2023-06-07T11:30"), 3.4,
             ymd_hm("2023-06-07T12:00"), 3.5,
             ymd_hm("2023-06-07T12:30"), 3.6,
             ymd_hm("2023-06-07T13:00"), 3.5,
             ymd_hm("2023-06-07T13:30"), 3.4,
             ymd_hm("2023-06-07T14:00"), 3.3,
             ymd_hm("2023-06-07T14:30"), 3.2,
             ymd_hm("2023-06-07T15:00"), 3.1,
             ymd_hm("2023-06-07T15:30"), 3,)

d %>% 
  ggplot(aes(x = factor(x), y = y)) +
  geom_point() + 
  scale_x_discrete(
    name = "Discrete Date",
    breaks = c("2023-06-05 09:30:00","2023-06-05 12:30:00","2023-06-06 09:30:00","2023-06-06 12:30:00","2023-06-07 09:30:00","2023-06-07 12:30:00"),
    labels = c("Jun 5","12:30","Jun 6", "12:30","Jun 7", "12:30")
  ) +
  # scale_x_datetime(
  #   "Date",
  #   breaks = scales::breaks_width("1 days"), 
  #   labels = scales::label_date("%m/%d")
  # )
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

f <- factor(d$x)
typeof(f)
class(f)
levels(f)

# library(glue)
# str_glue('{format(testDate, "%b %d")}', testDate = ymd_hm("2023-06-05T09:30"))
dttm <- ymd_hm("2023-06-05T09:30")
format(dttm, "%b %d")
format(dttm, "%H:%M")

format(dttm, "%H:%M") == "09:30"
format(dttm, "%H:%M") %in% c("09:30", "12:30")
format(dttm, "%H:%M") %>%  is.element(c("09:30", "12:30"))

dBreaks <- d$x %>% purrr::keep(function(x) format(x, "%H:%M") %in% c("09:30", "12:30"))
dBreaks

dLabels <- dBreaks %>% purrr::map_chr(function(x) { if_else(format(x, "%H:%M") == "09:30", format(x, "%b %d"), format(x, "%H:%M")) })
dLabels

z <- tribble(~xmins,~xmaxs, ~ymins, ~ymaxs,
             "2023-06-06 10:00:00", "2023-06-06 14:00:00", -Inf, Inf,
             "2023-06-07 11:00:00", "2023-06-07 13:30:00", -Inf, Inf)

d %>% 
  ggplot(aes(x = factor(x), y = y)) +
  # ggplot() +
  geom_rect(aes(xmin = "2023-06-06 10:00:00", xmax = "2023-06-06 14:00:00", ymin = -Inf, ymax = Inf), fill = "steelblue", colour = "transparent", alpha = 0.01) +
  # geom_rect(data = z, aes(xmin = xmins, xmax = xmaxs, ymin = ymins, ymax = ymaxs), fill = "steelblue", colour = "transparent", alpha = 0.01) +
  geom_point() +
  # geom_point(data = d, aes(x = factor(x), y = y)) +
  scale_x_discrete(
    name = "Discrete Date",
    breaks = factor(dBreaks),
    labels = dLabels
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

c <- tribble(~rn, ~dt, ~l, ~o, ~c, ~h, ~hasCond, ~z, ~dd,
             1, ymd_hm("2023-06-05T09:30"), 5, 5.4, 5.6, 6, FALSE, 1, 0,
             2, ymd_hm("2023-06-05T10:00"), 5.1, 5.4, 5.6, 6.1, FALSE, 1, 0,
             3, ymd_hm("2023-06-05T10:30"), 5.2, 5.6, 5.3, 6.2, TRUE, 1, -0.3,
             4, ymd_hm("2023-06-05T11:00"), 5.3, 5.3, 5.6, 6.3, TRUE, 1, 0,
             5, ymd_hm("2023-06-05T11:30"), 5.4, 5.6, 6.1, 6.4, TRUE, 1, 0,
             6, ymd_hm("2023-06-05T12:00"), 5.5, 6.1, 5.8, 6.5, TRUE, 1, -0.3,
             7, ymd_hm("2023-06-05T12:30"), 5.6, 5.8, 6.3, 6.6, TRUE, 1, 0,
             8, ymd_hm("2023-06-05T13:00"), 5.5, 6.3, 5.8, 6.5, FALSE, 1, -0.5,
             9, ymd_hm("2023-06-05T13:30"), 5.4, 5.8, 6.1, 6.4, FALSE, 1, -0.2,
             10, ymd_hm("2023-06-05T14:00"), 5.3, 6.1, 5.6, 6.3, FALSE, 1, -0.7,
             11, ymd_hm("2023-06-05T14:30"), 5.2, 5.6, 5.3, 6.2, FALSE, 1, -1,
             12, ymd_hm("2023-06-05T15:00"), 5.1, 5.3, 5.5, 6.1, FALSE, 1, -0.8,
             13, ymd_hm("2023-06-05T15:30"), 5, 5.5, 6, 6, FALSE, 1, -0.3,
             14, ymd_hm("2023-06-06T09:30"), 10, 10.8, 11.2, 12, FALSE, 1, 0,
             15, ymd_hm("2023-06-06T10:00"), 10.2, 10.8, 11.2, 12.2, FALSE, 1, 0,
             16, ymd_hm("2023-06-06T10:30"), 10.4, 11.2, 10.6, 12.4, FALSE, 1, -0.6,
             17, ymd_hm("2023-06-06T11:00"), 10.6, 10.6, 11.2, 12.6, FALSE, 1, 0,
             18, ymd_hm("2023-06-06T11:30"), 10.8, 11.2, 12.2, 12.8, FALSE, 1, 0,
             19, ymd_hm("2023-06-06T12:00"), 11, 12.2, 11.6, 13, FALSE, 1, -0.6,
             20, ymd_hm("2023-06-06T12:30"), 11.2, 11.6, 12.6, 13.2, FALSE, 1, 0,
             21, ymd_hm("2023-06-06T13:00"), 11, 12.6, 11.6, 13, FALSE, 1, -1,
             22, ymd_hm("2023-06-06T13:30"), 10.8, 11.6, 12.2, 12.8, FALSE, 1, -0.4,
             23, ymd_hm("2023-06-06T14:00"), 10.6, 12.2, 11.2, 12.6, FALSE, 1, -1.4,
             24, ymd_hm("2023-06-06T14:30"), 10.4, 11.2, 10.6, 12.4, FALSE, 1, -2,
             25, ymd_hm("2023-06-06T15:00"), 10.2, 10.6, 11, 12.2, FALSE, 1, -1.6,
             26, ymd_hm("2023-06-06T15:30"), 10, 11, 12, 12, FALSE, 1, -0.6,
             27, ymd_hm("2023-06-07T09:30"), 20, 21.6, 22.4, 24, FALSE, 1, 0,
             28, ymd_hm("2023-06-07T10:00"), 20.4, 21.6, 22.4, 24.4, FALSE, 1, 0,
             29, ymd_hm("2023-06-07T10:30"), 20.8, 22.4, 21.2, 24.8, FALSE, 1, -1.2,
             30, ymd_hm("2023-06-07T11:00"), 21.2, 21.2, 22.4, 25.2, TRUE, 1, 0,
             31, ymd_hm("2023-06-07T11:30"), 21.6, 22.4, 24.4, 25.6, TRUE, 1, 0,
             32, ymd_hm("2023-06-07T12:00"), 22, 24.4, 23.2, 26, TRUE, 1, -1.2,
             33, ymd_hm("2023-06-07T12:30"), 22.4, 23.2, 25.2, 26.4, TRUE, 1, 0,
             34, ymd_hm("2023-06-07T13:00"), 22, 25.2, 23.2, 26, TRUE, 1, -2,
             35, ymd_hm("2023-06-07T13:30"), 21.6, 23.2, 24.4, 25.6, TRUE, 1, -0.8,
             36, ymd_hm("2023-06-07T14:00"), 21.2, 24.4, 22.4, 25.2, TRUE, 1, -1.8,
             37, ymd_hm("2023-06-07T14:30"), 20.8, 22.4, 21.2, 24.8, FALSE, 1, -4,
             38, ymd_hm("2023-06-07T15:00"), 20.4, 21.2, 22, 24.4, FALSE, 1, -3.2,
             39, ymd_hm("2023-06-07T15:30"), 20, 22, 24, 24, FALSE, 1, -1.2,)

pt <- c %>% 
  ggplot(aes(x = rn, y = c)) +
  geom_tile(aes(height = if_else(hasCond, Inf, NA), width = 1, alpha = 0.01), na.rm = TRUE, alpha = 0.4, fill = "khaki3") +
  geom_candlestick(aes(open = o, close = c, high = h, low = l), fill_up = "dodgerblue3", colour_up = "dodgerblue3", fill_down = "firebrick3", colour_down = "firebrick3") +
  coord_trans(y = "log2") +
  #coord_trans(y = "log10") +
  labs(y = "Price ($)", title = "My Graph") +
  scale_x_continuous(
    name = NULL,
    breaks = NULL,
    minor_breaks = c(1, 7, 14, 20, 27, 33)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
      
pb1 <- c %>% 
  ggplot(aes(x = rn)) +
  geom_tile(aes(y = dd, height = if_else(hasCond, Inf, NA), width = 1, alpha = 0.01), na.rm = TRUE, alpha = 0.4, fill = "khaki3") +
  geom_area(aes(y = dd, fill = "red"), alpha = 0.4) +
  geom_hline(aes(yintercept = 0), colour = "gray") +
  labs(y = "Draw Down ($)", caption = "30 min") +
  scale_x_continuous(
    name = "Continous Date",
    breaks = c(1, 7, 14, 20, 27, 33),
    labels = c("Jun 5","12:30","Jun 6","12:30","Jun 7","12:30"),
    minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
cowplot::plot_grid(pt, pb1, ncol = 1, align = "v", axis = "lr", rel_heights = c(1, 0.5))  

#?scale_x_continuous