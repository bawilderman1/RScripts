library(tidyverse)
library(lubridate, warn.conflicts = false)

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
