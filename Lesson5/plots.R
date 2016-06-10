library(ggplot2)
library(dplyr)
library(magrittr)
data(diamonds)

# Plot 1
ggplot(aes(x = price, fill = cut), data = diamonds) +
    geom_histogram() +
    scale_fill_brewer(type = "qual") +
    scale_x_log10() +
    facet_wrap(~color)

# PLot 2
ggplot(aes(x = table, y = price), data = diamonds) +
    geom_point(aes(color = cut)) +
    scale_color_brewer(type = "qual")

summary(diamonds %>% filter(cut == "Ideal") %>% select(table))
summary(diamonds %>% filter(cut == "Premium") %>% select(table))

# Plot 3
diamonds %<>% mutate(volume = x * y * z)
ggplot(aes(x = volume, y = price), data = diamonds) +
    geom_point(aes(color = clarity)) +
    scale_color_brewer(type = "qual") +
    scale_y_log10() +
    xlim(0,quantile(diamonds$volume,0.99))

# Create Variable
pf <- read.delim('pseudo_facebook.tsv')

# pf %<>% mutate(prop_initiated = ifelse(friend_count == 0, 0, friendships_initiated / friend_count))
pf$prop_initiated <- ifelse(pf$friend_count == 0, 0, pf$friendships_initiated / pf$friend_count)


# Plot 4
pf %<>% mutate(year_joined = floor(2014 - tenure / 365))
pf %<>% mutate(year_joined.bucket = cut(year_joined,c(2004,2009,2011,2012,2014)))

ggplot(aes(x = tenure, y = prop_initiated), data = pf) +
    geom_line(aes(color = year_joined.bucket), stat = "summary", fun.y = median)

# Plot 5
ggplot(aes(x = tenure, y = prop_initiated), data = pf) +
    geom_line(aes(color = year_joined.bucket), stat = "summary", fun.y = median) +
    geom_smooth(method = "lm")

# Plot 6
ggplot(aes(x = cut, y = price/carat), data = diamonds) +
    geom_point(aes(color = color)) +
    scale_color_brewer(type = 'div') +
    facet_wrap(~clarity)
