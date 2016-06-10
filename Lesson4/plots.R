library(ggplot2)
library(magrittr)
library(dplyr)
data(diamonds)

# Scatter plot of price vs. x
ggplot(aes(x = x, y = price), data = diamonds) +
    geom_point()

cor(diamonds$price, diamonds$x)
cor(diamonds$price, diamonds$y)
cor(diamonds$price, diamonds$z)

# Scatter plot of price vs. depth
ggplot(aes(x = depth, y = price), data = diamonds) +
    geom_point()

min(diamonds$depth)
max(diamonds$depth)
cor(diamonds$depth, diamonds$price)

# Scatter plot of price vs. carat without top 1% of both values
ggplot(aes(x = carat, y = price), data = diamonds) + 
    geom_point() +
    xlim(0,quantile(diamonds$carat,0.99)) +
    ylim(0,quantile(diamonds$price,0.99))

# Scatter plot of price vs. volume
ggplot(aes(x = (x*y*z), y = price), data = diamonds) + 
    geom_point() +
    xlab("volume")

diamonds %<>% mutate(volume = x*y*z)
with(diamonds %>% filter(volume > 0 & volume < 800), cor(price, volume))

# Adjust above to exclude outliers and add linear fit
ggplot(aes(x = volume, y = price), data = diamonds %>% filter(volume > 0 & volume < 800)) +
    geom_point(alpha = 0.1) +
    geom_smooth()

# Create data frame diamondsByClarity
# suppressMessages(library(ggplot2))
# suppressMessages(library(dplyr))
# data(diamonds)

diamondsByClarity <- diamonds %>%
    group_by(clarity) %>%
    summarise(mean_price = mean(price),
                median_price = median(price),
                min_price = min(price),
                max_price = max(price),
                n = n())

# Create bar plots
diamonds_by_clarity <- diamonds %>% group_by(clarity) %>% summarise(mean_price = mean(price))

diamonds_by_color <- diamonds %>% group_by(color) %>% summarise(mean_price = mean(price))

p1 <- ggplot(aes(x = clarity, y = mean_price), data = diamonds_by_clarity) +
    geom_bar(position = "dodge", stat = "identity")

p2 <- ggplot(aes(x = color, y = mean_price), data = diamonds_by_color) +
    geom_bar(position = "dodge", stat = "identity")

grid.arrange(p1, p2, ncol=2)

