library(ggplot2)
diamonds

ggplot(data = diamonds, aes(x = cut)) +
geom_bar(fill = "blue", colour = "black")

ggplot(data = diamonds, aes(x = price)) +
geom_histogram()

ggplot(data = diamonds, aes(x = table, y=depth)) +
geom_point()


library(ggplot2)
library(GGally)
library(scales)
library(memisc)

set.seed(20022012)
diamond_samp <- diamonds[sample(1:length(diamonds$price), 10000), ]
ggpairs(diamond_samp,lower= list(continuous = wrap("points", shape = I('.'))),
        upper = list(combo = wrap("box", outlier.shape = I('.'))))