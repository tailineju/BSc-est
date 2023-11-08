if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, car)


# qual o estudo?
# verificar somente um efeito (a)
# controlar outros efeitos ( b e c) 

obs <- c(83,77,80,83,85,
         80,85,85,81,79,
         82,97,76,84,76,
         81,93,81,91,83,
         74,87,89,88,72)
