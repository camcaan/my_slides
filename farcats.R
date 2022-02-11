# install.packages('parcats')

# install.packages("devtools")
devtools::install_github("erblast/parcats")


# install.packages("easyalluvial")

library(easyalluvial)

library(tidyverse)

# alluvial from data in wide format

knitr::kable(head(mtcars2))

# plot
structure(list(id = c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2), drug_1 = c(0,
                                                                      0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1), drug_2 = c(0, 1, 1, 1, 1, 0,
                                                                                                                   1, 0, 0, 1, 0, 1)), class = "data.frame", row.names = c(NA, -12L
                                                                                                                   ))
