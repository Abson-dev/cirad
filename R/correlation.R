# Loading
library(ggcorrplot)
# Compute a correlation matrix
data(mtcars)
corr <- round(cor(mtcars), 1)
head(corr[, 1:6])

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(mtcars)
head(p.mat[, 1:4])

# Visualize the correlation matrix
# --------------------------------
# method = "square" (default)
ggcorrplot(corr)
# method = "circle"
ggcorrplot(corr, method = "circle")


# Reordering the correlation matrix
# --------------------------------
# using hierarchical clustering
ggcorrplot(corr, hc.order = TRUE, outline.color = "white")


# Types of correlogram layout
# --------------------------------
# Get the lower triangle
ggcorrplot(corr,
           hc.order = TRUE,
           type = "lower",
           outline.color = "white")


# Change colors and theme
# --------------------------------
# Argument colors
ggcorrplot(
  corr,
  hc.order = TRUE,
  type = "lower",
  outline.color = "white",
  ggtheme = ggplot2::theme_gray,
  colors = c("#6D9EC1", "white", "#E46726")
)


# Add correlation coefficients
# --------------------------------
# argument lab = TRUE
ggcorrplot(corr,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)


# Add correlation significance level
# --------------------------------
# Argument p.mat
# Barring the no significant coefficient
ggcorrplot(corr,
           hc.order = TRUE,
           type = "lower",
           p.mat = p.mat)


# Leave blank on no significant coefficient
ggcorrplot(
  corr,
  p.mat = p.mat,
  hc.order = TRUE,
  type = "lower",
  insig = "blank"
)