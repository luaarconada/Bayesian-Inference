# Sample data from a bivariate regression
n <- 300
set.seed(123456)
X <- mvtnorm::rmvnorm(n = n, mean = c(0, 0),
                      sigma = matrix(c(2, 0.5, 0.5, 1.5), nrow = 2, ncol = 2))
m <- function(x) 0.5 * (x[, 1]^2 + x[, 2]^2)
epsilon <- rnorm(n)
Y <- m(x = X) + epsilon
# Plot sample and regression function
rgl::plot3d(x = X[, 1], y = X[, 2], z = Y, xlim = c(-3, 3), ylim = c(-3, 3),
            zlim = c(-4, 10), xlab = "X1", ylab = "X2", zlab = "Y")
lx <- ly <- 50
x_grid <- seq(-3, 3, l = lx)
y_grid <- seq(-3, 3, l = ly)
xy_grid <- as.matrix(expand.grid(x_grid, y_grid))
rgl::surface3d(x = x_grid, y = y_grid,
               z = matrix(m(xy_grid), nrow = lx, ncol = ly),
               col = "lightblue", alpha = 1, lit = FALSE)
# Local constant fit
# An alternative for calling np::npregbw without formula
bw0 <- np::npregbw(xdat = X, ydat = Y, regtype = "lc")
kre0 <- np::npreg(bws = bw0, exdat = xy_grid) # Evaluation grid is now a matrix
rgl::surface3d(x = x_grid, y = y_grid,
               z = matrix(kre0$mean, nrow = lx, ncol = ly),
               col = "red", alpha = 0.25, lit = FALSE)
# Local linear fit
bw1 <- np::npregbw(xdat = X, ydat = Y, regtype = "ll")
kre1 <- np::npreg(bws = bw1, exdat = xy_grid)
rgl::surface3d(x = x_grid, y = y_grid,
               z = matrix(kre1$mean, nrow = lx, ncol = ly),
               col = "green", alpha = 0.25, lit = FALSE)
rgl::rglwidget()

apply(wine[c("Age", "WinterRain", "AGST", "HarvestRain")], 2, median)