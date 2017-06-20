library(MASS)
library(ggplot2)
library(dplyr)

se.kernel <- function(x1, x2) {
    exp(-.5 * (x1 - x2)^2)
}

pdist <- function(x, y, fn) outer(x, y, Vectorize(fn))

posterior <- function(X.train, Y.train, X.test, k = se.kernel) {
    K <- function(X1, X2) pdist(X1, X2, k)
    Mean <- K(X.test, X.train) %*% solve(K(X.train, X.train), Y.train)

    Sigma <- K(X.test, X.test) - K(X.test, X.train) %*%
        solve(K(X.train, X.train), K(X.train,X.test))

    list(y = mvrnorm(1, Mean, Sigma), mean = Mean, var = diag(Sigma))
}

draw <- function (n, k = se.kernel, X = c(), Y = c()) {
    X.test <- sort(runif(n, 0, 10))
    p <- posterior(X, Y, X.test, se.kernel)
    data.frame(x = X.test, y = p$y, mean = p$mean, var = p$var)
}

draws <- function(n, m, X, Y, k = se.kernel) {
    df <- data.frame()
    for (i in 1:m) {
        df <- rbind(df, draw(n, k, X, Y) %>% mutate(draw = i))
    }
    df
}

gen.samples <- function(fn, n=5, m=10) {
    x <- runif(n, 0, m)
    y <- sapply(x, fn)
    cbind(x,y)
}

plot.gp <- function (fn) {
    dat <- gen.samples(fn)
    draws(100, 7, dat[,1], dat[,2]) %>%
        ggplot(aes(x = x)) +
        geom_ribbon(aes(ymin = mean - 1.96*var, ymax = mean + 1.96*var), fill = "grey80") +
        geom_line(aes(y = y, color = as.factor(draw))) +
        geom_line(aes(y = mean))
}

plot.gp(sin)
plot.gp(cos)
plot.gp(function (x) x^-.3)
