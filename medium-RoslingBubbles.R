library(animint2)

Rosling.bubbles <- function(
    x, y, data, nmax=50
  ) {
  n <- 10
  if (missing(data)) data <- data.frame(year = rep(1:nmax, each = n))
  n <- nrow(data)%/%nmax
  if (missing(x)) x <- runif(n) + sort(rnorm(n * nmax, 0, 0.02))
  if (missing(y)) y <- runif(n) + sort(rnorm(n * nmax, 0, 0.02))
  data$x <- x
  data$y <- y
  data$bg <- rgb(runif(n), runif(n), runif(n), 0.5)
  data$group <- 1:n
  data$size <- runif(n * nmax, min = 2, max = 70)
  
  bubble <- ggplot(data) +
    geom_point(aes(x = x, y = y, fill = bg, size = size, key = bg),
               showSelected="year",
               shape = 21, alpha = 0.5) + 
    scale_fill_identity() +
    scale_size(range = c(1, 70)) +
    theme(legend.position = "none") +
    xlab("x") +
    ylab("y")
  select.year <- ggplot()+
    geom_tallrect(aes(
      xmin=year-1/2, xmax=year+1/2),
      clickSelects="year",
      alpha=1/2,
      data=data)
  (viz <- animint(bubble, select.year,
                 time=list(variable="year", ms=200)))
}

Rosling.bubbles()
