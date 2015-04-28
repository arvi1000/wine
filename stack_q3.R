library(data.table)
library(ggplot2)

# generate dummy data
set.seed(1)
pieces <- 5

my_data <-
  rbindlist(
    lapply(1:pieces, function(p) {
      
      this_n <- round(runif(1, min=5, max=20), 0)
      this_beta <- runif(1, min=-5, max=5)
      this_alpha <- runif(1, min=-10, max=10)
      first_x <- round(runif(1, min=-10, max=10), 0)
      this_x <- first_x:(first_x+this_n-1)
      
      data.frame(id=rep(p, this_n),
                 x=this_x,
                 y=this_alpha + this_beta*this_x + rnorm(this_n))
    }))

# have a look
ggplot(my_data, aes(x=x, y=y, color=factor(id))) +
  geom_point(size=3)

# fit a linear model for each piece, and extract the residuals
my_data$residuals <- unlist(
  lapply(split(my_data, my_data$id),
         function(x) lm(y~x, data=x)$residuals))

# tss / rss by piece
tss_rss <-
  my_data[, .(tss=sum( (y-mean(y))^2 ), rss=sum(residuals^2)), by=id]

# global r.sq:
with(tss_rss, 1 - sum(rss) / sum(tss))
