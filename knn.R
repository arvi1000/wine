library(FNN)

scree <- 
  sapply(c(2:10, 29), function(k) {
    
    cat('Fitting LOOCV knn model for k=', k, '\n')
    knn_resids <- unlist(
      sapply(users, function(u) {
        knn.reg(train= md2[review_userid==u, names(v_scores)[-1], with=F],
                y= md2[review_userid==u, review_points],
                k=k)$residuals
      })
    )
    
    knn_dt <- data.table(review_userid=md2$review_userid,
                         review_points=md2$review_points,
                         knn_resids = knn_resids)
    
    with(knn_dt[, .(tss=sum((mean(review_points) - review_points)^2),
                    rss=sum(knn_resids^2)), by=review_userid],
         1 - ( sum(rss) / sum(tss)) )
  })

data.frame(k=c(2:10, 29), r_sq=scree) %>%
  ggplot(aes(x=k, y=r_sq)) + 
  geom_hline(yintercept=0, color='red') +
  geom_point() + geom_line() +
  theme_bw() +
  ggtitle('Global r-square for knn_reg\n(via leave one out cross validation)')

# how about for any particular users?
knn_3nn_r.sq <-
  sapply(users, function(u) {
    knn.reg(train= md2[review_userid==u, names(v_scores)[-1], with=F],
            y= md2[review_userid==u, review_points],
            k=3)$R2Pred
  })

qplot(knn_3nn_r.sq, binwidth=0.05) + 
  theme_bw() + labs(x='Test R^2 (LOOCV)')

# does individual knn rsq relate to number/diversity of reviews?
last_one <-
  merge(data.table(knn_rsq=knn_3nn_r.sq, id=users),
        user_diversity[user_N>=30 & max_v_share<=0.25, 
                       .(id=review_userid, max_v_share, user_N)],
        by='id')

ggplot(last_one, aes(x=user_N, y=knn_rsq)) +
  geom_point(alpha=.5) +
  scale_x_log10() +
  labs(y='R^2, cross validated knn fit', x='Reviews per user') +
  theme_bw()

ggplot(last_one, aes(x=max_v_share, y=knn_rsq)) +
  geom_point(alpha=.5, color='navy') +
  labs(y='R^2, cross validated knn fit', x='maximum varietal share') +
  theme_bw()
