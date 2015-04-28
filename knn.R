library(FNN)
qwe <-
  knn.reg(train= fold1_data[review_userid==259, names(v_scores)[-1], with=F],
          test=  fold2_data[review_userid==259, names(v_scores)[-1], with=F],
          y= fold1_data[review_userid==259, review_points],
          k=3)

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

scree

