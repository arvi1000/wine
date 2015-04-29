md2 <- copy(my_fit$model_data)

# sort by user + random order within users
set.seed(1)
md2 <- md2[order(review_userid, rnorm(nrow(md2))), ]
# assign folds for 2-fold CV 
# (alternate rows, now that we have random sort order)
md2[, fold:=rep_len(1:2, length.out=nrow(.SD)), by=review_userid]

fold1_data <- md2[fold==1,]
fold2_data <- md2[fold==2,]

# model fold 1
fold1_models <- 
  lapply(split(fold1_data, fold1_data$review_userid), function(x) {  
    lm(review_points ~
         red + black + floral + herb + pepper + 
         earth + baking + leather + astringency + body,
       data=x)
})

# global r.squared calc
fold1_data$resids <- unlist(sapply(fold1_models, `[`, 'residuals'))
with(fold1_data[, .(tss=sum((review_points - mean(review_points))^2),
                    rss=sum(resids^2)), by=review_userid],
     1 - ( sum(rss) / sum(tss)) )

# how does it fare on fold2?

# vector of users
users <- unique(fold1_data$review_userid)

# predict fold2 response for user u, from fold1_models
fold2_data$yhat <- unlist(
  sapply(seq_along(users), function(i) {
    predict(fold1_models[[i]], newdata=fold2_data[review_userid==users[i], ])
  })
)

# residuals and r.sq: ack! negative!
fold2_data[, resids:=yhat - review_points]
with(fold2_data[, .(tss=sum((mean(review_points) - review_points)^2),
                    rss=sum(resids^2)), by=review_userid],
     1 - ( sum(rss) / sum(tss)) )

# better for any particular users?
fold2_data[, .(tss=sum((mean(review_points) - review_points)^2),
               rss=sum(resids^2)), by=review_userid][
                 , .(rsq=1-rss/tss)][, pmax(rsq, -1.05)] %>%
  qplot(xlab='Test R^2 (2-fold validation)', binwidth=0.05)
