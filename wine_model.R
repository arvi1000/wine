library(data.table)
library(ggplot2)
library(magrittr)
library(reshape2)

# read in data: wine reviews, and varietal features scores
wine_dt <- fread('wine_data/cellartracker_dataframe_clean.csv')
v_scores <- fread('varietal_scores.csv')

# basic descr stats
wine_dt[, .N]
wine_dt[, length(unique(review_userid))]
wine_dt[, length(unique(wine_wineid))]
wine_dt[, length(unique(wine_variant))]
wine_dt[!is.na(review_points), .(.N, pct=.N/nrow(wine_dt))]
round(c(summary(wine_dt$review_points)[-7], 
        SD=sd(wine_dt$review_points, na.rm=T)), 2)

# user mean/var
wine_dt[, .(.N, mu=mean(review_points, na.rm=T), 
            sigma=sd(review_points, na.rm=T)), by=review_userid][
              order(-N), ][1:5000,] %>%
  ggplot(aes(x=mu, y=sigma)) +
  geom_point(alpha=.1, color='navy') + #geom_density2d(color='black') +
  theme_bw() +
  ggtitle('Mean vs Std Dev of Rating\nTop 5k Most-Active Users')

# wine mean/var
wine_dt[, .(.N, mu=mean(review_points, na.rm=T), 
            sigma=sd(review_points, na.rm=T)), by=wine_wineid][
              order(-N), ][1:5000,] %>%
  ggplot(aes(x=mu, y=sigma)) +
  geom_point(alpha=.1, color='darkred') + #geom_density2d(color='black') +
  theme_bw() +
  ggtitle('Mean vs Std Dev of Rating\nTop 5k Most Review Wines')

# get unique wine_variants
wvs <- unique(wine_dt$wine_variant)

# find matching wine_variant values by varietal, while
# throwing out blends of two named grapes (e.g. "Cabernet-Syrah")
# which are distinguished by the presences of dashes
my_variants <-
  sapply(v_scores$varietal, 
         function(x) wvs[grepl(x, wvs, ignore.case=T) & !grepl('\\-', wvs)])
rm(wvs) # clean up

# in the full dataset, how many wines are among these? (a little under half)
wine_dt[, .(.N, pct=.N/nrow(wine_dt)), 
        by=.(wine_variant %in% unlist(my_variants))]

# create variant/varietal lookup
vv_lkp <-
  data.frame(wine_variant=unname(unlist(my_variants)),
             varietal=rep(v_scores$varietal, sapply(my_variants, length)),
             stringsAsFactors=F)

# subset to variants of interest (reviews with score), 
# and merge corresponding varietal
wine <- wine_dt[wine_variant %in% unlist(my_variants) & !is.na(review_points),]
wine <- merge(wine, vv_lkp, by='wine_variant')
rm(vv_lkp)

# relevant review counts by varietal
wine[, .N, by=varietal][order(-N)]

# counts by user
wine[, user_N:=.N, by=review_userid]

# diversity: total reviews, total varietals, max share of reviews
user_diversity <- 
  wine[, .(v_share=.N/user_N), by=.(review_userid, varietal)][
    , .(v_types=length(unique(varietal)), max_v_share=max(v_share), user_N=.N), 
    by=review_userid]

# How many users/reviews with at least 30 reviews/users and at most
# x% of reviews coming from single varietal
chk_points <- seq(.1, .5, .05)
lapply(chk_points, function(x) {
  user_diversity[user_N>=30 & max_v_share <= x,
                 .(users=.N, reviews=sum(user_N))]
  }) %>%
  rbindlist() %>%
  cbind(max_v_share=chk_points, .)
  
#   %>% melt(id.var='max_v_share') %>%
#   ggplot(aes(x=max_v_share, y=value)) +
#   geom_point() + geom_line() +
#   facet_wrap(~variable, scales='free', nrow=2)

rm(chk_points)

# global r sq func
piece_fit <- function(max_v_share_thresh=0.25,
                        min_reviews=30,
                        which_scores=1:10,
                        which_varietals=1:16) {
  
  # assemble basic data for the review set of interest:
  model_data <- merge(
    
    # a) data from users meeting min_reviews/max_v_share_thresh
    user_diversity[user_N>= min_reviews &  max_v_share <= max_v_share_thresh, 
                   .(review_userid)],
    
    # b) reviews of varietals of interest
    wine[varietal %in% names(my_variants)[which_varietals],
         .(review_userid, review_username, 
           wine_wineid, varietal, review_points)],
    
    by='review_userid')
  
  # merge in (selected) feature scores
  model_data <- merge(model_data, 
                      v_scores[, c(1, which_scores+1), with=F], 
                      by='varietal')
  
  # assemble lm formula
  formula_str <-
    paste('review_points ~ ', 
          paste(names(v_scores)[which_scores+1], collapse=' + '))
  
  # 1 model per user:
  #cat('Fitting', length(unique(model_data$review_userid)), 'models...')
  cat('Fitting models...')
  split_models <- lapply(split(model_data, model_data$review_userid),
                         function(x) lm(as.formula(formula_str), data=x))
  cat('done\n')
  
  # global r.squared calc
  model_data$resids <- unlist(sapply(split_models, `[`, 'residuals'))
  tss_rss_by_model <- 
    model_data[, .(tss=sum((mean(review_points) - review_points)^2),
                   rss=sum(resids^2)),
               by=review_userid]
  global_r.squared <- with(tss_rss_by_model, 1 - ( sum(rss) / sum(tss)) )
  
  return(list(model_data=model_data,
              split_models=split_models,
              tss_rss_by_model=tss_rss_by_model,
              global_r.squared=global_r.squared))
}


# global_r.squared w default params: 0.1164558
my_fit <- piece_fit()
my_fit$global_r.squared

# when we are more tolerant of varietal-homogenous tasting sets, r_sq is 
# slightly worse (as expected)
piece_fit(max_v_share_thresh=.3)$global_r.squared

# which varietals are least well predicted? 
  # 1: try leaving each one out (loo=leave one out)
  loo_v <- lapply(1:16, function(x) setdiff(1:16, x))
  loo_v_rsq <- 
    lapply(loo_v, function(x) piece_fit(which_varietals=x)$global_r.squared)
  loo_v_df <-
    data.table(varietal=v_scores$varietal, loo_rsq=unlist(loo_v_rsq))
  loo_v_df[order(loo_rsq),]
  
  # 2: MSE by varietal
  my_fit$model_data[, .(mse=mean(resids^2)), by=varietal][order(-mse),]

# which features/scores do the least work
loo_s <- lapply(1:10, function(x) setdiff(1:10, x))
loo_s_rsq <- 
  lapply(loo_s, function(x) piece_fit(which_scores=x)$global_r.squared)
data.frame(feature=names(v_scores)[-1], loo_rsq=unlist(loo_s_rsq))

# how far apart are the varietals?
v_dist_mat <- as.matrix(dist(v_scores[, 2:11, with=F], upper=T, diag=T))
dimnames(v_dist_mat) <- list(v_scores$varietal, v_scores$varietal)
v_dist_mat[upper.tri(v_dist_mat)] <- NA
diag(v_dist_mat) <- NA

ggplot(melt(v_dist_mat), aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(label=round(value, 1)), size=4) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_fill_gradient('Eucl. distance', high='#a50f15', low='#fff5f0')
