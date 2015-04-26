library(data.table)
library(ggplot2)
library(magrittr)
library(reshape2)

# read in data: wine reviews, and varietal features scores
wine_dt <- fread('wine_data/cellartracker_dataframe_clean.csv')
v_scores <- fread('varietal_scores.csv')

# get unique wine_variants
wvs <- unique(wine_dt$wine_variant)

# find matching wine_variant values by varietal
# raw:
sapply(v_scores$varietal, grep, wvs, ignore.case=T, value=T)
# ...and throwing out blends of two named grapes (e.g. "Cabernet-Syrah")
my_variants <-
  sapply(v_scores$varietal, 
         function(x) wvs[grepl(x, wvs, ignore.case=T) & !grepl('\\-', wvs)])

# don't need this vector anymore
rm(wvs) 

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

# assemble basic data for the review set of interest
model_data <-
  merge(user_diversity[user_N>=30 & max_v_share <= 0.25, .(review_userid)],
        wine[, .(review_userid, review_username, 
                 wine_wineid, varietal, 
                 review_points)],
        by='review_userid')

# merge in feature scores
model_data <- merge(model_data, v_scores, by='varietal')


# 1 model per user!
split_models <- 
  lapply(split(model_data, model_data$review_userid),
         function(x) lm(review_points ~ 
                          red + black + floral + herb + pepper + earth + 
                          baking + leather + astringency + body,
                        data=x))

# global r.squared calc
model_data$resids <- unlist(sapply(split_models, `[`, 'residuals'))
tss_rss_by_model <- 
  model_data[, .(tss=sum((review_points - mean(review_points))^2),
                 rss=sum(resids^2)),
             by=review_userid]
global_r.squared <- with(tss_rss_by_model, 1 - ( sum(rss) / sum(tss)) )

6^10^16

