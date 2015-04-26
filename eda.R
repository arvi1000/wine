library(data.table)
library(ggplot2)

wine_utf8 <- read.csv('wine_data/cellartracker_dataframe_utf8.csv',
                      colClasses=c('character', 'numeric', 'character', 
                                   'integer', 'integer', 'numeric', 
                                   'numeric', 'character', 'character'),
                      stringsAsFactors=FALSE)
wine_dt <- data.table(wine_utf8)
setkey(wine_dt, wine_wineid)

# CDF by wine----
freq <- wine_dt[, .N, by=wine_wineid][order(-N), N]
cdf_df <- data.frame(y=cumsum(freq)/nrow(wine_dt), x=c(1:length(freq)))
cdf_plot <- ggplot(cdf_df, aes(x=x/1000, y=y)) + geom_line() + theme_bw()

cdf_plot + 
  xlab('Number of Unique Bottles (k)') + ylab('Proportion of Total Reviews') +
  ggtitle('CDF of CellarTracker dataset (n=1.8M)')
#ggsave(filename='cdf_bottle.png', width=4, height=3.5, units='in', dpi=150)

#The top 5000 bottles cover 20% of the data set
cdf_df[5000,]

# The number 1, 100, 1000, 2500 and 5000 most reviewed bottles have this
# many ratings
freq[c(1,100,1000,2500,5000)]

# CDF by user----
freq_u <- wine_dt[, .N, by=review_userid][order(-N), N]
cdf_df_u <- data.frame(y=cumsum(freq_u)/nrow(wine_dt), x=c(1:length(freq_u)))
cdf_plot_u <- ggplot(cdf_df_u, aes(x=x/1000, y=y)) + geom_line() + theme_bw()

cdf_plot_u + 
  xlab('Number of Reviewers (k)') + ylab('Proportion of Total Reviews') +
  ggtitle('CDF of CellarTracker dataset (n=1.8M)')
ggsave(filename='cdf_user.png', width=4, height=3.5, units='in', dpi=100)

#The top 100 users cover ~20% of the data set
cdf_df_u[100,]
cdf_df_u[5000,]

# nth user has left r ratings. Whoa, some nut left 30K+ ratings? Was it anon?
freq_u[c(1,10,100,250,500)]

# No, but there are several anons. When we aggregate them, that's 103K
wine_dt[, .N, by=list(review_username)][order(-N), ][1:100]

# How many users left at least 30 reviews? 6725!
min(which(freq_u<31))
min(which(freq_u<501))

# ratings by bottle----
top_bottles <- 
  wine_dt[, .N, by=list(wine_wineid, wine_name, wine_variant)][order(-N)]

wines5k <- 
  wine_dt[, list(.N,
                 mu=mean(review_points, na.rm=T),
                 sigma=sd(review_points, na.rm=T)),
          by=list(wine_wineid, wine_name)][order(-N)][1:5000]

ggplot(wines5k) +
  geom_point(aes(x=mu, y=sigma), alpha=.2, color='darkred') + theme_bw() +
  ggtitle('Mean vs Std Dev of Rating for Top 5k Most-Rated Wines')
  #+scale_color_continuous(low='navy', high='orange')

ggplot(wines5k[1:500,]) +
  geom_point(aes(x=mu, y=sigma, size=N), alpha=.6, shape=1, color='darkred') + 
  theme_bw() +
  scale_size_continuous('Number of ratings', range=c(1,15)) +
  ggtitle('Mean vs Std Dev of Rating for Top 500 Most-Rated Wines')

# ratings by type----
wines_variants <- 
  wine_dt[, list(.N,
                 mu=mean(review_points, na.rm=T),
                 sigma=sd(review_points, na.rm=T)),
          by=wine_variant]
wines_variants <- wines_variants[order(-N)]

n_thresh <- match(TRUE, wines_variants$N < 50) - 1

ggplot(wines_variants[1:n_thresh,]) +
  geom_point(aes(x=mu, y=sigma, color=cut_number(N,5)),
             alpha=.7, size=3) +    
  theme_bw() +
  #scale_size_continuous('Number of ratings', range=c(3,15)) +
  scale_color_brewer(name='Number of Ratings', type='seq', palette='YlGnBu') +
  ggtitle('Mean vs Std Dev of Rating for Types with N>=50 ratings')

# dist of user mean ratings----
user_baseline <- 
  wine_dt[, list(.N,
                 mu=mean(review_points, na.rm=T),
                 sigma=sd(review_points, na.rm=T)),
          by=list(review_userid)][N>=50 & !is.na(sigma)][order(-N)]

plot_thresh <- 200
ggplot(user_baseline[N>=plot_thresh,],
       aes(x=mu, y=sigma, color=cut_number(N, 5))) +
  geom_point(alpha=.5) +
  stat_smooth(method='lm', se=FALSE) +
  theme_bw() +
  scale_color_brewer(name='Number of Ratings', type='seq', palette='YlOrBr') +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  ggtitle(paste0('Mean vs Std Dev of Rating for Users with N>=', 
                  plot_thresh, ' ratings'))

wine_dt[review_userid %in% user_baseline[N>=200, review_userid], 
        .N, by=wine_variant][order(-N)][1:100]

# popular bottles -----

wine_count <-
  wine_dt[, list(.N, Unq=length(unique(review_userid))),
          by=list(wine_wineid, wine_name)][order(-N)]