# relevant review counts by varietal
wine[, .N, by=varietal][order(-N)]
# ggplot(wine[, .N, by=.(varietal, wine_variant)][order(-N)],
#        aes(x=factor(varietal, levels=varietal), y=N, fill=varietal)) +
#   geom_bar(stat='identity', color='black') +
#   scale_y_log10() +
#   scale_fill_discrete(guide=F) +
#   coord_flip()

# How many users have at least x reviews?
wine[, .(user_N=.N), by=review_userid][, .N, by=user_N][
  order(-user_N), .(user_N, cumul_users=cumsum(N))] %>%
  ggplot(aes(x=user_N, y=cumul_users)) +
  geom_line() +
  scale_x_log10()


# tile plot
user_diversity[, .N, by=.(mxvs=round(max_v_share*20, 0)/20, v_types)] %>%
  ggplot(aes(x=v_types, y=mxvs, fill=log(N))) + 
  geom_tile()


wine[user_N>=100, 
     .(mu=mean(review_points), sigma=sd(review_points), .N), 
     by=varietal] %>%
  ggplot(aes(x=mu, y=sigma)) +
  geom_point(aes(color=cut_number(N, 5)), size=5) +
  geom_text(aes(label=varietal), size=4, hjust=-.2) +
  xlim(85,92) +
  scale_color_brewer(name='Number of Ratings', type='seq', palette='YlGnBu') +
  ggtitle('Mean vs Std Dev of Rating by Varietal
          among users with >=100 ratings')