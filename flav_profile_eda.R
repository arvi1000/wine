# data
source('~/Documents/Analytic DB/copy_to_clipboard.R')
clipcopy(v_scores)

# histograms
ggplot(melt(v_scores, id.var='varietal'), aes(x=value)) + 
  geom_bar(binwidth=1) + facet_wrap(~variable, ncol=5) +
  labs(title='Histogram of score by feature', y='frequency count', x='rating') +
  theme_bw()

# dist mat: how far apart are the varietals?
v_dist_mat <- as.matrix(dist(v_scores[, 2:11, with=F], upper=T, diag=T))
dimnames(v_dist_mat) <- list(v_scores$varietal, v_scores$varietal)
v_dist_mat[upper.tri(v_dist_mat)] <- NA
diag(v_dist_mat) <- NA

ggplot(melt(v_dist_mat), aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(label=round(value, 1)), size=4) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_fill_gradient('Eucl. distance', high='#a50f15', low='#fff5f0')
