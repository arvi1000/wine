# among the top wine varietals, do we need to collapse categories?
top_v <- 
  wines_variants[!grepl('blend', wine_variant, ignore.case=T), wine_variant]

upto_first_comma <- function(x) {
  comma_pos <- regexpr(',', x)
  return( ifelse(comma_pos > 0, substring(x, 1, comma_pos-1), x) )
}

v_falloff <- 
  sapply(top_v[1:25],
         function(x) wines_variants[grepl(upto_first_comma(x),
                                          wine_variant, ignore.case=T), N])

max_len <- max(sapply(v_falloff, length))
v_falloff_pct <- lapply(v_falloff, 
                        function(x) c(x / x[1], rep(NA, max_len-length(x))))
v_falloff_pct <- as.data.frame(v_falloff_pct)
v_falloff_pct$x <- 1:max_len

library(reshape2)
ggplot(melt(v_falloff_pct, id.vars='x'),
       aes(x=x, y=value, color=variable)) + 
  geom_point() + geom_line() + theme_bw()




