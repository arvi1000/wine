# library(foreach)
# library(stringr)
# 
# #system("wc -l wine_data/cellartracker.txt")
# #there are 17,881,768 lines in this file!
# 
# # read data
# con <- file('wine_data/cellartracker.txt')
# wine <- readLines(con)
# close(con)
# rm(con)
# 
# # separate field names from values by splitting on colon
# spl <- strsplit(wine, ':')
# 
# # now we have "field/value" pairs in a list. Get rid of list items
# # that aren't two items long (which arise from blank rows)
# spl_len <- unlist(lapply(spl, length))
# spl <- spl[spl_len==2]
# 
# # do we have an exact multiple of 9?
# trunc(length(spl) / 9) == length(spl) / 9
# 
# # index of all fields and values
# field_ind <- seq(from=1, by=2, length.out=length(spl))
# value_ind <- seq(from=2, by=2, length.out=length(spl))
# 
# # vector of all fields and values
# field_data <- unlist(spl)[field_ind]
# value_data <- unlist(spl)[value_ind]
# 
# # vector of indices for beginning of each record
# rec_begin <- which(field_data == 'wine/name')
# 
# # assemble data.frame
# wine_df <- foreach (i=1:9, .combine=cbind) %do%
#   value_data[rec_begin+i-1]
# wine_df <- data.frame(wine_df, stringsAsFactors=FALSE)
# names(wine_df) <- sub('/', '_', field_data[1:9])
# 
# # convert numeric fields
# num_fields <- c('wine_wineId', 'wine_year', 
#                 'review_points', 'review_userId', 'review_time')
# wine_df[, num_fields] <- lapply(wine_df[, num_fields], as.numeric)
# 
# # trim char fields
# chr_fields <- c('wine_name', 'wine_variant', 'review_userName', 'review_text')
# wine_df[, chr_fields] <- lapply(wine_df[, chr_fields], str_trim)
# 
# # remove objs
# rm(list=c('field_data', 'field_ind', 'value_data', 'value_ind', 'wine',
#           'spl', 'spl_len', 'rec_begin', 'i', 'num_fields', 'chr_fields'))
# 
# write.csv(wine_df, 
#           file='wine_data/cellartracker_dataframe.csv', row.names=FALSE)

library(data.table)

wine_dt <- data.table(wine_df)
setnames(wine_dt, tolower(names(wine_dt)))
setkey(wine_dt, wine_wineid)

# strip non standard encoding----
flag_rw <- grep('#', wine_dt$wine_variant)
start_char <- str_locate(wine_dt$wine_variant[flag_rw], '&')[,1]
sort(unique(substr(wine_dt$wine_variant[flag_rw], start_char, start_char+5)))

letter_subs <- rbind(
  c('&#192;?|&#193;?|&#194;?|&#195;?|&#196;?', 'A'),
  c('&#224;?|&#225;?|&#226;?|&#227;?|&#228;?|&#259;?', 'a'),
  c('&#200;?|&#201;?|&#202;?|&#203;?', 'E'),
  c('&#232;?|&#233;?|&#234;?|&#235;?', 'e'),
  c('&#236;?|&#237;?|&#238;?|&#239;?|&#305;', 'i'),
  c('&#241;?', 'n'),
  c('&#242;?|&#243;?|&#244;?|&#246;?|&#248;?', 'o'),
  c('&#199;?', 'C'),
  c('&#231;?|&#263;?', 'c'),
  c('&#252;?|&#369;?', 'u'),
  c('&#287;?', 'g'),
  c('&#345;?', 'r'),
  c('&#353;?', 's'),
  c('&#253;?', 'y'),
  c('&#381;?', 'Z'))

#name
for (i in 1:nrow(letter_subs)) {
  cat('Subbing in', letter_subs[i, 2], '...', '\n')
  
  win_idx <- grep(letter_subs[i, 1], wine_dt$wine_name)
  cat(length(win_idx), 'matches', '\n')
  
  if(length(win_idx)>0) {
    wine_dt$wine_name[win_idx] <-
      gsub(letter_subs[i, 1], letter_subs[i, 2], wine_dt$wine_name[win_idx])
  }
}
#variant
for (i in 1:nrow(letter_subs)) {
  cat('Subbing in', letter_subs[i, 2], '...', '\n')
  
  win_idx <- grep(letter_subs[i, 1], wine_dt$wine_variant)
  cat(length(win_idx), 'matches', '\n')
  
  if(length(win_idx)>0) {
    wine_dt$wine_variant[win_idx] <-
      gsub(letter_subs[i, 1], letter_subs[i, 2], wine_dt$wine_variant[win_idx])
  }
}

write.csv(wine_dt, 
          file='wine_data/cellartracker_dataframe_utf8.csv', row.names=FALSE)