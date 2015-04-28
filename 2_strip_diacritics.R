#source('cellartracker_to_dataframe.R')

library(data.table)

wine_dt <- data.table(wine_df)
setnames(wine_dt, tolower(names(wine_dt)))
setkey(wine_dt, wine_wineid)

# strip non standard encoding----
letter_subs <- rbind(
  c('&#192;?|&#193;?|&#194;?|&#195;?|&#196;?', 'A'),
  c('&#224;?|&#225;?|&#226;?|&#227;?|&#228;?|&#259;?', 'a'),
  c('&#200;?|&#201;?|&#202;?|&#203;?', 'E'),
  c('&#232;?|&#233;?|&#234;?|&#235;?', 'e'),
  c('&#236;?|&#237;?|&#238;?|&#239;?|&#305;', 'i'),
  c('&#241;?', 'n'),
  c('&#214;?', 'O'),
  c('&#242;?|&#243;?|&#244;?|&#246;?|&#248;?', 'o'),
  c('&#199;?', 'C'),
  c('&#231;?|&#263;?', 'c'),
  c('&#252;?|&#369;?', 'u'),
  c('&#287;?', 'g'),
  c('&#345;?', 'r'),
  c('&#352;?', 'S'),
  c('&#351;?|&#353;?', 's'),
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

# write to file
write.csv(wine_dt, 
          file='wine_data/cellartracker_dataframe_clean.csv', row.names=FALSE)

# clean up
rm(i, win_idx, wine_df, letter_subs)
