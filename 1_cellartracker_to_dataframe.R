library(foreach)
library(stringr)

#system("wc -l wine_data/cellartracker.txt")
#there are 17,881,768 lines in this file!

# read data
wine <- readLines('wine_data/cellartracker.txt')

# separate field names from values by splitting on colon
spl <- strsplit(wine, ':')

# now we have "field/value" pairs in a list. Get rid of list items
# that aren't two items long (which arise from blank rows)
spl_len <- unlist(lapply(spl, length))
spl <- spl[spl_len==2]

# do we have an exact multiple of 9? should be TRUE
trunc(length(spl) / 9) == length(spl) / 9

# index of all fields and values
field_ind <- seq(from=1, by=2, length.out=length(spl))
value_ind <- seq(from=2, by=2, length.out=length(spl))

# vector of all fields and values
field_data <- unlist(spl)[field_ind]
value_data <- unlist(spl)[value_ind]

# vector of indices for beginning of each record
rec_begin <- which(field_data == 'wine/name')

# assemble data.frame
wine_df <- foreach (i=1:9, .combine=cbind) %do%
  value_data[rec_begin+i-1]
wine_df <- data.frame(wine_df, stringsAsFactors=FALSE)
names(wine_df) <- sub('/', '_', field_data[1:9])

# convert numeric fields
num_fields <- c('wine_wineId', 'wine_year', 
                'review_points', 'review_userId', 'review_time')
wine_df[, num_fields] <- lapply(wine_df[, num_fields], as.numeric)

# trim char fields
chr_fields <- c('wine_name', 'wine_variant', 'review_userName', 'review_text')
wine_df[, chr_fields] <- lapply(wine_df[, chr_fields], str_trim)

# remove objs
rm(list=c('field_data', 'field_ind', 'value_data', 'value_ind', 'wine',
          'spl', 'spl_len', 'rec_begin', 'i', 'num_fields', 'chr_fields'))

# write to file
write.csv(wine_df, 
          file='wine_data/cellartracker_dataframe.csv', row.names=FALSE)