
library(stringr)
library(microbenchmark)

texts <- replicate(1e5, paste(sample(letters, 10), collapse = " "))
                   
microbenchmark(
 word = word(texts, 1),
 sub = sub("(\\w+).*", "\\1", texts),
 strsplit = sapply(strsplit(texts, "\\s+"), `[`, 1),
 str_extract = str_extract(texts, "^\\w+"),
 times = 10
)
