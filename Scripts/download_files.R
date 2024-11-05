# PURPOSE:
# 1) To summarize CMIIP6 files by file identifiers
# 2) To download CMIIP6 files

library(stringr)

# Set path and read in url + filename csv

path <- c("//akc0ss-n086/RACE_SAP/CMIIP6 downscaled models/")

urls <- read.csv("./Data/full_noaa_dl_url_list.csv")

# Try breaking filenames down
file.sum <- data.frame()

for(ii in 1:nrow(try)){
  # identify locations of "/" in file name
  lst <- gregexpr('/', try$name[ii], fixed = TRUE)
  
  # extract information based on "/" positions
  region <- substring(try$name[ii], sapply(lst, `[`, 1) + 1, sapply(lst, `[`, 2) - 1)
  pathway <- substring(try$name[ii], sapply(lst, `[`, 2) + 1, sapply(lst, `[`, 3) - 1)
  model <- substring(try$name[ii], sapply(lst, `[`, 3) + 1, sapply(lst, `[`, 4) - 1)
  variable <- substring(try$name[ii], sapply(lst, `[`, 4) + 1, sapply(lst, `[`, 5) - 1)

  # replace all "_" with "/"
  dat <- substring(try$name[1], sapply(lst, `[`, 5))
  
  # identify locations of "/" in file name
  lst2 <- gregexpr('_', dat, fixed = TRUE)
  
  # extract information based on "/" positions
  model.name <- str_extract(str_extract(dat, '(?<=/).*(?=\\_)'), '(?<=).*(?=\\_r)')
  model.run <- substring(dat, sapply(lst2, `[`, 1) + 1, sapply(lst2, `[`, 2) - 1)
  sd.ba <- substring(dat, sapply(lst2, `[`, 2) + 1, sapply(lst2, `[`, 3) - 1)
  final.raw <- str_detect(dat, " sd+ba ")
  depth <- substring(dat, sapply(lst2, `[`, 5) + 1, sapply(lst2, `[`, 7) - 1)
  last.bit <- substring(dat, sapply(lst2, `[`, 7)+1)
  
  # bind information into summary table
  file.sum <- rbind(file.sum, data.frame(region = region,
                                         pathway = pathway,
                                         model = model,
                                         variable = variable,
                                         model.name = model.name,
                                         model.run,
                                         sd.ba,
                                         final.raw,
                                         depth,
                                         last.bit))
  
}

try <- urls[1:4,]
region <- stringr::str_extract(try$name, '(?<=/).*(?=\\/ssp)')
pathway <- stringr::str_extract(try$name, '(?<=/).*(?<=/).')

lst1 <- gregexpr('/', try$name, fixed = TRUE)
substring(try$name, sapply(lst1, `[`, 3) + 1, sapply(lst1, `[`, 4) - 1)

#[1] "0.0"  "0.02" "0.1" 