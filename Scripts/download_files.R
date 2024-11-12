# PURPOSE:
# 1) To summarize CMIIP6 files by file identifiers
# 2) To download CMIIP6 files

library(stringr)

# Read in url + filename csv
urls <- read.csv("./Data/updated_noaa_signed_urls.csv")

# Breakdown/summarize filenames ----
# specify data frame 
file.sum <- data.frame()

# run loop
for(ii in 1:nrow(urls)){
  # identify locations of "/" in file name
  lst <- gregexpr('/', urls$name[ii], fixed = TRUE)
  
  # extract information based on "/" positions
  region <- substring(urls$name[ii], sapply(lst, `[`, 1) + 1, sapply(lst, `[`, 2) - 1)
  pathway <- substring(urls$name[ii], sapply(lst, `[`, 2) + 1, sapply(lst, `[`, 3) - 1)
  model <- substring(urls$name[ii], sapply(lst, `[`, 3) + 1, sapply(lst, `[`, 4) - 1)
  variable <- substring(urls$name[ii], sapply(lst, `[`, 4) + 1, sapply(lst, `[`, 5) - 1)

  # identify locations of "_" after "/"
  dat <- substring(urls$name[ii], sapply(lst, `[`, 5))
  
  lst2 <- gregexpr('_', dat, fixed = TRUE)
  
  # ensemble model run file names are different, specify position of information based on this
 
  if(model == "ensemble"){
    perc.stat <- ifelse(str_detect(dat, "perc") == TRUE, "perc", "stats")
    model.run = "ensemble"
    final.raw = NA
  }else{
    perc.stat <- NA
    model.run <- substring(dat, sapply(lst2, `[`, 1) + 1, sapply(lst2, `[`, 2) - 1)
    final.raw <- ifelse(str_detect(dat, "final") == TRUE, "final", "raw")
  }
  
  # extract the rest of the information using str_detect
  sd.ba <- ifelse(str_detect(dat, "sd+") == TRUE, "sd+ba", NA)
  depth <- ifelse(str_detect(dat, "surface") == TRUE, "surface", "bottom")
  
 
  
  # bind information into summary table
  file.sum <- rbind(file.sum, data.frame(index = row(urls)[ii],
                                         region,
                                         pathway,
                                         model,
                                         variable,
                                         model.run,
                                         sd.ba,
                                         final.raw,
                                         depth,
                                         perc.stat))
  
}

# save csv
file.sum %>%
  group_by(pathway, model, variable, depth) %>%
  reframe(total = n()) -> file.sum2

write.csv(file.sum2, "./Output/file.summary.csv")

urls$name[2] -> tt
str_after_last(tt, "/") -> kk


# Download files ----
# set path/folder to store downloaded models
path <- c("//akc0ss-n086/RACE_SAP/CMIIP6 downscaled models/")

# Loop through file names/downloads
for(ii in 135:nrow(urls)){
  print(paste0("Downloading file #", row(urls)[ii]))
  
  # identify locations of "/" in file name
  lst <- gregexpr('/', urls$name[ii], fixed = TRUE)
  
  # extract information based on "/" positions
  region <- substring(urls$name[ii], sapply(lst, `[`, 1) + 1, sapply(lst, `[`, 2) - 1)
  pathway <- substring(urls$name[ii], sapply(lst, `[`, 2) + 1, sapply(lst, `[`, 3) - 1)
  
  urls$name[ii] -> tt
  str_after_last(tt, "/") -> kk
  
  download.file(urls$url[ii], destfile = paste0(path, region, "_", pathway, "_", kk))
}

options(timeout = 5000)


