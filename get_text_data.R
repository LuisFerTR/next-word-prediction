get_text_data <- function(zip_url, zip_file, data_dir) {
  # get_text_data: Check if the data is already downloaded and 
  # unzipped otherwise download data zip file and unzipped it.
  
  # zip_url: URL where zip file is stored
  # zip_file: zip file name
  # data_dir: Directory of unzipped zip file
  
  if(!file.exists(paste0("./data/", zip_file))) {
    download(zip_url)
  }
  
  if(!file.exists(data_dir)) {
    setwd("./data")
    unzip(zip_file)
    setwd("../")
  }
}