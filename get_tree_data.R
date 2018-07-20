# Use some important libraries for accessing the API
library(httr)
library(jsonlite)

# It appears that treating strings as factors is problematic.  This string helps.
options(stringsAsFactors = FALSE)

# Store the information of the tree API, which is on NOAA's website.
url  <- "https://www.ncdc.noaa.gov/"
path <- "paleo-search/study/search.json?dataPublisher=NOAA&dataTypeId=18&headersOnly=true"

# Now we download the information from the API
raw.result <- GET(url = url, path = path)
