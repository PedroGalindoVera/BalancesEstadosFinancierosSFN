source("scripts/analisisPaginaWebEnlaces.R")
#source("analisisPaginaWebEnlaces.R")

# Set the enlaces of the file
enlaces <- analisisPaginaWeb()

# Verificamos la existencia del paquete necesario
if (!require("httr")) {
  install.packages("httr")
}

# Load the httr package
library(httr)

# Retrieve the metadata of the file
response <- sapply(enlaces, function(x) httr::HEAD(x))

# Print the metadata
str(response)

# Elegir unicamente los url de descarga
url <- unlist(response["url",])

# Define a function to extract the content-type attribute from a list
get_content_type <- function(x) {
  if (is.list(x) && "content-type" %in% names(x)) {
    return(x$`content-type`)
  } else {
    return(NA)
  }
}

# Apply the function to each element of the response list
content_types <- lapply(response, get_content_type)

# Print the content-types
print(unique(content_types))

# Load the tools package
library(tools)

# Determine the file extensions
extensions <- unique(sapply(url, function(x) file_ext(x)))

# Print the file extensions
print(extensions)
