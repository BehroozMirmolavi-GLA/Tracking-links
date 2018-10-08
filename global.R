library(shiny)
library(shinydashboard)
library(shinyjs)
library(googlesheets)
library(googleAuthR)
library(httr)
library(htmltools)
library(jsonlite)
library(shinycssloaders)
library(tidyverse)
library(DT)

options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/firebase")
service_token <-
  gar_auth_service(json_file = "my-project-1531324857394-firebase-adminsdk-f4jhu-819f25b2a5.json")

# test <- drive_find("test.csv")
# drive_download(test, path = "test.csv", type = "csv",overwrite = TRUE)
# responses <- read.csv("test.csv", row.names = T)

test <- gs_key("")
test2 <- as.data.frame(gs_read_csv(test))
responses <- test2
rm(test2)

# Get table metadata. For now, just the fields
# Further development: also define field types
# and create inputs generically
GetTableMetadata <- function() {
  fields <- c(
    date = "Date",
    email = "Email",
    url = "URL",
    source = "Source",
    medium = "Medium",
    campaign = "Campaign",
    content = "Content",
    shorturl = "shortURL",
    longurl = "longURL"
  )
  
  result <- list(fields = fields)
  return (result)
}


#C
CreateData <- function(data) {
  
  data <- CastData2(data)
  
  responses <<- rbind(responses, data)

}

#R
ReadData <- function() {
  if (exists("responses")) {
    responses
  } else
  {
    Sys.sleep(0.1)
    responses
  }
}



#U
UpdateData <- function(data) {
  
  datar <- CastData2(data)
  
}




# Cast from Inputs to a one-row data.frame
CastData <- function(data) {
  datar <- data.frame(
    date = data["date"],
    email = data["email"],
    url = data["url"],
    source = data["source"],
    medium = data["medium"],
    campaign = data["campaign"],
    content = data["content"],
    shorturl = data["shorturl"],
    longurl = data["longurl"],
    stringsAsFactors = FALSE
  )
  
  return (datar)
}

CastData2 <- function(data) {
  madelink <- paste0(
    data["url"]
    ,
    "/?utm_source=",
    data["source"]
    ,
    "&utm_medium=",
    data["medium"]
    ,
    "&utm_campaign=",
    data["campaign"]
    ,
    "&utm_content=",
    data["content"]
  )

  shortlink <- if (grepl("london.gov.uk", madelink)) {
  
  a <-  POST(
      'https://firebasedynamiclinks.googleapis.com/v1/shortLinks?',
      add_headers(
        "Content-Type" = "application/json",
        "Authorization" = paste("Bearer", service_token$credentials$access_token)
      ),
      body = jsonlite:::toJSON(
        list(
          dynamicLinkInfo = list(dynamicLinkDomain = "londongov.page.link",
                                 link = madelink),
          suffix = list(option = "SHORT")
        ),
        pretty = T,
        auto_unbox = T
      )
    )

   as.character(httr:::content(a)$`shortLink`)
   } else {
   ""
    }

  
  datar <- data.frame(
    date = format(Sys.Date(), "%Y-%m-%d"),
    email = data["email"],
    url = data["url"],
    source = data["source"],
    medium = data["medium"],
    campaign = data["campaign"],
    content = data["content"],
    shorturl = shortlink,
    longurl = madelink,
    stringsAsFactors = FALSE
  )
  
  return (datar)
}


# Return an empty, new record
CreateDefaultRecord <- function() {
  mydefault <- CastData(
    list(
      date = format(Sys.Date(), "%Y-%m-%d")
      ,
      email = ""
      ,
      url = ""
      ,
      source = "Source"
      ,
      medium = "Medium"
      ,
      campaign = ""
      ,
      content = ""
      ,
      shorturl = ""
      ,
      longurl = ""
    )
  )
  return (mydefault)
}

# Fill the input fields with the values of the selected record in the table
UpdateInputs <- function(data, session) {
  updateTextInput(session, "date", value = unname(data["date"]))
  updateTextInput(session, "email", value = unname(data["email"]))
  updateTextInput(session, "url", value = unname(data["url"]))
  updateSelectInput(session,
                    "source",
                    label = unname(data["source"]),
                    selected = unname(data["source"]))
  updateTextInput(session, "medium", value = unname(data["medium"]))
  updateTextInput(session, "campaign", value = unname(data["campaign"]))
  updateTextInput(session, "content", value = unname(data["content"]))
  updateTextInput(session, "shorturl", value = unname(data["shorturl"]))
  updateTextInput(session, "longurl", value = unname(data["longurl"]))
}

choices <- data.frame(
  sourcechoice = c(
    'Facebook',
    'Facebook',
    'Facebook',
    'Facebook',
    'Facebook',
    'Facebook',
    'twitter',
    'twitter',
    'twitter',
    'twitter',
    'twitter',
    'instagram',
    'instagram',
    'email',
    'email',
    'email',
    'email',
    'search',
    'display',
    'display',
    'display'
  ),
  mediumchoice = c(
    'post',
    'link-ad',
    'boosted-ad',
    'video',
    'video-ad',
    'carousel-ad',
    'post',
    'photo',
    'video',
    'link-ad',
    'video-ad',
    'story',
    'profile',
    'newsletter',
    'welcome',
    'campaign',
    'mail-from-the-mayor',
    'ppc',
    'banner',
    'text',
    'image'
  )
)

createLink <- function(a) {
  paste0('<a href="',as.character(a),'" target="_blank" class="btn btn-primary">','Link</a>')
}
