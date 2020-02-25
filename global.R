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
library(jsonlite)

#t <- sheets_auth()
#saveRDS(t, "token.rds")

gs_auth(token = "token.rds")
test <- gs_key("1Dt3OkkZUiutTXxjPWI00uNiNX0A57sXIVw-vS9PW0g0")
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
  data2 <- CastData2(data)
  
  responses <<- rbind(responses, data2)
  
}

#R
ReadData <- function() {
  if (exists("responses")) {
    responses %>%
      mutate(
        source = factor(source)
        ,
        medium = factor(medium)
        ,
        campaign = factor(campaign)
      )
  } else
  {
    Sys.sleep(0.1)
    responses %>%
      mutate(source = factor(source))
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
  string <- as.character(data["url"])
  madelink <-
    gsub(
      " ",
      "",
        if (grepl("\\?",string)) {
        paste0(
        gsub("\\?","",str_extract(string,".*\\?"))
        ,
        "?"  
        ,
        "utm_source=",
        str_replace_all(data["source"], "[^[:alnum:]]", "")
        ,
        "&utm_medium=",
        str_replace_all(data["medium"], "[^[:alnum:]]", "")
        ,
        "&utm_campaign=",
        str_replace_all(data["campaign"], "[^[:alnum:]]", "")
        ,
        "&utm_content=",
        str_replace_all(data["content"], "[^[:alnum:]]", "")
        ,
        "&"
        ,
        gsub("\\?","",str_extract(string,"\\?.*"))
        )
        } else {
        paste0(
        string
        ,
        "?"
        ,
        "utm_source=",
        str_replace_all(data["source"], "[^[:alnum:]]", "")
        ,
        "&utm_medium=",
        str_replace_all(data["medium"], "[^[:alnum:]]", "")
        ,
        "&utm_campaign=",
        str_replace_all(data["campaign"], "[^[:alnum:]]", "")
        ,
        "&utm_content=",
        str_replace_all(data["content"], "[^[:alnum:]]", "")  
        ) 
        },
      fixed = T
    )
  
  shortlink <- if (grepl("", madelink)) {
    a <- POST(
      url = 'https://api-ssl.bitly.com/v4/shorten'
      ,add_headers('Authorization' = 'Bearer 2c614489ecf9895ef1bb383c70c23324f0972fed'
                   ,'Content-Type' = 'application/json')
      ,body = jsonlite:::toJSON(
        list(
          long_url = madelink
        ),
        auto_unbox = T
      )
    )
    
    if (length(as.character(httr:::content(a)$link)) == 0) {
      "Invalid URL for bit.ly"
    } else{
      as.character(httr:::content(a)$link)
    }
    
    
  } else {
    ""
  }
  
  
  datar <- data.frame(
    date = format(Sys.Date(), "%Y-%m-%d"),
    email = data["email"],
    url = data["url"],
    source = data["source"],
    medium = data["medium"],
    campaign = gsub(" ", "", data["campaign"], fixed = T),
    content = gsub(" ", "", data["content"], fixed = T),
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
  updateSelectizeInput(session,
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
    'facebook',
    'facebook',
    'facebook',
    'facebook',
    'facebook',
    'facebook',
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
    'display',
    'LinkedIn',
    'LinkedIn',
    'Snapchat',
    'Website'
  ),
  mediumchoice = c(
    'post',
    'link-ad',
    'boosted-ad',
    'video',
    'video-ad',
    'carousel-ad',
    'post',
    'ad',
    'story',
    'profile',
    'newsletter',
    'welcome',
    'campaign',
    'mail-from-the-mayor',
    'ppc',
    'banner',
    'text',
    'image',
    'post',
    'link-ad',
    'Snapchat'
    ,'Referral'
  )
)

createLink <- function(a) {
  paste0(
    '<a href="',
    as.character(a),
    '" target="_blank" class="btn btn-primary">',
    'Link</a>'
  )
}
