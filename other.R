library(shiny)
library(shinyjs)
#library(googledrive)
library(googlesheets)
library(googleAuthR)
library(httr)
library(htmltools)
library(jsonlite)
library(shinycssloaders)

options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/firebase")
service_token <-
  gar_auth_service(json_file = "my-project-1531324857394-firebase-adminsdk-f4jhu-819f25b2a5.json")


b <-  GET('https://firebasedynamiclinks.googleapis.com/v1/https%3A%2F%2Flondongov.page.link%2FcWUb/linkStats?durationDays=7',
          add_headers(
            "Content-Type"="application/json",
            "Authorization"= paste("Bearer", service_token$credentials$access_token)),
          body='{
          "dynamicLinkInfo": {
          "dynamicLinkDomain": "londongov.page.link"
          },
          "suffix": {
          "option": "SHORT"
          }           }'
)

parsed_req <- httr::content(b)
fieldnum <-3

df <- data.frame(matrix(as.character(unlist(parsed_req$linkEventStats)),ncol = fieldnum, byrow = T))
colnames(df) <- unique(names(unlist(parsed_req$linkEventStats)))
