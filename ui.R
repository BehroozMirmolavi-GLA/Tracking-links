ui <- dashboardPage(
  #header
  dashboardHeader(title = "Tracking Links"),
  
  ## Sidebar content
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Build a link",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    menuItem("Find a link", tabName = "widgets", icon = icon("th"))
  )),
  
  ## Body content
  dashboardBody(
    actionButton("browser", "browser"),
    tags$script("$('#browser').hide();"),
    tags$script(src = "onclickcopy.js"),
    
    tabItems(
      # First tab content
      tabItem(
        tabName = "dashboard",
        shinyjs::useShinyjs(),
        br(),
        br(),
        fluidRow(
          # column(width = 1,
          # shinyjs::disabled(textInput("date", "Date", "0"))),
          column(width = 4,
                 textInput("email", "Email", "")),
          column(width = 4,
                 textInput("url", "URL", "")),
          column(
            width = 4,
            selectInput(
              "source",
              "Source",
              c(
                "facebook" = "facebook",
                "twitter" = "twitter",
                "instagram" = "instagram",
                "email" = "email",
                "search" = "search",
                "display" = "display",
                "LinkedIn" = "LinkedIn",
                "Snapchat" = "Snapchat",
                "Website" = "Website"

              ),
              selected = "twitter"
            )
          )),
        fluidRow(
          column(width = 4,
                 htmlOutput("mediumUI")),
          column(width = 4,
                 textInput("campaign", "Campaign", "")),
          column(width = 4,
                 textInput("content", "Content", ""))#,
          #column(width = 1,
          #shinyjs::disabled(textInput("shorturl", "shortURL", "0"))),
          # column(width = 1,
          #        shinyjs::disabled(textInput("longurl", "longURL", "0")))
        ) ,
        
        #action buttons
        fluidRow(column(
          width = 6,
          actionButton("submit", "Submit"),
          actionButton("submit2", "Please fill the inputs"),
          actionButton("submit3", "Please wait for your new link")
        ))
        
        
      ),
      
      # Second tab content
      tabItem(
        tabName = "widgets",
        #data table
        withSpinner(
          DT::dataTableOutput("responses")
          ,
          type = getOption("spinner.type", default = 6)
        ),
        
        downloadButton("downloadData", "Download")
      )
    )
  )
  
  
)
