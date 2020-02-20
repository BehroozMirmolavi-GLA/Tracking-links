server <- function(input, output, session) {
  hide("submit3")
  
  #observe source changes
  #subTable
  searchResult <- reactive({
    subset(choices, grepl(input$source, choices$sourcechoice))
  })
  
  output$mediumUI <- renderUI({
    selectizeInput("medium",
                   "Medium",
                   c("", as.character(searchResult()[, 2])),
                   options = list(create = TRUE))
  })
  
  
  # input fields are treated as a group
  formData <- reactive({
    sapply(names(GetTableMetadata()$fields), function(x)
      input[[x]])
  })
  
  observe({
    if (is.null(input$url) | input$url == "" |
        is.null(input$source) | input$source == "" |
        is.null(input$campaign) | input$campaign == "" |
        is.null(input$content) | input$content == "") {
      hide("submit")
      show("submit2")
    }
    else{
      show("submit")
      hide("submit2")
    }
  })
  
  
  
  # Click "Submit" button -> save data
  observeEvent(input$submit, {
    hide("submit")
    show("submit3")
    
    CreateData(formData())
    
    Sys.sleep(0.05)
    
    gs_add_row(test
               , input =
                 tail(ReadData(), 1))
    
    
    
    
    showModal(
      modalDialog(
        title = "Here is your tracking link",
        
        HTML(
          '<input type="text" id="newurlshort" value="',
          tail(ReadData(), 1)[, 8],
          '">'
        ),
        HTML('<button onclick="onclickcopy()">Copy link</button>'),
        br(),
        HTML(
          '<input type="text" id="newurllong" value="',
          tail(ReadData(), 1)[, 9],
          '">'
        ),
        HTML('<button onclick="onclickcopy2()">Copy link</button>'),
        
        easyClose = TRUE,
        footer = NULL
      ),
      session
    )
    
    show("submit")
    hide("submit3")
    
  }, priority = 1)
  
  # Press "New" button -> display empty record
  observeEvent(input$new, {
    UpdateInputs(CreateDefaultRecord(), session)
  })
  
  
  
  
  
  
  # display table
  output$responses <- DT::renderDataTable({
    #update after submit is clicked
    input$submit
    ReadData() %>% mutate(url = createLink(url),
                          shorturl = createLink(shorturl),
                          longurl = createLink(longurl))
  }, server = FALSE, selection = "single", extensions = c('Scroller', 'ColReorder', 'Buttons'),
  colnames = unname(GetTableMetadata()$fields)
  , options = list(
    dom = 'tp'
    ,
    columnDefs = list(list(
      targets = c(7, 8), searchable = FALSE
    ))
    ,
    autoWidth = TRUE
    ,
    scrollT = TRUE
    ,
    colReorder = TRUE
  )
  , rownames = FALSE, filter = 'top', escape = FALSE)
  
  output$downloadData <- downloadHandler(
    filename = "Tracking Links.csv",
    content = function(file) {
      write.csv(ReadData()[input[["responses_rows_all"]],], file, row.names = F)
    }
    
  )
  
#use to show r in browser
  observeEvent(input$browser, {
    
  }) #$('#browser').show();


  observeEvent(input$timeout, {
    stopApp()
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}
