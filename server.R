server <- function(input, output, session) {
  
  
  #observe source changes
  #subTable
  searchResult <- reactive({
    subset(choices, grepl(input$source, choices$sourcechoice))
  })
  
  output$mediumUI <- renderUI({ 
    selectInput("medium", "Medium", searchResult()[,2])
  })
  
  
  # input fields are treated as a group
  formData <- reactive({
    sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
  })
  
  observe({
    if(
      is.null(input$url) | input$url == "" |
      is.null(input$source) | input$source == "" |
      is.null(input$campaign) | input$campaign == "" |
      is.null(input$content) | input$content == ""
    ){
      disable("submit")
    }
    else{
      enable("submit")
    }
  })
  

  
  # Click "Submit" button -> save data
  observeEvent(input$submit, {
    
      CreateData(formData())
      
      Sys.sleep(0.05)

      gs_add_row(test
                 , input = 
                   tail(ReadData(),1)
                   
      )
      


	
	showModal(
	  modalDialog(
        title = "Here is your tracking link",
        
        div(id='newurl'
            , a(tail(ReadData(),1)[,9]
                , href = tail(ReadData(),1)[,9]
            , target="_blank")),
        br(),
        div(id='newurl'
            , a(tail(ReadData(),1)[,8]
                , href = tail(ReadData(),1)[,8]
                , target="_blank")),

        easyClose = TRUE,
        footer = NULL
      ),session)
	
  }, priority = 1)
  
  # Press "New" button -> display empty record
  observeEvent(input$new, {
    UpdateInputs(CreateDefaultRecord(), session)
  })
  

  


  
  # display table
  output$responses <- DT::renderDataTable({
    #update after submit is clicked
    input$submit
    ReadData() %>% mutate(shorturl = createLink(shorturl),longurl = createLink(longurl))
  }, server = FALSE, selection = "single", extensions = c('Scroller','ColReorder','Buttons'),
  colnames = unname(GetTableMetadata()$fields)
  , options = list(dom = 'tp'
                   ,scrollT = TRUE
                   ,colReorder = TRUE), rownames= FALSE, filter = 'top', escape = FALSE
  )     
  
  output$downloadData <- downloadHandler(
    
    filename = "Tracking Links.csv",
    content = function(file) {
      write.csv(ReadData(), file, row.names = F)
    }
    
  )
   
}
