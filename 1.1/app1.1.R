# These libraries are required to run the application, if prompted, install them.
library(shiny)
library(shinydashboard)
library(pdfsearch)
library(DT)

# From here on our, we establish the server and ui functions for the shinyApp.
# This implementation is exactly based off the shinyApp implementation provided
# in the PDFSearch library.
# This is the ui of the PDFSearch package, one of the two components required for the shinyApp.
ui <- dashboardPage(
  # Color of the top bar.
  skin = "purple",
  # This section establishes the left side section of search options.
  dashboardHeader(title = "DocSearchApp"),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Search Options', tabName = 'search', 
               icon = icon('info-circle')),
      menuItem('Results', tabName = 'results',
               icon = icon('table')),
      menuItem('Test Tab', tabName = 'test',
               icon = icon('cog'))
    )
  ),
  dashboardBody(
    tabItems(
      # From here on out controls the UI of the main search page. The tabItems stand for the different pages on the left side of the screen.
      # We add the "implementation" of the tabs here, but in order to actually make it visible and make the tabs accessable, we must add it
      # To the menuItem's above.
      tabItem(tabName = 'search',
              h2('Enter pdf search parameters'),
              h3('Input keywords to search pdf library for.'),
              fluidRow(
                box(title = 'Keywords',
                    status = 'primary', width = 12,
                    numericInput('num_key', 'Number of Keywords', 
                                 value = 1, min = 1, max = NA),
                    uiOutput('key_vals'),
                ),
                box(title = 'Search Settings',
                    status = 'primary', width = 12,
                    # fileInput('path', 'Choose pdf file(s)',
                    #           multiple = TRUE,
                    #           accept = '.pdf'),
                    # p('You may select more than one pdf file'),
                    # hr(),
                    radioButtons('surround', 'Extract surrounding lines?',
                                 choices = c('No' = 1, 'Yes' = 2),
                                 selected = 1),
                    conditionalPanel(
                      condition = 'input.surround == 2',
                      numericInput('num_surround', 'Number of Surrounding lines to extract',
                                   val = 1, min = 1, max = NA)
                    ),
                    radioButtons('ignore_case', 'Ignore Case-sensitivity in Search?',
                                 choices = c('No' = FALSE, 'Yes' = TRUE, 'Vector' = 3),
                                 selected = 1),
                    conditionalPanel(
                      condition = 'input.ignore_case == 3',
                      uiOutput('vec_ignore')
                    )
                )
              ),
              fluidRow(
                box(title = 'Run Search', status = 'warning',
                    width = 12,
                    actionButton('run_search', 'Run Search')
                )
              )
      ),
      tabItem(tabName = 'results',
              h2('View search results'),
              fluidRow(
                box(title = 'Table of Search Results',
                    downloadButton('down_results', label = "", icon = shiny::icon("download"), style = "float:right"),
                    status = 'primary',
                    width = 12,
                    hr(),
                    dataTableOutput('search_results')
                )
              )
      ),
      tabItem(tabName = 'test',
              h2('This is just a test tab I made so that we can easily add other tabs here')
      )
    )
  )
)

# Server function implementation.
server <- function(input, output, session) {
  # Here is the retrieval and processing of the keywords inputted.
  output$key_vals <- renderUI({
    num_keys <- input$num_key
    lapply(1:num_keys, function(i)
      div(style = 'display:inline-block',
          textInput(paste0('key', i), label = paste0('Keyword', i), 
                    value = '', width = '75px'))
    )
  })
  # Here is the retrieval and processing of case ignore.
  output$vec_ignore <- renderUI({
    num_keys <- input$num_key
    lapply(1:num_keys, function(i)
      div(style = 'display:inline-block',
          radioButtons(paste0('ig_case', i), label = paste0('Ignore Case', i), 
                       choices = c('No' = FALSE, 'Yes' = TRUE),
                       selected = 1))
    )
  })
  # This only happens once the user presses run search
  keyword_result <- eventReactive(input$run_search, {
    # Creates the number of keys and applies settings selected in ui, including case sensitivity and surrounding.
    num_keys <- input$num_key
    keywords <- do.call('c', lapply(1:num_keys, function(xx) 
      eval(parse(text = paste0('input$key', xx)))))
    if(input$surround == 1) {
      srd <- FALSE
    } else {
      srd <- input$num_surround
    }
    if(input$ignore_case == 3) {
      ign_cs <- do.call('c', lapply(1:input$num_key, function(xx) 
        eval(parse(text = paste0('ig_case', xx)))))
    } else {
      ign_cs <- input$ignore_case
    }
    # Gets the file path of the app
    script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
    # Gets the folder location of the 'library' folder, the data base that will be searched
    library_folder <- file.path(script_dir, "library")
    # Searches the library folder for all pdfs and concatinates them into a list for searching
    pdf_files <- list.files(path = library_folder, pattern = '\\.pdf$', full.names = TRUE)
    # Initializes the results list
    results <- list()
    # For every file in the pdf list, it runs keyword search on all the keywords
    for (file in pdf_files) {
      search_result <- keyword_search(file,
                                      keyword = keywords,
                                      path = TRUE,
                                      surround_lines = srd,
                                      ignore_case = ign_cs,
                                      token_results = FALSE)
    # Error checking to not combine empty frames in case a file does not have a term (this was causing the errors before)
      if (!is.null(search_result) && nrow(search_result) > 0) {
        # Adding the name of the file to the dataframe as well.
        search_result <- cbind(Name = basename(file), search_result)
        results <- c(results, list(search_result))
      }
    }
    # Combining all the data frames to be passed to the output results
    do.call(rbind, results)
  })
  # Outputting the results to the search
  output$search_results <- renderDataTable({
    keyword_result()
  })


  keyword_flatten <- reactive({
    text_lines <- data.frame(do.call('rbind', keyword_result()[['line_text']]))
    if(input$surround == 1) {
      srd <- 0
    } else {
      srd <- input$num_surround
    }
    names(text_lines) <- paste0('line_text_', 1:ncol(text_lines))
    key_flat <- keyword_result()[c('keyword', 'page_num', 'line_num')]
    cbind(key_flat, text_lines)
  })

  output$down_results <- downloadHandler(
    filename = function() { 
      paste0('results', '.csv')
    },
    content = function(file) {
      write.csv(keyword_flatten(), file)
    }
  )
  
}

shinyApp(ui, server)