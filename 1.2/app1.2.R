# These libraries are required to run the application, if prompted, install them.
library(shiny)
library(shinydashboard)
library(pdfsearch)
library(pdftools)
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
  # Here is the menu items for the side bar, the first field is their actual name, the tabName is used to add functionality server side, and icon is the icon.
  dashboardSidebar(
    sidebarMenu(
      menuItem('Search Options', tabName = 'search', 
               icon = icon('magnifying-glass')),
      menuItem('Results', tabName = 'results',
               icon = icon('table')),
      menuItem('Database', tabName = 'database',
               icon = icon('database'))
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
                    # Takes in an input and saves it as "input_keys", this is processed server side.
                    textInput("input_keys","Enter keywords, seperated by comma's")
                ),
                box(title = 'Search Settings',
                    status = 'primary', width = 12,
                    # Takes in a choice boolean to be saved as "andor" to only check documents that have all keywords
                    radioButtons('andor', 'Search for only documents that contain ALL keywords?',
                                 choices = c('No' = 1, 'Yes' = 2),
                                 selected = 1),
                    # Takes in a boolean choice to be saved as "surround" to extract or not extract surrounding lines
                    radioButtons('surround', 'Extract surrounding lines?',
                                 choices = c('No' = 1, 'Yes' = 2),
                                 selected = 1),
                    # If yes is selected, then it prompts an option to input a number of lines, saved as num_surround
                    conditionalPanel(
                      condition = 'input.surround == 2',
                      numericInput('num_surround', 'Number of Surrounding lines to extract',
                                   val = 1, min = 1, max = NA)
                    ),
                    # Takes in a boolean "ignore_case" that selects whether case sensitivity is considered or not.
                    radioButtons('ignore_case', 'Ignore Case-sensitivity in Search?',
                                 choices = c('No' = FALSE, 'Yes' = TRUE),
                                 selected = TRUE)

                    
                )
              ),
              # When this button is pressed, the search is run. 
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
                    # Download button on the right side of screen.
                    downloadButton('down_results', label = "", icon = shiny::icon("download"), style = "float:right"),
                    status = 'primary',
                    width = 12,
                    hr(),
                    # Outputting the reuslts (processed server side)
                    dataTableOutput('search_results')
                )
              )
      ),
      tabItem(tabName = 'database',
              h2('Search Database'),
              width = 12,
              fileInput("file_input", "Add PDF to database (Wont visually update list until restart):"),
              verbatimTextOutput("pdf_list")
      )
    )
  )
)

# Server function implementation.
server <- function(input, output, session) {
  # Gets the directory that the application is currently in.
  script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  # Gets the folder location of the 'library' folder, the data base that will be searched
  library_folder <- file.path(script_dir, "library")
  # This creates the list of pdf files from the folder
  pdf_files <- reactiveVal(list.files(library_folder, pattern = "\\.pdf$", full.names = TRUE))
  # This outputs the names of the file to the ui output to list in the database tab.
  output$pdf_list <- renderText({
    paste(basename(pdf_files()), collapse = "\n")
  })
  # Below is the process to update the table if any new files are added, although it doesnt work for some reason. Will fix later.
  observe({
    pdf_files(list.files(library_folder, pattern = "\\.pdf$", full.names = TRUE))
  })
  
  observeEvent(input$file_input, {
    file_to_add <- input$file_input$datapath
    file_name <- input$file_input$name
    file.copy(file_to_add, file.path(library_folder, file_name), overwrite = TRUE)
  })
  
  # This only happens once the user presses run search
  keyword_result <- eventReactive(input$run_search, {
    # This takes the input of keywords, splits them up by commas, turns them into a list, and removes any leading or trailing spaces around the words but not inbetween
    keywords <- trimws(unlist(strsplit(input$input_keys, ",")))
    # Creates the number of keys and applies settings selected in ui, including case sensitivity and surrounding.
    if(input$surround == 1) {
      srd <- FALSE
    } else {
      srd <- input$num_surround
    }
    # Sets the ignore case to the one selected in the input.
    ign_cs <- input$ignore_case
    
    # Searches the library folder for all PDFs and concatenates them into a list for searching
    pdf_files <- list.files(path = library_folder, pattern = '\\.pdf$', full.names = TRUE)
    # The if statement occurs if the "search only files with all keywords" is selected. It goes through every file in the database and if any of the keywords are not present, that file is noted.
    # Finally, all of the files noted above are removed from the pdf_files list before the actual search takes place.
    if (input$andor == 2) {
      files_to_remove <- c()
      # the outerloop to tag all files for removal
      for (file in pdf_files) {
        # Contains is set to true initially, and is set to false if any word in the keyword list is NOT in the current document
        contains <- TRUE
        # taking in and processing pdf file contents
        pdf_text <- pdf_text(file)
        pdf_text <- paste(pdf_text, collapse = " ")
        # Checking if any of the words arent in the file
        for (word in keywords) {
          if (!grepl(word, pdf_text, ignore.case = ign_cs)) {
            contains <- FALSE
            break  # No need to continue checking other words if one is missing
          }
        }
        # Addition of file to removal list
        if (!contains) {
          files_to_remove <- c(files_to_remove, file)
        }
      }
      # Removal of files.
      pdf_files <- pdf_files[!pdf_files %in% files_to_remove]
    }
    
    # Initializes the results list
    results <- list()
    # For every file in the pdf list, it runs keyword search on all the keywords
    for (file in pdf_files) {
      # Creation of dataframe by calling the keyword_search function
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

  # This is called for the download in order to format the search results for the csv file.
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
  # The download button for the results, downloading them in a csv file.
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