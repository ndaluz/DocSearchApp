# These libraries are required to run the application, if prompted, install them.
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(pdfsearch)
library(pdftools)
library(DT)

# This javascript segment makes pressing "enter" run search, similar to google or other search engines.
js <- '
$(document).keyup(function(event) {
  if (event.keyCode == 13) {
      $("#run_search").click();
  }
});
'

# From here on our, we establish the server and ui functions for the shinyApp.
# This implementation is exactly based off the shinyApp implementation provided
# in the PDFSearch library.
# This is the ui of the PDFSearch package, one of the two components required for the shinyApp.

ui <- dashboardPage(
  skin = "black",
  # This section establishes the header/top bar as well as the information/help button on the top right of the screen.
  dashboardHeader(title = "DocSearch",
                  # The only thing that can be placed here is either notifications, tasks, or messages. I use notifications with no icon and divs inside the notifications which are styled far below as a tutorial/help.
                  dropdownMenu(type = "notifications", icon = icon("circle-info"), headerText = "", badgeStatus = NULL,
                               notificationItem(
                                 text = tags$div(tags$b("Basic Search Functions:"), "Please enter keywords seperated by commas, phrases can be entered too, various acronyms are also compatable. ", tags$br(), " Once phrases are entered press 'Run Search' or click enter on the keyboard to run the search."),
                                 icon = icon("")
                               ),
                               notificationItem(
                                 text = tags$div(tags$b("Search Options:"), " To access various search options, click on the three dash icon at the top left of the screen. The search options contain the following:", tags$br(), "Searching for documents that contain all provided keywords, searching for documents from specific years, or in a specific year range."),
                                 icon = icon("")
                               ),
                               notificationItem(
                                 text = tags$div(tags$b("Case Sensitive Words:")," To enter a case sensitive word, please surround the word with parenthesis like this: '(word)'. The word will be matched exactly."),
                                 icon = icon("")
                               ),
                               notificationItem(
                                 text = tags$div(tags$b("Proximity Operators:")," To use a proximity operator, enter in the comma seperated list the two words you would like to search between in the format ", tags$br(), "'word1:x:word2' where x the number of words between the two words. Note that the search result will include the phrase between the two words. ", tags$br(), "The maximum distance between the words is 50."),
                                 icon = icon("")
                               ),
                               notificationItem(
                                 text = tags$div(tags$b("Downloading Results:")," To download results, click the download button at the bottom right of the search results after any search."),
                                 icon = icon("")
                               )
                  )
                  ),

  # Here is the menu items for the side bar, the first field is their actual name, the tabName is used to add functionality server side, and icon is the icon.
  dashboardSidebar(
    # The sidebar starts collapsed, the sidebar includes the main page, "search options", as well as the actual search options/settings.
    collapsed = TRUE,
    sidebarMenu(
      menuItem('Search Options', tabName = 'search', 
               icon = icon('magnifying-glass')),
     
      # Takes in a choice boolean to be saved as "andor" to only check documents that have all keywords
      radioButtons('andor', 'Search for only documents that contain ALL keywords?',
                   choices = c('No' = 1, 'Yes' = 2),
                   selected = 1),
      # "Search specific states?" button, enables the state input indicator.
      radioButtons("search_states", "Search specific states?", choices = c("No", "Yes"), selected = "No"),
      # Selects states input based on the selection
      conditionalPanel(
        condition = "input.search_states == 'Yes'",
        selectInput("states", "Select States", choices = state.name, multiple = TRUE)
      ),
      # "Search specific years?" button, enables the year range indicator
      radioButtons("search_years", "Search specific years?", choices = c("No", "Yes"), selected = "No"),
      
      # Inputs the year range input based on the selection
      conditionalPanel(
        condition = "input.search_years == 'Yes'",
        numericInput("start_year", "Start Year:", value = 2000),
        numericInput("end_year", "End Year:", value = format(Sys.Date(), "%Y"))
      )
    )
  ),
  dashboardBody(
    # Enables the use of shiny js
    useShinyjs(),
    # Applies the js specified at the top of the file, which makes it so when you press enter, the runsearch button is pressed
    tags$script(HTML(js)),
    # Styling the help button on the top right of the screen
    tags$head(tags$style(HTML('
  .navbar-custom-menu>.navbar-nav>li>.dropdown-menu {
  width:900px;
  padding-top: 0px;
  font-family: Copperplate;
  }
  '))),
    # The background color of the application
    setBackgroundColor(
      # color = "#1D1D1D",
      color = "#222D32",
      shinydashboard = TRUE
    ),
    # Styling the space between the keyword entry and the run search button to be 0
    tags$style(HTML("
      .box {margin-bottom: 0px !important;}")),
    tabItems(
      # Here is where the run search button, keyword input, and results are shown, alongside the download button.
      tabItem(tabName = 'search',
              fluidRow(
                box(solidHeader = TRUE, width = 12, 
                    # Takes in an input and saves it as "input_keys", this is processed server side.
                    textInput("input_keys","Enter comma seperated keywords")
                )
              ),
              fluidRow(
                div(class = "col-sm-12",
                    actionButton('run_search', 'Run Search', width = "100%")
                    
                )
              ),
              # When this button is pressed, the search is run. The results box is hidden until the first time that the run search is pressed
              shinyjs::hidden(
                fluidRow(
                  id = "hiddenresultsbox",
                  box(
                    # Download button on the right side of screen.
                    solidHeader = FALSE,
                    width = 12,
                    hr(),
                    # Outputting the reuslts (processed server side)
                    dataTableOutput('search_results'),
                    downloadButton('down_results', label = "", icon = shiny::icon("download"), style = "float:right")
                  )
                )
              )

      )
    )
  ),

)

# Server function implementation.
server <- function(input, output, session) {

  # -------------------------------------------------------------
  # -------------------------------------------------------------
  # Pre-Search Processing and Database Functions
  # -------------------------------------------------------------
  # -------------------------------------------------------------
  # Gets the directory that the application is currently in.
  script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  # Gets the folder location of the 'library' folder, the data base that will be searched
  library_folder <- file.path(script_dir, "library")
  # This creates the list of pdf files from the folder
  pdf_files <- list.files(library_folder, pattern = "^[^-]+-[^-]+-[^-]+\\.pdf$", full.names = TRUE)

  # -------------------------------------------------------------
  # -------------------------------------------------------------
  
  # show the results box after the user presses run search
  observeEvent(input$run_search, {
    shinyjs::show(id = "hiddenresultsbox")
  })
  
  # This only happens once the user presses run search
  
  keyword_result <- eventReactive(input$run_search, {

    # Processing searching of only specific states if option is selected
    if (input$search_states == "Yes") {
      input$states
      for (file_name in pdf_files) {
        # Extract the state and year from the file name
        file_parts <- unlist(strsplit(basename(file_name), "-"))
        state <- file_parts[1]
        if (!(state %in% input$states)) {
          pdf_files <- pdf_files[pdf_files != file_name]
        }
      }
    }
    # Processing searching of only specific years if option is selected
    if (input$search_years == "Yes") {
      start_year <- input$start_year
      end_year <- input$end_year
      for (file_name in pdf_files) {
        # Extract the state and year from the file name
        file_parts <- unlist(strsplit(basename(file_name), "-"))
        year <- as.integer(file_parts[2])
        if (year < start_year || year > end_year) {
          pdf_files <- pdf_files[pdf_files != file_name]
        }
      }
    }
    # -------------------------------------------------------------
    # -------------------------------------------------------------
    # Keyword Tokenization and Processing
    # -------------------------------------------------------------
    # -------------------------------------------------------------
    # This takes the input of keywords, splits them up by commas, turns them into a list, and removes any leading or trailing spaces around the words but not inbetween
    keywords <- trimws(unlist(strsplit(input$input_keys, ",")))
    # Initialize proximity list
    proximity <- list()
    
    # Iterate over keywords
    for (word in keywords) {
      # Check if word is in the format "word1:x:word2" where x is a number
      if (grepl("^[^:]+:[0-5]?[0-9]:[^:]+$", word)) {
        # Split the word by colon and extract the relevant parts
        parts <- unlist(strsplit(word, ":"))
        inner_list <- list(parts[1], parts[3], as.integer(parts[2]))
        # Add the inner list to the proximity list
        proximity <- c(proximity, list(inner_list))
        keywords <- setdiff(keywords, word)
      }
    }
    
    #initialize casesensitive list
    casesensitive <- list()
    # Iterate over keywords
    for (word in keywords) {
      # Check if word is in the format "(word)"
      if (grepl("^\\([^)]+\\)$", word)) {
        # Extract the word between parentheses
        # Remove the parentheses using gsub
        caseword <- gsub("^\\(|\\)$", "", word)  
        
        # Add the inner word to the proximity list
        casesensitive <- c(casesensitive, caseword)
        
        # Remove the matched word from the keywords list
        keywords <- setdiff(keywords, word)
      }
    }
    # This next section adds acronyms of words into the search.
    acronymList <- list(
      # The inner lists should include the groups of acronyms grouped together, for example, If you want PFAS and PFOS to be interchangable in search
      # Then add list("PFAS", "PFOS"). 
      list("Drinking Water Program", "DWP"),
      list("SSPC", "Steel Structures Painting Council"),
      list("USDA", "United States Department of Agriculture"),
      list("ASME", "American Society of Mechanical Engineers"),
      list("NSF", "National Sanitation Foundation"),
      list("MSDH", "Mississippi State Department of Health"),
      list("PWS", "Public Water Systems"),
      list("Best Available Technology", "BAT"),
      list("District Engineer", "DE"),
      list("Water Treatment Committee", "WTC"),
      list("Environmental Technology Verification", "ETV"),
      list("Maximum Contaminant Level", "MCL"),
      list("ANSI", "American National Standards Institute"),
      list("ASTM", "American Society of Testing and Materials"),
      list("AWWA", "American Water Works Association"),
      list("BDR", "Basis of Design Report"),
      list("CCR", "Colorado Code of Regulations"),
      list("CDPHE", "Colorado Department of Public Health and Environment"),
      list("CEB", "Chemically enhanced backwash"),
      list("CFR", "Code of Federal Regulations"),
      list("CIP", "Clean in place"),
      list("CRS", "Colorado Revised Statutes"),
      list("CT", "Contact time"),
      list("CWS", "Community water system"),
      list("DAF", "Dissolved air flotation"),
      list("DORA", "Colorado Department of Regulatory Agencies"),
      list("EPA", "United States Environmental Protection Agency"),
      list("FEMA", "Federal Emergency Management Agency"),
      list("GAC", "Granular activated carbon"),
      list("gpd", "Gallons per day"),
      list("gpm", "Gallons per minute"),
      list("gpm/ft2", "Gallons per minute per square foot"),
      list("GW", "Groundwater"),
      list("GWUDI", "Groundwater under the direct influence"),
      list("LSI", "Langelier Saturation Index"),
      list("MFGM", "Membrane filtration guidance manual"),
      list("MGD", "Million gallons per day"),
      list("MCL", "Maximum Contaminant Level"),
      list("NEC", "National Electrical Code"),
      list("NEMA", "National Electrical Manufacturers' Association"),
      list("NFPA", "National Fire Protection Association"),
      list("NTNC", "Non-Transient Non-Community Water System"),
      list("NPWSCPM", "New Public Water System Capacity Planning Manual"),
      list("O&M", "Operations and maintenance"),
      list("OSHA", "Occupational Safety and Health Administration"),
      list("PFD", "Process flow diagram"),
      list("P&ID", "Process and instrumentation"),
      list("POU", "Point of use device"),
      list("POE", "Point of entry device", "Point of"),
      list("RFI", "Request for information"),
      list("SCADA", "Supervisory Control and Data Acquisition"),
      list("SSCT", "Small System Compliance Technologies"),
      list("SW", "Surface water"),
      list("TMP", "Transmembrane pressure"),
      list("UVT", "Ultraviolet transmittance"),
      list("UVDGM", "Ultraviolet disinfection guidance manual"),
      list("TNC", "Transient Non-Community Water System"),
      list("WQCC", "Water Quality Control Commission"),
      list("WTP", "Water treatment plant")
    )
    # Create an empty vector to store the acronym additions
    acronyms <- character()
    
    # Iterate for all the acronym lists, and if any keywords are in the acronym group, if that acronym group isnt added already, it adds it to acronyms.
    for (group in acronymList) {
      # Checking if keyword is in the acronym group
      if (any(keywords %in% group)) {
        # Checking to not add identical acronym groups
        if (!identical(group, acronyms)) {
          # Add the acronym group to acronyms
          acronyms <- c(acronyms, group)
        }
      }
    }
    # Add the acronyms to the original keywords.
    keywords<- c(keywords, acronyms)
    # Creates the number of keys and applies settings selected in ui, including case sensitivity and surrounding.
    # -------------------------------------------------------------
    # -------------------------------------------------------------
    # Narrowing Library and applying search options.
    # -------------------------------------------------------------
    # -------------------------------------------------------------
    # Searches the library folder for all PDFs and concatenates them into a list for searching

    # Sets the ignore case to the one selected in the input.
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
          if (!grepl(word, pdf_text, ignore.case = TRUE)) {
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
    # -------------------------------------------------------------
    # -------------------------------------------------------------
    # Searching and Compiling Results
    # -------------------------------------------------------------
    # -------------------------------------------------------------
    results <- list()
    # For every file in the pdf list, it runs keyword search on all the keywords
    for (file in pdf_files) {
      # If theres any words labeled for proximity searching. Here's how the algorithm works:
      # If a token is in format 'word1:x:word2', then its saved as a list of 3, with the first word and second word being the two entrees, and the third entree being the integer 'x'
      # Its added to the list "proximity", if the length of proximity is not 0, then for every pdf file, the pdf is broken down and tokenized
      # Then each instance where one of the two words is found within "x" words of one another is saved under "sentence" and added to a "sentences" list 
      # Finally, the sentences list is added to the keywords to be searched, effectively adding the entire phrase between the two words that occured within 'x' words.
      # The program then resumes as normal, with the added phrases in the keywords.
      if (length(proximity) != 0) {
        sentences <- list()
        pdf_text <- pdf_text(file)
        pdf_text <- paste(pdf_text, collapse = " ")
        words <- unlist(strsplit(pdf_text, "\\s+"))
        for (i in 1:length(words)) {
          word <- words[i]
          for (prox in proximity) {
            word1 <- prox[[1]]
            word2 <- prox[[2]]
            distance <- prox[[3]]
            if (word == word1){
              check <- word2
            } else if (word == word2) {
              check <- word1
            } else {
              next
            }
            for (j in max(i - distance, 1):(i-1)) {
              left_word <- words[j]
              if (left_word == check) {
                sentence <- paste(words[j:i], collapse = " ")
                if (!(sentence %in% sentences)) {
                  sentences <- c(sentences, sentence) 
                }
              }
            }
            for (j in (i + 1):min(length(words), i + distance)) {
              right_word <- words[j]
              if (right_word == check) {
                sentence <- paste(words[i:j], collapse = " ")
                if (!(sentence %in% sentences)) {
                  sentences <- c(sentences, sentence) 
                }
              }
            }
          }
        }
        for (i in seq_along(sentences)) {
          sentence <- sentences[[i]]
          sentence <- gsub("\\(|\\)", "", sentence)  # Remove parentheses
          sentences[[i]] <- sentence
        }
        keywords <- c(keywords, sentences)
      }
      
      # Creation of dataframe by calling the keyword_search function
      search_result <- keyword_search(file,
                                      keyword = keywords,
                                      path = TRUE,
                                      surround_lines = 1,
                                      ignore_case = TRUE,
                                      token_results = FALSE, )
    # Error checking to not combine empty frames in case a file does not have a term (this was causing the errors before)
      if (!is.null(search_result) && nrow(search_result) > 0) {
        # Adding the name of the file to the dataframe as well.
        search_result <- cbind(Name = basename(file), search_result)
        results <- c(results, list(search_result))
      }
      # Search for case sensitive words too if they are in the query.
      if(length(casesensitive) >0) 
        {
        case_result <- keyword_search(file,
                                        keyword = casesensitive,
                                        path = TRUE,
                                        surround_lines = 1,
                                        ignore_case = FALSE,
                                        token_results = FALSE, )
        # Error checking to not combine empty frames in case a file does not have a term (this was causing the errors before)
        if (!is.null(case_result) && nrow(case_result) > 0) {
          # Adding the name of the file to the dataframe as well.
          case_result <- cbind(Name = basename(file), case_result)
          results <- c(results, list(case_result))
        }
      }

    }
    # Combining all the data frames to be passed to the output results
    do.call(rbind, results)
  })
  # Outputting the results to the search
  # output$search_results <- renderDataTable({
  #   keyword_result()
  # })
  # This doesnt work yet, because we need to add the pdf files to a server and have it open it on that server instead of locally.
    output$search_results <- DT::renderDataTable({
    datatable(keyword_result(), selection = 'none', callback = JS("
      table.on('click.dt', 'tr', function() {
        var data = table.row(this).data();
        var file = data['pdf_file'];
        window.open(file);
      });
    "))
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