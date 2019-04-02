#######################################################################################
# CHARGE LIBRARIES AND FUNCTIONS
#######################################################################################
#***********************************************************
# ATENTION 1: set the working directory in the forked folder
#***********************************************************
# Funtion that installs the packages used by the Shiny App:
source("./charge_libraries.R") ; charge_libraries()
# It may take some time. If there is a problem, access to charge_libraries.R file
# and import manually the packages.

# Charge the functions that RShiny will use:
functions_path <- gsub(" ", "", paste(getwd(),"/scripts"), fixed = TRUE)
files.sources = list.files(path = functions_path, full.names = T)
sapply(files.sources, source)

#######################################################################################
# SHINY APP
#######################################################################################
shinyApp(
  ui = tagList(
    navbarPage(
      theme = shinytheme("flatly"),                           # Theme selection
      "Accounting Program",                                   # Title
      tabPanel("Data",                                        # Three tab panels in the menu
               sidebarPanel(                                  
                 fileInput("file", "Import data (.xlsx)")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Income & Expenses",
                            DT::dataTableOutput("table"), tags$script(HTML('$(document).on("click", "input", function () {
                       var checkboxes = document.getElementsByName("selected");
                       var checkboxesChecked = [];
                       for (var i=0; i<checkboxes.length; i++) {
                       if (checkboxes[i].checked) {
                       checkboxesChecked.push(checkboxes[i].value);
                      }
                      }
                     Shiny.onInputChange("checked_rows",checkboxesChecked);  })'))
                   )
                 )
               )
      ),
      tabPanel("Control",
               tabsetPanel(
                 tabPanel("QuickBooks Income and Expenses (2018)", plotlyOutput("plot1")),
                 tabPanel("Incomes by class", plotlyOutput("plot2")),
                 tabPanel("Expenses by class", plotlyOutput("plot3"))
               )
      ),
      tabPanel("About",uiOutput("md_file"))
    )
  ),
  server = function(input, output) {
    data <- eventReactive(input$file, {
      data_cleaning(input$file$datapath) }) # code of data_cleaning in /scripts/data_cleaning.R
    output$table <- DT::renderDataTable({
      datatable(data(), 
                filter = 'top', options = list(scrollX = T, paging = FALSE, escape = FALSE),
                rownames= FALSE,
                class = 'cell-border compact', editable = T,
                selection="none")})
    output$plot1 <- renderPlotly({
      ggplotly(plot_1(data())) # code of plot_1 in /scripts/plots.R
    })
    output$plot2 <- renderPlotly({
      ggplotly(plot_2(data())) # code of plot_2 in /scripts/plots.R
    })
    output$plot3 <- renderPlotly({
      ggplotly(plot_3(data())) # code of plot_3 in /scripts/plots.R
    })
    output$md_file <- renderUI({
      file <- "./Quick start Guide.Rmd"
      includeMarkdown(file)
    })
  }
)


