library(shiny)
library(readxl)

# 1. UI Section (Paste content from your old ui.R here)
ui <- fluidPage(navbarPage("Simple Linear Regression",
                           tabPanel("Data",
                                    # Sidebar with a slider input for number of bins
                                    sidebarLayout(
                                      sidebarPanel(
                                        checkboxInput("header", "Column names in first row", TRUE),
                                        fileInput("file1", "Choose an Excel (.XLSX), Comma-Separated Values (.CSV), or Tab-Separated (.TXT) File",
                                                  multiple = FALSE,
                                                  accept = c(".csv",
                                                             ".txt",
                                                             ".xlsx")),
                                        # Horizontal line ----
                                        tags$hr(),
                                      ),
                                      
                                      # Show a plot of the generated distribution
                                      mainPanel(
                                        h4("Directions"),  
                                        p("First, if column names are not in the first row of your data, uncheck the box to the left. Otherwise, leave the box checked. Browse for an Excel (.XLSX), comma-separated values (.CSV), tab-separated (.TXT) file to import and use for Simple Linear Regression. If importing an Excel file, the data must be in the first sheet. Once a file is uploaded, the dimensionality, a preview of the data (first 6 observations), and summary statistic information is shown."),  
                                        p(tags$b("Note:"), "The X and Y variables must be numerical."),
                                        hr(),
                                        fluidRow(column(6,
                                                        h4("Data Overview"),
                                                        tableOutput("overview")),
                                                 column(6,
                                                        h4("Data Preview"),
                                                        tableOutput("contents"))),
                                        hr(),
                                        fluidRow(column(6,
                                                        h4("Summary Statistics"),
                                                        verbatimTextOutput("sum"))),
                                      )
                                    )
                           ),
                           tabPanel("Regression Analysis",
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("xvar", "Choose the Independent (X) Variable", NULL),
                                        selectInput("yvar", "Choose the Dependent (Y) Variable", NULL)
                                      ),
                                      # Sidebar with a slider input for number of bins
                                      # Show a plot of the generated distribution
                                      mainPanel(
                                        h4("Directions"),
                                        p("Based on the dataset imported on the Data tab, the numerical variables will display as options for the X (Independent) and Y (Dependent) variable in the drop-down lists. Choose the Independent (X) and Dependent (Y) variable for the Simple Linear Regression analysis from the drop-downs. The summary output from Simple Linear Regression analysis and a Scatter Plot with the line of best fit will be displayed."),  
                                        hr(),
                                        fluidRow(column(6,
                                                        h4("Model Variables"),
                                                        tableOutput("lmover")),
                                                 column(6,
                                                        h4("Regression Statistics"),
                                                        tableOutput("regstats"))),
                                        hr(),
                                        fluidRow(column(7,
                                                        h4("Equation & Coefficients"),
                                                        br(),
                                                        textOutput("reg_eq"),
                                                        br(),
                                                        br(),
                                                        tableOutput("coef"),
                                                        br(),
                                                        br(),
                                                        h4("ANOVA"),
                                                        tableOutput("anov")),
                                                 column(5,
                                                        h4("Scatter Plot"),
                                                        plotOutput("XYPlotlm")))
                                        
                                        
                                        
                                      )))))

  
  # 2. Server Section (Paste content from your old server.R here)
  server <- function(input, output){
    
    data <- reactive({
      req(input$file1)
      
      ext <- tools::file_ext(input$file1$name)
      switch(ext,
             csv = read.csv(input$file1$datapath, header = input$header),
             txt = read.delim(input$file1$datapath, header = input$header),
             xlsx = as.data.frame(read_xlsx(input$file1$datapath, sheet = 1, col_names = input$header, .name_repair = "universal")),
             validate("Invalid file; Please upload a .xlsx, .csv, or .txt file")
      )
    })
    
    
    ## OLD
    #data <- reactive({
    #  req(input$file1)
    #  tryCatch(
    #    {
    #      df <- read.csv(input$file1$datapath,
    #                     header = input$header,
    #                     stringsAsFactors = TRUE)
    #    },
    #    error = function(e) {
    #      # return a safeError if a parsing error occurs
    #      stop(safeError(e))
    #    }
    #  )
    #})
    
    
    output$contents <- renderTable({
      head(data())}, spacing = "xs", align = "l") 
    
    
    output$overview <- renderTable({
      cbind(Measures = c("Observations", "Variables"),
            Values = c(nrow(data()), ncol(data())))}, spacing = "xs", align = "l"
    )
    
    output$sum <- renderPrint({
      summary(data())
    })
    
    observeEvent(input$file1, {
      updateSelectInput(inputId = "yvar", choices = colnames(Filter(is.numeric,data())), selected=colnames(Filter(is.numeric,data()))[2])
    })
    
    observeEvent(input$file1, {
      updateSelectInput(inputId = "xvar", choices = colnames(Filter(is.numeric,data())), selected=colnames(Filter(is.numeric,data()))[1])
    })
    
    output$XYPlot <- renderPlot({
      yvariable <- which(colnames(data()) %in% input$yvar)
      xvarnum <- which(colnames(data()) %in% input$xvar)
      xvarname <- names(data())[xvarnum]
      plot(x = data()[,xvarnum], y = data()[,yvariable], main = "", xlab = paste0("X: " ,xvarname), ylab = paste0("Y: ", input$yvar), xlim = c(0, max(data()[,xvarnum])), ylim = c(0, max(data()[,yvariable])))
      
    })
    
    output$covcor <- renderTable({
      yvariable <- which(colnames(data()) %in% input$yvar)
      xvarnum <- which(colnames(data()) %in% input$xvar)
      cov_val <- cov(data()[,xvarnum],data()[,yvariable])
      cor_val <- cor(data()[,xvarnum],data()[,yvariable])
      cbind(Measures = c("Covariance", "Correlation"),
            Values = c(round(cov_val, 4), round(cor_val,4)))
    })
    
    regmod <- reactive({
      yvariable <- which(colnames(data()) %in% input$yvar)
      xvarnum <- which(colnames(data()) %in% input$xvar)
      xvarname <- names(data())[xvarnum]
      yvarname <- names(data())[yvariable]
      X <- data()[,xvarnum]
      Y <- data()[,yvariable]
      st <- summary(lm(Y~X, data()))
      st
    })
    
    output$XYPlotlm <- renderPlot({
      yvariable <- which(colnames(data()) %in% input$yvar)
      xvarnum <- which(colnames(data()) %in% input$xvar)
      xvarname <- names(data())[xvarnum]
      plot(x = data()[,xvarnum], y = data()[,yvariable], main = "", xlab = paste0("X: " ,xvarname), ylab = paste0("Y: ", input$yvar), xlim = c(0, max(data()[,xvarnum])), ylim = c(0, max(data()[,yvariable])))
      abline(lm(data()[,yvariable]~data()[,xvarnum]), col = "red", lty = 2)
    })
    
    output$reg_eq <- renderText({paste0("The regression equation is: Y = ",  round(regmod()$coefficients[1],4), " + ",round(regmod()$coefficients[2],4)," * X")})
    
    output$lmover <- renderTable({
      yvariable <- which(colnames(data()) %in% input$yvar)
      xvarnum <- which(colnames(data()) %in% input$xvar)
      xvarname <- names(data())[xvarnum]
      yvarname <- names(data())[yvariable]
      cbind(Variable = c("X", "Y"),
            Name = c(xvarname, yvarname))})
    
    output$regstats <- renderTable({
      yvariable <- which(colnames(data()) %in% input$yvar)
      xvarnum <- which(colnames(data()) %in% input$xvar)
      rval <- cor(data()[,yvariable], data()[,xvarnum])
      rsq <- rval^2
      obs <- nrow(data())
      se <- regmod()$sigma
      cbind(Measures = c("Correlation, r", "R Square", "Standard Error", "Observations, n"),
            Values = c(round(rval,4), round(rsq,4), round(se,4), obs))
    }, digits = 4)
    
    output$coef <- renderTable({
      regmod()$coefficients
    }, digits = 4, rownames = TRUE)
    
    output$anov <- renderTable({
      yvariable <- which(colnames(data()) %in% input$yvar)
      xvarnum <- which(colnames(data()) %in% input$xvar)
      xvarname <- names(data())[xvarnum]
      yvarname <- names(data())[yvariable]
      X <- data()[,xvarnum]
      Y <- data()[,yvariable]
      lmregmod <- lm(Y ~ X, data())
      anovout <- as.data.frame(anova(lmregmod), rownames = T)
      anovout[3,] <- c(sum(anovout[,1]), sum(anovout[,2]), NA, NA, NA)
      rownames(anovout) <- c("Regresion", "Residual", "Total")
      anovout
    }, digits = 4, rownames = TRUE)
    
    
  }


    
    # 3. App Launch (This binds the UI and Server together)
    shinyApp(ui = ui, server = server)