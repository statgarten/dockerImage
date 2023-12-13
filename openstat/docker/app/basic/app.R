library(shiny)
library(data.table)
library(readxl)
library(DT)
library(jstable)
library(shinycustomloader)
library(tableone)
library(labelled)
library(markdown)
# library(epiDisplay)
library(ggplot2)
library(GGally)
library(jsmodule)
library(survC1)
options(shiny.sanitize.errors = F, shiny.maxRequestSize = 5 * 1024^2)
nfactor.limit <- 20

ui <- navbarPage("Basic statistics",
  header = tagList(
    includeCSS('www/style.css'),
  ),
  # theme = bslib::bs_theme(bootswatch = 'solar'),
  inverse = TRUE,
  # title = span(
  #   'Basic statistics',
  #   span( # Github & Homepage
  #     a(
  #       icon('house'),
  #       href = 'https://www.zarathu.com/',
  #       target = "_blank",
  #       style = 'color: white;margin-right: 1em;'
  #     ),
  #     a(
  #       icon('github'),
  #       href = 'https://github.com/jinseob2kim/jsmodule',
  #       target = "_blank",
  #       style = 'color: white;'
  #     ),
  #     style = "right: 1em; position: absolute;"
  #   )
  # ),
  # Data
  tabPanel(
    title = "Data",
    icon = icon("table"),
    sidebarLayout(
      sidebarPanel(
        uiOutput("import"),
        tags$div(
          tags$p("Made by ", tags$a(href = "https://www.zarathu.com", "Zarathu Co.,Ltd."), tags$br()),
          tags$p("For larger file(> 5 mb), ", tags$a(href = "https://github.com/jinseob2kim/jsmodule", tags$code("jsmodule")), " provides ", strong("Rstudio Addins"), ".")
        ),
        downloadButton(outputId = "downloadData", label = "Example data",class = 'primary')
      ),
      mainPanel(
        tabsetPanel(
          type = "pills",
          tabPanel(
            title = "Data",
            style = 'margin-top:1em;',
            markdown("> Category data is shown with <span style='background: #337ab7; color: #fff; border-radius: 3px; margin: 0 3px 3px 0; padding: 1px 3px;'>**Blue**</span>."),
            withLoader(
              DTOutput("data"),
              type = "html",
              loader = "loader6"
            )
          ),
          tabPanel(
            title = "Label",
            style = 'margin-top:1em;',
            withLoader(
              DTOutput("data_label", width = "100%"),
              type = "html",
              loader = "loader6"
            )
          )
        ),
        htmlOutput("naomit")
      )
    )
  ),
  # Table 1
  tabPanel(
    title = "Table 1",
    icon = icon("percentage"),
    sidebarLayout(
      sidebarPanel(
        tb1moduleUI("tb1")
      ),
      mainPanel(
        markdown("> Table 1 for Demographic and Other Baseline Characteristics, see
          <a target = '_blank' href = 'https://www.ema.europa.eu/en/documents/scientific-guideline/ich-e-3-structure-content-clinical-study-reports-step-5_en.pdf'>Ich E3 Guideline 11.2</a>"),
        withLoader(
          DTOutput("table1"),
          type = "html",
          loader = "loader6"
        ),
        wellPanel(
          h5("Normal continuous variables  are summarized with Mean (SD) and t-test (2 groups) or ANOVA (> 2 groups)"),
          h5("Non-normal continuous variables are summarized with median [IQR or min,max] and kruskal-wallis test"),
          h5("Categorical variables  are summarized with table")
        )
      )
    )
  ),
  # Regression
  navbarMenu(
    title = "Regression",
    icon = icon("list-alt"),
    tabPanel(
      title = "Linear regression",
      sidebarLayout(
        sidebarPanel(
          regressModuleUI("linear")
        ),
        mainPanel(
          withLoader(DTOutput("lineartable"),
                     type = "html",
                     loader = "loader6"),
          br(),
          uiOutput("warning_linear")
        )
      )
    ),
    tabPanel(
      title = "Logistic regression",
      sidebarLayout(
        sidebarPanel(
          regressModuleUI("logistic")
        ),
        mainPanel(
          withLoader(DTOutput("logistictable"),
                     type = "html",
                     loader = "loader6")
        )
      )
    ),
    tabPanel(
      title = "Cox model",
      sidebarLayout(
        sidebarPanel(
          coxUI("cox")
        ),
        mainPanel(
          withLoader(DTOutput("coxtable"),
                     type = "html",
                     loader = "loader6")
        )
      )
    )
  ),
  navbarMenu(
    title = "Plot",
    icon = icon("chart-column"),
    tabPanel(
      title = "Basic plot",
      sidebarLayout(
        sidebarPanel(
          ggpairsModuleUI1("ggpairs")
        ),
        mainPanel(
          withLoader(
            plotOutput("ggpairs_plot"),
            type = "html",
            loader = "loader6"),
          ggpairsModuleUI2("ggpairs")
        )
      )
    ),
    tabPanel(
      title = "Scatterplot",
      sidebarLayout(
        sidebarPanel(
          scatterUI("scatter")
        ),
        mainPanel(
          withLoader(
            plotOutput("scatter_plot"),
            type = "html",
            loader = "loader6"),
          ggplotdownUI("scatter")
        )
      )
    ),
    tabPanel(
      title = "Boxplot",
      sidebarLayout(
        sidebarPanel(
          boxUI("box")
        ),
        mainPanel(
          withLoader(
            plotOutput("box_plot"),
            type = "html",
            loader = "loader6"),
          ggplotdownUI("box")
        )
      )
    ),
    tabPanel(
      title = "Barplot",
      sidebarLayout(
        sidebarPanel(
          barUI("bar")
        ),
        mainPanel(
          withLoader(
            plotOutput("bar_plot"),
            type = "html",
            loader = "loader6"),
          ggplotdownUI("bar")
        )
      )
    ),
    tabPanel(
      title = "Lineplot",
      sidebarLayout(
        sidebarPanel(
          lineUI("line")
        ),
        mainPanel(
          withLoader(plotOutput("line_plot"), type = "html", loader = "loader6"),
          ggplotdownUI("line")
        )
      )
    ),
    tabPanel(
      title = "Kaplan-meier plot",
      sidebarLayout(
        sidebarPanel(
          kaplanUI("kaplan")
        ),
        mainPanel(
          optionUI("kaplan"),
          withLoader(plotOutput("kaplan_plot"), type = "html", loader = "loader6"),
          ggplotdownUI("kaplan")
        )
      )
    )
  ),
  navbarMenu(
    title = "ROC analysis",
    icon = icon("check"),
    tabPanel(
      title = "ROC",
      sidebarLayout(
        sidebarPanel(
          rocUI("roc")
        ),
        mainPanel(
          withLoader(plotOutput("plot_roc"), type = "html", loader = "loader6"),
          tableOutput("cut_roc"),
          ggplotdownUI("roc"),
          withLoader(DTOutput("table_roc"), type = "html", loader = "loader6")
        )
      )
    ),
    tabPanel(
      title = "Time-dependent ROC",
      sidebarLayout(
        sidebarPanel(
          timerocUI("timeroc")
        ),
        mainPanel(
          withLoader(plotOutput("plot_timeroc"), type = "html", loader = "loader6"),
          ggplotdownUI("timeroc"),
          withLoader(DTOutput("table_timeroc"), type = "html", loader = "loader6")
        )
      )
    )
  ),
)




server <- function(input, output, session) {
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("example_basic", ".csv", sep = "")
    },
    content = function(file) {
      out <- survival::lung
      out$status <- as.integer(out$status == 1)
      data.table::fwrite(out, file)
    }
  )

  output$import <- renderUI({
    csvFileInput(id = "datafile")
  })

  data.info <- callModule(csvFile, "datafile", nfactor.limit = nfactor.limit)
  data <- reactive(data.info()$data)
  data.label <- reactive(data.info()$label)

  output$data <- renderDT({
    PRdata <- data()
    dl <- data.label()

    nv <- dl$variable[which(dl$class %in% c('factor', 'character'))]

    v <- sapply(colnames(PRdata), function(i){
      if(i %in% nv)
        return(paste0("<div style = 'background: #337ab7; color: #fff; border-radius: 3px; margin: 0 3px 3px 0; padding: 1px 3px;'>", i, "</div>"))
      return(i)
    }, simplify = TRUE, USE.NAMES = FALSE)

    colnames(PRdata) <- unlist(v)

    datatable(
      data = PRdata, # column name change
      #data = data(),
      rownames = F,
      editable = F,
      extensions = c("Buttons", 'ColReorder', 'KeyTable'),
      # filter = 'top', # critical issue with scrollX
      escape = FALSE,

      # caption = "Data",
      options =
        # opt.data("data"),
        list(
          # dom = 'tlfBrip', # Length, Table, Filter, Button, Information, Pagination
          dom = 'lftBrp', # Length, Table, Filter, Button, Information, Pagination
          lengthMenu = list(c(10, 25, -1), c("10", "25", "All")),
          pageLength = 10,
          scrollX = TRUE,
          buttons = c('copy', 'print', 'csv', 'excel', 'pdf', I('colvis')),
          colReorder = TRUE,
          keys = TRUE
        )
    )
  })


  output$data_label <- renderDT({
    datatable(
      data = data.label(),
      rownames = F,
      editable = F,
      extensions = c("Buttons", 'KeyTable'),
      # filter = 'top', # Not works
      # caption = "Label of data",
      options = c(
        opt.data("label"),
        list(
          scrollX = TRUE,
          keys = TRUE
        )
      )
    )
  })

  output$naomit <- renderText({
    data.info()$naomit
  })




  out_tb1 <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

  output$table1 <- renderDT({
    tb <- out_tb1()$table
    cap <- out_tb1()$caption
    out.tb1 <- datatable(tb,
      rownames = T, extensions = "Buttons", caption = cap,
      options = c(
        opt.tb1("tb1"),
        list(columnDefs = list(list(visible = FALSE, targets = which(colnames(tb) %in% c("test", "sig"))))),
        list(scrollX = TRUE)
      )
    )
    if ("sig" %in% colnames(tb)) {
      out.tb1 <- out.tb1 %>% formatStyle("sig", target = "row", backgroundColor = styleEqual("**", "fed9cc"))
    }
    return(out.tb1)
  })

  out_linear <- callModule(regressModule2, "linear", data = data, data_label = data.label, data_varStruct = NULL, default.unires = T, nfactor.limit = nfactor.limit)

  output$lineartable <- renderDT({
    hide <- which(colnames(out_linear()$table) == "sig")
    datatable(out_linear()$table,
      rownames = T, extensions = "Buttons", caption = out_linear()$caption,
      options = c(
        opt.tbreg(out_linear()$caption),
        list(columnDefs = list(list(visible = FALSE, targets = hide))),
        list(scrollX = TRUE)
      )
    ) %>% formatStyle("sig", target = "row", backgroundColor = styleEqual("**", "fed9cc"))
  })

  output$warning_linear <- renderText({
    paste("<b>", out_linear()$warning, "</b>")
  })

  out_logistic <- callModule(logisticModule2, "logistic", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

  output$logistictable <- renderDT({
    hide <- which(colnames(out_logistic()$table) == "sig")
    datatable(out_logistic()$table,
      rownames = T, extensions = "Buttons", caption = out_logistic()$caption,
      options = c(
        opt.tbreg(out_logistic()$caption),
        list(columnDefs = list(list(visible = FALSE, targets = hide))),
        list(scrollX = TRUE)
      )
    ) %>% formatStyle("sig", target = "row", backgroundColor = styleEqual("**", "fed9cc"))
  })


  out_cox <- callModule(coxModule, "cox", data = data, data_label = data.label, data_varStruct = NULL, default.unires = T, nfactor.limit = nfactor.limit)

  output$coxtable <- renderDT({
    hide <- which(colnames(out_cox()$table) == c("sig"))
    datatable(out_cox()$table,
      rownames = T, extensions = "Buttons", caption = out_cox()$caption,
      options = c(
        opt.tbreg(out_cox()$caption),
        list(columnDefs = list(list(visible = FALSE, targets = hide)))
      )
    ) %>% formatStyle("sig", target = "row", backgroundColor = styleEqual("**", "fed9cc"))
  })

  out_ggpairs <- callModule(ggpairsModule2, "ggpairs", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

  output$ggpairs_plot <- renderPlot({
    print(out_ggpairs())
  })

  out_scatter <- scatterServer("scatter", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

  output$scatter_plot <- renderPlot({
    print(out_scatter())
  })

  out_box <- boxServer("box", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

  output$box_plot <- renderPlot({
    print(out_box())
  })

  out_bar <- barServer("bar", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

  output$bar_plot <- renderPlot({
    print(out_bar())
  })

  out_line <- lineServer("line", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

  output$line_plot <- renderPlot({
    print(out_line())
  })

  out_kaplan <- callModule(kaplanModule, "kaplan", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

  output$kaplan_plot <- renderPlot({
    print(out_kaplan())
  })

  out_roc <- callModule(rocModule, "roc", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

  output$plot_roc <- renderPlot({
    print(out_roc()$plot)
  })

  output$cut_roc <- renderTable({
    print(out_roc()$cut)
  })

  output$table_roc <- renderDT({
    datatable(out_roc()$tb,
      rownames = F, editable = F, extensions = "Buttons",
      caption = "ROC results",
      options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE))
    )
  })



  out_timeroc <- callModule(timerocModule, "timeroc", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

  output$plot_timeroc <- renderPlot({
    print(out_timeroc()$plot)
  })

  output$table_timeroc <- renderDT({
    datatable(out_timeroc()$tb,
      rownames = F, editable = F, extensions = "Buttons", caption = "ROC results",
      options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE))
    )
  })

  session$onSessionEnded(function() {
    session$close()
  })
}

shinyApp(ui, server)
