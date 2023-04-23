library(shiny);library(data.table);library(readxl);library(DT);library(jstable);library(shinycustomloader);library(tableone);library(labelled)
library(markdown)
library(ggplot2);library(GGally);library(jsmodule);library(survC1)
options(shiny.sanitize.errors = F)
nfactor.limit <- 20

ui <- navbarPage("Repeated measure analysis",
                 tabPanel("Data", icon = icon("table"),
                          sidebarLayout(
                            sidebarPanel(
                              uiOutput("import"),
                              tags$div(tags$p("Made by ", tags$a(href = "https://www.zarathu.com", "Zarathu Co.,Ltd."), tags$br()),
                                       tags$p("For larger file(> 5 mb), ", tags$a(href = "https://github.com/jinseob2kim/jsmodule", tags$code("jsmodule")), " provides ", strong("Rstudio Addins"), ".")),
                              downloadButton("downloadData", "Example data")
                            ),
                            mainPanel(
                              tabsetPanel(type = "pills",
                                          tabPanel("Data", withLoader(DTOutput("data"), type="html", loader="loader6")),
                                          tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type="html", loader="loader6"))
                              ),
                              htmlOutput("naomit")
                              
                            )
                          )
                 ),
                 tabPanel("Table 1", icon = icon("percentage"),
                          sidebarLayout(
                            sidebarPanel(
                              tb1moduleUI("tb1")
                            ),
                            mainPanel(
                              withLoader(DTOutput("table1"), type="html", loader="loader6"),
                              wellPanel(
                                h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                h5("Non-normal continuous variables are summarized with median [IQR or min,max] and kruskal-wallis test"),
                                h5("Categorical variables  are summarized with table")
                              )
                            )
                          )
                          
                 ),
                 navbarMenu("GEE", icon = icon("list-alt"),
                            tabPanel("Linear",
                                     sidebarLayout(
                                       sidebarPanel(
                                         GEEModuleUI("linear")
                                       ),
                                       mainPanel(
                                         withLoader(DTOutput("lineartable"), type="html", loader="loader6")
                                       )
                                     )
                            ),
                            tabPanel("Binomial",
                                     sidebarLayout(
                                       sidebarPanel(
                                         GEEModuleUI("logistic")
                                       ),
                                       mainPanel(
                                         withLoader(DTOutput("logistictable"), type="html", loader="loader6")
                                       )
                                     )
                            ),
                            tabPanel("Marginal cox model",
                                     sidebarLayout(
                                       sidebarPanel(
                                         coxUI("cox")
                                       ),
                                       mainPanel(
                                         withLoader(DTOutput("coxtable"), type="html", loader="loader6")
                                       )
                                     )
                            )
                            
                 ),
                 navbarMenu("Plot", icon = icon("bar-chart-o"), 
                            tabPanel("Scatter plot",
                                     sidebarLayout(
                                       sidebarPanel(
                                         ggpairsModuleUI1("ggpairs")
                                       ),
                                       mainPanel(
                                         withLoader(plotOutput("ggpairs_plot"), type="html", loader="loader6"),
                                         ggpairsModuleUI2("ggpairs")
                                       )
                                     )
                            ),
                            tabPanel("Kaplan-meier plot",
                                     sidebarLayout(
                                       sidebarPanel(
                                         kaplanUI("kaplan")
                                       ),
                                       mainPanel(
                                         optionUI("kaplan"),
                                         withLoader(plotOutput("kaplan_plot"), type="html", loader="loader6"),
                                         ggplotdownUI("kaplan")
                                       )
                                     )
                            )
                            
                 ),
                 navbarMenu("ROC analysis", icon = icon("check"),
                            tabPanel("ROC",
                                     sidebarLayout(
                                       sidebarPanel(
                                         rocUI("roc")
                                       ),
                                       mainPanel(
                                         withLoader(plotOutput("plot_roc"), type="html", loader="loader6"),
                                         ggplotdownUI("roc"),
                                         withLoader(DTOutput("table_roc"), type="html", loader="loader6")
                                       )
                                     )
                            ),
                            tabPanel("Time-dependent ROC",
                                     sidebarLayout(
                                       sidebarPanel(
                                         timerocUI("timeroc")
                                       ),
                                       mainPanel(
                                         withLoader(plotOutput("plot_timeroc"), type="html", loader="loader6"),
                                         ggplotdownUI("timeroc"),
                                         withLoader(DTOutput("table_timeroc"), type="html", loader="loader6")
                                       )
                                     )
                            )
                 ),
)




server <- function(input, output, session) {
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("example_repeated", ".csv", sep = "")
    },
    content = function(file) {
      data.table::fwrite(survival::colon[, -2], file)
    }
  )

  output$import <- renderUI({
    FileRepeatedInput("datafile")
    
  })
  
  data.info <- callModule(FileRepeated, "datafile", nfactor.limit = nfactor.limit)
  data <- reactive(data.info()$data)
  data.label <- reactive(data.info()$label)
  id.gee <- reactive(data.info()$id.gee)
  
  output$data <- renderDT({
    datatable(data(), rownames=F, editable = F, extensions= "Buttons", caption = "Data",
              options = c(opt.data("data"), list(scrollX = TRUE))
    )
  })
  
  
  output$data_label <- renderDT({
    datatable(data.label(), rownames=F, editable = F, extensions= "Buttons", caption = "Label of data",
              options = c(opt.data("label"), list(scrollX = TRUE))
    )
  })
  
  output$naomit <- renderText({
    data.info()$naomit
  })
  
  
  
  
  out_tb1 <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
  
  output$table1 <- renderDT({
    tb = out_tb1()$table
    cap = out_tb1()$caption
    out.tb1 = datatable(tb, rownames = T, extensions= "Buttons", caption = cap,
                        options = c(opt.tb1("tb1"),
                                    list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
                                    ),
                                    list(scrollX = TRUE)
                        )
    )
    if ("sig" %in% colnames(tb)){
      out.tb1 = out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
    }
    return(out.tb1)
  })
  
  out_linear <- callModule(GEEModuleLinear, "linear", data = data, data_label = data.label, data_varStruct = NULL, id.gee = id.gee, nfactor.limit = nfactor.limit)
  
  output$lineartable <- renderDT({
    hide = which(colnames(out_linear()$table) == "sig")
    datatable(out_linear()$table, rownames=T, extensions= "Buttons", caption = out_linear()$caption,
              options = c(opt.tbreg(out_linear()$caption),
                          list(columnDefs = list(list(visible=FALSE, targets =hide))
                          ),
                          list(scrollX = TRUE)
              )
    ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
  })
  
  out_logistic <- callModule(GEEModuleLogistic, "logistic", data = data, data_label = data.label, data_varStruct = NULL, id.gee = id.gee, nfactor.limit = nfactor.limit)
  
  output$logistictable <- renderDT({
    hide = which(colnames(out_logistic()$table) == "sig")
    datatable(out_logistic()$table, rownames=T, extensions= "Buttons", caption = out_logistic()$caption,
              options = c(opt.tbreg(out_logistic()$caption),
                          list(columnDefs = list(list(visible=FALSE, targets =hide))
                          ),
                          list(scrollX = TRUE)
              )
    ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
  })
  
  out_cox <- callModule(coxModule, "cox", data = data, data_label = data.label, data_varStruct = NULL, default.unires = T, id.cluster = id.gee, nfactor.limit = nfactor.limit)
  
  output$coxtable <- renderDT({
    hide = which(colnames(out_cox()$table) == c("sig"))
    datatable(out_cox()$table, rownames=T, extensions= "Buttons", caption = out_cox()$caption,
              options = c(opt.tbreg(out_cox()$caption),
                          list(columnDefs = list(list(visible=FALSE, targets= hide))
                          )
              )
    )  %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
  })
  
  out_ggpairs <- callModule(ggpairsModule2, "ggpairs", data = data, data_label = data.label, data_varStruct = NULL)
  
  output$ggpairs_plot <- renderPlot({
    print(out_ggpairs())
  })
  
  out_kaplan <- callModule(kaplanModule, "kaplan", data = data, data_label = data.label, data_varStruct = NULL, id.cluster = id.gee, nfactor.limit = nfactor.limit)
  
  output$kaplan_plot <- renderPlot({
    print(out_kaplan())
  })
  
  out_roc <- callModule(rocModule, "roc", data = data, data_label = data.label, data_varStruct = NULL, id.cluster = id.gee, nfactor.limit = nfactor.limit)
  
  output$plot_roc <- renderPlot({
    print(out_roc()$plot)
  })
  
  output$table_roc <- renderDT({
    datatable(out_roc()$tb, rownames=F, editable = F, extensions= "Buttons",
              caption = "ROC results",
              options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
  })
  
  out_timeroc <- callModule(timerocModule, "timeroc", data = data, data_label = data.label, data_varStruct = NULL, id.cluster = id.gee, nfactor.limit = nfactor.limit)
  
  output$plot_timeroc <- renderPlot({
    print(out_timeroc()$plot)
  })
  
  output$table_timeroc <- renderDT({
    datatable(out_timeroc()$tb, rownames=F, editable = F, extensions= "Buttons", caption = "ROC results",
              options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
  })
  
  session$onSessionEnded(function() {
    session$close()
  })
  
  
}

shinyApp(ui, server)