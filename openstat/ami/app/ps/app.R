library(shiny);library(DT);library(data.table);library(readxl);library(jstable);library(haven);library(shinycustomloader);library(MatchIt);library(jsmodule);library(survey)
library(jskm);library(ggplot2);library(markdown);library(survC1)
options(shiny.sanitize.errors = F)
nfactor.limit <- 20


  ui <- navbarPage("Propensity score analysis",
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
                                            tabPanel("Matching data", withLoader(DTOutput("matdata"), type="html", loader="loader6")),
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
                                tabsetPanel(type = "pills",
                                            tabPanel("Original", 
                                                     withLoader(DTOutput("table1_original"), type="html", loader="loader6"),
                                                     wellPanel(
                                                       h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                                       h5("Non-normal continuous variables are summarized with median [IQR or min,max] and kruskal-wallis test"),
                                                       h5("Categorical variables  are summarized with table")
                                                     )
                                            ),
                                            tabPanel("Matching", 
                                                     withLoader(DTOutput("table1_ps"), type="html", loader="loader6"),
                                                     wellPanel(
                                                       h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                                       h5("Non-normal continuous variables are summarized with median [IQR or min,max] and kruskal-wallis test"),
                                                       h5("Categorical variables  are summarized with table")
                                                     )
                                            ),
                                            tabPanel("IPTW", 
                                                     withLoader(DTOutput("table1_iptw"), type="html", loader="loader6"),
                                                     wellPanel(
                                                       h5("Normal continuous variables  are summarized with Mean (SD) and complex survey regression"),
                                                       h5("Non-normal continuous variables are summarized with median [IQR or min,max] and complex sampling rank test"),
                                                       h5("Categorical variables  are summarized with table")
                                                     )
                                            )
                                )
                              )
                            )
                            ),
                   navbarMenu("Regression", icon = icon("list-alt"),
                              tabPanel("Linear regression",
                                       sidebarLayout(
                                         sidebarPanel(
                                           regressModuleUI("linear")
                                         ),
                                         mainPanel(
                                           tabsetPanel(type = "pills",
                                                       tabPanel("Original", 
                                                                withLoader(DTOutput("linear_original"), type="html", loader="loader6"),
                                                                br(),
                                                                uiOutput("warning_linear_original")
                                                       ),
                                                       tabPanel("Matching", 
                                                                withLoader(DTOutput("linear_ps"), type="html", loader="loader6"),
                                                                br(),
                                                                uiOutput("warning_linear_ps")
                                                       ),
                                                       tabPanel("IPTW", 
                                                                withLoader(DTOutput("linear_iptw"), type="html", loader="loader6")
                                                       )
                                           )
                                           
                                         )
                                       )
                              ),
                              tabPanel("Logistic regression",
                                       sidebarLayout(
                                         sidebarPanel(
                                           regressModuleUI("logistic")
                                         ),
                                         mainPanel(
                                           tabsetPanel(type = "pills",
                                                       tabPanel("Original", 
                                                                withLoader(DTOutput("logistic_original"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("Matching", 
                                                                withLoader(DTOutput("logistic_ps"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("IPTW", 
                                                                withLoader(DTOutput("logistic_iptw"), type="html", loader="loader6")
                                                       )
                                           )
                                         )
                                       )
                              ),
                              tabPanel("Cox model",
                                       sidebarLayout(
                                         sidebarPanel(
                                           coxUI("cox")
                                         ),
                                         mainPanel(
                                           tabsetPanel(type = "pills",
                                                       tabPanel("Original", 
                                                                withLoader(DTOutput("cox_original"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("Matching", 
                                                                withLoader(DTOutput("cox_ps"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("IPTW", 
                                                                withLoader(DTOutput("cox_iptw"), type="html", loader="loader6")
                                                       )
                                           )
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
                                           tabsetPanel(type = "pills",
                                                       tabPanel("Original", 
                                                                withLoader(plotOutput("ggpairs_plot_original"), type="html", loader="loader6")
                                                                
                                                       ),
                                                       tabPanel("Matching", 
                                                                withLoader(plotOutput("ggpairs_plot_ps"), type="html", loader="loader6")
                                                       )
                                                       
                                           ),
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
                                           tabsetPanel(type = "pills",
                                                       tabPanel("Original", 
                                                                withLoader(plotOutput("kaplan_plot_original"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("Matching", 
                                                                withLoader(plotOutput("kaplan_plot_ps"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("IPTW", 
                                                                withLoader(plotOutput("kaplan_plot_iptw"), type="html", loader="loader6")
                                                       )
                                           ),
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
                                           tabsetPanel(type = "pills",
                                                       tabPanel("Original", 
                                                                withLoader(plotOutput("plot_roc_original"), type="html", loader="loader6"),
                                                                withLoader(DTOutput("table_roc_original"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("Matching", 
                                                                withLoader(plotOutput("plot_roc_ps"), type="html", loader="loader6"),
                                                                withLoader(DTOutput("table_roc_ps"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("IPTW", 
                                                                withLoader(plotOutput("plot_roc_iptw"), type="html", loader="loader6"),
                                                                withLoader(DTOutput("table_roc_iptw"), type="html", loader="loader6")
                                                       )
                                           ),
                                           ggplotdownUI("roc")
                                         )
                                       )
                              ),
                              tabPanel("Time-dependent ROC",
                                       sidebarLayout(
                                         sidebarPanel(
                                           timerocUI("timeroc")
                                         ),
                                         mainPanel(
                                           tabsetPanel(type = "pills",
                                                       tabPanel("Original", 
                                                                withLoader(plotOutput("plot_timeroc_original"), type="html", loader="loader6"),
                                                                withLoader(DTOutput("table_timeroc_original"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("Matching", 
                                                                withLoader(plotOutput("plot_timeroc_ps"), type="html", loader="loader6"),
                                                                withLoader(DTOutput("table_timeroc_ps"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("IPTW", 
                                                                withLoader(plotOutput("plot_timeroc_iptw"), type="html", loader="loader6"),
                                                                withLoader(DTOutput("table_timeroc_iptw"), type="html", loader="loader6")
                                                       )
                                           ),
                                           ggplotdownUI("timeroc")
                                         )
                                       )
                              )
                   ),
                   
                   
  )

    


  server <- function(input, output, session) {
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("example_ps", ".csv", sep = "")
      },
      content = function(file) {
        out <- survival::pbc
        out$status <- as.integer(out$status == 2)
        data.table::fwrite(na.omit(out)[, -1], file)
      }
    )

    
    output$import <- renderUI({
      FilePsInput("datafile")
    })
    
    mat.info <- callModule(FilePs, "datafile", nfactor.limit = nfactor.limit)

    output$data <- renderDT({
      datatable(mat.info()$data, rownames=F, editable = F, extensions= "Buttons", caption = "Data with ps, iptw",
                options = c(opt.data("data"), list(scrollX = TRUE))
      )
    })

    output$matdata <- renderDT({
      datatable(mat.info()$matdata, rownames=F, editable = F, extensions= "Buttons", caption = "Matching data",
                options = c(opt.data("matching_data"), list(scrollX = TRUE))
      )
    })

    output$data_label <- renderDT({
      datatable(mat.info()$data.label, rownames=F, editable = F, extensions= "Buttons", caption = "Label of data",
                options = c(opt.data("data_label"), list(scrollX = TRUE))
                )
      })
    
    output$naomit <- renderText({
      paste("<font size = 5 ><b>", "The variables below contain missing values.</b></font><br>", '<font size = 4 color=\"#FF0000\"><b>', mat.info()$naomit, "</b></font>")
      #mat.info()$naomit
      })
    
    
    ## tb1
    data <- reactive({
      mat.info()$data[, .SD, .SDcols = -c("iptw")]
      })
    matdata <- reactive(data.table::data.table(mat.info()$matdata))
    data.label <- reactive(mat.info()$data.label)
    #data_varStruct <- reactive(list(variable = names(mat.info()$matdata)))
    design.survey <- reactive(survey::svydesign(ids = ~ 1, data = mat.info()$data[!is.na(iptw), ], weights = ~ iptw))
    
    
    tb1_original <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = NULL, design.survey = NULL, nfactor.limit = nfactor.limit)
    tb1_ps <- callModule(tb1module2, "tb1", data = matdata, data_label = data.label, data_varStruct = NULL, design.survey = NULL, nfactor.limit = nfactor.limit)
    tb1_iptw <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, nfactor.limit = nfactor.limit)
    
    
    output$table1_original <- renderDT({
      tb <- tb1_original()$table
      cap <- tb1_original()$caption
      out.tb1 <- datatable(tb, rownames = T, extensions= "Buttons", caption = cap,
                           options = c(opt.tb1("tb1"),
                                       list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
                                       ),
                                       list(scrollX = TRUE)
                           )
      )
      if ("sig" %in% colnames(tb)){
        out.tb1 <- out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
      }
      return(out.tb1)
    })
    
    output$table1_ps <- renderDT({
      tb <- tb1_ps()$table
      cap <- tb1_ps()$caption
      out.tb1 <- datatable(tb, rownames = T, extensions= "Buttons", caption = cap,
                           options = c(opt.tb1("tb1"),
                                       list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
                                       ),
                                       list(scrollX = TRUE)
                           )
      )
      if ("sig" %in% colnames(tb)){
        out.tb1 <- out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
      }
      return(out.tb1)
    })
    
    output$table1_iptw <- renderDT({
      tb <- tb1_iptw()$table
      cap <- tb1_iptw()$caption
      out.tb1 <- datatable(tb, rownames = T, extensions= "Buttons", caption = cap,
                           options = c(opt.tb1("tb1"),
                                       list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
                                       ),
                                       list(scrollX = TRUE)
                           )
      )
      if ("sig" %in% colnames(tb)){
        out.tb1 <- out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
      }
      return(out.tb1)
    })
    
    
    ## Regression
    
    out_linear_original <- callModule(regressModule2, "linear", data = data, data_label = data.label, data_varStruct = NULL, default.unires = F, nfactor.limit = nfactor.limit)
    out_linear_ps <- callModule(regressModule2, "linear", data = matdata, data_label = data.label, data_varStruct = NULL, default.unires = F, nfactor.limit = nfactor.limit)
    out_linear_iptw <- callModule(regressModule2, "linear", data = data, data_label = data.label, data_varStruct = NULL, default.unires = F, design.survey = design.survey, nfactor.limit = nfactor.limit)
    

    output$linear_original <- renderDT({
      hide <- which(colnames(out_linear_original()$table) == "sig")
      datatable(out_linear_original()$table, rownames=T, extensions= "Buttons", caption = out_linear_original()$caption,
                options = c(opt.tbreg(out_linear_original()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            ),
                            list(scrollX = TRUE)
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })
    
    output$warning_linear_original <- renderText({
      paste("<b>", out_linear_original()$warning, "</b>")
    })
    
    output$linear_ps <- renderDT({
      hide <- which(colnames(out_linear_ps()$table) == "sig")
      datatable(out_linear_ps()$table, rownames=T, extensions= "Buttons", caption = out_linear_ps()$caption,
                options = c(opt.tbreg(out_linear_ps()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            ),
                            list(scrollX = TRUE)
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })
    
    output$warning_linear_ps <- renderText({
      paste("<b>", out_linear_ps()$warning, "</b>")
    })
    
    output$linear_iptw <- renderDT({
      hide <- which(colnames(out_linear_iptw()$table) == "sig")
      datatable(out_linear_iptw()$table, rownames=T, extensions= "Buttons", caption = out_linear_iptw()$caption,
                options = c(opt.tbreg(out_linear_iptw()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            ),
                            list(scrollX = TRUE)
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })
    
    
    ## Logistic
    
    out_logistic_original <- callModule(logisticModule2, "logistic", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_logistic_ps <- callModule(logisticModule2, "logistic", data = matdata, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_logistic_iptw <- callModule(logisticModule2, "logistic", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, nfactor.limit = nfactor.limit)
    
    
    output$logistic_original <- renderDT({
      hide = which(colnames(out_logistic_original()$table) == "sig")
      datatable(out_logistic_original()$table, rownames=T, extensions= "Buttons", caption = out_logistic_original()$caption,
                options = c(opt.tbreg(out_logistic_original()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            ),
                            list(scrollX = TRUE)
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })
    
    output$logistic_ps <- renderDT({
      hide = which(colnames(out_logistic_ps()$table) == "sig")
      datatable(out_logistic_ps()$table, rownames=T, extensions= "Buttons", caption = out_logistic_ps()$caption,
                options = c(opt.tbreg(out_logistic_ps()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            ),
                            list(scrollX = TRUE)
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })
    
    output$logistic_iptw <- renderDT({
      hide = which(colnames(out_logistic_iptw()$table) == "sig")
      datatable(out_logistic_iptw()$table, rownames=T, extensions= "Buttons", caption = out_logistic_iptw()$caption,
                options = c(opt.tbreg(out_logistic_iptw()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            ),
                            list(scrollX = TRUE)
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })
    
    
    ## Cox
    
    out_cox_original <- callModule(coxModule, "cox", data = data, data_label = data.label, data_varStruct = NULL, default.unires = F, nfactor.limit = nfactor.limit)
    out_cox_ps <- callModule(coxModule, "cox", data = matdata, data_label = data.label, data_varStruct = NULL, default.unires = F, nfactor.limit = nfactor.limit)
    out_cox_iptw <- callModule(coxModule, "cox", data = data, data_label = data.label, data_varStruct = NULL, default.unires = F, design.survey = design.survey, nfactor.limit = nfactor.limit)
    
    output$cox_original <- renderDT({
      hide = which(colnames(out_cox_original()$table) == c("sig"))
      datatable(out_cox_original()$table, rownames=T, extensions= "Buttons", caption = out_cox_original()$caption,
                options = c(opt.tbreg(out_cox_original()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets= hide))
                            )
                )
      )  %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })
    
    output$cox_ps <- renderDT({
      hide = which(colnames(out_cox_ps()$table) == c("sig"))
      datatable(out_cox_ps()$table, rownames=T, extensions= "Buttons", caption = out_cox_ps()$caption,
                options = c(opt.tbreg(out_cox_ps()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets= hide))
                            )
                )
      )  %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })
    
    output$cox_iptw <- renderDT({
      hide = which(colnames(out_cox_iptw()$table) == c("sig"))
      datatable(out_cox_iptw()$table, rownames=T, extensions= "Buttons", caption = out_cox_iptw()$caption,
                options = c(opt.tbreg(out_cox_iptw()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets= hide))
                            )
                )
      )  %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })
    
    ## ggpairs
    
    out_ggpairs_original <- callModule(ggpairsModule2, "ggpairs", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_ggpairs_ps <- callModule(ggpairsModule2, "ggpairs", data = matdata, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    
    output$ggpairs_plot_original <- renderPlot({
      print(out_ggpairs_original())
    })
    
    output$ggpairs_plot_ps <- renderPlot({
      print(out_ggpairs_ps())
    })
    
    
    ## Kaplan
    
    out_kaplan_original <- callModule(kaplanModule, "kaplan", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_kaplan_ps <- callModule(kaplanModule, "kaplan", data = matdata, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_kaplan_iptw <- callModule(kaplanModule, "kaplan", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, nfactor.limit = nfactor.limit)
    
    output$kaplan_plot_original <- renderPlot({
      print(out_kaplan_original())
    })
    
    output$kaplan_plot_ps <- renderPlot({
      print(out_kaplan_ps())
    })
    
    output$kaplan_plot_iptw <- renderPlot({
      print(out_kaplan_iptw())
    })
    
    
    ## ROC
    
    out_roc_original <- callModule(rocModule2, "roc", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_roc_ps <- callModule(rocModule2, "roc", data = matdata, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_roc_iptw <- callModule(rocModule2, "roc", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, nfactor.limit = nfactor.limit)
    
    
    output$plot_roc_original <- renderPlot({
      print(out_roc_original()$plot)
    })
    
    output$table_roc_original <- renderDT({
      datatable(out_roc_original()$tb, rownames=F, editable = F, extensions= "Buttons",
                caption = "ROC results",
                options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
    })
    
    output$plot_roc_ps <- renderPlot({
      print(out_roc_ps()$plot)
    })
    
    output$table_roc_ps <- renderDT({
      datatable(out_roc_ps()$tb, rownames=F, editable = F, extensions= "Buttons",
                caption = "ROC results",
                options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
    })
    
    output$plot_roc_iptw <- renderPlot({
      print(out_roc_iptw()$plot)
    })
    
    output$table_roc_iptw <- renderDT({
      datatable(out_roc_iptw()$tb, rownames=F, editable = F, extensions= "Buttons",
                caption = "ROC results",
                options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
    })
    
    ## Time-ROC
    
    out_timeroc_original <- callModule(timerocModule2, "timeroc", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_timeroc_ps <- callModule(timerocModule2, "timeroc", data = matdata, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_timeroc_iptw <- callModule(timerocModule2, "timeroc", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, nfactor.limit = nfactor.limit)
    
    
    output$plot_timeroc_original <- renderPlot({
      print(out_timeroc_original()$plot)
    })
    
    output$table_timeroc_original <- renderDT({
      datatable(out_timeroc_original()$tb, rownames=F, editable = F, extensions= "Buttons", caption = "ROC results",
                options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
    })
    
    output$plot_timeroc_ps <- renderPlot({
      print(out_timeroc_ps()$plot)
    })
    
    output$table_timeroc_ps <- renderDT({
      datatable(out_timeroc_ps()$tb, rownames=F, editable = F, extensions= "Buttons", caption = "ROC results",
                options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
    })
    
    output$plot_timeroc_iptw <- renderPlot({
      print(out_timeroc_iptw()$plot)
    })
    
    output$table_timeroc_iptw <- renderDT({
      datatable(out_timeroc_iptw()$tb, rownames=F, editable = F, extensions= "Buttons", caption = "ROC results",
                options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
    })
    
    session$onSessionEnded(function() {
      session$close()
    })
  
  
    }

  shinyApp(ui, server)

