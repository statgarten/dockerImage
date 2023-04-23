library(shiny);library(rhandsontable);library(magrittr);library(meta);library(shinycustomloader);library(colourpicker);library(shinythemes)

data.default <- data.frame(
    Study = c("KDH", "Kangwon", "DCMC", "SMC", "AJOU"),
    Measure = c(0.56, 1.8, 1.35, 0.75, 1.09),
    LCI = c(0.24, 0.62, 0.73, 0.31, 0.68),
    UCI = c(1.25, 5.86, 2.57, 1.77, 1.77), 
    Subgroup = c("Metropolitan", "Non-metropolitan", "Non-metropolitan", "Metropolitan", "Metropolitan"), stringsAsFactors = F
)

data.bin <- data.frame(
    Study = c("KDH", "Kangwon", "DCMC", "SMC", "AJOU"),
    event.experimental = c(1, 4, 6, 20, 69),
    n.experimental = c(12, 21, 43, 83, 373),
    event.control = c(4, 7, 7, 15, 94),
    n.control = c(11, 21, 41, 84, 357),
    Subgroup = c("Metropolitan", "Non-metropolitan", "Non-metropolitan", "Metropolitan", "Metropolitan"), stringsAsFactors = F
)

data.cont <- data.frame(
    Study = c("KDH", "Kangwon", "DCMC", "SMC", "AJOU"),
    n.experimental = c(12, 21, 43, 83, 373),
    mean.experimental = c(5, 4.9, 22.5, 12.5, 6.5),
    sd.experimental = c(4.7, 1.71, 3.44, 1.47, 0.76),
    n.control = c(11, 21, 41, 84, 357),
    mean.control = c(6.5, 6.1, 24.9, 12.3, 7.38),
    sd.control = c(3.8, 2.3, 10.65, 1.66, 1.41),
    Subgroup = c("Metropolitan", "Non-metropolitan", "Non-metropolitan", "Metropolitan", "Metropolitan"), stringsAsFactors = F
)

data.prop <- data.frame(
    Study = c("KDH", "Kangwon", "DCMC", "SMC", "AJOU"),
    event = c(4, 7, 7, 15, 94),
    n = c(11, 21, 41, 84, 357),
    Subgroup = c("Metropolitan", "Non-metropolitan", "Non-metropolitan", "Metropolitan", "Metropolitan"), stringsAsFactors = F
)

#library(showtext)
#showtext_opts(dpi = 90)                                                ## same to rmd chunk
#font_add("NanumGothic", "/usr/share/fonts/truetype/nanum/NanumGothic.ttf")
#showtext_auto()


mkforest <- function(obj.meta, input_fixed_random,
                     label.right = "Target worse", col.label.right = "red",
                     label.left = "Comparator worse", col.label.left = "green", layout = "meta", xlim = NULL, spacing = 1.5, overall = T){
 
    if (is.null(xlim)){
        forest(obj.meta, 
               label.right = label.right, col.label.right = col.label.right,
               label.left = label.left, col.label.left = col.label.left,
               fixed = ("fixed effect" %in% input_fixed_random), random = ("random effects" %in% input_fixed_random), 
               layout= layout, spacing = spacing, overall = overall, col.by = "black", digits.se = 2)
    } else{
        forest(obj.meta, 
               label.right = label.right, col.label.right = col.label.right,
               label.left = label.left, col.label.left = col.label.left,
               fixed = ("fixed effect" %in% input_fixed_random), random = ("random effects" %in% input_fixed_random), 
               layout= layout, xlim = xlim, spacing = spacing, overall = overall, col.by = "black", digits.se = 2)
    }
}


mkfunnel <- function(obj.meta, input_fixed_random, level = 0.95, studlab, yaxis, contour = F, legend.pos = "topright", backtransf = F){
    if (contour == F){
        funnel(obj.meta, fixed = ("fixed effect" %in% input_fixed_random), random = ("random effects" %in% input_fixed_random),
               level = level, studlab = studlab, yaxis = yaxis, backtransf = backtransf)
    } else{
        funnel(obj.meta, fixed = ("fixed effect" %in% input_fixed_random), random = ("random effects" %in% input_fixed_random),
               level = level, studlab = studlab, yaxis = yaxis, contour = c(0.9, 0.95, 0.99), backtransf = backtransf)
        legend(legend.pos,
               c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"), fill = gray(seq(0.5, 0.9, len = 3)))
    }
    
    
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinythemes::shinytheme("flatly"),

    # Application title
    titlePanel("Meta analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("inputtype", "Input type", c("Measure & 95% CI", "Event & N", "Mean & S.D")),
            radioButtons("measure", "Measure", c("HR", "OR", "RR", "MD", "SMD"), inline = T),
            checkboxGroupInput("fixed_random", "Estimate type", choices = c("fixed effect", "random effects"), selected = c("random effects"), inline = T),
            checkboxInput("trimfill", "Apply Trim & fill method", F),
            tags$div(tags$p("Made by ", tags$a(href = "https://www.zarathu.com", "Zarathu Co.,Ltd."), tags$br()),
                     tags$p("Use ", tags$a(href = "https://cran.r-project.org/web/packages/meta/index.html", tags$code("meta")), " package.")),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h2("Input meta result"),
            h3("Add & remove rows via mouse right click"),
            rHandsontableOutput("hot"),
            br(),
            tabsetPanel(type = "pills",
                        tabPanel("Forest plot", 
                                 radioButtons("style_forest", "Style", c("Default" = "meta", "JAMA" = "JAMA", "RevMan5" = "RevMan5"), inline = T),
                                 withLoader(plotOutput("forest"), type="html", loader="loader6"),
                                 h3("Options"),
                                 wellPanel(
                                     fluidRow(
                                         column(4, checkboxInput("subgroup", "Include subgroup", F)),
                                         column(4, checkboxInput("overall", "Include overall summaries" , T)),
                                         column(4, sliderInput("spacing", "Line space" , min = 1, max = 2, value = 1.5, step = 0.1))
                                     ),
                                     fluidRow(
                                         column(3, textInput("label_comp", "Comparator label", "Comparator worse")),
                                         column(3, colourInput("col_comp", "Color", value = "red", palette = "limited", returnName = T)),
                                         column(3, textInput("label_target", "Target label", "Target worse")),
                                         column(3, colourInput("col_target", "Color", value = "green", palette = "limited", returnName = T)),
                                     ),
                                     fluidRow(
                                         column(3, checkboxInput("x_forest", "Change x-axis range" , F)),
                                         column(3, conditionalPanel("input.x_forest == true", 
                                                                    numericInput("xmax_forest", "Maximum x", min = 1, value = 10)
                                         ))
                                     ),
                                     uiOutput("xrange_forest"),
                                 ),
                                 h3("Download options"),
                                 wellPanel(
                                     uiOutput("downloadControls_forest"),
                                     downloadButton("downloadButton_forest", label = "Download the plot")
                                 )
                        ),
                        tabPanel("Funnel plot",
                                 radioButtons("ytype_funnel", "Weight type", c("S.E" = "se", "Inverse variance" = "invvar", "Inverse S.E" = "invse"), inline = T),
                                 checkboxGroupInput("option_funnel", NULL, c("Show study" = "studlab", "Show contour" = "contour", "Original scale(HR, RR, OR)" = "backtransf"), "studlab", inline = T),
                                 withLoader(plotOutput("funnel", width = "100%"), type="html", loader="loader6"),
                                 h3("Download options"),
                                 wellPanel(
                                     uiOutput("downloadControls_funnel"),
                                     downloadButton("downloadButton_funnel", label = "Download the plot")
                                 )
                                 
                        ),
                        tabPanel("R output", 
                                 withLoader(verbatimTextOutput("metaout"), type="html", loader="loader6"),
                                 withLoader(verbatimTextOutput("egger"), type="html", loader="loader6")
                        ),
                        tabPanel("Drapery plot",
                                 radioButtons("ytype_drapery", "Y-axis type", c("pvalue", "zvalue"), inline = T),
                                 fluidRow(column(4, checkboxInput("legend_drapery", "Legend", T)),
                                          column(8, conditionalPanel(condition = "input.legend_drapery == true",
                                                                     selectInput("legendposition_drapery", "Legend position", c("bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center"), "topleft")))),
                                 
                                 withLoader(plotOutput("drapery", width = "100%"), type="html", loader="loader6"),
                                 h3("Download options"),
                                 wellPanel(
                                   uiOutput("downloadControls_drapery"),
                                   downloadButton("downloadButton_drapery", label = "Download the plot")
                                 )
                                 
                        )
            )
        
            

            
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observe({
        x <- input$inputtype
        
        if (x == "Event & N"){
            updateRadioButtons(session, "measure",
                               label = "Measure",
                               choices = c("RR", "OR", "RD", "ASD", "Single proportion"), inline = T
            )
        } else if (x == "Mean & S.D"){
            updateRadioButtons(session, "measure",
                               label = "Measure",
                               choices = c("MD", "SMD", "ROM"), inline = T
            )
        } else{
            updateRadioButtons(session, "measure",
                               label = "Measure",
                               c("HR", "OR", "RR", "MD", "SMD"), inline = T) 
        }
        
        
    })
    

    output$hot <- renderRHandsontable({
        DF <- switch(input$inputtype,
                     "Measure & 95% CI" = data.default,
                     "Event & N" = data.bin,
                     "Mean & S.D" = data.cont
                     )
        
        if (input$measure == "Single proportion"){
            DF <- data.prop
        }

        rhandsontable(DF,  stretchH = "all") %>% 
            hot_context_menu(
                customOpts = list(
                    csv = list(name = "Download to CSV",
                               callback = htmlwidgets::JS(
                                   "function (key, options) {
                         var csv = csvString(this, sep=',', dec='.');

                         var link = document.createElement('a');
                         link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                           encodeURIComponent(csv));
                         link.setAttribute('download', 'data.csv');

                         document.body.appendChild(link);
                         link.click();
                         document.body.removeChild(link);
                       }"))))
    })
    
    
    obj.meta <- reactive({
        req(input$hot)
        DF <- hot_to_r(input$hot)
        byvar <- NULL
        if (input$subgroup == T) {byvar <- DF$Subgroup}
        
        if (input$inputtype == "Measure & 95% CI"){
            req(input$measure %in% c("HR", "OR", "RR", "MD", "SMD"))
            out.meta <- metagen(TE = log(DF$Measure), lower = log(DF$LCI), upper = log(DF$UCI), studlab = DF$Study, sm = input$measure, subgroup = byvar, subgroup.name  = "")
            if (input$measure %in% c("MD", "SMD")){
                out.meta <- metagen(TE = DF$Measure, lower = DF$LCI, upper = DF$UCI, studlab = DF$Study, sm = input$measure, subgroup = byvar, subgroup.name  = "")  
            }
        } else if (input$inputtype == "Event & N"){
            req(input$measure %in% c("RR", "OR", "RD", "ASD", "Single proportion"))
            if (input$measure == "Single proportion"){
                out.meta <- metaprop(event = DF$event, n = DF$n, studlab = DF$Study, subgroup = byvar, subgroup.name = "", sm = "PRAW")
            } else{
                out.meta <- metabin(event.e = DF$event.experimental, DF$n.experimental, DF$event.control, DF$n.control, DF$Study,  sm = input$measure, subgroup = byvar, subgroup.name  = "")
            }
            
        } else{
            req(input$measure %in% c("MD", "SMD", "ROM"))
            out.meta <- metacont(n.e = DF$n.experimental, DF$mean.experimental, DF$sd.experimental, DF$n.control, DF$mean.control, DF$sd.control, DF$Study,  sm = input$measure, subgroup = byvar, subgroup.name  = "")
        }
        
        
        if (input$trimfill == T){
            out.meta <- trimfill(out.meta)
        }
        return(out.meta)
    })
    

    
    
    
    output$forest <- renderPlot({
        xlim <- NULL
        if (input$x_forest == T){
            if (input$measure %in% c("HR", "OR", "RR")){
                xlim <- c(1/input$xmax_forest, input$xmax_forest)
            } else{
                xlim <- c(-input$xmax_forest, input$xmax_forest)
            }
        }
        
        if (input$measure == "Single proportion"){
            forest(obj.meta(), fixed = ("fixed effect" %in% input$fixed_random), random = ("random effects" %in% input$fixed_random), 
                   layout= input$style_forest, spacing = input$spacing, overall = input$overall, col.by = "black", digits.se = 2)
        } else{
            mkforest(obj.meta(), input$fixed_random, label.right = input$label_target, label.left = input$label_comp,
                     col.label.right = input$col_target, col.label.left = input$col_comp, layout = input$style_forest, xlim = xlim, overall = input$overall, spacing = input$spacing)
            
        }
        
    })
    
    
    output$downloadControls_forest <- renderUI({
        fluidRow(
            column(4,
                   selectizeInput("forest_file_ext", "File extension (dpi = 300)", 
                                  choices = c("jpg","pdf", "tiff", "svg", "pptx"), multiple = F, 
                                  selected = "pptx"
                   )
            ),
            column(4,
                   sliderInput("fig_width_forest", "Width (in):",
                               min = 5, max = 20, value = 8
                   )
            ),
            column(4,
                   sliderInput("fig_height_forest", "Height (in):",
                               min = 5, max = 20, value = 6
                   )
            )
        )
    })
    
    output$downloadButton_forest <- downloadHandler(
        filename =  function() {
            paste("forestplot.", input$forest_file_ext ,sep="")
        },
        # content is a function with argument file. content writes the plot to the device
        content = function(file) {
            withProgress(message = 'Download in progress',
                         detail = 'This may take a while...', value = 0, {
                             for (i in 1:15) {
                                 incProgress(1/15)
                                 Sys.sleep(0.01)
                             }
                             ff <- function(){
                                 xlim <- NULL
                                 if (input$x_forest == T){
                                     if (input$measure %in% c("HR", "OR", "RR")){
                                         xlim <- c(1/input$xmax_forest, input$xmax_forest)
                                     } else{
                                         xlim <- c(-input$xmax_forest, input$xmax_forest)
                                     }
                                 }
                                 mkforest(obj.meta(), input$fixed_random, label.right = input$label_target, label.left = input$label_comp,
                                          col.label.right = input$col_target, col.label.left = input$col_comp, layout = input$style_forest, xlim = xlim, overall = input$overall, spacing = input$spacing)
                             }
                             
                             if (input$forest_file_ext == "pptx"){
                                 my_vec_graph <- rvg::dml(code = ff())
                                 doc <- officer::read_pptx()
                                 doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
                                 doc <- officer::ph_with(doc, my_vec_graph, location = officer::ph_location(width = input$fig_width_forest, height = input$fig_height_forest) )
                                 print(doc, target = file)
                                 
                             } else if (input$forest_file_ext == "jpg"){
                                 jpeg(file, width = input$fig_width_forest, height =input$fig_height_forest, units = "in", res = 600)
                                 ff()
                                 dev.off()
                             } else if (input$forest_file_ext == "tiff"){
                                 tiff(file, width = input$fig_width_forest, height =input$fig_height_forest, units = "in", res = 600, compression = "zip")
                                 ff()
                                 dev.off()
                                 
                             } else if (input$forest_file_ext == "pdf"){
                                 pdf(file, width = input$fig_width_forest, height =input$fig_height_forest)
                                 ff()
                                 dev.off()
                                 
                             } else if (input$forest_file_ext == "svg"){
                                 svglite::svglite(file, width = input$fig_width_forest, height =input$fig_height_forest)
                                 ff()
                                 dev.off()
                                 
                             } 
                             
                         })
            
            
        })
    
    
    output$funnel <- renderPlot({
        mkfunnel(obj.meta(), input$fixed_random, level = 0.95, studlab = "studlab" %in% input$option_funnel, yaxis = input$ytype_funnel, 
                 contour = "contour" %in% input$option_funnel, legend.pos = "topright", backtransf = "backtransf" %in% input$option_funnel)
    })
    
    
    
    output$downloadControls_funnel <- renderUI({
        fluidRow(
            column(4,
                   selectizeInput("funnel_file_ext", "File extension (dpi = 300)", 
                                  choices = c("jpg","pdf", "tiff", "svg", "pptx"), multiple = F, 
                                  selected = "pptx"
                   )
            ),
            column(4,
                   sliderInput("fig_width_funnel", "Width (in):",
                               min = 5, max = 20, value = 8
                   )
            ),
            column(4,
                   sliderInput("fig_height_funnel", "Height (in):",
                               min = 5, max = 20, value = 6
                   )
            )
        )
    })
    
    output$downloadButton_funnel <- downloadHandler(
        filename =  function() {
            paste("funnel.", input$funnel_file_ext ,sep="")
        },
        # content is a function with argument file. content writes the plot to the device
        content = function(file) {
            withProgress(message = 'Download in progress',
                         detail = 'This may take a while...', value = 0, {
                             for (i in 1:15) {
                                 incProgress(1/15)
                                 Sys.sleep(0.01)
                             }
                             ff <- function(){
                                 mkfunnel(obj.meta(), input$fixed_random, level = 0.95, studlab = "studlab" %in% input$option_funnel, yaxis = input$ytype_funnel, contour = "contour" %in% input$option_funnel, 
                                          legend.pos = "topright", backtransf = "backtransf" %in% input$option_funnel)
                             }
                             
                             if (input$funnel_file_ext == "pptx"){
                                 my_vec_graph <- rvg::dml(code = ff())
                                 doc <- officer::read_pptx()
                                 doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
                                 doc <- officer::ph_with(doc, my_vec_graph, location = officer::ph_location(width = input$fig_width_funnel, height = input$fig_height_funnel) )
                                 print(doc, target = file)
                                 
                             } else if (input$funnel_file_ext == "jpg"){
                                 jpeg(file, width = input$fig_width_funnel, height =input$fig_height_funnel, units = "in", res = 600)
                                 ff()
                                 dev.off()
                             } else if (input$funnel_file_ext == "tiff"){
                                 tiff(file, width = input$fig_width_funnel, height =input$fig_height_funnel, units = "in", res = 600, compression = "zip")
                                 ff()
                                 dev.off()
                                 
                             } else if (input$funnel_file_ext == "pdf"){
                                 pdf(file, width = input$fig_width_funnel, height =input$fig_height_funnel)
                                 ff()
                                 dev.off()
                                 
                             } else if (input$funnel_file_ext == "svg"){
                                 svglite::svglite(file, width = input$fig_width_funnel, height =input$fig_height_funnel)
                                 ff()
                                 dev.off()
                                 
                             } 
                             
                         })
            
            
        })
    
    
    output$metaout <- renderPrint({
        obj.meta()
    })
    
    output$egger <- renderPrint({
        metabias(obj.meta(), k.min = obj.meta()$k)
    })
    
    
    output$drapery <- renderPlot({
      drapery(obj.meta(), 
              labels = "studlab",
              type = input$ytype_drapery, 
              legend = input$legend_drapery,
              pos.legen = input$legendposition_drapery)
    })
    
    
    output$downloadControls_drapery <- renderUI({
      fluidRow(
        column(4,
               selectizeInput("drapery_file_ext", "File extension (dpi = 300)", 
                              choices = c("jpg","pdf", "tiff", "svg", "pptx"), multiple = F, 
                              selected = "pptx"
               )
        ),
        column(4,
               sliderInput("fig_width_drapery", "Width (in):",
                           min = 5, max = 20, value = 8
               )
        ),
        column(4,
               sliderInput("fig_height_drapery", "Height (in):",
                           min = 5, max = 20, value = 6
               )
        )
      )
    })
    
    output$downloadButton_drapery <- downloadHandler(
      filename =  function() {
        paste("drapery.", input$drapery_file_ext ,sep="")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        withProgress(message = 'Download in progress',
                     detail = 'This may take a while...', value = 0, {
                       for (i in 1:15) {
                         incProgress(1/15)
                         Sys.sleep(0.01)
                       }
                       ff <- function(){
                         drapery(obj.meta(), 
                                 labels = "studlab",
                                 type = input$ytype_drapery, 
                                 legend = input$legend_drapery,
                                 pos.legen = input$legendposition_drapery)
                       }
                       
                       if (input$drapery_file_ext == "pptx"){
                         my_vec_graph <- rvg::dml(code = ff())
                         doc <- officer::read_pptx()
                         doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
                         doc <- officer::ph_with(doc, my_vec_graph, location = officer::ph_location(width = input$fig_width_drapery, height = input$fig_height_drapery) )
                         print(doc, target = file)
                         
                       } else if (input$drapery_file_ext == "jpg"){
                         jpeg(file, width = input$fig_width_drapery, height =input$fig_height_drapery, units = "in", res = 600)
                         ff()
                         dev.off()
                       } else if (input$drapery_file_ext == "tiff"){
                         tiff(file, width = input$fig_width_drapery, height =input$fig_height_drapery, units = "in", res = 600, compression = "zip")
                         ff()
                         dev.off()
                         
                       } else if (input$drapery_file_ext == "pdf"){
                         pdf(file, width = input$fig_width_drapery, height =input$fig_height_drapery)
                         ff()
                         dev.off()
                         
                       } else if (input$drapery_file_ext == "svg"){
                         svglite::svglite(file, width = input$fig_width_drapery, height =input$fig_height_drapery)
                         ff()
                         dev.off()
                         
                       } 
                       
                     })
        
        
      })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
