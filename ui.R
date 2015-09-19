library('shiny')
library('markdown')

shinyUI(
  navbarPage(
    title = 'HDNOM',
    windowTitle = 'HDNOM Web App',
    theme = 'lumen.css',
    inverse = FALSE,
    id = 'mainnavbar',

    tabPanel(title = 'Home',

             fluidRow(
               column(width = 10, offset = 1,
                      div(class = 'jumbotron',
                          h1('HDNOM'),
                          h3('Nomograms for high-dimensional data, built with ease.'),
                          p('Learn how to use this web app within seconds:'),
                          actionButton('learnmore', 'Learn More', icon('search'), class = 'btn-primary btn-lg')
                      ),

                      tags$blockquote("“Hiding within those mounds of data is knowledge that could change the life of a patient, or change the world.”",
                                      tags$small('Atul Butte, Professor, Stanford School of Medicine'))

               )),

             fluidRow(
               column(width = 10, offset = 1,
                      includeMarkdown('footer.md')
               )
             )

    ),

    tabPanel(title = 'Upload',

             fluidRow(

               column(width = 10, offset = 1,
                      sidebarPanel(width = 12,
                                   radioButtons('data_type', label = 'Enter data',
                                                choices = list('Load example dataset' = 'example',
                                                               'Upload your dataset' = 'upload'),
                                                selected = 'example'),
                                   conditionalPanel(
                                     condition = "input.data_type == 'upload'",
                                     radioButtons('sep_type', label = 'Delimiter Type:',
                                                  choices = list('Comma' = 'comma', 'Tab' = 'tab', 'Semicolon' = 'semi'),
                                                  selected = 'comma'),
                                     fileInput('filex', label = 'Upload design matrix X:'),
                                     fileInput('filey', label = 'Upload response matrix y:'),
                                     p(a('Sample data x (Comma-separated)', href = 'example/x.csv', target = '_blank'),
                                       HTML('&nbsp;&nbsp;&nbsp;&nbsp;'),
                                       a('Sample data y (Comma-separated)', href = 'example/y.csv', target = '_blank')),
                                     p(a('Sample data x (Tab-separated)', href = 'example/x.tsv', target = '_blank'),
                                       HTML('&nbsp;&nbsp;&nbsp;&nbsp;'),
                                       a('Sample data y (Tab-separated)', href = 'example/y.tsv', target = '_blank')),
                                     p(a('Sample data x (Semicolon-separated)', href = 'example/x.txt', target = '_blank'),
                                       HTML('&nbsp;&nbsp;&nbsp;&nbsp;'),
                                       a('Sample data y (Semicolon-separated)', href = 'example/y.txt', target = '_blank'))
                                   )
                      )

               ),

               column(width = 10, offset = 1,
                      mainPanel(width = 12,
                                tabsetPanel(
                                  tabPanel("Data Table",
                                           dataTableOutput("print_dataset")),
                                  tabPanel("Data Summary", verbatimTextOutput("summary_dataset"))
                                )
                      )
               )

             )

    ),

    tabPanel(title = 'Nomogram',

             fluidRow(

               column(width = 4, offset = 1,
                      sidebarPanel(width = 12,
                                   selectInput('model_type', label = 'Choose model type',
                                               choices = list('Lasso' = 'lasso',
                                                              'Adaptive Lasso' = 'alasso',
                                                              'Fused Lasso' = 'flasso',
                                                              'Elastic-Net' = 'enet',
                                                              'Adaptive Elastic-Net' = 'aenet',
                                                              'MCP' = 'mcp',
                                                              'Mnet' = 'mnet',
                                                              'SCAD' = 'scad',
                                                              'Snet' = 'snet'),
                                               selected = 'lasso'),
                                   conditionalPanel(
                                     condition = "input.model_type == 'lasso'",
                                     sliderInput("lasso_nfolds", "CV fold number:",
                                                 min = 3, max = 10, value = 5, step = 1, ticks = FALSE),
                                     radioButtons('lasso_lambda_rule', label = 'λ selection rule:',
                                                  choices = list('Minimal' = 'lambda.min',
                                                                 '1 Standard Error' = 'lambda.1se'),
                                                  selected = 'lambda.1se'),
                                     numericInput("lasso_pred_at", label = 'Prediction time point:', min = 0, value = 365),
                                     numericInput("lasso_seed", label = 'Set seed:', min = 1, value = 42)
                                   ),
                                   conditionalPanel(
                                     condition = "input.model_type == 'alasso'",
                                     sliderInput("alasso_nfolds", "CV fold number:",
                                                 min = 3, max = 10, value = 5, step = 1, ticks = FALSE),
                                     radioButtons('alasso_lambda_rule', label = 'λ selection rule:',
                                                  choices = list('Minimal' = 'lambda.min',
                                                                 '1 Standard Error' = 'lambda.1se'),
                                                  selected = 'lambda.1se'),
                                     numericInput("alasso_pred_at", label = 'Prediction time point:', min = 0, value = 365),
                                     numericInput("alasso_seed1", label = 'Set seed 1:', min = 1, value = 42),
                                     numericInput("alasso_seed2", label = 'Set seed 2:', min = 1, value = 22)
                                   ),
                                   conditionalPanel(
                                     condition = "input.model_type == 'flasso'",
                                     sliderInput("flasso_nfolds", "CV fold number:",
                                                 min = 3, max = 10, value = 5, step = 1, ticks = FALSE),
                                     numericInput("flasso_pred_at", label = 'Prediction time point:', min = 0, value = 365),
                                     numericInput("flasso_seed", label = 'Set seed:', min = 1, value = 42)
                                   ),
                                   conditionalPanel(
                                     condition = "input.model_type == 'enet'",
                                     sliderInput("enet_alpha", "α:",
                                                 min = 0, max = 1, value = 0.5, step = 0.01),
                                     sliderInput("enet_nfolds", "CV fold number:",
                                                 min = 3, max = 10, value = 5, step = 1, ticks = FALSE),
                                     radioButtons('enet_lambda_rule', label = 'λ selection rule:',
                                                  choices = list('Minimal' = 'lambda.min',
                                                                 '1 Standard Error' = 'lambda.1se'),
                                                  selected = 'lambda.1se'),
                                     numericInput("enet_pred_at", label = 'Prediction time point:', min = 0, value = 365),
                                     numericInput("enet_seed", label = 'Set seed:', min = 1, value = 42)
                                   ),
                                   conditionalPanel(
                                     condition = "input.model_type == 'aenet'",
                                     sliderInput("aenet_alpha1", "First α:",
                                                 min = 0, max = 1, value = 0.5, step = 0.01),
                                     sliderInput("aenet_alpha2", "Second α:",
                                                 min = 0, max = 1, value = 0.5, step = 0.01),
                                     sliderInput("aenet_nfolds", "CV fold number:",
                                                 min = 3, max = 10, value = 5, step = 1, ticks = FALSE),
                                     radioButtons('aenet_lambda_rule', label = 'λ selection rule:',
                                                  choices = list('Minimal' = 'lambda.min',
                                                                 '1 Standard Error' = 'lambda.1se'),
                                                  selected = 'lambda.1se'),
                                     numericInput("aenet_pred_at", label = 'Prediction time point:', min = 0, value = 365),
                                     numericInput("aenet_seed1", label = 'Set seed 1:', min = 1, value = 42),
                                     numericInput("aenet_seed2", label = 'Set seed 2:', min = 1, value = 22)
                                   ),
                                   conditionalPanel(
                                     condition = "input.model_type == 'mcp'",
                                     numericInput("mcp_gamma", "γ:",
                                                  min = 1.01, value = 3),
                                     sliderInput("mcp_nfolds", "CV fold number:",
                                                 min = 3, max = 10, value = 5, step = 1, ticks = FALSE),
                                     numericInput("mcp_pred_at", label = 'Prediction time point:', min = 0, value = 365),
                                     numericInput("mcp_seed", label = 'Set seed:', min = 1, value = 42)
                                   ),
                                   conditionalPanel(
                                     condition = "input.model_type == 'scad'",
                                     numericInput("scad_gamma", "γ:",
                                                  min = 2.01, value = 3.7),
                                     sliderInput("scad_nfolds", "CV fold number:",
                                                 min = 3, max = 10, value = 5, step = 1, ticks = FALSE),
                                     numericInput("scad_pred_at", label = 'Prediction time point:', min = 0, value = 365),
                                     numericInput("scad_seed", label = 'Set seed:', min = 1, value = 42)
                                   ),
                                   conditionalPanel(
                                     condition = "input.model_type == 'mnet'",
                                     numericInput("mnet_gamma", "γ:",
                                                  min = 1.01, value = 3),
                                     sliderInput("mnet_alpha", "α:",
                                                 min = 0, max = 1, value = 0.5, step = 0.01),
                                     sliderInput("mnet_nfolds", "CV fold number:",
                                                 min = 3, max = 10, value = 5, step = 1, ticks = FALSE),
                                     numericInput("mnet_pred_at", label = 'Prediction time point:', min = 0, value = 365),
                                     numericInput("mnet_seed", label = 'Set seed:', min = 1, value = 42)
                                   ),
                                   conditionalPanel(
                                     condition = "input.model_type == 'snet'",
                                     numericInput("snet_gamma", "γ:",
                                                  min = 2.01, value = 3.7),
                                     sliderInput("snet_alpha", "α:",
                                                 min = 0, max = 1, value = 0.5, step = 0.01),
                                     sliderInput("snet_nfolds", "CV fold number:",
                                                 min = 3, max = 10, value = 5, step = 1, ticks = FALSE),
                                     numericInput("snet_pred_at", label = 'Prediction time point:', min = 0, value = 365),
                                     numericInput("snet_seed", label = 'Set seed:', min = 1, value = 42)
                                   ),
                                   actionButton(inputId = 'calcNomogramButton',
                                                label = 'Make Nomogram',
                                                icon = icon('pencil'), class = 'btn-primary')
                      )
               ),

               column(width = 6,
                      mainPanel(width = 12,
                                tabsetPanel(
                                  tabPanel("Plot", plotOutput("plot_nomogram", width = '600px', height = '600px')),
                                  tabPanel("Print", verbatimTextOutput("print_nomogram"))
                                )
                      )
               )

             )

    ),

    tabPanel(title = 'Validation',

             fluidRow(

               column(width = 4, offset = 1,
                      sidebarPanel(width = 12,
                                   selectInput('validate_method', label = 'Choose validation method',
                                               choices = list('Bootstrap' = 'bootstrap',
                                                              'Cross-Validation' = 'cv',
                                                              'Repeated Cross-Validation' = 'repeated.cv'),
                                               selected = 'bootstrap'),
                                   conditionalPanel(
                                     condition = "input.validate_method == 'bootstrap'",
                                     numericInput('validate_boot_times', 'Bootstrap times', min = 1, value = 3, step = 1)
                                   ),
                                   conditionalPanel(
                                     condition = "input.validate_method == 'cv'",
                                     sliderInput('validate_cv_nfolds', 'Cross-validation fold number:', min = 3, max = 10, value = 5, step = 1, ticks = FALSE)
                                   ),
                                   conditionalPanel(
                                     condition = "input.validate_method == 'repeated.cv'",
                                     sliderInput('validate_rcv_nfolds', 'Repeated cross-validation fold number:', min = 3, max = 10, value = 5, step = 1, ticks = FALSE),
                                     numericInput('validate_rcv_rep_times', 'Repeat times', min = 3, value = 3, step = 1)
                                   ),
                                   selectInput('validate_tauc_type', label = 'tAUC Type',
                                               choices = list('Uno (2007)' = 'UNO',
                                                              'Song & Zhou (2008)' = 'SZ',
                                                              'Chambless & Diao (2006)' = 'CD'),
                                               selected = 'UNO'),
                                   numericInput('tauc_from', 'Evaluation Start Time Point:', value = 365),
                                   numericInput('tauc_to', 'Evaluation End Time Point:', value = 730),
                                   numericInput('tauc_by', 'Evaluation Time Point Step Size:', value = 90),
                                   actionButton(inputId = 'calcValidateButton',
                                                label = 'Validate the Model',
                                                icon = icon('cogs'), class = 'btn-primary')
                      )
               ),

               column(width = 6,
                      mainPanel(width = 12,
                                tabsetPanel(
                                  tabPanel("Plot", plotOutput("plot_validate", width = '600px', height = '600px')),
                                  tabPanel("Summary",
                                           h3('Time-Dependent AUC Summary at Evaluation Time Points'),
                                           dataTableOutput("summary_validate")),
                                  tabPanel("Print", verbatimTextOutput("print_validate"))
                                )
                      )
               )

             )

    ),
    tabPanel(title = 'Calibration',

             fluidRow(

               column(width = 4, offset = 1,
                      sidebarPanel(width = 12,
                                   selectInput('calibrate_method', label = 'Choose calibration method',
                                               choices = list('Fitting' = 'fitting',
                                                              'Bootstrap' = 'bootstrap',
                                                              'Cross-Validation' = 'cv',
                                                              'Repeated Cross-Validation' = 'repeated.cv'),
                                               selected = 'fitting'),
                                   conditionalPanel(
                                     condition = "input.calibrate_method == 'bootstrap'",
                                     numericInput('calibrate_boot_times', 'Bootstrap times', min = 1, value = 4, step = 1)
                                   ),
                                   conditionalPanel(
                                     condition = "input.calibrate_method == 'cv'",
                                     sliderInput('calibrate_cv_nfolds', 'Cross-validation fold number:', min = 3, max = 10, value = 5, step = 1, ticks = FALSE)
                                   ),
                                   conditionalPanel(
                                     condition = "input.calibrate_method == 'repeated.cv'",
                                     sliderInput('calibrate_rcv_nfolds', 'Repeated cross-validation fold number:', min = 3, max = 10, value = 5, step = 1, ticks = FALSE),
                                     numericInput('calibrate_rcv_rep_times', 'Repeat times', min = 3, value = 10, step = 1)
                                   ),
                                   sliderInput('calibrate_ngroup', label = 'Number of groups to be formed:',
                                               min = 2, value = 5, max = 10, step = 1, ticks = FALSE),
                                   sliderInput('calibrate_xlim_lo', label = 'Lower limit of plot range:',
                                               min = 0.01, value = 0.5, max = 1, step = 0.01),
                                   sliderInput('calibrate_xlim_up', label = 'Upper limit of plot range:',
                                               min = 0.02, value = 1, max = 1, step = 0.01),
                                   actionButton(inputId = 'calcCalibrateButton',
                                                label = 'Calibrate the Model',
                                                icon = icon('wrench'), class = 'btn-primary')
                      )
               ),

               column(width = 6,
                      mainPanel(width = 12,
                                tabsetPanel(
                                  tabPanel("Plot", plotOutput("plot_calibrate", width = '600px', height = '600px')),
                                  tabPanel("Summary",
                                           h3('Calibration Summary Table'),
                                           dataTableOutput("summary_calibrate")),
                                  tabPanel("Print", verbatimTextOutput("print_calibrate"))
                                )
                      )
               )

             )

    ),
    tabPanel(title = 'Report',
             fluidRow(
               column(width = 10, offset = 1,
                      sidebarPanel(width = 12,
                                   p('Note: please make sure the model, validation, and calibration parameters',
                                     HTML('<br>'),
                                     'are what you wanted before generating and downloading the report.'),
                                   p('Feel free to adjust them and regenerate the results if needed.'),
                                   radioButtons('format', 'Choose report format:', c('PDF', 'HTML', 'Word'),
                                                inline = TRUE),
                                   downloadButton('downloadReport', label = 'Generate & Download Report', class = 'btn-primary')
                      )
               )
             )),
    tabPanel(title = 'Help',
             fluidRow(
               column(width = 10, offset = 1,
                      includeMarkdown('help.md')
               )
             ))
  ))
