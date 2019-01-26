library("shiny")
library("shinythemes")
library("markdown")

shinyUI(
  navbarPage(
    title = "hdnom.io",
    id = "mainnavbar",
    theme = shinytheme("cosmo"),
    inverse = TRUE,
    windowTitle = "hdnom.io - Nomograms for High-Dimensional Data",

    tabPanel(
      title = "Home",

      fluidRow(
        column(
          width = 10, offset = 1,
          div(
            class = "jumbotron",
            h1("hdnom.io"),
            h3("Nomograms for high-dimensional data, built with ease."),
            br(),
            actionButton("learnmore", "Learn More", icon("search"), class = "btn-primary btn-lg")
          ),

          tags$blockquote(
            "“Hiding within those mounds of data is knowledge that could change the life of a patient, or change the world.”",
            tags$small("Atul Butte, Professor, Stanford School of Medicine")
          )
        )
      ),

      fluidRow(
        column(
          width = 10, offset = 1,
          includeMarkdown("include/footer.md")
        )
      )
    ),

    tabPanel(
      title = "Data",

      fluidRow(
        column(
          width = 10, offset = 1,
          sidebarPanel(
            width = 12,
            radioButtons("data_type",
                         label = "Use example data or upload your data:",
                         choices = list(
                           "Load example dataset" = "example",
                           "Upload your dataset" = "upload"
                         ),
                         selected = "example"
            ),
            conditionalPanel(
              condition = "input.data_type == 'upload'",
              p("Please read our ", a("data privacy policy", href = "https://github.com/road2stat/hdnom-doc/blob/master/privacy.md", target = "_blank"), "before uploading any data."),
              p("Read a detailed explanation about the ", a("upload data format", href = "https://github.com/road2stat/hdnom-doc/blob/master/upload.md", target = "_blank"), ". An example dataset is provided below."),
              radioButtons("sep_type",
                           label = "Delimiter Type:",
                           choices = list("Comma" = "comma", "Tab" = "tab", "Semicolon" = "semi"),
                           selected = "comma"
              ),
              fileInput("filex", label = "Upload design matrix X:"),
              a("Sample data x (Comma-separated)", href = "example/x.csv", target = "_blank"),
              HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
              a("Sample data x (Tab-separated)", href = "example/x.tsv", target = "_blank"),
              HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
              a("Sample data x (Semicolon-separated)", href = "example/x.txt", target = "_blank"),
              fileInput("filey", label = "Upload response matrix y:"),
              a("Sample data y (Comma-separated)", href = "example/y.csv", target = "_blank"),
              HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
              a("Sample data y (Tab-separated)", href = "example/y.tsv", target = "_blank"),
              HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
              a("Sample data y (Semicolon-separated)", href = "example/y.txt", target = "_blank")
            )
          )
        ),

        column(
          width = 10, offset = 1,
          mainPanel(
            width = 12,
            tabsetPanel(
              tabPanel(
                "Data Table",
                dataTableOutput("print_dataset")
              ),
              tabPanel("Data Summary", verbatimTextOutput("summary_dataset"))
            )
          )
        )
      )
    ),

    tabPanel(
      title = "Nomogram",

      fluidRow(
        column(
          width = 4, offset = 1,
          sidebarPanel(
            width = 12,
            selectInput("model_type",
                        label = "Select model type:",
                        choices = list(
                          "Lasso" = "lasso",
                          "Adaptive Lasso" = "alasso",
                          "Fused Lasso" = "flasso",
                          "Elastic-Net" = "enet",
                          "Adaptive Elastic-Net" = "aenet",
                          "MCP" = "mcp",
                          "Mnet" = "mnet",
                          "SCAD" = "scad",
                          "Snet" = "snet"
                        ),
                        selected = "lasso"
            ),
            conditionalPanel(
              condition = "input.model_type == 'lasso'",
              sliderInput("lasso_nfolds", "CV fold number:",
                          min = 3, max = 10, value = 5, step = 1, ticks = FALSE
              ),
              radioButtons("lasso_lambda_rule",
                           label = "λ selection rule:",
                           choices = list(
                             "Minimal" = "lambda.min",
                             "1 Standard Error" = "lambda.1se"
                           ),
                           selected = "lambda.1se"
              ),
              numericInput("lasso_pred_at", label = "Prediction time point:", min = 0, value = 365),
              numericInput("lasso_seed", label = "Set seed:", min = 1, value = 42)
            ),
            conditionalPanel(
              condition = "input.model_type == 'alasso'",
              sliderInput("alasso_nfolds", "CV fold number:",
                          min = 3, max = 10, value = 5, step = 1, ticks = FALSE
              ),
              radioButtons("alasso_lambda_rule",
                           label = "λ selection rule:",
                           choices = list(
                             "Minimal" = "lambda.min",
                             "1 Standard Error" = "lambda.1se"
                           ),
                           selected = "lambda.1se"
              ),
              numericInput("alasso_pred_at", label = "Prediction time point:", min = 0, value = 365),
              numericInput("alasso_seed1", label = "Set seed 1:", min = 1, value = 42),
              numericInput("alasso_seed2", label = "Set seed 2:", min = 1, value = 22)
            ),
            conditionalPanel(
              condition = "input.model_type == 'flasso'",
              sliderInput("flasso_nfolds", "CV fold number:",
                          min = 3, max = 10, value = 5, step = 1, ticks = FALSE
              ),
              numericInput("flasso_pred_at", label = "Prediction time point:", min = 0, value = 365),
              numericInput("flasso_seed", label = "Set seed:", min = 1, value = 42)
            ),
            conditionalPanel(
              condition = "input.model_type == 'enet'",
              sliderInput("enet_alpha", "α:",
                          min = 0, max = 1, value = 0.5, step = 0.01
              ),
              sliderInput("enet_nfolds", "CV fold number:",
                          min = 3, max = 10, value = 5, step = 1, ticks = FALSE
              ),
              radioButtons("enet_lambda_rule",
                           label = "λ selection rule:",
                           choices = list(
                             "Minimal" = "lambda.min",
                             "1 Standard Error" = "lambda.1se"
                           ),
                           selected = "lambda.1se"
              ),
              numericInput("enet_pred_at", label = "Prediction time point:", min = 0, value = 365),
              numericInput("enet_seed", label = "Set seed:", min = 1, value = 42)
            ),
            conditionalPanel(
              condition = "input.model_type == 'aenet'",
              sliderInput("aenet_alpha1", "First α:",
                          min = 0, max = 1, value = 0.5, step = 0.01
              ),
              sliderInput("aenet_alpha2", "Second α:",
                          min = 0, max = 1, value = 0.5, step = 0.01
              ),
              sliderInput("aenet_nfolds", "CV fold number:",
                          min = 3, max = 10, value = 5, step = 1, ticks = FALSE
              ),
              radioButtons("aenet_lambda_rule",
                           label = "λ selection rule:",
                           choices = list(
                             "Minimal" = "lambda.min",
                             "1 Standard Error" = "lambda.1se"
                           ),
                           selected = "lambda.1se"
              ),
              numericInput("aenet_pred_at", label = "Prediction time point:", min = 0, value = 365),
              numericInput("aenet_seed1", label = "Set seed 1:", min = 1, value = 42),
              numericInput("aenet_seed2", label = "Set seed 2:", min = 1, value = 22)
            ),
            conditionalPanel(
              condition = "input.model_type == 'mcp'",
              numericInput("mcp_gamma", "γ:",
                           min = 1.01, value = 3
              ),
              sliderInput("mcp_nfolds", "CV fold number:",
                          min = 3, max = 10, value = 5, step = 1, ticks = FALSE
              ),
              numericInput("mcp_pred_at", label = "Prediction time point:", min = 0, value = 365),
              numericInput("mcp_seed", label = "Set seed:", min = 1, value = 42)
            ),
            conditionalPanel(
              condition = "input.model_type == 'scad'",
              numericInput("scad_gamma", "γ:",
                           min = 2.01, value = 3.7
              ),
              sliderInput("scad_nfolds", "CV fold number:",
                          min = 3, max = 10, value = 5, step = 1, ticks = FALSE
              ),
              numericInput("scad_pred_at", label = "Prediction time point:", min = 0, value = 365),
              numericInput("scad_seed", label = "Set seed:", min = 1, value = 42)
            ),
            conditionalPanel(
              condition = "input.model_type == 'mnet'",
              numericInput("mnet_gamma", "γ:",
                           min = 1.01, value = 3
              ),
              sliderInput("mnet_alpha", "α:",
                          min = 0, max = 1, value = 0.5, step = 0.01
              ),
              sliderInput("mnet_nfolds", "CV fold number:",
                          min = 3, max = 10, value = 5, step = 1, ticks = FALSE
              ),
              numericInput("mnet_pred_at", label = "Prediction time point:", min = 0, value = 365),
              numericInput("mnet_seed", label = "Set seed:", min = 1, value = 42)
            ),
            conditionalPanel(
              condition = "input.model_type == 'snet'",
              numericInput("snet_gamma", "γ:",
                           min = 2.01, value = 3.7
              ),
              sliderInput("snet_alpha", "α:",
                          min = 0, max = 1, value = 0.5, step = 0.01
              ),
              sliderInput("snet_nfolds", "CV fold number:",
                          min = 3, max = 10, value = 5, step = 1, ticks = FALSE
              ),
              numericInput("snet_pred_at", label = "Prediction time point:", min = 0, value = 365),
              numericInput("snet_seed", label = "Set seed:", min = 1, value = 42)
            ),
            actionButton(
              inputId = "calcNomogramButton",
              label = "Make Nomogram",
              icon = icon("pencil"), class = "btn-primary"
            )
          )
        ),

        column(
          width = 6,
          mainPanel(
            width = 12,
            tabsetPanel(
              tabPanel("Nomogram", plotOutput("plot_nomogram", width = "600px", height = "600px")),
              tabPanel("Nomogram Information", verbatimTextOutput("print_nomogram")),
              tabPanel("Model Information", verbatimTextOutput("print_model"))
            )
          )
        )
      )
    ),

    navbarMenu(
      "Model Validation",

      tabPanel(
        title = "Internal Validation",

        fluidRow(
          column(
            width = 4, offset = 1,
            sidebarPanel(
              width = 12,
              selectInput("validate_method",
                          label = "Select validation method:",
                          choices = list(
                            "Bootstrap" = "bootstrap",
                            "Cross-Validation" = "cv",
                            "Repeated Cross-Validation" = "repeated.cv"
                          ),
                          selected = "cv"
              ),
              conditionalPanel(
                condition = "input.validate_method == 'bootstrap'",
                sliderInput("validate_boot_times", "Bootstrap times:", min = 2, max = 200, value = 10, step = 1, ticks = FALSE)
              ),
              conditionalPanel(
                condition = "input.validate_method == 'cv'",
                sliderInput("validate_cv_nfolds", "Cross-validation fold number:", min = 3, max = 10, value = 5, step = 1, ticks = FALSE)
              ),
              conditionalPanel(
                condition = "input.validate_method == 'repeated.cv'",
                sliderInput("validate_rcv_nfolds", "Repeated cross-validation fold number:", min = 3, max = 10, value = 5, step = 1, ticks = FALSE),
                sliderInput("validate_rcv_rep_times", "Repeat times:", min = 3, max = 20, value = 10, step = 1, ticks = FALSE)
              ),
              selectInput("validate_tauc_type",
                          label = "Time-dependent AUC type:",
                          choices = list(
                            "Uno (2007)" = "UNO",
                            "Song & Zhou (2008)" = "SZ",
                            "Chambless & Diao (2006)" = "CD"
                          ),
                          selected = "UNO"
              ),
              numericInput("tauc_from", "Evaluation start time point:", value = 365),
              numericInput("tauc_to", "Evaluation end time point:", value = 730),
              numericInput("tauc_by", "Evaluation time point step size:", value = 90),
              numericInput("validate_seed", label = "Set seed:", min = 1, value = 42),
              actionButton(
                inputId = "calcValidateButton",
                label = "Validate the Model",
                icon = icon("cogs"), class = "btn-primary"
              )
            )
          ),

          column(
            width = 6,
            mainPanel(
              width = 12,
              tabsetPanel(
                tabPanel("Validation Plot", plotOutput("plot_validate", width = "600px", height = "600px")),
                tabPanel(
                  "Validation Summary",
                  h3("Time-Dependent AUC Summary at Evaluation Time Points"),
                  dataTableOutput("summary_validate")
                ),
                tabPanel("Validation Information", verbatimTextOutput("print_validate"))
              )
            )
          )
        )
      ),

      tabPanel(
        title = "Internal Calibration",

        fluidRow(
          column(
            width = 4, offset = 1,
            sidebarPanel(
              width = 12,
              selectInput("calibrate_method",
                          label = "Select calibration method:",
                          choices = list(
                            "Fitting" = "fitting",
                            "Bootstrap" = "bootstrap",
                            "Cross-Validation" = "cv",
                            "Repeated Cross-Validation" = "repeated.cv"
                          ),
                          selected = "fitting"
              ),
              conditionalPanel(
                condition = "input.calibrate_method == 'bootstrap'",
                sliderInput("calibrate_boot_times", "Bootstrap times:", min = 2, max = 200, value = 10, step = 1, ticks = FALSE)
              ),
              conditionalPanel(
                condition = "input.calibrate_method == 'cv'",
                sliderInput("calibrate_cv_nfolds", "Cross-validation fold number:", min = 3, max = 10, value = 5, step = 1, ticks = FALSE)
              ),
              conditionalPanel(
                condition = "input.calibrate_method == 'repeated.cv'",
                sliderInput("calibrate_rcv_nfolds", "Repeated cross-validation fold number:", min = 3, max = 10, value = 5, step = 1, ticks = FALSE),
                sliderInput("calibrate_rcv_rep_times", "Repeat times:", min = 3, max = 20, value = 10, step = 1, ticks = FALSE)
              ),
              sliderInput("calibrate_ngroup",
                          label = "Number of groups to be formed:",
                          min = 2, value = 3, max = 9, step = 1, ticks = FALSE
              ),
              numericInput("calibrate_pred_at", label = "Prediction time point:", min = 0, value = 365),
              numericInput("calibrate_seed", label = "Set seed:", min = 1, value = 42),
              sliderInput("calibrate_xlim_lo",
                          label = "Lower limit of plot range:",
                          min = 0.01, value = 0.85, max = 1, step = 0.01
              ),
              sliderInput("calibrate_xlim_up",
                          label = "Upper limit of plot range:",
                          min = 0.02, value = 1, max = 1, step = 0.01
              ),
              actionButton(
                inputId = "calcCalibrateButton",
                label = "Calibrate the Model",
                icon = icon("wrench"), class = "btn-primary"
              )
            )
          ),

          column(
            width = 6,
            mainPanel(
              width = 12,
              tabsetPanel(
                tabPanel("Calibration Plot", plotOutput("plot_calibrate", width = "600px", height = "600px")),
                tabPanel(
                  "Calibration Summary",
                  h3("Calibration Summary Table"),
                  dataTableOutput("summary_calibrate")
                ),
                tabPanel("Calibration Information", verbatimTextOutput("print_calibrate"))
              )
            )
          )
        )
      ),

      tabPanel(
        title = "Kaplan-Meier Analysis for Internal Calibration",

        fluidRow(
          column(
            width = 4, offset = 1,
            sidebarPanel(
              width = 12,
              numericInput("calibrate_km_from", "Evaluation start time point:", value = 365),
              numericInput("calibrate_km_to", "Evaluation end time point:", value = 2190),
              numericInput("calibrate_km_by", "Evaluation time point step size:", value = 365),
              actionButton(
                inputId = "calcCalibrateKMButton",
                label = "Perform K-M Analysis & Log-Rank Test",
                icon = icon("wrench"), class = "btn-primary"
              )
            )
          ),

          column(
            width = 6,
            mainPanel(
              width = 12,
              tabsetPanel(
                tabPanel("Kaplan-Meier Curve", plotOutput("plot_calibrate_km", width = "600px", height = "600px")),
                tabPanel("Log-Rank Test", verbatimTextOutput("print_calibrate_logrank"))
              )
            )
          )
        )
      ),

      tabPanel(
        title = "External Validation",

        fluidRow(
          column(
            width = 4, offset = 1,
            sidebarPanel(
              width = 12,
              radioButtons("ext_val_sep_type",
                           label = "Delimiter Type:",
                           choices = list("Comma" = "comma", "Tab" = "tab", "Semicolon" = "semi"),
                           selected = "comma"
              ),
              fileInput("ext_val_filex", label = "Upload design matrix of the external dataset:"),
              p(
                a("Sample data x (Comma-separated)", href = "example/x_new.csv", target = "_blank"),
                br(),
                a("Sample data x (Tab-separated)", href = "example/x_new.tsv", target = "_blank"),
                br(),
                a("Sample data x (Semicolon-separated)", href = "example/x_new.txt", target = "_blank")
              ),
              fileInput("ext_val_filey", label = "Upload response matrix of the external dataset:"),
              p(
                a("Sample data y (Comma-separated)", href = "example/y_new.csv", target = "_blank"),
                br(),
                a("Sample data y (Tab-separated)", href = "example/y_new.tsv", target = "_blank"),
                br(),
                a("Sample data y (Semicolon-separated)", href = "example/y_new.txt", target = "_blank")
              ),
              selectInput("external_validate_tauc_type",
                          label = "Time-dependent AUC type:",
                          choices = list(
                            "Uno (2007)" = "UNO",
                            "Song & Zhou (2008)" = "SZ",
                            "Chambless & Diao (2006)" = "CD"
                          ),
                          selected = "UNO"
              ),
              numericInput("external_tauc_from", "Evaluation start time point:", value = 365),
              numericInput("external_tauc_to", "Evaluation end time point:", value = 730),
              numericInput("external_tauc_by", "Evaluation time point step size:", value = 90),
              actionButton(
                inputId = "calcExternalValidateButton",
                label = "Validate the Model",
                icon = icon("cogs"), class = "btn-primary"
              )
            )
          ),

          column(
            width = 6,
            mainPanel(
              width = 12,
              tabsetPanel(
                tabPanel("External Validation Plot", plotOutput("plot_external_validate", width = "600px", height = "600px")),
                tabPanel(
                  "External Validation Summary",
                  h3("Time-Dependent AUC Summary at Evaluation Time Points"),
                  dataTableOutput("summary_external_validate")
                ),
                tabPanel("External Validation Information", verbatimTextOutput("print_external_validate"))
              )
            )
          )
        )
      ),

      tabPanel(
        title = "External Calibration",

        fluidRow(
          column(
            width = 4, offset = 1,
            sidebarPanel(
              width = 12,
              radioButtons("ext_cal_sep_type",
                           label = "Delimiter Type:",
                           choices = list("Comma" = "comma", "Tab" = "tab", "Semicolon" = "semi"),
                           selected = "comma"
              ),
              fileInput("ext_cal_filex", label = "Upload design matrix of the external dataset:"),
              p(
                a("Sample data x (Comma-separated)", href = "example/x_new.csv", target = "_blank"),
                br(),
                a("Sample data x (Tab-separated)", href = "example/x_new.tsv", target = "_blank"),
                br(),
                a("Sample data x (Semicolon-separated)", href = "example/x_new.txt", target = "_blank")
              ),
              fileInput("ext_cal_filey", label = "Upload response matrix of the external dataset:"),
              p(
                a("Sample data y (Comma-separated)", href = "example/y_new.csv", target = "_blank"),
                br(),
                a("Sample data y (Tab-separated)", href = "example/y_new.tsv", target = "_blank"),
                br(),
                a("Sample data y (Semicolon-separated)", href = "example/y_new.txt", target = "_blank")
              ),
              sliderInput("external_calibrate_ngroup",
                          label = "Number of groups to be formed:",
                          min = 2, value = 3, max = 9, step = 1, ticks = FALSE
              ),
              numericInput("external_calibrate_pred_at", label = "Prediction time point:", min = 0, value = 365),
              sliderInput("external_calibrate_xlim_lo",
                          label = "Lower limit of plot range:",
                          min = 0.01, value = 0.85, max = 1, step = 0.01
              ),
              sliderInput("external_calibrate_xlim_up",
                          label = "Upper limit of plot range:",
                          min = 0.02, value = 1, max = 1, step = 0.01
              ),
              actionButton(
                inputId = "calcExternalCalibrateButton",
                label = "Calibrate the Model",
                icon = icon("wrench"), class = "btn-primary"
              )
            )
          ),

          column(
            width = 6,
            mainPanel(
              width = 12,
              tabsetPanel(
                tabPanel("External Calibration Plot", plotOutput("plot_external_calibrate", width = "600px", height = "600px")),
                tabPanel(
                  "External Calibration Summary",
                  h3("External Calibration Summary Table"),
                  dataTableOutput("summary_external_calibrate")
                ),
                tabPanel("External Calibration Information", verbatimTextOutput("print_external_calibrate"))
              )
            )
          )
        )
      ),

      tabPanel(
        title = "Kaplan-Meier Analysis for External Calibration",

        fluidRow(
          column(
            width = 4, offset = 1,
            sidebarPanel(
              width = 12,
              numericInput("external_calibrate_km_from", "Evaluation start time point:", value = 365),
              numericInput("external_calibrate_km_to", "Evaluation end time point:", value = 2190),
              numericInput("external_calibrate_km_by", "Evaluation time point step size:", value = 365),
              actionButton(
                inputId = "calcExternalCalibrateKMButton",
                label = "Perform K-M Analysis & Log-Rank Test",
                icon = icon("wrench"), class = "btn-primary"
              )
            )
          ),

          column(
            width = 6,
            mainPanel(
              width = 12,
              tabsetPanel(
                tabPanel("Kaplan-Meier Curve", plotOutput("plot_external_calibrate_km", width = "600px", height = "600px")),
                tabPanel("Log-Rank Test", verbatimTextOutput("print_external_calibrate_logrank"))
              )
            )
          )
        )
      )
    ),

    navbarMenu(
      "Model Comparison",

      tabPanel(
        title = "by Validation",

        fluidRow(
          column(
            width = 4, offset = 1,
            sidebarPanel(
              width = 12,

              checkboxGroupInput("models_compare_validate",
                                 label = "Select model types to compare:",
                                 choices = list(
                                   "Lasso" = "lasso",
                                   "Adaptive Lasso" = "alasso",
                                   "Fused Lasso" = "flasso",
                                   "Elastic-Net" = "enet",
                                   "Adaptive Elastic-Net" = "aenet",
                                   "MCP" = "mcp",
                                   "Mnet" = "mnet",
                                   "SCAD" = "scad",
                                   "Snet" = "snet"
                                 )
              ),

              selectInput("compare_validate_method",
                          label = "Select validation method:",
                          choices = list(
                            "Bootstrap" = "bootstrap",
                            "Cross-Validation" = "cv",
                            "Repeated Cross-Validation" = "repeated.cv"
                          ),
                          selected = "cv"
              ),
              conditionalPanel(
                condition = "input.compare_validate_method == 'bootstrap'",
                sliderInput("compare_validate_boot_times", "Bootstrap times:", min = 2, max = 200, value = 10, step = 1, ticks = FALSE)
              ),
              conditionalPanel(
                condition = "input.compare_validate_method == 'cv'",
                sliderInput("compare_validate_cv_nfolds", "Cross-validation fold number:", min = 3, max = 10, value = 5, step = 1, ticks = FALSE)
              ),
              conditionalPanel(
                condition = "input.compare_validate_method == 'repeated.cv'",
                sliderInput("compare_validate_rcv_nfolds", "Repeated cross-validation fold number:", min = 3, max = 10, value = 5, step = 1, ticks = FALSE),
                sliderInput("compare_validate_rcv_rep_times", "Repeat times:", min = 3, max = 20, value = 10, step = 1, ticks = FALSE)
              ),
              selectInput("compare_validate_tauc_type",
                          label = "Time-dependent AUC type:",
                          choices = list(
                            "Uno (2007)" = "UNO",
                            "Song & Zhou (2008)" = "SZ",
                            "Chambless & Diao (2006)" = "CD"
                          ),
                          selected = "UNO"
              ),
              numericInput("compare_tauc_from", "Evaluation start time point:", value = 365),
              numericInput("compare_tauc_to", "Evaluation end time point:", value = 730),
              numericInput("compare_tauc_by", "Evaluation time point step size:", value = 90),
              numericInput("compare_validate_seed", label = "Set seed:", min = 1, value = 42),
              checkboxInput("compare_validate_interval", label = "Show maximum, minimum, 0.25, and 0.75 quantiles?", value = TRUE),
              actionButton(
                inputId = "calcCompareValidateButton",
                label = "Compare Models",
                icon = icon("cogs"), class = "btn-primary"
              )
            )
          ),

          column(
            width = 6,
            mainPanel(
              width = 12,
              tabsetPanel(
                tabPanel("Validation Plot", plotOutput("plot_compare_validate", width = "600px", height = "600px")),
                tabPanel(
                  "Validation Summary",
                  h3("Validation Summary"),
                  verbatimTextOutput("summary_compare_validate")
                ),
                tabPanel("Validation Information", verbatimTextOutput("print_compare_validate"))
              )
            )
          )
        )
      ),
      tabPanel(
        title = "by Calibration",

        fluidRow(
          column(
            width = 4, offset = 1,
            sidebarPanel(
              width = 12,

              checkboxGroupInput("models_compare_calibrate",
                                 label = "Select model types to compare:",
                                 choices = list(
                                   "Lasso" = "lasso",
                                   "Adaptive Lasso" = "alasso",
                                   "Fused Lasso" = "flasso",
                                   "Elastic-Net" = "enet",
                                   "Adaptive Elastic-Net" = "aenet",
                                   "MCP" = "mcp",
                                   "Mnet" = "mnet",
                                   "SCAD" = "scad",
                                   "Snet" = "snet"
                                 )
              ),

              selectInput("compare_calibrate_method",
                          label = "Select calibration method:",
                          choices = list(
                            "Fitting" = "fitting",
                            "Bootstrap" = "bootstrap",
                            "Cross-Validation" = "cv",
                            "Repeated Cross-Validation" = "repeated.cv"
                          ),
                          selected = "fitting"
              ),
              conditionalPanel(
                condition = "input.compare_calibrate_method == 'bootstrap'",
                sliderInput("compare_calibrate_boot_times", "Bootstrap times:", min = 2, max = 200, value = 10, step = 1, ticks = FALSE)
              ),
              conditionalPanel(
                condition = "input.compare_calibrate_method == 'cv'",
                sliderInput("compare_calibrate_cv_nfolds", "Cross-validation fold number:", min = 3, max = 10, value = 5, step = 1, ticks = FALSE)
              ),
              conditionalPanel(
                condition = "input.compare_calibrate_method == 'repeated.cv'",
                sliderInput("compare_calibrate_rcv_nfolds", "Repeated cross-validation fold number:", min = 3, max = 10, value = 5, step = 1, ticks = FALSE),
                sliderInput("compare_calibrate_rcv_rep_times", "Repeat times:", min = 3, max = 20, value = 10, step = 1, ticks = FALSE)
              ),
              sliderInput("compare_calibrate_ngroup",
                          label = "Number of groups to be formed:",
                          min = 2, value = 3, max = 9, step = 1, ticks = FALSE
              ),
              numericInput("compare_calibrate_pred_at", label = "Prediction time point:", min = 0, value = 365),
              numericInput("compare_calibrate_seed", label = "Set seed:", min = 1, value = 42),
              sliderInput("compare_calibrate_xlim_lo",
                          label = "Lower limit of plot range:",
                          min = 0.01, value = 0.85, max = 1, step = 0.01
              ),
              sliderInput("compare_calibrate_xlim_up",
                          label = "Upper limit of plot range:",
                          min = 0.02, value = 1, max = 1, step = 0.01
              ),
              actionButton(
                inputId = "calcCompareCalibrateButton",
                label = "Compare Models",
                icon = icon("wrench"), class = "btn-primary"
              )
            )
          ),

          column(
            width = 6,
            mainPanel(
              width = 12,
              tabsetPanel(
                tabPanel("Calibration Plot", plotOutput("plot_compare_calibrate", width = "600px", height = "600px")),
                tabPanel(
                  "Calibration Summary",
                  h3("Calibration Summary"),
                  verbatimTextOutput("summary_compare_calibrate")
                ),
                tabPanel("Calibration Information", verbatimTextOutput("print_compare_calibrate"))
              )
            )
          )
        )
      )
    ),

    tabPanel(
      title = "Prediction",

      fluidRow(
        column(
          width = 4, offset = 1,
          sidebarPanel(
            width = 12,
            h4("Please make a nomogram first, then answer the following questions:"),
            uiOutput("prediction_controls")
          )
        ),

        column(
          width = 3,
          sidebarPanel(
            width = 12,
            numericInput("prediction_pred_at",
                         label = "Prediction time point:",
                         min = 0, value = 365
            ),
            actionButton(
              inputId = "calcPredictionButton",
              label = "Predict Survival Probability",
              icon = icon("wrench"), class = "btn-primary"
            )
          )
        ),

        column(
          width = 3,
          mainPanel(
            width = 12,
            tabsetPanel(
              tabPanel(
                "Predicted Overall Survival Probability",
                h2(textOutput("print_prediction"))
              )
            )
          )
        )
      )
    ),

    tabPanel(
      title = "Reports",
      fluidRow(
        column(
          width = 10, offset = 1,
          sidebarPanel(
            width = 12,
            h4("Report & Model Download"),
            p("Please choose one or more from the three types of report according to the\n analysis you have done. The R model object is also available."),
            p("Note: please make sure the parameter settings in the previous steps\n are what you wanted before generating the report.\n Feel free to adjust them and regenerate the results if needed.")
          )
        ),

        column(
          width = 10, offset = 1,
          sidebarPanel(
            width = 5,
            h4("Basic Report"),
            p("Nomogram, internal validation, and calibration results"),
            radioButtons("format_basic", "Choose report format:", c("PDF", "HTML", "Word"),
                         inline = TRUE
            ),
            downloadButton("download_report_basic", label = "Generate & Download Report", class = "btn-primary")
          ),
          sidebarPanel(
            width = 5,
            h4("External Validation Report"),
            p("External validation and calibration results"),
            radioButtons("format_external", "Choose report format:", c("PDF", "HTML", "Word"),
                         inline = TRUE
            ),
            downloadButton("download_report_external", label = "Generate & Download Report", class = "btn-primary")
          )
        )
      ),

      fluidRow(
        column(
          width = 10, offset = 1,
          sidebarPanel(
            width = 5,
            h4("Model Comparison Report"),
            p("Model comparison results"),
            radioButtons("format_compare", "Choose report format:", c("PDF", "HTML", "Word"),
                         inline = TRUE
            ),
            downloadButton("download_report_compare", label = "Generate & Download Report", class = "btn-primary")
          ),
          sidebarPanel(
            width = 5,
            h4("R Model Object"),
            p(
              'Load the model object in R with load("hdnom-model.Rdata") for prediction. Or try',
              a("hdnom appmaker", href = "https://github.com/road2stat/hdnom-appmaker", target = "_blank"),
              "to make your own nomogram app."
            ),
            br(),
            downloadButton("download_model_rdata", label = "Download R Model Object", class = "btn-primary")
          )
        )
      )
    ),

    tabPanel(
      title = "Help",
      fluidRow(
        column(
          width = 10, offset = 1,
          includeMarkdown("include/help.md")
        )
      )
    )
  )
)
