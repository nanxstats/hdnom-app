library("shiny")
library("survival")
library("rmarkdown")
library("knitr")
library("hdnom")

shinyServer(function(input, output, session) {
  observe({
    if (input$learnmore != 0L) {
      updateTabsetPanel(session, "mainnavbar", selected = "Help")
    }
  })

  readData <- reactive({
    if (input$data_type == "upload" & (!is.null(input$filex) & !is.null(input$filey))) {
      switch(
        input$sep_type,
        comma = {
          x <- read.table(file = input$filex$datapath, header = TRUE, sep = ",", as.is = TRUE)
          y <- read.table(file = input$filey$datapath, header = TRUE, sep = ",", as.is = TRUE)
        },
        tab = {
          x <- read.table(file = input$filex$datapath, header = TRUE, sep = "\t", as.is = TRUE)
          y <- read.table(file = input$filey$datapath, header = TRUE, sep = "\t", as.is = TRUE)
        },
        semi = {
          x <- read.table(file = input$filex$datapath, header = TRUE, sep = ";", as.is = TRUE)
          y <- read.table(file = input$filey$datapath, header = TRUE, sep = ";", as.is = TRUE)
        }
      )

      x <- as.matrix(x)
      time <- y[, 1L]
      event <- y[, 2L]
      y <- Surv(time, event)
    } else if (input$data_type == "example") {
      x <- readRDS("data/x.rds")
      y <- readRDS("data/y.rds")
      time <- readRDS("data/time.rds")
      event <- readRDS("data/event.rds")
    } else {
      x <- NULL
      y <- NULL
      time <- NULL
      event <- NULL
    }

    return(list("x" = x, "y" = y, "time" = time, "event" = event))
  })

  readDataExtVal <- reactive({
    if (!is.null(input$ext_val_filex) & !is.null(input$ext_val_filey)) {
      switch(
        input$ext_val_sep_type,
        comma = {
          x_new <- read.table(file = input$ext_val_filex$datapath, header = TRUE, sep = ",", as.is = TRUE)
          y_new <- read.table(file = input$ext_val_filey$datapath, header = TRUE, sep = ",", as.is = TRUE)
        },
        tab = {
          x_new <- read.table(file = input$ext_val_filex$datapath, header = TRUE, sep = "\t", as.is = TRUE)
          y_new <- read.table(file = input$ext_val_filey$datapath, header = TRUE, sep = "\t", as.is = TRUE)
        },
        semi = {
          x_new <- read.table(file = input$ext_val_filex$datapath, header = TRUE, sep = ";", as.is = TRUE)
          y_new <- read.table(file = input$ext_val_filey$datapath, header = TRUE, sep = ";", as.is = TRUE)
        }
      )

      x_new <- as.matrix(x_new)
      time_new <- y_new[, 1L]
      event_new <- y_new[, 2L]
      y_new <- Surv(time_new, event_new)
    } else {
      x_new <- NULL
      y_new <- NULL
      time_new <- NULL
      event_new <- NULL
    }

    return(list("x_new" = x_new, "y_new" = y_new, "time_new" = time_new, "event_new" = event_new))
  })

  readDataExtCal <- reactive({
    if (!is.null(input$ext_cal_filex) & !is.null(input$ext_cal_filey)) {
      switch(
        input$ext_cal_sep_type,
        comma = {
          x_new <- read.table(file = input$ext_cal_filex$datapath, header = TRUE, sep = ",", as.is = TRUE)
          y_new <- read.table(file = input$ext_cal_filey$datapath, header = TRUE, sep = ",", as.is = TRUE)
        },
        tab = {
          x_new <- read.table(file = input$ext_cal_filex$datapath, header = TRUE, sep = "\t", as.is = TRUE)
          y_new <- read.table(file = input$ext_cal_filey$datapath, header = TRUE, sep = "\t", as.is = TRUE)
        },
        semi = {
          x_new <- read.table(file = input$ext_cal_filex$datapath, header = TRUE, sep = ";", as.is = TRUE)
          y_new <- read.table(file = input$ext_cal_filey$datapath, header = TRUE, sep = ";", as.is = TRUE)
        }
      )

      x_new <- as.matrix(x_new)
      time_new <- y_new[, 1L]
      event_new <- y_new[, 2L]
      y_new <- Surv(time_new, event_new)
    } else {
      x_new <- NULL
      y_new <- NULL
      time_new <- NULL
      event_new <- NULL
    }

    return(list("x_new" = x_new, "y_new" = y_new, "time_new" = time_new, "event_new" = event_new))
  })

  calcNomogram <- eventReactive(input$calcNomogramButton, {
    loadedData <- readData()
    x <- loadedData$"x"
    y <- loadedData$"y"
    time <- loadedData$"time"
    event <- loadedData$"event"

    if (is.null(x) | is.null(y) | is.null(time) | is.null(event)) {
      stop("Please check if the datasets were uploaded correctly.")
    }

    x.df <- as.data.frame(x)
    #     dd <<- datadist(x.df)  # TODO: randomfy this variable name
    #     options(datadists = 'dd')

    # a hack to randomize global variable name to avoid conflict across sessions
    rndstr <- gsub("\\.", "", as.character(runif(1)))
    eval(parse(text = paste0("dd_", rndstr, " <<- rms::datadist(x.df)")))
    eval(parse(text = paste0("options(datadists = 'dd_", rndstr, "')")))

    if (input$model_type %in% c("flasso") & nrow(x) >= 501L) {
      return(stop("Considering long computation time, the fused lasso model
                  now only supports data with <= 500 samples.
                  Please try other model types or use the hdnom package instead."))
    }

    switch(
      input$model_type,
      lasso = {
        object <- hdcox.lasso(
          x = x, y = y, nfolds = input$lasso_nfolds,
          rule = input$lasso_lambda_rule,
          seed = input$lasso_seed
        )

        nom <- hdnom.nomogram(object$lasso_model,
                              model.type = "lasso",
                              x, time, event, ddist = x.df,
                              pred.at = input$lasso_pred_at,
                              funlabel = "Predicted OS Prob."
        )
      },
      alasso = {
        object <- hdcox.alasso(
          x = x, y = y, nfolds = input$alasso_nfolds,
          rule = input$alasso_lambda_rule,
          seed = c(input$alasso_seed1, input$alasso_seed2)
        )

        nom <- hdnom.nomogram(object$alasso_model,
                              model.type = "alasso",
                              x, time, event, ddist = x.df,
                              pred.at = input$alasso_pred_at,
                              funlabel = "Predicted OS Prob."
        )
      },
      enet = {
        object <- hdcox.enet(
          x = x, y = y, nfolds = input$enet_nfolds,
          alphas = input$enet_alpha,
          rule = input$enet_lambda_rule,
          seed = input$enet_seed
        )

        nom <- hdnom.nomogram(object$enet_model,
                              model.type = "enet",
                              x, time, event, ddist = x.df,
                              pred.at = input$enet_pred_at,
                              funlabel = "Predicted OS Prob."
        )
      },
      aenet = {
        object <- hdcox.aenet(
          x = x, y = y, nfolds = input$aenet_nfolds,
          alphas = c(input$aenet_alpha1, input$aenet_alpha2),
          rule = input$aenet_lambda_rule,
          seed = c(input$aenet_seed1, input$aenet_seed2)
        )

        nom <- hdnom.nomogram(object$aenet_model,
                              model.type = "aenet",
                              x, time, event, ddist = x.df,
                              pred.at = input$aenet_pred_at,
                              funlabel = "Predicted OS Prob."
        )
      },
      flasso = {
        object <- hdcox.flasso(
          x = x, y = y,
          nfolds = input$flasso_nfolds,
          seed = input$flasso_seed
        )

        nom <- hdnom.nomogram(object$flasso_model,
                              model.type = "flasso",
                              x, time, event, ddist = x.df,
                              pred.at = input$flasso_pred_at,
                              funlabel = "Predicted OS Prob."
        )
      },
      mcp = {
        object <- hdcox.mcp(
          x = x, y = y,
          nfolds = input$mcp_nfolds,
          gammas = input$mcp_gamma,
          seed = input$mcp_seed
        )

        nom <- hdnom.nomogram(object$mcp_model,
                              model.type = "mcp",
                              x, time, event, ddist = x.df,
                              pred.at = input$mcp_pred_at,
                              funlabel = "Predicted OS Prob."
        )
      },
      mnet = {
        object <- hdcox.mnet(
          x = x, y = y,
          nfolds = input$mnet_nfolds,
          gammas = input$mnet_gamma,
          alphas = input$mnet_alpha,
          seed = input$mnet_seed
        )

        nom <- hdnom.nomogram(object$mnet_model,
                              model.type = "mnet",
                              x, time, event, ddist = x.df,
                              pred.at = input$mnet_pred_at,
                              funlabel = "Predicted OS Prob."
        )
      },
      scad = {
        object <- hdcox.scad(
          x = x, y = y,
          nfolds = input$scad_nfolds,
          gammas = input$scad_gamma,
          seed = input$scad_seed
        )

        nom <- hdnom.nomogram(object$scad_model,
                              model.type = "scad",
                              x, time, event, ddist = x.df,
                              pred.at = input$scad_pred_at,
                              funlabel = "Predicted OS Prob."
        )
      },
      snet = {
        object <- hdcox.snet(
          x = x, y = y,
          nfolds = input$snet_nfolds,
          gammas = input$snet_gamma,
          alphas = input$snet_alpha,
          seed = input$snet_seed
        )

        nom <- hdnom.nomogram(object$snet_model,
                              model.type = "snet",
                              x, time, event, ddist = x.df,
                              pred.at = input$snet_pred_at,
                              funlabel = "Predicted OS Prob."
        )
      }
    )

    list("object" = object, "nom" = nom)
  })

  calcValidate <- eventReactive(input$calcValidateButton, {
    loadedData <- readData()
    x <- loadedData$"x"
    time <- loadedData$"time"
    event <- loadedData$"event"

    calcedNomogram <- calcNomogram()
    modelObject <- calcedNomogram$"object"

    switch(
      input$validate_method,
      bootstrap = {
        valObj <- switch(input$model_type,
                         lasso = {
                           hdnom.validate(x, time, event,
                                          model.type = "lasso",
                                          alpha = 1, lambda = modelObject$"lasso_best_lambda",
                                          method = "bootstrap", boot.times = input$validate_boot_times,
                                          tauc.type = input$validate_tauc_type,
                                          tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                          seed = input$validate_seed,
                                          trace = FALSE
                           )
                         },
                         alasso = {
                           hdnom.validate(x, time, event,
                                          model.type = "alasso",
                                          alpha = 1, lambda = modelObject$"alasso_best_lambda",
                                          pen.factor = modelObject$"pen_factor",
                                          method = "bootstrap", boot.times = input$validate_boot_times,
                                          tauc.type = input$validate_tauc_type,
                                          tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                          seed = input$validate_seed,
                                          trace = FALSE
                           )
                         },
                         enet = {
                           hdnom.validate(x, time, event,
                                          model.type = "enet",
                                          alpha = input$enet_alpha, lambda = modelObject$"enet_best_lambda",
                                          method = "bootstrap", boot.times = input$validate_boot_times,
                                          tauc.type = input$validate_tauc_type,
                                          tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                          seed = input$validate_seed,
                                          trace = FALSE
                           )
                         },
                         aenet = {
                           hdnom.validate(x, time, event,
                                          model.type = "aenet",
                                          alpha = modelObject$"aenet_best_alpha",
                                          lambda = modelObject$"aenet_best_lambda",
                                          pen.factor = modelObject$"pen_factor",
                                          method = "bootstrap", boot.times = input$validate_boot_times,
                                          tauc.type = input$validate_tauc_type,
                                          tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                          seed = input$validate_seed,
                                          trace = FALSE
                           )
                         },
                         flasso = {
                           hdnom.validate(x, time, event,
                                          model.type = "flasso",
                                          lambda = modelObject$"flasso_best_lambda",
                                          method = "bootstrap", boot.times = input$validate_boot_times,
                                          tauc.type = input$validate_tauc_type,
                                          tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                          seed = input$validate_seed,
                                          trace = FALSE
                           )
                         },
                         mcp = {
                           hdnom.validate(x, time, event,
                                          model.type = "mcp", alpha = 1,
                                          gamma = input$mcp_gamma, lambda = modelObject$"mcp_best_lambda",
                                          method = "bootstrap", boot.times = input$validate_boot_times,
                                          tauc.type = input$validate_tauc_type,
                                          tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                          seed = input$validate_seed,
                                          trace = FALSE
                           )
                         },
                         mnet = {
                           hdnom.validate(x, time, event,
                                          model.type = "mnet", alpha = input$mnet_alpha,
                                          gamma = input$mnet_gamma, lambda = modelObject$"mnet_best_lambda",
                                          method = "bootstrap", boot.times = input$validate_boot_times,
                                          tauc.type = input$validate_tauc_type,
                                          tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                          seed = input$validate_seed,
                                          trace = FALSE
                           )
                         },
                         scad = {
                           hdnom.validate(x, time, event,
                                          model.type = "scad", alpha = 1,
                                          gamma = input$scad_gamma, lambda = modelObject$"scad_best_lambda",
                                          method = "bootstrap", boot.times = input$validate_boot_times,
                                          tauc.type = input$validate_tauc_type,
                                          tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                          seed = input$validate_seed,
                                          trace = FALSE
                           )
                         },
                         snet = {
                           hdnom.validate(x, time, event,
                                          model.type = "snet", alpha = input$snet_alpha,
                                          gamma = input$snet_gamma, lambda = modelObject$"snet_best_lambda",
                                          method = "bootstrap", boot.times = input$validate_boot_times,
                                          tauc.type = input$validate_tauc_type,
                                          tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                          seed = input$validate_seed,
                                          trace = FALSE
                           )
                         }
        )
      },

      cv = {
        valObj <-
          switch(
            input$model_type,
            lasso = {
              hdnom.validate(x, time, event,
                             model.type = "lasso",
                             alpha = 1, lambda = modelObject$"lasso_best_lambda",
                             method = "cv", nfolds = input$validate_cv_nfolds,
                             tauc.type = input$validate_tauc_type,
                             tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                             seed = input$validate_seed,
                             trace = FALSE
              )
            },
            alasso = {
              hdnom.validate(x, time, event,
                             model.type = "alasso",
                             alpha = 1, lambda = modelObject$"alasso_best_lambda",
                             pen.factor = modelObject$"pen_factor",
                             method = "cv", nfolds = input$validate_cv_nfolds,
                             tauc.type = input$validate_tauc_type,
                             tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                             seed = input$validate_seed,
                             trace = FALSE
              )
            },
            enet = {
              hdnom.validate(x, time, event,
                             model.type = "enet",
                             alpha = input$enet_alpha, lambda = modelObject$"enet_best_lambda",
                             method = "cv", nfolds = input$validate_cv_nfolds,
                             tauc.type = input$validate_tauc_type,
                             tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                             seed = input$validate_seed,
                             trace = FALSE
              )
            },
            aenet = {
              hdnom.validate(x, time, event,
                             model.type = "aenet",
                             alpha = modelObject$"aenet_best_alpha",
                             lambda = modelObject$"aenet_best_lambda",
                             pen.factor = modelObject$"pen_factor",
                             method = "cv", nfolds = input$validate_cv_nfolds,
                             tauc.type = input$validate_tauc_type,
                             tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                             seed = input$validate_seed,
                             trace = FALSE
              )
            },
            flasso = {
              hdnom.validate(x, time, event,
                             model.type = "flasso",
                             lambda = modelObject$"flasso_best_lambda",
                             method = "cv", nfolds = input$validate_cv_nfolds,
                             tauc.type = input$validate_tauc_type,
                             tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                             seed = input$validate_seed,
                             trace = FALSE
              )
            },
            mcp = {
              hdnom.validate(x, time, event,
                             model.type = "mcp", alpha = 1,
                             gamma = input$mcp_gamma, lambda = modelObject$"mcp_best_lambda",
                             method = "cv", nfolds = input$validate_cv_nfolds,
                             tauc.type = input$validate_tauc_type,
                             tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                             seed = input$validate_seed,
                             trace = FALSE
              )
            },
            mnet = {
              hdnom.validate(x, time, event,
                             model.type = "mnet", alpha = input$mnet_alpha,
                             gamma = input$mnet_gamma, lambda = modelObject$"mnet_best_lambda",
                             method = "cv", nfolds = input$validate_cv_nfolds,
                             tauc.type = input$validate_tauc_type,
                             tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                             seed = input$validate_seed,
                             trace = FALSE
              )
            },
            scad = {
              hdnom.validate(x, time, event,
                             model.type = "scad", alpha = 1,
                             gamma = input$scad_gamma, lambda = modelObject$"scad_best_lambda",
                             method = "cv", nfolds = input$validate_cv_nfolds,
                             tauc.type = input$validate_tauc_type,
                             tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                             seed = input$validate_seed,
                             trace = FALSE
              )
            },
            snet = {
              hdnom.validate(x, time, event,
                             model.type = "snet", alpha = input$snet_alpha,
                             gamma = input$snet_gamma, lambda = modelObject$"snet_best_lambda",
                             method = "cv", nfolds = input$validate_cv_nfolds,
                             tauc.type = input$validate_tauc_type,
                             tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                             seed = input$validate_seed,
                             trace = FALSE
              )
            }
          )
      },

      repeated.cv = {
        valObj <-
          switch(
            input$model_type,
            lasso = {
              hdnom.validate(x, time, event,
                             model.type = "lasso",
                             alpha = 1, lambda = modelObject$"lasso_best_lambda",
                             method = "repeated.cv", nfolds = input$validate_rcv_nfolds, rep.times = input$validate_rcv_rep_times,
                             tauc.type = input$validate_tauc_type,
                             tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                             seed = input$validate_seed,
                             trace = FALSE
              )
            },
            alasso = {
              hdnom.validate(x, time, event,
                             model.type = "alasso",
                             alpha = 1, lambda = modelObject$"alasso_best_lambda",
                             pen.factor = modelObject$"pen_factor",
                             method = "repeated.cv", nfolds = input$validate_rcv_nfolds, rep.times = input$validate_rcv_rep_times,
                             tauc.type = input$validate_tauc_type,
                             tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                             seed = input$validate_seed,
                             trace = FALSE
              )
            },
            enet = {
              hdnom.validate(x, time, event,
                             model.type = "enet",
                             alpha = input$enet_alpha, lambda = modelObject$"enet_best_lambda",
                             method = "repeated.cv", nfolds = input$validate_rcv_nfolds, rep.times = input$validate_rcv_rep_times,
                             tauc.type = input$validate_tauc_type,
                             tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                             seed = input$validate_seed,
                             trace = FALSE
              )
            },
            aenet = {
              hdnom.validate(x, time, event,
                             model.type = "aenet",
                             alpha = modelObject$"aenet_best_alpha",
                             lambda = modelObject$"aenet_best_lambda",
                             pen.factor = modelObject$"pen_factor",
                             method = "repeated.cv", nfolds = input$validate_rcv_nfolds, rep.times = input$validate_rcv_rep_times,
                             tauc.type = input$validate_tauc_type,
                             tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                             seed = input$validate_seed,
                             trace = FALSE
              )
            },
            flasso = {
              hdnom.validate(x, time, event,
                             model.type = "flasso",
                             lambda = modelObject$"flasso_best_lambda",
                             method = "repeated.cv", nfolds = input$validate_rcv_nfolds, rep.times = input$validate_rcv_rep_times,
                             tauc.type = input$validate_tauc_type,
                             tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                             seed = input$validate_seed,
                             trace = FALSE
              )
            },
            mcp = {
              hdnom.validate(x, time, event,
                             model.type = "mcp", alpha = 1,
                             gamma = input$mcp_gamma, lambda = modelObject$"mcp_best_lambda",
                             method = "repeated.cv", nfolds = input$validate_rcv_nfolds, rep.times = input$validate_rcv_rep_times,
                             tauc.type = input$validate_tauc_type,
                             tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                             seed = input$validate_seed,
                             trace = FALSE
              )
            },
            mnet = {
              hdnom.validate(x, time, event,
                             model.type = "mnet", alpha = input$mnet_alpha,
                             gamma = input$mnet_gamma, lambda = modelObject$"mnet_best_lambda",
                             method = "repeated.cv", nfolds = input$validate_rcv_nfolds, rep.times = input$validate_rcv_rep_times,
                             tauc.type = input$validate_tauc_type,
                             tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                             seed = input$validate_seed,
                             trace = FALSE
              )
            },
            scad = {
              hdnom.validate(x, time, event,
                             model.type = "scad", alpha = 1,
                             gamma = input$scad_gamma, lambda = modelObject$"scad_best_lambda",
                             method = "repeated.cv", nfolds = input$validate_rcv_nfolds, rep.times = input$validate_rcv_rep_times,
                             tauc.type = input$validate_tauc_type,
                             tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                             seed = input$validate_seed,
                             trace = FALSE
              )
            },
            snet = {
              hdnom.validate(x, time, event,
                             model.type = "snet", alpha = input$snet_alpha,
                             gamma = input$snet_gamma, lambda = modelObject$"snet_best_lambda",
                             method = "repeated.cv", nfolds = input$validate_rcv_nfolds, rep.times = input$validate_rcv_rep_times,
                             tauc.type = input$validate_tauc_type,
                             tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                             seed = input$validate_seed,
                             trace = FALSE
              )
            }
          )
      }
    )

    valObj
  })

  calcCalibrate <- eventReactive(input$calcCalibrateButton, {
    loadedData <- readData()
    x <- loadedData$"x"
    time <- loadedData$"time"
    event <- loadedData$"event"

    calcedNomogram <- calcNomogram()
    modelObject <- calcedNomogram$"object"

    switch(
      input$calibrate_method,

      fitting = {
        calObj <-
          switch(input$model_type,
                 lasso = {
                   hdnom.calibrate(x, time, event,
                                   model.type = "lasso",
                                   alpha = 1, lambda = modelObject$"lasso_best_lambda",
                                   method = "fitting",
                                   pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                                   seed = input$calibrate_seed,
                                   trace = FALSE
                   )
                 },
                 alasso = {
                   hdnom.calibrate(x, time, event,
                                   model.type = "alasso",
                                   alpha = 1, lambda = modelObject$"alasso_best_lambda",
                                   pen.factor = modelObject$"pen_factor",
                                   method = "fitting",
                                   pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                                   seed = input$calibrate_seed,
                                   trace = FALSE
                   )
                 },
                 enet = {
                   hdnom.calibrate(x, time, event,
                                   model.type = "enet",
                                   alpha = input$enet_alpha, lambda = modelObject$"enet_best_lambda",
                                   method = "fitting",
                                   pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                                   seed = input$calibrate_seed,
                                   trace = FALSE
                   )
                 },
                 aenet = {
                   hdnom.calibrate(x, time, event,
                                   model.type = "aenet",
                                   alpha = modelObject$"aenet_best_alpha",
                                   lambda = modelObject$"aenet_best_lambda",
                                   pen.factor = modelObject$"pen_factor",
                                   method = "fitting",
                                   pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                                   seed = input$calibrate_seed,
                                   trace = FALSE
                   )
                 },
                 flasso = {
                   hdnom.calibrate(x, time, event,
                                   model.type = "flasso",
                                   lambda = modelObject$"flasso_best_lambda",
                                   method = "fitting",
                                   pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                                   seed = input$calibrate_seed,
                                   trace = FALSE
                   )
                 },
                 mcp = {
                   hdnom.calibrate(x, time, event,
                                   model.type = "mcp", alpha = 1,
                                   gamma = input$mcp_gamma, lambda = modelObject$"mcp_best_lambda",
                                   method = "fitting",
                                   pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                                   seed = input$calibrate_seed,
                                   trace = FALSE
                   )
                 },
                 mnet = {
                   hdnom.calibrate(x, time, event,
                                   model.type = "mnet", alpha = input$mnet_alpha,
                                   gamma = input$mnet_gamma, lambda = modelObject$"mnet_best_lambda",
                                   method = "fitting",
                                   pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                                   seed = input$calibrate_seed,
                                   trace = FALSE
                   )
                 },
                 scad = {
                   hdnom.calibrate(x, time, event,
                                   model.type = "scad", alpha = 1,
                                   gamma = input$scad_gamma, lambda = modelObject$"scad_best_lambda",
                                   method = "fitting",
                                   pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                                   seed = input$calibrate_seed,
                                   trace = FALSE
                   )
                 },
                 snet = {
                   hdnom.calibrate(x, time, event,
                                   model.type = "snet", alpha = input$snet_alpha,
                                   gamma = input$snet_gamma, lambda = modelObject$"snet_best_lambda",
                                   method = "fitting",
                                   pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                                   seed = input$calibrate_seed,
                                   trace = FALSE
                   )
                 }
          )
      },

      bootstrap = {
        calObj <-
          switch(
            input$model_type,
            lasso = {
              hdnom.calibrate(x, time, event,
                              model.type = "lasso",
                              alpha = 1, lambda = modelObject$"lasso_best_lambda",
                              method = "bootstrap", boot.times = input$calibrate_boot_times,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            alasso = {
              hdnom.calibrate(x, time, event,
                              model.type = "alasso",
                              alpha = 1, lambda = modelObject$"alasso_best_lambda",
                              pen.factor = modelObject$"pen_factor",
                              method = "bootstrap", boot.times = input$calibrate_boot_times,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            enet = {
              hdnom.calibrate(x, time, event,
                              model.type = "enet",
                              alpha = input$enet_alpha, lambda = modelObject$"enet_best_lambda",
                              method = "bootstrap", boot.times = input$calibrate_boot_times,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            aenet = {
              hdnom.calibrate(x, time, event,
                              model.type = "aenet",
                              alpha = modelObject$"aenet_best_alpha",
                              lambda = modelObject$"aenet_best_lambda",
                              pen.factor = modelObject$"pen_factor",
                              method = "bootstrap", boot.times = input$calibrate_boot_times,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            flasso = {
              hdnom.calibrate(x, time, event,
                              model.type = "flasso",
                              lambda = modelObject$"flasso_best_lambda",
                              method = "bootstrap", boot.times = input$calibrate_boot_times,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            mcp = {
              hdnom.calibrate(x, time, event,
                              model.type = "mcp", alpha = 1,
                              gamma = input$mcp_gamma, lambda = modelObject$"mcp_best_lambda",
                              method = "bootstrap", boot.times = input$calibrate_boot_times,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            mnet = {
              hdnom.calibrate(x, time, event,
                              model.type = "mnet", alpha = input$mnet_alpha,
                              gamma = input$mnet_gamma, lambda = modelObject$"mnet_best_lambda",
                              method = "bootstrap", boot.times = input$calibrate_boot_times,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            scad = {
              hdnom.calibrate(x, time, event,
                              model.type = "scad", alpha = 1,
                              gamma = input$scad_gamma, lambda = modelObject$"scad_best_lambda",
                              method = "bootstrap", boot.times = input$calibrate_boot_times,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            snet = {
              hdnom.calibrate(x, time, event,
                              model.type = "snet", alpha = input$snet_alpha,
                              gamma = input$snet_gamma, lambda = modelObject$"snet_best_lambda",
                              method = "bootstrap", boot.times = input$calibrate_boot_times,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            }
          )
      },

      cv = {
        calObj <-
          switch(
            input$model_type,
            lasso = {
              hdnom.calibrate(x, time, event,
                              model.type = "lasso",
                              alpha = 1, lambda = modelObject$"lasso_best_lambda",
                              method = "cv", nfolds = input$calibrate_cv_nfolds,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            alasso = {
              hdnom.calibrate(x, time, event,
                              model.type = "alasso",
                              alpha = 1, lambda = modelObject$"alasso_best_lambda",
                              pen.factor = modelObject$"pen_factor",
                              method = "cv", nfolds = input$calibrate_cv_nfolds,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            enet = {
              hdnom.calibrate(x, time, event,
                              model.type = "enet",
                              alpha = input$enet_alpha, lambda = modelObject$"enet_best_lambda",
                              method = "cv", nfolds = input$calibrate_cv_nfolds,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            aenet = {
              hdnom.calibrate(x, time, event,
                              model.type = "aenet",
                              alpha = modelObject$"aenet_best_alpha",
                              lambda = modelObject$"aenet_best_lambda",
                              pen.factor = modelObject$"pen_factor",
                              method = "cv", nfolds = input$calibrate_cv_nfolds,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            flasso = {
              hdnom.calibrate(x, time, event,
                              model.type = "flasso",
                              lambda = modelObject$"flasso_best_lambda",
                              method = "cv", nfolds = input$calibrate_cv_nfolds,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            mcp = {
              hdnom.calibrate(x, time, event,
                              model.type = "mcp", alpha = 1,
                              gamma = input$mcp_gamma, lambda = modelObject$"mcp_best_lambda",
                              method = "cv", nfolds = input$calibrate_cv_nfolds,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            mnet = {
              hdnom.calibrate(x, time, event,
                              model.type = "mnet", alpha = input$mnet_alpha,
                              gamma = input$mnet_gamma, lambda = modelObject$"mnet_best_lambda",
                              method = "cv", nfolds = input$calibrate_cv_nfolds,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            scad = {
              hdnom.calibrate(x, time, event,
                              model.type = "scad", alpha = 1,
                              gamma = input$scad_gamma, lambda = modelObject$"scad_best_lambda",
                              method = "cv", nfolds = input$calibrate_cv_nfolds,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            snet = {
              hdnom.calibrate(x, time, event,
                              model.type = "snet", alpha = input$snet_alpha,
                              gamma = input$snet_gamma, lambda = modelObject$"snet_best_lambda",
                              method = "cv", nfolds = input$calibrate_cv_nfolds,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            }
          )
      },

      repeated.cv = {
        calObj <-
          switch(
            input$model_type,
            lasso = {
              hdnom.calibrate(x, time, event,
                              model.type = "lasso",
                              alpha = 1, lambda = modelObject$"lasso_best_lambda",
                              method = "repeated.cv", nfolds = input$calibrate_rcv_nfolds, rep.times = input$calibrate_rcv_rep_times,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            alasso = {
              hdnom.calibrate(x, time, event,
                              model.type = "alasso",
                              alpha = 1, lambda = modelObject$"alasso_best_lambda",
                              pen.factor = modelObject$"pen_factor",
                              method = "repeated.cv", nfolds = input$calibrate_rcv_nfolds, rep.times = input$calibrate_rcv_rep_times,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            enet = {
              hdnom.calibrate(x, time, event,
                              model.type = "enet",
                              alpha = input$enet_alpha, lambda = modelObject$"enet_best_lambda",
                              method = "repeated.cv", nfolds = input$calibrate_rcv_nfolds, rep.times = input$calibrate_rcv_rep_times,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            aenet = {
              hdnom.calibrate(x, time, event,
                              model.type = "aenet",
                              alpha = modelObject$"aenet_best_alpha",
                              lambda = modelObject$"aenet_best_lambda",
                              pen.factor = modelObject$"pen_factor",
                              method = "repeated.cv", nfolds = input$calibrate_rcv_nfolds, rep.times = input$calibrate_rcv_rep_times,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            flasso = {
              hdnom.calibrate(x, time, event,
                              model.type = "flasso",
                              lambda = modelObject$"flasso_best_lambda",
                              method = "repeated.cv", nfolds = input$calibrate_rcv_nfolds, rep.times = input$calibrate_rcv_rep_times,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            mcp = {
              hdnom.calibrate(x, time, event,
                              model.type = "mcp", alpha = 1,
                              gamma = input$mcp_gamma, lambda = modelObject$"mcp_best_lambda",
                              method = "repeated.cv", nfolds = input$calibrate_rcv_nfolds, rep.times = input$calibrate_rcv_rep_times,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            mnet = {
              hdnom.calibrate(x, time, event,
                              model.type = "mnet", alpha = input$mnet_alpha,
                              gamma = input$mnet_gamma, lambda = modelObject$"mnet_best_lambda",
                              method = "repeated.cv", nfolds = input$calibrate_rcv_nfolds, rep.times = input$calibrate_rcv_rep_times,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            scad = {
              hdnom.calibrate(x, time, event,
                              model.type = "scad", alpha = 1,
                              gamma = input$scad_gamma, lambda = modelObject$"scad_best_lambda",
                              method = "repeated.cv", nfolds = input$calibrate_rcv_nfolds, rep.times = input$calibrate_rcv_rep_times,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            },
            snet = {
              hdnom.calibrate(x, time, event,
                              model.type = "snet", alpha = input$snet_alpha,
                              gamma = input$snet_gamma, lambda = modelObject$"snet_best_lambda",
                              method = "repeated.cv", nfolds = input$calibrate_rcv_nfolds, rep.times = input$calibrate_rcv_rep_times,
                              pred.at = input$calibrate_pred_at, ngroup = input$calibrate_ngroup,
                              seed = input$calibrate_seed,
                              trace = FALSE
              )
            }
          )
      }
    )

    calObj
  })

  calcCalibrateKM <- eventReactive(input$calcCalibrateKMButton, {
    calcedCalibrate <- calcCalibrate()

    calKMObj <-
      hdnom.kmplot(
        calcedCalibrate,
        time.at = seq(input$calibrate_km_from, input$calibrate_km_to, input$calibrate_km_by)
      )

    calKMObj
  })

  calcCalibrateLogRank <- eventReactive(input$calcCalibrateKMButton, {
    calcedCalibrate <- calcCalibrate()

    calLogRankObj <- hdnom.logrank(calcedCalibrate)

    calLogRankObj
  })

  calcExternalValidate <- eventReactive(input$calcExternalValidateButton, {
    loadedData <- readData()
    x <- loadedData$"x"
    time <- loadedData$"time"
    event <- loadedData$"event"

    calcedNomogram <- calcNomogram()
    modelObject <- calcedNomogram$"object"

    loadedDataExtVal <- readDataExtVal()
    x_new <- loadedDataExtVal$"x_new"
    time_new <- loadedDataExtVal$"time_new"
    event_new <- loadedDataExtVal$"event_new"

    if (is.null(x_new) | is.null(time_new) | is.null(event_new)) {
      stop("Please upload the external dataset first.")
    }

    extValObj <-
      hdnom.external.validate(
        modelObject, x, time, event, x_new, time_new, event_new,
        tauc.type = input$external_validate_tauc_type,
        tauc.time = seq(input$external_tauc_from, input$external_tauc_to, input$external_tauc_by)
      )

    extValObj
  })

  calcExternalCalibrate <- eventReactive(input$calcExternalCalibrateButton, {
    loadedData <- readData()
    x <- loadedData$"x"
    time <- loadedData$"time"
    event <- loadedData$"event"

    calcedNomogram <- calcNomogram()
    modelObject <- calcedNomogram$"object"

    loadedDataExtCal <- readDataExtCal()
    x_new <- loadedDataExtCal$"x_new"
    time_new <- loadedDataExtCal$"time_new"
    event_new <- loadedDataExtCal$"event_new"

    if (is.null(x_new) | is.null(time_new) | is.null(event_new)) {
      stop("Please upload the external dataset first.")
    }

    extCalObj <-
      hdnom.external.calibrate(
        modelObject, x, time, event, x_new, time_new, event_new,
        pred.at = input$external_calibrate_pred_at,
        ngroup = input$external_calibrate_ngroup
      )

    extCalObj
  })

  calcExternalCalibrateKM <- eventReactive(input$calcExternalCalibrateKMButton, {
    calcedExternalCalibrate <- calcExternalCalibrate()

    extCalKMObj <-
      hdnom.kmplot(
        calcedExternalCalibrate,
        time.at = seq(input$external_calibrate_km_from, input$external_calibrate_km_to, input$external_calibrate_km_by)
      )

    extCalKMObj
  })

  calcExternalCalibrateLogRank <- eventReactive(input$calcExternalCalibrateKMButton, {
    calcedExternalCalibrate <- calcExternalCalibrate()

    extCalLogRankObj <- hdnom.logrank(calcedExternalCalibrate)

    extCalLogRankObj
  })

  calcCompareValidate <- eventReactive(input$calcCompareValidateButton, {
    loadedData <- readData()
    x <- loadedData$"x"
    time <- loadedData$"time"
    event <- loadedData$"event"

    switch(
      input$compare_validate_method,
      bootstrap = {
        cmpValObj <-
          hdnom.compare.validate(
            x, time, event,
            model.type = input$models_compare_validate,
            method = "bootstrap", boot.times = input$compare_validate_boot_times,
            tauc.type = input$compare_validate_tauc_type,
            tauc.time = seq(input$compare_tauc_from, input$compare_tauc_to, input$compare_tauc_by),
            seed = input$compare_validate_seed,
            trace = FALSE
          )
      },

      cv = {
        cmpValObj <-
          hdnom.compare.validate(
            x, time, event,
            model.type = input$models_compare_validate,
            method = "cv", nfolds = input$compare_validate_cv_nfolds,
            tauc.type = input$compare_validate_tauc_type,
            tauc.time = seq(input$compare_tauc_from, input$compare_tauc_to, input$compare_tauc_by),
            seed = input$compare_validate_seed,
            trace = FALSE
          )
      },

      repeated.cv = {
        cmpValObj <-
          hdnom.compare.validate(
            x, time, event,
            model.type = input$models_compare_validate,
            method = "repeated.cv", nfolds = input$compare_validate_rcv_nfolds, rep.times = input$compare_validate_rcv_rep_times,
            tauc.type = input$compare_validate_tauc_type,
            tauc.time = seq(input$compare_tauc_from, input$compare_tauc_to, input$compare_tauc_by),
            seed = input$compare_validate_seed,
            trace = FALSE
          )
      }
    )

    cmpValObj
  })

  calcCompareCalibrate <- eventReactive(input$calcCompareCalibrateButton, {
    loadedData <- readData()
    x <- loadedData$"x"
    time <- loadedData$"time"
    event <- loadedData$"event"

    switch(
      input$compare_calibrate_method,
      fitting = {
        cmpCalObj <-
          hdnom.compare.calibrate(
            x, time, event,
            model.type = input$models_compare_calibrate,
            method = "fitting",
            pred.at = input$compare_calibrate_pred_at,
            ngroup = input$compare_calibrate_ngroup,
            seed = input$compare_calibrate_seed,
            trace = FALSE
          )
      },

      bootstrap = {
        cmpCalObj <-
          hdnom.compare.calibrate(
            x, time, event,
            model.type = input$models_compare_calibrate,
            method = "bootstrap", boot.times = input$compare_calibrate_boot_times,
            pred.at = input$compare_calibrate_pred_at,
            ngroup = input$compare_calibrate_ngroup,
            seed = input$compare_calibrate_seed,
            trace = FALSE
          )
      },

      cv = {
        cmpCalObj <-
          hdnom.compare.calibrate(
            x, time, event,
            model.type = input$models_compare_calibrate,
            method = "cv", nfolds = input$compare_calibrate_cv_nfolds,
            pred.at = input$compare_calibrate_pred_at,
            ngroup = input$compare_calibrate_ngroup,
            seed = input$compare_calibrate_seed,
            trace = FALSE
          )
      },

      repeated.cv = {
        cmpCalObj <-
          hdnom.compare.calibrate(
            x, time, event,
            model.type = input$models_compare_calibrate,
            method = "repeated.cv", nfolds = input$compare_calibrate_rcv_nfolds, rep.times = input$compare_calibrate_rcv_rep_times,
            pred.at = input$compare_calibrate_pred_at,
            ngroup = input$compare_calibrate_ngroup,
            seed = input$compare_calibrate_seed,
            trace = FALSE
          )
      }
    )

    cmpCalObj
  })

  output$print_dataset <- renderDT({
    loadedData <- readData()
    x <- loadedData$"x"
    time <- loadedData$"time"
    event <- loadedData$"event"

    if (!is.null(x)) {
      if (ncol(x) <= 4L) {
        dataset_show <- cbind("Time" = time, "Event" = event, x)
      } else {
        dataset_show <- cbind(
          "Time" = time, "Event" = event,
          x[, 1L:2L],
          "..." = "...",
          x[, (ncol(x) - 1L):ncol(x)]
        )
      }
    } else {
      dataset_show <- NULL
    }

    dataset_show
  }, options = list(searching = FALSE, pageLength = 10))

  output$summary_dataset <- renderPrint({
    loadedData <- readData()
    x <- loadedData$"x"
    Hmisc::describe(x)
  }, width = 80L)

  output$plot_nomogram <- renderPlot({
    calcedNomogram <- calcNomogram()
    plot(calcedNomogram$"nom")
  })

  output$print_nomogram <- renderPrint({
    calcedNomogram <- calcNomogram()
    print(calcedNomogram$"nom")
  })

  output$print_model <- renderPrint({
    calcedNomogram <- calcNomogram()
    print(calcedNomogram$"object")
  })

  output$plot_validate <- renderPlot({
    calcedValidate <- calcValidate()
    plot(calcedValidate)
  })

  output$summary_validate <- renderDT({
    calcedValidate <- calcValidate()
    summaryCalcedValidate <- format(summary(calcedValidate), digits = 4L)
    cbind("-" = rownames(summaryCalcedValidate), summaryCalcedValidate)
  }, options = list(searching = FALSE, paging = FALSE))

  output$print_validate <- renderPrint({
    calcedValidate <- calcValidate()
    print(calcedValidate)
  })

  output$plot_calibrate <- renderPlot({
    calcedCalibrate <- calcCalibrate()
    plot(calcedCalibrate,
         xlim = c(input$calibrate_xlim_lo, input$calibrate_xlim_up),
         ylim = c(input$calibrate_xlim_lo, input$calibrate_xlim_up)
    )
  })

  output$summary_calibrate <- renderDT({
    calcedCalibrate <- calcCalibrate()
    summaryCalcedCalibrate <- format(summary(calcedCalibrate), digits = 4L)
    cbind("Group" = rownames(summaryCalcedCalibrate), summaryCalcedCalibrate)
  }, options = list(searching = FALSE, paging = FALSE))

  output$print_calibrate <- renderPrint({
    calcedCalibrate <- calcCalibrate()
    print(calcedCalibrate)
  })

  output$plot_calibrate_km <- renderPlot({
    calcedCalibrateKM <- calcCalibrateKM()
    plot(calcedCalibrateKM)
  })

  output$print_calibrate_logrank <- renderPrint({
    calcedCalibrateLogRank <- calcCalibrateLogRank()
    print(calcedCalibrateLogRank)
    cat("\n", " The p-value of log-rank test:\n")
    cat(" ", calcedCalibrateLogRank$pval)
  })

  output$plot_external_validate <- renderPlot({
    calcedExternalValidate <- calcExternalValidate()
    plot(calcedExternalValidate)
  })

  output$summary_external_validate <- renderDT({
    calcedExternalValidate <- calcExternalValidate()
    summaryCalcedExternalValidate <- format(summary(calcedExternalValidate), digits = 4L)
    cbind("-" = rownames(summaryCalcedExternalValidate), summaryCalcedExternalValidate)
  }, options = list(searching = FALSE, paging = FALSE))

  output$print_external_validate <- renderPrint({
    calcedExternalValidate <- calcExternalValidate()
    print(calcedExternalValidate)
  })

  output$plot_external_calibrate <- renderPlot({
    calcedExternalCalibrate <- calcExternalCalibrate()
    plot(calcedExternalCalibrate,
         xlim = c(input$external_calibrate_xlim_lo, input$external_calibrate_xlim_up),
         ylim = c(input$external_calibrate_xlim_lo, input$external_calibrate_xlim_up)
    )
  })

  output$summary_external_calibrate <- renderDT({
    calcedExternalCalibrate <- calcExternalCalibrate()
    summaryCalcedExternalCalibrate <- format(summary(calcedExternalCalibrate), digits = 4L)
    cbind("Group" = rownames(summaryCalcedExternalCalibrate), summaryCalcedExternalCalibrate)
  }, options = list(searching = FALSE, paging = FALSE))

  output$print_external_calibrate <- renderPrint({
    calcedExternalCalibrate <- calcExternalCalibrate()
    print(calcedExternalCalibrate)
  })

  output$plot_external_calibrate_km <- renderPlot({
    calcedExternalCalibrateKM <- calcExternalCalibrateKM()
    plot(calcedExternalCalibrateKM)
  })

  output$print_external_calibrate_logrank <- renderPrint({
    calcedExternalCalibrateLogRank <- calcExternalCalibrateLogRank()
    print(calcedExternalCalibrateLogRank)
    cat("\n", " The p-value of log-rank test:\n")
    cat(" ", calcedExternalCalibrateLogRank$pval)
  })

  output$plot_compare_validate <- renderPlot({
    calcedCompareValidate <- calcCompareValidate()
    plot(calcedCompareValidate, interval = input$compare_validate_interval)
  })

  output$summary_compare_validate <- renderPrint({
    calcedCompareValidate <- calcCompareValidate()
    summary(calcedCompareValidate)
  })

  output$print_compare_validate <- renderPrint({
    calcedCompareValidate <- calcCompareValidate()
    print(calcedCompareValidate)
  })

  output$plot_compare_calibrate <- renderPlot({
    calcedCompareCalibrate <- calcCompareCalibrate()
    plot(calcedCompareCalibrate,
         xlim = c(input$compare_calibrate_xlim_lo, input$compare_calibrate_xlim_up),
         ylim = c(input$compare_calibrate_xlim_lo, input$compare_calibrate_xlim_up)
    )
  })

  output$summary_compare_calibrate <- renderPrint({
    calcedCompareCalibrate <- calcCompareCalibrate()
    summary(calcedCompareCalibrate)
  })

  output$print_compare_calibrate <- renderPrint({
    calcedCompareCalibrate <- calcCompareCalibrate()
    print(calcedCompareCalibrate)
  })

  output$prediction_controls <- renderUI({
    loadedData <- readData()
    x <- loadedData$"x"
    calcedNomogram <- calcNomogram()
    modelObject <- calcedNomogram$"object"

    varinfo <- hdnom.varinfo(modelObject, x)

    var_ui_gen <- function(varinfo) {
      nvar <- length(varinfo[["name"]])
      ui_list <- vector("list", nvar)

      for (i in 1L:nvar) {
        if (varinfo[["type"]][i] == "logical") {
          choices_list <- list(
            "name1" = as.character(varinfo[["domain"]][[i]][1L]),
            "name2" = as.character(varinfo[["domain"]][[i]][2L])
          )
          choices_list <- setNames(
            choices_list,
            c(
              as.character(varinfo[["domain"]][[i]][1L]),
              as.character(varinfo[["domain"]][[i]][2L])
            )
          )
          ui_list[[i]] <- selectInput(paste0("pred_var_", varinfo[["name"]][i]),
                                      label = paste0(
                                        varinfo[["name"]][i], " (",
                                        as.character(varinfo[["domain"]][[i]][1L]),
                                        ", ", as.character(varinfo[["domain"]][[i]][2L]), ")"
                                      ),
                                      choices = choices_list
          )
        }

        if (varinfo[["type"]][i] == "categorical") {
          ui_list[[i]] <- numericInput(paste0("pred_var_", varinfo[["name"]][i]),
                                       label = paste0(
                                         varinfo[["name"]][i], " (",
                                         as.character(varinfo[["domain"]][[i]][1L]),
                                         " ~ ", as.character(varinfo[["domain"]][[i]][2L]), ")"
                                       ),
                                       min = varinfo[["domain"]][[i]][1L],
                                       max = varinfo[["domain"]][[i]][2L],
                                       value = varinfo[["domain"]][[i]][1L]
          )
        }

        if (varinfo[["type"]][i] == "continuous") {
          ui_list[[i]] <- numericInput(paste0("pred_var_", varinfo[["name"]][i]),
                                       label = paste0(
                                         varinfo[["name"]][i], " (",
                                         as.character(varinfo[["domain"]][[i]][1L]),
                                         " ~ ", as.character(varinfo[["domain"]][[i]][2L]), ")"
                                       ),
                                       min = varinfo[["domain"]][[i]][1L],
                                       max = varinfo[["domain"]][[i]][2L],
                                       value = varinfo[["domain"]][[i]][1L]
          )
        }
      }

      ui_list
    }

    var_ui_list <- var_ui_gen(varinfo)
    var_ui_list
  })

  calcPrediction <- eventReactive(input$calcPredictionButton, {
    loadedData <- readData()
    x <- loadedData$"x"
    time <- loadedData$"time"
    event <- loadedData$"event"
    y <- Surv(time, event)

    calcedNomogram <- calcNomogram()
    modelObject <- calcedNomogram$"object"

    varinfo <- hdnom.varinfo(modelObject, x)

    newx <- matrix(0, nrow = 1L, ncol = ncol(x))
    colnames(newx) <- colnames(x)
    for (i in varinfo[["name"]]) newx[1L, i] <- as.numeric(input[[paste0("pred_var_", i)]])

    predProb <- format(predict(modelObject, x, y, newx, input$prediction_pred_at)[1L, 1L], digits = 4L)
    names(predProb) <- NULL

    predProb
  })

  output$print_prediction <- renderText({
    calcedPrediction <- calcPrediction()
    calcedPrediction
  })

  output$download_report_basic <- downloadHandler(
    filename = function() {
      paste("hdnom-report-basic",
            sep = ".",
            switch(input$format_basic, PDF = "pdf", HTML = "html", Word = "docx")
      )
    },

    content = function(file) {
      src <- normalizePath("template/template-basic.Rmd")
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "template-basic.Rmd")
      out <- render(
        "template-basic.Rmd",
        switch(
          input$format_basic,
          PDF = pdf_document(toc = TRUE, number_sections = TRUE, template = NULL),
          HTML = html_document(toc = TRUE, number_sections = TRUE),
          Word = word_document(fig_width = 8, fig_height = 8)
        )
      )
      file.rename(out, file)
    }
  )

  output$download_report_external <- downloadHandler(
    filename = function() {
      paste("hdnom-report-external",
            sep = ".",
            switch(input$format_external, PDF = "pdf", HTML = "html", Word = "docx")
      )
    },

    content = function(file) {
      src <- normalizePath("template/template-external.Rmd")
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "template-external.Rmd")
      out <- render(
        "template-external.Rmd",
        switch(
          input$format_external,
          PDF = pdf_document(toc = TRUE, number_sections = TRUE, template = NULL),
          HTML = html_document(toc = TRUE, number_sections = TRUE),
          Word = word_document(fig_width = 8, fig_height = 8)
        )
      )
      file.rename(out, file)
    }
  )

  output$download_report_compare <- downloadHandler(
    filename = function() {
      paste("hdnom-report-compare",
            sep = ".",
            switch(input$format_compare, PDF = "pdf", HTML = "html", Word = "docx")
      )
    },

    content = function(file) {
      src <- normalizePath("template/template-compare.Rmd")
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "template-compare.Rmd")
      out <- render(
        "template-compare.Rmd",
        switch(
          input$format_compare,
          PDF = pdf_document(toc = TRUE, number_sections = TRUE, template = NULL),
          HTML = html_document(toc = TRUE, number_sections = TRUE),
          Word = word_document(fig_width = 8, fig_height = 8)
        )
      )
      file.rename(out, file)
    }
  )

  output$download_model_rdata <- downloadHandler(
    filename = "hdnom-model.Rdata",
    content = function(file) {
      calcedNomogram <- calcNomogram()
      hdnom_model <- calcedNomogram$"object"
      save(hdnom_model, file = file)
    }
  )
})
