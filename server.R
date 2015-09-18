library('shiny')
library('survival')
library('Hmisc')
library('rms')
library('hdnom')
library('rmarkdown')
library('knitr')

shinyServer(function(input, output, session) {

  observe({
    if (input$learnmore != 0L) {
      updateTabsetPanel(session, "mainnavbar", selected = "Help")
    }
  })

  readData = reactive({

    if (input$data_type == 'upload' & (!is.null(input$filex) & !is.null(input$filey))) {

      switch (input$sep_type,
              comma = {
                x = read.table(file = input$filex$datapath, header = TRUE, sep = ',', as.is = TRUE)
                y = read.table(file = input$filey$datapath, header = TRUE, sep = ',', as.is = TRUE)
              },
              tab = {
                x = read.table(file = input$filex$datapath, header = TRUE, sep = '\t', as.is = TRUE)
                y = read.table(file = input$filey$datapath, header = TRUE, sep = '\t', as.is = TRUE)
              },
              semi = {
                x = read.table(file = input$filex$datapath, header = TRUE, sep = ';', as.is = TRUE)
                y = read.table(file = input$filey$datapath, header = TRUE, sep = ';', as.is = TRUE)
              }
      )

      x = as.matrix(x)
      time  = y[, 1L]
      event = y[, 2L]
      y = Surv(time, event)

    } else if (input$data_type == 'example') {

      x = readRDS('data/x.rds')
      y = readRDS('data/y.rds')
      time = readRDS('data/time.rds')
      event = readRDS('data/event.rds')

    } else {
      x = NULL
      y = NULL
      time = NULL
      event = NULL
    }

    return(list('x' = x, 'y' = y, 'time' = time, 'event' = event))

  })

  calcNomogram = eventReactive(input$calcNomogramButton, {

    loadedData = readData()
    x = loadedData$'x'
    y = loadedData$'y'
    time = loadedData$'time'
    event = loadedData$'event'

    x.df = as.data.frame(x)
    dd <<- datadist(x.df)  # TODO: randomfy this variable name
    options(datadists = 'dd')

    if (input$model_type %in% c('flasso', 'mcp', 'mnet', 'scad', 'snet') &
        nrow(x) >= 601L) {
      return(stop("We're sorry. Considering long computation time, fused lasso, SCAD, Snet,
                  MCP, and Mnet models now only support data with <= 600 samples.
                  Please try other model types or use the hdnom package instead."))
    }

    switch (input$model_type,
            lasso = {
              object = hdcox.lasso(x = x, y = y, nfolds = input$lasso_nfolds,
                                   rule = input$lasso_lambda_rule,
                                   seed = input$lasso_seed)

              nom = hdnom.nomogram(object$lasso_model, model.type = 'lasso',
                                   x, time, event, ddist = x.df,
                                   lambda = object$lasso_best_lambda,
                                   pred.at = input$lasso_pred_at,
                                   funlabel = 'Predicted OS Prob.')
            },
            alasso = {
              object = hdcox.alasso(x = x, y = y, nfolds = input$alasso_nfolds,
                                    rule = input$alasso_lambda_rule,
                                    seeds = c(input$alasso_seed1, input$alasso_seed2))

              nom = hdnom.nomogram(object$alasso_model, model.type = 'alasso',
                                   x, time, event, ddist = x.df,
                                   lambda = object$alasso_best_lambda,
                                   pred.at = input$alasso_pred_at,
                                   funlabel = 'Predicted OS Prob.')
            },
            enet = {
              object = hdcox.enet(x = x, y = y, nfolds = input$enet_nfolds,
                                  alphas = input$enet_alpha,
                                  rule = input$enet_lambda_rule,
                                  seed = input$enet_seed)

              nom = hdnom.nomogram(object$enet_model, model.type = 'enet',
                                   x, time, event, ddist = x.df,
                                   lambda = object$enet_best_lambda,
                                   pred.at = input$enet_pred_at,
                                   funlabel = 'Predicted OS Prob.')
            },
            aenet = {
              object = hdcox.aenet(x = x, y = y, nfolds = input$aenet_nfolds,
                                   alphas = c(input$aenet_alpha1, input$aenet_alpha2),
                                   rule = input$aenet_lambda_rule,
                                   seeds = c(input$aenet_seed1, input$aenet_seed2))

              nom = hdnom.nomogram(object$aenet_model, model.type = 'aenet',
                                   x, time, event, ddist = x.df,
                                   lambda = object$aenet_best_lambda,
                                   pred.at = input$aenet_pred_at,
                                   funlabel = 'Predicted OS Prob.')
            },
            flasso = {
              object = hdcox.flasso(x = x, y = y, nfolds = input$flasso_nfolds,
                                    seed = input$flasso_seed)

              nom = hdnom.nomogram(object$flasso_model, model.type = 'flasso',
                                   x, time, event, ddist = x.df,
                                   lambda = object$flasso_best_lambda,
                                   pred.at = input$flasso_pred_at,
                                   funlabel = 'Predicted OS Prob.')
            },
            mcp = {
              object = hdcox.mcp(x = x, y = y, nfolds = input$mcp_nfolds,
                                 gammas = input$mcp_gamma,
                                 seed = input$mcp_seed)

              nom = hdnom.nomogram(object$mcp_model, model.type = 'mcp',
                                   x, time, event, ddist = x.df,
                                   lambda = object$mcp_best_lambda,
                                   pred.at = input$mcp_pred_at,
                                   funlabel = 'Predicted OS Prob.')
            },
            mnet = {
              object = hdcox.mnet(x = x, y = y, nfolds = input$mnet_nfolds,
                                  gammas = input$mnet_gamma,
                                  alphas = input$mnet_alpha,
                                  seed = input$mnet_seed)

              nom = hdnom.nomogram(object$mnet_model, model.type = 'mnet',
                                   x, time, event, ddist = x.df,
                                   lambda = object$mnet_best_lambda,
                                   pred.at = input$mnet_pred_at,
                                   funlabel = 'Predicted OS Prob.')
            },
            scad = {
              object = hdcox.scad(x = x, y = y, nfolds = input$scad_nfolds,
                                  gammas = input$scad_gamma,
                                  seed = input$scad_seed)

              nom = hdnom.nomogram(object$scad_model, model.type = 'scad',
                                   x, time, event, ddist = x.df,
                                   lambda = object$scad_best_lambda,
                                   pred.at = input$scad_pred_at,
                                   funlabel = 'Predicted OS Prob.')
            },
            snet = {
              object = hdcox.snet(x = x, y = y, nfolds = input$snet_nfolds,
                                  gammas = input$snet_gamma,
                                  alphas = input$snet_alpha,
                                  seed = input$snet_seed)

              nom = hdnom.nomogram(object$snet_model, model.type = 'snet',
                                   x, time, event, ddist = x.df,
                                   lambda = object$snet_best_lambda,
                                   pred.at = input$snet_pred_at,
                                   funlabel = 'Predicted OS Prob.')
            }
    )

    list('object' = object, 'nom' = nom)

    })

  calcValidate = eventReactive(input$calcValidateButton, {

    loadedData = readData()
    x = loadedData$'x'
    time = loadedData$'time'
    event = loadedData$'event'

    calcedNomogram = calcNomogram()
    modelObj = calcedNomogram$'object'

    switch (input$validate_method,
            bootstrap = {

              valObj = switch (input$model_type,
                               lasso = {
                                 hdnom.validate(x, time, event, model.type = 'lasso',
                                                alpha = 1, lambda = modelObj$'lasso_best_lambda',
                                                method = 'bootstrap', boot.times = input$validate_boot_times,
                                                tauc.type = input$validate_tauc_type,
                                                tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                                trace = FALSE)
                               },
                               alasso = {
                                 hdnom.validate(x, time, event, model.type = 'alasso',
                                                alpha = 1, lambda = modelObj$'alasso_best_lambda',
                                                pen.factor = modelObj$'pen_factor',
                                                method = 'bootstrap', boot.times = input$validate_boot_times,
                                                tauc.type = input$validate_tauc_type,
                                                tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                                trace = FALSE)
                               },
                               enet = {
                                 hdnom.validate(x, time, event, model.type = 'enet',
                                                alpha = input$enet_alpha, lambda = modelObj$'enet_best_lambda',
                                                method = 'bootstrap', boot.times = input$validate_boot_times,
                                                tauc.type = input$validate_tauc_type,
                                                tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                                trace = FALSE)
                               },
                               aenet = {
                                 hdnom.validate(x, time, event, model.type = 'aenet',
                                                alpha = modelObj$'aenet_best_alpha',
                                                lambda = modelObj$'aenet_best_lambda',
                                                pen.factor = modelObj$'pen_factor',
                                                method = 'bootstrap', boot.times = input$validate_boot_times,
                                                tauc.type = input$validate_tauc_type,
                                                tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                                trace = FALSE)
                               },
                               flasso = {
                                 hdnom.validate(x, time, event, model.type = 'flasso',
                                                lambda = modelObj$'flasso_best_lambda',
                                                method = 'bootstrap', boot.times = input$validate_boot_times,
                                                tauc.type = input$validate_tauc_type,
                                                tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                                trace = FALSE)
                               },
                               mcp = {
                                 hdnom.validate(x, time, event, model.type = 'mcp', alpha = 1,
                                                gamma = input$mcp_gamma, lambda = modelObj$'mcp_best_lambda',
                                                method = 'bootstrap', boot.times = input$validate_boot_times,
                                                tauc.type = input$validate_tauc_type,
                                                tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                                trace = FALSE)
                               },
                               mnet = {
                                 hdnom.validate(x, time, event, model.type = 'mnet', alpha = input$mnet_alpha,
                                                gamma = input$mnet_gamma, lambda = modelObj$'mnet_best_lambda',
                                                method = 'bootstrap', boot.times = input$validate_boot_times,
                                                tauc.type = input$validate_tauc_type,
                                                tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                                trace = FALSE)
                               },
                               scad = {
                                 hdnom.validate(x, time, event, model.type = 'scad', alpha = 1,
                                                gamma = input$scad_gamma, lambda = modelObj$'scad_best_lambda',
                                                method = 'bootstrap', boot.times = input$validate_boot_times,
                                                tauc.type = input$validate_tauc_type,
                                                tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                                trace = FALSE)
                               },
                               snet = {
                                 hdnom.validate(x, time, event, model.type = 'snet', alpha = input$snet_alpha,
                                                gamma = input$snet_gamma, lambda = modelObj$'snet_best_lambda',
                                                method = 'bootstrap', boot.times = input$validate_boot_times,
                                                tauc.type = input$validate_tauc_type,
                                                tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                                trace = FALSE)
                               }
              )

            },

            cv = {
              valObj =
                switch (input$model_type,
                        lasso = {
                          hdnom.validate(x, time, event, model.type = 'lasso',
                                         alpha = 1, lambda = modelObj$'lasso_best_lambda',
                                         method = 'cv', nfolds = input$validate_cv_nfolds,
                                         tauc.type = input$validate_tauc_type,
                                         tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                         trace = FALSE)
                        },
                        alasso = {
                          hdnom.validate(x, time, event, model.type = 'alasso',
                                         alpha = 1, lambda = modelObj$'alasso_best_lambda',
                                         pen.factor = modelObj$'pen_factor',
                                         method = 'cv', nfolds = input$validate_cv_nfolds,
                                         tauc.type = input$validate_tauc_type,
                                         tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                         trace = FALSE)
                        },
                        enet = {
                          hdnom.validate(x, time, event, model.type = 'enet',
                                         alpha = input$enet_alpha, lambda = modelObj$'enet_best_lambda',
                                         method = 'cv', nfolds = input$validate_cv_nfolds,
                                         tauc.type = input$validate_tauc_type,
                                         tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                         trace = FALSE)
                        },
                        aenet = {
                          hdnom.validate(x, time, event, model.type = 'aenet',
                                         alpha = modelObj$'aenet_best_alpha',
                                         lambda = modelObj$'aenet_best_lambda',
                                         pen.factor = modelObj$'pen_factor',
                                         method = 'cv', nfolds = input$validate_cv_nfolds,
                                         tauc.type = input$validate_tauc_type,
                                         tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                         trace = FALSE)
                        },
                        flasso = {
                          hdnom.validate(x, time, event, model.type = 'flasso',
                                         lambda = modelObj$'flasso_best_lambda',
                                         method = 'cv', nfolds = input$validate_cv_nfolds,
                                         tauc.type = input$validate_tauc_type,
                                         tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                         trace = FALSE)
                        },
                        mcp = {
                          hdnom.validate(x, time, event, model.type = 'mcp', alpha = 1,
                                         gamma = input$mcp_gamma, lambda = modelObj$'mcp_best_lambda',
                                         method = 'cv', nfolds = input$validate_cv_nfolds,
                                         tauc.type = input$validate_tauc_type,
                                         tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                         trace = FALSE)
                        },
                        mnet = {
                          hdnom.validate(x, time, event, model.type = 'mnet', alpha = input$mnet_alpha,
                                         gamma = input$mnet_gamma, lambda = modelObj$'mnet_best_lambda',
                                         method = 'cv', nfolds = input$validate_cv_nfolds,
                                         tauc.type = input$validate_tauc_type,
                                         tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                         trace = FALSE)
                        },
                        scad = {
                          hdnom.validate(x, time, event, model.type = 'scad', alpha = 1,
                                         gamma = input$scad_gamma, lambda = modelObj$'scad_best_lambda',
                                         method = 'cv', nfolds = input$validate_cv_nfolds,
                                         tauc.type = input$validate_tauc_type,
                                         tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                         trace = FALSE)
                        },
                        snet = {
                          hdnom.validate(x, time, event, model.type = 'snet', alpha = input$snet_alpha,
                                         gamma = input$snet_gamma, lambda = modelObj$'snet_best_lambda',
                                         method = 'cv', nfolds = input$validate_cv_nfolds,
                                         tauc.type = input$validate_tauc_type,
                                         tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                         trace = FALSE)
                        }
                )

            },

            repeated.cv = {
              valObj =
                switch (input$model_type,
                        lasso = {
                          hdnom.validate(x, time, event, model.type = 'lasso',
                                         alpha = 1, lambda = modelObj$'lasso_best_lambda',
                                         method = 'repeated.cv', nfolds = input$validate_rcv_nfolds, rep.times = input$validate_rcv_rep_times,
                                         tauc.type = input$validate_tauc_type,
                                         tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                         trace = FALSE)
                        },
                        alasso = {
                          hdnom.validate(x, time, event, model.type = 'alasso',
                                         alpha = 1, lambda = modelObj$'alasso_best_lambda',
                                         pen.factor = modelObj$'pen_factor',
                                         method = 'repeated.cv', nfolds = input$validate_rcv_nfolds, rep.times = input$validate_rcv_rep_times,
                                         tauc.type = input$validate_tauc_type,
                                         tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                         trace = FALSE)
                        },
                        enet = {
                          hdnom.validate(x, time, event, model.type = 'enet',
                                         alpha = input$enet_alpha, lambda = modelObj$'enet_best_lambda',
                                         method = 'repeated.cv', nfolds = input$validate_rcv_nfolds, rep.times = input$validate_rcv_rep_times,
                                         tauc.type = input$validate_tauc_type,
                                         tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                         trace = FALSE)
                        },
                        aenet = {
                          hdnom.validate(x, time, event, model.type = 'aenet',
                                         alpha = modelObj$'aenet_best_alpha',
                                         lambda = modelObj$'aenet_best_lambda',
                                         pen.factor = modelObj$'pen_factor',
                                         method = 'repeated.cv', nfolds = input$validate_rcv_nfolds, rep.times = input$validate_rcv_rep_times,
                                         tauc.type = input$validate_tauc_type,
                                         tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                         trace = FALSE)
                        },
                        flasso = {
                          hdnom.validate(x, time, event, model.type = 'flasso',
                                         lambda = modelObj$'flasso_best_lambda',
                                         method = 'repeated.cv', nfolds = input$validate_rcv_nfolds, rep.times = input$validate_rcv_rep_times,
                                         tauc.type = input$validate_tauc_type,
                                         tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                         trace = FALSE)
                        },
                        mcp = {
                          hdnom.validate(x, time, event, model.type = 'mcp', alpha = 1,
                                         gamma = input$mcp_gamma, lambda = modelObj$'mcp_best_lambda',
                                         method = 'repeated.cv', nfolds = input$validate_rcv_nfolds, rep.times = input$validate_rcv_rep_times,
                                         tauc.type = input$validate_tauc_type,
                                         tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                         trace = FALSE)
                        },
                        mnet = {
                          hdnom.validate(x, time, event, model.type = 'mnet', alpha = input$mnet_alpha,
                                         gamma = input$mnet_gamma, lambda = modelObj$'mnet_best_lambda',
                                         method = 'repeated.cv', nfolds = input$validate_rcv_nfolds, rep.times = input$validate_rcv_rep_times,
                                         tauc.type = input$validate_tauc_type,
                                         tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                         trace = FALSE)
                        },
                        scad = {
                          hdnom.validate(x, time, event, model.type = 'scad', alpha = 1,
                                         gamma = input$scad_gamma, lambda = modelObj$'scad_best_lambda',
                                         method = 'repeated.cv', nfolds = input$validate_rcv_nfolds, rep.times = input$validate_rcv_rep_times,
                                         tauc.type = input$validate_tauc_type,
                                         tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                         trace = FALSE)
                        },
                        snet = {
                          hdnom.validate(x, time, event, model.type = 'snet', alpha = input$snet_alpha,
                                         gamma = input$snet_gamma, lambda = modelObj$'snet_best_lambda',
                                         method = 'repeated.cv', nfolds = input$validate_rcv_nfolds, rep.times = input$validate_rcv_rep_times,
                                         tauc.type = input$validate_tauc_type,
                                         tauc.time = seq(input$tauc_from, input$tauc_to, input$tauc_by),
                                         trace = FALSE)
                        }
                )

            }

    )

    valObj

  })

  calcCalibrate = eventReactive(input$calcCalibrateButton, {

    loadedData = readData()
    x = loadedData$'x'
    time = loadedData$'time'
    event = loadedData$'event'

    calcedNomogram = calcNomogram()
    modelObj = calcedNomogram$'object'

    switch (input$calibrate_method,

            fitting = {

              calObj =
                switch (input$model_type,
                        lasso = {
                          hdnom.calibrate(x, time, event, model.type = 'lasso',
                                          alpha = 1, lambda = modelObj$'lasso_best_lambda',
                                          method = 'fitting',
                                          pred.at = input$lasso_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        alasso = {
                          hdnom.calibrate(x, time, event, model.type = 'alasso',
                                          alpha = 1, lambda = modelObj$'alasso_best_lambda',
                                          pen.factor = modelObj$'pen_factor',
                                          method = 'fitting',
                                          pred.at = input$alasso_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        enet = {
                          hdnom.calibrate(x, time, event, model.type = 'enet',
                                          alpha = input$enet_alpha, lambda = modelObj$'enet_best_lambda',
                                          method = 'fitting',
                                          pred.at = input$enet_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        aenet = {
                          hdnom.calibrate(x, time, event, model.type = 'aenet',
                                          alpha = modelObj$'aenet_best_alpha',
                                          lambda = modelObj$'aenet_best_lambda',
                                          pen.factor = modelObj$'pen_factor',
                                          method = 'fitting',
                                          pred.at = input$aenet_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        flasso = {
                          hdnom.calibrate(x, time, event, model.type = 'flasso',
                                          lambda = modelObj$'flasso_best_lambda',
                                          method = 'fitting',
                                          pred.at = input$flasso_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        mcp = {
                          hdnom.calibrate(x, time, event, model.type = 'mcp', alpha = 1,
                                          gamma = input$mcp_gamma, lambda = modelObj$'mcp_best_lambda',
                                          method = 'fitting',
                                          pred.at = input$mcp_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        mnet = {
                          hdnom.calibrate(x, time, event, model.type = 'mnet', alpha = input$mnet_alpha,
                                          gamma = input$mnet_gamma, lambda = modelObj$'mnet_best_lambda',
                                          method = 'fitting',
                                          pred.at = input$mnet_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        scad = {
                          hdnom.calibrate(x, time, event, model.type = 'scad', alpha = 1,
                                          gamma = input$scad_gamma, lambda = modelObj$'scad_best_lambda',
                                          method = 'fitting',
                                          pred.at = input$scad_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        snet = {
                          hdnom.calibrate(x, time, event, model.type = 'snet', alpha = input$snet_alpha,
                                          gamma = input$snet_gamma, lambda = modelObj$'snet_best_lambda',
                                          method = 'fitting',
                                          pred.at = input$snet_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        }
                )

            },

            bootstrap = {

              calObj =
                switch (input$model_type,
                        lasso = {
                          hdnom.calibrate(x, time, event, model.type = 'lasso',
                                          alpha = 1, lambda = modelObj$'lasso_best_lambda',
                                          method = 'bootstrap', boot.times = input$calibrate_boot_times,
                                          pred.at = input$lasso_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        alasso = {
                          hdnom.calibrate(x, time, event, model.type = 'alasso',
                                          alpha = 1, lambda = modelObj$'alasso_best_lambda',
                                          pen.factor = modelObj$'pen_factor',
                                          method = 'bootstrap', boot.times = input$calibrate_boot_times,
                                          pred.at = input$alasso_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        enet = {
                          hdnom.calibrate(x, time, event, model.type = 'enet',
                                          alpha = input$enet_alpha, lambda = modelObj$'enet_best_lambda',
                                          method = 'bootstrap', boot.times = input$calibrate_boot_times,
                                          pred.at = input$enet_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        aenet = {
                          hdnom.calibrate(x, time, event, model.type = 'aenet',
                                          alpha = modelObj$'aenet_best_alpha',
                                          lambda = modelObj$'aenet_best_lambda',
                                          pen.factor = modelObj$'pen_factor',
                                          method = 'bootstrap', boot.times = input$calibrate_boot_times,
                                          pred.at = input$aenet_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        flasso = {
                          hdnom.calibrate(x, time, event, model.type = 'flasso',
                                          lambda = modelObj$'flasso_best_lambda',
                                          method = 'bootstrap', boot.times = input$calibrate_boot_times,
                                          pred.at = input$flasso_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        mcp = {
                          hdnom.calibrate(x, time, event, model.type = 'mcp', alpha = 1,
                                          gamma = input$mcp_gamma, lambda = modelObj$'mcp_best_lambda',
                                          method = 'bootstrap', boot.times = input$calibrate_boot_times,
                                          pred.at = input$mcp_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        mnet = {
                          hdnom.calibrate(x, time, event, model.type = 'mnet', alpha = input$mnet_alpha,
                                          gamma = input$mnet_gamma, lambda = modelObj$'mnet_best_lambda',
                                          method = 'bootstrap', boot.times = input$calibrate_boot_times,
                                          pred.at = input$mnet_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        scad = {
                          hdnom.calibrate(x, time, event, model.type = 'scad', alpha = 1,
                                          gamma = input$scad_gamma, lambda = modelObj$'scad_best_lambda',
                                          method = 'bootstrap', boot.times = input$calibrate_boot_times,
                                          pred.at = input$scad_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        snet = {
                          hdnom.calibrate(x, time, event, model.type = 'snet', alpha = input$snet_alpha,
                                          gamma = input$snet_gamma, lambda = modelObj$'snet_best_lambda',
                                          method = 'bootstrap', boot.times = input$calibrate_boot_times,
                                          pred.at = input$snet_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        }
                )

            },

            cv = {
              calObj =
                switch (input$model_type,
                        lasso = {
                          hdnom.calibrate(x, time, event, model.type = 'lasso',
                                          alpha = 1, lambda = modelObj$'lasso_best_lambda',
                                          method = 'cv', nfolds = input$calibrate_cv_nfolds,
                                          pred.at = input$lasso_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        alasso = {
                          hdnom.calibrate(x, time, event, model.type = 'alasso',
                                          alpha = 1, lambda = modelObj$'alasso_best_lambda',
                                          pen.factor = modelObj$'pen_factor',
                                          method = 'cv', nfolds = input$calibrate_cv_nfolds,
                                          pred.at = input$alasso_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        enet = {
                          hdnom.calibrate(x, time, event, model.type = 'enet',
                                          alpha = input$enet_alpha, lambda = modelObj$'enet_best_lambda',
                                          method = 'cv', nfolds = input$calibrate_cv_nfolds,
                                          pred.at = input$enet_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        aenet = {
                          hdnom.calibrate(x, time, event, model.type = 'aenet',
                                          alpha = modelObj$'aenet_best_alpha',
                                          lambda = modelObj$'aenet_best_lambda',
                                          pen.factor = modelObj$'pen_factor',
                                          method = 'cv', nfolds = input$calibrate_cv_nfolds,
                                          pred.at = input$aenet_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        flasso = {
                          hdnom.calibrate(x, time, event, model.type = 'flasso',
                                          lambda = modelObj$'flasso_best_lambda',
                                          method = 'cv', nfolds = input$calibrate_cv_nfolds,
                                          pred.at = input$flasso_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        mcp = {
                          hdnom.calibrate(x, time, event, model.type = 'mcp', alpha = 1,
                                          gamma = input$mcp_gamma, lambda = modelObj$'mcp_best_lambda',
                                          method = 'cv', nfolds = input$calibrate_cv_nfolds,
                                          pred.at = input$mcp_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        mnet = {
                          hdnom.calibrate(x, time, event, model.type = 'mnet', alpha = input$mnet_alpha,
                                          gamma = input$mnet_gamma, lambda = modelObj$'mnet_best_lambda',
                                          method = 'cv', nfolds = input$calibrate_cv_nfolds,
                                          pred.at = input$mnet_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        scad = {
                          hdnom.calibrate(x, time, event, model.type = 'scad', alpha = 1,
                                          gamma = input$scad_gamma, lambda = modelObj$'scad_best_lambda',
                                          method = 'cv', nfolds = input$calibrate_cv_nfolds,
                                          pred.at = input$scad_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        snet = {
                          hdnom.calibrate(x, time, event, model.type = 'snet', alpha = input$snet_alpha,
                                          gamma = input$snet_gamma, lambda = modelObj$'snet_best_lambda',
                                          method = 'cv', nfolds = input$calibrate_cv_nfolds,
                                          pred.at = input$snet_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        }
                )

            },

            repeated.cv = {
              calObj =
                switch (input$model_type,
                        lasso = {
                          hdnom.calibrate(x, time, event, model.type = 'lasso',
                                          alpha = 1, lambda = modelObj$'lasso_best_lambda',
                                          method = 'repeated.cv', nfolds = input$calibrate_rcv_nfolds, rep.times = input$calibrate_rcv_rep_times,
                                          pred.at = input$lasso_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        alasso = {
                          hdnom.calibrate(x, time, event, model.type = 'alasso',
                                          alpha = 1, lambda = modelObj$'alasso_best_lambda',
                                          pen.factor = modelObj$'pen_factor',
                                          method = 'repeated.cv', nfolds = input$calibrate_rcv_nfolds, rep.times = input$calibrate_rcv_rep_times,
                                          pred.at = input$alasso_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        enet = {
                          hdnom.calibrate(x, time, event, model.type = 'enet',
                                          alpha = input$enet_alpha, lambda = modelObj$'enet_best_lambda',
                                          method = 'repeated.cv', nfolds = input$calibrate_rcv_nfolds, rep.times = input$calibrate_rcv_rep_times,
                                          pred.at = input$enet_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        aenet = {
                          hdnom.calibrate(x, time, event, model.type = 'aenet',
                                          alpha = modelObj$'aenet_best_alpha',
                                          lambda = modelObj$'aenet_best_lambda',
                                          pen.factor = modelObj$'pen_factor',
                                          method = 'repeated.cv', nfolds = input$calibrate_rcv_nfolds, rep.times = input$calibrate_rcv_rep_times,
                                          pred.at = input$aenet_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        flasso = {
                          hdnom.calibrate(x, time, event, model.type = 'flasso',
                                          lambda = modelObj$'flasso_best_lambda',
                                          method = 'repeated.cv', nfolds = input$calibrate_rcv_nfolds, rep.times = input$calibrate_rcv_rep_times,
                                          pred.at = input$flasso_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        mcp = {
                          hdnom.calibrate(x, time, event, model.type = 'mcp', alpha = 1,
                                          gamma = input$mcp_gamma, lambda = modelObj$'mcp_best_lambda',
                                          method = 'repeated.cv', nfolds = input$calibrate_rcv_nfolds, rep.times = input$calibrate_rcv_rep_times,
                                          pred.at = input$mcp_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        mnet = {
                          hdnom.calibrate(x, time, event, model.type = 'mnet', alpha = input$mnet_alpha,
                                          gamma = input$mnet_gamma, lambda = modelObj$'mnet_best_lambda',
                                          method = 'repeated.cv', nfolds = input$calibrate_rcv_nfolds, rep.times = input$calibrate_rcv_rep_times,
                                          pred.at = input$mnet_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        scad = {
                          hdnom.calibrate(x, time, event, model.type = 'scad', alpha = 1,
                                          gamma = input$scad_gamma, lambda = modelObj$'scad_best_lambda',
                                          method = 'repeated.cv', nfolds = input$calibrate_rcv_nfolds, rep.times = input$calibrate_rcv_rep_times,
                                          pred.at = input$scad_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        },
                        snet = {
                          hdnom.calibrate(x, time, event, model.type = 'snet', alpha = input$snet_alpha,
                                          gamma = input$snet_gamma, lambda = modelObj$'snet_best_lambda',
                                          method = 'repeated.cv', nfolds = input$calibrate_rcv_nfolds, rep.times = input$calibrate_rcv_rep_times,
                                          pred.at = input$snet_pred_at, ngroup = input$calibrate_ngroup,
                                          trace = FALSE)
                        }
                )

            }

    )

    calObj

  })


  output$print_dataset_head = renderDataTable({

    loadedData = readData()
    x = loadedData$'x'
    time = loadedData$'time'
    event = loadedData$'event'

    dataset_head = cbind('Time' = head(time), 'Event' = head(event),
                         head(x)[, 1L:5L], '...' = '...')
    dataset_head

  }, options = list(searching = FALSE, paging = FALSE))

  output$print_dataset_tail = renderDataTable({

    loadedData = readData()
    x = loadedData$'x'
    y = loadedData$'y'
    time = loadedData$'time'
    event = loadedData$'event'

    dataset_tail =  cbind('Time' = tail(time), 'Event' = tail(event),
                          tail(x)[, 1L:4L], '...' = '...')
    dataset_tail

  }, options = list(searching = FALSE, paging = FALSE))

  output$summary_dataset = renderPrint({

    loadedData = readData()
    x = loadedData$'x'
    describe(x)

  }, width = 80L)

  output$print_nomogram = renderPrint({

    calcedNomogram = calcNomogram()
    print(calcedNomogram$'nom')

  })

  output$plot_nomogram = renderPlot({

    calcedNomogram = calcNomogram()
    plot(calcedNomogram$'nom')

  })

  output$plot_validate = renderPlot({
    calcedValidate = calcValidate()
    plot(calcedValidate)
  })

  output$summary_validate = renderDataTable({
    calcedValidate = calcValidate()
    summaryCalcedValidate = format(summary(calcedValidate), digits = 4L)
    cbind('-' = rownames(summaryCalcedValidate), summaryCalcedValidate)
  }, options = list(searching = FALSE, paging = FALSE))

  output$print_validate = renderPrint({
    calcedValidate = calcValidate()
    print(calcedValidate)
  })

  output$plot_calibrate = renderPlot({
    calcedCalibrate = calcCalibrate()
    plot(calcedCalibrate,
         xlim = c(input$calibrate_xlim_lo, input$calibrate_xlim_up),
         ylim = c(input$calibrate_xlim_lo, input$calibrate_xlim_up))
  })

  output$summary_calibrate = renderDataTable({
    calcedCalibrate = calcCalibrate()
    summaryCalcedCalibrate = format(summary(calcedCalibrate), digits = 4L)
    cbind('Group' = rownames(summaryCalcedCalibrate), summaryCalcedCalibrate)
  }, options = list(searching = FALSE, paging = FALSE))

  output$print_calibrate = renderPrint({
    calcedCalibrate = calcCalibrate()
    print(calcedCalibrate)
  })

  output$downloadReport = downloadHandler(
    filename = function() {
      paste('hdnom-report', sep = '.',
            switch(input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'))
    },

    content = function(file) {
      src = normalizePath('template.Rmd')
      owd = setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'template.Rmd')
      out = render('template.Rmd',
                   switch(input$format,
                          PDF = pdf_document(toc = TRUE, number_sections = TRUE, template = NULL),
                          HTML = html_document(toc = TRUE, number_sections = TRUE, theme = 'readable'),
                          Word = word_document(fig_width = 8, fig_height = 8)
                   ))
      file.rename(out, file)
    }
  )

  })
