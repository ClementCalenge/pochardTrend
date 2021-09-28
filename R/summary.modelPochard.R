summary.modelPochard <-
function(object, ...)
{
    if (!inherits(object, "modelPochard")) {
        stop("object should inherit the class modelPochard")
    }

    cat("\n******\n** Object of class \'modelPochard\'\n\n")

    ## table of fixed coefficients:
    dff <- data.frame(Name=c("beta0","latitude_effect"),
                      Parameter=c(object$fixed$beta0,
                                  object$fixed$latitude_effect),
                      SE=c(object$se.fixed$beta0,
                           object$se.fixed$latitude_effect))
    row.names(dff) <- 1:2


    ## log(SE)
    dfrls <- data.frame(Name=c("log_sigmabqi", "log_sigmadi", "log_sigmae"),
                        ParamLogScale=c(object$fixed$log_sigmabqi,
                                        object$fixed$log_sigmadi,
                                        object$fixed$log_sigmae),
                        SE=c(object$se.fixed$log_sigmabqi,
                             object$se.fixed$log_sigmadi,
                             object$se.fixed$log_sigmae),
                        ParamLinScale=exp(c(object$fixed$log_sigmabqi,
                                            object$fixed$log_sigmadi,
                                            object$fixed$log_sigmae)))
    row.names(dfrls) <- 1:3

    ## Show estimates
    cat("\n*** Fixed parameters:\n\n")
    print(dff)

    cat("\n*** Standard deviation random effects:\n\n")
    print(dfrls)

    ## A few info on the random effects
    di <- quantile(object$random$di$effect)
    bqi <- quantile(object$random$bqi$effect)
    eit <- quantile(object$random$eit)

    cat("\n\n*** Quantiles of the distribution of the random effects:\n\n")
    cn <- rbind(di,bqi, eit)
    colnames(cn) <- c("Min","25%","Median","75%", "Max")
    print(cn)

    cat("\n\nThis object is a list. Results returned by the function sdreport()\n",
        "of the package TMB are available in the component $rawTMB\n")
}
