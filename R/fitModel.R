fitModel <-
function(count, site, year, quadrat, latQuadrat,
                     sv, control=list())
{

    le <- unique(c(length(count), length(site), length(year), length(quadrat), length(latQuadrat)))
    if (length(le)!=1) {
        stop("Differing length for input vectors")
    }


    ## check counts
    if ((!is.numeric(count))|any(count<0))
        stop("count should be numeric and nonnegative")

    ## check year
    if (!is.numeric(year))
        stop("year should be numeric")


    ## check latQuadrat
    if (!is.numeric(latQuadrat))
        stop("latQuadrat should be numeric")


    ## Relabel site from 1 to n
    lev <- levels(factor(site))
    siter <- as.numeric(factor(site))


    ## Relabel site from 1 to n
    levq <- levels(factor(quadrat))
    quadratr <- as.numeric(factor(quadrat))



    ## Prepare the data for TMB
    da <- list()
    da$y <- count
    da$x <- year-mean(unique(year))
    da$QuadratOfSite <- factor(quadratr)
    da$whichquad <- factor(as.numeric(quadratr))
    da$whichsite <- factor(as.numeric(siter))
    dq <- data.frame(quad=quadratr, latQuadrat=latQuadrat)
    dq <- unique(dq[order(dq$quad),])
    latq <- dq$latQuadrat
    da$latitude <- latq


    param <- list()

    if (!missing(sv)) {
        na <- names(sv)
        if (!all(na%in%c("site_effect", "log_sigmabqi","log_sigmadi","log_sigmae")))
            stop("Non convenient starting values")
        param <- list(glou=0)
    } else {
        sv <- list(glou=0)
    }

    ## Starting values for the parameters
    if (all(names(sv)!="site_effect"))
        param$site_effect <- rep(0, max(as.numeric(da$whichsite)))
    if (all(names(sv)!="bqi"))
        param$bqi <- rep(0, max(as.numeric(da$whichquad)))
    if (all(names(sv)!="di"))
        param$di <- rep(0, max(as.numeric(da$whichsite)))
    if (all(names(sv)!="eit"))
        param$eit <- rep(0,length(count))
    if (all(names(sv)!="beta0"))
        param$beta0 <- 0
    if (all(names(sv)!="latitude_effect"))
        param$latitude_effect <- 0
    if (all(names(sv)!="log_sigmabqi"))
        param$log_sigmabqi <- log(0.04)
    if (all(names(sv)!="log_sigmadi"))
        param$log_sigmadi <- log(0.02)
    if (all(names(sv)!="log_sigmae"))
        param$log_sigmae <- log(1.3)
    param$glou <- NULL



    ## Phase 1: we fix random effects to 0 and we estimate fixed effects
    map <- list(bqi=factor(rep(NA,length(param$bqi))),
                di=factor(rep(NA,length(param$di))),
                eit=factor(rep(NA,length(param$eit))),
                log_sigmabqi=factor(NA),
                log_sigmadi=factor(NA),
                log_sigmae=factor(NA))

    ## order parameters
    param <- param[c("beta0","latitude_effect","site_effect",
                     "log_sigmabqi","log_sigmadi","log_sigmae",
                     "bqi","di","eit")]

    ## Functions to minimize
    obj <- MakeADFun(da,param,map=map,silent=TRUE,DLL="pochardTrend")

    ## Optimization with effets aléatoires fixés à 0
    opt <- optim(obj$par,obj$fn,obj$gr, method="BFGS")

    ## We get optimal values for this first step
    pl <- obj$env$parList(opt$par)

    ## Phase 2: all parameters are active, including random effects
    obj <- MakeADFun(da,pl,random=c("bqi","di","eit"),
                     silent=TRUE,DLL="pochardTrend")

    ## Main Optimization
    opt2 <- optim(obj$par, obj$fn ,
                  obj$gr, method="BFGS", control = control)

    if (opt2$convergence!=0) {
        stop(paste0("Convergence code: ", opt2$convergence, ".\n Try to change optim() control parameters"))
    }
    ## Calculation of the Hessian and estimate of standard errors on parameters
    rep <- sdreport(obj, par.fixed = opt2$par)


    ## Prepare results
    fixed <- list(beta0=rep$par.fixed[names(rep$par.fixed)=="beta0"],
                  latitude_effect=rep$par.fixed[names(rep$par.fixed)=="latitude_effect"],
                  site_effect=data.frame(site=lev, effect=rep$par.fixed[names(rep$par.fixed)=="site_effect"]),
                  log_sigmabqi=rep$par.fixed[names(rep$par.fixed)=="log_sigmabqi"],
                  log_sigmadi=rep$par.fixed[names(rep$par.fixed)=="log_sigmadi"],
                  log_sigmae=rep$par.fixed[names(rep$par.fixed)=="log_sigmae"])


    sooe <- sqrt(diag(rep$cov.fixed))
    sefixed <- list(beta0=sooe[names(rep$par.fixed)=="beta0"],
                    latitude_effect=sooe[names(rep$par.fixed)=="latitude_effect"],
                    site_effect=data.frame(site=lev, se.effect=sooe[names(rep$par.fixed)=="site_effect"]),
                    log_sigmabqi=sooe[names(rep$par.fixed)=="log_sigmabqi"],
                    log_sigmadi=sooe[names(rep$par.fixed)=="log_sigmadi"],
                    log_sigmae=sooe[names(rep$par.fixed)=="log_sigmae"])


    ree <- rep$par.random
    random <- list(di=data.frame(site=lev, effect=rep$par.random[names(ree)=="di"]),
                   bqi=data.frame(quadrat=levq, effect=rep$par.random[names(ree)=="bqi"]),
                   eit=rep$par.random[names(ree)=="eit"])


    li <- list(fixed=fixed, se.fixed=sefixed, random=random, rawTMB=rep)

    class(li) <- "modelPochard"
    return(li)

}
