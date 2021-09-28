showTrends <- function(model, year, lat, nrep=1000, levelIC=c(0.025, 0.975), addSite=TRUE, addQuadrat=TRUE)
{
    y <- year
    yc <- y-mean(y)
    fe <- model$rawTMB$par.fixed
    vc <- model$rawTMB$cov.fixed
    sco <- mvrnorm(nrep, fe, vc)

    moy <- sapply(1:length(lat), function(j) {
        exp(mean(fe[names(fe)=="site_effect"])+(fe["latitude_effect"]*lat[j]+fe["beta0"])*yc)
    })

    IC <- do.call(rbind, lapply(1:length(lat), function(j) {
                             IC <- t(exp(apply(apply(sco, 1, function(x)
                                 mean(x[names(fe)=="site_effect"])+
                                 (x[names(fe)=="beta0"]+
                                  x[names(fe)=="latitude_effect"]*lat[j])*
                                 yc),1,
                             quantile,
                             levelIC)))
                     }))

    ## quadrat Level
    slopes <- rnorm(nrep, 0, exp(model$fixed$log_sigmabqi))
    si <- do.call(rbind, lapply(1:length(lat), function(j) {
                             si <- t(apply(sapply(yc, function(x)
                                 exp(apply(sco[,colnames(sco)=="site_effect"],1,mean)+
                                     (sco[,"beta0"]+sco[,"latitude_effect"]*lat[j]+slopes)*x)),2,
                                 quantile,levelIC))
                         }) )



    ## Site level
    slopeSite <- rnorm(nrep, 0, exp(model$fixed$log_sigmadi))
    si2 <- do.call(rbind, lapply(1:length(lat), function(j) {
                              si2 <- t(apply(sapply(yc, function(x)
                                  exp(apply(sco[,colnames(sco)=="site_effect"],1,mean)+
                                      (sco[,1]+sco[,2]*lat[j]+slopeSite)*x)),2,
                                  quantile,c(0.025, 0.975)))
                          }))



    prp1 <- data.frame(year=year, mean=as.vector(moy),
                       latitude=rep(paste0("Latitude = ", lat," degrees"), each=nrow(moy)))


    prp2 <- data.frame(annee=rep(year,length(lat)*3),
                       latitude=rep(rep(paste0("Latitude = ", lat," degrees"), each=length(yc)),3),
                       minq=c(si[,1],si2[,1],IC[,1]),
                       maxq=c(si[,2], si2[,2],IC[,2]),
                       Interval=factor(c(rep("quadrats",length(yc)*length(lat)),
                                         rep("sites",length(yc)*length(lat)),
                                         rep("CI",length(yc)*length(lat))),
                                       levels=c("sites","quadrats","CI")))

    prp2$IC <- factor(prp2$Interval=="CI", levels=c("TRUE","FALSE"))

    if (!addQuadrat)
        prp2 <- prp2[prp2$Interval!="quadrats",]
    if (!addSite)
        prp2 <- prp2[prp2$Interval!="sites",]

    ## Le graphe
    ggplot2::ggplot(prp1)+ggplot2::geom_line(ggplot2::aes(x=.data$year, y=.data$mean))+
        ggplot2::facet_wrap(~.data$latitude)+
        ggplot2::geom_ribbon(data=prp2, ggplot2::aes(x=.data$annee, ymin=.data$minq,
                                   ymax=.data$maxq, fill=.data$Interval),
                    alpha=0.4)+
        ggplot2::scale_x_continuous("Year", breaks=seq(year[1],year[length(year)],by=2))+
        ggplot2::scale_y_continuous("Population size")
}
