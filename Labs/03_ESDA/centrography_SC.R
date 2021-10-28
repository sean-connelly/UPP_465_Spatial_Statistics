#this is the calc_sdd (circle) function from the aspace library (with a slightly modified output)
"calc_sdd2"<-
function (id = 1, filename = "SDD_Output.txt", centre.xy = NULL, 
    calccentre = TRUE, weighted = FALSE, weights = NULL, points = activities, 
    verbose = FALSE) 
{
    errorcode <- 1000
    n <- dim(points)[1]
    if (calccentre) {
        if (length(centre.xy) == 2) {
            errorcode <- 21
            cat("\n\nWARNING: Invalid combination: calccentre=TRUE and centre.xy!=NULL")
            cat("\nERROR CODE: ", errorcode, "\n\n", sep = "")
            return("ERROR")
        }
        else {
            if (weighted) {
                wt.x <- points[, 1] * weights
                wt.y <- points[, 2] * weights
                WMC.x <- c(sum(wt.x)/sum(weights))
                WMC.y <- c(sum(wt.y)/sum(weights))
                centre.xy[1] <- WMC.x
                centre.xy[2] <- WMC.y
            }
            else {
                meanx <- sum(points[, 1])/n
                meany <- sum(points[, 2])/n
                centre.xy[1] <- meanx
                centre.xy[2] <- meany
            }
        }
    }
    dist <- distances(centre.xy, points)
    if (length(dist) >= 3) {
        if (weighted) {
            SDD <- sqrt(sum((weights * dist^2)/((sum(weights)) - 
                2)))
        }
        else {
            SDD <- sqrt(sum(dist^2/(length(dist) - 2)))
        }
        sddarea <- pi * SDD^2
        B <- min(SDD, SDD)
        A <- max(SDD, SDD)
        d2 <- (A - B) * (A + B)
        phi <- 2 * pi * seq(0, 1, len = 360)
        sp <- sin(phi)
        cp <- cos(phi)
        r <- SDD * SDD/sqrt(B^2 + d2 * sp^2)
        xy <- r * cbind(cp, sp)
        al <- 0 * pi/180
        ca <- cos(al)
        sa <- sin(al)
        coordsSDD <- xy %*% rbind(c(ca, sa), c(-sa, ca)) + cbind(rep(centre.xy[1], 
            360), rep(centre.xy[2], 360))
        sddloc <- as.data.frame(cbind(id, coordsSDD))
        colnames(sddloc) = c("id", "x", "y")
        write.table(sddloc, sep = ",", file = filename, col.names = FALSE)
        assign("sddloc", sddloc, pos = 1)
        r.SDD <- list(id = id, points = points, coordsSDD = coordsSDD, 
            SDD = SDD, calccentre = calccentre, weighted = weighted, 
            weights = weights, CENTRE.x = centre.xy[1], CENTRE.y = centre.xy[2], 
            SDD.area = sddarea)
        assign("r.SDD", r.SDD, pos = 1)
         result.sdd <- 
        list(id = id, calccentre = calccentre, 
            weighted = weighted, CENTRE.x = centre.xy[1], CENTRE.y = centre.xy[2], 
            SDD.radius = SDD, SDD.area = sddarea)
         # print(result.sdd)
         result.sdd <- as.data.frame(result.sdd)
         assign("sddatt", result.sdd, pos = 1)
        return(r.SDD)
    }
    else {
        errorcode <- 25
        if (verbose) {
            cat("\n\nWARNING: Not enough values to compute SDD.")
            cat("\nERROR CODE: ", errorcode, "\n\n", sep = "")
        }
        return("ERROR")
    }
}

#this is the calc_sde (ellipse) function from the aspace library (with a slightly modified output)
"calc_sde2"<-
	function (id = 1, filename = "SDE_Output.txt", centre.xy = NULL, 
    calccentre = TRUE, weighted = FALSE, weights = NULL, points = activities, 
    verbose = FALSE) 
{
    errorcode <- 1000
    if (length(dim(points)) != 2) {
        errorcode <- 61
        cat("\n\nWARNING: Provided points input matrix has fewer than 2 columns.")
        cat("\nERROR CODE: ", errorcode, "\n\n", sep = "")
        return("ERROR")
    }
    if (dim(points)[2] != 2) {
        errorcode <- 60
        cat("\n\nWARNING: Provided points input matrix has too many columns, only 2 are allowed.")
        cat("\nERROR CODE: ", errorcode, "\n\n", sep = "")
        return("ERROR")
    }
    else {
        n <- dim(points)[1]
        if (calccentre) {
            if (length(centre.xy) == 2) {
                errorcode <- 21
                cat("\n\nWARNING: Invalid combination: calccentre=TRUE and centre.xy!=NULL")
                cat("\nERROR CODE: ", errorcode, "\n\n", sep = "")
                return("ERROR")
            }
            else {
                if (weighted) {
                  wt.x <- points[, 1] * weights
                  wt.y <- points[, 2] * weights
                  WMC.x <- c(sum(wt.x)/sum(weights))
                  WMC.y <- c(sum(wt.y)/sum(weights))
                  centre.xy[1] <- WMC.x
                  centre.xy[2] <- WMC.y
                }
                else {
                  meanx <- sum(points[, 1])/n
                  meany <- sum(points[, 2])/n
                  centre.xy[1] <- meanx
                  centre.xy[2] <- meany
                }
            }
        }
    }
    points <- cbind(points, points[, 1]^2, points[, 2]^2)
    points <- cbind(points, points[, 1] - centre.xy[1], points[, 
        2] - centre.xy[2])
    points <- cbind(points, points[, 5]^2, points[, 6]^2, points[, 
        5] * points[, 6])
    names(points) <- c("x", "y", "x2", "y2", "x'", "y'", "x'2", 
        "y'2", "x'y'")
    if (weighted) {
        top1 <- sum(weights * points[, 7]) - sum(weights * points[, 
            8])
        top2 <- sqrt((sum(weights * points[, 7]) - sum(weights * 
            points[, 8]))^2 + 4 * (sum(weights * points[, 9]))^2)
        bottom <- (2 * sum(weights * points[, 9]))
        tantheta <- (top1 + top2)/bottom
    }
    else {
        top1 <- sum(points[, 7]) - sum(points[, 8])
        top2 <- sqrt((sum(points[, 7]) - sum(points[, 8]))^2 + 
            4 * (sum(points[, 9]))^2)
        bottom <- (2 * sum(points[, 9]))
        tantheta <- (top1 + top2)/bottom
    }
    if (tantheta < 0) {
        theta <- 180 + (atan_d(tantheta))
    }
    else {
        theta <- atan_d(tantheta)
    }
    sintheta <- sin_d(theta)
    costheta <- cos_d(theta)
    sin2theta <- sintheta^2
    cos2theta <- costheta^2
    sinthetacostheta <- sintheta * costheta
    if (weighted) {
        sigmax <- sqrt(2) * sqrt(((sum(weights * points[, 7])) * 
            (cos2theta) - 2 * (sum(weights * points[, 9])) * 
            (sinthetacostheta) + (sum(weights * points[, 8])) * 
            (sin2theta))/((sum(weights)) - 2))
        sigmay <- sqrt(2) * sqrt(((sum(weights * points[, 7])) * 
            (sin2theta) + 2 * (sum(weights * points[, 9])) * 
            (sinthetacostheta) + (sum(weights * points[, 8])) * 
            (cos2theta))/((sum(weights)) - 2))
    }
    else {
        sigmax <- sqrt(2) * sqrt(((sum(points[, 7])) * (cos2theta) - 
            2 * (sum(points[, 9])) * (sinthetacostheta) + (sum(points[, 
            8])) * (sin2theta))/(n - 2))
        sigmay <- sqrt(2) * sqrt(((sum(points[, 7])) * (sin2theta) + 
            2 * (sum(points[, 9])) * (sinthetacostheta) + (sum(points[, 
            8])) * (cos2theta))/(n - 2))
    }
    if (sigmax > sigmay) {
        Major <- "SigmaX"
        Minor <- "SigmaY"
    }
    else {
        Major <- "SigmaY"
        Minor <- "SigmaX"
    }
    lengthsigmax <- 2 * sigmax
    lengthsigmay <- 2 * sigmay
    areaSDE <- pi * sigmax * sigmay
    eccentricity <- sqrt(1 - ((min(sigmax, sigmay)^2)/(max(sigmax, 
        sigmay)^2)))
    B <- min(sigmax, sigmay)
    A <- max(sigmax, sigmay)
    d2 <- (A - B) * (A + B)
    phi <- 2 * pi * seq(0, 1, len = 360)
    sp <- sin(phi)
    cp <- cos(phi)
    r <- sigmax * sigmay/sqrt(B^2 + d2 * sp^2)
    xy <- r * cbind(cp, sp)
    al <- (90 - theta) * pi/180
    ca <- cos(al)
    sa <- sin(al)
    coordsSDE <- xy %*% rbind(c(ca, sa), c(-sa, ca)) + cbind(rep(centre.xy[1], 
        360), rep(centre.xy[2], 360))
    if (verbose) {
        cat("\n----------------------------------------------")
        cat("\nCoordinates of centre (x): ", centre.xy[1], sep = "")
        cat("\nCoordinates of centre (y): ", centre.xy[2], sep = "")
        cat("\nAngle of rotation: ", round(theta, 2), " clockwise degrees", 
            sep = "")
        cat("\nLength of X axis: ", round(lengthsigmax, 2), sep = "")
        cat("\nLength of Y axis: ", round(lengthsigmay, 2), sep = "")
        cat("\nArea of SDE ellipse: ", round(areaSDE, 2), sep = "")
        cat("\ntantheta: ", tantheta, sep = "")
        cat("\ntheta: ", theta, sep = "")
        cat("\nsintheta: ", sintheta, sep = "")
        cat("\ncostheta: ", costheta, sep = "")
        cat("\nsinthetacostheta: ", sinthetacostheta, sep = "")
        cat("\nsin2theta: ", sin2theta, sep = "")
        cat("\ncos2theta: ", cos2theta, sep = "")
        cat("\nsigmax: ", sigmax, sep = "")
        cat("\nsigmay: ", sigmay, sep = "")
        cat("\neccentricity: ", eccentricity, sep = "")
        cat("\n----------------------------------------------\n\n")
    }
    if (sigmax < sigmay) {
        Theta.Corr <- theta
    }
    else {
        Theta.Corr <- theta + 90
    }
    r.SDE <- list(id = id, points = points, coordsSDE = coordsSDE, 
        calccentre = calccentre, CENTRE.x = centre.xy[1], CENTRE.y = centre.xy[2], 
        Major = Major, Minor = Minor, theta = theta, Sigma.x = sigmax, 
        Sigma.y = sigmay, Eccentricity = eccentricity, Area.sde = areaSDE, 
        TanTheta = tantheta, SinTheta = sintheta, CosTheta = costheta, 
        SinThetaCosTheta = sinthetacostheta, Sin2Theta = sin2theta, 
        Cos2Theta = cos2theta, ThetaCorr = Theta.Corr, weighted = weighted, 
        weights = weights)
    assign("r.SDE", r.SDE, pos = 1)
    result.sde <- 
    list(id = id, CALCCENTRE = calccentre, weighted = weighted, 
        CENTRE.x = centre.xy[1], CENTRE.y = centre.xy[2], Sigma.x = sigmax, 
        Sigma.y = sigmay, Major = Major, Minor = Minor, Theta = theta, 
        Eccentricity = eccentricity, Area.sde = areaSDE, TanTheta = tantheta, 
        SinTheta = sintheta, CosTheta = costheta, SinThetaCosTheta = sinthetacostheta, 
        Sin2Theta = sin2theta, Cos2Theta = cos2theta, ThetaCorr = Theta.Corr)
    # print(result.sde)
     result.sde <- as.data.frame(result.sde)
     assign("sdeatt", result.sde, pos = 1)
     sdeloc <- as.data.frame(cbind(id, coordsSDE))
     colnames(sdeloc) = c("id", "x", "y")
     write.table(sdeloc, sep = ",", file = filename, col.names = FALSE)
     assign("sdeloc", sdeloc, pos = 1)
     return(r.SDE)
}


#modifed plot_sdd from aspace allowing you to specify the sdd object

plot_sdd2<- function (x, plotnew = TRUE, plothv = FALSE, plotweightedpts = FALSE, 
    weightedpts.col = "black", weightedpts.pch = 19, plotpoints = TRUE, 
    points.col = "black", points.pch = 1, plotcentre = TRUE, 
    centre.col = "black", centre.pch = 8, centre.cex= 1, titletxt = "Title", 
    xaxis = "Easting (m)", yaxis = "Northing (m)", sdd.col = "black", 
    sdd.lwd = 2, jpeg = FALSE, ...) 
{
    par(...)
    min.x <- min((x$CENTRE.x - x$SDD), min(x$points[, 
        1]))
    max.x <- max((x$CENTRE.x + x$SDD), max(x$points[, 
        1]))
    min.y <- min((x$CENTRE.y - x$SDD), min(x$points[, 
        2]))
    max.y <- max((x$CENTRE.y + x$SDD), max(x$points[, 
        2]))
    if (jpeg) {
        jpeg(filename = paste("SDD", x$id, ".jpg", sep = ""), 
            width = 600, height = 600, pointsize = 12, quality = 90, 
            bg = "white", res = NA)
    }
    if (plotnew) {
        plot(0,type = "n", asp = 1, xlab = xaxis, ylab = yaxis, 
            xlim = c(min.x, max.x), ylim = c(min.y, max.y))
    }
    lines(x$coordsSDD, col = sdd.col, lwd = sdd.lwd)
    title(paste(titletxt, sep = ""))
    if (plothv) {
        abline(h = x$CENTRE.y, col = 1, lty = 2)
        abline(v = x$CENTRE.x, col = 1, lty = 2)
    }
if (plotweightedpts) {
        points(x$points, cex = x$weights, col = weightedpts.col, 
            pch = weightedpts.pch)
    }
    if (plotpoints) {
        points(x$points, col = points.col, pch = points.pch)
    }
    if (plotcentre) {
        points(x$CENTRE.x, x$CENTRE.y, col = centre.col, 
            pch = centre.pch, cex = centre.cex)
    }
    if (jpeg) {
        dev.off()
    }
}


#modifed plot_sde from aspace allowing you to specify the sde object

plot_sde2<-function (x, plotnew = TRUE, plotSDEaxes = FALSE, plotweightedpts = FALSE, 
    weightedpts.col = "black", weightedpts.pch = 19, plotpoints = TRUE, 
    points.col = "black", points.pch = 1, plotcentre = TRUE, 
    centre.col = "black", centre.pch = 8, centre.cex = 1, titletxt = "Title", 
    xaxis = "Easting (m)", yaxis = "Northing (m)", sde.col = "black", 
    sde.lwd = 2, jpeg = FALSE, ...) 
{
    par(...)
    min.x <- min((x$CENTRE.x - max(x$Sigma.x, x$Sigma.y)), 
        min(x$points[, 1]))
    max.x <- max((x$CENTRE.x + max(x$Sigma.x, x$Sigma.y)), 
        max(x$points[, 1]))
    min.y <- min((x$CENTRE.y - max(x$Sigma.x, x$Sigma.y)), 
        min(x$points[, 2]))
    max.y <- max((x$CENTRE.y + max(x$Sigma.x, x$Sigma.y)), 
        max(x$points[, 2]))
    if (jpeg) {
        jpeg(filename = paste("SDE", x$id, ".jpg", sep = ""), 
            width = 600, height = 600, pointsize = 12, quality = 90, 
            bg = "white", res = NA)
    }
    if (plotnew) {
        plot(1, type = "n", asp = 1, xlab = xaxis, ylab = yaxis, 
            xlim = c(min.x, max.x), ylim = c(min.y, max.y))
    }
    lines(x$coordsSDE, col = sde.col, lwd = sde.lwd)
    title(paste(titletxt, sep = ""))
    if (plotSDEaxes) {
        xprime <- x$CENTRE.x + (x$Sigma.y * cos_d(90 - 
            x$theta))
        yprime <- x$CENTRE.y + (x$Sigma.y * sin_d(90 - 
            x$theta))
        segments(x$CENTRE.x, x$CENTRE.y, xprime, yprime, 
            col = 3)
        xprime <- x$CENTRE.x + (x$Sigma.x * cos_d(x$theta))
        yprime <- x$CENTRE.y - (x$Sigma.x * sin_d(x$theta))
        segments(x$CENTRE.x, x$CENTRE.y, xprime, yprime, 
            col = 4)
    }
    if (plotweightedpts) {
        points(x$points, cex = x$weights/quantile(x$weights,0.9), col = weightedpts.col, 
            pch = weightedpts.pch)
    }
    if (plotpoints) {
        points(x$points, col = points.col, pch = points.pch)
    }
    if (plotcentre) {
        points(x$CENTRE.x, x$CENTRE.y, col = centre.col, 
            pch = centre.pch, cex = centre.cex)
    }
    if (jpeg) {
        dev.off()
    }
}


#Calc_box2

"calc_box2"<-
function (id = 1, filename = "BOX_Output.txt", centre.xy = NULL, 
    calccentre = TRUE, weighted = FALSE, weights = NULL, points = activities, 
    verbose = FALSE) 
{
    require(Hmisc)
    errorcode <- 1000
    n <- dim(points)[1]
    if (calccentre) {
        if (length(centre.xy) == 2) {
            errorcode <- 21
            cat("\n\nWARNING: Invalid combination: calccentre=TRUE and centre.xy!=NULL")
            cat("\nERROR CODE: ", errorcode, "\n\n", sep = "")
            return("ERROR")
        }
        else {
            if (weighted) {
                wt.x <- points[, 1] * weights
                wt.y <- points[, 2] * weights
                WMC.x <- c(sum(wt.x)/sum(weights))
                WMC.y <- c(sum(wt.y)/sum(weights))
                centre.xy[1] <- WMC.x
                centre.xy[2] <- WMC.y
            }
            else {
                meanx <- sum(points[, 1])/n
                meany <- sum(points[, 2])/n
                centre.xy[1] <- meanx
                centre.xy[2] <- meany
            }
        }
    }
    dist <- distances(centre.xy, points)
    if (length(dist) >= 3) {
        if (weighted) {
            SDD <- sqrt(sum((weights * dist^2)/((sum(weights)) - 
                2)))
            SDx <- sqrt(wtd.var(points[, 1], weights))
            SDy <- sqrt(wtd.var(points[, 2], weights))
        }
        else {
            SDD <- sqrt(sum(dist^2/(length(dist) - 2)))
            SDx <- sd(points[, 1])
            SDy <- sd(points[, 2])
        }
        areabox <- (2 * SDx) * (2 * SDy)
        NW <- cbind((centre.xy[1] - (SDx)), (centre.xy[2] + (SDy)))
        NE <- cbind((centre.xy[1] + (SDx)), (centre.xy[2] + (SDy)))
        SW <- cbind((centre.xy[1] - (SDx)), (centre.xy[2] - (SDy)))
        SE <- cbind((centre.xy[1] + (SDx)), (centre.xy[2] - (SDy)))
        box.points <- rbind(NW, NE, SE, SW)
        coordsBOX <- cbind(box.points[, 1], box.points[, 2])
        boxloc <- as.data.frame(cbind(id, coordsBOX))
        colnames(boxloc) = c("id", "x", "y")
        write.table(boxloc, sep = ",", file = filename, col.names = FALSE)
        assign("boxloc", boxloc, pos = 1)
        r.BOX <- list(id = id, points = points, calccentre = calccentre, 
            weighted = weighted, weights = weights, CENTRE.x = centre.xy[1], 
            CENTRE.y = centre.xy[2], SDD = SDD, SDx = SDx, SDy = SDy, 
            Box.area = areabox, NW.coord = NW, NE.coord = NE, 
            SW.coord = SW, SE.coord = SE)
        assign("r.BOX", r.BOX, pos = 1)
        result.box <- list(id = id, calccentre = calccentre, 
            weighted = weighted, CENTRE.x = centre.xy[1], CENTRE.y = centre.xy[2], 
            SD.x = SDx, SD.y = SDy, Box.area = areabox, NW.coord = NW, 
            NE.coord = NE, SW.coord = SW, SE.coord = SE)
        result.box <- as.data.frame(result.box)
#        print(result.box)
        assign("boxatt", result.box, pos = 1)
        return(r.BOX)
    }
    else {
        errorcode <- 25
        if (verbose) {
            cat("\n\nWARNING: Not enough values to compute SDD.")
            cat("\nERROR CODE: ", errorcode, "\n\n", sep = "")
        }
        return("ERROR")
    }
}



plot_box2<- 
function (x, plotnew = TRUE, plothv = FALSE, plotweightedpts = FALSE, 
    weightedpts.col = "black", weightedpts.pch = 19, plotpoints = TRUE, 
    points.col = "black", points.pch = 1, plotcentre = TRUE, 
    centre.col = "black", centre.pch = 19, titletxt = "Title", 
    xaxis = "Easting (m)", yaxis = "Northing (m)", box.col = "black", 
    box.lwd = 2, jpeg = FALSE, ...) 
{
    par(...)
    min.x <- min((x$CENTRE.x - x$SDD), min(x$points[, 
        1]))
    max.x <- max((x$CENTRE.x + x$SDD), max(x$points[, 
        1]))
    min.y <- min((x$CENTRE.y - x$SDD), min(x$points[, 
        2]))
    max.y <- max((x$CENTRE.y + x$SDD), max(x$points[, 
        2]))
    if (jpeg) {
        jpeg(filename = paste("BOX", x$id, ".jpg", sep = ""), 
            width = 600, height = 600, pointsize = 12, quality = 90, 
            bg = "white", res = NA)
    }
    if (plotnew) {
        plot(1, type = "n", asp = 1, xlab = xaxis, ylab = yaxis, 
            xlim = c(min.x, max.x), ylim = c(min.y, max.y))
    }
    title(paste(titletxt, sep = ""))
    segments(x$NW.coord[1], x$NW.coord[2], x$NE.coord[1], 
        x$NE.coord[2], col = box.col, lwd = box.lwd)
    segments(x$SW.coord[1], x$SW.coord[2], x$SE.coord[1], 
        x$SE.coord[2], col = box.col, lwd = box.lwd)
    segments(x$NE.coord[1], x$NE.coord[2], x$SE.coord[1], 
        x$SE.coord[2], col = box.col, lwd = box.lwd)
    segments(x$NW.coord[1], x$NW.coord[2], x$SW.coord[1], 
        x$SW.coord[2], col = box.col, lwd = box.lwd)
    if (plothv) {
        abline(h = x$CENTRE.y, col = 1, lty = 2)
        abline(v = x$CENTRE.x, col = 1, lty = 2)
    }
    if (plotweightedpts) {
        points(x$points, cex = x$weights, col = weightedpts.col, 
            pch = weightedpts.pch)
    }
    if (plotpoints) {
        points(x$points, col = points.col, pch = points.pch)
    }
    if (plotcentre) {
        points(x$CENTRE.x, x$CENTRE.y, col = centre.col, 
            pch = centre.pch)
    }
    if (jpeg) {
        dev.off()
    }
}


# Helper functions
# SSB
ssb_as_sf <- function(x, orig_crs, centre_dummy = FALSE) {
  
  require(tidyverse)
  require(sf)

  # Convert SSB to sf polygon
  ssb_sf <- names(x) %>% 
    str_detect("coord$") %>%
    keep(x, .) %>% 
    bind_rows() %>%
    t() %>%
    as.data.frame() %>% 
    rownames_to_column("label") %>% 
    rename("x" = V1, "y" = V2) %>% 
    mutate(order = case_when(str_sub(label, 1, 2) == "NW" ~ 1,
                             str_sub(label, 1, 2) == "NE" ~ 2,
                             str_sub(label, 1, 2) == "SE" ~ 3,
                             str_sub(label, 1, 2) == "SW" ~ 4)) %>%
    arrange(order) %>%
    st_as_sf(., coords = c("x", "y"), crs = orig_crs) %>% 
    st_combine() %>% 
    st_cast("POLYGON")

  # Convert Centre to sf points
  centre_sf <- names(x) %>% 
    str_detect("CENTRE") %>%
    keep(x, .) %>% 
    bind_cols() %>% 
    st_as_sf(., coords = c("CENTRE.x", "CENTRE.y"), crs = orig_crs) %>% 
    st_union() %>% 
    st_cast("POINT")

  # Get key metrics
  metrics <- names(x) %>% 
    str_detect("^(weight|SD)") %>%
    keep(x, .) %>% 
    rbind() %>% 
    as_tibble() %>% 
    unnest(cols = c("weighted", "SDD", "SDx", "SDy")) %>% 
    dplyr::select(everything(), weighted)
    
  # Return ssb or centre depending on entry
  if (centre_dummy == TRUE) {
    final <- centre_sf %>% st_as_sf()
  } else {
    final <- cbind(metrics, ssb_sf) %>% st_as_sf()
  }

  return(final)
  
}

# SDD
sdd_as_sf <- function(x, orig_crs) {
  
  require(tidyverse)
  require(sf)
  
  # Convert SDD to sf polygon
  sdd_sf <- names(x) %>% 
    str_detect("coords") %>%
    keep(x, .) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    st_as_sf(., coords = c("coordsSDD.1", "coordsSDD.2"), crs = orig_crs) %>% 
    st_combine() %>% 
    st_union() %>% 
    st_cast("POLYGON")
  
  # Get key metrics
  metrics <- names(x) %>% 
    str_detect("^(weight|SDD)") %>%
    keep(x, .) %>% 
    rbind() %>% 
    as_tibble() %>% 
    unnest(cols = c("SDD", "weighted", "SDD.area"))
  
  # Return SDD
  final <- cbind(metrics, sdd_sf) %>% 
    st_as_sf()

  return(final)
  
}

# SDE
sde_as_sf <- function(x, orig_crs) {
  
  require(tidyverse)
  require(sf)
  
  # Convert SDE to sf polygon
  sde_sf <- names(x) %>% 
    str_detect("coords") %>%
    keep(x, .) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    st_as_sf(., coords = c("coordsSDE.1", "coordsSDE.2"), crs = orig_crs) %>% 
    st_combine() %>% 
    st_union() %>% 
    st_cast("POLYGON")
  
  # Get key metrics
  metrics <- names(x) %>% 
    str_detect("^(CENTRE|Major|Minor|(t|T)heta|Sigma|Eccent|Area|Tan|Sin|Cos|weight)") %>%
    keep(x, .) %>% 
    rbind() %>% 
    as_tibble() %>% 
    unnest(cols = c("CENTRE.x", "CENTRE.y", "Major", "Minor", "theta", "Sigma.x", "Sigma.y", 
                    "Eccentricity", "Area.sde", "TanTheta", "SinTheta", "CosTheta", "SinThetaCosTheta", 
                    "Sin2Theta", "Cos2Theta", "ThetaCorr", "weighted"))

  # Return SDD
  final <- cbind(metrics, sde_sf) %>% 
    st_as_sf()
  
  return(final)
  
}