stppm <- function (X, formula, W, formula_mark = NULL, covs = NULL, marked = FALSE, 
                   
                   spatial.cov = FALSE, verbose = FALSE, mult = 4, interp = TRUE, 
                   parallel = FALSE, sites = 1, seed = NULL, ncube = NULL, grid = FALSE, 
                   ncores = 2) 
{
  if (!inherits(X, c("stp", "stpm"))) 
    stop("x should be either of class stp or stpm")
  time1 <- Sys.time()
  if (!is.numeric(mult)) {
    stop("mult should be a numeric value")
  }
  else {
    if (mult <= 0) {
      stop("mult should be mult > 0")
    }
  }
  if (!is.numeric(sites)) {
    stop("sites should be a numeric value")
  }
  else {
    if (sites <= 0) {
      stop("sites should be sites > 0")
    }
  }
  if (!is.null(ncube)) {
    if (!is.numeric(ncube)) {
      stop("ncube should be a numeric value")
    }
    else {
      if (ncube <= 0) {
        stop("ncube should be ncube > 0")
      }
    }
  }
  if (is.null(formula_mark)) {
    formula_mark <- formula
  }
  X0 <- X
  X <- X$df
  nX <- nrow(X)
  x <- X[, 1]
  y <- X[, 2]
  t <- X[, 3]
  if (interp) {
    s.region <- splancs::sbox(cbind(x, y), xfrac = 0.01, 
                              yfrac = 0.01)
    xr = range(t, na.rm = TRUE)
    xw = diff(xr)
    t.region <- c(xr[1] - 0.01 * xw, xr[2] + 0.01 * xw)
    HomLambda <- nX
    rho <- mult * HomLambda
    if (grid == TRUE) {
      ff <- floor(rho^(1/3))
      x0 <- y0 <- t0 <- 1:ff
      scale_to_range <- function(x, new_min, new_max) {
        ((x - min(x))/(max(x) - min(x))) * (new_max - 
                                              new_min) + new_min
      }
      x0 <- scale_to_range(x0, s.region[1, 1], s.region[2, 
                                                        1])
      y0 <- scale_to_range(y0, s.region[1, 2], s.region[3, 
                                                        2])
      t0 <- scale_to_range(t0, t.region[1], t.region[2])
      df0 <- expand.grid(x0, y0, t0)
      dummy_points <- stp(cbind(df0$Var1, df0$Var2, df0$Var3))$df
    }
    else {
      if (!is.null(seed)) 
        set.seed(seed)
      dummy_points <- rpp(npoints = rho,
                          s.region = cbind(W$bdry[[1]]$x,
                                           W$bdry[[1]]$y),
                          t.region = range(t))$xyt[,1:3]
      # dummy_points <- rstpp(lambda = rho, nsim = 1, verbose = F, 
      #                       minX = s.region[1, 1], maxX = s.region[2, 1], 
      #                       minY = s.region[1, 2], maxY = s.region[3, 2], 
      #                       minT = t.region[1], maxT = t.region[2])$df
    }
  }
  else {
    dummy_points <- stp(cbind(covs[[sites]][, 1], covs[[sites]][, 
                                                                2], covs[[sites]][, 3]))$df
  }
  # quad_p <- rbind(X[, 1:3], dummy_points)
  quad_p <- rbind(X[, 1:3], dummy_points)
  xx <- quad_p[, 1]
  xy <- quad_p[, 2]
  xt <- quad_p[, 3]
  win <- spatstat.geom::box3(xrange = range(xx), yrange = range(xy), 
                             zrange = range(xt))
  if (is.null(ncube)) {
    ncube <- .default.ncube(quad_p)
  }
  ncube <- rep.int(ncube, 3)
  nx <- ncube[1]
  ny <- ncube[2]
  nt <- ncube[3]
  nxyt <- nx * ny * nt
  cubevolume <- spatstat.geom::volume(win)/nxyt
  volumes <- rep.int(cubevolume, nxyt)
  id <- .grid.index(xx, xy, xt, win$xrange, win$yrange, win$zrange, 
                    nx, ny, nt)$index
  w <- .counting.weights(id, volumes)#, lsr
  Wdat <- w[1:nX]
  Wdum <- w[-c(1:nX)]
  ndata <- nrow(X)
  ndummy <- nrow(dummy_points)
  if (marked == T & spatial.cov == T) {
    dati.interpolati <- interp.covariate(X, dummy_points, 
                                         covs, formula, parallel, interp, xx, xy, xt, ncores, 
                                         verbose = verbose)
    marked.process <- dummy.marked.result(X, formula = formula_mark, 
                                          dummy_points, Wdum, Wdat, ndata, ndummy)
    z <- c(rep(1, ndata), rep(0, length(marked.process$dumb$data$x)))
    w_final <- c(w[1:ndata], marked.process$Wdumb)
    y_resp <- z/w_final
    # if (lsr) {
    #   z
    # }
    # else {
    #   
    # }
    dati.modello <- data.frame(y_resp, w_final, c(X$x, marked.process$dumb$data$x), 
                               c(X$y, marked.process$dumb$data$y), c(X$t, marked.process$dumb$data$z))
    colnames(dati.modello) <- c("y_resp", "w", "x", "y", 
                                "t")
    dati.modello <- cbind(dati.modello, rbind(marked.process$df_marks, 
                                              marked.process$total_dummy_marks))
    dati.interpolati.rep <- as.data.frame(matrix(NA, nrow = nrow(dati.modello), 
                                                 ncol = ncol(dati.interpolati) - 3))
    colnames(dati.interpolati.rep) <- names(dati.interpolati)[-c(1, 
                                                                 2, 3)]
    q <- 1
    for (i in 4:ncol(dati.interpolati)) {
      dati.interpolati.rep[, q] <- c(dati.interpolati[1:nX, 
                                                      i], rep(dati.interpolati[(nX + 1):nrow(dati.interpolati), 
                                                                               i], marked.process$n_comb_levels), rep(dati.interpolati[1:nX, 
                                                                                                                                       i], marked.process$n_comb_levels - 1))
      q <- q + 1
    }
    dati.cov.marks <- cbind(dati.modello, dati.interpolati.rep)
  }
  else if (marked == F & spatial.cov == T) {
    dati.interpolati <- interp.covariate(X, dummy_points, 
                                         covs, formula, parallel, interp, xx, xy, xt, ncores, 
                                         verbose = verbose)
    z <- c(rep(1, ndata), rep(0, nrow(dummy_points)))
    y_resp <- z/w
    # if (lsr) {
    #   z
    # }
    # else {
    #   z/w
    # }
    dati.cov.marks <- cbind(y_resp, w, dati.interpolati)
  }
  else if (marked == F & spatial.cov == F) {
    colnames(quad_p) <- c("x", "y", "t")
    z <- c(rep(1, ndata), rep(0, nrow(dummy_points)))
    y_resp <- z/w
    # if (lsr) {
    #   z
    # }
    # else {
    #   
    # }
    dati.cov.marks <- cbind(y_resp, w, quad_p)
  }
  else if (marked == T & spatial.cov == F) {
    marked.process <- dummy.marked.result(X, formula = formula_mark, 
                                          dummy_points, Wdum, Wdat, ndata, ndummy)
    z <- c(rep(1, ndata), rep(0, length(marked.process$dumb$data$x)))
    w_final <- c(w[1:ndata], marked.process$Wdumb)
    y_resp <- z/w_final 
    # if (lsr) {
    #   z
    # }
    # else {
    #   
    # }
    dati.modello <- data.frame(y_resp, w_final, c(X$x, marked.process$dumb$data$x), 
                               c(X$y, marked.process$dumb$data$y), c(X$t, marked.process$dumb$data$z))
    colnames(dati.modello) <- c("y_resp", "w", "x", "y", 
                                "t")
    dati.modello <- cbind(dati.modello, rbind(marked.process$df_marks, 
                                              marked.process$total_dummy_marks))
    dati.cov.marks <- dati.modello
  }
  # if (lsr) {
  #   suppressWarnings(mod_global <- try(gam(as.formula(paste("y_resp", 
  #                                                           paste(formula, collapse = " "), "+ offset(log(w))", 
  #                                                           sep = " ")), family = binomial, data = dati.cov.marks), 
  #                                      silent = T))
  # }
  # else {
  suppressWarnings(mod_global <- try(gam(as.formula(paste("y_resp", 
                                                          paste(formula, collapse = " "), sep = " ")), weights = w, 
                                         family = poisson, data = dati.cov.marks), silent = T))
  # }
  res_global <- coef(mod_global)
  pred_global <- exp(predict(mod_global, newdata = dati.cov.marks[1:nX, 
  ]))
  time2 <- Sys.time()
  list.obj <- list(IntCoefs = res_global, X = X0, nX = ndata, 
                   I = z, y_resp = y_resp, formula = formula, l = as.vector(pred_global), 
                   mod_global = mod_global, newdata = dati.cov.marks[1:ndata, 
                   ], ncube = ncube, time = paste0(round(as.numeric(difftime(time1 = time2, 
                                                                             time2 = time1, units = "sec")), 3), " sec"))
  class(list.obj) <- "stppm"
  return(list.obj)
}

plot.stppm <- function(x, W,
                       scaler = c("silverman", "IQR", "sd", "var"),
                       do.points = TRUE,
                       print.bw = FALSE,
                       zap = 0.00001,
                       par = TRUE, mark_int = NULL,
                       ...){
  
  
  if(inherits(x$IntCoefs, "numeric") & length(x$IntCoefs) == 1){
    stop("Constant intensity, no plot to show")
  }
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  if(is.null(mark_int)) mark_int <- x$l
  
  id <- which(!is.na(x$l))
  
  ppx_int <- suppressWarnings(spatstat.geom::ppp(x$X$df$x[id], x$X$df$y[id],
                                                 marks = mark_int[id],
                                                 window = W#spatstat.geom::owin(range(x$X$df$x[id]), range(x$X$df$y[id]))
  ))
  sig <- sparr::OS(unmark(ppx_int), scaler = scaler)
  
  if(par == T){
    par(mfrow = c(1, 2))
    par(mar = c(5, 4, 4, 2) + 0.1 - c(4, 1 , 1, 1))
    plot(spatstat.explore::Smooth(ppx_int, sigma = sig), zap = zap,
         col = attr(spatstat.geom::colourmap(grDevices::hcl.colors(100, "Viridis",
                                                                   rev = TRUE),
                                             range = range(mark_int[id])),
                    "stuff")$outputs,
         main = c("Intensity in space \n Density Kernel Smoothing"))
    if(do.points == T){plot(spatstat.geom::unmark(ppx_int), add = T, cex = 0.5)}
    
    par(mar = c(5, 4, 4, 2) + 0.1 - c(4, 1 , 1, -0.5))
    
    plot3D::scatter3D(x$X$df$x[id], x$X$df$y[id], x$X$df$t[id],
                      theta = - 45, phi = 20,
                      col = attr(spatstat.geom::colourmap(grDevices::hcl.colors(100, "Viridis", rev = TRUE),
                                                          range = range(mark_int[id])),
                                 "stuff")$outputs,
                      ticktype = "detailed", pch = 20,
                      colvar = mark_int[id],
                      xlab="x",ylab="y",zlab="t",
                      main = c("Intensity in space-time \n Pointwise computation"))
    par(mar = c(5, 4, 4, 2) + 0.1)
  } else {
    par(mfrow = c(1, 1))
    par(ask = TRUE)
    plot(spatstat.explore::Smooth(ppx_int, sigma = sig), zap = zap,
         col = attr(spatstat.geom::colourmap(grDevices::hcl.colors(100, "Viridis", rev = TRUE),
                                             range = range(mark_int[id])),
                    "stuff")$outputs,
         main = c("Intensity in space \n Density Kernel Smoothing"))
    if(do.points == T){plot(spatstat.geom::unmark(ppx_int[id]), add = T, cex = 0.5)}
    par(ask = TRUE)
    par(mar = c(5, 4, 4, 2) + 0.1 - c(4, 1 , 1, -0.5))
    plot3D::scatter3D(x$X$df$x[id], x$X$df$y[id], x$X$df$t[id],
                      theta = - 45, phi = 20,
                      col = attr(spatstat.geom::colourmap(grDevices::hcl.colors(100, "Viridis", rev = TRUE),
                                                          range = range(mark_int[id])),
                                 "stuff")$outputs,
                      ticktype = "detailed", pch = 20,
                      colvar = mark_int[id],
                      xlab="x",ylab="y",zlab="t",
                      main = c("Intensity in space-time \n Pointwise computation"))
    par(mar = c(5, 4, 4, 2) + 0.1)
    par(ask = FALSE)
  }
  if(print.bw == T){print(sig)}
}

.default.ncube <- function(X){
  guess.ngrid <- floor((splancs::npts(X) / 2) ^ (1 / 3))
  max(5, guess.ngrid)
}

.grid.index <- function(x, y, t, xrange, yrange, trange, nx, ny, nt) {
  
  ix <- .grid1.index(x, xrange, nx)
  iy <- .grid1.index(y, yrange, ny)
  it <- .grid1.index(t, trange, nt)
  
  return(list(ix = ix, iy = iy, it = it, index = as.integer((iy - 1) * nx + ix + (it - 1) * nx * ny)))
}

.grid1.index <- function(x, xrange, nx) {
  i <- ceiling(nx * (x - xrange[1]) / diff(xrange))
  i <- pmax.int(1, i)
  i <- pmin.int(i, nx)
  i
}

.counting.weights <- function(id, volumes) {
  id <- as.integer(id)
  fid <- factor(id, levels = seq_along(volumes))
  counts <- table(fid)
  w <- volumes[id] / counts[id]
  w <- as.vector(w)
  names(w) <- NULL
  return(w)
}




as.stpp <- function(x){
  if(!inherits(x,"stp")) stop("class(x) must be stp")
  
  out <- cbind(x$df$x, x$df$y, x$df$t)
  colnames(out) <- c("x", "y", "t")
  class(out) <- "stpp"
  return(out)
}

g.sep_st_exp_exp2 <- function(param, useq, vseq, ghat, transform, power){
  uv <- expand.grid(useq, vseq)
  g.parametric <- exp(param[1] * exp( - uv[, 2] / param[3]) * exp( - uv[, 1] / param[2]))
  ghat2 <- as.vector(ghat)
  return(sum((ghat2 ^ power - g.parametric ^ power) ^ 2))
}

stlgcppm <- function (X, formula = ~1, W, verbose = TRUE, seed = NULL, cov = c("separable", 
                                                                               "gneiting", "iaco-cesare"), first = c("global", "local"), 
                      second = c("global", "local"), mult = 4, hs = c("global", 
                                                                      "local"), npx0 = 10, npt0 = 10, itnmax = 100, min_vals = NULL, 
                      max_vals = NULL) 
{
  if (!inherits(X, c("stp"))) 
    stop("X should be from class stp")
  time1 <- Sys.time()
  cov <- match.arg(cov)
  first <- match.arg(first)
  second <- match.arg(second)
  hs <- match.arg(hs)
  if (!is.numeric(mult)) {
    stop("mult should be a numeric value")
  }
  else {
    if (mult <= 0) {
      stop("mult should be mult > 0")
    }
  }
  if (!is.numeric(npx0)) {
    stop("npx0 should be a numeric value")
  }
  else {
    if (npx0 <= 0) {
      stop("npx0 should be npx0 > 0")
    }
  }
  if (!is.numeric(npt0)) {
    stop("npt0 should be a numeric value")
  }
  else {
    if (npt0 <= 0) {
      stop("npt0 should be npt0 > 0")
    }
  }
  if (!is.numeric(itnmax)) {
    stop("itnmax should be a numeric value")
  }
  else {
    if (itnmax <= 0) {
      stop("itnmax should be itnmax > 0")
    }
  }
  nX <- nrow(X$df)
  x <- X$df$x
  y <- X$df$y
  t <- X$df$t
  s.region <- splancs::sbox(cbind(x, y), xfrac = 0.01, yfrac = 0.01)
  xr = range(t, na.rm = TRUE)
  xw = diff(xr)
  t.region <- c(xr[1] - 0.01 * xw, xr[2] + 0.01 * xw)
  HomLambda <- nX
  rho <- mult * HomLambda
  set.seed(seed)
  dummy_points <- rpp(npoints = rho,
                      s.region = cbind(W$bdry[[1]]$x,
                                       W$bdry[[1]]$y),
                      t.region = range(t))$xyt[,1:3]
  # dummy_points <- rstpp(lambda = rho, nsim = 1, verbose = F, 
  #                       minX = s.region[1, 1], maxX = s.region[2, 1], minY = s.region[1, 
  #                                                                                     2], maxY = s.region[3, 2], minT = t.region[1], maxT = t.region[2])$df
  quad_p <- rbind(X$df, dummy_points)
  z <- c(rep(1, nX), rep(0, dim(dummy_points)[1]))
  xx <- quad_p[, 1]
  xy <- quad_p[, 2]
  xt <- quad_p[, 3]
  win <- spatstat.geom::box3(xrange = range(xx), yrange = range(xy), 
                             zrange = range(xt))
  ncube <- .default.ncube(quad_p)
  length(ncube) == 1
  ncube <- rep.int(ncube, 3)
  nx <- ncube[1]
  ny <- ncube[2]
  nt <- ncube[3]
  nxyt <- nx * ny * nt
  cubevolume <- spatstat.geom::volume(win)/nxyt
  volumes <- rep.int(cubevolume, nxyt)
  id <- .grid.index(xx, xy, xt, win$xrange, win$yrange, win$zrange, 
                    nx, ny, nt)$index
  w <- .counting.weights(id, volumes)
  y_resp <- z/w
  dati.modello <- cbind(y_resp, w, quad_p[, 1], quad_p[, 2], 
                        quad_p[, 3])
  colnames(dati.modello) <- c("y_resp", "w", "x", "y", "t")
  dati.modello <- as.data.frame(dati.modello)
  suppressWarnings(mod_global <- try(glm(as.formula(paste("y_resp", 
                                                          paste(formula, collapse = " "), sep = " ")), weights = w, 
                                         family = poisson, data = dati.modello), silent = T))
  res_global <- mod_global$coefficients
  pred_global <- exp(predict(mod_global, newdata = dati.modello[1:nX, 
  ]))
  nU <- dim(quad_p)[1]
  xx <- quad_p[, 1]
  xy <- quad_p[, 2]
  xt <- quad_p[, 3]
  h_x <- MASS::bandwidth.nrd(x)
  h_y <- MASS::bandwidth.nrd(y)
  h_t <- MASS::bandwidth.nrd(t)
  if (first == "local") {
    if (hs == "local") {
      h_x <- h_y <- kde2d.new.var(x, y, gx = x, gy = y, 
                                  np = npx0)$hx
      h_t <- density.new.var(t, gx = t, np = npt0)$h
    }
    localwt <- matrix(NA, nrow = nX, ncol = nU)
    if (verbose) 
      cat(paste("\n", "Computing Kernel Densities to the", 
                nX, "points", "\n", "\n"))
    for (j in 1:nX) {
      if (verbose) 
        spatstat.geom::progressreport(j, nX)
      localwt[j, ] <- switch(hs, local = dnorm(xx - x[j], 
                                               sd = h_x[j]) * dnorm(xy - y[j], sd = h_y[j]) * 
                               dnorm(xt - t[j], sd = h_t[j]), global = dnorm(xx - 
                                                                               x[j], sd = h_x) * dnorm(xy - y[j], sd = h_y) * 
                               dnorm(xt - t[j], sd = h_t))
    }
    a_s <- localwt * w
    res_local <- matrix(NA, nrow = nX, ncol = length(mod_global$coefficients))
    pred_local <- vector(length = nX)
    if (verbose) 
      cat(paste("\n", "Fitting local model to the", nX, 
                "points", "\n", "\n"))
    for (i in 1:nX) {
      progressreport(i, nX)
      suppressWarnings(mod_local <- try(glm(as.formula(paste("y_resp", 
                                                             paste(formula, collapse = " "), sep = " ")), 
                                            weights = a_s[i, ], family = poisson, data = dati.modello), 
                                        silent = T))
      res_local[i, ] <- mod_local$coefficients
      pred_local[i] <- exp(predict(mod_local, newdata = dati.modello[i, 
      ]))
    }
  }
  tlim <- max(t)
  cube <- (range(x) - min(x))[2]
  lgrid_s <- 25
  rsup_t <- tlim/4
  dt <- dist(t)
  bw_vector_t <- KernSmooth::dpik(dt, kernel = "box", range.x = c(min(dt), 
                                                                  max(dt)))
  rmin_vector_t2 <- (bw_vector_t * ((1 + lgrid_s)/lgrid_s))
  vals_t <- cbind(rmin_vector_t2, bw_vector_t)
  rsup_s <- cube/4
  ds <- dist(cbind(x, y))
  bw2_vector <- KernSmooth::dpik(ds, kernel = "epanech", range.x = c(min(ds), 
                                                                     max(ds)))
  rmin3_vector <- bw2_vector * ((1 + lgrid_s)/lgrid_s)
  vals_s <- cbind(rmin3_vector, bw2_vector)
  s.region <- matrix(c(range(x)[1] - 0.05, range(x)[2] + 0.05, 
                       range(x)[2] + 0.05, range(x)[1] - 0.05, range(y)[1] - 
                         0.05, range(y)[1] - 0.05, range(y)[2] + 0.05, range(y)[2] + 
                         0.05), ncol = 2)
  t.region <- c(min(t) - 1, tlim + 1)
  u <- seq(vals_s[1], rsup_s, len = lgrid_s)
  v <- seq(vals_t[1], rsup_t, len = lgrid_s)
  us0 <- vals_s[2]
  vt0 <- vals_t[2]
  if (first == "global") {
    if (second == "global") {
      g0 <- stpp::PCFhat(xyt = as.stpp(X), lambda = pred_global, 
                         s.region = s.region, t.region = t.region, dist = u, 
                         times = v, ks = "epanech", hs = us0, kt = "box", 
                         ht = vt0)
    }
    else {
      g0 <- stpp::LISTAhat(xyt = as.stpp(X), lambda = pred_global, 
                           s.region = s.region, t.region = t.region, dist = u, 
                           times = v, ks = "epanech", hs = us0, kt = "box", 
                           ht = vt0)
    }
  }
  else {
    if (second == "global") {
      g0 <- stpp::PCFhat(xyt = as.stpp(X), lambda = pred_local, 
                         s.region = s.region, t.region = t.region, dist = u, 
                         times = v, ks = "epanech", hs = us0, kt = "box", 
                         ht = vt0)
    }
    else {
      g0 <- stpp::LISTAhat(xyt = as.stpp(X), lambda = pred_local, 
                           s.region = s.region, t.region = t.region, dist = u, 
                           times = v, ks = "epanech", hs = us0, kt = "box", 
                           ht = vt0)
    }
  }
  max_dist <- max(dist(s.region))
  max_dist_t <- max(diff(t.region))
  if (second == "local") {
    nonpar_g_st_finite <- is.finite(g0$list.LISTA)
    g0$list.LISTA[!nonpar_g_st_finite] <- 0
  }
  else {
    nonpar_g_st_finite <- is.finite(g0$pcf)
    g0$pcf[!nonpar_g_st_finite] <- 0
  }
  if (cov == "separable") {
    if (is.null(min_vals)) 
      min_vals <- c(0.01, 0.001, 0.01)
    if (is.null(max_vals)) 
      max_vals <- c(50, 8, 1500)
    start_par <- c(log(nX)/2, max_dist/100, max_dist_t/100)
    if (second == "global") {
      MINCON.EXP_EXP <- c(NA, NA, NA)
      MINCON.EXP_EXP <- optimx::optimx(par = start_par, 
                                       fn = g.sep_st_exp_exp2, useq = g0$dist, vseq = g0$times, 
                                       ghat = g0$pcf, transform = NULL, power = 1, method = c("nlm"), 
                                       lower = min_vals, upper = max_vals, itnmax = itnmax)
      res <- as.numeric(as.vector(MINCON.EXP_EXP[1:3]))
    }
    else {
      res <- matrix(NA, nrow = nX, ncol = 3)
      if (verbose) 
        cat(paste("\n", "Fitting local Separable covariance to the", 
                  nX, "points", "\n", "\n"))
      for (id in 1:nX) {
        spatstat.geom::progressreport(id, nX)
        MINCON.EXP_EXP <- c(NA, NA, NA)
        wi <- dnorm(x - x[id], sd = h_x) * dnorm(y - 
                                                   y[id], sd = h_y) * dnorm(t - t[id], sd = h_t)
        numer0 <- sapply(1:nX, function(x) g0$list.LISTA[, 
                                                         , x] * wi[x], simplify = "array")
        numer <- apply(numer0, 1:2, sum)
        denom0 <- sapply(1:nX, function(x) nonpar_g_st_finite[, 
                                                              , x] * wi[x], simplify = "array")
        denom <- apply(denom0, 1:2, sum)
        avg_lista <- numer/denom
        MINCON.EXP_EXP <- optimx::optimx(par = start_par, 
                                         fn = g.sep_st_exp_exp2, useq = g0$dist, vseq = g0$times, 
                                         ghat = avg_lista, transform = NULL, power = 1, 
                                         method = c("nlm"), lower = min_vals, upper = max_vals, 
                                         itnmax = itnmax)
        res[id, ] <- as.numeric(as.vector(MINCON.EXP_EXP[1:3]))
      }
    }
  }
  if (cov == "gneiting") {
    if (is.null(min_vals)) 
      min_vals <- c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01)
    if (is.null(max_vals)) 
      max_vals <- c(nX, max_dist * 250, max_dist_t * 250, 
                    2, 2, 2)
    start_par <- c(log(nX)/2, max_dist, max_dist_t, 1, 1, 
                   1)
    if (second == "global") {
      MINCON_GN <- c(NA, NA, NA, NA, NA, NA)
      MINCON_GN <- optimx::optimx(par = start_par, fn = g_st, 
                                  useq = g0$dist, vseq = g0$times, ghat = g0$pcf, 
                                  transform = NULL, power = 1, method = c("nlm"), 
                                  lower = min_vals, upper = max_vals, itnmax = itnmax)
      res <- as.numeric(as.vector(MINCON_GN[1:6]))
    }
    else {
      res <- matrix(NA, nrow = nX, ncol = 6)
      if (verbose) 
        cat(paste("\n", "Fitting local Gneiting covariance to the", 
                  nX, "points", "\n", "\n"))
      for (id in 1:nX) {
        spatstat.geom::progressreport(id, nX)
        MINCON_GN <- c(NA, NA, NA, NA, NA, NA)
        wi <- dnorm(x - x[id], sd = h_x) * dnorm(y - 
                                                   y[id], sd = h_y) * dnorm(t - t[id], sd = h_t)
        numer0 <- sapply(1:nX, function(x) g0$list.LISTA[, 
                                                         , x] * wi[x], simplify = "array")
        numer <- apply(numer0, 1:2, sum)
        denom0 <- sapply(1:nX, function(x) nonpar_g_st_finite[, 
                                                              , x] * wi[x], simplify = "array")
        denom <- apply(denom0, 1:2, sum)
        avg_lista <- numer/denom
        MINCON_GN <- optimx::optimx(par = start_par, 
                                    fn = g_st, useq = g0$dist, vseq = g0$times, 
                                    ghat = avg_lista, transform = NULL, power = 1, 
                                    method = c("nlm"), lower = min_vals, upper = max_vals, 
                                    itnmax = itnmax)
        res[id, ] <- as.numeric(as.vector(MINCON_GN[1:6]))
      }
    }
  }
  if (cov == "iaco-cesare") {
    if (is.null(min_vals)) 
      min_vals <- c(0.01, 0.01, 0.01, 0.01, 0.01, 1.5)
    if (is.null(max_vals)) 
      max_vals <- c(nX, max_dist * 250, max_dist_t * 250, 
                    2, 2, 15)
    start_par <- c(log(nX)/2, max_dist, max_dist_t, 1, 1, 
                   8)
    if (second == "global") {
      MINCON_IACO <- c(NA, NA, NA, NA, NA, NA)
      MINCON_IACO <- optimx::optimx(par = start_par, fn = g_st_iaco, 
                                    useq = g0$dist, vseq = g0$times, ghat = g0$pcf, 
                                    transform = NULL, power = 1, method = c("nlm"), 
                                    lower = min_vals, upper = max_vals, itnmax = itnmax)
      res <- as.numeric(as.vector(MINCON_IACO[1:6]))
    }
    else {
      res <- matrix(NA, nrow = nX, ncol = 6)
      if (verbose) 
        cat(paste("\n", "Fitting local Iaco-Cesare covariance to the", 
                  nX, "points", "\n", "\n"))
      for (id in 1:nX) {
        spatstat.geom::progressreport(id, nX)
        MINCON_IACO <- c(NA, NA, NA, NA, NA, NA)
        wi <- dnorm(x - x[id], sd = h_x) * dnorm(y - 
                                                   y[id], sd = h_y) * dnorm(t - t[id], sd = h_t)
        numer0 <- sapply(1:nX, function(x) g0$list.LISTA[, 
                                                         , x] * wi[x], simplify = "array")
        numer <- apply(numer0, 1:2, sum)
        denom0 <- sapply(1:nX, function(x) nonpar_g_st_finite[, 
                                                              , x] * wi[x], simplify = "array")
        denom <- apply(denom0, 1:2, sum)
        avg_lista <- numer/denom
        MINCON_IACO <- optimx::optimx(par = start_par, 
                                      fn = g_st_iaco, useq = g0$dist, vseq = g0$times, 
                                      ghat = avg_lista, transform = NULL, power = 1, 
                                      method = c("nlm"), lower = min_vals, upper = max_vals, 
                                      itnmax = itnmax)
        res[id, ] <- as.numeric(as.vector(MINCON_IACO[1:6]))
      }
    }
  }
  time2 <- Sys.time()
  if (second == "local") {
    colnames(res) <- switch(cov, separable = c("sigma", "alpha", 
                                               "beta"), gneiting = c("sigma", "alpha", "beta", "gamma_s", 
                                                                     "gamma_t", "delta"), `iaco-cesare` = c("sigma", "alpha", 
                                                                                                            "beta", "gamma_s", "gamma_t", "delta"))
    res <- as.data.frame(res)
  }
  else {
    names(res) <- switch(cov, separable = c("sigma", "alpha", 
                                            "beta"), gneiting = c("sigma", "alpha", "beta", "gamma_s", 
                                                                  "gamma_t", "delta"), `iaco-cesare` = c("sigma", "alpha", 
                                                                                                         "beta", "gamma_s", "gamma_t", "delta"))
  }
  if (inherits(res, "numeric")) {
    int2 <- rep(res[1], nX)/2
  }
  else {
    int2 <- res$sigma/2
  }
  if (first == "local") {
    names(res_global) <- names(mod_global$coefficients)
    res_local <- data.frame(res_local)
    colnames(res_local) <- names(mod_global$coefficients)
    if (hs == "global") {
      bw <- c(round(h_x, 3), round(h_y, 3), round(h_t, 
                                                  3))
      names(bw) <- c("h_x", "h_y", "h_t")
    }
    else {
      bw <- cbind(round(h_x, 3), round(h_y, 3), round(h_t, 
                                                      3))
      colnames(bw) <- c("h_x", "h_y", "h_t")
    }
    quad_p <- rbind(X$df)
    quad_p <- as.data.frame(quad_p)
    list.obj <- list(IntCoefs = res_local, CovCoefs = res, 
                     X = X, formula = formula, cov = cov, l = as.vector(pred_local), 
                     mu = as.vector(pred_local - int2), mod_global = mod_global, 
                     newdata = dati.modello[1:nX, ], time = paste0(round(as.numeric(difftime(time1 = time2, 
                                                                                             time2 = time1, units = "mins")), 3), " minutes"))
  }
  else {
    list.obj <- list(IntCoefs = res_global, CovCoefs = res, 
                     X = X, formula = formula, cov = cov, l = as.vector(pred_global), 
                     mu = as.vector(pred_global - int2), mod_global = mod_global, 
                     newdata = dati.modello[1:nX, ], time = paste0(round(as.numeric(difftime(time1 = time2, 
                                                                                             time2 = time1, units = "mins")), 3), " minutes"))
  }
  class(list.obj) <- "stlgcppm"
  return(list.obj)
}
