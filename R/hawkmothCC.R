hawkmothCC <-
  function(obj1,
           obj2,
           illumination,
           envbg,
           sens.UVS,
           sens.SWS,
           sens.MWS,
           sens.MWSalong,
           n = 568,
           delta.rho = 3,
           D = 29,
           kappa = 0.5,
           tau = 0.8,
           delta.t = 0.036,
           k = 0.0067,
           l = 414) {
    N.obj1.UVS <-
      1.13 * pi / 4 * n * delta.rho ^ 2 * D ^ 2 * delta.t *
      intergral(seq(300, 700, 0.1), kappa * tau *
                  (1 - 1 / exp(k * sens.UVS * l)) * illumination * obj1)
    N.obj1.SWS <-
      1.13 * pi / 4 * n * delta.rho ^ 2 * D ^ 2 * delta.t *
      intergral(seq(300, 700, 0.1), kappa * tau *
                  (1 - 1 / exp(k * sens.SWS * l)) * illumination * obj1)
    N.obj1.MWS <-
      1.13 * pi / 4 * n * delta.rho ^ 2 * D ^ 2 * delta.t *
      intergral(seq(300, 700, 0.1), kappa * tau *
                  (1 - 1 / exp(k * sens.MWS * l)) * illumination * obj1)
    N.obj1.MWSalong <-
      1.13 * pi / 4 * n * delta.rho ^ 2 * D ^ 2 * delta.t *
      intergral(seq(300, 700, 0.1), kappa * tau *
                  (1 - 1 / exp(k * sens.MWSalong * l)) * illumination * obj1)
    N.obj2.UVS <-
      1.13 * pi / 4 * n * delta.rho ^ 2 * D ^ 2 * delta.t *
      intergral(seq(300, 700, 0.1), kappa * tau *
                  (1 - 1 / exp(k * sens.UVS * l)) * illumination * obj2)
    N.obj2.SWS <-
      1.13 * pi / 4 * n * delta.rho ^ 2 * D ^ 2 * delta.t *
      intergral(seq(300, 700, 0.1), kappa * tau *
                  (1 - 1 / exp(k * sens.SWS * l)) * illumination * obj2)
    N.obj2.MWS <-
      1.13 * pi / 4 * n * delta.rho ^ 2 * D ^ 2 * delta.t *
      intergral(seq(300, 700, 0.1), kappa * tau *
                  (1 - 1 / exp(k * sens.MWS * l)) * illumination * obj2)
    N.obj2.MWSalong <-
      1.13 * pi / 4 * n * delta.rho ^ 2 * D ^ 2 * delta.t *
      intergral(seq(300, 700, 0.1), kappa * tau *
                  (1 - 1 / exp(k * sens.MWSalong * l)) * illumination * obj2)
    
    N.bg.UVS <-
      1.13 * pi / 4 * n * delta.rho ^ 2 * D ^ 2 * delta.t *
      intergral(seq(300, 700, 0.1), kappa * tau *
                  (1 - 1 / exp(k * sens.UVS * l)) * illumination * envgb)
    N.bg.SWS <-
      1.13 * pi / 4 * n * delta.rho ^ 2 * D ^ 2 * delta.t *
      intergral(seq(300, 700, 0.1), kappa * tau *
                  (1 - 1 / exp(k * sens.SWS * l)) * illumination * envgb)
    N.bg.MWS <-
      1.13 * pi / 4 * n * delta.rho ^ 2 * D ^ 2 * delta.t *
      intergral(seq(300, 700, 0.1), kappa * tau *
                  (1 - 1 / exp(k * sens.MWS * l)) * illumination * envgb)
    N.bg.MWSalong <-
      1.13 * pi / 4 * n * delta.rho ^ 2 * D ^ 2 * delta.t *
      intergral(seq(300, 700, 0.1), kappa * tau *
                  (1 - 1 / exp(k * sens.MWSalong * l)) * illumination * envgb)
    
    q.obj1.denom <- (N.obj1.UVS + N.obj1.SWS + N.obj1.MWS)
    q.obj1.UVS <- N.obj1.UVS / q.obj1.denom
    q.obj1.SWS <- N.obj1.SWS / q.obj1.denom
    q.obj1.MWS <- N.obj1.MWS / q.obj1.denom
    q.obj2.denom <- (N.obj2.UVS + N.obj2.SWS + N.obj2.MWS)
    q.obj2.UVS <- N.obj2.UVS / q.obj2.denom
    q.obj2.SWS <- N.obj2.SWS / q.obj2.denom
    q.obj2.MWS <- N.obj2.MWS / q.obj2.denom
    
    qcc.obj1.denom <-
      (N.obj1.UVS / N.bg.UVS + N.obj1.SWS / N.bg.SWS + N.obj1.MWS / N.bg.MWS)
    qcc.obj1.UVS <- N.obj1.UVS / N.bg.UVS / qcc.denom
    qcc.obj1.SWS <- N.obj1.SWS / N.bg.SWS / qcc.denom
    qcc.obj1.MWS <- N.obj1.MWS / N.bg.MWS / qcc.denom
    qcc.obj2.denom <-
      (N.obj2.UVS / N.bg.UVS + N.obj2.SWS / N.bg.SWS + N.obj2.MWS / N.bg.MWS)
    qcc.obj2.UVS <- N.obj2.UVS / N.bg.UVS / qcc.denom
    qcc.obj2.SWS <- N.obj2.SWS / N.bg.SWS / qcc.denom
    qcc.obj2.MWS <- N.obj2.MWS / N.bg.MWS / qcc.denom
    
    x.obj1 <- 1 / sqrt(2) * (q.obj1.MWS - q.obj1.SWS)
    xcc.obj1 <- 1 / sqrt(2) * (qcc.obj1.MWS - qcc.obj1.SWS)
    x.obj2 <- 1 / sqrt(2) * (q.obj1.MWS - q.obj1.SWS)
    xcc.obj1 <- 1 / sqrt(2) * (qcc.obj2.MWS - qcc.obj2.SWS)
    
    y.obj1 <-
      sqrt(2) / sqrt(3) * (q.obj1.UVS - (q.obj1.MWS + q.obj1.SWS) / 2)
    ycc.obj1 <-
      sqrt(2) / sqrt(3) * (qcc.obj1.UVS - (qcc.obj1.MWS + qcc.obj1.SWS) / 2)
    y.obj2 <-
      sqrt(2) / sqrt(3) * (q.obj2.UVS - (q.obj2.MWS + q.obj2.SWS) / 2)
    ycc.obj1 <-
      sqrt(2) / sqrt(3) * (qcc.obj2.UVS - (qcc.obj2.MWS + qcc.obj2.SWS) / 2)
    
    CC.chromatic <- sqrt((x.obj1 - x.obj2) ^ 2 + (y.obj1 - y.obj2) ^ 2)
    CCcc.chromatic <- sqrt((xcc.obj1 - xcc.obj2) ^ 2 + (ycc.obj1 - ycc.obj2) ^ 2)
    CC.achromatic <- (N.obj1.MWSalong - N.obj2.MWSalong) / (N.obj1.MWSalong + N.obj2.MWSalong)

    return(
      c(
        CC.chro = CC.chromatic,
        CC.colorConstancy.chro = CCcc.chromatic,
        CC.achro.diff = CC.achromatic
      )
    )
  }
