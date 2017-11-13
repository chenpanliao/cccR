honeybeeCC <-
  function(obj1,
           obj2,
           illumination,
           envbg,
           sens.UVS,
           sens.MWS,
           sens.SWS) {
    k.UVS <-
      1 / intergral(seq(300, 700, 0.1), sens.UVS * envbg * illumination)
    k.SWS <-
      1 / intergral(seq(300, 700, 0.1), sens.SWS * envbg * illumination)
    k.MWS <-
      1 / intergral(seq(300, 700, 0.1), sens.MWS * envbg * illumination)
    
    Q.UVS.obj1 <-
      intergral(seq(300, 700, 0.1), sens.UVS * obj1 * illumination)
    Q.SWS.obj1 <-
      intergral(seq(300, 700, 0.1), sens.SWS * obj1 * illumination)
    Q.MWS.obj1 <-
      intergral(seq(300, 700, 0.1), sens.MWS * obj1 * illumination)
    Q.UVS.obj2 <-
      intergral(seq(300, 700, 0.1), sens.UVS * obj2 * illumination)
    Q.SWS.obj2 <-
      intergral(seq(300, 700, 0.1), sens.SWS * obj2 * illumination)
    Q.MWS.obj2 <-
      intergral(seq(300, 700, 0.1), sens.MWS * obj2 * illumination)
    
    q.UVS.obj1 <- k.UVS * Q.UVS.obj1
    q.SWS.obj1 <- k.SWS * Q.SWS.obj1
    q.MWS.obj1 <- k.MWS * Q.MWS.obj1
    q.UVS.obj2 <- k.UVS * Q.UVS.obj2
    q.SWS.obj2 <- k.SWS * Q.SWS.obj2
    q.MWS.obj2 <- k.MWS * Q.MWS.obj2
    
    e.UVS.obj1 <- q.UVS.obj1 / (q.UVS.obj1 + 1)
    e.SWS.obj1 <- q.SWS.obj1 / (q.SWS.obj1 + 1)
    e.MWS.obj1 <- q.MWS.obj1 / (q.MWS.obj1 + 1)
    e.UVS.obj2 <- q.UVS.obj2 / (q.UVS.obj2 + 1)
    e.SWS.obj2 <- q.SWS.obj2 / (q.SWS.obj2 + 1)
    e.MWS.obj2 <- q.MWS.obj2 / (q.MWS.obj2 + 1)
    
    x.obj1 <- sin(pi / 3) * (e.MWS.obj1 - e.UVS.obj1)
    x.obj2 <- sin(pi / 3) * (e.MWS.obj2 - e.UVS.obj2)
    y.obj1 <- e.SWS.obj1 - 0.5 * (e.UVS.obj1 + e.MWS.obj1)
    y.obj2 <- e.SWS.obj2 - 0.5 * (e.UVS.obj2 + e.MWS.obj2)
    
    CC.chromatic <-
      sqrt((x.obj1 - x.obj2) ^ 2 + (y.obj1 - y.obj2) ^ 2)
    CC.achromatic.diff <-
      abs(e.MWS.obj1 - e.MWS.obj2)
    CC.achromatic.ratio <-
      e.MWS.obj1 / e.MWS.obj2
    
    return(
      c(
        CC.chro = S2.chromatic,
        CC.achro.diff = S.achromatic.diff,
        CC.achro.ratio = CC.achro.ratio
      )
    )
  }