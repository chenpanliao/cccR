## unit of spectrum need to be 0-1 not 0-100%

lizardCC <-
  function(obj1,
           obj2,
           illumination,
           envbg,
           sens.UVS,
           sens.SWS,
           sens.MWS,
           sens.LWS,
           # number of cones per ganglion cell field
           cellRatio.UVS = 1,
           cellRatio.SWS = 1,
           cellRatio.MWS = 1,
           cellRatio.LWS = 3,
           # irradiance (μmol m-2 s-1) normal to the surface of the eye
           irradianceIntensity = 25,
           # acceptance angle of individual photoreceptor
           deltaR = 1.2 / 3000,
           # pupil diameter; 1mm based on Underwood (1970)
           d = 1000,
           # outer segment length
           # 8 μm for m and l cones and 7 μm for s and u cones
           L.UVS = 7,
           L.SWS = 7,
           L.MWS = 8,
           L.LWS = 8,
           weberFraction.UVS = 0.05,
           weberFraction.SWS = 0.05,
           weberFraction.MWS = 0.05,
           weberFraction.LWS = 0.05) {
    # temporal integration
    temporalIntegration <- 3.5 * (log10(irradianceIntensity) + 14.5)
    # optical density if pigment type 𝒊 as a function of wavelength
    mu.UVS <- sens.UVS * 0.015
    mu.SWS <- sens.SWS * 0.015
    mu.MWS <- sens.MWS * 0.015
    mu.LWS <- sens.LWS * 0.015
    
    S.UVS <-
      cellRatio.UVS * temporalIntegration * (pi / 4) ^ 2 * deltaR ^ 2 * d ^ 2 *
      (1 - exp(-1 * mu.UVS * L.UVS))
    S.SWS <-
      cellRatio.SWS * temporalIntegration * (pi / 4) ^ 2 * deltaR ^ 2 * d ^ 2 *
      (1 - exp(-1 * mu.SWS * L.SWS))
    S.MWS <-
      cellRatio.MWS * temporalIntegration * (pi / 4) ^ 2 * deltaR ^ 2 * d ^ 2 *
      (1 - exp(-1 * mu.MWS * L.MWS))
    S.LWS <-
      cellRatio.LWS * temporalIntegration * (pi / 4) ^ 2 * deltaR ^ 2 * d ^ 2 *
      (1 - exp(-1 * mu.LWS * L.LWS))
    
    k.UVS <-
      1 / intergral(seq(300, 700, 0.1), S.UVS * envbg * illumination)
    k.SWS <-
      1 / intergral(seq(300, 700, 0.1), S.SWS * envbg * illumination)
    k.MWS <-
      1 / intergral(seq(300, 700, 0.1), S.MWS * envbg * illumination)
    k.LWS <-
      1 / intergral(seq(300, 700, 0.1), S.LWS * envbg * illumination)
    
    Q.UVS.obj1 <-
      intergral(seq(300, 700, 0.1), S.UVS * obj1 * illumination)
    Q.SWS.obj1 <-
      intergral(seq(300, 700, 0.1), S.SWS * obj1 * illumination)
    Q.MWS.obj1 <-
      intergral(seq(300, 700, 0.1), S.MWS * obj1 * illumination)
    Q.LWS.obj1 <-
      intergral(seq(300, 700, 0.1), S.LWS * obj1 * illumination)
    Q.UVS.obj2 <-
      intergral(seq(300, 700, 0.1), S.UVS * obj2 * illumination)
    Q.SWS.obj2 <-
      intergral(seq(300, 700, 0.1), S.SWS * obj2 * illumination)
    Q.MWS.obj2 <-
      intergral(seq(300, 700, 0.1), S.MWS * obj2 * illumination)
    Q.LWS.obj2 <-
      intergral(seq(300, 700, 0.1), S.LWS * obj2 * illumination)
    
    # rhoQ.UVS.obj1 <-
    #   sqrt(Q.UVS.obj1 + weberFraction.UVS ^ 2 * Q.UVS.obj1 ^ 2)
    # rhoQ.SWS.obj1 <-
    #   sqrt(Q.SWS.obj1 + weberFraction.SWS ^ 2 * Q.SWS.obj1 ^ 2)
    # rhoQ.MWS.obj1 <-
    #   sqrt(Q.MWS.obj1 + weberFraction.MWS ^ 2 * Q.MWS.obj1 ^ 2)
    # rhoQ.LWS.obj1 <-
    #   sqrt(Q.LWS.obj1 + weberFraction.LWS ^ 2 * Q.LWS.obj1 ^ 2)
    # rhoQ.UVS.obj2 <-
    #   sqrt(Q.UVS.obj2 + weberFraction.UVS ^ 2 * Q.UVS.obj2 ^ 2)
    # rhoQ.SWS.obj2 <-
    #   sqrt(Q.SWS.obj2 + weberFraction.SWS ^ 2 * Q.SWS.obj2 ^ 2)
    # rhoQ.MWS.obj2 <-
    #   sqrt(Q.MWS.obj2 + weberFraction.MWS ^ 2 * Q.MWS.obj2 ^ 2)
    # rhoQ.LWS.obj2 <-
    #   sqrt(Q.LWS.obj2 + weberFraction.LWS ^ 2 * Q.LWS.obj2 ^ 2)
    
    # N.UVS.obj1 <- rhoQ.UVS.obj1 / Q.UVS.ob1
    # N.SWS.obj1 <- rhoQ.SWS.obj1 / Q.SWS.ob1
    # N.MWS.obj1 <- rhoQ.MWS.obj1 / Q.MWS.ob1
    # N.LWS.obj1 <- rhoQ.LWS.obj1 / Q.LWS.ob1
    # N.UVS.obj2 <- rhoQ.UVS.obj2 / Q.UVS.ob2
    # N.SWS.obj2 <- rhoQ.SWS.obj2 / Q.SWS.ob2
    # N.MWS.obj2 <- rhoQ.MWS.obj2 / Q.MWS.ob2
    # N.LWS.obj2 <- rhoQ.LWS.obj2 / Q.LWS.ob2
    
    q.UVS.obj1 <- k.UVS * Q.UVS.obj1
    q.SWS.obj1 <- k.SWS * Q.SWS.obj1
    q.MWS.obj1 <- k.MWS * Q.MWS.obj1
    q.LWS.obj1 <- k.LWS * Q.LWS.obj1
    q.UVS.obj2 <- k.UVS * Q.UVS.obj2
    q.SWS.obj2 <- k.SWS * Q.SWS.obj2
    q.MWS.obj2 <- k.MWS * Q.MWS.obj2
    q.LWS.obj2 <- k.LWS * Q.LWS.obj2
    
    # relative stimulation
    # RS.UVS.obj1 <- q.UVS.obj1 / (q.UVS.obj1 + q.SWS.obj1 + q.MWS.obj1 + q.LWS.obj1)
    # RS.SWS.obj1 <- q.SWS.obj1 / (q.UVS.obj1 + q.SWS.obj1 + q.MWS.obj1 + q.LWS.obj1)
    # RS.MWS.obj1 <- q.MWS.obj1 / (q.UVS.obj1 + q.SWS.obj1 + q.MWS.obj1 + q.LWS.obj1)
    # RS.LWS.obj1 <- q.LWS.obj1 / (q.UVS.obj1 + q.SWS.obj1 + q.MWS.obj1 + q.LWS.obj1)
    # RS.UVS.obj2 <- q.UVS.obj2 / (q.UVS.obj2 + q.SWS.obj2 + q.MWS.obj2 + q.LWS.obj2)
    # RS.SWS.obj2 <- q.SWS.obj2 / (q.UVS.obj2 + q.SWS.obj2 + q.MWS.obj2 + q.LWS.obj2)
    # RS.MWS.obj2 <- q.MWS.obj2 / (q.UVS.obj2 + q.SWS.obj2 + q.MWS.obj2 + q.LWS.obj2)
    # RS.LWS.obj2 <- q.LWS.obj2 / (q.UVS.obj2 + q.SWS.obj2 + q.MWS.obj2 + q.LWS.obj2)
    
    # noise
    noise.UVS <- weberFraction.UVS / sqrt(cellRatio.UVS)
    noise.SWS <- weberFraction.SWS / sqrt(cellRatio.SWS)
    noise.MWS <- weberFraction.MWS / sqrt(cellRatio.MWS)
    noise.LWS <- weberFraction.LWS / sqrt(cellRatio.LWS)
    
    # difference of photon capture
    deltaF.UVS <- log(q.UVS.obj1 / q.UVS.obj2)
    deltaF.SWS <- log(q.SWS.obj1 / q.SWS.obj2)
    deltaF.MWS <- log(q.MWS.obj1 / q.MWS.obj2)
    deltaF.LWS <- log(q.LWS.obj1 / q.LWS.obj2)
    
    # Tetrachromat JND^2
    S2.chromatic <-
      (
        (noise.UVS * noise.SWS) ^ 2 * (deltaF.LWS - deltaF.MWS) ^ 2 +
          (noise.UVS * noise.MWS) ^ 2 * (deltaF.LWS - deltaF.SWS) ^ 2 +
          (noise.UVS * noise.LWS) ^ 2 * (deltaF.MWS - deltaF.SWS) ^ 2 +
          (noise.SWS * noise.MWS) ^ 2 * (deltaF.LWS - deltaF.UVS) ^ 2 +
          (noise.UVS * noise.LWS) ^ 2 * (deltaF.MWS - deltaF.UVS) ^ 2 +
          (noise.MWS * noise.LWS) ^ 2 * (deltaF.SWS - deltaF.UVS) ^ 2
      ) / (
        (noise.UVS * noise.SWS * noise.MWS) ^ 2 +
          (noise.UVS * noise.SWS * noise.LWS) ^ 2 +
          (noise.UVS * noise.MWS * noise.LWS) ^ 2 +
          (noise.SWS * noise.MWS * noise.LWS) ^ 2
      )
    S.achromatic <- abs(deltaF.LWS / noise.LWS)
    c(JND.chro = sqrt(S2.chromatic), JND.achro = S.achromatic)
  }
