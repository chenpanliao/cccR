color.dist <- function(
  colorDir1, colorDir2,
  interp = c(300, 700, 0.1),
  diurnal.illum = read.spectra("diurnalIllumination.txt"),
  nocturnal.illum = read.spectra("nocturnalIllumination.txt"),
  bgcolor = read.spectra("envBackground.txt"),
  rec.bee = read.spectra("honeyBeeReceptor.txt", cols = 3),
  rec.moth = read.spectra("hawkmothReceptor.txt", cols = 4),
  rec.bird = read.spectra("bluetitReceptor.txt", cols = 5),
){

  #
  E.color1.bee <- vector("list", 3)
  E.color1.bee[[1]] <- intergral(
    color1$outDataInterp[,1],
    color1$outDataInterp[,2] * diurnal.illum$outdata[,2] * rec.bee[,2]
  )
  P.ij <-

}
