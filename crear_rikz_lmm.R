# ── Ejecutar este script en RStudio dentro del proyecto StatModels ──
# Crea rikz_lmm.rda y lo guarda en inst/app/data/

rikz_lmm <- data.frame(
  Richness = c(11,10,13,11,10,8,9,8,19,17,6,1,4,3,3,1,3,3,1,4,
               3,22,6,0,6,5,4,1,6,4,2,1,1,3,4,3,5,7,5,0,7,11,3,0,2),
  Exposure = c(10,10,10,10,10,8,8,8,8,8,11,11,11,11,11,11,11,11,11,11,
               10,10,10,10,10,11,11,11,11,11,11,11,11,11,11,10,10,10,10,10,
               10,10,10,10,10),
  NAP      = c(0.045,-1.036,-1.336,0.616,-0.684,1.19,0.82,0.635,0.061,-1.334,
               -0.976,1.494,-0.201,-0.482,0.167,1.768,-0.03,0.46,1.367,-0.811,
               1.117,-0.503,0.729,1.627,0.054,-0.578,-0.348,2.222,-0.893,0.766,
               0.883,1.786,1.375,-0.06,0.367,1.671,-0.375,-1.005,0.17,2.052,
               -0.356,0.094,-0.002,2.255,0.865),
  Beach    = factor(rep(1:9, each = 5)),
  Site     = factor(rep(1:5, times = 9))
)

# Guardar en inst/app/data/
save(rikz_lmm,
     file = "inst/app/data/rikz_lmm.rda",
     compress = "xz")

cat("rikz_lmm.rda guardado en inst/app/data/\n")
cat("Dimensiones:", nrow(rikz_lmm), "x", ncol(rikz_lmm), "\n")
