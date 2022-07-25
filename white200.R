library(dplyr)
par(family= "HiraKakuProN-W3") # to put Japanese on figure

# make RGB colors
palette_mono <- data.frame(numbers=255:246, keys=rep(0, 10))
palette_bi <- left_join(palette_mono, palette_mono, by = "keys")
palette_tri <-left_join(palette_mono, palette_bi, by = 'keys')

palette <- 
  palette_tri %>%
  select(R=numbers.x, G=numbers.y, B=numbers)

# arrange in order of proximity to (R, G, B) = (255, 255, 255)
ds_sq <- c()
for (i in 1:nrow(palette)) {
  d_sq = (palette[[1]][i])^2 + (palette[[2]][i])^2 + (palette[[3]][i])^2
  ds_sq <- c(ds_sq, d_sq)
}
palette <-
  palette %>%
  mutate(SquaredDistance=ds_sq)
palette <- palette[order(palette$SquaredDistance, decreasing=T), ]

anmika <- function(n=200) {
  
  # decide width and height to plot points
  a = as.integer(sqrt(n))
  while (0 < a) {
    b = n / a
    if (b %% 1 == 0) {
      break
    }
    a <- a - 1
  }
  height = a
  width = b
  x <- 1:width
  y <- rep(1:height, width/height)
  coordinates = expand.grid(x, y)
  coordinates_random = sample_n(tbl = coordinates, size = nrow(coordinates))
  
  for (i in 1:n) {
    # convert decimal into hexadecimal
    ffs = c()
    for (j in 1:3) {
      ff = sprintf("%02X", palette[[j]][i])
      ffs = c(ffs, ff)
    }
    RGB = paste0('#', ffs[1], ffs[2], ffs[3])
    # plot
    plot(x = coordinates_random$Var1,
         y = coordinates_random$Var2,
         col = RGB, bg = RGB, pch = 15,
         xlim = c(0, width), ylim = c(0, height),
         xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    par(new=T)
  }
  title(paste0('白って', n, '色あんねん'))
  # output .png
  dev.copy(png, file = paste0("white", n, ".png"))
  dev.off()
}

input <- readline("Enter any number (1~1000): ")
anmika(as.integer(input))
