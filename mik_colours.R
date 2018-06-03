library(colourlovers)
palette1 <- clpalette('113451')
palette2 <- clpalette('92095')
palette3 <- clpalette('629637')
palette4 <- clpalette('694737')

top <- clpalettes('top')
# plot all top palettes (interactively)
plot(top)
# plot them all as pie charts of the included colors
plot(top, type='pie')
# extract color swatches from new palettes
par(ask=FALSE)
par(mfrow=c(2,2))for(i in 1:4) plot(top[[i]], type='pie')

par(mfrow=c(4,5))
for(i in 1:20) plot(top[[i]])
