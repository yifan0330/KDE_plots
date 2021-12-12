library('ks')
library('rgl')

# load data from csv file
data = read.csv("Cr-Zr-La.csv", stringsAsFactors=FALSE)
names(data) = c('Type', 'Cr', 'Zr', 'Y', 'La', 'Ti')

# m-Na-Al-1 glass
X_soda1 = subset(data, data[,1] == 'm-Na-Al-1')
X_soda1 = data.frame( X_soda1$Cr, X_soda1$Zr, X_soda1$Y, X_soda1$La, X_soda1$Ti) 
names(X_soda1) = c('Cr', 'Zr', 'Y', 'La', 'Ti')

X_soda1['x'] = X_soda1['Cr']
X_soda1['y'] = X_soda1['Zr'] 
X_soda1['z'] = X_soda1['La']
soda1_coord = data.frame(X_soda1$x, X_soda1$y, X_soda1$z)

# m-Na-Al-3
X_soda3 = subset(data, data[,1] == 'm-Na-Al-3')
X_soda3 = data.frame(X_soda3$Cr, X_soda3$Zr, X_soda3$Y, X_soda3$La, X_soda3$Ti) 
names(X_soda3) = c('Cr', 'Zr', 'Y', 'La', 'Ti')
X_soda3['x'] = X_soda3['Cr']
X_soda3['y'] = X_soda3['Zr']
X_soda3['z'] = X_soda3['La']
soda3_coord = data.frame(X_soda3$x, X_soda3$y, X_soda3$z)

# Potash-1 glass
X_Potash1 = subset(data, data[,1] == 'Potash-1')
X_Potash1 = data.frame(X_Potash1$Cr, X_Potash1$Zr, X_Potash1$Y, X_Potash1$La, X_Potash1$Ti)
names(X_Potash1) = c('Cr', 'Zr', 'Y', 'La', 'Ti')
X_Potash1['x'] = X_Potash1['Cr']
X_Potash1['y'] = X_Potash1['Zr']
X_Potash1['z'] = X_Potash1['La']
Potash1_coord = data.frame(X_Potash1$x, X_Potash1$y, X_Potash1$z)

# Potash-3 glass
X_Potash3 = subset(data, data[,1] == 'Potash-3') 
X_Potash3 = data.frame(X_Potash3$Cr, X_Potash3$Zr, X_Potash3$Y, X_Potash3$La, X_Potash3$Ti) 
names(X_Potash3) = c('Cr', 'Zr', 'Y', 'La', 'Ti')
X_Potash3['x'] = X_Potash3['Cr']
X_Potash3['y'] = X_Potash3['Zr']
X_Potash3['z'] = X_Potash3['La']
Potash3_coord = data.frame(X_Potash3$x, X_Potash3$y, X_Potash3$z) 

# kernel density estimation (with optimal bandwidth)
fhat_soda1 = kde(x = soda1_coord, H = Hscv(x=soda1_coord, pre='sphere', optim.fun="optim"), compute.cont=TRUE)
fhat_soda3 = kde(x = soda3_coord, H = Hscv(x=soda3_coord, pre='sphere', optim.fun="optim"), compute.cont=TRUE)
fhat_Potash1 = kde(x = Potash1_coord, H = Hscv(x=Potash1_coord, pre='sphere', optim.fun="optim"), compute.cont=TRUE)
fhat_Potash3 = kde(x = Potash3_coord, H = Hscv(x=Potash3_coord, pre='sphere', optim.fun="optim"), compute.cont=TRUE)

rglwidget()
par3d(cex=1)
# kde contours for each group 
plot(fhat_Potash3, col.fun=colorRampPalette(c('grey')), col.pt = 'grey',add=TRUE, drawpoints = FALSE, box=FALSE, axes=FALSE,display='rgl', alpha=0.3)
plot(fhat_soda1, col.fun=colorRampPalette(c('yellow')), col.pt = 'blue',add=TRUE, drawpoints = FALSE, box=FALSE, axes=FALSE,display='rgl', alpha=0.3)
plot(fhat_soda3, col.fun=colorRampPalette(c('red')), col.pt = 'red', add=TRUE, drawpoints = FALSE, box=FALSE, axes=FALSE,col.axis = 'blue',display='rgl', alpha=0.3)
plot(fhat_Potash1, col.fun=colorRampPalette(c('green')), col.pt = 'green', add=TRUE, drawpoints = FALSE, box=FALSE, axes=FALSE,display='rgl', alpha=0.3)
rgl.bbox(color=c("grey","black"), emission="#333377", specular="#3333FF", shininess=5, alpha=0.8 )
htmlwidgets::saveWidget(rglwidget(width = 1000, height = 1000),"figure.html")
#axis label
title3d(xlab = "Cr", ylab = "Zr", zlab = "La")
# save plot
rgl.snapshot(filename='3trace.png', fmt = "png", top = TRUE )
