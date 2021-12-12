library('ks')
library('rgl')

# load data from csv file
data = read.csv("cr-ti-zr-la.csv", stringsAsFactors=FALSE)
names(data) = c('Type','La', 'Cr', 'Zr', 'Ti')

# m-Na-Al-1 glass
X_soda1 = subset(data, data[,1] == 'm-Na-Al-1')
X_soda1 = data.frame(X_soda1$La, X_soda1$Cr, X_soda1$Zr, X_soda1$Ti) 
names(X_soda1) = c('La', 'Cr', 'Zr', 'Ti')

# convert data into tetrahedron coordinates
# x = (r-l+1)/2
# y = sqrt(3)/2 * t + sqrt(3)/6 * f
# z = sqrt(6)/3 * f
X_soda1['x'] = (X_soda1['La'] + 1 - X_soda1['Cr'])/2
X_soda1['y'] = sqrt(3)/2 * X_soda1['Zr'] + sqrt(3)/6 * X_soda1['Ti']
X_soda1['z'] = sqrt(3)/6 * X_soda1['Ti']
soda1_coord = data.frame(X_soda1$x, X_soda1$y, X_soda1$z)

# m-Na-Al-3 glass
X_soda3 = subset(data, data[,1] == 'm-Na-Al-3')
X_soda3 = data.frame(X_soda3$La, X_soda3$Cr, X_soda3$Zr, X_soda3$Ti) 
names(X_soda3) = c('La', 'Cr', 'Zr', 'Ti')
X_soda3['x'] = (X_soda3['La'] + 1 - X_soda3['Cr'])/2
X_soda3['y'] = sqrt(3)/2 * X_soda3['Zr'] + sqrt(3)/6 * X_soda3['Ti']
X_soda3['z'] = sqrt(3)/6 * X_soda3['Ti']
soda3_coord = data.frame(X_soda3$x, X_soda3$y, X_soda3$z)

# Potash-1 glass
X_Potash1 = subset(data, data[,1] == 'Potash-1')
X_Potash1 = data.frame(X_Potash1$La, X_Potash1$Cr, X_Potash1$Zr, X_Potash1$Ti)
names(X_Potash1) = c('La', 'Cr', 'Zr', 'Ti')
X_Potash1['x'] = (X_Potash1['La'] + 1 - X_Potash1['Cr'])/2
X_Potash1['y'] = sqrt(3)/2 * X_Potash1['Zr'] + sqrt(3)/6 * X_Potash1['Ti']
X_Potash1['z'] = sqrt(3)/6 * X_Potash1['Ti']
Potash1_coord = data.frame(X_Potash1$x, X_Potash1$y, X_Potash1$z)

# Potash-3 glass
X_Potash3 = subset(data, data[,1] == 'Potash-3') 
X_Potash3 = data.frame(X_Potash3$La, X_Potash3$Cr, X_Potash3$Zr, X_Potash3$Ti) 
names(X_Potash3) = c('La', 'Cr', 'Zr', 'Ti')
X_Potash3['x'] = (X_Potash3['La'] + 1 - X_Potash3['Cr'])/2
X_Potash3['y'] = sqrt(3)/2 * X_Potash3['Zr'] + sqrt(3)/6 * X_Potash3['Ti']
X_Potash3['z'] = sqrt(3)/6 * X_Potash3['Ti']
Potash3_coord = data.frame(X_Potash3$x, X_Potash3$y, X_Potash3$z) 

# kernel density estimation (with optimal bandwidth)
fhat_soda1 = kde(x = soda1_coord, H = Hscv(x=soda1_coord, pre='sphere', optim.fun="optim"), compute.cont=TRUE)
fhat_soda3 = kde(x = soda3_coord, H = Hscv(x=soda3_coord, pre='sphere', optim.fun="optim"), compute.cont=TRUE)
fhat_Potash1 = kde(x = Potash1_coord, H = Hscv(x=Potash1_coord, pre='sphere', optim.fun="optim"), compute.cont=TRUE)
fhat_Potash3 = kde(x = Potash3_coord, H = Hscv(x=Potash3_coord, pre='sphere', optim.fun="optim"), compute.cont=TRUE)

# draw tetrahedron in x/y/z coordinates
x_coord <- c(0, 1, 1/2, 1/2)
y_coord <- c(0, 0, sqrt(3)/2, sqrt(3)/6)
z_coord <- c(0,0,0, sqrt(3)/2)
plot3d(x_coord,y_coord,z_coord, add=TRUE)
par3d(cex=2.0)
text3d(x=-0.05,y=0,z=0,texts='Cr', col='black', font=4)
text3d(x=1.05,y=0,z=0, texts='La', col='black', font=4)
text3d(x=0.55,y=sqrt(3)/2,z=0,texts='Zr', col='black', font=4)
text3d(x=1/2,y=sqrt(3)/6,z=sqrt(3)/2 + 0.05,texts='Ti/10', col='black', font=4)

segments3d(x_coord[1:2],y_coord[1:2],z_coord[1:2],col="black",lwd=2, add=TRUE)
segments3d(x_coord[2:3],y_coord[2:3],z_coord[2:3],col="black",lwd=2, add=TRUE)
segments3d(c(0,1/2),c(0, sqrt(3)/2),c(0,0),col="black",lwd=2, add=TRUE)
segments3d(c(0,1/2),c(0, sqrt(3)/6),c(0,sqrt(3)/2),col="black",lwd=2, add=TRUE)
segments3d(c(1,1/2),c(0, sqrt(3)/6),c(0,sqrt(3)/2),col="black",lwd=2, add=TRUE)
segments3d(c(1/2,1/2),c(sqrt(3)/2, sqrt(3)/6),c(0,sqrt(3)/2),col="black",lwd=2, add=TRUE)

# kde contours for each group
plot(fhat_Potash3, col.fun=colorRampPalette(c('grey')), col.pt = 'black',add=TRUE, drawpoints = FALSE, box=FALSE, axes=FALSE,display='rgl', alpha=0.3)
plot(fhat_soda3, col.fun=colorRampPalette(c('red')), col.pt = 'black', add=TRUE, drawpoints = FALSE, box=FALSE, axes=FALSE,col.axis = 'blue',display='rgl', alpha=0.3)
plot(fhat_Potash1, col.fun=colorRampPalette(c('green')), col.pt = 'black', add=TRUE, drawpoints = FALSE, box=FALSE, axes=FALSE,display='rgl', alpha=0.3)
plot(fhat_soda1, col.fun=colorRampPalette(c('blue')), col.pt = 'black',add=TRUE, drawpoints = FALSE, box=FALSE, axes=FALSE,display='rgl', alpha=0.3)

htmlwidgets::saveWidget(rglwidget(width = 1000, height = 1000),"y.html", background = "white")
# save plot
rgl.snapshot(filename='4trace.png', fmt = "png", top = TRUE )
