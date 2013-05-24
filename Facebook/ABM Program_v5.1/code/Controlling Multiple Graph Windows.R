dev.off()
dev.off()

windows(width = 7, height = 3, xpos = -5, ypos = 5)
windows(width = 7, height = 3, xpos = -5, ypos = 325)

x <<- c(1:100)

y <<- cos(x)+x

y2 <<- 100 + sin(x) - x

for (i in 1:100) {
	if (i == 1) {
		z1 <<- data.frame(x = x[1], y = y[1])
		z2 <<- data.frame(x = x[1], y = y2[2])
		dev.set(2)
		plot(z1$x, z1$y, col = "red", type = "l", lwd = 1)
		dev.set(3)
		plot(z2$x, z2$y, col = "blue", type = "l", lwd = 1)
	} else {
		tmp1 <<- data.frame(x = x[i], y = y[i])
		tmp2 <<- data.frame(x = x[i], y = y2[i])
		z1 <<- rbind(z1, tmp1)
		z2 <<- rbind(z2, tmp2)
		dev.set(2)
		plot(z1$x, z1$y, col = "red", type = "l", lwd = 1)
		dev.set(3)
		plot(z2$x, z2$y, col = "blue", type = "l", lwd = 1)

	}
}

