### Ice Cream Tube Lines? ###

### 1. Install packages if needed ###

# install.packages("magick")
library(magick)                            #This R package makes images readable

set.seed(123)

### 2. Load Tube map background ###

tube_img <- image_read("tube.png")         #Renaming .png as a variable name
tube_raster <- as.raster(tube_img)

### 3. Create Tube Line Names for x-axis ###

tube_lines <- c(
  "Bakerloo","Central","Circle","District",
  "Hammersmith","Jubilee",
  "Northern","Piccadilly Circus","Brixton","Waterloo & City",
  "Finsbury Park","Camden Town","Temple", "Stockwell", "Waterloo",
  "Covent Garden", "Elephant & Castle", "High Street Kensington",
  "Hyde Park Corner", "Kings Cross/St Pancras", "Leicester Square",
  "Liverpool Street", "London Bridge", "Stratford", "Victoria", "Westminster"
)

n <- length(tube_lines)                    #Defining the length of x-axis

### 4. Create Fake Data & Empty Plot to Build On ###

y <- runif(300)                            #Fake Data

par(bg = "limegreen")                      #Background color, green because colorblind

plot(1:n, runif(n),                        #Blank plot to build on
     type="n",
     xlim=c(1,n),
     ylim=c(0,1),
     axes=FALSE,
     xlab="LOL",
     ylab="WOW",
     main="Ice Cream Tube Lines?")

### 5. Add Tube Map as Background ###

rasterImage(tube_raster, 1, 0, n, 1)       #Use image variable to add .png image

### 6. Add chaotic points ###

points(sample(1:n, 200, replace=TRUE),     #These are the "sample" points
       runif(200),
       col = rgb(1, 0.5, 0, 0.6),
       pch = 16,
       cex = 1.5)

### 7. Add Tons of Different Lines ###

for(i in 1:40){                            #Dashed Lines
  segments(
    x0 = runif(1, 1, n),
    y0 = runif(1, 0, 1),
    x1 = runif(1, 1, n),
    y1 = runif(1, 0, 1),
    col = sample(c("red","blue","purple","orange"),1),
    lwd = runif(1, 1, 8),
    lty = sample(1:6, 1)
  )
}

### 8. Add Bubbles ###

n_bubbles <- 80                            #Bubble size saved as variable

points(
  runif(n_bubbles, 1, n),
  runif(n_bubbles, 0, 1),
  pch = 21,  # filled circle
  bg = rgb(runif(n_bubbles), runif(n_bubbles), runif(n_bubbles), 0.5),
  col = "red",
  cex = runif(n_bubbles, 2, 10)  
)

### 9. Add messy horizontal lines ###

for(i in 1:10){                            #"Normal" lines
  segments(1, runif(1), n, runif(1),
           col = "blue",
           lwd = 2)
}

### 10. Add Extremely Cluttered x-axis Labels ###

x_positions <- seq(1, 1.5, length.out = n) #Length of x-axis

par(xpd = NA)

for(i in 1:n){                             #For loop so tube line names overlap
  text(x = x_positions[i] + runif(1,-0.02,0.02),
       y = -0.15,
       labels = tube_lines[i],
       srt = sample(c(0,45,90),1),
       cex = 2.5,
       col = "red")                        #Because colorblind
}

### 11. Ugly y-axis ###

axis(2,                                    #Absolutely unrelated y-axis
     at = seq(0,1,0.25),
     col.axis = "red",
     cex.axis = 1.5)

### 12. Completely unrelated legend ###

legend("topright",                         #The legend is obviously not related
       legend = c("Vanilla","Chocolate","Strawberry"),
       col = c("yellow","yellow","yellow"),
       pch = c(16,17,18),
       title = "Ice Cream???",
       cex = 1.5,
       text.col = "yellow",
       bg = "white")                       #And naturally hard to read
