# Load in packages
library(Rcpp)
library(tidyverse)
library(reshape2)
library(colourlovers)
library(here)

# Import C++ code
sourceCpp(here('content/post/first-art/stepping-stone.cpp'))

# The plot will be a 400 x 400 raster
pixels <- 2000

# Initialization of matrix A:
A <- matrix(0, pixels, pixels) #A is a zero matrix

# Number of water drops in the canvas
ndrops <- sample(10:50, 1)

# Generate drops creating a circle randomly located of radius r  
for (n in 1:ndrops){
  #x <- rbeta(1,shape1 = 0.4, shape2 = 0.6)*pixels 
  x <- runif(1, min = 1, max = pixels)  
  y <- rbeta(1, shape1 = 4, shape2 = 3)*pixels  
  #y <- runif(1, min = 1, max = pixels)  
  r <- runif(1, min = pixels/50, max = pixels/10)
  
  for (i in 1:pixels){
    for (j in 1:pixels){
      #d <- sqrt((i-x)^2+(j-y)^2)
      #d <- max(sqrt((i-x)^2+(j-y)^2),0.01) 
      #d <- max((abs(i-x)+abs(j-y)),0.01) 
      #d <- max(((i-x)+(j-y)),0.01) 
      d <- max(((i-y)+(j-x)),0.01) 
      #if (d < r) A[i,j] <- A[i,j]+rnorm(1, mean = d, sd = d/2)
      if (d < r) A[i,j] <- A[i,j]+rnorm(1, mean = 1/max(d,0.01), sd = d)
      #if (d < r) A[i,j] <- A[i,j]+rt(1,df = d)
    }
  }
}

# Iterations of stepping-stone algorithm
iters <- sample(200:1500, 1)

for (i in 1:iters) A <- iterate_stepping(A)

# Choose a top palette from colourlovers
palette <- sample(clpalettes('top'), 1)[[1]] 
colors <- palette %>% swatch %>% .[[1]]

# Convert matrix B into  data frame preserving indexes
df <- melt(A)

# To name columns
colnames(df) <- c("x","y","c") 

# Do the plot
ggplot(df, aes(x, y, color = c)) + 
  geom_point() + 
  scale_colour_gradientn(colors = colors) +
  coord_equal() +
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_continuous(expand = c(0,0)) +
  theme_void() +
  theme(legend.position='none',
        panel.border = element_rect(color="black", fill = NA)) -> plot

# Do you like it? Save it!
ggsave(here('content/post/first-art/new_trial.png'), plot, height =  6, width =  6)



