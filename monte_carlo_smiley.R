# Clear workspace
# ------------------------------------------------------------------------------
rm(list=ls())

# Define functions
# ------------------------------------------------------------------------------
mk_hit_matrix = function(x_min,x_max,y_min,y_max,reps,f){
  x0  = runif(n = reps, min = x_min, max = x_max)
  y0  = runif(n = reps, min = y_min, max = y_max)
  hit = ifelse(x_min <= x0 & x0 <= x_max & f(x0) <= y0 & y0 <= 0,1,0)
  out = data.frame(x0=x0,y0=y0,hit=hit)
  out = out[order(out[,1]),]
  rownames(out) = NULL
  return(out)
}
f = function(x){
  return(x ** 2 - 4)
}

# Settings
# ------------------------------------------------------------------------------
set.seed(8720221)
x_min = -3
x_max = 3
y_min = -6
y_max = 4
reps  = 1e5

# Run simulations
# ------------------------------------------------------------------------------
hit_mat     = mk_hit_matrix(x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,reps=reps,f=f)
total_area  = (x_max - x_min) * (y_max - y_min)
smiley_area = total_area * mean(hit_mat$hit)
L           = c(-2,2)
actual_area = (1/3 * L[1] ** 3 - 4 * L[1]) - (1/3 * L[2] ** 3 - 4 * L[2])

# Display results on stdout
# ------------------------------------------------------------------------------
cat("Area calculated by simulations = ",smiley_area,"\n",sep='')
cat("Actual area = ",actual_area,"\n",sep='')

# Plot smiley
# ------------------------------------------------------------------------------
par(pty='s')
plot(hit_mat$x0,f(hit_mat$x0),xlim=c(x_min,x_max),ylim=c(y_min,y_max),
     xlab='',ylab='',bty='n',type="n")
points(hit_mat$x0,hit_mat$y0,col=ifelse(hit_mat$hit==1,1,7),pch=19,cex=0.5)
