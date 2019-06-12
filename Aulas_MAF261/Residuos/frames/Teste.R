#number of frames or plots
frames <- 50

# function for creating file name with leading zeros
# makes it easier to process them sequentially
rename <- function(x){
  if (x < 10) {
    return(name <- paste('000',i,'plot.png',sep=''))
  }
  if (x < 100 && i >= 10) {
    return(name <- paste('00',i,'plot.png', sep=''))
  }
  if (x >= 100) {
    return(name <- paste('0', i,'plot.png', sep=''))
  }
}

#loop through plots
for(i in 1:frames){
  name <- rename(i)
  
  #saves the plot as a .png file in the working directory
  png(name)
  sd <- 10
  n  <- 10000
  factor <- i * 2
  m  <- 50 + factor
  x  <- rnorm(n, m, sd)
  hist(x,
       xlim=c(0,200),
       ylim=c(0,2000),
       main = paste('Histogram of rnorm() n = ', n, ' mean = ', m, ' sd = ', sd)
  )
  dev.off()
}

#run ImageMagick
my_command <- 'convert *.png -delay 3 -loop 0 animation.gif'
system(my_command)














setwd("E:/Documentos/GitHub/MAF261/maf261.github.io/Aulas_MAF261/Residuos/frames")

# example 1: simple animated countdown from 10 to "GO!".
png(file="example%02d.png", width=200, height=200)
for (i in c(10:1, "G0!")){
  plot.new()
  text(.5, .5, i, cex = 6)
}
dev.off()

# convert the .png files to one .gif file using ImageMagick. 
# The system() function executes the command as if it was done
# in the terminal. the -delay flag sets the time between showing
# the frames, i.e. the speed of the animation.
system('"C:\\Program Files\\ImageMagick-7.0.8-Q16\\convert.exe" -delay 20 -loop 0 files_*.png animation.gif')
# to not leave the directory with the single jpeg files
# I remove them.
file.remove(list.files(pattern=".png"))
