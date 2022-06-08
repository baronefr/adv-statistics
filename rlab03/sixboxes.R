#!/usr/bin/Rscript

################################################################
#  AdvStat4PhysAna - RLab 3
#  coded by Barone Francesco Pio, student ID: 2058478
#  University of Padua, 15 april 2022
#  
#  This is the script for manual six boxes sampling experiment.
#  Run this script with   Rscript sixboxes.R
#
################################################################


# six boxes toy model options
nboxes <- 6  # number of boxes  (I tested it for 6, but should work fine for other numbers)
nextr <- 20  # number of extractions (you can extend it!)

nextr_grow <- 10 # how much you want to grow the number of extractions when you are asked




# plot environment options
X11()    # for graphical interface in Linux
options(repr.plot.width=8, repr.plot.height=5)  # adjust as you wish
par(mar = c(2.4,2,3,2))  # tight layout




# +------------------+
# | Useful functions |
# +------------------+

#  Preallocate matrix & init probability values
prb <- matrix(0, nboxes, nextr+1)
prb[,1] <- 1/nboxes                   # initial probability of each box
prbH <- ((1:nboxes) -1)*1/(nboxes-1)  # probability of white for each box

#  This function implements the Bayes formula on a vector of probabilities prb,
#  given the latest extraction (False for Black, True for White).
bayes <- function(extr, prb) {
    if(extr) { mult <- prbH }   # White
    else     { mult <- 1-prbH } # Black
    return(mult*prb/sum(mult*prb))
}

#  This function takes a matrix of probabilities and plots them for each box.
# The argument np selects the number of extracted points to plot.
plot_probabilities <- function(mprb, np=nextr) {
    
    par( mfrow=c( ceiling(nboxes/3), 3) )  # divide in 3 columns
    
    for(box in 1:nboxes) {
        this_prb = mprb[box,1:(np+1)]
        this_prb
        plot( 0:np, this_prb, main=paste0('H', box-1),
             pch=16, xlim=c(0,nextr), ylim=c(0,1),
             xlab="extraction", ylab="probability",
             col = ifelse(this_prb==0,'grey',"#0066FF") )
             # the color becomes grey when probability is null
    }
}



# +-----------------------+
# | Printing instructions |
# +-----------------------+


# printing some instructions

instructions <- paste0(' Write \'w\' for a white ball, \'b\' for a black ball.\n',
                ' You can insert capital letters, \'white\' or \'black\' as well.\n\n',
                ' Prompting \'q\' will stop the input process.\n\n');
                
cat('++++ Six boxes model ++++\n\n')
cat(instructions)

plot.new()  #plotting a dummy txt with instructions
text(0.5, 0.5,  'Insert the extracted values in the prompt', cex=1.7, pos=3)
text(0.5, 0.25,  instructions, cex=1, pos=3)



# +------------------------+
# | Interactive extraction |
# +------------------------+

i <- 1  # counts how many correct extraction have been inserted
while(i <= nextr) {
    
    # take the extracted value from stdin
    cat(' > ');    extr <- readLines("stdin",1);
    
    # parsing the input...
    if( extr=='W' | extr=='w' | extr=='white' | extr=='White')
        { extr <- TRUE  }
    else if( extr=='B' | extr=='b' | extr=='black' | extr=='Black')
        { extr <- FALSE }
        
    # ... and managing some errors & special inputs 
    else if( extr=='q' | extr=='quit')
         { cat('[INFO] quit from input mode\n\n');    break; }
    else { cat('[INFO] invalid input, try again\n');  next;  }
    
    # computing the probabilities & updating the plot
    prb[,i+1] <- bayes(extr, prb[,i])
    cat( paste(ifelse(extr,'White','Black')), '| ')
    cat(prb[,i+1], '\n')
    plot_probabilities(prb, i)
    
    # moving to next iteration
    i <- i+1
    if(i>nextr) {   # I give the chance to extend the number of inputs
        cat('\nYou have reached the maximum number of inputs.\nYou wish to continue? [y for \'YES\'] > ')
        todo <- readLines("stdin",1);
        if( todo=='y' | extr=='yes' | extr=='Y' | extr=='Yes') {
          nextr <- nextr + nextr_grow
          prb <- cbind(prb, matrix(0, nboxes, nextr_grow))
          # I grow the matrix if need for more inputs
          cat('\n')
        }
        else { cat('I\'ll take that as a no.\n\n') }
    }
}

cat( paste0('The most probable box (by inference) is H',
            which.max(prb[,i])-1, ', with probability ',
            max(prb[,i]), '%.\n\n') )
            
cat('press [enter] to exit\n')
invisible(readLines("stdin",1))

quit(status=0)
