library(markovchain)
library(diagram)
library(ggplot2)
library(stats)

#Set up the transition matrix
TM <- matrix(c(0.82,0.18,0.00,0.00,0.47,0.53,0.10,0.00,0.90), nrow=3, ncol=3, byrow=T)
TM <- as.data.frame(TM)
states <- c("S","I", "R")
dimnames(TM) <- list(states, states)
TM

#Plotting of the transition matrix
plotmat(TM, pos=c(1,2), lwd = 1, box.lwd = 1, cex.txt = 0.5, box.size = 0.1, box.type = "circle", 
        box.prop = 0.5, box.col = "light yellow", arr.length = 0.2, arr.width = 0.2, 
        self.cex = 0.4,self.shifty = -0.05, self.shiftx = 0.13, main = "Transition Diagram")

          
#Markovchain building
MC<-new("markovchain", states=states, transitionMatrix=
           matrix(c(0.82,0.18,0.00,0.00,0.47,0.53,0.10,0.00,0.90), nrow=3, 
                  byrow=TRUE, dimnames=list(states, states)))

plot_data <- c()
for (i in c(1:35)){
  x <- MC^i
  print(x[i])
  plot_data[i] <- x[i]
}
plot_data


#Building an stationary MC probability distribution matrix
v1 <- steadyStates(MC)


#The distributions of individuals when the MC becomes stationary will be
n_individuals10000 <- steady_V*10000
n_individuals10000




#*****SIMULATION*******

#Set the seed to create consistent random number
set.seed(1)

#Random state picker for the Markov Chain
state_picker<-function(p){
  cs <-cumsum(p) - runif(1,min = 0,max = 1)
  min(which(cs>0))
}


#Set the transition matrix again as the function works only with a matrix and not a df
TM <- matrix(c(0.82,0.18,0.00,0.00,0.47,0.53,0.10,0.00,0.90), nrow=3, ncol=3, byrow=T)

#Set the n_people and the desired n_iterations (which is necessary to reach the stationary distribution)
n_people <- 10000
n_iterations <- 75

peopleM<-matrix(1,n_people,1)
iterationsM = matrix (nrow=n_iterations, ncol=3)

#Set the initial state
iterationsM[1,] = c(n_people, 0,0)

#MarkovChain function 
for (iter in c(2:n_iterations)){
  for(person in c(1:n_people)){
    if (peopleM[person]==1) 
      nrow<-1
    else if (peopleM[person]==2)
      nrow<-2
    else if (peopleM[person]==3)
      nrow<-3
    
    peopleM[person]<-state_picker(TM[nrow,])
  } 
  iterationsM[iter,] =  c(sum(peopleM==1),sum(peopleM==2),sum(peopleM ==3))
}

#Return the distribution of individuals at the last iteration made
v2 <- iterationsM[n_iterations,]
v2

#Plotting and saving
iterationsM_df <- as.data.frame(iterationsM)
iterationsM_df
states <- c("S","I", "R")
names(iterationsM_df) <- states

ggplot() +
  geom_line(data = iterationsM_df, aes(x = c(1:n_iterations),
                                       y = S, color = "S")) +
  geom_line(data = iterationsM_df, aes(x = c(1:n_iterations),
                                       y = I, color = "I")) +
  geom_line(data = iterationsM_df, aes(x = c(1:n_iterations),
                                       y = R, color = "R")) +
  labs(colour = "States", x = "Number of Iterations", y = "People") +
  ggtitle("Realisation of the epidemic") +
  theme(plot.title = element_text(hjust = 0.5))
  
  
#ggsave("Realisation.png")

#Compute the variation distance between the two vectors
distance <- (abs(v1[1] - v2[1]/10000) + abs(v1[2] - v2[2]/10000) + abs(v1[3] - v2[3]/10000))/2
distance
