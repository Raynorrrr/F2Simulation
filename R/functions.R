# function.R is a script with helper functions for F2 simulations
# input: NA
# output: NA
# notes: add source("functions.R") to any file that needs these functions


# function for P generation

generating_parent <- function(chromosome_length = 100){
  ## Assigning parent 1 to an all 0 chromosome
  parent_1_ch1 <- rep ( x = 0 , chromosome_length)
  parent_1_ch2 <- parent_1_ch1
  parent1 <- data.frame(CH1 = parent_1_ch1, CH2 = parent_1_ch2)
  
  ## Assigning parent 2 to an all 1 chromosome
  parent_2_ch1 <- rep ( x = 1 , chromosome_length)
  parent_2_ch2 <- parent_2_ch1
  parent2 <- data.frame(CH1 = parent_2_ch1, CH2 = parent_2_ch2)
  
  return(list(P.1 = parent1, P.2 = parent2))
}

# function for simulating new mouse from parents
generating_mouse <- function(parent_chromosomes, num_recombinations = 3){
  #choose which parents to breed
  p <- sample(x = 1:length(parent_chromosomes), size = 2,replace = FALSE)
  parents <- parent_chromosomes[p]
  
  ## Which parent 1 chromosome mouse "a" got
  chr <- sample(x = c(1, 2), size = 1)
  
  parent1_chrom <- parents[[1]][[chr]]
  
  ## Which parent 2 chromosome mouse "a" got, taking a random sample from 1 or 2 to decide like previous
  chr <- sample(x = c(1, 2), size = 1)
  parent2_chrom <- parents[[2]][[chr]]

  ## choosing recombination locations from 100-loci chromosome
  recomb_loci <- sort(sample( 1:length(parent1_chrom), size = num_recombinations, replace = FALSE))
  
  ## Recombining loci by alternating b/w the parent's chromosome at the recombination loci
  for(i in 1:(num_recombinations+1)){
    if(i == 1){
      a_CH1 <- parent1_chrom[1:recomb_loci[i]]
      a_CH2 <- parent2_chrom[1:recomb_loci[i]]
    }else if(i == num_recombinations+1){ 
       if(i %% 2 != 0){
        a_CH1 <- append(x = a_CH1, values = parent1_chrom[(recomb_loci[i-1]+1):length(parent1_chrom)])
        a_CH2 <- append(x = a_CH2, values = parent2_chrom[(recomb_loci[i-1]+1):length(parent1_chrom)])
      }else if(i %% 2 == 0){
        a_CH1 <- append(x = a_CH1, values = parent2_chrom[(recomb_loci[i-1]+1):length(parent1_chrom)])
        a_CH2 <- append(x = a_CH2, values = parent1_chrom[(recomb_loci[i-1]+1):length(parent1_chrom)])
      }
    }else if(i %% 2 != 0){
      a_CH1 <- append(x = a_CH1, values = parent1_chrom[(recomb_loci[i-1]+1):recomb_loci[i]])
      a_CH2 <- append(x = a_CH2, values = parent2_chrom[(recomb_loci[i-1]+1):recomb_loci[i]])
    }else if(i %% 2 == 0){
      a_CH1 <- append(x = a_CH1, values = parent2_chrom[(recomb_loci[i-1]+1):recomb_loci[i]])
      a_CH2 <- append(x = a_CH2, values = parent1_chrom[(recomb_loci[i-1]+1):recomb_loci[i]])
    }# this is for the all other recombination chunks 
  }
  return(data.frame(CH1 = a_CH1, CH2 = a_CH2))
}

# function for simulating new generation from parents
generating_generation <- function(parent_chromosomes, num_recombinations = 3, 
num_mice = 400, generation = c("F1","F2","F3")){
  generation <- generation[1]
  for(i in 1:num_mice){
    name_mouse <- paste0(generation, ".", i)
    if(i == 1){
      population_list <- list(generating_mouse(parent_chromosomes = parent_chromosomes,
  num_recombinations = num_recombinations))
      names(population_list)[1] <- name_mouse
    }else{
      population_list <- append(x= population_list, values= list(generating_mouse(parent_chromosomes = parent_chromosomes,
  num_recombinations = num_recombinations)))
      names(population_list)[i] <- name_mouse
    }  
  }
  return(population_list)
}

# visualization of one of the chromosomes
chromo_fig <- function(chromosomes, colors = c("darkorange1","dodgerblue1") )
  if(!require("ggplot2")){
  install.packages("ggplot2")
  }
require("ggplot2")

i <- sample(1:length(chromosomes), size = 1)
chromo_to_plot <- as.data.frame(chromosomes[i])
chromo_to_plot$loci - 1:now(chromo_to_plot)

ch1.y <- 1
ch2.y <-1

plotting_data <- data.frame(xmin = numeric(), xmax = numeric(), ymin = numeric(), 
  ymax = numeric(), fill = character())

for(i in 1:(nrow(chromo_to_plot)-1)){
  if(chromo_to_plot[i,1] == chromo_to_plot[i+1,1]){
    next
  }else{
    plotting_data <- append(plotting_data, c(xmin = 0, xmax = 1, ymin = 0, ymax = chromo_to_plot[i, 3], 
      fill = "darkorange1"))
    
    
    ch1.y <- append(ch1.y, chromo_to_plot[i,3])
    ch1.y <- append(ch1.y, chromo_to_plot[i+1,3])
  }
  
  if(chromo_to_plot[2,i] == chromo_to_plot[i+1, 2]){
    next
  }else{
    ch2.y <- append(ch2.y, chromo_to_plot[i,3])
    ch2.y <- append(ch2.y, chromo_to_plot[i+``,3])
  }
}

plot <- ggpot(data = x)
# simulating phenotype 


# running QTL on simulated data

