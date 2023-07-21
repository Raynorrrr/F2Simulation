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
      print(paste(i, length(a_CH1), "statement 1" ,sep = " / "))
    }else if(i == num_recombinations+1){ 
       if(i %% 2 != 0){
        a_CH1 <- append(x = a_CH1, values = parent1_chrom[(recomb_loci[i-1]+1):length(parent1_chrom)])
        a_CH2 <- append(x = a_CH2, values = parent2_chrom[(recomb_loci[i-1]+1):length(parent1_chrom)])
        print(paste(i, length(a_CH1), "statement 2" , sep = " / "))
      }else if(i %% 2 == 0){
        a_CH1 <- append(x = a_CH1, values = parent2_chrom[(recomb_loci[i-1]+1):length(parent1_chrom)])
        a_CH2 <- append(x = a_CH2, values = parent1_chrom[(recomb_loci[i-1]+1):length(parent1_chrom)])
        print(paste(i, length(a_CH1), "statement 3" , sep = " / "))
      }
    }else if(i %% 2 != 0){
      a_CH1 <- append(x = a_CH1, values = parent1_chrom[(recomb_loci[i-1]+1):recomb_loci[i]])
      a_CH2 <- append(x = a_CH2, values = parent2_chrom[(recomb_loci[i-1]+1):recomb_loci[i]])
      print(paste(i, length(a_CH1),  "statement 4" ,sep = " / "))
    }else if(i %% 2 == 0){
      a_CH1 <- append(x = a_CH1, values = parent2_chrom[(recomb_loci[i-1]+1):recomb_loci[i]])
      a_CH2 <- append(x = a_CH2, values = parent1_chrom[(recomb_loci[i-1]+1):recomb_loci[i]])
      print(paste(i, length(a_CH1), "statement 5" , sep = " / "))
    }# this is for the all other recombination chunks 
  }
  return(data.frame(CH1 = a_CH1, CH2 = a_CH2))
}

# function for simulating new generation from parents
generating_generation <- function(parent_chromosomes, num_recombinations = 3, num_mice = 400, generation = c("F1","F2","F3")){
  generation <- generation[1]
  for(i in num_mice){
    name_mouse <- paste0(generation, ".", i)
    if( i == 1){
      population_list <- list(name_mouse = generating_mouse(parent_chromosomes = parent_chromosomes,num_recombinations = num_recombinations))
    }  
  }
}

# visualization of one of the "chromosomes


# simulating phenotype 


# running QTL on simulated data

