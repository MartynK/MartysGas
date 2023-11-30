library(dplyr)

set.seed(420)
block_size <- 4
no_blocks  <- 5
one_per_block <- floor( block_size/2)
rand <- rep( 0, block_size * no_blocks)

for (i in 1:no_blocks) {
  block <- sample( c(rep(1,one_per_block), rep(0,block_size - one_per_block)))
  rand[((i-1)*4+1):(i*4)] <- block
}

out <- data.frame( date      = as.Date(0:(block_size*no_blocks-1),
                                       origin="2021-10-18"),
                   treatment = rand)
