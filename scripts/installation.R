# install Phylosim

# get the updated version of Annika with DESCRIPTION -> RoxygenNote: 7.3.2

# remove the old package
remove.packages("PhyloSim")

# change directory to the package &
# build the 
Rcpp::compileAttributes('~/cyber_synch/EcoPhyloSim/PhyloSim/')

# do this in the terminal
# cd navigate to one folder above Phylosim (e.g., Eco_phyloSim-master_Anika)
# R CMD build PhyloSim
# R CMD INSTALL PhyloSim_0.3.1.tar.gz

library(PhyloSim)
