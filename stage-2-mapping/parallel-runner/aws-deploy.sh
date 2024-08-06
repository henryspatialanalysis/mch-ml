#!/usr/bin/bash
## #######################################################################################
## 
## INSTALLATION SCRIPT FOR DATA PREP EC2 INSTANCE
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: 20 July 2024
## PURPOSE: Run once on every new AWS instance that will be used to run MBG
##
## #######################################################################################

# Auto-restart services when needed
sudo sed -i "/#\$nrconf{restart} = 'i';/s/.*/\$nrconf{restart} = 'a';/" \
    /etc/needrestart/needrestart.conf &&
    sudo needrestart;

# First time: install dependencies
sudo apt update &&
    sudo apt -y upgrade &&
    sudo apt install -y postgresql postgresql-client nfs-kernel-server \
    libgdal-dev gdal-bin libudunits2-dev libssl-dev libfontconfig1-dev libharfbuzz-dev \
    libfribidi-dev cmake openjdk-21-jdk-headless;

# Add default Java path
cd /usr/lib/jvm/ &&
  sudo ln -s ./java-21-openjdk-amd64 ./default-java &&
  cd ~;

# First time or on reboot: mount EFS instance
mkdir ~/efs-mount;
sudo mount \
    -t nfs \
    -o nfsvers=4.1,rsize=1048576,wsize=1048576,hard,timeo=600,retrans=2,noresvport \
    172.31.24.184:/ \
    ~/efs-mount;

# Copy bash profile from efs mount to home and source it
cp ~/efs-mount/.profile ~/ && source ~/.profile;

# Update Git username and email
git config --global user.name "Nathaniel Henry";
git config --global user.email "nat@henryspatialanalysis.com";

# Set up Git SSH key
eval "$(ssh-agent -s)" &&
    sudo cp ~/efs-mount/.ssh/id_ed25519 ~/.ssh &&
    sudo chmod 0600 /home/ubuntu/.ssh/id_ed25519 &&
    sudo chown ubuntu /home/ubuntu/.ssh/id_ed25519 &&
    ssh-add ~/.ssh/id_ed25519;

# Copy over default ~/.Renviron and ~/.Rprofile
sudo cp ~/efs-mount/.Rprofile ~/ && 
    sudo cp ~/efs-mount/.Renviron ~/;

# Install R packages
# --> Add CRAN repositories
sudo apt update -qq &&
    # install two helper packages we need
    sudo apt install --no-install-recommends software-properties-common dirmngr &&
    # add the signing key (by Michael Rutter) for these repos
    # To verify key, run gpg --show-keys /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc 
    # Fingerprint: E298A3A825C0D65DFD57CBB651716619E084DAB9
    wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | \
        sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc &&
    # add the R 4.0 repo from CRAN -- adjust 'focal' to 'groovy' or 'bionic' as needed
    sudo add-apt-repository -y "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/";
# --> Install R base
sudo apt update &&
    sudo apt -y upgrade &&
    sudo apt install -y r-base r-base-dev r-base-core;
# --> Update Matrix package
sudo R -e "install.packages('Matrix')";
# --> Install all other INLA and MBG dependencies
sudo R -e "install.packages(setdiff(c( \
  'argparse', 'assertthat', 'caret', 'data.table', 'devtools', 'elasticnet', 'fmesher', \
  'gbm', 'glue', 'graphics', 'grDevices', 'gtools', 'lifecycle', 'MatrixModels', \
  'matrixStats', 'methods', 'mgcv', 'nlme', 'nnet', 'osmdata', 'parallel', 'purrr', 'r5r', \
  'R6', 'rdhs', 'rJava', 'rlang', 'RPostgreSQL', 'sn', 'sf', 'splines', 'stats', 'terra', \
  'tictoc', 'tidytransit', 'utils', 'versioning', 'withr'
), installed.packages()[, 'Package']))";
# --> INLA
sudo R -e "install.packages('INLA', repos='https://inla.r-inla-download.org/R/stable', dep=TRUE)";
# --> MBG
cd ~/efs-mount/repos/ &&
    sudo R CMD INSTALL mbg_0.9.1.tar.gz &&
    sudo R CMD INSTALL pixel2poly_0.0.0.9000.tar.gz &&
    sudo R CMD INSTALL mch.ml_0.0.0.9000.tar.gz &&
    cd ~/;
# Fix for INLA permissions
sudo chmod -R 0777 /usr/local/lib/R/site-library/INLA/bin/linux/;
