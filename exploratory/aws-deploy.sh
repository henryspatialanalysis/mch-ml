# INSTALLATION SCRIPT FOR DATA PREP EC2 INSTANCE

# Auto-restart services when needed
sudo sed -i "/#\$nrconf{restart} = 'i';/s/.*/\$nrconf{restart} = 'a';/" \
    /etc/needrestart/needrestart.conf &&
    sudo needrestart;

# First time: install dependencies
sudo apt update &&
    sudo apt -y upgrade &&
    sudo apt install -y postgresql postgresql-client nfs-kernel-server r-base-dev r-base-core \
    libgdal-dev gdal-bin libudunits2-dev libssl-dev libfontconfig1-dev libharfbuzz-dev \
    libfribidi-dev cmake;

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
sudo R CMD INSTALL ~/efs-mount/repos/colorout;
# sudo rm -rf /usr/local/lib/R/site-library/00LOCK-*
sudo R -e "install.packages(setdiff(c( \
  'data.table', 'R6', 'caret', 'glue', 'gtools', 'haven', 'httr', 'rdhs', 'sf', 'stats', \
  'tictoc', 'utils', 'xgboost', 'stepPlr', 'randomForest', 'ada', 'caTools' \
), installed.packages()[, 'Package']))";
