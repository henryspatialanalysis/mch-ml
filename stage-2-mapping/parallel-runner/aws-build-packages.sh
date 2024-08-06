#!/usr/bin/bash

# Build MBG and pixel2poly packages on remote
# This script only has to be run once
cd ~/efs-mount/repos &&
    git clone git@github.com:henryspatialanalysis/mbg.git &&
    git clone git@github.com:henryspatialanalysis/pixel2poly.git &&
    git clone git@github.com:henryspatialanalysis/usaid-mch-ml.git;
R CMD build --no-build-vignettes mbg;
R CMD build --no-build-vignettes pixel2poly;
cd usaid-mch-ml && R CMD build --no-build-vignettes r-package && mv mch.ml*.tar.gz ../ && cd ../;

# Set up directories that will store MBG inputs and outputs
cd ~/efs-mount/ &&
    sudo mkdir mbg_inputs &&
    cd mbg_inputs &&
    sudo mkdir covariates input_data shp &&
    sudo chmod -R 0777 ~/efs-mount/mbg_inputs &&
    cd ~/efs-mount/ &&
    sudo mkdir mbg_outputs &&
    sudo chmod -R 0777 ~/efs-mount/mbg_outputs;
