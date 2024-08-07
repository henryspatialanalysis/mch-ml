#!/usr/bin/bash
## #######################################################################################
## 
## DOWNLOAD RESULTS TO LOCAL MACHINE
## 
## AUTHOR: Nat Henry
## CREATED: 5 August 2024
## PURPOSE: Download model results to local computer from AWS
## 
## #######################################################################################

# Connection details
EC2_USERNAME="ubuntu";
KEY_PATH="~/.ssh/aws-key-pair-v1.pem";
EC2_HOST="ec2-35-164-160-53.us-west-2.compute.amazonaws.com";
MODEL_VERSION="20240805_SEN_wasting_no_ad1";
SYNC_DIR="~/temp_data/usaid-mch-ml/mbg_model_results/${MODEL_VERSION}"

mkdir ~/temp_data/usaid-mch-ml/mbg_model_results/$MODEL_VERSION &&
  rsync -r -e "ssh -i ${KEY_PATH}" \
    ubuntu@$EC2_HOST:~/efs-mount/mbg_outputs/$MODEL_VERSION/ \
    ~/temp_data/usaid-mch-ml/mbg_model_results/$MODEL_VERSION/ --info=progress2;
