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
EC2_HOST="ec2-54-201-66-112.us-west-2.compute.amazonaws.com";
RUN_SET="20240805";
REMOTE_DIR="/home/ubuntu/efs-mount/mbg_outputs/selected_${RUN_SET}"
LOCAL_DIR="/home/nathenry/temp_data/usaid-mch-ml/mbg_model_results/${RUN_SET}"

mkdir $LOCAL_DIR &&
  rsync -r -e "ssh -i ${KEY_PATH}" "ubuntu@$EC2_HOST:${REMOTE_DIR}" $LOCAL_DIR --info=progress2;
