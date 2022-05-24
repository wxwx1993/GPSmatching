
set -xv




docker run -it --rm \
        -p 8231:8787 \
        -e USER=rstudio \
        -e PASSWORD=pass \
        -v "/Users/nak443/Documents/Naeem_folder_mac_h/Research_projects/F2021_001_Harvard/experiment_001_20210113_RPackage_GPSmatching/analysis/CausalGPS:/home/rstudio/CausalGPS" causalgps_dev
