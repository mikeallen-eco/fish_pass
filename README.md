# Migratory passage and run size of American shad and river herring
This repository provides code and data to reproduce the analyses and figures in the manuscript: "Migratory passage and run size of American shad and river herring in the Raritan River, New Jersey, USA" (Jensen et al., 2024). 

Jensen, O. P., Vastano, A. R., Allen, M. C., Hernandez, M. F., Lockwood, J. L., Vasslides, J., & Weldon, O. (2024). Migratory passage and run size of American shad and river herring in the Raritan River, New Jersey, USA. Transactions of the American Fisheries Society, 153(3), 289-300. [link](https://doi.org/10.1002/tafs.10467)

# List of files

/

Script & project files in the main folder
1. fish_pass.Rmd - re-create the analyses and figures in the manuscript
2. fish_pass.Rproj - double click on this file to open the R Project in RStudio

/data

Files in the data folder referenced in fish_pass.Rmd
1. IFW Tagging Data Golden Master 2_25_22.xlsx - data on each tagged fish
2. IFW Video Data Golden Master_4_24_22.xlsx - video fish count data
3. run_size_estimates.csv - run size estimates represented in Figure 5
4. /receiver_files - text files containing "ping data" of fish tag antennas
5. /sun - text files containing sunrise and sunset data for Figure S1

/figures

All figures in the manuscript created using the code in this repository.

/output

These .rds files store model output or data that is referenced in the fish_pass.Rmd.

/scripts

These .R files are functions and scripts to process data as referenced in the fish_pass.Rmd file. 
The .txt files are the JAGS models.
