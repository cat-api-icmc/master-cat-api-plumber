# install base64enc
Rscript -e 'install.packages("base64enc", repos="https://cran.rstudio.com/")'

# install dotenv
Rscript -e "install.packages('dotenv', repos='https://cran.rstudio.com/')"

# install plumber
Rscript -e 'install.packages("plumber")'

# install mirtCAT
Rscript -e "install.packages('remotes', repos='https://cran.rstudio.com/')"
Rscript -e "remotes::install_github('philchalmers/mirtCAT')"
# Rscript -e "install.packages('mirtCAT', repos='https://cran.rstudio.com/')"
