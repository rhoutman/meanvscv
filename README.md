```R 
if (!requireNamespace("remotes"))
  install.packages("remotes")

remotes::install_github("rstudio/renv")

renv::init()

git config --global user.email "rene_houtman@hotmail.com"
git config --global user.name "Rene Houtman"
  
rstudio@fa45e7086ceb:~/tercen_operator/meanvscv$ git tag -a 0.9 -m "meanvscv"
rstudio@fa45e7086ceb:~/tercen_operator/meanvscv$ git push --tags

```