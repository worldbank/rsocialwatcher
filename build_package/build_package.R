if(F){
  setwd("~/Documents/Github/rSocialWatcher")
  
  roxygen2::roxygenise("~/Documents/Github/rSocialWatcher")
  
  pkgdown::clean_site()
  pkgdown::deploy_to_branch()
  usethis::use_pkgdown_github_pages() #####
  usethis::use_github_action_check_standard()
  
  ## Comand line code for building and checking package
  #R CMD build --as-cran "~/Documents/Github/googletraffic"
  #R CMD check --as-cran "~/Documents/Github/googletraffic/googletraffic_0.0.0.9000.tar.gz"
  
  devtools::check("~/Documents/Github/rSocialWatcher")
  
  devtools::check_win_devel("~/Documents/Github/rSocialWatcher")
  devtools::check_win_release("~/Documents/Github/rSocialWatcher")
  devtools::check_win_oldrelease("~/Documents/Github/rSocialWatcher")
  
  devtools::build("~/Documents/Github/rSocialWatcher")
}