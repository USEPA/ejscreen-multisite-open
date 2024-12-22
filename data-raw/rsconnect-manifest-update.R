################################################################################################## #
# Note the package here is called 'EJAM' even though a repo can be called something other than 'EJAM'
# and still contain/offer an installable copy of a branch/version/release of the EJAM pkg.

# The public repo 'USEPA/ejscreen-multisite-open' main branch is a copy of
# the internal repo 'USEPA/EJAM' branch called 'ejscreen-multisite-open'
# and they are similar to the USEPA/EJAM branch called 'PUBLIC-EJSCREEN' but cleaned up somewhat. 

# The public repo 'USEPA/EJAM-open' main branch would be a copy of 
# the internal repo 'USEPA/EJAM' branch called 'EJAM-open' 
# and they are similar to the USEPA/EJAM branch called 'main' but cleaned up somewhat.

######################################################## ######################################################### #
######################################################## ######################################################### #

#     HOW TO REDEPLOY SHINY APP TO POSIT CONNECT SERVER ####
#
#     SCRIPT USED TO REDEPLOY SHINY APP AFTER UPDATES/EDITS
#
# Steps:

# 1) Source the steps below to A) install the pkg from github, and B) update the manifest file.
# 2) commit changes (updated manifest file) to git repo 
# 3) either manually update deployed app via posit connect server management page, 
#    or just wait until it periodically updates by itself.

## To deploy, 1st reinstall package to this machine, from specified repo and branch (needed for manifest)
#
## You might be able to do that now simply via something like this:
# xyz.tar.gz <- "https://github.com/USEPA/ejscreen-multisite-open/archive/refs/tags/v2.32-ejscreen-multisite-open.tar.gz" # for the public app. 
# install.packages(xyz.tar.gz)  # probably does NOT require PAT set up/ authentication if simply downloading source bundle from a public repo.
## or else do it like this:

## source the script in the file called 
# 
#      ".\data-raw\rsconnect-manifest-update.R"
# 
## which might be something like the following:


######################################################### #

devtools::install_github(
  
  #### *** uncomment one of these... varies depending on which repository and which branch you need:
  
  # repo = 'USEPA/ejscreen-multisite-open', ref = 'main', # to install from the public repo  - requires a PAT; but install.packages() is easier
  # repo = 'USEPA/EJAM', ref = 'ejscreen-multisite-open', # to install from an internal repo - requires access,token,authentication
  
  # repo = 'USEPA/EJAM-open',               ref = 'main', # to install from the public repo  - requires a PAT; but install.packages() is easier
  # repo = 'USEPA/EJAM', ref = 'EJAM-open',               # to install from an internal repo - requires access,token,authentication
  
  # repo = 'USEPA/EJAM', ref = 'deploy-posit',    # had been using to deploy web app internally
  # repo = 'USEPA/EJAM', ref = 'PUBLIC-EJSCREEN', # had been using to deploy web app internally as preview of public
  
  build_vignettes = FALSE,
  build_manual    = FALSE,
  build           = FALSE,
  dependencies    = TRUE, # so it checks that you installed all packages in Suggests not just Imports
  upgrade = "never"
)

## Get list of files now found in EJAM source root directory 
## (***BUT, should it check source or installed folder? should be the same?)

all_files <- rsconnect::listDeploymentFiles(getwd())

## exclude certain subfolders from being searched for dependencies
deploy_files <- all_files[-c(grep('dev/',          all_files),
                             grep('docs/',         all_files),
                             grep('data-raw/',     all_files),
                             grep('.github/',      all_files),
                             grep('tests/',        all_files),
                             grep('vignettes/',    all_files),
                             grep('.arrow',        all_files),
                             grep('.gitignore',    all_files),
                             grep('.Rhistory',     all_files),
                             grep('.Rbuildignore', all_files),
                             grep('EJAM.rproj',    all_files)
)
]

# print(deploy_files)

## check dependency list
x <- rsconnect::appDependencies(appFiles = deploy_files) # > roughly 220 

print(dim(x))

## update manifest.json file
rsconnect::writeManifest(appFiles = deploy_files)
beepr::beep() # to make an alert sound when this is finished.

cat("Now commit that updated manifest file, push to github, 
    and if server is set to deploy from that repo and branch it will detect the changes and redeploy \n")
######################################################### #


######################################################## ######################################################### #
######################################################## ######################################################### #

