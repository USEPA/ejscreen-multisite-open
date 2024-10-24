
## reinstall package from deploy-posit branch (needed for manifest)
devtools::install_github('USEPA/EJAM', ref='PUBLIC-EJSCREEN')

## list files in EJAM root directory
all_files <- rsconnect::listDeploymentFiles(getwd())

## exclude certain subfolders from being searched for dependencies
deploy_files <- all_files[-c(grep('dev/', all_files),
                             grep('docs/', all_files),
                             grep('data-raw/', all_files),
                          grep('.github/', all_files),
                          grep('tests/', all_files),
                          grep('vignettes/', all_files),
                          grep('.arrow', all_files),
                          grep('.gitignore', all_files),
                          grep('.Rhistory', all_files),
                          grep('.Rbuildignore', all_files),
                          grep('EJAM.rproj', all_files)
)
                          ]

deploy_files 

## check dependency list
x <- rsconnect::appDependencies(appFiles=deploy_files) #222 at last check

## update manifest.json file
rsconnect::writeManifest(appFiles=deploy_files)

