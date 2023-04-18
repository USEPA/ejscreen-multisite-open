
######################################################################################################### #

# see  https://usepa.sharepoint.com/sites/oei_Community/HESC/SitePages/Home.aspx

# webinar transcript I saved:  "transcript webinar PARALLEL PROCESSING via EPA HPCC 2023-02-17.txt"
# webinar video: https://usepa-my.sharepoint.com/personal/vega_ann_epa_gov/_layouts/15/stream.aspx?id=%2Fpersonal%2Fvega%5Fann%5Fepa%5Fgov%2FDocuments%2FRecordings%2FR%20user%20group%2Dmonthly%20webinar%20series%2D20230215%5F140239%2DMeeting%20Recording%2Emp4&ga=1 


# USING EPA ATMOS - HIGH PERFORMANCE CLUSTER 
# - you need an account.
# MOSTLY it is AVAIL FOR ORD AND OAR, IN RTP, BUT OTHER OFFICES MIGHT HAVE A WAY TO GET TIME ON IT.
# YOU APPLY FOR PROJECT TIME ON THE SHAREPOINT SITE. ANYONE IN ORD CAN APPLY FOR A TESTING PROJECT, UP TO 3K HOURS USE-TIME.
# YEARLY YOU SUBMIT PROPOSAL FOR TIME. <20K HOURS DO NOT NEED EXTRA FUNDING OR APPROVALS.

# NEED TO KNOW WHICH SETUP TO USE !! (or it is slower than not using it at all!)
# Each node is for a certain purpose- speed, memory, etc.
# "compute" node has 32 cores (high perform laptop has 16).
# largemem node has 72 cores or 4 processors per node, 1 node, but 1536 GB memory per node.
# for example, dataset of 6k wells, find 5 nearest neighbors and calc mean distance,
#  1 epa high perf laptop = 20 minutes (0.18 seconds each iteration)
#  same but 8 cores, 3 minutes.
#  Atmos- largemem, 1 core, WORSE:  48 minutes. and 71 cores is 39 minutes.
# Atmos - compute node, 1 core is 42 minutes.
# Atmos compute node with 31 cores, though, is 1.2 minutes !! NEED TO KNOW WHICH SETUP TO USE !! 
#
# STEPS in using Atmos High performance cluster: (see webinar)
# 
# get Atmos account &apply for test-project time via sharepoint site. 
# need download Mobaxterm free epa-approved software. This is the SSH client to use.
# set up ssh connection with EPA credentials. atmos1.nesc.epa.gov, port 22.
# config your login behavior, not simple.   The .tcshrc file in home dir does that, see template the speaker provided (R User webinar 2/17/23)
# keep your data and scripts in your work dir, not your home dir. 
# copy data and scripts to Atmos.  can click/drag in  SSH session. can pull from EPA github repo 
# test script in R
# send script to your selected Atmos node 
# use the debug node just to test your code - up to 4 hrs "debug node session"  From that node you launch an RStudio session on that server, but you CANNOT INSTALL R PACKAGES ON ATMOS - IT HAS 2K PKGS ALREADY BUT NOT YOUR OWN PKG. ASK SUPPORT STAFF TO INSTALL A CUSTOM PKG IF NEEDED. 
# Write the .sh file that tells Atmos (built on linux) what to do. you make that .sh file once and then can slightly modify it as needed. specifies amounts of memory need, which node, name of job, time needed, text files like mylog.err and mylog.out, to print() or cat() errors/msg to, stock list of modules you are using (try typing module help r) like rstudio, git, R, geos, gcc, etc.; and last line in .sh file is path to your R script you are running. 
# In cmd line of MobaXterm/Atmos session, to submit batch, type    sbatch R_User_Group/samplesize.sh    for example.  Remote host  is atmos1.nesc.epa.gov,  port 22. 
# type in file folder window,    home/username,   load modules,   then go to   work/username 
# check your data is on the server.
#  cd /work/myuserfolderprojectname 
#  cd /home/mcorrale  
#  srun --ntasks=32 --partition=debug --time=2:00:00 --xll --pty csh -l # a 2hour debug test session
#  rstudio  (typed on ssh command line) # to launch remote rstudio session
#  sbatch myfolder/samplesize_inloop.sh  # to submit job with file you made
#  squeue -u myusername  # to see what is running 

########################################################################### # 














