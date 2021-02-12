
#Calls to be made to rsconnect once on the local machine that is doing the deploying

#this one increases the disk limit up to the 5GB
ONE.GB = 1048576000
options(rsconnect.max.bundle.size=5*ONE.GB)

print(paste0("Max Bundle Size is set to: ", 
             getOption('rsconnect.max.bundle.size'),
             " (", round(getOption('rsconnect.max.bundle.size')/ONE.GB,1), " GB)"))
