# write out the file to be sourced
fLS = file(description = "Code/8-LocalSource-Input.R", open = "w+")
write(x ="a = 1; print(a)", file = fLS)
close(fLS)

# source the file
a = 3
sourceEnv = new.env()
with(sourceEnv, source("Code//8-LocalSource-Input.R", local = TRUE))
a