fi <- tempfile()
writeLines("f()", fi)
f <- function() print(sys.frame(-4)$srcfile)
source(fi)
fi
