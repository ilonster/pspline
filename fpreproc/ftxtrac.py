# python implementation of "ftxtrac" -- extract lines btw
#  "\begin{verbatim}" and "\end{verbatim}", respectively.
# typically: extract a fortran source from a .tex document...

import sys
import string

mode="tex"

bstring='\\begin{verbatim}'
estring='\\end{verbatim}'

flines = sys.stdin.readlines()

for fline in flines:
    line = string.rstrip(fline)
    if mode == "f77":
        if string.find(line,estring) > -1:
            mode = "tex"
            continue
        else:
            print line

    else:
        if string.find(line,bstring) > -1:
            mode = "f77"

# the end.
