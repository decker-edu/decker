set samples 20, 20
set isosamples 20, 20
set hidden3d back offset 1 trianglepattern 3 undefined 1 altdiagonal bentover
set style data lines
set xrange [ -3.00000 : 3.00000 ] noreverse nowriteback
set yrange [ -2.00000 : 2.00000 ] noreverse nowriteback
DEBUG_TERM_HTIC = 119
DEBUG_TERM_VTIC = 118
splot 1 / (x*x + y*y + 0.5)