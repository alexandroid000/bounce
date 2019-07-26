stack exec -- bounce-exe -o start2.gif -e plane -n 80 -a 0.62 -g -s 0.6
stack exec -- bounce-exe -o start1.gif -e plane -n 80 -a 0.62 -g -s 0.7

convert start1.gif -coalesce a-%04d.gif                         # separate frames of 1.gif
convert start2.gif -coalesce b-%04d.gif                         # separate frames of 2.gif
for f in a-*.gif; do convert $f ${f/a/b} +append $f; done  # append frames side-by-side
convert -loop 0 -delay 20 a-*.gif plane.gif               # rejoin frames

stack exec -- bounce-exe -o twoconv.gif -e twoconv -n 120 -a -1 -g -s 0.04

cp twoconv.gif start1.gif start2.gif plane.gif ~/projects/talks/wafr/images
