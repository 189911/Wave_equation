 set ylabel "Y(x,t)"
 set xlabel "X"
 set xrange [0:   4.0000000000000000      ]
 set yrange [-0.07:0.07]
 set title "ONDA"
 set style data linespoints
 set terminal gif animate delay 2
 set output "cuerda.gif"
 stats "fotograma.txt" name "A"
 file1="fotograma.txt"
 do for [i=0:int(A_blocks-1)]{plot file1 index i w l lc 1 lw 2}                                      
