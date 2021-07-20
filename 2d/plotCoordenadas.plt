 set terminal gif animate delay 2
 set output "cuerda.gif"
 #set terminal x11
 set ylabel "Y"
 set xlabel "X"
 set zlabel "Z"
 set xrange [0:   3.0000000000000000      ]
 set yrange [0:   7.0000000000000000      ]
 set zrange [ -7.88329913972543039E-002 :  7.88329913972543039E-002 ]
 set title "COMPORTAMIENDO DE ONDA"
 set style data points
 set surface
 stats "fotograma.txt" name "A"
 file1="fotograma.txt"
 do for [i=0:int(A_blocks-1)]{splot file1 index i w p}                                                  
