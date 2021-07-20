 #set terminal x11
 set ylabel "Y"
 set xlabel "X"
 set zlabel "Z"
 set xrange [0:   4.0000000000000000      ]
 set yrange [0:   8.0000000000000000      ]
 set zrange [ -7.34058247979829659E-002 :  7.34058247979829659E-002 ]
 set title "COMPORTAMIENDO DE ONDA"
 set style data points
 set surface
 set termoption dashed
 set isosamples 51
 stats "fotograma.txt" name "A"
 file1="fotograma.txt"
 do for [i=0:int(A_blocks-1)]{splot file1 index i w p}                                                  
