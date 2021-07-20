      program derivadasParciales
      implicitnone
!      es una ecuaciondiferencial que tiene los valores de frontera de
!      y(t,x) donde y(0,x)=0 y y(t,0)=0
      !se realizara el procedimiento utilizando la logica de las difer
!     encias finitas
      real*8:: h,ht,L,FinX,FinT,x,e,c,r,pendiente,Fmult,Fsum,t
      real*8:: F_analitica,err
      integer::j,i,k,intervalos,or,n,p
      character*100::z
      real*8,dimension(1000,50000)::mat,mat2,mat3
      real*8,dimension(1000,1)::matError
      write(*,*)"ingrese el numero de intervalos de tiempo"
      read(*,*)intervalos
      do i=1,intervalos
         matError(i,1)=0.0
         end do


      !para poder evealuar necesitamos las ocndiciones inicales
      !suponiendo que tenemos una cuerda de longitudd
c     para llenar las condiciones iniciales de las cuales partiremos
c     prociedemos a iniciar las primeras condiciones de la matriz
      write(*,*)"Programa encargado de resolver EDP hiperbolicas"
      write(*,*)"si la ecuacion es de primer orden ingrece 1"
      write(*,*)"si la ecualcion es de segundo orden ingrese 2"
      read(*,*) or
      
      write(*,*) "ingrese la longitud de la cuerda"
      read(*,*) L
      write(*,*)"ingrese el valor de los incrementos en x"
      read(*,*)h
      ht=h/100.0
      pendiente=(ht/h)**or
      
      PRINT*,PENDIENTE
c     el numero de intervalos en lso que se partira la cuerda es grande
c     dependiendo del tama¤o de la misma para tener ma¤or presision

      r= L/h+1
      n=int(r)
      print*,n

      do i=1,intervalos
      do j=1,n
      mat(i,j)=0
      end do
      end do

      !!!------------------importante declaracion de los estados inicia
      !! ------------------estados iniciales si no no funciona el pedo
      !-------------------hay que cambiarlos dependiendo de la funcion-
      j=1
      x=0
      do while (x.le.L)
         mat(1,j)=finX(x)
         j=j+1
         x=x+h
      end do
      t=0.0
      do i=1,intervalos
        mat(i,1)=fint(t)
        mat(i,n)=0.0
        t=t+ht
       end do
      !--------------------alto------------------------------------
      !------------------------------------------------------------
      !-------------------------------------------------------------
c     hasta aqui llevamos declaradas las condiciones iniciales y proce
c     demos a calcular mediante la logica del meotodo de diferencias
c     centradas

      !Yi+1,j = 2Yi,j - Yi-1,j + C [ Yi,j-1 - 2Yi,j + Yi,j+1 ]
      if (or.eq.2)then
         call primerOrden(pendiente,intervalos,mat,h,ht,n,L,matERROR)
      end if
      
      
      
      
      
      
      
      !-------------------se imprime la matriz---------------------
      !--------archivos de las coordenadas ------------------------
      open(18,file="archivo.txt")
      !----------------------comandos de g nupliot----------------
      open(30,file="plotCoordenadas.plt")
      write(30,*)"set ylabel ""Y(x,t)"""
      write(30,*)"set xlabel ""X"""
      write(30,*)"set xrange [0:",L,"]"
      write(30,*)"set yrange [-1.3:1.3]"
      write(30,*)"set title ""ONDA"""
      write(30,*)"set style data linespoints"
      !write(30,*)"plot ""archivo.txt"" using  1:2"
      !do i=3,intervalos+1
      !write(30,*)"replot ""archivo.txt"" using  1:",i,""
      !end do
      write(30,*)"set terminal gif animate delay 2"

#Archivo gif de salida
      write(30,*)"set output ""cuerda.gif"""

# Usamos "stats" para leer la cantidad de fotogramas en el archivo de salida
# cada fotograma debe estar separado por dos espacios en blanco
      write(30,*)"stats ""fotograma.txt"" name ""A"""
      

      write(30,*)"file1=""fotograma.txt"""
      z="do for [i=0:int(A_blocks-1)]{plot file1 index i w l lc 1 lw 2}"
# Ciclo que imprime cada fotograma
# "Index" indica el fotograma en especifico a imprimir
# int(A_blocks-1) es el numero de fotogramas totales en el archivo de datos
      write(30,*)z

      close(30)
      
      !---------en este bloque se imprimen los datos en cada archivo
      !--------------------------------------------------------------
      r=0.0
      j=1

      do while (r.le.L+h)
      write(18,*)r,mat(1:intervalos,j)
      
      j=j+1
      r=r+h
      end do
      close(18)
      !-------------------------------------------------------------
      !--------------------------------------------------------------
      !archivo para graficar en python
      open(60,file="archivoPy.txt")
      r=0.0
      k=1
      do i=1,intervalos
      p=1
      r=0.0
      do j=1,2*n,2
      mat2(i,j)=r
      mat2(i,j+1)=mat(k,p)
      p=p+1
      r=r+h
      end do
      k=k+1
      end do
      do i=1,intervalos
      write(60,*) mat2(i,1:2*n)
      end do
      close(60)
      !--------------------------------------------------------------
      !--------------------------------------------------------------
      !---------------------archivo fotograma para gnuplot----------
      open(16,file="fotograma.txt")
      r=0.0
      k=1
      !write(16,*)0.d0,0.d0
      !write(16,*)x1,y1
      do i=1,intervalos
      do j=1,2*n,2
      write(16,*) mat2(i,j:j+1)
      end do
      write(16,*)"                                  "
      write(16,*)"                                  "
      end do
      close(16)
      
      
      

      !----------------------------------------------------------
      !_----------------------------------------------------------
      
      
      
      
      
      
      
      
      
      open(9,file="archivo9.txt")
      do i=1,intervalos
      write(9,*) mat(i,1:n)
      end do
      close(9)
      call system("plotCoordenadas.plt")
      err=0.0
      !call Error(intervalos,mat,h,ht,n,err,L,matError)
      write(*,*)"-----------------------error-------------------------"
      t=0.0
      do i=1,intervalos
      write(*,*)"t=",t,"   Error  ", matError(i,1)
      t=t+ht
      end do
      print*, err
      
      pause
      end
        



      
      
      
      
      
      
      
      
      
      !-------funciones para las condiciones iniciales-----------------
      !---------------------------------------------------------------
      !---------------------------------------------------------------
      !_---------------------------------------------------------------
      
      !funciones de las condiciones iniciales en t=0
      function FinX(x)
        real*8:: x
        ! finx=0
        if (x.le.1.5)then
           FinX=0.04*x

        elseif (x.le.4.0)then
           FinX=-0.024*x+0.096
        !end if
        end if
        end
      !funciones de las condiciones iniciales con x=0
      function FinT(t)
        real*8:: t
        !FinT=sin(30*t)
        FinT=0.0

        end
      function F_analitica(x,t,L)
        real*8::x,t,a,pi,E,h,L,su,fmult
        integer::i
        h=0.06
        E=1.5
        pi=acos(-1.0)
        su=0.0
        do i=1,500
      su=su+(1/(i**2))*sin(i*pi*E/L)*sin(i*pi*x/L)*cos(i*pi*9651.0*t/L)
        end do
        F_analitica=su*2.0*h*(L**2)/(pi**2*E*(L-E))
        end

        
c     condiciones iniciales para 2 variavles espaciales--------


 
      !----------------------------------------------------------------
      !----------------------------------------------------------------
      !----------------------------------------------------------------
      !----------------------------------------------------------------


c     a continuacion las funciones que acompa¤an a las derivadas
c     la derivada con respecto al tiempo siempre debe ir libre
!----------------------------------------------------------------------
!_---------------------------------------------------------------------
      !funciones para las ecuaciones de 1 variable espacial
      
      function Fmult(t,x)
          real*8:: t,x
          Fmult=9651.0
          end
      function Fsum(t,x)
          real*8:: t,x
          Fsum=0.0
          end


      !-----------------------------------------------------------
      !--------------------------------------------------------
      !---------------------------------------------------------
      !-------------------------------------------------------
c     subrutunas depara el caso de primer y segundo ordern
      !-------------------------------------------------------
      !------------------------------------------------------
      
      subroutine primerOrden(pendiente,intervalos,mat,h,ht,n,L,matError)
      implicitnone
      real*8::t,x,pendiente,h,ht,e,c,fmult,fsum,Sum_error,cont,L
      real*8::F_analitica
      integer::i,j,n,intervalos
      real*8,dimension(1000,50000)::mat
      real*8,dimension(1000,1)::matError
      cont=0
      Sum_error=0.0
      t=ht
      do i=1,intervalos-1
      x=h
      do j=2,n-1
      c=Fmult(t,x)*pendiente


      if (i.eq.1)then
         mat(i+1,j)=mat(i,j)+c*(mat(i,j+1)-2*mat(i,j)+mat(i,j-1))/2.0
          !mat(i+1,j)=F_analitica(x,t,L)
      else
      mat(i+1,j)=2*(1-c)*mat(i,j)+c*(mat(i,j-1)+mat(i,j+1))-mat(i-1,j)

      end if
      if (F_analitica(x,t,L).ne.0)then
      Sum_error=Sum_error+abs(mat(i+1,j)/F_analitica(x,t,L))
      cont=cont+1.0
      end if
      x=x+h
      end do
      matError(i,1)=Sum_error/cont
      Sum_error=0.0
      cont=0.0
      t=t+ht
      end do
      end


      subroutine Error(intervalos,mat,h,ht,n,err,L,matError)
      implicitnone
      real*8::t,x,h,ht,sum,sum2,err,L,F_analitica
      integer::i,j,n,intervalos,contador
      real*8,dimension(1000,50000)::mat
      real*8,dimension(1,50000)::matError
      contador=0
      t=0.0
      do i=1,intervalos
      x=0.0
      do j=1,n
      sum=abs(mat(i,j)-F_analitica(x,t,L))+sum
      sum2=(mat(i,j)-F_analitica(x,t,L))+sum2
      contador=contador+1
      x=x+h
      end do
      matError(1,i)=sum2/n
      sum2=0.0
      t=t+ht
      end do
      err=sum/contador



      end
      
      
      
      
