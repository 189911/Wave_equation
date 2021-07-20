      PROGRAM hiperbolica
      IMPLICIT NONE
      REAL*8 L,H,HT,ALPHA,R,U,A,PI,X,TI,T1,TF,T,T2,M,MAT2
      INTEGER I,J,K,N1,N2,P
      character*100 z
      DIMENSION A(100,100),U(100,100),TI(100),M(100,100),MAT2(100,100)
      
      PI=ACOS(-1.0)
      !Condiciones de la cuerda
      TF=2  !Tiempo final
      L=5   !Longitud de la cuerda
      H=0.1  !delta de x
      HT=0.001  !delta de t
      ALPHA=1  !constante 1/velocidad

      R=ALPHA*HT/H !Constante

      N1=INT(L/H)
      N2=INT(TF/HT)

      OPEN(5,FILE='HIPER.TXT')
      WRITE(5,*)N1,N2

      !LAS MATRICES SE HACEN 0
      DO I=1,N2+1
         DO J=1,N1+1
            U(I,J)=0
         END DO
      END DO
      
      DO I=2,N1
         DO J=2,N1
            A(I,J)=0
         END DO
      END DO
      
      !CONDICIONES INICIALES
      X=0
      I=1
      DO WHILE(X.LE.L)
         U(1,I) = -SIN(PI*X) !Funci¢n de condiciones iniciales
         !WRITE(5,*)U(0,I)
         !WRITE(*,*)X
         X=X+H
         I=I+1
      END DO
      !Condiciones en la frontera
      DO I=1,N2+1
         U(I,1)=0
         U(I,9)=0
      END DO

      X=2*H
      I=2
      DO WHILE(X.LE.L)
         DO J=I-1,I+1
            IF(I.NE.2.OR.J.NE.1)THEN
            IF(J.EQ.I-1.OR.J.EQ.I+1)THEN
                A(I,J)=-R**2/2
            ELSE
                A(I,J)=(2.0+R**2)
            END IF
            END IF
         END DO
         TI(I)=(R**2/2)*(U(1,I-1)-2*U(1,I)+U(1,I+1))+2*U(1,I)
         WRITE(5,*)TI(I)
         A(I,N1+1)=TI(I)
         X=X+H
         I=I+1
      END DO

      WRITE(5,*)" "
      WRITE(5,*)"   "
      WRITE(5,*)"MATRIZ DE COFACTORES"
      DO I=2,N1
         WRITE(5,*)(A(I,J),J=2,N1+1)
      END DO

      CALL GAUSS(A,N1+1)

      DO I=2,N1+1
         U(2,I)=A(I,N1+1)
      END DO

      !SIGUIENTES MATRICES DE COFACTORES
      K=2
      T=2*HT
      DO WHILE(T.LE.TF)
         DO I=2,N1
            TI(I)=0
            DO J=2,N1+1
               A(I,J)=0
            END DO
         END DO
         X=2*H
         I=2
         DO WHILE(X.LE.L)
            DO J=I-1,I+1
               IF(I.NE.2.OR.J.NE.1)THEN
               IF(J.EQ.I-1.OR.J.EQ.I+1)THEN
                  A(I,J)=-R**2/4
               ELSE
                  A(I,J)=(1.0+R**2/2)
               END IF
               END IF
            END DO
            !T‚rmino independiente
            T1=0.25*(U(K-1,I-1)-2*U(K-1,I)+U(K-1,I+1))
            T2=(R**2)*(T1+.5*(U(K,I-1)-2*U(K,I)+U(K,I+1)))
            TI(I)=T2+2*U(K,I)-U(K-1,I)
            A(I,N1+1)=TI(I)
            X=X+H
            I=I+1
         END DO
         WRITE(5,*)" "
         DO I=2,N1
            WRITE(5,*)(A(I,J),J=2,N1+1)
         END DO
         CALL GAUSS(A,N1+1)
         DO I=2,N1+1
            U(K+1,I)=A(I,N1+1)
         END DO
         T=T+HT
         K=K+1
      END DO

      WRITE(5,*)" "
      DO I=1,N2+1
         WRITE(5,101)(U(I,J),J=1,N1+1)
      END DO
      !Se llena la matriz de la que se va a graficar
      X=0
      DO I=1,N2+2
         DO J=1,N1+1
            IF(I.EQ.1)THEN
               M(J,I)=X
               X=X+H
            ELSE
               M(J,I)=U(I-1,J)
            END IF
         END DO
      END DO
      
      OPEN(6,FILE="implicito.txt")
      WRITE(5,*)" "
      DO I=1,N1+1
         WRITE(5,102)(M(I,J),J=1,N2+2)
         WRITE(6,*)(M(I,J),J=1,N2+2)
      END DO
      CLOSE(5)
      CLOSE(6)
      
      !Script de gnuplot para graficar
      OPEN(7,FILE="implicito.plt")
      WRITE(7,*)'set title "Metodo implicito" font ",12"'
      write(7,*)"set grid"
      write(7,*)"set zeroaxis"
      write(7,*)'set xrange[0:',l,']'
      !write(7,*)'set yrange[-1.2:0]'
      write(7,*)'set xlabel "x" font ",12" '
      write(7,*)'set ylabel "u(x,t)" font ",12" '
      write(7,*)'plot "implicito.txt" using 1:2 w lp'
      do i=3,N2+1
         write(7,*)'replot "implicito.txt" using 1:',i,' w lp'
      end do
      close(7)
      CALL SYSTEM('implicito.plt')

  101 FORMAT(9F9.5)
  102 FORMAT(10F9.5)
      PAUSE
      END PROGRAM
      
      !RESOLVER LA MATRIZ
      subroutine gauss(mat1,B)
      integer a,b
      real*8, dimension (100,101):: mat1,mat2,mat3
      a=B-1


       l=2
      do while(l.le.(a-1))
      if (mat1(l,l).eq.0)then
      do while (mat1(l,l).eq.0)
      do i=l+1,a
      do j=1,b
      mat1(l,j)=mat1(l,j)+mat1(i,j)
      end do
      end do
      end do
      end if


      r=1/(mat1(l,l))
      do j=1,b
      mat1(l,j)= mat1(l,j)*r
      end do


      do i=l+1,a
      f=mat1(i,l)*mat1(l,l)

      do j=1,b
      mat1(i,j)=mat1(i,j)-mat1(l,j)*f
      end do

      end do
      l=l+1

      end do


      r=1/(mat1(l,l))
      do j=2,b
      mat1(l,j)= mat1(l,j)*r
      end do

      do while(l.ge.2)
      i=l-1
      do while(i.ge.1)
      f=mat1(i,l)*mat1(l,l)


       j=b
      do while(j.ge.l)
      mat1(i,j)=mat1(i,j)-mat1(l,j)*f

      j=j-1
      end do
      i=i-1

      end do
      l=l-1

      end do


      do n=2,a
      write(5,*) n,"U=",mat1(n,b)
      end do
      RETURN
      end subroutine

