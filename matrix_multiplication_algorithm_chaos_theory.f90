program calcul
implicit none
integer, parameter :: n = 100
integer, parameter :: m = 41
integer i,j,k,k1,k2,k3, ii, iii,p
real*8 a(n,n), b(n,n), c(n,n), d(n,n), e(n,n), f(m), s, s1, u, u1, u2, u3, medie, u4, u5, u6
open(1, file='date.txt')

do i=1,n
do j=1,n
call random_number(s)
call random_number(s1)
a(i,j)=s
b(i,j)=s1
enddo
enddo
c = matmul(a,b)

medie = 0.0
!do iii=1,n
!do j = 1,n
!medie = medie+ b(iii,j)/n/n
!enddo
!enddo


f(1) = 0
!do ii=1,10
do i = 2,m
call random_number(u)
call random_number(u1)
call random_number(u2)
call random_number(u3)
k = 1 + floor((100+1-1)*u)
k1 = 1 + floor((100+1-1)*u1)
!k2 = 1 + floor((3+1-1)*u2)
!k3 = 1 + floor((3+1-1)*u3)
call random_number(u4)
call random_number(u5)
call random_number(u6)
f(i) = a(1,1)*(b(k,k1)+a(1,1)/u4/u5/u6) !This is where the magic happens. 
if ((f(i)-f(i-1)) > 100) then
!medie = f(i)/2
!write(1,*) c(1,1) - medie
endif

write(1,*)c(1,1) - f(i), f(i)
enddo
!write(1,*) c(1,1) - medie



f = 0
!enddo
close(1)
!do i=1,100
!call random_number(u)
!k = 1 + floor((3+1-1)*u)
!write(*,*)k
!enddo



!write(*,*)c

!write(*,*)a
!write(*,*)b

!call random_number(s)

!do i=1,100
!call random_number(s)
!write(*,*)s
!enddo



!a(1,1) = 1
!a(1,2) = 2
!a(2,1) = 3
!a(2,2) = 4

!b(1,1) = 1
!b(1,2) = 2
!b(2,1) = 3
!b(2,2) = 4

!c = matmul(a,b)/4325968212.
!d(1,1) = a(1,1)*b(2,1)
!d(1,2) = a(1,1)*b(2,1)
!d(2,1) = a(1,1)*b(2,1)
!d(2,2) = a(1,1)*b(2,2)

!f(1,1) = (d(1,1)+d(1,2)+d(2,1)+d(2,2))/4/4325968212.

!write(*,*)c
!write(*,*)f



end program calcul

!If someone runs this script, he will obtain a file that presents the fact that the solutions obtaine are sometimes pozite, sometimes negative.
!As implied, if a solution is negative and anoter positive, since we have considered that the atractor is the actual solution, this means that
!we have the exact solution between the solutions given by the algorithm, so that a simple numerical method, such as interval splicing can be
!used to determin the solution of the matrix multiplication.
!Important to note that since this algorithm involves the trunchiation errors, the formula used to make the process chaotic will have to be
!adjusted for every computer architecture in question. In some cases it may turn the process into a chaotic one, in other cases it may not,
!depending on the processor type, gpu type, etc... Probably a universal formula can be obtained, one that will work on any architecture, but
!I did not find one. I'm that type of programmer: It works on my computer... :))))))
!The script was compiled in gfortran. This also may affect the behavior. Very sensitive to the fact that depending on the architecture, compiler, etc... (maybe other factors
!I do not know about), the process may not turn chaotic and the multiplication may not give correct results
