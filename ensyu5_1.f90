module subprogs
    implicit none
contains
    subroutine gaiseki(a,b,g,alp)
        real(8),intent(in) :: a(1:3), b(1:3)
	real(8) :: c(1:2,1:3), vl
	real(8),intent(out) :: g(1:3)
	real(8),intent(in),optional :: alp
	integer :: i

        c(1,1:3) = a(1:3)
        c(2,1:3) = b(1:3)
    
        do i = 1, 3
            c = cshift(c, 1, 2)
            g(i) = c(1,1) * c(2,2) - c(1,2) * c(2,1)
        enddo

        if(present(alp)) then
	    vl = alp/sqrt(dot_product(g,g))
	else
	    vl = 1._8/sqrt(dot_product(g,g))
	endif

        g(:) = vl * g(:)
    end subroutine gaiseki
end module subprogs

program main
    use subprogs
    implicit none
    real(8) :: a(1:3), b(1:3), g1(1:3), g2(1:3)
    
    call random_seed
    call random_number(a)
    call random_number(b)

    call gaiseki(a,b,g1,3._8)
    call gaiseki(a,b,g2)
    write(*,*) 'a =', a
    write(*,*) 'b =', b
    write(*,*) 'a \times b(norm = 3)=', g1
    write(*,*) 'a \times b(norm = 1) =', g2

end program main



