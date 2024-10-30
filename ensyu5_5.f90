module subprogs
    implicit none
    interface print_mat
        module procedure print_mat_r_m, print_mat_r_h, print_mat_i_m, print_mat_i_h
    end interface
contains
    subroutine print_mat_r_m(a,n,m,title)
        integer,intent(in) :: n, m
        real(8),intent(in) :: a(n,m)
        character(*),optional,intent(in) :: title
        integer :: i

        if(present(title)) then
            write(*,*) title
        endif

        do i = 1, n
            write(*,*) a(i,1:m)
        enddo
    end subroutine print_mat_r_m
        
    subroutine print_mat_r_h(a,title)
        real(8),intent(in) :: a(:,:)
        character(*),optional,intent(in) :: title
        integer :: i

        if(present(title)) then
            write(*,*) title
        endif

        do i = 1, size(a,1)
            write(*,*) a(i,1:size(a,2))
        enddo
    end subroutine print_mat_r_h

    subroutine print_mat_i_m(a,n,m,title)
        integer,intent(in) :: n, m
        integer,intent(in) :: a(n,m)
        character(*),optional,intent(in) :: title
        integer :: i

        if(present(title)) then
            write(*,*) title
        endif

        do i = 1, n
            write(*,*) a(i,1:m)
        enddo
    end subroutine print_mat_i_m

    subroutine print_mat_i_h(a,title)
        integer,intent(in) :: a(:,:)
        character(*),optional,intent(in) :: title
        integer :: i

        if(present(title)) then
            write(*,*) title
        endif

        do i = 1, size(a,1)
            write(*,*) a(i,1:size(a,2))
        enddo
    end subroutine print_mat_i_h

end module subprogs

program main
    use subprogs
    implicit none
    real(8) :: a(1:3,1:2)
    integer :: b(1:2,1:4)

    call random_seed
    call random_number(a)
    b(1,1:4) = (/1,2,3,4/)
    b(2,1:4) = (/5,6,7,8/)
    call print_mat(a,3,2,'test')
    call print_mat(a)
    call print_mat(b,2,4)
    call print_mat(b,'test2')
end program main

