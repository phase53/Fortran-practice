program main
    use subprogs
    implicit none
    call enzan
    write(*,*) a, b
    write(*,*) c(:)
    d = d + b
    write(*,*) d
end program main
