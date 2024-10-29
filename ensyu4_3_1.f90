program chk
    use interface_mod
    implicit none
    character(11) :: c = 'I prefer Pi'
    write(*,*) c
    write(*,*) revchar(c)
end program chk
