program moji
    use interface_mod
    implicit none
    character(5) :: c = 'hello'
    call print_title(c)
    call print_title('good bye')
end program moji
