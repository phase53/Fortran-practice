module subprogs
    implicit none
contains
    subroutine count
        integer :: fo = 11
	integer, save :: ic = 1
        
	if(ic == 1) then
	    open(fo, file = 'output.d')
	endif

	write(fo,*) ic

	if(ic == 10) then
	    close(fo)
	    stop
	endif

        ic = ic + 1
    end subroutine count
end module subprogs

program main
    use subprogs
    implicit none

    do
        call count
    enddo
end program main
	
