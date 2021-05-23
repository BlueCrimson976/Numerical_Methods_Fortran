
! Trapezoid Rule : h/2 *(y1 + y2) = area of 1 trapezoid
! Summation of all the areas of trapezoid
! f(x) = 1/x , ans = 0.695635

program Trapezoid
        real :: upper_limit , lower_limit , tarea
        
        print*, "Enter the Upper and lower limits : "
        read*, upper_limit , lower_limit
        
        call Summation(upper_limit , lower_limit , tarea)
        
        print*, "The area within the interval is : " , tarea

end program Trapezoid
!________________________________________________________
real function curve(x)
      implicit none
      real :: x
      curve = 1/x
end function curve


subroutine area_1_trap(u_b , l_b , p1 , p2 , dx , trap)
        real :: u_b , l_b ,  p1 , p2 , trap
        integer , parameter :: n = 1000
        real :: dx
        dx = (u_b - l_b)/n
        y1 = curve(p1)
        y2 = curve(p2)
        trap = (dx/2.0)*(y1 + y2)
end subroutine area_1_trap

subroutine Summation(u_b , l_b , area)
        integer , parameter :: n = 1000
        real :: trap ,dx , p1 , p2
        real :: u_b , l_b , area , initial = 0.0 
        integer :: count
        do count = 1 , n-1
            p1 = l_b + float(count)*((u_b - l_b)/float(n))
            p2 = l_b + float(count + 1)*((u_b - l_b)/float(n))
            call area_1_trap(u_b , l_b , p1 , p2 , dx , trap)
            area = initial + trap
            initial = area
        end do
end subroutine Summation		
            
            
! Working!!!! 		

        
    
    
    