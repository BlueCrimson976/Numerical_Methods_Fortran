program bisect
    
    real :: a , b , c ! Initial bounds , c = midpoint
    real , parameter :: e = 1.0E-06 ! e = accuracy   [Scientific Notation of 10*(-6)]
    real , parameter :: pi = 3.14159
    integer , parameter :: n = 80 
    integer ::  ni = 0 ! To find out the number of iterations
    integer :: count
    
    a = pi/2      !! between pi/2 and (3/2)*pi
    b = 1.5*pi 

    do  count = 1 , n                                  
        y_a = f(a)
        y_b = f(b) 
        if (y_a*y_b < 0) then
            call midpoint(a , b , c)
            y_c = f(c)
            if (abs(y_c) <= e) then 
               exit
            end if
            call interval_choice(a , b , c)
        end if  
        ni = ni + 1 
    end do    
    
    print*, "The solution of equation is :", c
    print*, "The number of iterations is :", ni
    

end program bisect

!--------------------------------------------------------------------------------------

!Subroutines used in the code

! function (equation of curve)
real function f(x)
    implicit none
    real :: x 
    f = sin(x) 
end function f   
    
! to calculate the midpoints
subroutine midpoint(b1 , b2 , mid)
    implicit none
    real :: b1 , b2 , mid
    mid = (b1 + b2) / 2 
end subroutine midpoint

! Choosing the variable to swap with c to make a right choice of interval

subroutine interval_choice(bound1 , bound2 , mid)
 
    real :: bound1 , bound2 , mid
    real :: y1 , y2 , y3
    y1 = f(bound1) 
    y2 = f(bound2) 
    y3 = f(mid)
    if(y1*y3 < 0) then
        bound2 = mid
    elseif (y2*y3 < 0) then
        bound1 = mid    
    end if
end subroutine interval_choice   