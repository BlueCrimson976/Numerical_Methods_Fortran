!Newton Raphson Method Prototype 1 2.0 * x**3 - 2.5 * x - 5


program N_R

    real :: initial , final
    
    print*, "Enter the initial guess"
    read*, initial

    call Newton_Raphson(initial , final)

end program N_R    

!________________________________________________________________________

! Curve Equation
real function y(x)
     implicit none
     real :: x
     y = 2.0 * x**3 - 2.5 * x - 5
end function y  

! Finding derivative using First Principal
real function dif(point)
     real :: point
     real , parameter :: h = 1.0E-06 !! Should be same as precision(accuracy)
     y1 = y(point)
     y2 = y((point + h))
     dif = (y2 - y1)/h
end function dif     

! Newton Raphson Logic / Subroutine
subroutine Newton_Raphson(point , point2) ! point = guess point , point2 = Estimated point
    real :: point , point2
    real , parameter :: accuraccy = 1.0E-06
    real :: f , f_prime 
    integer :: count 
    integer :: n = 0 ! To keep track of number of iterations

    f = y(point)
    if (abs(f) <= accuraccy) then
        print*, "The root of the equation is : " , point
    else     
       do count = 1 , 300
           f = y(point)
           f_prime = dif(point)
           point2 = point - (f/f_prime)
           f2 = y(point2)
           if(abs(f2) <= accuraccy) then
              print*, "The root is :" , point2 
              exit      
           end if  
           point = point2
           n = n + 1  
       end do
    end if

    print*, "The number of iterations is : " , n   

end subroutine Newton_Raphson    
        