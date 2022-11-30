! *** 
! P250 - Pset 6  - Main Code File 
! Noah Dohrmann
! Due: 14 December 2021
! *** 
module m6
contains 
!

! place any needed functions and subrotines here ! 

end module m6

program main
! use the above subroutines and functions 
use m6
! want to be very careful that everyhing is defined correctly  
implicit none 

! *** Definitions ***

! The space itself is 81,81 cm (need 81 points in each direction) (the boundary itself
! is implicitly taken to be outside of the array
! IC: T(t = 0) = 0 C 
! BC: T(x = 0, y = 0, x =81, y = 81) = rate*t / 3600
! Diff Const. D = 0.002 cm^2 / s 

integer n, i, j, k ! counters for loops 
real, dimension(81,81) :: temp_array ! the array of the temp for each 0.5x0.5cm cell
real, dimension(81,81) :: delta_array ! the array of temp deltas at each step
real del ! local variable to store temp change 
real D  ! the diffusion coefficient 
real t ! time (time step from one calc to the next) [seconds]
real duration ! the total duration of the simulation
integer n_steps ! the number of total time steps to take to achieve the total duration
real, dimension(50) ::  ramp_rate ! the rate at which the bath temp will increase (deg/hr)
real loc_rate ! the local ramp rate at each loop
real boundary_temp ! the temperature of the boundary at each time (depends on rate)
real max_grad ! the max - min temp  (overall max recorded)
real loc_grad ! the local maximum temp gradient 
real loc_hours ! the local time at each point in hours


! *** Actions ***
D = 0.002  ! diffusion coeff
t = 20.     ! time step 
loc_grad = 0.
max_grad = 0.

! make a file to store the maximum temp gradient
! for each warming rate of the boundary conditions
call execute_command_line("> max_grad_t1_d01")
open(81,file="max_grad_t1_d01",status='unknown',action='write')
! in [Deg / hr] , [Deg]
write(81,*)"Ramp_Rate,Max_Grad"

do i = 1, 50
        ramp_rate(i) = real(i)/40.
enddo


! main loop that changes ramp rate
do k = 1, 50
loc_rate = ramp_rate(k) 
! duration in hours
duration = 15. / loc_rate
! number of time steps to take 
n_steps = int(((duration * 3600) / t) + 0.5) 


!initialize the temperature array
do i = 1, 81
        do j = 1, 81
                ! establish the IC's
                temp_array(i,j) = 0.0
        enddo
enddo

!initialize the delta array
do i = 1, 81
        do j = 1, 81
                delta_array(i,j) = 0.0
        enddo
enddo

! the time loop 
do n = 1, n_steps
!
loc_hours = (real(n)*t)/ 3600. 
boundary_temp = loc_rate * loc_hours

! find the delta arrays for each element
do i = 1, 81
        do j = 1, 81
                !check the boundaries start corners then do edges
                if (i==1 .and. j==1) then
                        del = D * (boundary_temp - 2*temp_array(i,j) + temp_array(i+1,j))
                        del = del + D*(boundary_temp - 2*temp_array(i,j) + temp_array(i,j+1))
                else if (i == 1 .and. j == 81) then
                        del = D * (boundary_temp - 2*temp_array(i,j) + temp_array(i+1,j))
                        del = del + D*(temp_array(i,j-1) - 2*temp_array(i,j) + boundary_temp)
                else if (i == 81 .and. j ==1) then
                        del = D * (temp_array(i-1,j) - 2*temp_array(i,j) + boundary_temp)
                        del = del + D *(boundary_temp - 2*temp_array(i,j) + temp_array(i,j+1))
                else if (i == 81 .and. j == 81) then
                        del = D *(temp_array(i-1,j) - 2*temp_array(i,j) + boundary_temp)
                        del = del + D*(temp_array(i,j-1) - 2*temp_array(i,j) + boundary_temp)
                else if (i == 1) then
                        del = D * (boundary_temp - 2*temp_array(i,j) + temp_array(i+1,j))
                        del = del + D *(temp_array(i,j-1) - 2*temp_array(i,j) + temp_array(i,j+1))
                else if (i == 81) then
                        del = D *(temp_array(i-1,j) - 2*temp_array(i,j) + boundary_temp)
                        del = del + D*(temp_array(i,j-1) -2*temp_array(i,j) + temp_array(i,j+1))
                else if (j == 1) then
                        del = D*(temp_array(i-1,j) - 2*temp_array(i,j) + temp_array(i+1,j))
                        del = del + D*(boundary_temp - 2*temp_array(i,j) + temp_array(i,j+1))
                else if (j == 81) then
                        del = D*(temp_array(i-1,j) -2*temp_array(i,j) + temp_array(i+1,j))
                        del = del + D*(temp_array(i,j-1) -2*temp_array(i,j) + boundary_temp)
                else 
                        del = D*(temp_array(i-1,j) -2*temp_array(i,j) + temp_array(i+1,j))
                        del = del + D*(temp_array(i,j-1) -2*temp_array(i,j) + temp_array(i,j+1))
                endif
                ! adjust by the time step (works w/o if t = 1 s) 
                del = del * t 
                delta_array(i,j) = del
        enddo
enddo

! update the temp array
temp_array = temp_array + delta_array
! check for gradients
loc_grad = maxval(temp_array) - minval(temp_array)
if (loc_grad > max_grad) then
        max_grad = loc_grad
endif
 
! end the time evol loop (at each rate)
enddo

! record data to file 
write(81,*)loc_rate,",",max_grad


! end the ramp rate loop
enddo


close(81)
stop
end program main
