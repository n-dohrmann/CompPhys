! ***
! P250 - Problem Set 4: Monte Carlo
! Due: 18 November 2021
! Noah Dohrmann
!
!  ** higher ints, reals not doubles ***
!
! this is a copy of the code to test the greater than or equal to 
! (gte) change in checking for hits . 
! 
! ***

! using a module here to contain all the relevant functions and subroutines
module p4
contains
!
function sqr(x,y)
        !checks whether a point falls within the bounds of the circle
        !return 1. if inside, 0. if the point is outside of the circle 
        real h, x, y
        h = sqrt(1-x**2)
        if (y <= h) then
                sqr = 1.
        else
                sqr = 0.
        endif
        return
end function

! recycled random number generator from pset1 .
function lin_cong(x_0)
      integer m, a, x_new, x_temp
      real lin_cong
      !print *, "debug: x_0: ",x_0
      x_temp = int(x_0) 
      m = 2**32
      a = 11

      x_new = mod((a * x_temp),m)
      !print *, "x_new ", x_new
      x_0 = x_new
      lin_cong = x_new
return 
end

end module 



program main
!
use p4

! explicit intiializations below
real :: x, y
real , dimension(:), allocatable :: rands
real rnd, lin_max
integer(kind=int64) trial_list(40)
double precision PI, approx, perc_err

! define the "true" value of pi from intrrinsic functions
PI=4.D0*DATAN(1.D0)

do i =1, 40
        trial_list(i) = int(2**i)
enddo


call execute_command_line('> real_lte_data')
open(1, file="real_lte_data",status="unknown",position="append")
write(1,*)"Trials,Percent_Error"

n_counter = 1

! line number to be used for looping

do while (n_counter < 40)
!

print *, "trial: ",n_counter

rnd = 0.
lin_max = 0. 

! reset the rand number generator so that it is different each run
x_0 = 65539
n_seed = mod(time(),17) * 6559
call srand(n_seed)

!

n_trials = trial_list(n_counter)
! number of samples of random numbers to take, 2 per trial
n_samples = 2*n_trials
allocate(rands(n_samples))
n_hits = 0

do i = 1, n_samples
        rnd =  lin_cong(x_0)
        rands(i) = rnd
enddo

! need to normalize the values obtained by lin cong 
! generator - first find the maximum value
do i = 1, n_samples
        if (rands(i) > lin_max) then
                lin_max = rands(i)
        endif
enddo

! then divide each value by the maximum, ensuring none of the 
! values are now greater than one
do i = 1, n_samples
        rands(i) = rands(i) / lin_max
        if (rands(i) > 1.) print *, "error"
enddo



! the formula that we want to use is 
! A_tot = 4 A,  = pi/4 --> pi = 4 (n_hits/n_samples)
! A = hits / samples
! A_tot = pi

do i = 1, n_samples/2
        x = rands(2*i - 1)
        y = rand(2*i)
        hit = sqr(x,y)
        if (hit == 1.) then
                n_hits = n_hits + 1 
        endif
enddo

! adding a factor of 2 since there are 2 as many randos generated as there 
! coordinate pairs generated
approx = (4.0 *(2.0*dble(n_hits)/dble(n_samples)))
perc_err = dble(PI-approx)/dble(PI)
perc_err = abs(perc_err)

write(1,*)n_trials,",",perc_err


! this method gives a number that is correct to 5 digits:
! pi ~ 3.14160752
!
! (counting the 6 in "60" as right since the real value has a "59" 
! there)

! iterate the counter by one
n_counter = n_counter + 1
! the rands array needs to be made new again for the next runthrough
deallocate(rands)

!end the loop, the file should be written now 
enddo



! make sure to close the file  (usually doesnt matter)
close(1)


end program main
