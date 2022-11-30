! ***
! P250 - Problem Set 4: Monte Carlo
! Due: 18 November 2021
! Noah Dohrmann
!
! 
! ### This is a vanilla version of the code just used as a proof
! of concept ### 
!
! ***

module p4
contains
!
function sqr(x,y)
        !checks whether a point falls within the bounds 
        real h
        h = sqrt(1-x**2)
        if (y < h) then
                sqr = 1.
        else
                sqr = 0.
        endif
        return
end function

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
real, dimension(:), allocatable :: rands
real rnd, lin_max
rnd = 0.
lin_max = 0. 

x_0 = 65539
n_seed = mod(time(),17) * 6559
call srand(n_seed)

!

n_samples = 2000000000
n_trials = n_samples/2
allocate(rands(n_samples))
n_hits = 0

do i = 1, n_samples
        rnd =  lin_cong(x_0)
        rands(i) = rnd
enddo

do i = 1, n_samples
        if (rands(i) > lin_max) then
                lin_max = rands(i)
        endif
enddo

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
print *, "The approximation is: ",(4.0 *(2.0*real(n_hits)/real(n_samples)))

! this method gives a number that is correct to 5 digits:
! pi ~ 3.14160752
!
! (counting the 6 in "60" as right since the real value has a "59" 
! there)



end program main
