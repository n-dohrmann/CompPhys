!*****
! This is a copy of the code that is modified to have the Game of Life
! with two differing teams in competition, @ and #, which will influence
! each other's chances at death or survival when they are in each other's
! vicinity. The rules of the game are provided by the subroutines at the 
! end of the file. 
!
! The only major departure from the spirit of the Original Game of Life
! besides team platy is a modification of the "infinite growth rule", 
! ie. cells are now  allowed to survive in the presence of 4 teammates.
! This is so that the teams may "capture" area of the board, which is 
! counted towards their final score. 
!
!*****
subroutine display(status,n)
dimension status(35,35)
character*70 string

do i = 1, n
        string=' '
        do j = 1, n
                if (status(i,j) == 1.) then
                        string=trim(string)//' @ '
                else if (status(i,j) == 2.) then
                        string=trim(string)//' # '
                else
                        string=trim(string)//' . '
                endif
        enddo
        write(*,*)string
enddo
write(*,*) "-----------"
do j = 1, 4500000
        rn1=rand()
enddo
return
end

module counter
contains 
!
function cell_count_team(status,i,j,n) ! cell counter for team play 
!
!common n, gen, static
real status(n,n)

real cell_count_team(2) !array that holds results for team 1, 2 
real t1, t2 ! count of team 1 and 2
t1 = 0.
t2 = 0.


lower_i = -1
lower_j = -1
upper_i = 1
upper_j = 1

if (i == 1) then
        lower_i = 0
endif
if  (j == 1) then
        lower_j = 0
endif
if (i == n) then
        upper_i = 0
endif
if (j == n) then
        upper_j = 0
endif


do k = lower_i, upper_i
        do l = lower_j, upper_j
                if (k == 0 .and. l == 0) then
                        cycle !dont want to count the cell itself
                endif
                if (status(i+k,j+l) == 1.) then
                        t1 = t1 + 1.
                else if (status(i+k,j+l) == 2.) then
                        t2 = t2 + 1.
                endif
        enddo
enddo

cell_count_team(1) = t1
cell_count_team(2) = t2
return
end function cell_count_team

end module counter


!#### BEGIN MAIN PROGRAM 
program life
!

use counter



!
! n = size
! gen = generation counter
! static = static counter
! last_cc = last cell count 
common n, gen, static, last_cc
common /seed/ x_0
real status(35,35), next(35,35)
integer gen
integer max_gen
n = 35
gen = 0
max_gen = 500
static = 0 
last_cc = 0
two_last_cc = 0 
three_last_cc = 0
x_0 = mod(time(), 17) * 1759


write(*,*)"Enter 1 for manual setup, 0 for random"
read(*,*) ran

!initialize the board 
do i = 1, n
        do j = 1, n
                status(i,j)=0.
                next(i,j) = 0.
        enddo
enddo

if (ran == 1.) then
15       write(*,*)"enter coordinates and team of live cells, 0,0 when done"
read(*,*)i,j,team
if((i == 0.).and.(j == 0.)) then
        goto 17
endif
status(i,j) = team
call display(status,n)
goto 15

else
!
write(*,*)"Enter a five digit number to change seed"
read(*,*)nseed
call srand(nseed)

do i = 1,n
        do j = 1,n
                rn1 = rand()
                
                ! testing equal populations of team @(1) and #(2)
                if (rn1 > 0.75) then
                        status(i,j) = 2.
                else if (rn1 < 0.25) then
                        status(i,j) = 1.
                endif
        enddo
enddo
endif



17 continue 
! everything above this line is copied over from Professor Collar's code


!! main rules of the game here 

!! make the status matrix for the next round of the game 
initial_cc = live_count(status)

! main loop, where the action happens 
do while (gen < max_gen)
        !make the next matrix
        call update(status,next)

        if (gen == 2) then
                two_last_cc = initial_cc 
        endif

        if (gen == 3) then
                three_last_cc = initial_cc
        endif

        ! can turn off display if only doing computation
        call display(status,n)
        !update status
        status = next 
        current_live = live_count(status)

        if (current_live == last_cc .or. current_live == two_last_cc .or. current_live == three_last_cc) then
                static = static + 1
        else
                static = 0
        endif

        ! update last_cc and gen
        three_last_cc = two_last_cc
        two_last_cc = last_cc 
        last_cc = int(current_live)
        gen = gen + 1 

        ! end if static for more than 16 cycles 
        ! might not implement this for the team-based game, 
        ! as it might prevent actually non-static games from
        ! progressing. 
        ! static = 0
        if (static > 16) then
                print *, "Initial Cells: ",initial_cc
                print *, "gen count: ",gen

                if (current_live > 0) then
                        print *, "Ended: ALIVE"
                else
                        print *, "Ended: DEAD"
                endif

                exit ! exits the main loop 
        endif
enddo

if (gen ==  max_gen) then
        print *, "The game ended at ",gen," cycles"
endif

victor = winner(status)
print *, "The winner is team: ",int(victor)



stop
end program life

! *******
! functions and subroutines below 
!
! *******


function cell_count(status,i,j)
        !i, and j are the indices of the matix 
        ! *non* toric version here 
        common n , gen, static
        real status(n,n)

        real cell_count
        cell_count = 0.

        lower_i = -1
        lower_j = -1
        upper_i = 1
        upper_j = 1

        if (i == 1) then
                lower_i = 0
        endif
        if  (j == 1) then
                lower_j = 0
        endif
        if (i == n) then
                upper_i = 0
        endif
        if (j == n) then
                upper_j = 0
        endif

        ! print *, ""
        ! print *, "debug:"
        ! print *, "i, j",i,j
        ! print *, "status(i,j): ",status(i,j)
        ! print *, "lower i :",lower_i
        ! print *, "lower j :",lower_j
        ! print *, "upper i :",upper_i
        ! print *, "upper j :",upper_j

        do k = lower_i, upper_i
                do l = lower_j, upper_j
                        if (k == 0 .and. l == 0) then
                                cycle !dont want to count the cell itself
                        endif
                        if (status(i+k,j+l) == 1.) then
                                cell_count = cell_count + 1.
                        endif
                enddo
        enddo


return
end


! modify update to team conditions 
subroutine update(status,next)
! updates the "next" matrix 
use counter
common  n , gen, static 
real status(n,n)
real next(n,n)
real cc(2)
do i = 1, n
        do j = 1, n
                cc = cell_count_team(status,i,j,n)
                n_1 = int(cc(1))
                n_2 = int(cc(2))

                stat = status(i,j)

                if (stat == 0.) then
                        if (n_1 == 3 .and. n_2 == 0) then
                                next(i,j) = 1.
                        else if (n_1 == 0 .and. n_2 == 3) then
                                next(i,j) = 2.
                        endif
                endif


                if (stat ==1.) then
                        if (n_1 == 0 .and. n_2 == 0) then
                                next(i,j) = 0.
                        else if (n_1 == 0 .and. n_2 == 1) then
                                next(i,j) = 1. ! stay alive for back up
                        else if (n_1 == 1 .and. n_2 == 0) then
                                next(i,j) = 0.
                        else if (n_1 == 0 .and. n_2 == 2) then
                                surv = rand() 
                                if (surv > 0.33) then
                                        next(i,j) = 0.
                                else
                                        next(i,j) = 1.
                                endif
                        else if (n_1 == 1 .and. n_2 == 1) then
                                surv = rand()
                                if (surv > 0.66) then
                                        next(i,j) = 0.
                                else
                                        next(i,j) = 1.
                                endif
                        else if (n_1 == 2 .and. n_2 == 0) then
                                next(i,j) = 1.
                        else if (n_1 == 0 .and. n_2 == 3) then
                                surv = rand()
                                if (surv > 0.10) then
                                        next(i,j) = 0.
                                else 
                                        next(i,j) = 1.
                                endif
                        else if (n_1 == 1 .and. n_2 == 2) then
                                surv = rand()
                                if (surv > 0.50) then
                                        next(i,j) = 0.
                                else 
                                        next(i,j) = 1.
                                endif
                        else if (n_1 == 2 .and. n_2 == 1) then
                                surv = rand()
                                if (surv > 0.90) then
                                        next(i,j) = 0.
                                else 
                                        next(i,j) = 1.
                                endif
                        else if (n_1 == 3 .and. n_2 == 0) then
                                next(i,j) = 1.
                        else if (n_1 == 0 .and. n_2 == 4) then
                                surv = rand()
                                if (surv > 0.05) then
                                        next(i,j) = 0.
                                else
                                        next(i,j) = 1.
                                endif
                        else if (n_1 == 1 .and. n_2 == 3) then
                                surv = rand() 
                                if (surv > 0.40) then
                                        next(i,j) = 0.
                                else
                                        next(i,j) = 1.
                                endif
                        else if (n_1 == 2 .and. n_2 == 2) then
                                surv = rand()
                                if (surv > 0.60) then
                                        next(i,j) = 0.
                                else
                                        next(i,j) = 1.
                                endif
                        else if (n_1 == 3 .and. n_2 == 1) then
                                surv = rand() 
                                if (surv > 0.95) then
                                        next(i,j) = 0.
                                else 
                                        next(i,j) = 1.
                                endif
                        else if (n_1 == 4 .and. n_2 == 0) then
                                next(i,j) = 1.
                        else if (n_1 + n_2 > 4) then
                                next(i,j) = 0.
                        endif
                endif


                if (stat ==2.) then
                        if (n_1 == 0 .and. n_2 == 0) then
                                next(i,j) = 0.
                        else if (n_1 == 0 .and. n_2 == 1) then
                                next(i,j) = 0.
                        else if (n_1 == 1 .and. n_2 == 0) then
                                next(i,j) = 2. ! stay alive for backup
                        else if (n_1 == 2 .and. n_2 == 0) then
                                surv = rand() 
                                if (surv > 0.33) then
                                        next(i,j) = 0.
                                else
                                        next(i,j) = 2.
                                endif
                        else if (n_1 == 1 .and. n_2 == 1) then
                                surv = rand()
                                if (surv > 0.66) then
                                        next(i,j) = 0.
                                else
                                        next(i,j) = 2.
                                endif
                        else if (n_1 == 0 .and. n_2 == 2) then
                                next(i,j) = 2.
                        else if (n_1 == 3 .and. n_2 == 0) then
                                surv = rand()
                                if (surv > 0.10) then
                                        next(i,j) = 0.
                                else 
                                        next(i,j) = 2.
                                endif
                        else if (n_1 == 2 .and. n_2 == 1) then
                                surv = rand()
                                if (surv > 0.50) then
                                        next(i,j) = 0.
                                else 
                                        next(i,j) = 2.
                                endif
                        else if (n_1 == 1 .and. n_2 == 2) then
                                surv = rand()
                                if (surv > 0.90) then
                                        next(i,j) = 0.
                                else 
                                        next(i,j) = 2.
                                endif
                        else if (n_1 == 0 .and. n_2 == 3) then
                                next(i,j) = 2.
                        else if (n_1 == 4 .and. n_2 == 0) then
                                surv = rand()
                                if (surv > 0.05) then
                                        next(i,j) = 0.
                                else
                                        next(i,j) = 2.
                                endif
                        else if (n_1 == 3 .and. n_2 == 1) then
                                surv = rand() 
                                if (surv > 0.40) then
                                        next(i,j) = 0.
                                else
                                        next(i,j) = 2.
                                endif
                        else if (n_1 == 2 .and. n_2 == 2) then
                                surv = rand()
                                if (surv > 0.60) then
                                        next(i,j) = 0.
                                else
                                        next(i,j) = 2.
                                endif
                        else if (n_1 == 1 .and. n_2 == 3) then
                                surv = rand() 
                                if (surv > 0.95) then
                                        next(i,j) = 0.
                                else 
                                        next(i,j) = 2.
                                endif
                        else if (n_1 == 0 .and. n_2 == 4) then
                                next(i,j) = 2.
                        else if (n_1 + n_2 > 4) then
                                next(i,j) = 0.
                        endif
                endif
                        


                ! if (stat == 0. .and. n_cc == 3) then
                !         next(i,j) = 1.
                ! endif
                ! if (n_cc >= 4) then
                !         next(i,j) = 0.
                ! endif
                ! if (n_cc == 3 .or. n_cc == 2) then
                !         next(i,j) = 1.
                ! endif
                ! if (n_cc < 2) then
                !         next(i,j) = 0.
                ! endif
        enddo
enddo
return
end

function live_count(status)
!
common n, gen, static, last_cc
real status(n,n)
integer live_count
live_count = 0 

do i = 1, n
        do j = 1, n
                if (status(i,j) == 1. .or. status(i,j) == 2.) then
                        live_count = live_count + 1
                endif
        enddo
enddo

return
end

! determine which team is the winner - whoever has control of more area
function winner(status)
!
common n, gen, static, last_cc
real status(n,n)
winner = 0.
team_1 = 0.
team_2 = 0.
a1 = 0.
a2 = 0.

lower_i = -1
lower_j = -1
upper_i = 1
upper_j = 1

! re-implement strategies from 'cell_count_team' here since 
! getting them to work together was giving errors

do i = 1, n
        do j = 1, n
                if (i == 1) then
                        lower_i = 0
                endif
                if  (j == 1) then
                        lower_j = 0
                endif
                if (i == n) then
                        upper_i = 0
                endif
                if (j == n) then
                        upper_j = 0
                endif
                if (status(i,j) == 1.) then
                        team_1 = team_1 + 1
                else if (status(i,j) == 2.) then
                        team_2 = team_2 + 1
                !this if statement compares the blank space to
                ! its surroundings, giving points to either team
                ! if they have more cells in the area 
                else if (status(i,j) == 0.) then
                        do k = lower_i,upper_i
                        do l = lower_j,upper_j
                        if(k ==0 .and. l==0) then
                                cycle
                        endif
                        if(status(i+k,j+l) == 1.) then
                                a1 = a1 + 1
                        else if (status(i+k,j+l) == 2.)then
                                a2 = a2 + 1
                        endif
                        if (a1 > a1) then
                                team_1 = team_1 + 1
                                a1 = 0
                                a2 = 0
                        else if (a2 > a1) then
                                team_2 = team_2 + 1 
                                a1 = 0
                                a2 = 0
                        else
                                a1 = 0
                                a2 = 0
                        endif
                        enddo
                        enddo
                endif
        enddo
enddo

if (team_1 > team_2) then
        winner = 1. ! team 1 wins 
else if (team_2 > team_1) then
        winner = 2. ! team 2 wins 
else
        winner = 0. ! tie 
endif

!debug 
! print *, "Area 1: ",team_1
! print *, "Area 2: ",team_2

print *, "Team 1: ",int(team_1/(team_1+team_2)*100 + 0.5),"%"
print *, "Team 2: ",int(team_2/(team_1+team_2)*100 + 0.5),"%"

return
end


! re-use random number generator 
! from pset 1 
subroutine lin_cong(rnd)
        common /seed/ x_0
        integer m, a, x_new, x_temp
        !print *, "debug: x_0: ",x_0
        x_temp = int(x_0) 
        m = 2**32
        a = 11

        x_new = mod((a * x_temp),m)
        !print *, "x_new ", x_new
        x_0 = x_new
        rnd = dble(x_new)

        

return 
end

