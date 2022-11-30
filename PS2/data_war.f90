!*****
! Copy of "war_games.f90" for data production 
!
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
dimension status(64,64)
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
! counts the number of cells of each team around the target cell
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

!use the module defined above 
use counter



!
! n = size
! gen = generation counter
! static = static counter
! last_cc = last cell count 
common n, gen, static, last_cc

! initial seed for lin_cong
common /seed/ x_0
real status(64,64), next(64,64)
integer gen
integer max_gen, n_trials, n_counter, n_spaces
common /adv/ t2_adv

! unfair advantage given to team 2 , boost their survival odds
t2_adv = 10.

! board size
n = 64
gen = 0
!stop game at 500 gens for war games
max_gen = 500
static = 0 
last_cc = 0
two_last_cc = 0 
three_last_cc = 0
x_0 = mod(time(), 17) * 1759
n_trials = 100000
n_counter = 0
n_spaces = n**2

! start board at 30% capacity, give random distribution to teams
! from here, then we will see how balanced the winning results are compared to 
! the initial distribution 

!n_cells_to_gen = int((0.3 * n**2) + 0.5)
!chance_to_gen = real(n_cells_to_gen)/real(n_spaces)




call execute_command_line('echo "Initial_Team1_Frac,End_Team1" > war_data')

! anything below this line should change game-by-game
21 continue

!make the distribution between teams 1 and 2. Ratio of team 1 cells to 
! team 2 cells 
dist = rand()

! debugging file 
open(2,file='lin_cong_out',status='unknown',position='append')
write(2,*)dist
close(2)


!write(*,*)"Enter 1 for manual setup, 0 for random"
!read(*,*) ran
ran = 0.
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
call lin_cong(rseed)
nseed = int(mod(time(),int(rseed))*5) 
call srand(nseed)


do i = 1,n
        do j = 1,n
                rn1 = rand()
                
                ! testing weighted populations for team @(1) and #(2)
                if (rn1 > 0.70) then
                        chance_t1 = rand()
                        if (chance_t1 < dist) then
                                status(i,j) = 1.
                        else
                                status(i,j) = 2.
                        endif
                endif
        enddo
enddo
endif

! find the actual starting distribution between the teams 
start_actual_dist = r_initial_dist(status)



17 continue 


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
        !call display(status,n)
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

victor = winner(status)
!print *, "The winner is team: ",int(victor)

!write intial fraction team 1 then victor to csv file 
open(1, file='war_data',status='unknown',position='append')
write(1,*)start_actual_dist,",",victor
close(1)


n_counter = n_counter + 1 

! just so I can see if the code is running as intended 
if (mod(n_counter,1000) == 0) then
        print *, n_counter/1000," k"
endif

if (n_counter < n_trials) then
        goto 21 ! loop another game 
else
        continue  ! end the program 
endif


call execute_command_line("sed 's/ //g' war_data > temp")
call execute_command_line("cat temp > war_data; rm temp")


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
                        else if (status(i+k,j+l) == 2.) then
                                cell_count = cell_count + 2.
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
common /adv/ t2_adv
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
                                if (surv > 0.33*t2_adv) then
                                        next(i,j) = 0.
                                else
                                        next(i,j) = 2.
                                endif
                        else if (n_1 == 1 .and. n_2 == 1) then
                                surv = rand()
                                if (surv > 0.66*t2_adv) then
                                        next(i,j) = 0.
                                else
                                        next(i,j) = 2.
                                endif
                        else if (n_1 == 0 .and. n_2 == 2) then
                                next(i,j) = 2.
                        else if (n_1 == 3 .and. n_2 == 0) then
                                surv = rand()
                                if (surv > 0.10*t2_adv) then
                                        next(i,j) = 0.
                                else 
                                        next(i,j) = 2.
                                endif
                        else if (n_1 == 2 .and. n_2 == 1) then
                                surv = rand()
                                if (surv > 0.50*t2_adv) then
                                        next(i,j) = 0.
                                else 
                                        next(i,j) = 2.
                                endif
                        else if (n_1 == 1 .and. n_2 == 2) then
                                surv = rand()
                                if (surv > 0.90*t2_adv) then
                                        next(i,j) = 0.
                                else 
                                        next(i,j) = 2.
                                endif
                        else if (n_1 == 0 .and. n_2 == 3) then
                                next(i,j) = 2.
                        else if (n_1 == 4 .and. n_2 == 0) then
                                surv = rand()
                                if (surv > 0.05*t2_adv) then
                                        next(i,j) = 0.
                                else
                                        next(i,j) = 2.
                                endif
                        else if (n_1 == 3 .and. n_2 == 1) then
                                surv = rand() 
                                if (surv > 0.40*t2_adv) then
                                        next(i,j) = 0.
                                else
                                        next(i,j) = 2.
                                endif
                        else if (n_1 == 2 .and. n_2 == 2) then
                                surv = rand()
                                if (surv > 0.60*t2_adv) then
                                        next(i,j) = 0.
                                else
                                        next(i,j) = 2.
                                endif
                        else if (n_1 == 1 .and. n_2 == 3) then
                                surv = rand() 
                                if (surv > 0.95*t2_adv) then
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
                        if (a1 > a2) then
                                team_1 = team_1 + 1
                                a1 = 0.
                                a2 = 0.
                        else if (a2 > a1) then
                                team_2 = team_2 + 1 
                                a1 = 0.
                                a2 = 0.
                        else
                                a1 = 0.
                                a2 = 0.
                        endif
                        enddo
                        enddo
                endif
        enddo
enddo

! if (team_1 > team_2) then
!         winner = 1. ! team 1 wins 
! else if (team_2 > team_1) then
!         winner = 2. ! team 2 wins 
! else
!         winner = 0. ! tie 
! endif

!debug 
! print *, "Area 1: ",team_1
! print *, "Area 2: ",team_2

t1 = (team_1/(team_1+team_2))*100
t2 = (team_2/(team_1+team_2))*100

! in this version of winner, we just return the difference in team 1 % 
! to team 2 %. (positive percent indicates winner)

winner =  t1 - t2 


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

function r_initial_dist(status)
!
common n, gen, static, last_cc
real status(n,n)
real r_initial_dist
team_1 = 0. 
team_2 = 0.

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
                endif
        enddo
enddo

r_initial_dist = team_1/team_2
return
end
