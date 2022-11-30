! *****
! This is a copy of the code for data production, so the "display" part is not necessary 
! 
! will write relevant data to csv files 
! *****
subroutine display(status,n)
dimension status(64,64)
character*70 string

do i = 1, n
        string=' '
        do j = 1, n
                if (status(i,j) == 1.) then
                        string=trim(string)//' @ '
                else
                        string=trim(string)//' . '
                endif
        enddo
        write(*,*)string
enddo

do j = 1, 9000000
        rn1=rand()
enddo

return
end

!#### BEGIN MAIN PROGRAM 
program life
!
! n = size
! gen = generation counter
! static = static counter
! last_cc = last cell count 
common n, gen, static, last_cc
common /seed/ x_0
real status(64,64), next(64,64)
integer gen, max_gen, dead_or_alive
real r 

! number of times for the main code to run 
integer n_trials, n_counter

common /len/ n_cells_to_gen

integer n_spaces

! is this the best way to do this ? 
integer, allocatable :: ncoords(:,:)

! initialize this file once (1) for everytime this file is run 
call execute_command_line('echo "Initial,Perc,Gen,Alive?" > life_data_vanilla')
n_trials = 1000
n_counter = 0

x_0 = mod(time(), 17) * 1759
! loop the main program again
21 continue


n_spaces = n**2 ! the number of cells on the board 
!call lin_cong(c)
call random_number(r)
n_cells_to_gen = int(r * n**2)

allocate(ncoords(n_cells_to_gen, 2))




! 0 = dead, 1 = alive 
dead_or_alive = 0
n = 64
gen = 0
max_gen = 5000
static = 0 
last_cc = 0
two_last_cc = 0 
three_last_cc = 0


!write(*,*)"Enter 1 for manual setup, 0 for random"
!read(*,*) ran

!always set to zero for data production 
ran = 0.




!initialize the board 
do i = 1, n
        do j = 1, n
                status(i,j)=0.
                next(i,j) = 0.
        enddo
enddo

if (ran == 1.) then
15       write(*,*)"enter coordinates of live cells, 0,0 when done"
read(*,*)i,j
if((i == 0.).and.(j == 0.)) then
        goto 17
endif
status(i,j) = 1.
call display(status,n)
goto 15

else
!
!write(*,*)"Enter a five digit number to change seed"
!read(*,*)nseed
call lin_cong(rseed)
nseed = int(rseed)
call srand(nseed)

!make random coords for the cells 
! some cells are getting set twice 


! want a certain percent of the board filled 
call lin_cong(threshold)

do i = 1, n
        do j = 1, n
                call lin_cong(rng)
                if (rng < threshold) then
                        status(i,j) = 1.
                endif
        enddo
enddo




!do i = 1, n_cells_to_gen
!         !call lin_cong(r)
!         n_x = int(rand() * n + 0.5)
!         if (n_x < 1) n_x = n_x + 1
!         !call lin_cong(s)
!         n_y = int(rand() * n + 0.5) 
!         if (n_y < 1) n_y = n_y + 1 
!         ncoords(i,1) = n_x
!         ncoords(i,2) = n_y
!enddo

!put the cells into the status array 
!do i = 1, n_cells_to_gen
!        !print *, "x: ",ncoords(i,1)
!        !print *, "y: ",ncoords(i,2)
!        status(ncoords(i,1),ncoords(i,2)) = 1.
!enddo

        




! comment out below intialization block 

! do i = 1,n
!         do j = 1,n
!                 rn1 = rand()
!                 if (rn1 < 0.75) then
!                         status(i,j) = 0.
!                 else
!                         status(i,j) = 1.
!                 endif
!         enddo
! enddo

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

        if (gen > 4900 .and. gen < 4910) then
                call display(status,n)
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
        last_cc = current_live
        gen = gen + 1 

        ! end if static for more than 16 cycles 
        ! should also check if same to 2 gens ago 
        if (static > 16) then
                ! print *, "Initial Cells: ",initial_cc
                ! print *, "gen count: ",gen

                if (current_live > 0) then
                        !print *, "Ended: ALIVE"
                        dead_or_alive = 1
                else
                        !print *, "Ended: DEAD"
                        dead_or_alive = 0
                endif

                exit
        endif
enddo

! print *, "init: ",initial_cc
!print *, "n: ",n
n_spaces = n**2
!print *, "spaces: ", n_spaces

!if (initial_cc == 0 .and. 

perc = real(initial_cc) / n_spaces
! print *, "perc: ",perc
open(1, file="life_data_vanilla",status="unknown",position="append")
!write(1,*)"Initial_Count,Gen,Alive?"
write(1,*)initial_cc,",",perc,",",gen,",",dead_or_alive
close(1)

if (gen ==  max_gen) then
        ! print *, "The game survived ",gen," cycles"
        gen = 0 
endif


n_counter = n_counter + 1

if (mod(n_counter, 50) == 0) then
        print *, n_counter
endif


if (n_counter < n_trials) then
        deallocate(ncoords)
        goto 21
else
        continue
endif

call execute_command_line("sed 's/ //g' life_data_vanilla > temp")
call execute_command_line("cat temp > life_data_vanilla; rm temp")

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


subroutine update(status,next)
! updates the "next" matrix 
common  n , gen, static 
real status(n,n)
real next(n,n)
do i = 1, n
        do j = 1, n
                cc = cell_count(status,i,j)
                n_cc = int(cc)
                stat = status(i,j)

                if (stat ==0.) then
                        if (n_cc == 3) then
                                next(i,j) = 1.
                        endif
                endif

                if (stat == 1.) then
                        if (n_cc < 2) then
                                next(i,j) = 0.
                        endif
                        if (n_cc == 2 .or. n_cc ==3) then
                                next(i,j) = 1.
                        endif
                        if (n_cc > 3) then
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
                if (status(i,j) == 1.) then
                        live_count = live_count + 1
                endif
        enddo
enddo

return
end

! re-use random number generator 
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
