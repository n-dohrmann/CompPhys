subroutine display(status,n)
dimension status(35,35)
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

do j = 1, 4500000
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
real status(35,35), next(35,35)
integer gen
integer max_gen
n = 35
gen = 0
max_gen = 2000
static = 0 
last_cc = 0


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
write(*,*)"Enter a five digit number to change seed"
read(*,*)nseed
call srand(nseed)

do i = 1,n
        do j = 1,n
                rn1 = rand()
                if (rn1 < 0.75) then
                        status(i,j) = 0.
                else
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


        ! can turn off display if only doing computation
        call display(status,n)
        !update status
        status = next 
        current_live = live_count(status)
        if (current_live == last_cc) then
                static = static + 1
        else
                static = 0
        endif

        ! update last_cc and gen
        last_cc = current_live
        gen = gen + 1 

        ! end if static for more than 16 cycles 
        if (static > 16) then
                print *, "Initial Cells: ",initial_cc
                print *, "gen count: ",gen

                if (current_live > 0) then
                        print *, "Ended: ALIVE"
                else
                        print *, "Ended: DEAD"
                endif

                exit
        endif
enddo

if (gen ==  max_gen) then
        print *, "The game survived ",gen," cycles"
endif




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

