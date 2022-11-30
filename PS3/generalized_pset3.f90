!******
! P250 - Pset 3 - Noah Dohrmann 
! Due: 16 November 2021
! Artificial Neural Networks 
!
! generalized version of the code that should be adaptable
! to differeing forms of input from a csv file, whose last column
! is the training output (everything else is a training input). 
! To run this you need to use the following function in the shell 
! with usage
!    "fort <code> <csv_file>"
! 
! function fort() {
! 		# $2 etc are optional arguments to be passed at run time 
!       # Number of rows 
! 		rows=$(cat $2 | wc -l | awk '{print $1}')
!       # number of columns
! 		cols=$(head -1 $2 | grep -o "," | wc -l)
!       # increase the number of cols by once since defined by number of commas
! 		((cols += 1))
!       # debug statements, not necessary 
! 		echo "rows: "$rows
! 		echo "cols: "$cols
!       # compilation
! 		gfortran -g -fcheck=all -Wall  $1 -o temp && ./temp $2 $rows $cols && rm temp
! }
!
!******

program main
!

! I = training input 
! T = training output (correct output)
! w = weights 
! O = output from transfer function
! D = delta (used for backpropagation) 
character :: rows, columns
integer :: n_rows, n_columns
double precision, allocatable, dimension(:,:) :: I
double precision, allocatable, dimension(:,:) :: T 
double precision, allocatable, dimension(:,:) :: w
double precision, allocatable, dimension(:,:) :: O
double precision, allocatable, dimension(:,:) :: D
double precision, allocatable, dimension(:,:) :: array
double precision, allocatable, dimension(:,:) :: local_err
double precision, dimension(8,3) :: input_vecs  ! the input vec for all cases 
!double precision, dimension(1,3) :: local_vec


double precision a ! the learning rate 
integer epochs
double precision, allocatable, dimension(:,:) :: vec

! these two used to test the weights at each epoch
! (changed the error scheme so that they are not needed)
!double precision test_out(1,1)
!double precision test_err

double precision output(1,1)
double precision time_total, avg_time
double precision grand_average ! average time over the 10000 trials
double precision user_input ! whether to take user input 
double precision timing ! whether to do the method timing

! whether we will use the sigmoid or tanh as the transfer function
! needs to be edited when ran with each instance. 1 = sigmoid, 0 = tanh 
double precision sigmoid
double precision avg_err


! max file length of this many characters 
character*30 csv_file
! remember that if a character string is allocated too 
! many bites for what it is later assigned, then it will be truncated

sigmoid = 1.
! just to make me aware of what I set it
print *, "sigmoid is: ",sigmoid

! get  the arguments passed to the executable at run time 
call get_command_argument(1,csv_file)
call get_command_argument(2,rows)
call get_command_argument(3,columns)

! change from string to int 
read(rows,*) n_rows
read(columns,*) n_columns

! confirm size of input array 
allocate(array(n_rows,n_columns))

! csv should be in the same directory
open(10,file=csv_file)
read(10,*) array
close(10)

allocate(I(n_rows,n_columns-1))
I = array(:,1:n_columns-1)
T = reshape(array(:,n_columns),[n_rows,1])

! initialize weights with random values centered around zero. 
allocate(w(n_columns-1,1))
do n = 1, n_columns-1
        w(n,1) = -1 + 2*rand()
enddo
w = reshape(w,[n_columns-1,1])

! do n = 1, n_columns-1
!         print *, w(n,:)
! enddo

allocate(O(n_rows,1))
O = reshape(O,[n_rows,1])
allocate(D(n_rows,1))
D = reshape(D,[n_rows,1])

allocate(local_err(n_rows,1))
local_err = reshape(local_err,[n_rows,1])

a = 500.
epochs = 100000

! change to higher vals when doing timing 
n_trials = 1!00
n_counter = 0
! average across 100 trials of fining the optimized weights
grand_average = 0.

timing = 0. ! set timing to false for the latter half of the proj.

if (timing == 1.) then

10 continue

if (sigmoid == 1.) then
!
do n = 1, epochs
        call cpu_time(start)
        O = 1./(1. + exp(-matmul(I,w)))
        D = (O - T) * (O*(1. - O))
        w = w - a * matmul(transpose(I),D)
        call cpu_time(finish)
        time_total = time_total + (finish - start)
enddo

avg_time = time_total/dble(epochs)
grand_average = grand_average + avg_time

else if (sigmoid == 0.) then
!
do n = 1, epochs
        call cpu_time(start)
        O = (1. - exp(-2*matmul(I,w)))/(1. + exp(-2*matmul(I,w)))
        D = (O - T) * (1 - O**2)
        w = w - a * matmul(transpose(I),D)
        call cpu_time(finish)
        time_total = time_total + (finish - start)
enddo

avg_time = time_total/dble(epochs)
grand_average = grand_average + avg_time


endif

if (n_counter < n_trials) then
        n_counter = n_counter + 1 
        if (mod(n_counter,5)==0) print *, real(n_counter)/real(n_trials)
        do n = 1, n_columns-1
                w(n,1) = 1.
        enddo
        goto 10
else
        grand_average = grand_average / n_trials
        if (sigmoid ==1.) print *, "For the sigmoid function:"
        if (sigmoid ==0.) print *, "For the tanh function:"
        print *, "the grand average is:",grand_average
endif

!else ! do this if not timing the computation
endif

! *** Learning rate optimization below 

! these vectors are no longer needed for the current version of the code
input_vecs(1,:) = [0,0,0]
input_vecs(2,:) = [0,0,1]
input_vecs(3,:) = [0,1,0]
input_vecs(4,:) = [0,1,1]
input_vecs(5,:) = [1,0,0]
input_vecs(6,:) = [1,0,1]
input_vecs(7,:) = [1,1,0]
input_vecs(8,:) = [1,1,1]


if (sigmoid == 1.) then
!

open(20, file="sigmoid_err",status='unknown')
write(20,*)"Epoch, Average Absolute Error"

do n = 1, epochs
        O = 1./(1. + exp(-matmul(I,w)))
        D = (O - T) * (O*(1. - O))
        w = w - a * matmul(transpose(I),D)
        local_err = 0.5 * (T - O)**2
        avg_err = 0.
        ! do j = 1, 8
        !         true = 0.
        !         if (j > 4) true = 1.
        !         test_out = 1./(1. + exp(-matmul(reshape(input_vecs(j,:),[1,3]),w)))
        !         test_err = abs(true - test_out(1,1)) 
        !         avg_err = avg_err + test_err
        ! enddo
        do j = 1, n_rows
                avg_err = avg_err + local_err(j,1)
        enddo
        avg_err = avg_err / dble(n_rows)

        write(20,*)n,",",avg_err


enddo

else if (sigmoid == 0.) then ! tanh case 

open(20, file="tanh_err",status='unknown')
write(20,*)"Epoch,AverageAbsoluteError"
!
do n = 1, epochs
        O = (1. - exp(-2*matmul(I,w)))/(1. + exp(-2*matmul(I,w)))
        D = (O - T) * (1 - O**2)
        w = w - a * matmul(transpose(I),D)
        local_err = 0.5 * (T - O)**2
        avg_err = 0.
        ! do j = 1, 8
        !         true = 0.
        !         if (j > 4) true = 1.
        !         local_vec = reshape(input_vecs(j,:),[1,3])
        !         test_out = (1. - exp(-2*matmul(local_vec,w)))/(1. + exp(-2*matmul(local_vec,w)))
        !         test_err = abs(true - test_out(1,1))
        !         avg_err = avg_err + test_err
        ! enddo
        do j = 1, n_rows
                avg_err = avg_err + local_err(j,1)
        enddo
        avg_err = avg_err/ dble(n_rows)
        write(20,*)n,",",avg_err
enddo

!endif

endif


!allocate the user input vector shape
allocate(vec(1,n_columns-1))
vec = reshape(vec,[1,n_columns-1])

! dont want user input for error data production
user_input = 1.
if (user_input == 1.) then

do while (2 > 0)
!
print *, "please give ",n_columns-1," vals for input"

do n = 1, n_columns-1
        read *, a1 
        vec(1,n) = a1
enddo

if (sigmoid == 1.) then
        output = 1./(1. + exp(-matmul(vec,w)))
else if (sigmoid == 0.) then
        output = (1.-exp(-2*matmul(vec,w)))/(1.+exp(-2*matmul(vec,w)))
endif


print *, "gives: ",output
output = 0.
print *, ""

enddo
endif

print *, ""
print *, "run time complete!"
print *, ""

stop
end program main
