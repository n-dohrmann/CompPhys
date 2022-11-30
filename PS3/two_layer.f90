!******
! P250 - Pset 3 - Noah Dohrmann 
! Two Layer Perceptron Extension 
!
! To run this you need to use the following function in the shell 
! with usage
!    "fort <code> <csv_file>"
! 
! function fort() {
! 		# $2 etc are optional arguments to be passed at run time 
!       # Number of rows 
! 		rows=$(cat $2 | wc -l | awk '{print $1}') # awk deletes whitespace
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

! no need for a module if all matrix computation is done in
! the main code

program main
!
implicit none
character :: rows, columns
integer :: n_rows, n_columns, n_reduced, n, j ! n and j are generic counters
double precision a1 ! var used to store user input later
double precision user_in ! whether to ask for user input

! modify the below to the case of three neurons total (two layers) 
! First  layer of A and B, the second layer is just C 
! I = training input 
! T = training output (correct output)
! w = weights (going towards the A, B, and C neurons)
! O = output from transfer function
! D = delta (used for backpropagation) 
double precision, allocatable, dimension(:,:) :: I
double precision, allocatable, dimension(:,:) :: T 
double precision, allocatable, dimension(:,:) :: wa
double precision, allocatable, dimension(:,:) :: wb
double precision, allocatable, dimension(:,:) :: wc
double precision, allocatable, dimension(:,:) :: OA
double precision, allocatable, dimension(:,:) :: OB
double precision, allocatable, dimension(:,:) :: OC
double precision, allocatable, dimension(:,:) :: DA
double precision, allocatable, dimension(:,:) :: DB
double precision, allocatable, dimension(:,:) :: DC
double precision, allocatable, dimension(:,:) :: array
double precision, allocatable, dimension(:,:) :: vec
double precision, allocatable, dimension(:,:) :: local_err
!might not need this array
!double precision, allocatable, dimension(:,:) :: local_err

! 1,1 imtermediate ("imd") matrices needed to avoid dimensionality 
! issues when operating on scalar values within matrices
double precision, dimension(1,1) :: wa_c_imd
double precision, dimension(1,1) :: wb_c_imd
double precision, dimension(1,1) :: output


double precision a ! the learning rate 
integer epochs ! number of epochs to test over
double precision avg_err ! average the local err

!double precision output(1,1)

! whether we will use the sigmoid transfer function 
double precision sigmoid
!double precision avg_err

! max file length of this many characters 
character*30 csv_file

! get  the arguments passed to the executable at run time 
call get_command_argument(1,csv_file)
call get_command_argument(2,rows)
call get_command_argument(3,columns)


! change from string to int 
read(rows,*) n_rows
read(columns,*) n_columns

print *, "rows:", n_rows
print *, "columns: ", n_columns

! confirm size of input array 
allocate(array(n_rows,n_columns))

!allocate the user input vector shape for user input
allocate(vec(1,n_columns-1))
vec = reshape(vec,[1,n_columns-1])

! csv should be in the same directory, accessible for fortran compiler
! to read 
open(10,file=csv_file,access='sequential',status='old')
read(10,*) array
close(10)

! need to take into account that fortran reads things in a 
! column major rather than row major way.
array = transpose(array)


allocate(I(n_rows,n_columns-1))
I = reshape(I, [n_rows,n_columns-1])

n_reduced = n_columns -1 
do n = 1, n_rows
        do j = 1, n_reduced
                I(n,j) = array(n,j)
        enddo
enddo




T = reshape(array(:,n_columns),[n_rows,1])


! initialize weights with random values centered around zero. 

!reset the random number generator
!call srand(time())

allocate(wa(n_columns-1,1))
do n =1, n_columns-1
        wa(n,1) = -1 + 2*rand()
enddo
wa = reshape(wa,[n_columns-1,1])

allocate(wb(n_columns-1,1))
do n =1, n_columns-1
        wb(n,1) = -1 + 2*rand()
enddo
wb = reshape(wb,[n_columns-1,1])

! the weights for the C neuron are just a 2,1 vector  

! allocate(wc(n_columns-1,1))
! do n =1, n_columns-1
!         wc(n,1) = -1 + 2*rand()
! enddo
! wc = reshape(wc,[n_columns-1,1])

allocate(wc(2,1))
do n = 1, 2
        wc(n,1) = -1 * 2*rand()
enddo


allocate(OA(n_rows,1))
OA = reshape(OA,[n_rows,1])

allocate(OB(n_rows,1))
OB = reshape(OB,[n_rows,1])


allocate(OC(n_rows,1))
OC = reshape(OC,[n_rows,1])

allocate(DA(n_rows,1))
DA = reshape(DA,[n_rows,1])

allocate(DB(n_rows,1))
DB = reshape(DB,[n_rows,1])

allocate(DC(n_rows,1))
DC = reshape(DC,[n_rows,1])

! **********
! below implement the two layer network 
!
! Architecture: 
! R^3 input vector 
! 2 Neurons in the "hidden" layer (call them A, B)
! 1 Output layer neuron, gives the final "yes or no" output,
!    call this neuron C 
! 
! Notes:
! Need to be careful to correctly backpropagate with the 
! delta function. 
! 
! Steps:
! - Go through each layer of neurons to do the forward prop.,
!   then go backwards and correct the weights by the delta function.
! 
! **********

! local err vec for fining matrix error at each epoch
allocate(local_err(n_rows,1))
local_err = reshape(local_err,[n_rows,1])

a = 9.
epochs = 100000
sigmoid = 1.

open(20, file="tl_err",status='unknown')
write(20,*)"Epoch,AverageErr"

do n = 1, epochs
        ! using sigmoid transfer function, Steps:
        !
        ! Forward prop from input to neurons A, B
        ! Collect output, forward prop to C
        ! Collect output from C
        ! Find Delta for the C weights, correct C weights
        ! find delta for A and B weights, correct A and B weights
        
        !plug into first two neurons 
        OA = 1./(1. + exp(-matmul(I,wa)))
        OB = 1./(1. + exp(-matmul(I,wb)))

        !then plug into C 
        OC = 1./(1. + exp(-(OA*wc(1,1) + OB*wc(2,1))))

        ! find the local matrix error
        local_err = 0.5 * (OC - T)**2
        avg_err = 0.

        do j = 1, n_rows
                avg_err = avg_err + local_err(j,1)
        enddo
        avg_err = avg_err / dble(n_rows)
        write(20,*)n,",",avg_err

        !then find the delta for C, which is also used for deltas in A
        ! and B 
        DC = (OC - T)*(OC*(1. - OC))

        ! correct the weights used with neuron C 
        wa_c_imd = matmul(transpose(OA),DC)
        wc(1,1)  = wc(1,1) - a*wa_c_imd(1,1)
        wb_c_imd = matmul(transpose(OB),DC)
        wc(2,1)  = wc(2,1) - a*wb_c_imd(1,1)

        ! find deltas for neurons A and B
        DA = (wc(1,1)*DC)*(OA*(1. - OA))
        DB = (wc(2,1)*DC)*(OB*(1. - OB))

        !correct the weights used with neurons A and B 
        wa = wa - a*matmul(transpose(I),DA)
        wb = wb - a*matmul(transpose(I),DB)


enddo

! ask for user input below 

! do not need user in for error data production
user_in = 0.

if (user_in == 1.) then

do while (2 > 0)
        print *, "please give",n_columns-1,"vals for input, or enter '2' to exit"

        do n = 1, n_columns-1
                read *, a1
                if (a1 == 2.) goto 10
                vec(1,n) = a1
        enddo

        ! feed to A and B 
        OA = 1./(1. + exp(-matmul(vec,wa)))
        OB = 1./(1. + exp(-matmul(vec,wb)))

        OC = 1./(1. + exp(-(OA*wc(1,1) + OB*wc(2,1))))

        output = OC(1,1)
        print *, "gives output:",output

enddo

endif ! end the user_in if statement 

10 continue

print *, ""
print *, "run time complete!"
print *, ""

stop
end program main
