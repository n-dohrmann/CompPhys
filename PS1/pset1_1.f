C******************************************************
C File: 1 (of _ ) 
C
C Title: Random Number Tester - Pset 1 Phys 250
C Date: Due 21 October 2021 
C Author: Noah Dohrmann 
C
C Info: This is my version of the code provided by Prof. Collar 
C with my own changes as well as the other exercises for this 
C problem set. 
C
C (for me): Remember if ``implicit none" is not used, then all
C variables i-n will be assumed to be ints !!! 
C
C
C******************************************************

      program main

      ! how to make only certain variables follow these implicit
      ! statements? 
      ! 
      ! implicit double precision (B-H)
      ! implicit double precision (O-Z)

      ! Notes on Seeds: 
      ! use professor C's method of keeping the seed as a global
      ! variable and then changing it on each iteration - this
      ! eliminates the need to rely so heavily on arrays 


      ! The other values used as seeds are recorded in the rdn_test.out2
      ! files !! 

      common ix, x_0
      common /seeds/ n_s1, n_s2, n_s3
      ! common seed x_0
      ! this is the definition in which we will scan for random numbers
      ! create bins for the trial functions we will be using 
      dimension bins_randm(100000)
      dimension bins_rand(100000)
      dimension bins_randu(100000)
      dimension bins_rando(100000)
      dimension bins_lin(100000)
      dimension bins_wich(100000)

      real, dimension(10000000) :: lin_vals
      !dimension bins_bbs(100000)
      !dimension bbs_vals(10000000)

      double precision wich_index 
      !be 100% sure these are reals 
      real lin_max
      real precis

      ! misc declatation for below 
      real list(1000) 
      !real :: s, p, q 
      x_0 = 65539

      ! use some primes as additional seeds
      n_s1 = 5021
      n_s2 = 17047
      n_s3 = 23981

      ! intialize a seed for use in later functions 

      ! make var that holds this value
      precis = 100000. 

      ! test the same iseed as the original paper given on canvas
      iseed = 314159

      !debugging statements 
      print *, ""
      print *, "*** Debugging statements below ***"
      print *, ""

      ! put zero in each element of the array 
      do j = 1, int(precis)
              bins_randm(j)=0.
      enddo

      ! end the common block ?  --> used in seed generation for randm 
      ix = 777

      ! size of samples to take 
      rmc = 100.*precis

      ! confidence level of testing (change to 6 sigma) for other tests
      cl = 4. 

      !create files to store output 
      ! purpose of out1? 
      open(1, file="rndtest.out1",status='unknown')
      open(2, file="rndtest.out2",status='unknown')

      write(2,*)""
      write(2,*)" *** Seeds used: ***"
      write(2,*)"ix (randm): ",ix
      write(2,*)"iseed (randu): ",iseed
      write(2,*)"x_0 (lin cong): ",x_0
      write(2,*)"nn_s1, nn_s2, nn_s3 (WH): ",n_s1,n_s2,n_s3
      write(2,*)"" 
      write(2,*)"" 
      ! make more files with increasing numbers (but not 6) 

      ! fill the bins with random numbers from randm
      open(23, file="randm_generated",status="unknown")
      write(23,*)"Call,Result"

      do x = 1, rmc
              call randm(rn1)

              if (rn1 < 0) then !should never be less than zero right? 
                      rn1 = 0. 
              endif

              !write to result file
              write(23,*)int(x),",",rn1

              rn1 = rn1*precis ! scale by the precision value 
              index = int(rn1) + 1  ! creates an index where the first
              ! index is 1 

              ! make sure to not cross upper bound 
              if (index > int(precis)) then
                      index = int(precis)
              endif

              ! add +1 to that index of the bin 
              bins_randm(index) = bins_randm(index)+1  
      enddo

      ! write the bin number and number of bin hits from the randm
      ! method into a csv file 
      !     
      open(3, file="randm_bin_csv",status='unknown') 
      write(3,*)"Bin_Number,Counts" 
      do i = 1, precis
              write(3,*)i,",",int(bins_randm(i)) 
      enddo




      ! the expected average value (truly even dist)
      averag = rmc/precis
      ! standard error of the average 
      stderr = sqrt(averag)

      ! print the label information to the out file 
      write(2,*) ""
      write(2,*) "**** For: randm(seed) ****" 
      write(2,*) ""
      write(2,*) "The expected average of counts per bin is: ",averag
      write(2,*) "These bins fell more than ",cl," std errs away" 
      write(2,*) "Bin#        Bin_Count         #sigma away from mean" 

      do j = 1, int(precis)
              dev = abs( bins_randm(j) - averag)/stderr
              if (dev > cl) then
                      !record each instance that falls outside the 
                      !tolerance threshold 
                      !have to be careful not to cross the line limit
                 write(2,*)j,bins_randm(j),(bins_randm(j)-averag)/stderr
              endif
      enddo

C***
C This next block of code will test the intrinsic rand()
C function
C***


      !make new bins for the next function, repeat many steps from
      ! above 

      ! use same precis as before 

      do j = 1, int(precis)
              bins_rand(j) = 0.
      enddo

      !use same rmc number, cl 

      ! fill the bins with random numbers from randm


      open(24, file="rand_generated",status="unknown")
      write(24,*)"Call,Result"

      !use same open file #2 for now 

      do x = 1, rmc
              a = dble(rand())

              if (a < 0) then
                      a = 0. 
              endif

              write(24,*)int(x),",",a

              a = a * precis
              index = int(a) + 1 

              if (index > int(precis)) then
                      index = int(precis)
              endif

              ! add to index count again 
              bins_rand(index) = bins_rand(index) + 1 
      enddo

      ! use same average and standard err (the bin numbers should be
      ! the same) 

      !put a few blank lines in the out file 
      write(2,*) "" 
      write(2,*) "" 
      write(2,*) "" 
      write(2,*) "**** For: rand() ****" 
      write(2,*) ""

      write(2,*) "The expected average count per bin is: ", averag
      write(2,*) "These bins fell more than ",cl," std errs away"
      write(2,*) "Bin#          Bin_Count       #sigma from mean" 

      do j = 1, int(precis)
              dev = abs(bins_rand(j) - averag)/stderr
              if (dev > cl) then
                 write(2,*)j,bins_rand(j),(bins_rand(j)-averag)/stderr
         endif
      enddo

      ! write the bin number and number of bin hits from the randm
      ! method into a csv file 
      !     
      open(4, file="rand_bin_csv",status='unknown') 
      write(4,*)"Bin_Number,Counts" 
      do i = 1, precis
              write(4,*)i,",",int(bins_rand(i))
      enddo

C***
C This next block of code will test the randu(seed) function
C function
C***

      ! again, we will use the same constants as before to repeat
      ! the main steps of the populating the bins 

      do j = 1, int(precis)
              bins_randu(j) = 0.
      enddo


      !print *, randu(iseed) 

      open(25, file="randu_generated",status="unknown")
      write(25,*)"Call,Result"

      do x = 1, rmc
              a =  randu(iseed) 

              if (a < 0) then
                      a = 0.
              endif

              write(25,*)int(x),",",a

              a = a * precis 
              index = int(a) + 1 

              if (index > int(precis)) then
                      index = int(precis)
              endif

              !add to index count again
              bins_randu(index) = bins_randu(index) + 1 
      enddo

      ! again use the same average and standard err and write to the
      ! same out2 file 

      write(2,*) ""
      write(2,*) ""
      write(2,*) ""
      write(2,*) "**** For: randu(iseed) ****"
      write(2,*) ""

      write(2,*) "The expected average count per bin is: ", averag
      write(2,*) "These bins fell more than ",cl," std errs away"
      write(2,*) "Bin#          Bin_Count       #sigma from mean"

      do j = 1, int(precis)
              dev = abs(bins_randu(j) - averag)/stderr
              if (dev > cl) then
                 write(2,*)j,bins_randu(j),(bins_randu(j)-averag)/stderr
              endif
      enddo

      ! create new file to host this csv data 
      open(5, file="randu_bin_csv",status='unknown')
      write(5,*)"Bin_Number, Counts"
      do i = 1, precis
              write(5,*)i,",",int(bins_rand(i))
      enddo


C***
C This next block of code will test the rando function
C function
C***

      !call rando(rando) 
      do j = 1, int(precis)
              bins_rando(j)=0.
      enddo

      open(27, file="rando_generated",status="unknown")
      write(27,*)"Call,Result"

      do x = 1, rmc
              call random_number(a)
              a = dble(a) 

              if (a < 0) then
                      a = 0.
              endif

              write(27,*)int(x),",",a

              a = a * precis
              index = int(a) + 1 

              if(index > int(precis)) then
                      index = int(precis)
              endif

              bins_rando(index) = bins_rando(index) + 1
      enddo


      write(2,*) ""
      write(2,*) ""
      write(2,*) ""
      write(2,*) "**** For: random_number() ****"
      write(2,*) ""

      write(2,*) "The expected average count per bin is: ", averag
      write(2,*) "These bins fell more than ",cl," std errs away"
      write(2,*) "Bin#          Bin_Count       #sigma from mean"

      do j = 1, int(precis)
              dev = abs(bins_rando(j)-averag)/stderr
              if (dev > cl) then
                 write(2,*)j,bins_rando(j),(bins_rando(j)-averag)/stderr 
              endif
      enddo


      open(7, file="rando_bin_csv",status='unknown')
      write(7,*)"Bin_Number, Counts"
      do i = 1, precis
              write(7,*)i,",",int(bins_rando(i))
      enddo





C ******************    This block is just a proof of concept. 


      ! testing the igp function code here 
      ! 14423 16073
      !max = 0.
      !s = 3.
      !p = 14423.
      !q = 16073.
      !do j = 1, 1000
      !        list(j) =  bbs(s, j, p,q) 
      !        print *, "rand out: ",list(j)
      !        if (list(j) > max) then
      !                max = list(j)
      !        endif
      !enddo

      !! now make a normalization loop
      !do j =1, 1000 
      !        list(j) = list(j)/max
      !enddo
      !!print *, "The list is: ", list
C ******************      


      !s = 3.
      !p = 14423.
      !q = 16073.

      !! now for the main part of the code with bbs 

      !do j = 1, int(precis)
      !        bins_bbs = 0.
      !enddo


      !!need to get into the habit of putting r in front of reals ugh
      !r_max_bbs = bbs(s, int(rmc), p, q)

      !do i =1, rmc
      !        bbs_vals(x) = bbs_vals(x)/r_max_bbs

      !        if (bbs_vals(x) < 0) then
      !                bbs_vals(x) = 0.
      !        endif

      !        bbs_vals(x) = bbs_vals(x) * precis
      !        index = int(bbs_vals(x)) + 1 

      !        if (index > int(precis)) then
      !                index = int(precis)
      !        endif

      !        bins_bbs(index) = bins_bbs(index) + 1 

      !enddo
      
      

      !# put blum blum shub method on hold for now 


      !max = 0. 
      !do x = 1, rmc


      !        a = bbs(s, int(x), p, q)

      !        if (mod(int(x), 1000) == 0) then
      !                print *, "on term ",x
      !        endif

      !        if (a < 0) then
      !                a = 0.
      !        endif

      !        !normalize the sequence by the maximum value 
      !        if(a > max) then
      !                max = a
      !        endif

      !        a = a/max 

      !        a = a * precis
      !        index = int(a) + 1 

      !        if(index > int(precis)) then
      !                index = int(precis) 
      !        endif

      !        bins_bbs(index) = bins_bbs(index) + 1 
      !enddo

      ! write(2,*) ""
      ! write(2,*) ""
      ! write(2,*) ""
      ! write(2,*) "**** For: random_number() ****"
      ! write(2,*) ""

      ! write(2,*) "The expected average count per bin is: ", averag
      ! write(2,*) "These bins fell more than ",cl," std errs away"
      ! write(2,*) "Bin#          Bin_Count       #sigma from mean"

      ! do j = 1, int(precis)
      !         dev = abs(bins_bbs(j)-averag)/stderr
      !         if (dev > cl) then
      !               write(2,*)j,bins_bbs(j),(bins_bbs(j)-averag)/stderr
      !         endif
      ! enddo

      ! open(8, file="bbs_bin_csv",status='unknown')
      ! write(8,*)"Bin_Number, Counts"
      ! do i = 1, precis
      !         write(7,*)i,",",bins_bbs(i)
      ! enddo


C***
C This next block of code will test the Inversive Congruential
C generators method as outlined by 
C 
C https://en.wikipedia.org/wiki/Linear_congruential_generator
C*** 


      ! debug 
      call lin_cong(rnd)
      print *, "sample rnd: ",rnd

      do i=1, rmc
              call lin_cong(rnd)
              lin_vals(i) = abs(real(rnd)) 
              if (i ==51) then
                      print *, "lin val ",lin_vals(i)
              endif
      enddo

      lin_max = 0.
      
      do i = 1, rmc
              if (lin_vals(i) > lin_max) then
                      lin_max = lin_vals(i)
              endif
      enddo
      print *, "the max is ",lin_max

      do i = 1, rmc
              lin_vals(i) = real(lin_vals(i))/real(lin_max) 
      enddo

      do j = 1, int(precis)
              bins_lin(j) = 0.
      enddo

      open(28, file="lin_generated",status="unknown")
      write(28,*)"Call,Result"

      do i = 1, rmc
              if (lin_vals(i) < 0) then
                      lin_vals(i) = 0.
              endif

              write(28,*)int(i),",",lin_vals(i)

              lin_vals(i) = lin_vals(i)*precis
              index = int(lin_vals(i)) + 1 

              if (index > int(precis)) then
                      index = int(precis)
              endif

              bins_lin(index) = bins_lin(index) + 1 
              
      enddo

      !put a few blank lines in the out file 
      write(2,*) "" 
      write(2,*) "" 
      write(2,*) "" 
      write(2,*) "**** For: Linear Congruence Generator  ****" 
      write(2,*) ""

      write(2,*) "The expected average count per bin is: ", averag
      write(2,*) "These bins fell more than ",cl," std errs away"
      write(2,*) "Bin#          Bin_Count       #sigma from mean" 

      do j = 1, int(precis)
              dev = abs(bins_lin(j)-averag)/stderr
              if (dev > cl) then
                   write(2,*)j,bins_lin(j),(bins_lin(j)-averag)/stderr
              endif
      enddo

      open(9, file="lin_bin_csv",status='unknown')
      write(9,*)"Bin_Number,Counts"
      do i = 1, precis
              write(9,*)i,",",int(bins_lin(i))
      enddo

C*********
C code for the Wichmann-Hill Method as outlined by 
C https://en.wikipedia.org/wiki/Wichmann%E2%80%93Hill
C*********

      ! debugging calls 
      call wichmann_hill(wich)
      print *, "wichmann 1",wich
      call wichmann_hill(wich)
      print *, "wichmann 2",wich
      call wichmann_hill(wich)
      print *, "wichmann 3",wich
      call wichmann_hill(wich)
      print *, "wichmann 4",wich
      call wichmann_hill(wich)
      print *, "wichmann 5",wich

      do j = 1, int(precis)
              bins_wich(j) = 0.
      enddo

      open(29, file="wich_generated",status="unknown")
      write(29,*)"Call,Result"

      do x = 1, rmc
              call wichmann_hill(rnd)

              if (rnd < 0.) then
                      !this method, when done with abs() before 
                      !returning, gave numbers skewed towards the lower
                      ! end of the interval [0-1]. To fix this, I add
                      ! 1.0 to numbers smaller than 1, which is seen to
                      ! improve performance by quite a lot 
                      rnd = rnd + real(1.00000) 
              endif

              write(29,*)int(x),",",rnd

              rnd = rnd * precis

              index = int(rnd) + 1 

              ! this doesn't seem to be happening? 
              if (index > int(precis)) then
                      index = int(precis)
              endif

              bins_wich(index) = bins_wich(index) + 1
      enddo

      write(2,*) ""
      write(2,*) ""
      write(2,*) ""
      write(2,*) "**** For: Wichmann-Hill  ****"
      write(2,*) ""

      write(2,*) "The expected average count per bin is: ", averag
      write(2,*) "These bins fell more than ",cl," std errs away"
      write(2,*) "Bin#          Bin_Count       #sigma from mean"

      do j = 1, int(precis)
              dev = abs(bins_wich(j)-averag)/stderr
              if (dev > cl) then
                 write(2,*)j,bins_wich(j),(bins_wich(j)-averag)/stderr 
              endif
      enddo

      open(10, file='wichmann_bin_csv',status='unknown')
      write(10,*)"Bin_Number, Counts"
      do i = 1, precis
              write(10,*)i,",",int(bins_wich(i))
      enddo




C***
C end of the main program, define subroutines below 
C***
      print *, ""

      stop 
      end program main 


      ! calling this subrountine will allow you to invoke
      ! a random number named rn1 
      ! 
      ! this is the same code as from example 3 
      subroutine randm(rn1)
              common ix, x_0
              iy=ix*65539

              ! old way of writing if statement? 
              if(iy)5,6,6
              ! generate new seeds 
5             iy = iy + 2147483647+1
6             rn1 = iy
              rn1 = dble(rn1*.4656613E-9)
              ix = iy
      return
      end 

      function randu(iseed)
              parameter (IMAX = 2147483647, XMAX_INV= 1./IMAX)
              iseed = iseed * 65539

              if (iseed < 0) then
                      iseed = iseed + IMAX + 1 
              endif

              randu = dble(iseed * XMAX_INV)
              return 
      end

      ! NOT USED, BUGGED 
      function icg(p, a, b, y0, n)
              y_n_1 = y0

              do i = 0, n
                      y_bar = mod(y_n_1**(p-2), p)
                      y_n = a*y_bar +  mod(b,p) 
                      y_n_1 = y_n
              enddo

              icp = y_n/p
              return
      end

      ! un commment this function if needed later 
      !
      !function bbs(y_0, n, p, q)
      !        common ix 
      !        !The Blum Blum Shub method of generating a 
      !        ! random sequence 

      !        !initialize the new value of y for looping
      !        y_new = 0. 

      !        ! CAUTION: do not attempt to the variables that are passed
      !        ! into the function, eg y_0 - make a clone y_old 
      !        y_old = real(y_0) 
      !        M = p * q 

      !        r_max = 0.

      !        !need to make new array that will be returned 


      !        do i = 10, n+10
      !                y_new = amod(real(y_old**2), real(M)) 
      !                y_old = y_new

      !                ! check if these bounds are correct
      !                l = i - 9 
      !                bbs_vals(l) = y_new
      !                if (y_new > r_max) then
      !                        r_max = y_new
      !                endif


      !        enddo

      !        ! return a max value 
      !        bbs = max 
      !        return 
      !end

      ! linear congruential generator 

      subroutine lin_cong(rnd)
              common ix, x_0
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

      subroutine wichmann_hill(rnd)
              common /seeds/ n_s1, n_s2, n_s3

              n_s1 = mod(171 * int(n_s1), 30269)
              n_s2 = mod(172 * int(n_s2), 30307)
              n_s3 = mod(170 * int(n_s3), 30323)

              rn_s1 = real(n_s1)
              rn_s2 = real(n_s2)
              rn_s3 = real(n_s3)

              !note that leaving abs() out of the below will return
              !negative values on some calls
      rnd = amod(rn_s1/30269.0 + rn_s2/30307.0 +rn_s3/30323.0,1.0)
      rnd = dble(rnd) 
      return
      end
