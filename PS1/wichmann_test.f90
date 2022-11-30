!
! a test of the Wichmann-Hill generator 
!
program wich_test
!
common a 
common /seeds/ n_s1, n_s2, n_s3

a = 5.77
n_s1 = 5021
n_s2 = 17047
n_s3 = 23981


do i = 1, 20
        call wichmann_hill(rnd)
        print *, "Iteration ",i,": ",rnd
enddo





end program wich_test

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
rnd = amod(rn_s1/30269.0 + rn_s2/30307.0 + rn_s3/30323.0,1.0)
return
end
