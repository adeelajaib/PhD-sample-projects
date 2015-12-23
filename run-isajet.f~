      PROGRAM runisahet

!This program randomly scans the parameter space of a supersymmetric model using isajet

	integer t1,t2,lsize,lsize1,lsize2,ncheck,flag

       real m1a,m1amax,m1amin
       real m2a,m2amax,m2amin
       real m3a,m3amax,m3amin
       real mhd, mhdmin, mhdmax
       real mhu, mhumin, mhumax


	integer r,p1,wi
	real l1,l2
	integer p2
        real   m0,mhf,a0,tanb
        real   m0i,mhfi,a0i,tanbi,m1ai,m2ai,m3ai,mhdi,mhui
 	real inp(200000,9)
        real o1,o2,o3,o4,o5,o6,o7,o8,o9

        mz=91.187

	call system("rm out.txt")
	call system("rm lha-isa.txt")


! Open files

	open(unit=14,file='rnd-pts.txt')  ! file containing random points

	open(17,file='nuhm2-nugm.txt')    ! file to be read by isajet

	open(26,file='output-all-vars.dat')



! Define ranges of the parameter space 

	m0min=0
	m0max=1000

	mhfmin=0
	mhfmax=1000

	a0min=-3
	a0max=3

	tbmin=2
	tbmax=60

       mhdmin=0
       mhdmax=3000

       mhumin=0
       mhumax=3000


       m1amin=0
       m1amax=1000

       m2amin=0
       m2amax=1000

       m3amin=0
       m3amax=6000




!----------------------------------

	ncheck=0
        flag=0


	r=14
	p1=0


! Count the number of random points in the file

	call countl(r,p1)
        print *,"no. of random pts= ",p1

! read the point number to run from the file run-pt.txt

       open(unit=25,file='run-pt.txt')
       read(25,*)p2

        print*,'running pt ',p2

! read rnd numbers

	do l1=1,p1

	t2=l1

	read(r,*)(inp(t2,t1),t1=1,9)

 	enddo


! start scan

! run loop

!	p2=2

	do l1=p2,p2  

! Define the center of the gaussian distribution as the point read from the file rnd-pts.txt

	m0i=inp(l1,1)
	mhfi=inp(l1,2)
	a0i=inp(l1,3)*m0i
	tanbi=inp(l1,4)

        m1ai=inp(l1,5)
        m2ai=inp(l1,6)
        m3ai=inp(l1,7)

        mhdi=inp(l1,8)
        mhui=inp(l1,9)

        

111    continue

	lsize1=3
	lsize2=0

        lsize=lsize1       !define the size of the loop to search for points around the center

	do l2=0,lsize

	call system("rm out.txt")
	call system("rm lha-isa.txt")
	open(17,file='nuhm2-nugm.txt')


	print*,'loop nos.(l1,l2)=',l1,l2

!call the Gaussian subroutine to find a point around the center of the Gaussian

	call logdis(m0i,mhfi,a0i,tanbi
     &,m1ai,m2ai,m3ai,mhdi,mhui,l2,wi
     &,o1,o2,o3,o4,o5,o6,o7,o8,o9)


 	if (l2.eq.0.and.flag.eq.0) then

	m0=m0i
	mhf=mhfi
	a0=a0i
	tanb=tanbi

        m1a=m1ai
        m2a=m2ai
        m3a=-m3ai

        mhd=mhdi
        mhu=mhui

        flag=1

	else

	m0=o1
	mhf=o2
	a0=o3
	tanb=o4

        m1a=o5
        m2a=o6
        m3a=-o7

        mhd=o8
        mhu=o9


c
 	endif




       print*,'m0,mhf,a0,tanb,mub,m1,m2,m3,mhd,mhu'
       print*,m0,mhf,a0,tanb,mub,m1a,m2a,m3a,mhd,mhu

!Make sure that the point is in the range

	       if(m0.ge.m0max.or.m0.le.m0min) then 
	 print*,'m0 out of range ' 
	 goto 222 
	 endif

	     if(mhf.ge.mhfmax.or.mhf.le.mhfmin) then 
	 print*,'mhf out of range ' 
	 goto 222 
	 endif

	     if(a0/m0.ge.a0max.or.a0/m0.le.a0min) then 
	 print*,'a0 out of range ' 
	 goto 222 
	 endif


	     if(tanb.ge.tbmax.or.tanb.le.tbmin) then 
	 print*,'tanb out of range ' 
	 goto 222 
	 endif

           if(abs(m1a).ge.m1amax.or.abs(m1a).le.m1amin) then
        print*,'m1a out of range'
        goto 222
        endif

           if(abs(m2a).ge.m2amax.or.abs(m2a).le.m2amin) then
        print*,'m2a out of range'
        goto 222
        endif

           if(abs(m3a).ge.m3amax.or.abs(m3a).le.m3amin) then
        print*,'m3a out of range'
        goto 222
        endif

          if(mhd.ge.mhdmax.or.mhd.le.mhdmin) then
        print*,'mhd out of range'
        goto 222
        endif

          if(mhu.ge.mhumax.or.mhu.le.mhumin) then
        print*,'mhu out of range'
        goto 222
        endif




! Write output for isajet


	 write(17,*),'out.txt'
	 write(17,*),'lha-isa.txt'
	 write(17,*),'/'
	 write(17,*),'3'

	 write(17,*),m0
	 write(17,*),mhf
	 write(17,*),a0
	 write(17,*),tanb
	 write(17,*),'1'
	 write(17,*),'173.3'

	 write(17,*),' '

	 write(17,*),'1' !select nugm
	 write(17,*),m1a
	 write(17,*),m2a
	 write(17,*),m3a

	 write(17,*),' '

	 write(17,*),'3' !select nuhm2
	 write(17,*),mhd
	 write(17,*),mhu


	 write(17,*),' '

	 write(17,*),'0'
	 write(17,*),'1'

	 write(17,*),' '

	 write(17,*),'0'  !start selecting variables to calculate
	 write(17,*),'1'
	 write(17,*),'1'
	 write(17,*),'0'
	 write(17,*),'1'
	 write(17,*),'1'

	 write(17,*),'/'


         close(17)



	open(unit=15,file='inp-params.txt')
      write(15,'((9(E14.7,2x)))'),m0,mhf,a0,tanb,m1a,m2a,m3a,mhd,mhu

! Run isajet 

         call system('./isasugra.x < nuhm2-nugm.txt')


      close(15)


222	continue

	enddo

	enddo


         end

**************************************************************

	subroutine logdis(a,b,c,d,e,f,g,h,i,l2,wi
     &,m0,mhf,a0,tanb,m1a,m2a,m3a,mhd,mhu)

	real a,b,c,d,e,f,g,h,i,m0,mhf,a0,tanb,m1a,m2a,m3a,mhd,mhu

        integer wi

        external ranlux
        real   rvec,l2


        wi=5

        seed1=7654321

        seed=seed1+5*int(l2)

        print*,'l2=',l2

        print *,'wi=',wi



        CALL RLUXGO(4,seed,0,0)


c       Now to generate a random number for m0

        CALL RNORMX(rvec,1,ranlux)
	m0=a+(a/wi*rvec)


c       Now to generate a random number for mhf 

        CALL RNORMX(rvec,1,ranlux)
	mhf=b+(b/wi*rvec)

c       Now to generate a random number for a0 

        CALL RNORMX(rvec,1,ranlux)
	a0=c+(c/wi*rvec)


c       Now to generate a random number for tanb

        CALL RNORMX(rvec,1,ranlux)
	tanb=d+(d/wi*rvec)


c       Now to generate a random number for m1a

        CALL RNORMX(rvec,1,ranlux)
	m1a=e+(e/wi*rvec)

c       Now to generate a random number for m2a

        CALL RNORMX(rvec,1,ranlux)
	m2a=f+(f/wi*rvec)

c       Now to generate a random number for m3a

        CALL RNORMX(rvec,1,ranlux)
	m3a=g+(g/wi*rvec)

c       Now to generate a random number for mhd

        CALL RNORMX(rvec,1,ranlux)
	mhd=h+(h/wi*rvec)

c       Now to generate a random number for mhu

        CALL RNORMX(rvec,1,ranlux)
	mhu=i+(i/wi*rvec)



	end

*************************************************************

cc subroutine to count lines

           
	subroutine countl(r,p2)

	integer  io(41:51),p2,r

        do
        read (r,*,IOSTAT=io(r)) 
          if (io(r).lt.0) then 
           exit
         else
           p2=p2+1
         endif
        end do

	rewind r
	end

**************************************************************
