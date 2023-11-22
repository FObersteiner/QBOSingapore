      program qbo_composite

c sample code to read the ascii files containing
c .   time series of monthly average u (or t) at various levels
c .   for a few stations

c input files were created by manually editing the output of 
c .   rd391.job_sing and rd391.job_gan_canton

c program will also combine the 'combined singapore'
c .   with the canton island data

c Basically, Canton is used thru 8/67 and the 'combined singapore'
c .   is used thereafter. The reason is that the early singapore
c .   data did not contain 'upper' stratospheric reports until about this time
 
      parameter (nyrstrt=1957, nyrlast=1996
     *          ,nyrs=nyrlast-nyrstrt+1, nmos=12
     *          ,nlvl=15)        ! pres levels (same as npmax)
      real sing(nmos,nyrs,nlvl)  ! singapore 48690/48694
     *   , cant(nmos,nyrs,nlvl)  ! canton    91700/60703 (wmo/wban)
     *   , qbo (nmos,nyrs,nlvl)  ! combined canton and singapore

      integer iwork(100)
      integer ip(nlvl)           ! pressure levels
      data ip /1000,850,700,500,400,300,250,200,150,100
     *        ,  70, 50, 30, 20, 10/
      character ctype(3)*1, filnam*96
      data      ctype /'x','t','u'/                  ! x is a place holder (not used)
    
      
     
      xmsg  = -999.9
      iunit = 11

      do itype=2,3                                   ! =2 for temperature; =3 for u

         filnam = "/home/aktuell/data/qbo/data/shea/z.sing_" // 
     *             ctype(itype)
         lf     = index(filnam,' ')-1
         write (*,"(' lf, filnam=',i3,2x,a)") lf, filnam(1:lf)
         open (iunit,file=filnam(1:lf))                       
         call rd_data (iunit,sing,nmos,nyrs,nlvl,ip     ! read combined singapore
     *                ,nyrstrt,nyrlast,xmsg,ier)

         filnam = "/home/aktuell/data/qbo/data/shea/z.canton_" // 
     *            ctype(itype)
         lf     = index(filnam,' ')-1
         write (*,"(' lf, filnam=',i3,2x,a)") lf, filnam(1:lf)
         open (iunit,file=filnam(1:lf))                       
         call rd_data (iunit,cant,nmos,nyrs,nlvl,ip     ! read canton   
     *                ,nyrstrt,nyrlast,xmsg,ier)

         if (itype.eq.3) then                           ! canton   70mb u (57-60) miscoded
                                                        ! Singa  1000mb u (57-90)
             nl1000 = 0                                 ! 1000mb lvl subscript
             nl70 = 0                                   ! 70mb   lvl subscript
             do nl=1,nlvl                               ! 'fix' canton 70mb datq
                if (ip(nl).eq.70) then
                    nl70 = nl
                endif
                if (ip(nl).eq.1000) then
                    nl1000 = nl
                endif
             enddo
               
             do nyr=1,nyrs                              
                nyear = nyr+nyrstrt-1                  
                if (nyear.le.1960) then                 ! set Canton 70mb to msg
                    do nmo=1,nmos
                       cant(nmo,nyr,nl70) = xmsg
                    enddo
                endif
                if (nyear.le.1990) then                 ! set Sing 1000mb to msg
                    do nmo=1,nmos
                       sing(nmo,nyr,nl1000) = xmsg
                    enddo
                endif
             enddo
         endif       ! end itype test for canton/sing u
                                                        ! combine the data
         idqbo = 607030                                 ! Canton id
         do nyr=1,nyrs
            nyear = nyr+nyrstrt-1
           do nmo=1,nmos
              if (nmo.eq.10 .and. nyear.eq.1967) idqbo = 489680  ! Singapore id
             do nl=1,nlvl
                if (idqbo.eq.607030) then
                    qbo(nmo,nyr,nl) = cant(nmo,nyr,nl)
                elseif (idqbo.eq.489680) then
                    qbo(nmo,nyr,nl) = sing(nmo,nyr,nl)
                endif
             enddo
           enddo
         enddo   
                                                       ! print/save the series
                                                       ! same format as above files
         filnam = 'qbo_' // ctype(itype)
         lf     = index(filnam,' ')-1
         write (*,"(' lf, filnam=',i3,2x,a)") lf, filnam(1:lf)
         open (iunit,file=filnam(1:lf))                       

         idqbo = 607030
         do nyr=1,nyrs
            nyear = nyr+nyrstrt-1
           do nmo=1,nmos
              if (nmo.eq.10 .and. nyear.eq.1967) idqbo = 489680
             do nl=1,nlvl
                if (qbo(nmo,nyr,nl).eq.xmsg) then
                    iwork(nl) = -999
                else
                    iwork(nl) = int(qbo(nmo,nyr,nl)*10.)
                endif
             enddo
              write (*,"(i7,i2,2i3,17i5)") idqbo,itype,(nyear-1900),nmo
     *              ,(iwork(nl),nl=1,nlvl)   
              write (iunit,
     *              "(i7,i2,2i3,17i5)") idqbo,itype,(nyear-1900),nmo
     *              ,(iwork(nl),nl=1,nlvl)   
           enddo
         enddo   
         close (iunit)

         enddo          ! end itype


      end
c -----------------------------------------------------------
      subroutine rd_data (iunit,x,nmos,nyrs,nlvl,ip
     *                   ,nyrstrt,nyrlast,xmsg,ier)
      integer iunit,nmos,nyrs,nlvl,nyrstrt,nyrlast,ier
      real    x(nmos,nyrs,nlvl)
      integer ip(nlvl)              ! pressure levels

      integer iwork(100)            ! local 

      call setflt (x,nmos*nyrs*nlvl,xmsg,ier)

   10 read (iunit,"(i7,i2,2i3,17i5)",end=20) 
     *              idsta, itype, nyear, nmo
     *             ,(iwork(nl),nl=1,nlvl) 
      write (*,"(' echo: ',i7,i2,2i3,17i5)") 
     *              idsta, itype, nyear, nmo
     *             ,(iwork(nl),nl=1,nlvl) 
      nyear = nyear+1900
      if (nyear.ge.nyrstrt .and. nyear.le.nyrlast) then
          nyr = nyear-nyrstrt+1
          do nl=1,nlvl
             if (iwork(nl).ne.-999) then
                 x(nmo,nyr,nl) = real(iwork(nl))*0.1
             else
                 x(nmo,nyr,nl) = xmsg
             endif
          enddo
      endif
      go to 10

   20 close (iunit) 

      return
      end
c ----------------------------------------------
      subroutine setflt (fltvec,npts,fval,ier)                                  
                                                                                
      integer npts,ier
      real fltvec(1:npts), fval                                                       
                                                                                
c set a floating point vector to a floating point constant (type real)          
                                                                                
      ier = 0                                                                   
      if (npts.lt.1) then                                                       
         ier = 1                                                                
         return                                                                 
      endif                                                                     
                                                                                
      do n=1,npts                                                            
         fltvec(n) = fval                                                          
      enddo
                                                                                
      return                                                                    
      end                                                                       
