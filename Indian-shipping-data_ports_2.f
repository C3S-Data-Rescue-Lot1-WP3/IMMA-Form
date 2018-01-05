	program indianshippingdataports

	character line1*400,line2(13)*20,boat*40,dir*8,sea*30,weather*30
	real wdir,fxbfms
	character lat*22,lon*15,lat2*15,lon2*15,boata*40,boatname*44
	character avel*2
	integer di,wi

	open(10,file='indian shipping data 1893 in ship order_2.dat')
	open(12,file='teste.dat')
	open (20,file='vento1893.dat')

	read(10,*)
	read(10,*)
	boata='aaaa'
	nb2=0

	do while (ierr.eq.0)
c	   write(12,*) ierr
	   read(10,'(a200)',iostat=ierr) line1
c	   write(12,*) line1,ierr
	   open (14,file='temp.dat')
	   write (14,'(a200)') line1
	   close(14)
	   open(14,file='temp.dat')
	   if(line1(1:1).ne.'	') read (14,*) iano,imes,idia,ihora
	   ind=0
	   write(*,*) iano,imes,idia,ihora
	   do i=1,200
	      if(line1(i:i).eq.'	') ind=ind+1
            if (ind.eq.4) then
		    k=i+1
	        goto 100
	      endif
         enddo
 100	   write(12,*) ind,k
	   close(14)
	   open(14,file='temp.dat')
	   write (14,'(a200)') line1(k:200)
	   write (12,*) line1(k:200)
	   j=k
	   do i=k,200	     
	     	      
	      if (line1(i:i).eq.'	') then
		     l=i-1
	         ind=ind+1
	         if (j.le.l) then
	            if (line1(j:j).ne.' ') then
			      line2(ind)=line1(j:l)
                  else
	              line2(ind)='-'
                  endif
               else
	            line2(ind)='-'
               endif
               j=i+1
            endif
	      write(12,*) line2(ind),ind
	     
	   enddo
	   close(14)	  
	   do i=5,13
	      write(12,*) iano,imes,idia,ihora,line2(i),i
	      open (14,file='temp.dat')
	      write(14,*) line2(i)
	      close(14)
	      open(14,file='temp.dat')
	      if (i.eq.5) then
		     read(14,'(a40)') boat
               if (boat.ne.boata) then
	           nb=nb2
                 close(30)
	           write(boatname,'(a40,a4)') boat,'.dat'
                 open (30+nb,file=boatname)
                 boata=boat
			   nb2=nb+1	           
               endif
		  endif	   
	      if (i.eq.6) then
		   read(14,'(a22)') lat
c	       write(12,*) 'lat(1:1)',lat(1:1)
	      if ((lat(1:1).eq.'-'.or.lat(1:1).eq.'1'.or.lat(1:1).eq.'2'
     :.or.lat(1:1).eq.'3'.or.lat(1:1).eq.'4'.or.lat(1:1).eq.'5'.or.
     :lat(1:1).eq.'6'.or.lat(1:1).eq.'7'.or.lat(1:1).eq.'8'.or.lat(1:1)
     :.eq.'9'.or.lat(1:1).eq.'0').or.(lat(2:2).eq.'-'.or.lat(2:2).eq.'1'
     :.or.lat(2:2).eq.'2'
     :.or.lat(2:2).eq.'3'.or.lat(2:2).eq.'4'.or.lat(2:2).eq.'5'.or.
     :lat(2:2).eq.'6'.or.lat(2:2).eq.'7'.or.lat(2:2).eq.'8'.or.lat(2:2)
     :.eq.'9'.or.lat(2:2).eq.'0').or.(lat(3:3).eq.'-'.or.lat(3:3).eq.'1'
     :.or.lat(3:3).eq.'2'
     :.or.lat(3:3).eq.'3'.or.lat(3:3).eq.'4'.or.lat(3:3).eq.'5'.or.
     :lat(3:3).eq.'6'.or.lat(3:3).eq.'7'.or.lat(3:3).eq.'8'.or.lat(3:3)
     :.eq.'9'.or.lat(3:3).eq.'0')) then
	         lat=lat
	         lon2='a'
c	         write(12,*) 'lon2=',lon2
            else
c	         write(12,*) 'port',lat
	         call ports(lat,lat2,lon2)
	         lat=lat2
	         lon=lon2
               goto 20
            endif
	      endif
	      if (i.eq.7.and.lon2.eq.'a') read(14,'(a15)') lon
 20	      if (i.eq.8) then
               read(14,*) pres
               pres=pres*33.8638
            endif
	      if (i.eq.9) then
		     read(14,*) prescor
               prescor=prescor*33.8638
            endif
	      if (i.eq.10) read(14,*) dir
	      if (i.eq.11) then 
		     read(14,*) vel
               velms=fxbfms(int(vel))
            endif
	      if (line2(11).eq.'-') then
		     velms=-1.
	         wi=0
            else
	         wi=1
            endif
	      if (i.eq.12) read(14,'(a30)') sea
	      if (i.eq.13) read(14,'(a30)') weather
	      close(14)
         enddo
	   if (dir.eq.'-') then
	      wdir=-99.
	      di=0
         else
	      call wind128(dir,wdir)
	      di=1
         endif
c	   write(20,*) iano,imes,idia,ihora,boat,'vel=',velms,dir,wdir
         write(12,*) iano,imes,idia,ihora,boat,'vel=',velms,dir,wdir,lat
     :	   ,lon
c	   write(12,*) 'ierr',ierr
	   write(*,*) boat
         call asciiformat_icoads(boat,nb,iano,imes,idia,ihora,lat,
     :lon,wdir,di,velms,wi,pres,prescor) 
      enddo
	stop
	end


	subroutine wind128(dir,wdir)

      character awind(128)*8,char*3,dir*8,cdir*8,form*9
	real adir(128),wdir
	open (15,file='rosaventos_by_2.txt')
	open (13,file='teste2.dat')

      if (icount.eq.1) goto 50
	do i=1,128
	   read(15,'(a8,a3,f6.2)') awind(i),char,adir(i)
c	   write(13,*) awind(i),char,adir(i) 
      enddo
  50	icount=1
      
	do j=1,8
	  write(13,'(a3,a1)') 'hei',dir(j:j)
	  if (dir(j:j).ne.'') n=j
      enddo
	write(13,*) 'n',n
	write(cdir,'(a8)') dir
c	write(form,'(a3,i1,a2,i1,a2)') ''(a',n-1,',a',8-n+1,')''
c      write(cdir,form) dir,'       '
    
c      write (12,*) 'i',cdir,'i',dir
	do i=1,128
	   if (cdir.eq.awind(i)) then
	      wdir=adir(i)
	      write (13,*) 'match found',adir(i)
	      goto 40
         else
	      wdir=-99.
c		  write(12,*) wdir,i
	   endif
	enddo
  40  continue
      return
	end



	subroutine ports(lat,lat2,lon2)
      
	character port*22,lat1*15,lat*22,lon1*15,lat2*15,lon2*15,
     :lcase(26)*1,ucase(26)*1,portline*100
      data lcase/'a','b','c','d','e','f','g','h','i','j','k','l','m','n'
     :,'o','p','q','r','s','t','u','v','x','w','y','z'/
	data ucase/'A','B','C','D','E','F','G','H','I','J','K','L','M','N'
     :,'O','P','Q','R','S','T','U','V','X','W','Y','Z'/

      open (15,file='ports2.txt')
	open (17,file='testport.dat')

	read (15,*)

	write(17,*) 'port=',lat
      ierr=0
	do while (ierr.eq.0)
	   read (15,'(a100)',iostat=ierr) portline
	   do k=1,100
	      if (portline(k:k).eq.'	') then
	         open (19,file='temp1.dat')
	         write (19,*) portline(1:k-1)
               close (19)
               open (19,file='temp1.dat')
               read (19,'(a22)') port
	         close (19)
	         open (19,file='temp1.dat')
	         write (19,'(a100)') portline(k+1:100)
	         close(19)
               open (19,file='temp1.dat')
	         read (19,*)lon1,lat1
	         close (19)
	         goto 101
            endif
         enddo
 101     continue
c 	   write (17,*) 'portline=',port
         do i=2,22
	      do j=1,26
	        if (lat(i:i).eq.lcase(j)) lat(i:i)=ucase(j)
            enddo
         enddo
c         write(17,*) 'port',lat
	   if (lat.eq.port) then
	      lat2=lat1
	      lon2=lon1
            write(17,*) 'lat2,lon2',port,lat,lat2,lon2
         endif
      enddo
	close (15)

	return
	end	

	subroutine asciiformat_icoads(boat,k,iano,imes,idia,ihora,lat,
     :lon,wdir,di,velms,wi,pres,prescor)

      integer ndias(12),ano,mes,dia,idflag(2010,12,31,3)
	integer imflag(2010,12,3),horai,rlon,uhr,udy,di,wi
	character ames*2,adia*2,nome*13,lat*6,lon*6,
     :lat2*8,lon2*8,hora1*2,hora2*2,hora3*2,alt*4,alt2*6,
     :nome2*30,filein*35,minuj*2,horaj*2,boat*40
	character*1 flag1,flag2,flag3
	real p,wdir

	data ndias/31,28,31,30,31,30,31,31,30,31,30,31/

	open(22,file='teste3.dat')

      write(22,*) boat,k

	write(22,*) iano,imes,idia,ihora
	write(22,*) lat,lon
	write(22,*) lat2,lon2


      open(25,file='temp1.dat')
	write(25,*) lon
	close (25)
	open(25,file='temp1.dat')
	read(25,*) rlon
	close(25)

      ilon=int(rlon)
	dlon=(rlon-ilon)*100.
	flon=dlon/60.
	rlon=ilon+flon

      open(25,file='temp1.dat')
	write(25,*) lat
	close (25)
	open(25,file='temp1.dat')
	read(25,*) rlat
	close(25)

      ilat=int(rlat)
	dlat=(rlat-ilat)*100.
	flat=dlat/60.
	rlat=ilat+flat


      rlon=rlon*100
      jday=ixdtnd(idia,imes,iano)

	call rxltut(ihora,jday,rlon,uhr,udy)

	call rxnddt(udy,iday,imonth,iyear)
      
      ifile=k
    

	write(30+ifile,100) iyear,imonth,iday,uhr,rlat,rlon,2,
     :'1','0','0','9','9',99,10,'999999999','09',di,wdir,wi,velms,'0',
     :'99','99','9',pres,'0','000','0','9999','0','9999','0','9999',
     :'0','9999','9','9','A','0','A','A','A','38','99','99','38','99',
     :'99','99','10','0',prescor



 100  format(i4,i2,i2,i4,f5.2,f6.2,i2,5a1,2i2,a9,a2,i1,f6.1,i1,f4.1,a1,a2,
     :a2,a1,f6.1,a1,a3,a1,a4,a1,a4,a1,a4,a2,a4,7a1,6a2,a2,a2,a1,f6.1)


	return
	end   

C=======================================================================3456789
c-----time conversions--------------------------------------------------3456789
C=======================================================================3456789
      subroutine rxltut(ihr,idy,elon,uhr,udy)
c-----Convert local standard hour (ihr; in hundredths 0-2399) and "Julian"
c     day (i.e., any incrementable integer date) (idy) into coordinated
c     universal time (UTC) hour (uhr) and day (udy; decremented if the
c     dateline is crossed), using longitude (elon; in hundredths of degrees
c     0-35999, measured east of Greenwich).  Notes: a) Strict time zones,
c     including the International Date Line, are not employed.  b) In all
c     cases the western (eastern) boundary of each time zone is inclusive
c     (exclusive), i.e., 7.50W-7.49E, 7.50E-22.49E, ..., 172.50E-172.51W.
c-----sjw and sdw, 18 Jun 1998.
      integer ihr,idy,elon,uhr,udy
     +,wlon,dhr
      if(ihr.lt.0.or.ihr.gt.2399) then
         print *,'error rxltut. ihr=',ihr
         stop
      else if(elon.lt.0.or.elon.gt.35999) then
         print *,'error rxltut. elon=',elon
         stop
      endif
      wlon = 36000 - elon
      udy = idy
      dhr = (wlon + 749)/1500
      uhr = ihr + dhr*100
      if(uhr.ge.2400)then
         udy = udy + 1
         uhr = uhr - 2400
      endif
      if(wlon.ge.18000) udy = udy - 1
      return
      end

C-----------------------------------------------------------------------3456789
      integer function ixdtnd(iday,imonth,iyear)
c-----Convert from date (iday,imonth,iyear) to number of days since
c     1 Jan 1893.
c-----sjl, 17 Jun 1998.
c-----sjw and sdw, 27 Apr 1999: return ixdtnd=-1 if date is invalid.
c-----sdw, 26 Jan 2000: remove outdated variable ierr/comment.
c-----sjl, 17 Nov 2004: remove print statements.
      integer iday,imonth,iyear
     +,year,days(12)
      data days/31,28,31,30,31,30,31,31,30,31,30,31/
      logical leap
      leap(year) = mod(year,4).eq.0 .and. mod(year,100).ne.0
     + .or. mod(year,400).eq.0
c
      ixdtnd = -1
      if (iyear.lt.1893 .or. imonth.lt.1 .or. imonth.gt.12
     + .or. iday.lt.1 .or. iday.gt.days(imonth)
     + .and. (imonth.ne.2 .or. .not.leap(iyear) .or. iday.ne.29)) return
      ndays = 0
      do 190 year = 1893,iyear-1
        ndays = ndays + 365
        if (leap(year)) ndays = ndays + 1
  190 continue
      do 290 month = 1,imonth-1
        ndays = ndays + days(month)
        if (month.eq.2 .and. leap(iyear)) ndays = ndays + 1
  290 continue
      ndays = ndays + iday-1
      ixdtnd = ndays
      end
C-----------------------------------------------------------------------3456789
      subroutine rxnddt(ndays,iday,imonth,iyear)
c-----Convert from number of days (ndays) since 1 Jan 1893 to
c     date (iday,imonth,iyear).
c-----sjl, 17 Jun 1998.
c-----sjl, 17 Nov 2004: remove print statement and return
c     iday=-1, imonth=-1, and iyear=-1 if ndays is invalid.
      integer ndays,iday,imonth,iyear
     +,year,days(12)
      data days/31,28,31,30,31,30,31,31,30,31,30,31/
      logical leap
      leap(year) = mod(year,4).eq.0 .and. mod(year,100).ne.0
     + .or. mod(year,400).eq.0
c
      iday = -1
      imonth = -1
      iyear = -1
      if (ndays.lt.0) return
      mdays = ndays
      iyear = 1893
  100 continue
      n = 365
      if (leap(iyear)) n = n + 1
      if (mdays - n.ge.0) then
        mdays = mdays - n
        iyear = iyear + 1
        goto 100
      endif
      imonth = 1
  200 continue
      n = days(imonth)
      if (imonth.eq.2 .and. leap(iyear)) n = n + 1
      if (mdays - n.ge.0) then
        mdays = mdays - n
        imonth = imonth + 1
        goto 200
      endif
      iday = mdays + 1
      end
	
      real function fxbfms(bf)
c-----Convert from Beaufort force 0-12 (bf) to "old" (WMO code 1100)
c     midpoint in meters per second.  From Slutz et al. (1985) supp.
c     K, Table K5-5 (p. K29).  See {ixbfkt} for additional background.
c     Reference:
c     Slutz, R.J., S.J. Lubker, J.D. Hiscox, S.D. Woodruff, R.L. Jenne,
c           D.H. Joseph, P.M. Steurer, and J.D. Elms, 1985: Comprehensive
c           Ocean-Atmosphere Data Data Set; Release 1.  NOAA
c           Environmental Research Laboratories, Climate Research
c           Program, Boulder, Colo., 268 pp. (NTIS PB86-105723).
c-----sdw, 16 Jun 1998
      real ms(0:12)
      integer bf
      data ms/0.,1.,2.6,4.6,6.7,9.3,12.3,15.4,19.,22.6,26.8,30.9,35./
      if(bf.lt.0.or.bf.gt.12) then
         print *,'fxbfms error.  bf=',bf
         stop
      endif
      fxbfms = ms(bf)
      return
      end		      