subroutine readchangepar(nopt,parname,RunType,CurRunoffGenType,Solution,sensfun1rank,ipid)
        use SingleChangePara
        use dhmtype
        use temptype
		implicit none
		
		integer iopt,nopt
		integer isens,i
		integer iobj, nintval, iseed
		character*10 parname(nopt)			
		real bl1,bl2
		integer in1,in2,RunType,CurRunoffGenType,Solution,ipid
		real sensfun1rank(nopt,2)
        
		if(allocated(iiname))deallocate(iiname)
		allocate(iiname(nopt,2))
        if(allocated(imet))deallocate(imet)
        allocate(imet(nopt))
        if(allocated(bbound))deallocate(bbound)
        allocate(bbound(2,nopt))
        if(allocated(sensfun1mean))deallocate(sensfun1mean)
		allocate(sensfun1mean(nopt))
          
		bbound=0
        iopt=1
	    if(allocated(sinChangeParas))then
!             do i = 1,sinChangeParaNck
             do i = 1,sinChangeParaNparameter
                if(Runtype.eq. 2 .and. sinChangeParas(i,ipid).IRunoffRenType .eq. CurRunoffGenType .and. sinChangeParas(i,ipid).IsSenseUsed .eq. 1)then
 		            bbound(1,iopt)=sinChangeParas(i,ipid).bl1		! 参数下限
		            bbound(2,iopt)=sinChangeParas(i,ipid).bl2		! 参数上限
		            iiname(iopt,1)=sinChangeParas(i,ipid).in1		! 参数索引
		            iiname(iopt,2)=real(iopt)
		            imet(iopt)=sinChangeParas(i,ipid).in2			! 参数调整方法
		            parname(iopt)=sinChangeParas(i,ipid).pname		! 参数名称  
		            sensfun1rank(iopt,1)=real(sinChangeParas(i,ipid).sensfun1rank)	
		            sensfun1rank(iopt,2)=real(iopt)
		            sensfun1mean(iopt)=sinChangeParas(i,ipid).sensfun1mean
		            iopt = iopt + 1             
                else if(Runtype .ne.2 .and. sinChangeParas(i,ipid).IRunoffRenType .eq. CurRunoffGenType .and. sinChangeParas(i,ipid).IsParasolUsed .eq. 1)then
 		            bbound(1,iopt)=sinChangeParas(i,ipid).bl1		
		            bbound(2,iopt)=sinChangeParas(i,ipid).bl2		
		            iiname(iopt,1)=sinChangeParas(i,ipid).in1	
		            iiname(iopt,2)=real(iopt)
		            imet(iopt)=sinChangeParas(i,ipid).in2			
		            parname(iopt)=sinChangeParas(i,ipid).pname
		            sensfun1rank(iopt,1)=real(sinChangeParas(i,ipid).sensfun1rank)	
		            sensfun1rank(iopt,2)=real(iopt)	
		            sensfun1mean(iopt)=sinChangeParas(i,ipid).sensfun1mean
		            iopt = iopt + 1                   
                end if
             end do 
         end if
		return
	end
	
	!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine readmetfiles(iobj,calw, icalpar,isenspar,sensw,isens)
!	    use DBSQLLinkMod
	    use dhmtype
	    implicit real (a-h,o-z)
	    real calw(iobj), sensw(isens)
	    integer isens
	    integer iobj
	    integer icalpar(4,iobj), isenspar(4,isens)
	    character*10 icalpar1(22), icalpar2(8), isenspar2(5)
	    character*5 icalpar3a(22),icalpar3b(22)
	    character*1 ctmp
	    LOGICAL exists
	    common /iopar/ in,iprii

	    data icalpar1/'flow      ','sediment  ','organic N ','organic P ','nitrate   ','ammonia   ','nitrite   ','mineral P ','CBOD      ',	'oxygen    ','chl-a     ','sol pest  ','sor pest  ','perc bact ','see manual','see manual','metal#2   ','metal#3   ','temperat  ','kjel N    ','total N   ','total P   '/			
	    data icalpar2/'SSQ       ','SSQ-log   ','SSQ-sqrt  ','          ','SSQR      ','          ','          ','BIAS      '/
	    data isenspar2/'aver.','     ','          ','          ','          '/

	    data icalpar3a/'m3/s ','mg/L ','mg/L ','mg/L ','mg/L ','mg/L ','mg/L ','mg/L ','mg/L ','mg/L ','ug/L ','mg/L ','mg/L ','ct/L ','**** ','**** ','mg/L ','mg/L ','degC ','mg/L ','mg/L ','mg/L '/
	    data icalpar3b/'m3/s ','T/d  ','T/d  ','T/d  ','T/d  ','T/d  ','T/d  ','T/d  ','T/d  ','T/d  ','kg/d ','T/d  ','T/d  ','Gct/d','*****','*****','T/d  ','T/d  ','heat ','T/d  ','T/d  ','T/d  '/
    	
	    !  READ THE OBJECTIVE FUNCTIONS CONTROL PARAMETERS
        
        do mm=1,iobj
		    icalpar(1,mm)=objirde(mm,1)
		    icalpar(2,mm)=objirde(mm,2)
		    icalpar(3,mm)=objirde(mm,3)
		    icalpar(4,mm)=objirde(mm,4)
		    calw(mm)=objirde(mm,5)			
		    if (calw(mm).le.0.) calw(mm)=1.
		    if (icalpar(1,mm).le.0) go to 996
        end do
    996     continue


        do mm=1,isens
            isenspar(1,mm)=respirde(mm,1)
		    isenspar(2,mm)=respirde(mm,2)
		    isenspar(3,mm)=respirde(mm,3)
		    isenspar(4,mm)=respirde(mm,4)
		    sensw(mm)=respirde(mm,5)
		    if (isenspar(1,mm).le.0) go to 998
        end do
    998     continue

	    return
    end


	!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!	subroutine telobjresp(iobj,isens)
!		use dhmtype
!		use Objmet
!		use Responsmet
!		implicit real (a-h,o-z)
!
!		integer isens
!		integer iobj
!		integer i,j,k,l
!		real m
!        iobj = 1
!        if(allocated(objirde))deallocate(objirde)
!        allocate(objirde(iobj,5))    
!        do t=1,iobj
!            objirde(t,1)=Objmet_i
!            objirde(t,2)=Objmet_j
!            objirde(t,3)=Objmet_k
!            objirde(t,4)=Objmet_l
!            objirde(t,5)=Objmet_m
!	    end do
!        isens = 1
!        if(allocated(respirde))deallocate(respirde)
!        allocate(respirde(isens,5))
!        do t=1,isens
!            respirde(t,1)=Responsmet_i
!            respirde(t,2)=Responsmet_j
!            respirde(t,3)=Responsmet_k
!            respirde(t,4)=Responsmet_l
!            respirde(t,5)=Responsmet_m
!	    end do
!!	    iRet = SQLFreeHandle( SQL_HANDLE_STMT, hSQLStmt )
!		return
!    end
