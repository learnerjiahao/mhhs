! 生成CurPara.txt
	! 修改模型关键参数
	subroutine changemodPar(xxo,nopt,CurRunoffGenType)
		use dhmtype
		use temptype
		implicit none		
		real xxo(nopt)
		integer nopt,CurRunoffGenType
		character*20 TmpChar	
		
		! 改变参数		
		if (CurRunoffGenType == 1) then
		    call changeEasyDHMPar( xxo,nopt)
!		else if (CurRunoffGenType == 2) then
!		    call changeWetSpaPar( xxo,nopt)
!		else if (CurRunoffGenType == 3) then		    
!			call changeXAJPar( xxo,nopt)	
!		else if (CurRunoffGenType == 4) then
!			call changeHymodPar( xxo,nopt)	
		endif
		return
	end
	
	
	! 修改模型关键参数
	subroutine changeEasyDHMPar(xxo,nopt)
		use dhmtype
		use temptype
        use SingleChangePara
        use EasyDHMParamMod
        use ReachParamMod
        use solutionMod
        
		implicit none
		real xxo(nopt), parlim(2)
		integer num,i,iopt,iimet,Nopt
		integer iinr(nopt)
		character*1 tt
        integer IParamRange,ithpid
        
		!基本GIS产流参数 = 13
		real CondlyM(2),CN(6),gwdelay,UnitSlopeM,PorosityM,FieldCapM,alphabf,LaiMaxM(6),DepressM(6),RootDpthM(6),ItcmaxM(6),watercof
		real CH_S2M,CH_L2M,CH_N2M,CH_K2M,ImpM,Smfmx,Smfmn,TIMP,snocovmx,Sno50cov,dep_impM,tdrainpar,Sol_crkM,solf,solfm,solzcoe(1),petm
		!     ~ ~ ~ PURPOSE ~ ~ ~
		!     changing the parameters values for autocalibration
		!     ~ ~ ~ COMMON BLOCKS ~ ~ ~
		!
		!     ~ ~ ~ INCOMING VARIABLES ~ ~ ~
		!!    nopt        |none          |number of parameters to optimise
		!!    xxo(:)       |none          |new values of the parameters 
		!!    iiname(:)    |none          |code refering to which parameter to change, and how
		!!    iinr(:)      !              |number of HRUs to change for parameter (:)
		
		! 先读取默认参数
		 ithpid = cursolution.ithpid
         if(allocated(sinChangeParas))then
!             do i = 1,sinChangeParaNck
             do i = 1,sinChangeParaNparameter
                if(sinChangeParas(i,ithpid).IRunoffRenType .eq. 1)then
			    select case (sinChangeParas(i,ithpid).in1)
			        case(1)
				        CondlyM(1)=sinChangeParas(i,ithpid).Def
			        case(2)
				        CondlyM(2)=sinChangeParas(i,ithpid).Def
			        case(3)
				        CN(1)=sinChangeParas(i,ithpid).Def
			        case(4)
				        gwdelay=sinChangeParas(i,ithpid).Def
			        case(5)
				        UnitSlopeM=sinChangeParas(i,ithpid).Def
			        case(6)
				        PorosityM=sinChangeParas(i,ithpid).Def	
			        case(7)
				        FieldCapM=sinChangeParas(i,ithpid).Def				
			        case(8)
				        alphabf=sinChangeParas(i,ithpid).Def		
			        case(9)
				        LaiMaxM(1)=sinChangeParas(i,ithpid).Def
			        case(10)
				        DepressM(1)=sinChangeParas(i,ithpid).Def			
			        case(11)
				        RootDpthM(1)=sinChangeParas(i,ithpid).Def  
			        case(12)
				        ItcmaxM(1)=sinChangeParas(i,ithpid).Def
			        case(13)
				        CH_S2M=sinChangeParas(i,ithpid).Def
			        case(14)
				        CH_L2M=sinChangeParas(i,ithpid).Def
			        case(15)
				        CH_N2M=sinChangeParas(i,ithpid).Def
			        case(16)
				        CH_K2M=sinChangeParas(i,ithpid).Def
			        case(17)
				        ImpM=sinChangeParas(i,ithpid).Def
			        case(18)
				        Smfmx=sinChangeParas(i,ithpid).Def
			        case(19)
				        Smfmn=sinChangeParas(i,ithpid).Def
			        case(20)
				        TIMP=sinChangeParas(i,ithpid).Def
			        case(21)
				        snocovmx=sinChangeParas(i,ithpid).Def
			        case(22)
				        Sno50cov=sinChangeParas(i,ithpid).Def
			        case(23)
				        dep_impM=sinChangeParas(i,ithpid).Def
			        case(24)
				        tdrainpar=sinChangeParas(i,ithpid).Def
			        case(25)
				        Sol_crkM=sinChangeParas(i,ithpid).Def
			        case(26)
				        solf=sinChangeParas(i,ithpid).Def
			        case(27)
				        solfm=sinChangeParas(i,ithpid).Def
			        case(28)
				        solzcoe(1)=sinChangeParas(i,ithpid).Def
			        case(29)
				        petm=sinChangeParas(i,ithpid).Def	
			        case(30)
				        CN(2)=sinChangeParas(i,ithpid).Def
                    case(31)
				        LaiMaxM(2)=sinChangeParas(i,ithpid).Def				        
			        case(32)
				        RootDpthM(2)=sinChangeParas(i,ithpid).Def  
			        case(33)
				        ItcmaxM(2)=sinChangeParas(i,ithpid).Def
			        case(34)
				        CN(3)=sinChangeParas(i,ithpid).Def
                    case(35)
				        LaiMaxM(3)=sinChangeParas(i,ithpid).Def				        
			        case(36)
				        RootDpthM(3)=sinChangeParas(i,ithpid).Def  
			        case(37)
				        ItcmaxM(3)=sinChangeParas(i,ithpid).Def
			        case(38)
				        CN(5)=sinChangeParas(i,ithpid).Def
                    case(39)
				        LaiMaxM(5)=sinChangeParas(i,ithpid).Def				        
			        case(40)
				        RootDpthM(5)=sinChangeParas(i,ithpid).Def  
			        case(41)
				        ItcmaxM(5)=sinChangeParas(i,ithpid).Def
			        case(42)
				        CN(6)=sinChangeParas(i,ithpid).Def
                    case(43)
				        LaiMaxM(6)=sinChangeParas(i,ithpid).Def				        
			        case(44)
				        RootDpthM(6)=sinChangeParas(i,ithpid).Def  
			        case(45)
				        ItcmaxM(6)=sinChangeParas(i,ithpid).Def
			        case(46)
				         DepressM(2)=sinChangeParas(i,ithpid).Def
			        case(47)
				         DepressM(3)=sinChangeParas(i,ithpid).Def
			        case(48)
				         DepressM(5)=sinChangeParas(i,ithpid).Def
			        case(49)
				         DepressM(6)=sinChangeParas(i,ithpid).Def
!			        case(50)
!				         watercof=sinChangeParas(i).Def
			        end select	                
                endif 
             end do 
         end if

		
		do iopt= 1,nopt
			iimet=imet(iopt)
			parlim(1)=bbound(1,iopt)
			parlim(2)=bbound(2,iopt)		

			select case (iiname(iopt,1))		
			case(1)
				call chngp(CondlyM(1), xxo(iopt), iimet, parlim)
			case(2)
				call chngp(CondlyM(2), xxo(iopt), iimet, parlim)
			case(3)
				call chngp(CN(1), xxo(iopt), iimet, parlim)
			case(4)
				call chngp(gwdelay, xxo(iopt), iimet, parlim)
			case(5)
				call chngp(UnitSlopeM, xxo(iopt), iimet, parlim)
			case(6)
				call chngp(PorosityM, xxo(iopt), iimet, parlim)			
			case(7)
				call chngp(FieldCapM, xxo(iopt), iimet, parlim)				
			case(8)
				call chngp(alphabf, xxo(iopt), iimet, parlim)				
			case(9)
				call chngp(LaiMaxM(1), xxo(iopt), iimet, parlim)	
			case(10)
				call chngp(DepressM(1), xxo(iopt), iimet, parlim)					
			case(11)
				call chngp(RootDpthM(1), xxo(iopt), iimet, parlim)    
			case(12)
				call chngp(ItcmaxM(1), xxo(iopt), iimet, parlim)
			case(13)
				call chngp(CH_S2M, xxo(iopt), iimet, parlim)
			case(14)
				call chngp(CH_L2M, xxo(iopt), iimet, parlim)
			case(15)
				call chngp(CH_N2M, xxo(iopt), iimet, parlim)
			case(16)
				call chngp(CH_K2M, xxo(iopt), iimet, parlim)
			case(17)
				call chngp(ImpM, xxo(iopt), iimet, parlim)
			case(18)
				call chngp(Smfmx, xxo(iopt), iimet, parlim)
			case(19)
				call chngp(Smfmn, xxo(iopt), iimet, parlim)
			case(20)
				call chngp(TIMP, xxo(iopt), iimet, parlim)
			case(21)
				call chngp(snocovmx, xxo(iopt), iimet, parlim)
			case(22)
				call chngp(Sno50cov, xxo(iopt), iimet, parlim)
			case(23)
				call chngp(dep_impM, xxo(iopt), iimet, parlim)
			case(24)
				call chngp(tdrainpar, xxo(iopt), iimet, parlim)
			case(25)
				call chngp(Sol_crkM, xxo(iopt), iimet, parlim)
			case(26)
				!call chngp(solf, xxo(iopt), iimet, parlim)
			case(27)
				!call chngp(solfm, xxo(iopt), iimet, parlim)
			case(28)
				!call chngp(solzcoe(1), xxo(iopt), iimet, parlim)
			case(29)
				!call chngp(petm, xxo(iopt), iimet, parlim)		
			case(30)
				call chngp(CN(2), xxo(iopt), iimet, parlim)
			case(31)
				call chngp(LaiMaxM(2), xxo(iopt), iimet, parlim)	
			case(32)
				call chngp(RootDpthM(2), xxo(iopt), iimet, parlim)    
			case(33)
				call chngp(ItcmaxM(2), xxo(iopt), iimet, parlim)
			case(34)
				call chngp(CN(3), xxo(iopt), iimet, parlim)
			case(35)
				call chngp(LaiMaxM(3), xxo(iopt), iimet, parlim)	
			case(36)
				call chngp(RootDpthM(3), xxo(iopt), iimet, parlim)    
			case(37)
				call chngp(ItcmaxM(3), xxo(iopt), iimet, parlim)
			case(38)
				call chngp(CN(5), xxo(iopt), iimet, parlim)
			case(39)
				call chngp(LaiMaxM(5), xxo(iopt), iimet, parlim)	
			case(40)
				call chngp(RootDpthM(5), xxo(iopt), iimet, parlim)    
			case(41)
				call chngp(ItcmaxM(5), xxo(iopt), iimet, parlim)
			case(42)
				call chngp(CN(6), xxo(iopt), iimet, parlim)
			case(43)
				call chngp(LaiMaxM(6), xxo(iopt), iimet, parlim)	
			case(44)
				call chngp(RootDpthM(6), xxo(iopt), iimet, parlim)    
			case(45)
				call chngp(ItcmaxM(6), xxo(iopt), iimet, parlim)
			case(46)
				call chngp(DepressM(2), xxo(iopt), iimet, parlim)	
			case(47)
				call chngp(DepressM(3), xxo(iopt), iimet, parlim)	
			case(48)
				call chngp(DepressM(5), xxo(iopt), iimet, parlim)	
			case(49)
				call chngp(DepressM(6), xxo(iopt), iimet, parlim)	
!			case(50)
!				call chngp(watercof, xxo(iopt), iimet, parlim)	
			end select
		enddo
		IParamRange = Cursolution.IParamRange
						
        EasyDHMParams(IParamRange).ConductM=CondlyM 
        EasyDHMParams(IParamRange).CN2(:)=CN(:)
        EasyDHMParams(IParamRange).Agw_delay=gwdelay
        EasyDHMParams(IParamRange).UnitSlopeM=UnitSlopeM
        EasyDHMParams(IParamRange).PorosityM=PorosityM
        EasyDHMParams(IParamRange).FieldCapM=FieldCapM
        EasyDHMParams(IParamRange).Alpha_bf=alphabf
        EasyDHMParams(IParamRange).LaiMaxM(:)=LaiMaxM(:)
        EasyDHMParams(IParamRange).DepressM(:)=DepressM(:)
        EasyDHMParams(IParamRange).RtDpthM(:)=RootDpthM(:)
        EasyDHMParams(IParamRange).ItcMaxM(:)=ItcmaxM(:)
        EasyDHMParams(IParamRange).ImpM=ImpM
        EasyDHMParams(IParamRange).SmFmx=Smfmx
        EasyDHMParams(IParamRange).SmFmn=Smfmn
        EasyDHMParams(IParamRange).Timp=TIMP
        EasyDHMParams(IParamRange).Snocovmx=snocovmx
        EasyDHMParams(IParamRange).Sno50cov=Sno50cov
        EasyDHMParams(IParamRange).Dep_impM=dep_impM
        EasyDHMParams(IParamRange).DdrainM=tdrainpar
        EasyDHMParams(IParamRange).Sol_crkM=Sol_crkM
        EasyDHMParams(IParamRange).Solf=solf
        EasyDHMParams(IParamRange).Solfm=solfm
        EasyDHMParams(IParamRange).Solzcoe=solzcoe(1)
        EasyDHMParams(IParamRange).Petm=petm
!        EasyDHMParams(IParamRange).watercof=watercof
       
        ReachParams(IParamRange).CH_S2M=CH_S2M
        ReachParams(IParamRange).CH_L2M=CH_L2M
        ReachParams(IParamRange).CH_N2M=CH_N2M
        ReachParams(IParamRange).CH_K2M=CH_K2M
		
	end
			
!	subroutine changeWetSpaPar(xxo,nopt)
!		use dhmtype
!		use temptype
!        use SingleChangePara
!        use WetSpaParamMod
!        use ReachParamMod
!        use solutionMod
!		
!		implicit none
!		real xxo(nopt), parlim(2)
!		integer num,i,iopt,iimet,Nopt
!		integer iinr(nopt)
!		character*1 tt
!		integer IParamRange,ithpid
!		
!		real ki_sub,kg_tot,T0,k_snow,k_rain,p_max,UnitSlopeM,ConductM,PoreIndexM,LaiMaxM,DepressM,RootDpthM,ItcmaxM				
!		real CH_S2M,CH_L2M,CH_N2M,CH_K2M,ImpM,petm		
!		
!		!     ~ ~ ~ PURPOSE ~ ~ ~
!		!     changing the parameters values for autocalibration
!		!     ~ ~ ~ COMMON BLOCKS ~ ~ ~
!		!
!		!     ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!		!!    nopt        |none          |number of parameters to optimise
!		!!    xxo(:)       |none          |new values of the parameters 
!		!!    iiname(:)    |none          |code refering to which parameter to change, and how
!		!!    iinr(:)      !              |number of HRUs to change for parameter (:)
!		
!		 num = 0
!         ithpid = cursolution.ithpid
!         if(allocated(sinChangeParas))then
!             do i = 1,sinChangeParaNck
!                if(sinChangeParas(i,ithpid).IRunoffRenType .eq. 2)then
!                    num = num + 1
!			        select case (num)
!			        case(1)
!				        ki_sub=sinChangeParas(i,ithpid).Def
!			        case(2)
!				        kg_tot=sinChangeParas(i,ithpid).Def
!			        case(3)
!				        T0=sinChangeParas(i,ithpid).Def
!			        case(4)
!				        k_snow=sinChangeParas(i,ithpid).Def
!			        case(5)
!				        k_rain=sinChangeParas(i,ithpid).Def
!			        case(6)
!				        p_max=sinChangeParas(i,ithpid).Def	
!			        case(7)
!				        UnitSlopeM=sinChangeParas(i,ithpid).Def				
!			        case(8)
!				        ConductM=sinChangeParas(i,ithpid).Def		
!			        case(9)
!				        PoreIndexM=sinChangeParas(i,ithpid).Def
!			        case(10)
!				        LaiMaxM=sinChangeParas(i,ithpid).Def			
!			        case(11)
!				        DepressM=sinChangeParas(i,ithpid).Def  
!			        case(12)
!				        RootDpthM=sinChangeParas(i,ithpid).Def
!			        case(13)
!				        ItcmaxM=sinChangeParas(i,ithpid).Def				
!			        case(14)
!				        CH_S2M=sinChangeParas(i,ithpid).Def
!			        case(15)
!				        CH_L2M=sinChangeParas(i,ithpid).Def
!			        case(16)
!				        CH_N2M=sinChangeParas(i,ithpid).Def
!			        case(17)
!				        CH_K2M=sinChangeParas(i,ithpid).Def
!			        case(18)
!				        ImpM=sinChangeParas(i,ithpid).Def			
!			        case(19)
!				        petm=sinChangeParas(i,ithpid).Def	
!			        end select                
!                end if
!             end do 
!         end if
!		
!		do iopt= 1,nopt
!			iimet=imet(iopt)
!			parlim(1)=bbound(1,iopt)
!			parlim(2)=bbound(2,iopt)
!			select case (iiname(iopt,1))		
!			case(1)
!				call chngp(ki_sub, xxo(iopt), iimet, parlim)
!			case(2)
!				call chngp(kg_tot, xxo(iopt), iimet, parlim)
!			case(3)
!				call chngp(T0, xxo(iopt), iimet, parlim)
!			case(4)
!				call chngp(k_snow, xxo(iopt), iimet, parlim)
!			case(5)
!				call chngp(k_rain, xxo(iopt), iimet, parlim)
!			case(6)
!				call chngp(p_max, xxo(iopt), iimet, parlim)			
!			case(7)
!				call chngp(UnitSlopeM, xxo(iopt), iimet, parlim)				
!			case(8)
!				call chngp(ConductM, xxo(iopt), iimet, parlim)				
!			case(9)
!				call chngp(PoreIndexM, xxo(iopt), iimet, parlim)	
!			case(10)
!				call chngp(LaiMaxM, xxo(iopt), iimet, parlim)					
!			case(11)
!				call chngp(DepressM, xxo(iopt), iimet, parlim)    
!			case(12)
!				call chngp(RootDpthM, xxo(iopt), iimet, parlim)
!			case(13)
!				call chngp(ItcmaxM, xxo(iopt), iimet, parlim)				
!			case(14)
!				call chngp(CH_S2M, xxo(iopt), iimet, parlim)
!			case(15)
!				call chngp(CH_L2M, xxo(iopt), iimet, parlim)
!			case(16)
!				call chngp(CH_N2M, xxo(iopt), iimet, parlim)
!			case(17)
!				call chngp(CH_K2M, xxo(iopt), iimet, parlim)
!			case(18)
!				call chngp(ImpM, xxo(iopt), iimet, parlim)
!			case(19)
!				call chngp(petm, xxo(iopt), iimet, parlim)			
!			end select			
!		enddo
!		IParamRange = Cursolution.IParamRange
!					
!        WetSpaParams(IParamRange).Ki= ki_sub
!        WetSpaParams(IParamRange).Kg=kg_tot
!        WetSpaParams(IParamRange).T0=T0
!        WetSpaParams(IParamRange).k_snow=k_snow
!        WetSpaParams(IParamRange).k_rain=k_rain
!        WetSpaParams(IParamRange).p_max=p_max
!        WetSpaParams(IParamRange).UnitSlopeM=UnitSlopeM
!        WetSpaParams(IParamRange).ConductM=ConductM
!        WetSpaParams(IParamRange).PoreIndexM=PoreIndexM
!        WetSpaParams(IParamRange).LaiMaxM=LaiMaxM
!        WetSpaParams(IParamRange).DepressM=DepressM
!        WetSpaParams(IParamRange).RootDpthM=RootDpthM
!        WetSpaParams(IParamRange).ItcmaxM=ItcmaxM
!        WetSpaParams(IParamRange).ImpM=ImpM
!        WetSpaParams(IParamRange).Petm=petm
!       
!        ReachParams(IParamRange).CH_S2M=CH_S2M
!        ReachParams(IParamRange).CH_L2M=CH_L2M
!        ReachParams(IParamRange).CH_N2M=CH_N2M
!        ReachParams(IParamRange).CH_K2M=CH_K2M		
!	end
	
!	subroutine changeXAJPar(xxo,nopt)
!        use dhmtype
!        use temptype
!        use SingleChangePara
!        use XAJParamMod
!        use ReachParamMod
!        use solutionMod
!        
!		implicit none
!		real xxo(nopt), parlim(2)
!		integer num,i,iopt,iimet,Nopt
!		integer  iinr(nopt)
!		character*3 tt
!        integer IParamRange,ithpid
!        
!        real C,IMP,WM1,WM2,WM3,Bw,SM,EX,KG,KSS,KKG,KKSS,CH_S2M,CH_L2M,CH_N2M,CH_K2M,petm
!       
!
!		!     ~ ~ ~ PURPOSE ~ ~ ~
!		!     changing the parameters values for autocalibration
!		!     ~ ~ ~ COMMON BLOCKS ~ ~ ~
!		!
!		!     ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!		!!    nopt        |none          |number of parameters to optimise
!		!!    xxo(:)       |none          |new values of the parameters 
!		!!    iiname(:)    |none          |code refering to which parameter to change, and how
!		!!    iinr(:)      !              |number of HRUs to change for parameter (:)
!		 num = 0
!		! 先读取默认参数
!        ithpid = cursolution.ithpid
!        if(allocated(sinChangeParas))then
!             do i = 1,sinChangeParaNck
!                if(sinChangeParas(i,ithpid).IRunoffRenType .eq. 3)then
!                    num = num + 1 
!                    select case (num)
!		            case(1)
!			            C=sinChangeParas(i,ithpid).Def
!		            case(2)
!			            IMP=sinChangeParas(i,ithpid).Def
!		            case(3)
!			            WM1=sinChangeParas(i,ithpid).Def
!		            case(4)
!			            WM2=sinChangeParas(i,ithpid).Def
!		            case(5)
!			            WM3=sinChangeParas(i,ithpid).Def
!		            case(6)
!			            Bw=sinChangeParas(i,ithpid).Def
!		            case(7)
!			            SM=sinChangeParas(i,ithpid).Def
!		            case(8)
!			            EX=sinChangeParas(i,ithpid).Def
!		            case(9)
!			            KG=sinChangeParas(i,ithpid).Def
!		            case(10)
!			            KSS=sinChangeParas(i,ithpid).Def
!		            case(11)
!			            KKG=sinChangeParas(i,ithpid).Def
!		            case(12)
!			            KKSS=sinChangeParas(i,ithpid).Def
!		            case(13)
!			            CH_S2M=sinChangeParas(i,ithpid).Def
!		            case(14)
!			            CH_L2M=sinChangeParas(i,ithpid).Def
!		            case(15)
!			            CH_N2M=sinChangeParas(i,ithpid).Def
!		            case(16)
!			            CH_K2M=sinChangeParas(i,ithpid).Def
!		            case(17)
!			            petm=sinChangeParas(i,ithpid).Def
!                end select
!                end if
!             end do 
!        end if
!		
!		! 再修改默认参数
!		do iopt= 1,nopt
!			iimet=imet(iopt)
!			parlim(1)=bbound(1,iopt)
!			parlim(2)=bbound(2,iopt)			
!
!			select case (iiname(iopt,1))
!			case(1)
!				call chngp(C, xxo(iopt), iimet, parlim)
!			case(2)
!				call chngp(IMP, xxo(iopt), iimet, parlim)
!			case(3)
!				call chngp(WM1, xxo(iopt), iimet, parlim)
!			case(4)
!				call chngp(WM2, xxo(iopt), iimet, parlim)
!			case(5)
!				call chngp(WM3, xxo(iopt), iimet, parlim)
!			case(6)
!				call chngp(Bw, xxo(iopt), iimet, parlim)
!			case(7)
!				call chngp(SM, xxo(iopt), iimet, parlim)
!			case(8)
!				call chngp(EX, xxo(iopt), iimet, parlim)	
!			case(9)
!				call chngp(KG, xxo(iopt), iimet, parlim)	
!			case(10)
!				call chngp(KSS, xxo(iopt), iimet, parlim)	
!			case(11)
!				call chngp(KKG, xxo(iopt), iimet, parlim)
!			case(12)
!				call chngp(KKSS, xxo(iopt), iimet, parlim)
!			case(13)
!				call chngp(CH_S2M, xxo(iopt), iimet, parlim)
!			case(14)
!				call chngp(CH_L2M, xxo(iopt), iimet, parlim)
!			case(15)
!				call chngp(CH_N2M, xxo(iopt), iimet, parlim)
!			case(16)
!				call chngp(CH_K2M, xxo(iopt), iimet, parlim)
!			case(17)
!				call chngp(petm, xxo(iopt), iimet, parlim)				
!	        end select
!		enddo
!		
!		IParamRange = Cursolution.IParamRange
!		XAJParams(IParamRange).C=C
!		XAJParams(IParamRange).IMP=IMP
!		XAJParams(IParamRange).WM1=WM1;
!		XAJParams(IParamRange).WM2=WM2
!		XAJParams(IParamRange).WM3=WM3;
!		XAJParams(IParamRange).B=Bw
!		XAJParams(IParamRange).SM=SM;	
!		XAJParams(IParamRange).EX=EX
!		XAJParams(IParamRange).KG=KG;	
!		XAJParams(IParamRange).KSS=KSS
!		XAJParams(IParamRange).KKG=KKG;
!		XAJParams(IParamRange).KKSS=KKSS		
!		XAJParams(IParamRange).PETM=PETM
!		
!		ReachParams(IParamRange).CH_S2M=CH_S2M
!        ReachParams(IParamRange).CH_L2M=CH_L2M
!        ReachParams(IParamRange).CH_N2M=CH_N2M
!        ReachParams(IParamRange).CH_K2M=CH_K2M
!       return
!    end
!    
!	subroutine changeHymodPar(xxo,nopt)
!		use dhmtype
!		use temptype
!        use SingleChangePara
!        use HymodParamMod
!        use ReachParamMod
!        use solutionMod
!		implicit none
!		real xxo(nopt), parlim(2)
!		integer num,i,iopt,iimet,Nopt
!		integer iinr(nopt)
!		character*1 tt
!		real Cmax,Bexp,Alpha,Rss,Rqq,PETM
!		real CH_S2M,CH_L2M,CH_N2M,CH_K2M
!        integer IParamRange,ithpid
!		
!
!		!     ~ ~ ~ PURPOSE ~ ~ ~
!		!     changing the parameters values for autocalibration
!		!     ~ ~ ~ COMMON BLOCKS ~ ~ ~
!		!
!		!     ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!		!!    nopt        |none          |number of parameters to optimise
!		!!    xxo(:)       |none          |new values of the parameters 
!		!!    iiname(:)    |none          |code refering to which parameter to change, and how
!		!!    iinr(:)      !              |number of HRUs to change for parameter (:)
!		num = 0
!        ithpid = cursolution.ithpid
!		if(allocated(sinChangeParas))then
!             do i = 1,sinChangeParaNck
!                if(sinChangeParas(i,ithpid).IRunoffRenType .eq. 4)then
!                    num = num + 1 
!                    select case (num)
!			        case(1)
!				        Cmax=sinChangeParas(i,ithpid).Def
!			        case(2)
!				        Bexp=sinChangeParas(i,ithpid).Def
!			        case(3)
!				        Alpha=sinChangeParas(i,ithpid).Def
!			        case(4)
!				        Rss=sinChangeParas(i,ithpid).Def
!			        case(5)
!				        Rqq=sinChangeParas(i,ithpid).Def
!			        case(6)
!				        CH_S2M=sinChangeParas(i,ithpid).Def		
!			        case(7)
!				        CH_L2M=sinChangeParas(i,ithpid).Def				
!			        case(8)
!				        CH_N2M=sinChangeParas(i,ithpid).Def		
!			        case(9)
!				        CH_K2M=sinChangeParas(i,ithpid).Def	
!			        case(10)
!				        PETM=sinChangeParas(i,ithpid).Def
!			        end select
!                end if
!             end do 
!        end if   
!		! 再修改默认参数
!		do iopt= 1,nopt
!			iimet=imet(iopt)
!			parlim(1)=bbound(1,iopt)
!			parlim(2)=bbound(2,iopt)		
!
!			select case (iiname(iopt,1))
!			case(1)
!				call chngp(Cmax, xxo(iopt), iimet, parlim)
!			case(2)
!				call chngp(Bexp, xxo(iopt), iimet, parlim)
!			case(3)
!				call chngp(Alpha, xxo(iopt), iimet, parlim)
!			case(4)
!				call chngp(Rss, xxo(iopt), iimet, parlim)
!			case(5)
!				call chngp(Rqq, xxo(iopt), iimet, parlim)
!			case(6)
!				call chngp(CH_S2M, xxo(iopt), iimet, parlim)			
!			case(7)
!				call chngp(CH_L2M, xxo(iopt), iimet, parlim)				
!			case(8)
!				call chngp(CH_N2M, xxo(iopt), iimet, parlim)				
!			case(9)
!				call chngp(CH_K2M, xxo(iopt), iimet, parlim)	
!			case(10)
!				call chngp(PETM, xxo(iopt), iimet, parlim)	
!			end select
!		enddo
!		IParamRange = Cursolution.IParamRange
!        HymodParams(IParamRange).cmax  = Cmax  
!        HymodParams(IParamRange).bexp  = Bexp
!        HymodParams(IParamRange).alpha = Alpha
!        HymodParams(IParamRange).rqq   = Rqq
!        HymodParams(IParamRange).rss   = Rss
!        HymodParams(IParamRange).PETM  = PETM
!        ReachParams(IParamRange).CH_S2M=CH_S2M
!        ReachParams(IParamRange).CH_L2M=CH_L2M
!        ReachParams(IParamRange).CH_N2M=CH_N2M
!        ReachParams(IParamRange).CH_K2M=CH_K2M
!
!        return
!    end



	! 按照指定的3种方法进行参数调整
	subroutine chngp(parval, change, iimet, parlim)
		real parval, change, parlim(2)

		select case (iimet)
		case (1)
			parval=change
		case (2)
			parval=parval+change
		case (3)
		    parval=parval *(1+change/100.)
		case (4)
		    parval=parval *change
		end select
		parval=min(parval, parlim(2))
		parval=max(parval, parlim(1))
		return
	end
