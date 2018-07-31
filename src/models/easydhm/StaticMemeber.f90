module StaticMemeberMod
    
    use ModelconfigMod
    use SolutionMod
    use WaterShedMod
    use ReachInfoMod
    use UnitInfoMod
    use StringFiltrMod
    use HydroInfoMod
    use ResInfoMod
    use ResRangeMod
    use ParamRangeMod
    use SoilInfoMod
    use HydroDataMod 
    use SolutionMod
    use WeatherMod
    use EasyDHMParamMod
!    use WetSpaParamMod
!    use XAJParamMod
!    use HymodParamMod
    use ReachParamMod
    use InitDHMStateMod
!    use InitWSPStateMod
!    use InitXAJStateMod
!    use InitHymodStateMod
    use HydroObservedMod
!    use SolutionMod
    implicit none
    integer, dimension(:),allocatable :: Istr
    real,dimension(:),allocatable :: rstr
    integer:: ipidflood=1
    integer IRunoffGenType
contains

        subroutine IniSolution_Ex(npid)
            integer npid
            if(allocated(TotalSolution))then
            deallocate(TotalSolution)
            endif
            if(allocated(m_ModelConfig))then
            deallocate(m_ModelConfig)
            endif
            allocate(TotalSolution(npid))
            allocate(m_ModelConfig(npid))
        endsubroutine
        
        subroutine DestroySolution
            if(allocated(TotalSolution))then
            deallocate(TotalSolution)
            endif
            if(allocated(m_ModelConfig))then
            deallocate(m_ModelConfig)
            endif
        end subroutine

    subroutine InitModelConfigEx(ModelconfigVars,npid) !dcb  InitModelConfig
        Integer::i,len,length,npid,ipid

        real,dimension(12,npid) :: ModelconfigVars
        do ipid=1,npid
            m_Modelconfig(ipid)%SID                     = ModelconfigVars(1,ipid)
            m_Modelconfig(ipid)%pid                     = ModelconfigVars(2,ipid)
            m_ModelConfig(ipid)%Rtmdy                   = ModelconfigVars(3,ipid)
            m_ModelConfig(ipid)%IParaYear               = ModelconfigVars(4,ipid)
            m_ModelConfig(ipid)%NRunoffGenType          = ModelconfigVars(5,ipid)
            m_Modelconfig(ipid)%Runtype                 = ModelconfigVars(6,ipid)
!            m_Modelconfig(ipid)%Runtype                 = 5
            m_Modelconfig(ipid)%irte                    = ModelconfigVars(7,ipid)
            m_ModelConfig(ipid)%GWSimType               = ModelconfigVars(8,ipid)
            m_Modelconfig(ipid)%iPet                    = ModelconfigVars(9,ipid)
            m_ModelConfig(ipid)%RtmdyType               = ModelconfigVars(10,ipid)
            m_ModelConfig(ipid)%AR_NUM                  = ModelconfigVars(11,ipid)
            m_ModelConfig(ipid)%TimeStepOpt                  = ModelconfigVars(12,ipid)                 
        enddo
        initSuc = 1
    end subroutine

    subroutine InitModelConfigvar_Ex(idx,IRunoffGenTypeStr,len,npid) !dcb  InitModelConfig
        Integer::i,len,length,npid,idx
        character(len) IRunoffGenTypeStr1
        character*len::IRunoffGenTypeStr

                 
        if(allocated(m_ModelConfig(idx)%IRunoffGenType))deallocate(m_ModelConfig(idx)%IRunoffGenType)
        allocate(m_ModelConfig(idx)%IRunoffGenType(m_ModelConfig(idx)%NRunoffGenType))
        if(ichar(IRunoffGenTypeStr(1:1)) /= 0)then
            call GetStrSplitterCount(IRunoffGenTypeStr,len,length,' ')
            read(IRunoffGenTypeStr,*) (m_ModelConfig(idx)%IRunoffGenType(i),i=1,m_ModelConfig(idx)%NRunoffGenType,1)
            
        endif
        IRunoffGenType = m_ModelConfig(idx)%IRunoffGenType(1)
        initSuc = 1

    end subroutine


    
    subroutine initFloodPeakTime_Ex(nidx,TimeStart,TimeEnd,nCount,floodpidvar,npidflood,rank)
        integer ipid,ncount,isid,npidflood,floodpidvar(npidflood,2),nidx,rank,temp(npidflood,2),temp2,npid
!        character*30 timeStart(ncount),timeEnd(ncount)
        character*30 timeStart,timeEnd

        npid=size(totalSolution)
        temp=floodpidvar
        if(nidx==1) ipidflood=1
10      if(temp(ipidflood,2)==0)then
            do ipid=1,npid
                
                if(totalSolution(ipid)%IParamRange==temp(ipidflood,1))then
                    if (allocated(totalSolution(ipid)%StartDateTime))deallocate(totalSolution(ipid)%StartDateTime)
                    allocate(totalSolution(ipid)%StartDateTime(temp(ipidflood,2)))
                    if (allocated(totalSolution(ipid)%EndDateTime))deallocate(totalSolution(ipid)%EndDateTime)
                    allocate(totalSolution(ipid)%EndDateTime(temp(ipidflood,2)))
                    if (allocated(totalSolution(ipid)%NFloodSeries))deallocate(totalSolution(ipid)%NFloodSeries)
                    allocate(totalSolution(ipid)%NFloodSeries(temp(ipidflood,2)))
                    if (allocated(totalSolution(ipid)%floodloc))deallocate(totalSolution(ipid)%floodloc)
                    allocate(totalSolution(ipid)%floodloc(temp(ipidflood,2)))
                    if (allocated(totalSolution(ipid)%peakloc))deallocate(totalSolution(ipid)%peakloc)
                    allocate(totalSolution(ipid)%peakloc(temp(ipidflood,2)))
                    if (allocated(totalSolution(ipid)%rank))deallocate(totalSolution(ipid)%rank)
                    allocate(totalSolution(ipid)%rank(temp(ipidflood,2)))
                    if (allocated(totalSolution(ipid)%flooderr))deallocate(totalSolution(ipid)%flooderr)
                    allocate(totalSolution(ipid)%flooderr(temp(ipidflood,2),6))
                    if (allocated(totalSolution(ipid)%flooderrRes))deallocate(totalSolution(ipid)%flooderrRes)
                    allocate(totalSolution(ipid)%flooderrRes(temp(ipidflood,2),6))
                    totalSolution(ipid)%nflood=temp(ipidflood,2)
                    totalSolution(ipid)%NFloodSeries=0   
                    totalSolution(ipid)%Floodloc=0   
                    totalSolution(ipid)%peakloc=0
                    totalSolution(ipid)%flooderr=0   
                    totalSolution(ipid)%flooderrRes=0
                    exit   
                endif
            enddo
            ipidflood=ipidflood+1
            goto 10
        elseif(nidx==sum(temp(1:ipidflood-1,2))+1)then
            do ipid=1,npid
                if(totalSolution(ipid)%IParamRange==temp(ipidflood,1))then
                    if (allocated(totalSolution(ipid)%StartDateTime))deallocate(totalSolution(ipid)%StartDateTime)
                    allocate(totalSolution(ipid)%StartDateTime(temp(ipidflood,2)))
                    if (allocated(totalSolution(ipid)%EndDateTime))deallocate(totalSolution(ipid)%EndDateTime)
                    allocate(totalSolution(ipid)%EndDateTime(temp(ipidflood,2)))
                    if (allocated(totalSolution(ipid)%NFloodSeries))deallocate(totalSolution(ipid)%NFloodSeries)
                    allocate(totalSolution(ipid)%NFloodSeries(temp(ipidflood,2))) 
                    if (allocated(totalSolution(ipid)%floodloc))deallocate(totalSolution(ipid)%floodloc)
                    allocate(totalSolution(ipid)%floodloc(temp(ipidflood,2)))
                    if (allocated(totalSolution(ipid)%peakloc))deallocate(totalSolution(ipid)%peakloc)
                    allocate(totalSolution(ipid)%peakloc(temp(ipidflood,2)))
                    if (allocated(totalSolution(ipid)%rank))deallocate(totalSolution(ipid)%rank)
                    allocate(totalSolution(ipid)%rank(temp(ipidflood,2)))
                    if (allocated(totalSolution(ipid)%flooderr))deallocate(totalSolution(ipid)%flooderr)
                    allocate(totalSolution(ipid)%flooderr(temp(ipidflood,2),6))
                    if (allocated(totalSolution(ipid)%flooderrRes))deallocate(totalSolution(ipid)%flooderrRes)
                    allocate(totalSolution(ipid)%flooderrRes(temp(ipidflood,2),6))
                    totalSolution(ipid)%nflood=temp(ipidflood,2) 
                    totalSolution(ipid)%NFloodSeries=0
                    totalSolution(ipid)%Floodloc=0
                    totalSolution(ipid)%peakloc=0
                    totalSolution(ipid)%flooderr=0   
                    totalSolution(ipid)%flooderrRes=0 
                    temp2= sum(temp(1:ipidflood-1,2))
                    totalSolution(ipid)%rank(nidx-temp2)=rank
                    totalSolution(ipid)%StartDateTime(nidx-temp2)=TimeStart
                    totalSolution(ipid)%EndDateTime(nidx-temp2)=TimeEnd
                    exit
                endif
            enddo
            temp2=   sum(temp(1:ipidflood,2))
            if(nidx==temp2)then
                ipidflood=ipidflood+1
            endif
        else
            do ipid=1,npid
                if(totalSolution(ipid)%IParamRange==temp(ipidflood,1))then
                    totalSolution(ipid)%StartDateTime(nidx-sum(temp(1:ipidflood-1,2)))=TimeStart
                    totalSolution(ipid)%EndDateTime(nidx-sum(temp(1:ipidflood-1,2)))=TimeEnd
!                    totalSolution(ipid)%rank(temp(1:ipidflood-1,2))=rank
                    totalSolution(ipid)%rank(nidx-sum(temp(1:ipidflood-1,2)))=rank
                    exit
                endif
            enddo
            temp2=   sum(temp(1:ipidflood,2))
            if(nidx==temp2)then
                ipidflood=ipidflood+1
            endif
        endif


    end subroutine
    

      subroutine GetSolutionEx(SolutionVars,SolutionName,len,DateTemp1,DateTemp2,npid,obsorsim,DTALL,TIMESALL) !dcb GetSolution
        character(len)::SolutionName
        real,dimension(6) ::SolutionVars
        real,dimension(6) ::DateTemp1
        real,dimension(6) ::DateTemp2
        integer,dimension(3)::DTALL
        integer,dimension(3)::TIMESALL
        integer            ::len,i,ipid,npid,obsorsim
        do ipid=1,npid
            totalSolution(ipid)%SolutionId    = SolutionVars(1)
!            totalSolution(ipid)%IParamRange   = SolutionVars(2)
            totalSolution(ipid)%SolutionName  = SolutionName
!            totalSolution(ipid)%TimeStepOpt   = SolutionVars(3)
            totalSolution(ipid)%TimeStepOpt   = m_ModelConfig(ipid)%TimeStepOpt
            totalSolution(ipid)%NTimeStepInit = SolutionVars(4)
            totalSolution(ipid)%dtold(1)            = DTALL(1)
            totalSolution(ipid)%dtold(2)            = DTALL(2)
            totalSolution(ipid)%dtold(3)            = DTALL(3)
            totalSolution(ipid)%timesall(1)      = TIMESALL(1)
            totalSolution(ipid)%timesall(2)      = TIMESALL(2)
            totalSolution(ipid)%timesall(3)      = TIMESALL(3)
            totalSolution(ipid)%IWeather      = SolutionVars(6)
            
            totalSolution(ipid)%CalYears(1)=DateTemp1(1)
            totalSolution(ipid)%CalMonths(1)=DateTemp1(2)
            totalSolution(ipid)%CalDays(1)=DateTemp1(3)
            totalSolution(ipid)%CalHours(1)=DateTemp1(4)
            totalSolution(ipid)%CalYears(2)=DateTemp2(1)
            totalSolution(ipid)%CalMonths(2)=DateTemp2(2)
            totalSolution(ipid)%CalDays(2)=DateTemp2(3)
            totalSolution(ipid)%CalHours(2)=DateTemp2(4)
            totalSolution(ipid)%IParamYear = m_Modelconfig(ipid)%IParaYear
            totalSolution(ipid)%Rtmdy = m_Modelconfig(ipid)%Rtmdy
            totalSolution(ipid)%Runtype = m_Modelconfig(ipid)%Runtype
            totalSolution(ipid)%IRouteType = m_Modelconfig(ipid)%irte
            totalSolution(ipid)%GWSimType = m_Modelconfig(ipid)%GWSimType
            totalSolution(ipid)%iPet = m_Modelconfig(ipid)%iPet
            totalSolution(ipid)%RtmdyType = m_Modelconfig(ipid)%RtmdyType
            totalSolution(ipid)%AR_NUM = m_Modelconfig(ipid)%AR_NUM
            totalSolution(ipid)%NRunoffGenType = m_Modelconfig(ipid)%NRunoffGenType
            totalSolution(ipid)%pid = m_Modelconfig(ipid)%pid
            totalSolution(ipid)%RouteDT = totalSolution(ipid)%dtold(1)
            totalSolution(ipid)%dt = totalSolution(ipid)%dtold(1)
            totalSolution(ipid)%IParamRange   =m_Modelconfig(ipid)%pid  
            totalSolution(ipid)%obsorsim   =  obsorsim
!             totalSolution(ipid)%obsorsim   =  1

            if(allocated(totalSolution(ipid)%IRunoffGenType))deallocate(totalSolution(Param.ipid)%IRunoffGenType)
            allocate(totalSolution(ipid)%IRunoffGenType(totalSolution(ipid)%NRunoffGenType))
            
            do i = 1, totalSolution(ipid)%NRunoffGenType 
                totalSolution(ipid)%IRunoffGenType(i) = m_Modelconfig(ipid)%IRunoffGenType(i)
            enddo 
!        call CalMonth()						! 时间信息计算
        enddo        
        GetSolutionSuc = 1
    end subroutine  
    
    
!    subroutine GetSolutionEx(SolutionVars,SolutionName,len,DateTemp1,DateTemp2) !dcb GetSolution
!        character(len)::SolutionName
!        real,dimension(5) ::SolutionVars
!        real,dimension(6) ::DateTemp1
!        real,dimension(6) ::DateTemp2
!        integer            ::len,i
!        
!        CurSolution%SolutionId    = SolutionVars(1)
!        CurSolution%IParamRange   = SolutionVars(2)
!        CurSolution%SolutionName  = SolutionName
!        CurSolution%TimeStepOpt   = SolutionVars(3)
!        CurSolution%NTimeStepInit = SolutionVars(4)
!        CurSolution%dt            = SolutionVars(5)
!        
!        CurSolution%CalYears(1)=DateTemp1(1)
!        CurSolution%CalMonths(1)=DateTemp1(2)
!        CurSolution%CalDays(1)=DateTemp1(3)
!        CurSolution%CalHours(1)=DateTemp1(4)
!        CurSolution%CalYears(2)=DateTemp2(1)
!        CurSolution%CalMonths(2)=DateTemp2(2)
!        CurSolution%CalDays(2)=DateTemp2(3)
!        CurSolution%CalHours(2)=DateTemp2(4)
!        CurSolution%IParamYear = m_Modelconfig%IParaYear
!        CurSolution%Rtmdy = m_Modelconfig%Rtmdy
!        CurSolution%Runtype = m_Modelconfig%Runtype
!        CurSolution%IRouteType = m_Modelconfig%irte
!        CurSolution%GWSimType = m_Modelconfig%GWSimType
!        CurSolution%iPet = m_Modelconfig%iPet
!        CurSolution%RtmdyType = m_Modelconfig%RtmdyType
!        CurSolution%AR_NUM = m_Modelconfig%AR_NUM
!        CurSolution%NRunoffGenType = m_Modelconfig%NRunoffGenType
!        CurSolution%RouteDT = CurSolution%dt
!                
!        if(allocated(CurSolution%IRunoffGenType))deallocate(CurSolution%IRunoffGenType)
!        allocate(CurSolution%IRunoffGenType(CurSolution%NRunoffGenType))
!        
!        do i = 1, CurSolution%NRunoffGenType 
!            CurSolution%IRunoffGenType(i) = m_Modelconfig%IRunoffGenType(i)
!        enddo 
!        call CalMonth()						! 时间信息计算
!                
!        GetSolutionSuc = 1
!    end subroutine
    
!    subroutine ChangeSolutionParam(pid)
!        integer pid
!        CurSolution%IParamRange = pid
!    end subroutine
    
    subroutine InitWaterShedVars(vars)!dcb WaterShed
        integer             ::i
        real, dimension(7) ::vars
        WaterShed%NSubbasin        = vars(1)
        WaterShed%NUnit            = vars(2)   
        WaterShed%NReach           = vars(3)
        WaterShed%NParamRange      = vars(4)
        WaterShed%NHydroStation    = vars(5)
        WaterShed%NResRange        = vars(6)
        WaterShed%NLayer           = vars(7)
        if( WaterShed%NHydroStation ==WaterShed%NParamRange) then
            WaterShed%NParamRange = WaterShed%NParamRange - WaterShed%NResRange
        endif
        
        ! 水库基本信息
        do i=1,WaterShed%NResRange
            WaterShed%NResIn = WaterShed%NResIn + 1
            WaterShed%NResOut = WaterShed%NResOut + 1
        enddo 
    end subroutine

    subroutine ReachersChars(nIndex,nCount,str,len)
        character(30) ::str
        integer        ::nIndex,nCount,nStart,len,j,itemp,length1,temp
        integer, dimension(:),allocatable :: Istr

        nStart = nIndex
        temp = 0
        
        if(allocated(Reachs))then
           Reachs(nIndex)%UPRCHs(1:len) = str
           len = 30
           if (Reachs(nStart).NUPRCH .ne.0) then
                do j=1,30
                itemp = iachar(Reachs(nStart).UPRCHs(j:j))
                  if(itemp .eq. 0) then
                     temp = j
                     exit
                   endif
                end do
!                if (nstart == 113) then
!                    write(*,*)
!                endif
                call GetStrSplitterCount(Reachs(nStart).UPRCHs,len,length1,',')
                if(allocated(Reachs(nStart).IUPRCH))deallocate(Reachs(nStart).IUPRCH)
                allocate(Reachs(nStart).IUPRCH(Reachs(nStart).NUPRCH))
                if(length1 .gt. 0)then
                    if(allocated(istr)) deallocate(istr)
                    allocate(istr(length1))
                    read(Reachs(nStart).UPRCHs,*)  (istr(j),j=1,length1,1)
                    do j = 1,Reachs(nStart).NUPRCH
                        Reachs(nStart).IUPRCH(j) = istr(j)
                    enddo
                    
                endif
            endif  
        endif
    end subroutine
    
    subroutine InitReachsVars(NReachCKVar,ReachsVars)
        integer                                ::NReachCKVar 
        real,dimension(20,NReachCKVar)        ::ReachsVars
        integer                                ::nStart
        NReachCK = NReachCKVar
        if (NReachCK .ne. WaterShed.NReach) then
            !write(*,*) "河段数出现异常，按Enter继续"
        endif
        if(allocated(Reachs))deallocate(Reachs)
        allocate(Reachs(NReachCK))
        
        do nStart = 1,NReachCK 
!            Reachs(nStart)%ReachId = ReachsVars(1,nStart) 
!            Reachs(nStart)%My    = ReachsVars(2,nStart) 
!            Reachs(nStart)%Area  = ReachsVars(3,nStart) 
!            Reachs(nStart)%Xcoor = ReachsVars(4,nStart) 
!            Reachs(nStart)%Ycoor = ReachsVars(5,nStart) 
!            Reachs(nStart)%Lati  = ReachsVars(6,nStart) 
!            Reachs(nStart)%BsDem = ReachsVars(7,nStart) 
!            Reachs(nStart)%T_L1  = ReachsVars(8,nStart) 
!            Reachs(nStart)%T_S1  = ReachsVars(9,nStart) 
!            Reachs(nStart)%T_W1  = ReachsVars(10,nStart) 
!            Reachs(nStart)%T_K1  = ReachsVars(11,nStart) 
!            Reachs(nStart)%T_N1  = ReachsVars(12,nStart) 
!            Reachs(nStart)%M_W2  = ReachsVars(13,nStart) 
!            Reachs(nStart)%M_D   = ReachsVars(14,nStart) 
!            Reachs(nStart)%M_S2  = ReachsVars(15,nStart) 
!            Reachs(nStart)%M_L2  = ReachsVars(16,nStart) 
!            Reachs(nStart)%M_N2  = ReachsVars(17,nStart) 
!            Reachs(nStart)%M_K2  = ReachsVars(18,nStart) 
!            Reachs(nStart)%SLSUBBSN = ReachsVars(19,nStart) 
!            Reachs(nStart)%NUPRCH   = ReachsVars(20,nStart) 
!                      
!            Reachs(nStart)%Area = Reachs(nStart)%Area*1000000.0 
!            Reachs(nStart)%Lati = Reachs(nStart)%Lati*3.1415927/180.
!		    !处理平坡度
!            if (Reachs(nStart)%T_S1 .le. 0) then
!                Reachs(nStart)%T_S1 = 0.0001
!            endif
!            
!            if (Reachs(nStart)%M_S2 .le. 0) then
!                Reachs(nStart)%M_S2 = 0.0001
!            endif                    
!            Reachs(nStart)%lati = Reachs(nStart)%lati / 90.           
            m_Reach%ReachId = ReachsVars(1,nStart) 
            m_Reach%My    = ReachsVars(2,nStart) 
            m_Reach%Area  = ReachsVars(3,nStart) 
            m_Reach%Xcoor = ReachsVars(4,nStart) 
            m_Reach%Ycoor = ReachsVars(5,nStart) 
            m_Reach%Lati  = ReachsVars(6,nStart) 
            m_Reach%BsDem = ReachsVars(7,nStart) 
            m_Reach%T_L1  = ReachsVars(8,nStart) 
            m_Reach%T_S1  = ReachsVars(9,nStart) 
            m_Reach%T_W1  = ReachsVars(10,nStart) 
            m_Reach%T_K1  = ReachsVars(11,nStart) 
            m_Reach%T_N1  = ReachsVars(12,nStart) 
            m_Reach%M_W2  = ReachsVars(13,nStart) 
            m_Reach%M_D   = ReachsVars(14,nStart) 
            m_Reach%M_S2  = ReachsVars(15,nStart) 
            m_Reach%M_L2  = ReachsVars(16,nStart) 
            m_Reach%M_N2  = ReachsVars(17,nStart) 
            m_Reach%M_K2  = ReachsVars(18,nStart) 
            m_Reach%SLSUBBSN = ReachsVars(19,nStart) 
            m_Reach%NUPRCH   = ReachsVars(20,nStart) 
                      
            m_Reach%Area = m_Reach%Area*1000000.0 
            m_Reach%Lati = m_Reach%Lati*3.1415927/180.
		    !处理平坡度
            if (m_Reach%T_S1 .le. 0) then
                m_Reach%T_S1 = 0.0001
            endif
            
            if (m_Reach%M_S2 .le. 0) then
                m_Reach%M_S2 = 0.0001
            endif                    
            !m_Reach%lati = m_Reach%lati / 90.         ! 2017-08-31 这里到底是什么处理？在 426 行的时候已经换算成弧度了，这里为什么还要再除以90？ 所以就去掉了。
            Reachs(nStart)           = m_Reach    
        end do    
    end subroutine
    
    subroutine InitUnitsVars(NUnitCKVar,UnitVars,Nlanduse)!dcb InitUnits
        integer                        ::NUnitCKVar,nStart,Nlanduse
        real,dimension(46,NUnitCKVar) ::UnitVars
        if(allocated(Units))deallocate(Units)
        allocate(Units(WaterShed.NSubbasin,WaterShed.NUnit))
        
        do nStart = 1,NUnitCKVar
            m_Unit.ylanduse = UnitVars(1,nStart)
            m_Unit.SubId     = UnitVars(2,nStart)
            m_Unit.UnitId    = UnitVars(3,nStart)
            m_Unit.HEIGM     = UnitVars(4,nStart)
            m_Unit.Slope     = UnitVars(5,nStart)
            m_Unit.Area(1)      = UnitVars(6,nStart)
            m_Unit.With      = UnitVars(7,nStart)
            m_Unit.Alth      = UnitVars(8,nStart)
            m_Unit.Runoff    = UnitVars(9,nStart)
            m_Unit.Conduct   = UnitVars(10,nStart)
            m_Unit.Porosity  = UnitVars(11,nStart)
            m_Unit.Fieldcap  = UnitVars(12,nStart)
            m_Unit.Poreindex = UnitVars(13,nStart)
            m_Unit.Wilting   = UnitVars(14,nStart)
            m_Unit.Residual  = UnitVars(15,nStart)
            m_Unit.Lai_max(1)   = UnitVars(16,nStart)
            m_Unit.Lai_min(1)   = UnitVars(17,nStart)
            m_Unit.Depression= UnitVars(18,nStart)
            m_Unit.Rootdepth(1) = UnitVars(19,nStart)
            m_Unit.Imp       = UnitVars(20,nStart)
            m_Unit.Itc_max(1)   = UnitVars(21,nStart)
            m_Unit.Lai_max(2)       =   UnitVars(22,nStart)
            m_Unit.Lai_min(2)       =   UnitVars(23,nStart)
            m_Unit.Itc_max(2)       =   UnitVars(24,nStart)
            m_Unit.Rootdepth(2)       =   UnitVars(25,nStart)
            m_Unit.Lai_max(3)       =   UnitVars(26,nStart)
            m_Unit.Lai_min(3)       =   UnitVars(27,nStart)
            m_Unit.Itc_max(3)       =   UnitVars(28,nStart)
            m_Unit.Rootdepth(3)       =   UnitVars(29,nStart)
            m_Unit.Lai_max(4)       =   UnitVars(30,nStart)
            m_Unit.Lai_min(4)       =   UnitVars(31,nStart)
            m_Unit.Itc_max(4)       =   UnitVars(32,nStart)
            m_Unit.Rootdepth(4)       =   UnitVars(33,nStart)
            m_Unit.Lai_max(5)       =   UnitVars(34,nStart)
            m_Unit.Lai_min(5)       =   UnitVars(35,nStart)
            m_Unit.Itc_max(5)       =   UnitVars(36,nStart)
            m_Unit.Rootdepth(5)       =   UnitVars(37,nStart)
            m_Unit.Lai_max(6)       =   UnitVars(38,nStart)
            m_Unit.Lai_min(6)       =   UnitVars(39,nStart)
            m_Unit.Itc_max(6)       =   UnitVars(40,nStart)
            m_Unit.Rootdepth(6)       =   UnitVars(41,nStart)
            m_Unit.Area(2)        =   UnitVars(42,nStart)
            m_Unit.Area(3)        =   UnitVars(43,nStart)
            m_Unit.Area(4)        =   UnitVars(44,nStart)
            m_Unit.Area(5)        =   UnitVars(45,nStart)
            m_Unit.Area(6)        =   UnitVars(46,nStart)
            m_Unit.Area(1)      = m_Unit.Area(1)*1000000
            m_Unit.Area(2)      = m_Unit.Area(2)*1000000
            m_Unit.Area(3)      = m_Unit.Area(3)*1000000
            m_Unit.Area(4)      = m_Unit.Area(4)*1000000
            m_Unit.Area(5)      = m_Unit.Area(5)*1000000
            m_Unit.Area(6)      = m_Unit.Area(6)*1000000
            UnitNlanduse=Nlanduse
            if(Nlanduse == 1 .or. IRunoffGenType >1) then
                m_Unit.Area(1) = m_Unit.Area(1) + m_Unit.Area(2) + m_Unit.Area(3) + m_Unit.Area(4) + m_Unit.Area(5) + m_Unit.Area(6)
            endif

            Units(m_Unit.SubId,m_Unit.UnitId)           = m_Unit
            Units(m_Unit.SubId,m_Unit.UnitId).SubUnitId = nStart
        end do 
    end subroutine
    
    subroutine HydroInfosChars(nIdx,nCount,str1,len1,str2,len2)
        character(len1) ::str1
        character(len2) ::str2
        integer       ::nIdx,nCount,len1,len2
        
        if(allocated(HydroInfos))then
            HydroInfos(nIdx)%Stcd(1:len1)     = str1
            HydroInfos(nIdx)%Stnm(1:len2)     = str2          
        endif
    end subroutine
    
    subroutine InitHydroInfosVars(NHydroInfoCKVar,HydroInfosVars) 
        integer ::NHydroInfoCKVar,nStart
        real,dimension(5,NHydroInfoCKVar) ::HydroInfosVars
         NHydroInfoCK = NHydroInfoCKVar
        if (NHydroInfoCK .ne. WaterShed.NHydroStation) then
            !write(*,*) "水文站数出现异常，按Enter继续"
        endif
        if(allocated(HydroInfos))deallocate(HydroInfos)
        allocate(HydroInfos(NHydroInfoCK))
        do nStart = 1, NHydroInfoCKVar
            m_HydroInfo%HydroId  = HydroInfosVars(1,nStart)
            m_HydroInfo%Long     = HydroInfosVars(2,nStart)
            m_HydroInfo%Lati     = HydroInfosVars(3,nStart)
            m_HydroInfo%PointX   = HydroInfosVars(4,nStart)
            m_HydroInfo%PointY   = HydroInfosVars(5,nStart)
            HydroInfos(nStart)   = m_HydroInfo             
        end do 
    end subroutine
    
    subroutine ResInfosChars(nidx,ncount,stcd,lenstcd,resname,lenres)
        character(lenstcd) ::stcd
        character(lenres)  ::resname
        integer             ::nidx,ncount,lenstcd,lenres
        if(allocated(ResInfos))then
            m_ResInfo.Stcd(1:lenstcd)           = stcd
            m_ResInfo.ResName(1:lenres)         = resname
        endif
    endsubroutine
    
    subroutine InitResInfosVars(NResCKVar,ResInfosVars) 
        integer ::NResCKVar,nStart
        real,dimension(6,NResCKVar) ::ResInfosVars
        NResCK = NResCKVar
        
        if (NResCK .ne. WaterShed.NResRange) then
            !write(*,*) "水库数出现异常，按Enter继续"
        endif
        if(allocated(ResInfos))deallocate(ResInfos)
        allocate(ResInfos(NResCK))
        do nStart = 1,NResCKVar
            m_ResInfo.ResId          = ResInfosVars(1,nStart)
            m_ResInfo.ControledArea  = ResInfosVars(2,nStart)
            m_ResInfo.Long           = ResInfosVars(3,nStart)
            m_ResInfo.lati           = ResInfosVars(4,nStart)
            m_ResInfo.PointX         = ResInfosVars(5,nStart)
            m_ResInfo.PointY         = ResInfosVars(6,nStart)          
            ResInfos(nStart)         = m_ResInfo
        end do 
    end subroutine

    subroutine InitFloodStoreInfoVars(NFLoodStores,StoreInfosVars) 
        integer ::NFLoodStores,nstart,locsub
        real,dimension(7,NFLoodStores) ::StoreInfosVars

        do nStart = 1,NFLoodStores
            locsub = StoreInfosVars(3,nStart)
            Reachs(LocSub).IStore = 1
            Reachs(LocSub).StoreAreaID = StoreInfosVars(1,nStart)
            Reachs(LocSub).StoreCoef = StoreInfosVars(5,nStart)
            Reachs(LocSub).Qlimit = StoreInfosVars(6,nStart)
            Reachs(LocSub).Vlimit = StoreInfosVars(7,nStart)

        end do 
    end subroutine

    subroutine InitDiveInfoVars(NDives,DiveInfosVars)
        integer ::NDives,nstart,locsub
        real,dimension(5,NDives) ::DiveInfosVars

        do nStart = 1,NDives
            locsub = DiveInfosVars(3,nStart)
            Reachs(LocSub).IDive = 1
            Reachs(LocSub).DiveID = DiveInfosVars(1,nStart)
            Reachs(LocSub).QDive = DiveInfosVars(5,nStart)

        end do 
    end subroutine
    
    subroutine ResRangesChars(nidx,ncount,stnm,len1,PartSubbasinString,len2,UpStreamResRangeString,len3)
        character(len1)  ::stnm
        character(len2)  ::PartSubbasinString
        character(len3)  ::UpStreamResRangeString
        integer          ::nidx,ncount,len1,len2,len3,length1,length2,j,i,NResRangeCK
        NResRangeCK = ncount 
        if(NResRangeCK .gt. 0)then
           ResRanges(nidx).stnm(1:len1)                = stnm
           
           if(ResRanges(nidx).NPartSubbasin .ne. 0)then 
               if(ichar(PartSubbasinString(1:1)) /= 0)then         
                   call GetStrSplitterCount(PartSubbasinString,len2,length1,',')
                   if(allocated(istr)) deallocate(istr)
                   allocate(istr(length1))
                   read(PartSubbasinString,*) (istr(j),j=1,length1,1)
                   do j = 1,ResRanges(nidx).NPartSubbasin
                        ResRanges(nidx).PartSubbasins(j) = istr(j)
                   enddo
               endif
           endif              
           if(ResRanges(nidx).NUpStreamResRange .ne. 0)then
               if(ichar(UpStreamResRangeString(1:1)) /= 0)then
                   call GetStrSplitterCount(UpStreamResRangeString,len3,length2,',')
                   if(allocated(istr)) deallocate(istr)
                   allocate(istr(length2))
                   read(UpStreamResRangeString,*) (istr(j),j=1,length2,1)
                   do j = 1,ResRanges(nidx).NUpStreamResRange
                        ResRanges(nidx).UpStreamResRanges(j) = istr(j)
                   enddo
               endif
           endif           

        endif
        
        
    end subroutine
    
    subroutine EndResRanges  
           integer ::i,j,k  
           DO i=1,NResRangeCK,1
            call GetUpStreamResRangesAll(i,i)
            do j=1,ResRanges(i).NPartSubbasin,1
                ResRanges(i).AllSubbasins(ResRanges(i).PartSubbasins(j))=1
            end do
            Do k=1,NResRangeCK,1
                if(UpStreamResRangesAll(i,k).eq.1) then
                    do j=1,ResRanges(k).NPartSubbasin,1
                        ResRanges(i).AllSubbasins(ResRanges(k).PartSubbasins(j))=1
                    enddo
                endif
            Enddo
        ENDDO
    end subroutine
    
    subroutine InitResRangesVars(NResRangeCKVar,ResRangesVars) 
       integer ::NResRangeCKVar,i,j,k,length1,length2
       real,dimension(8,NResRangeCKVar) ::ResRangesVars
       NResRangeCK = NResRangeCKVar
       
       if (NResRangeCK .ne. WaterShed.NResRange) then
           !write(*,*) "水库数出现异常，按Enter继续"
       endif   
        if(allocated(ResRanges))deallocate(ResRanges)
        allocate(ResRanges(NResRangeCK))
        if(allocated(UpStreamResRangesAll))deallocate(UpStreamResRangesAll)
        allocate(UpStreamResRangesAll(NResRangeCK,NResRangeCK))
        UpStreamResRangesAll=0
        if(allocated(m_ResRange.PartSubbasins))deallocate(m_ResRange.PartSubbasins)
        allocate(m_ResRange.PartSubbasins(WaterShed.NSubbasin))
        m_ResRange.PartSubbasins=0
        if(allocated(m_ResRange.AllSubbasins))deallocate(m_ResRange.AllSubbasins)
        allocate(m_ResRange.AllSubbasins(WaterShed.NSubbasin))
        m_ResRange.AllSubbasins=0
        if(allocated(m_ResRange.UpStreamResRanges))deallocate(m_ResRange.UpStreamResRanges)
        allocate(m_ResRange.UpStreamResRanges(NResRangeCK))
        m_ResRange.UpStreamResRanges=0
        
        do i=1,NResRangeCK
            if(allocated(ResRanges(i).PartSubbasins))deallocate(ResRanges(i).PartSubbasins)
            allocate(ResRanges(i).PartSubbasins(WaterShed.NSubbasin))
            ResRanges(i).PartSubbasins=0
            if(allocated(ResRanges(i).AllSubbasins))deallocate(ResRanges(i).AllSubbasins)
            allocate(ResRanges(i).AllSubbasins(WaterShed.NSubbasin))
            ResRanges(i).AllSubbasins=0
            if(allocated(ResRanges(i).UpStreamResRanges))deallocate(ResRanges(i).UpStreamResRanges)
            allocate(ResRanges(i).UpStreamResRanges(NResRangeCK))
            ResRanges(i).UpStreamResRanges=0
        end do
        
        DO i=1,NResRangeCK,1
            m_ResRange.ResRangeId          = ResRangesVars(1,i)
            m_ResRange.RunYear             = ResRangesVars(2,i)
            m_ResRange.IsInObs             = ResRangesVars(3,i)
            m_ResRange.IsOutObs            = ResRangesVars(4,i)
            m_ResRange.LocSub              = ResRangesVars(5,i)
            m_ResRange.LocParamRange       = ResRangesVars(6,i)
            m_ResRange.NPartSubbasin       = ResRangesVars(7,i)
            m_ResRange.NUpStreamResRange   = ResRangesVars(8,i)            
            ResRanges(i) = m_ResRange
            Reachs(m_ResRange.LocSub).IRes = 1
            Reachs(m_ResRange.LocSub).ResID = m_ResRange.ResRangeId
        end DO
    end subroutine
    
    subroutine EndParamRanges(NParamRangeCK)
        integer ::NParamRangeCK,i,j,k
            DO i=1,NParamRangeCK
                !求i分区对应的上游所有参数分区，记录在ParamRangeUpStreamAll中
                call GetParamRangeUpStream(i,i)

                do j=1,ParamRanges(i).NPartSubbasin
                    ParamRanges(i).AllSubbasins(ParamRanges(i).PartSubbasins(j))=1
                end do

                do k=1,NParamRangeCK
                    if(ParamRangeUpStreamAll(i,k).eq.1) then
                        do j=1,ParamRanges(k).NPartSubbasin
                            ParamRanges(i).AllSubbasins(ParamRanges(k).PartSubbasins(j))=1
                        enddo
                    end if
                end do
            END DO         
    end subroutine
    
    subroutine ParamRangesChars(nidx,NParamRangeCK,ParamRangeName,len1,UpStreamParamRangeStr,len2,PartSubbasinStr,len3)
        integer ::nidx,NParamRangeCK,len1,len2,len3,j,length1,length2,k,i
        character(len1)  ::ParamRangeName
        character(len2)  ::UpStreamParamRangeStr,UpStreamParamRangeStr1
        character(len3)  ::PartSubbasinStr,PartSubbasinStr1
        if(NParamRangeCK .gt. 0)then
            ParamRanges(nidx).ParamRangeName(1:len1)         = ParamRangeName 
!            if(ichar(UpStreamParamRangeStr(1:1)) /= 0)then            
!                call GetStrSplitterCount(UpStreamParamRangeStr,len2,length1,',')
!            else
!                length1 = 0
!            endif
!            if(ichar(PartSubbasinStr(1:1)) /= 0)then
!                call GetStrSplitterCount(PartSubbasinStr,len3,length2,',')
!            else
!                length2 = 0
!            endif
!   
!            read(UpStreamParamRangeStr,*)    (ParamRanges(nidx).UpStreamParamRanges(j),j=1,length1,1)
!            read(PartSubbasinStr,*)          (ParamRanges(nidx).PartSubbasins(j),j=1,length2,1)

           if(ParamRanges(nidx).NUpStreamParamRange /= 0)then            
               call GetStrSplitterCount(UpStreamParamRangeStr,len2,length1,',')
               if(allocated(istr)) deallocate(istr)
               allocate(istr(length1))
               read(UpStreamParamRangeStr,*) (istr(j),j=1,length1,1)
               do j = 1,ParamRanges(nidx).NUpStreamParamRange
                    ParamRanges(nidx).UpStreamParamRanges(j) = istr(j)  
               enddo
           endif
           if(ParamRanges(nidx).NPartSubbasin /= 0)then
               call GetStrSplitterCount(PartSubbasinStr,len3,length2,',')
               if(allocated(istr)) deallocate(istr)
               allocate(istr(length2))
               read(PartSubbasinStr,*) (istr(j),j=1,length2,1)
               do j = 1,ParamRanges(nidx).NPartSubbasin
                    ParamRanges(nidx).PartSubbasins(j) = istr(j)  
               enddo
           endif           

            do j = 1,ParamRanges(nidx).NPartSubbasin
                ParamRanges(nidx).Area = ParamRanges(nidx).Area + Reachs(ParamRanges(nidx).PartSubbasins(j)).Area
            enddo
            ParamRanges(nidx).UpArea = ParamRanges(nidx).Area
            if(ParamRanges(nidx).NUpStreamParamRange > 0) then

                do j = 1,ParamRanges(nidx).NUpStreamParamRange
                    ParamRanges(nidx).UpArea = ParamRanges(nidx).UpArea + ParamRanges(ParamRanges(nidx).UpStreamParamRanges(j)).UpArea
                enddo

            endif

        endif
    end subroutine
    
    subroutine InitParamRangesVars(NParamRangeCKVar,ParamRangesVars) 
        integer ::NParamRangeCKVar
        integer :: i,j,k
        INTEGER :: LENStr = 28
        integer::Length1,length2,length3
        real,dimension(6,NParamRangeCKVar) ::ParamRangesVars
        
        NParamRangeCK = NParamRangeCKVar
        LENStr=NParamRangeCK 
        if (NParamRangeCK .ne. WaterShed.NParamRange) then
            !write(*,*) "参数分区数出现异常，按Enter继续"
        endif
        
                !分配空间并赋初值
        if(allocated(ParamRanges))deallocate(ParamRanges)
        allocate(ParamRanges(NParamRangeCK))
        if(allocated(m_ParamRange.UpStreamParamRanges))deallocate(m_ParamRange.UpStreamParamRanges)
        allocate(m_ParamRange.UpStreamParamRanges(NParamRangeCK))
        m_ParamRange.UpStreamParamRanges=0
        if(allocated(m_ParamRange.PartSubbasins))deallocate(m_ParamRange.PartSubbasins)
        allocate(m_ParamRange.PartSubbasins(WaterShed.NSubbasin))                  
        m_ParamRange.PartSubbasins=0
        if(allocated(m_ParamRange.AllSubbasins))deallocate(m_ParamRange.AllSubbasins)
        allocate(m_ParamRange.AllSubbasins(WaterShed.NSubbasin))
        m_ParamRange.AllSubbasins=0
        if(allocated(ParamRangeUpStreamAll))deallocate(ParamRangeUpStreamAll)
        allocate(ParamRangeUpStreamAll(NParamRangeCK,NParamRangeCK))
        ParamRangeUpStreamAll=0        
                        
        do i=1,NParamRangeCK,1
            if(allocated(ParamRanges(i).UpStreamParamRanges))deallocate(ParamRanges(i).UpStreamParamRanges)
            allocate(ParamRanges(i).UpStreamParamRanges(NParamRangeCK))
            ParamRanges(i).UpStreamParamRanges=0
            if(allocated(ParamRanges(i).PartSubbasins))deallocate(ParamRanges(i).PartSubbasins)
            allocate(ParamRanges(i).PartSubbasins(WaterShed.NSubbasin))
            ParamRanges(i).PartSubbasins=0
            if(allocated(paramRanges(i).AllSubbasins))deallocate(paramRanges(i).AllSubbasins)
            allocate(paramRanges(i).AllSubbasins(WaterShed.NSubbasin))
            paramRanges(i).AllSubbasins=0
        end do
        
        do i=1,NParamRangeCK,1
            m_ParamRange.pid                    = ParamRangesVars(1,i)
            m_ParamRange.HydroId                = ParamRangesVars(2,i)
            m_ParamRange.SubId                  = ParamRangesVars(3,i)
            m_ParamRange.NUpStreamParamRange    = ParamRangesVars(4,i)
            m_ParamRange.NPartSubbasin          = ParamRangesVars(5,i)
            m_ParamRange.Itype                  = ParamRangesVars(6,i)
            if(m_ParamRange.Itype == 2) m_ParamRange.Itype=1
            ParamRanges(i)=m_ParamRange
            ParamRanges(i).Area = 0.
        end do 
    end subroutine
    
    subroutine InitSoilInfoVars(NResCKVar,SoilInfoVars)!dcb InitSoilInfo 
        integer ::NResCKVar,i
        real,dimension(5,NResCKVar) ::SoilInfoVars
        NResCK = NResCKVar
        if(allocated(Soils))deallocate(Soils)
        allocate(Soils(Watershed.NSubbasin))
        do i = 1, NResCK
            m_soil.SubId    = SoilInfoVars(1,i)
            m_soil.dep_imp  = SoilInfoVars(2,i)
            m_soil.ddrain   = SoilInfoVars(3,i)           
            m_soil.sol_crk  = SoilInfoVars(4,i)
            m_soil.solh     = SoilInfoVars(5,i)     
            m_soil.solh0    = SoilInfoVars(5,i)
            soils(i)=m_soil
        end do 
    end subroutine
    
    subroutine iniYearTavgVars(nCount,m_y_WeatherVars)!dcb  m_y_Weather  
        real,dimension(nCount) ::m_y_WeatherVars
        integer ::i,nCount
        
       !分配内存空间
        if(allocated(y_Weather))deallocate(y_Weather)
        allocate(y_Weather(WaterShed.NSubbasin)) 
        
        do i = 1, nCount
            m_y_Weather.Tavg = m_y_WeatherVars(i)
            y_Weather(i) = m_y_Weather   
        end do         
    end subroutine
    
    subroutine ReadinEasyDHMParamChars(nidx,ncount,SolzcoeStr,len1,ConducMStr,len2)
        character(len1)  ::SolzcoeStr,SolzcoeStr1
        character(len2)  ::ConducMStr,ConducMStr1
        integer          ::nidx,ncount,len1,len2,length1,length2,j
        if(allocated(EasyDHMParams))then
            !处理读入字符串
            call GetStrSplitterCount(SolzcoeStr,len1,length1,',')
            call GetStrSplittercount(ConducMStr,len2,length2,',')
            !将字符串中的值传给相应数组
            
            if(allocated(rstr)) deallocate(rstr)
            allocate(rstr(length1))
            read(SolzcoeStr,*) (rstr(j),j=1,length1,1)
            do j = 1,WaterShed%NLayer  - 1
                EasyDHMParams(nidx)%Solzcoe(j) = rstr(j)  
            enddo
            if(allocated(rstr)) deallocate(rstr)
            allocate(rstr(length2))
            read(ConducMStr,*) (rstr(j),j=1,length2,1)
            do j = 1,WaterShed%NLayer
                EasyDHMParams(nidx)%ConductM(j)  = rstr(j)  
            enddo
        endif
    endsubroutine
    
    subroutine ReadinEasyDHMParamVars(NReadEasyDHMParamCKVar,ReadEasyDHMParamVars) !dcb ReadEasyDHMParam
        integer::i,j,length1,length2,NReadEasyDHMParamCKVar
        real,dimension(35,NReadEasyDHMParamCKVar) ::ReadEasyDHMParamVars
        
        if(allocated(EasyDHMParams))deallocate(EasyDHMParams)
        allocate(EasyDHMParams(WaterShed%NParamRange))
        do i=1,WaterShed%NParamRange
            if(allocated(EasyDHMParams(i)%SolzCoe))deallocate(EasyDHMParams(i)%SolzCoe)
            allocate(EasyDHMParams(i)%SolzCoe(WaterShed%NLayer-1))
            if(allocated(EasyDHMParams(i)%ConductM))deallocate(EasyDHMParams(i)%ConductM)
            allocate(EasyDHMParams(i)%ConductM(WaterShed%NLayer))
        end do
        if(allocated(m_EasyDHMParam%SolzCoe))deallocate(m_EasyDHMParam%SolzCoe)
        allocate(m_EasyDHMParam%SolzCoe(WaterShed%NLayer-1))
        if(allocated(m_EasyDHMParam%ConductM))deallocate(m_EasyDHMParam%ConductM)
        allocate(m_EasyDHMParam%ConductM(WaterShed%NLayer))
        
        do i = 1,NReadEasyDHMParamCKVar
            m_EasyDHMParam%pid             = ReadEasyDHMParamVars(1,i)
            m_EasyDHMParam%G0              = ReadEasyDHMParamVars(2,i)
            m_EasyDHMParam%Gwht0           = ReadEasyDHMParamVars(3,i)
            m_EasyDHMParam%Ch_revap        = ReadEasyDHMParamVars(4,i)
            m_EasyDHMParam%Agw_delay       = ReadEasyDHMParamVars(5,i)
            m_EasyDHMParam%Alpha_bf        = ReadEasyDHMParamVars(6,i)
            m_EasyDHMParam%Agwqmn          = ReadEasyDHMParamVars(7,i)
            m_EasyDHMParam%Archrg_dp       = ReadEasyDHMParamVars(8,i)
            m_EasyDHMParam%Arevapmn        = ReadEasyDHMParamVars(9,i)
            m_EasyDHMParam%Gw_spyld        = ReadEasyDHMParamVars(10,i)
            m_EasyDHMParam%Cn2(1)          = ReadEasyDHMParamVars(11,i)
            m_EasyDHMParam%P_max           = ReadEasyDHMParamVars(12,i)
            m_EasyDHMParam%ImpM            = ReadEasyDHMParamVars(13,i)
            m_EasyDHMParam%UnitSlopeM      = ReadEasyDHMParamVars(14,i)
            m_EasyDHMParam%RunoffCoeM      = ReadEasyDHMParamVars(15,i)
            m_EasyDHMParam%PorosityM       = ReadEasyDHMParamVars(16,i)
            m_EasyDHMParam%FieldCapM       = ReadEasyDHMParamVars(17,i)
            m_EasyDHMParam%LaiMaxM(1)      = ReadEasyDHMParamVars(18,i)
            m_EasyDHMParam%RtDpthM(1)      = ReadEasyDHMParamVars(19,i)
            m_EasyDHMParam%DepressM        = ReadEasyDHMParamVars(20,i)
            m_EasyDHMParam%ItcMaxM(1)      = ReadEasyDHMParamVars(21,i)
            m_EasyDHMParam%SFtmp           = ReadEasyDHMParamVars(22,i)
            m_EasyDHMParam%SmTmp           = ReadEasyDHMParamVars(23,i)
            m_EasyDHMParam%SmFmx           = ReadEasyDHMParamVars(24,i)
            m_EasyDHMParam%SmFmn           = ReadEasyDHMParamVars(25,i)
            m_EasyDHMParam%Timp            = ReadEasyDHMParamVars(26,i)
            m_EasyDHMParam%Snocovmx        = ReadEasyDHMParamVars(27,i)
            m_EasyDHMParam%Sno50cov        = ReadEasyDHMParamVars(28,i)
            m_EasyDHMParam%Dep_impM        = ReadEasyDHMParamVars(29,i)
            m_EasyDHMParam%DdrainM         = ReadEasyDHMParamVars(30,i)
            m_EasyDHMParam%Atdrain         = ReadEasyDHMParamVars(31,i)
            m_EasyDHMParam%Sol_crkM        = ReadEasyDHMParamVars(32,i)
            m_EasyDHMParam%Solf            = ReadEasyDHMParamVars(33,i)
            m_EasyDHMParam%Solfm           = ReadEasyDHMParamVars(34,i)
            m_EasyDHMParam%Petm            = ReadEasyDHMParamVars(35,i)
!            if(Nlanduse==6)then
                do j=2,6
                    m_EasyDHMParam%Cn2(j)=m_EasyDHMParam%Cn2(1)
                    m_EasyDHMParam%LaiMaxM(j) = m_EasyDHMParam%LaiMaxM(1)
                    m_EasyDHMParam%RtDpthM(j) = m_EasyDHMParam%RtDpthM(1)
                    m_EasyDHMParam%ItcMaxM(j) = m_EasyDHMParam%ItcMaxM(1)
                enddo
!            endif

            EasyDHMParams(i)=m_EasyDHMParam
        end do 
    end subroutine
    
!    subroutine ReadinWetSpaParam(nCount,ReadWetSpaParamVars)!dcb ReadWetSpaParam
!        integer::i,nCount
!        real,dimension(30,nCount) ::ReadWetSpaParamVars            
!        !用WaterShed中的参数分区个数分配WetSpaParams的大小
!        if(allocated(WetSpaParams))deallocate(WetSpaParams)
!        allocate(WetSpaParams(WaterShed.NParamRange))
!        
!        do i = 1,nCount
!            m_WetSpaParam.pid          = ReadWetSpaParamVars(1,i)
!            m_WetSpaParam.G0           = ReadWetSpaParamVars(2,i)
!            m_WetSpaParam.G_max        = ReadWetSpaParamVars(3,i)
!            m_WetSpaParam.K_ss0        = ReadWetSpaParamVars(4,i)
!            m_WetSpaParam.Gwht0        = ReadWetSpaParamVars(5,i)
!            m_WetSpaParam.Agw_delay    = ReadWetSpaParamVars(6,i)
!            m_WetSpaParam.Alpha_bf     = ReadWetSpaParamVars(7,i)
!            m_WetSpaParam.Agwqmn       = ReadWetSpaParamVars(8,i)
!            m_WetSpaParam.Archrg_dp    = ReadWetSpaParamVars(9,i)
!            m_WetSpaParam.Arevapmn     = ReadWetSpaParamVars(10,i)
!            m_WetSpaParam.Gw_spyld     = ReadWetSpaParamVars(11,i)
!            m_WetSpaParam.Petm         = ReadWetSpaParamVars(12,i)
!            m_WetSpaParam.Kg           = ReadWetSpaParamVars(13,i)
!            m_WetSpaParam.Ki           = ReadWetSpaParamVars(14,i)
!            m_WetSpaParam.T0           = ReadWetSpaParamVars(15,i)
!            m_WetSpaParam.K_snow       = ReadWetSpaParamVars(16,i)
!            m_WetSpaParam.K_rain       = ReadWetSpaParamVars(17,i)
!            m_WetSpaParam.K_run        = ReadWetSpaParamVars(18,i)
!            m_WetSpaParam.P_max        = ReadWetSpaParamVars(19,i)
!            m_WetSpaParam.ImpM         = ReadWetSpaParamVars(20,i)
!            m_WetSpaParam.UnitSlopeM   = ReadWetSpaParamVars(21,i)
!            m_WetSpaParam.RunoffCoeM   = ReadWetSpaParamVars(22,i)
!            m_WetSpaParam.ConductM     = ReadWetSpaParamVars(23,i)
!            m_WetSpaParam.PorosityM    = ReadWetSpaParamVars(24,i)
!            m_WetSpaParam.FieldCapM    = ReadWetSpaParamVars(25,i)
!            m_WetSpaParam.PoreIndexM   = ReadWetSpaParamVars(26,i)
!            m_WetSpaParam.LaimaxM      = ReadWetSpaParamVars(27,i)
!            m_WetSpaParam.DepressM     = ReadWetSpaParamVars(28,i)
!            m_WetSpaParam.RootDpthM    = ReadWetSpaParamVars(29,i)
!            m_WetSpaParam.ItcmaxM      = ReadWetSpaParamVars(30,i)
!            WetSpaParams(i)            = m_WetSpaParam  
!        end do 
!    end subroutine
    
!    subroutine ReadinXAJParam(nCount,ReadXAJParamVars)!dcb ReadXAJParam
!        integer::i,nCount
!        real,dimension(14,nCount) ::ReadXAJParamVars 
!        !用WaterShed中的参数分区个数分配XAJParams的大小
!        if(allocated(XAJParams))deallocate(XAJParams)
!        allocate(XAJParams(WaterShed.NParamRange))  
!        
!        do i = 1 ,nCount
!            m_XAJParam.pid   = ReadXAJParamVars(1,i) 
!            m_XAJParam.Petm  = ReadXAJParamVars(2,i) 
!            m_XAJParam.C     = ReadXAJParamVars(3,i) 
!            m_XAJParam.IMP   = ReadXAJParamVars(4,i) 
!            m_XAJParam.WM1   = ReadXAJParamVars(5,i) 
!            m_XAJParam.WM2   = ReadXAJParamVars(6,i) 
!            m_XAJParam.WM3   = ReadXAJParamVars(7,i) 
!            m_XAJParam.B     = ReadXAJParamVars(8,i) 
!            m_XAJParam.SM    = ReadXAJParamVars(9,i) 
!            m_XAJParam.EX    = ReadXAJParamVars(10,i) 
!            m_XAJParam.KG    = ReadXAJParamVars(11,i) 
!            m_XAJParam.KSS   = ReadXAJParamVars(12,i) 
!            m_XAJParam.KKG   = ReadXAJParamVars(13,i) 
!            m_XAJParam.KKSS  = ReadXAJParamVars(14,i) 
!            XAJParams(i)     = m_XAJParam
!        end do  
!    end subroutine
    
!    subroutine ReadinHymodParam(NParamRangeCheckVar,ReadHymodParamVars)!dcb NParamRangeCheck !dcb ReadHymodParam
!        integer ::NParamRangeCheckVar
!        integer(KIND=4) :: i
!        real,dimension(6,NParamRangeCheckVar) ::ReadHymodParamVars 
!        
!        NParamRangeChecK = NParamRangeChecKVar
!        if (NParamRangeChecK .ne. WaterShed.NParamRange) then
!            !write(*,*) "参数分区数出现异常，按Enter继续"
!        endif
!        if(allocated(HymodParams))deallocate(HymodParams)
!        allocate(HymodParams(NParamRangeCheck))
!        
!        do i = 1, NParamRangeCheckVar
!            m_HymodPara.PETM    = ReadHymodParamVars(1,i)
!            m_HymodPara.cmax    = ReadHymodParamVars(2,i)
!            m_HymodPara.bexp    = ReadHymodParamVars(3,i)
!            m_HymodPara.alpha   = ReadHymodParamVars(4,i)
!            m_HymodPara.rqq     = ReadHymodParamVars(5,i)
!            m_HymodPara.rss     = ReadHymodParamVars(6,i)
!            HymodParams(i) = m_Hymodpara
!        end do 
!    end subroutine
    
    subroutine ReadinReachParam(NParamRangeCheckVar,ReadReachParamVars) !dcb NParamRangeCheck  !dcb ReadReachParam  
        integer(KIND=4) :: i
        integer ::NParamRangeCheckVar
        real,dimension(4,NParamRangeCheckVar) ::ReadReachParamVars
        
        NReachParamRangeCheck = NParamRangeCheckVar
        if (NReachParamRangeCheck .ne. WaterShed.NParamRange) then
            !write(*,*) "参数分区数出现异常，按Enter继续"
        endif
        if(allocated(ReachParams))deallocate(ReachParams)
        allocate(ReachParams(NReachParamRangeCheck))
        do i = 1, NParamRangeCheckVar
            m_ReachPara.CH_S2M  = ReadReachParamVars(1,i)
            m_ReachPara.CH_L2M  = ReadReachParamVars(2,i) 
            m_ReachPara.CH_N2M  = ReadReachParamVars(3,i)
            m_ReachPara.CH_K2M  = ReadReachParamVars(4,i)
            ReachParams(i) = m_Reachpara
        end do 
    end subroutine 
    
    subroutine ReadinInitDHMStates(nCount,ReadInitDHMStatesVars)!dcb ReadInitDHMStates
        integer::i,j,nCount        
        real,dimension(11,nCount) ::ReadInitDHMStatesVars                 
        !用WaterShed中的参数分区个数分配EasyDHMStates的大小
        if(allocated(InitDHMStates))deallocate(InitDHMStates)
        allocate(InitDHMStates(WaterShed%NSubbasin,WaterShed.NUnit))
        
        do i = 1, nCount
            m_InitDHMStates%SubId           = ReadInitDHMStatesVars(1,i)
            m_InitDHMStates%UnitID          = ReadInitDHMStatesVars(2,i)
            m_InitDHMStates%init_flwout     = ReadInitDHMStatesVars(3,i)
            m_InitDHMStates%init_rchstor    = ReadInitDHMStatesVars(4,i)
            m_InitDHMStates%init_rechg      = ReadInitDHMStatesVars(5,i)
            m_InitDHMStates%init_rg         = ReadInitDHMStatesVars(6,i)
            m_InitDHMStates%init_snow       = ReadInitDHMStatesVars(7,i)
            m_InitDHMStates%init_sint       = ReadInitDHMStatesVars(8,i)
            m_InitDHMStates%init_sdep       = ReadInitDHMStatesVars(9,i)
            m_InitDHMStates%init_gt         = ReadInitDHMStatesVars(10,i)
            m_InitDHMStates%init_ss         = ReadInitDHMStatesVars(11,i)
            
            InitDHMStates(m_InitDHMStates.SubID,m_InitDHMStates.UnitID) = m_InitDHMStates
            
        end do 
    end subroutine
    
!    subroutine ReadinInitWSPStates(nCount,ReadInitWSPStatesVars)!dcb ReadInitWSPStates
!        integer::i,nCount   
!        real,dimension(10,nCount) ::ReadInitWSPStatesVars                
!        !用WaterShed中的参数分区个数分配WSPStates的大小
!        if(allocated(InitWSPStates))deallocate(InitWSPStates)
!        allocate(InitWSPStates(WaterShed%NSubbasin,WaterShed.NUnit))
!        
!        do i = 1, nCount
!            m_InitWSPStates%SubId          = ReadInitWSPStatesVars(1,i)
!            m_InitWSPStates%UnitID         = ReadInitWSPStatesVars(2,i)
!            m_InitWSPStates%init_flwout    = ReadInitWSPStatesVars(3,i)
!            m_InitWSPStates%init_rchstor   = ReadInitWSPStatesVars(5,i)
!            m_InitWSPStates%init_rg        = ReadInitWSPStatesVars(5,i)
!            m_InitWSPStates%init_snow      = ReadInitWSPStatesVars(6,i)
!            m_InitWSPStates%init_sint      = ReadInitWSPStatesVars(7,i)
!            m_InitWSPStates%init_sdep      = ReadInitWSPStatesVars(8,i)
!            m_InitWSPStates%init_gt        = ReadInitWSPStatesVars(9,i)
!            m_InitWSPStates%init_st        = ReadInitWSPStatesVars(10,i)
!            InitWSPStates(m_InitWSPStates.SubID,m_InitWSPStates.UnitID)=m_InitWSPStates
!        end do 
!    end subroutine
!    
!    subroutine ReadinInitXAJStates(nCount,ReadInitXAJStatesVars) !dcb ReadInitXAJStates
!        integer::i,nCount        
!        real,dimension(10,nCount) ::ReadInitXAJStatesVars                
!        !用WaterShed中的参数分区个数分配XAJStates的大小
!        if(allocated(InitXAJStates))deallocate(InitXAJStates)
!        allocate(InitXAJStates(WaterShed%NSubbasin,WaterShed.NUnit))
!        
!        do i = 1,nCount
!            m_InitXAJStates%SubId          = ReadInitXAJStatesVars(1,i)
!            m_InitXAJStates%UnitID         = ReadInitXAJStatesVars(2,i)
!            m_InitXAJStates%init_flwout    = ReadInitXAJStatesVars(3,i)
!            m_InitXAJStates%init_rchstor   = ReadInitXAJStatesVars(4,i)
!            m_InitXAJStates%init_WU        = ReadInitXAJStatesVars(5,i)
!            m_InitXAJStates%init_WL        = ReadInitXAJStatesVars(6,i)
!            m_InitXAJStates%init_WD        = ReadInitXAJStatesVars(7,i)
!            m_InitXAJStates%init_S         = ReadInitXAJStatesVars(8,i)
!            m_InitXAJStates%init_Qi        = ReadInitXAJStatesVars(9,i)
!            m_InitXAJStates%init_Qg        = ReadInitXAJStatesVars(10,i)
!            InitXAJStates(m_InitXAJStates.SubID,m_InitXAJStates.UnitID)=m_InitXAJStates
!        end do 
!    end subroutine
    
!    subroutine ReadinInitHymodStates(nCount,ReadInitHymodStatesVars) !dcb ReadInitHymodStates
!        integer::i,nCount
!        real,dimension(9,nCount) ::ReadInitHymodStatesVars
!                        
!        !用WaterShed中的参数分区个数分配HymodStates的大小
!        if(allocated(InitHymodStates))deallocate(InitHymodStates)
!        allocate(InitHymodStates(WaterShed%NSubbasin,WaterShed.NUnit))
!        
!        do i = 1,nCount
!            m_InitHymodStates%SubId                = ReadInitHymodStatesVars(1,i)
!            m_InitHymodStates%UnitID               = ReadInitHymodStatesVars(2,i)
!            m_InitHymodStates%init_flwout          = ReadInitHymodStatesVars(3,i)
!            m_InitHymodStates%init_rchstor         = ReadInitHymodStatesVars(4,i)
!            m_InitHymodStates%init_WaterStorage    = ReadInitHymodStatesVars(5,i)
!            m_InitHymodStates%init_slow            = ReadInitHymodStatesVars(6,i)
!            m_InitHymodStates%init_quick1          = ReadInitHymodStatesVars(7,i)
!            m_InitHymodStates%init_quick2          = ReadInitHymodStatesVars(8,i)
!            m_InitHymodStates%init_quick3          = ReadInitHymodStatesVars(9,i)
!            InitHymodStates(m_InitHymodStates.SubID,m_InitHymodStates.UnitID)=m_InitHymodStates
!        end do 
!    end subroutine
    
    subroutine GetUpStreamParamRangeDataEx(uprsvVars,rsv,upriverVars,river,uprivernpid,upresnpid,npid,ncount)
        integer ::j,i,rsvidx,riveridx,k,ipid,npid,ncount,uprivernpid,upresnpid
        integer ::rsv(upresnpid),river(uprivernpid)
!        real,dimension(nCount,upresnpid)::uprsvVars
!        real,dimension(nCount,uprivernpid)::upriverVars
        real,dimension(upresnpid,nCount)::uprsvVars
        real,dimension(uprivernpid,nCount)::upriverVars

        rsvidx = 1
        riveridx = 1

        
        do ipid=1,npid
            do i = 1,ParamRanges(totalSolution(ipid)%IParamRange)%NUpStreamParamRange
                
                do j=1,uprivernpid
                    if(ParamRanges(totalSolution(ipid)%IParamRange)%UpstreamParamRanges(i)==river(j))then
                       if(ParamRanges(ParamRanges(totalSolution(ipid)%IParamRange)%UpstreamParamRanges(i))%Itype .eq. 1)then !水文 
                             do k = 1,nCount
!                                TotalUpHydroFinal(totalSolution(ipid)%IParamRange)%UpHydroFinal(i).UpQFinal(k) = upriverVars(k,j)
                                TotalUpHydroFinal(totalSolution(ipid)%IParamRange)%UpHydroFinal(i).UpQFinal(k) = upriverVars(j,k)
                             enddo
                             riveridx = riveridx +1 
                        endif                                      
                       
                    endif
                enddo
                do j=1,upresnpid
                    if(ParamRanges(totalSolution(ipid)%IParamRange)%UpstreamParamRanges(i)==rsv(j))then
                        if(ParamRanges(ParamRanges(totalSolution(ipid)%IParamRange)%UpstreamParamRanges(i))%Itype .eq. 0)then ! 水库
                             do k = 1,nCount
!                                ResOutObserveds(i)%ResOutObserved(ParamRanges(totalSolution(ipid).IParamRange).HydroId) =   uprsvVars(k,j)
                                ResOutObserveds(i)%ResOutObserved(ParamRanges(totalSolution(ipid).IParamRange).HydroId) =   uprsvVars(j,k)
                                TotalUpHydroFinal(totalSolution(ipid)%IParamRange)%UpHydroFinal(i).UpQFinal(k) = uprsvVars(j,k)
                             enddo
                             rsvidx = rsvidx +1      
                        endif
                    endif
                end do
            enddo
     enddo
    end subroutine
    
    subroutine HydroDataDateTime(nidx,nCount,strDateTime,floodvar,floodpidvar,npid)
!        character*30,dimension(nCount) ::strDateTime
        character*30 ::strDateTime

        character(30) ::datatime1
        integer npid,datatime1int,datastarttimeint,dataendtimeint
        integer       ::nidx,nCount,iflood,ipid,floodpidvar(npid,2),floodvar(npid)
        
        if(allocated(HydroObserveds))then
            call deletedot(strDateTime,30,datatime1)
            HydroObserveds(nidx).DateTime = datatime1
            call ChangeStringInt(datatime1,len(datatime1),datatime1int)

             do ipid=1,npid
                 if(floodpidvar(ipid,2)>0)then
                    do iflood=1,floodpidvar(ipid,2)
                        call ChangeStringInt(totalSolution(ipid)%StartDateTime(iflood),len(totalSolution(ipid)%StartDateTime(iflood)),datastarttimeint)
                        call ChangeStringInt(totalSolution(ipid)%EndDateTime(iflood),len(totalSolution(ipid)%EndDateTime(iflood)),dataendtimeint)
                        if(datatime1int>2010081900)then
                            continue
                        endif
                        if(datatime1int>= datastarttimeint.and.datatime1int<=dataendtimeint)then
                            totalSolution(ipid)%NFloodSeries(iflood)=totalSolution(ipid)%NFloodSeries(iflood)+1
                            if(totalSolution(ipid)%NFloodSeries(iflood)==1) totalSolution(ipid)%Floodloc(iflood)=nidx
                        endif
                    enddo
                endif
            enddo          
        endif

    endsubroutine

    subroutine InitWeatherInfosVars(npid,nWeatherCount0,nWeatherCount1,weatherWeight0,weatherWeight1,sum_NSubbasin,nWeatherCount0max,nWeatherCount1max,nyear)!dcb nWeatherCount0,1 !dcb InitWeatherInfos
        integer::i,j,nNum,npid,sum_NSubbasin,nWeatherCount0max,nWeatherCount1max,ipid,nyear,iyear,sum,nsubbasin,iipid
        integer::nWeatherCount0(npid,2),nWeatherCount1(npid,2)

        real,dimension(nWeatherCount0max+1,sum_NSubbasin) ::weatherWeight0
        real,dimension(nWeatherCount1max,sum_NSubbasin) ::weatherWeight1     
        real weightmax,weightsum
        integer Imax
          
        if(allocated(totalWeatherInfos)) deallocate(totalWeatherInfos)
        allocate (totalWeatherInfos(WaterShed.NParamRange))

        nsubbasin=sum_NSubbasin/nyear
        do ipid=1,npid
            if(allocated(totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos)) deallocate(totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos)
            allocate (totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(ParamRanges(totalSolution(ipid)%IParamRange)%NPartSubbasin))

            do j = 1,ParamRanges(totalSolution(ipid)%IParamRange)%NPartSubbasin
                totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%nstcd0 = nWeatherCount0(ipid,2)       
                if(allocated(totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight0))deallocate(totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight0)
                allocate(totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight0(nWeatherCount0(ipid,2),nyear)) 
                totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%nstcd1 = nWeatherCount1(ipid,2)
                if(allocated(totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1))deallocate(totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1)
                allocate(totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(nWeatherCount1(ipid,2),nyear)) 
                
                if(ipid==1)then
                    do i = 1 , totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%nstcd0
                        if(nyear==1)then
                            totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight0(i,nyear) = weatherWeight0(i+1,j)
                        elseif(nyear>1)then
                            do iyear=1,nyear
                                totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight0(i,iyear) = weatherWeight0(i+1,j+(iyear-1)*nsubbasin)
                            enddo
                        endif
                    enddo
                
                    do i = 1,  totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%nstcd1
                        if(nyear==1)then
                            totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(i,nyear) = weatherWeight1(i,j)
                        elseif(nyear>1)then
                            do iyear=1,nyear
                                totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(i,iyear) = weatherWeight1(i,j+(iyear-1)*nsubbasin)
                            enddo
                        endif
                    end do 
                elseif(ipid>1)then
                    sum=0
                    do iipid=1,ipid-1
                        sum=sum+ParamRanges(totalSolution(iipid)%IParamRange)%NPartSubbasin
                    enddo
                    weightsum = 0
                    weightmax = 0
                    do i = 1 , totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%nstcd0

!                        totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight0(i) = weatherWeight0(i,ParamRanges(totalSolution(ipid-1)%IParamRange)%NPartSubbasin+j)
                        if(nyear==1)then
                            totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight0(i,nyear) = weatherWeight0(i+1,sum+j)
                            weightsum = weightsum + weatherWeight0(i+1,sum+j)
                            if(weightmax< weatherWeight0(i+1,sum+j)) then
                                weightmax = weatherWeight0(i+1,sum+j)
                                IMax = i
                            endif
                        elseif(nyear>1)then
                            do iyear=1,nyear
                                totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight0(i,iyear) = weatherWeight0(i+1,sum+j+(iyear-1)*nsubbasin)
                            enddo
                            weightsum = weightsum + weatherWeight0(i+1,sum+j+(iyear-1)*nsubbasin)
                            if(weightmax< weatherWeight0(i+1,sum+j+(iyear-1)*nsubbasin))   then
                                weightmax = weatherWeight0(i+1,sum+j+(iyear-1)*nsubbasin)
                                IMax = i
                            endif
                        endif
                    enddo
                    if(weightsum /= 1.0) then
                        if(nyear==1)then
                            totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight0(imax,nyear) =totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight0(imax,nyear)  - (weightsum - 1)
                            
                        elseif(nyear>1)then
                            do iyear=1,nyear
                                totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight0(imax,iyear) = totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight0(imax,iyear)  - (weightsum - 1)
                            enddo
                            
                            
                        endif

                    endif
                                        
                    do i = 1,  totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%nstcd1
!                        totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(i) = weatherWeight1(i,ParamRanges(totalSolution(ipid-1)%IParamRange)%NPartSubbasin+j)
                        if(nyear==1)then
                            totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(i,nyear) = weatherWeight1(i,sum+j)
                        elseif(nyear>1)then
                            do iyear=1,nyear
                                totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(i,iyear) = weatherWeight1(i,sum+j+(iyear-1)*nsubbasin)
                            enddo
                        endif
                    end do 
                endif
            enddo
        enddo
    end subroutine 
    
    subroutine GetHydroDataEx(hydronCount,hydropid,npid,resnpid,res,fQ,fq1,weatherdatacount,raincount,nWeatherCount1max,nWeatherCount0max,fhmdt,fWsws,fIslr,fTavg,fTmaxt,fTmin,fPPtn,nyear,updrp)!dcb 水文站时段记数
        use AutoTimes
        use WeatherInfoMod
        use WeatherMod
        implicit none
        integer ::hydronCount,i,ii,subid,j,ipid,firstpidcount,npid,resnpid,weatherdatacount,nWeatherCount1max,nWeatherCount0max,raincount
        integer hydropid(npid),res(resnpid),counteveyear,nyear,counteveyearweather,itemp
        integer iupres,iuphydro
        integer dt
        real fQ(npid*hydroncount),fq1(resnpid*hydroncount),updrp(npid*hydroncount)
!        real ::fhmdt(npid*weatherdatacount,nWeatherCount1max),fWsws(npid*weatherdatacount,nWeatherCount1max),fIslr(npid*weatherdatacount,nWeatherCount1max),fTavg(npid*weatherdatacount,nWeatherCount1max),fTmaxt(npid*weatherdatacount,nWeatherCount1max),fTmin(npid*weatherdatacount,nWeatherCount1max),fPPtn(npid*weatherdatacount,nWeatherCount0max)
        real ::fhmdt(nWeatherCount1max,npid*weatherdatacount),fWsws(nWeatherCount1max,npid*weatherdatacount),fIslr(nWeatherCount1max,npid*weatherdatacount),fTavg(nWeatherCount1max,npid*weatherdatacount),fTmaxt(nWeatherCount1max,npid*weatherdatacount),fTmin(nWeatherCount1max,npid*weatherdatacount),fPPtn(nWeatherCount0max,npid*raincount)
!        real,dimension(:,:),allocatable :: fPPtn
!       
!        if(totalSolution(1).Iweather == 1) then
!            allocate(fPPtn(nWeatherCount0max,npid*raincount))
!        else
!            allocate(fPPtn(ParamRanges(totalSolution(1)%IParamRange)%NPartSubbasin,npid*raincount))
!        endif
!        firstpidcount=nCount
        if(allocated(HydroObserveds))deallocate(HydroObserveds)
        allocate(HydroObserveds(hydronCount))
        
        if(allocated(ResOutObserveds))deallocate(ResOutObserveds)
        allocate(ResOutObserveds(hydronCount))                
        if(allocated(TotalUpHydroFinal))deallocate(TotalUpHydroFinal)
        allocate(TotalUpHydroFinal(WaterShed.NParamRange))
        do i=1,hydronCount                
            if(allocated(HydroObserveds(i)%QObserved))deallocate(HydroObserveds(i)%QObserved)
            allocate(HydroObserveds(i)%QObserved(WaterShed.NParamRange))
            if(allocated(ResOutObserveds(i)%ResOutObserved))deallocate(ResOutObserveds(i)%ResOutObserved)
            allocate(ResOutObserveds(i)%ResOutObserved(WaterShed.NParamRange))                    
        enddo
!        dt = totalSolution(1)%dtold(1)
        if(allocated(Upallrain))deallocate(Upallrain)
        allocate(Upallrain(npid,hydronCount))                
        
        NAllSeries = hydronCount !一个参数分区全部的数
        nProcessCount = NAllSeries
        iupres = 0
        iuphydro = 0

        if(nyear>1)then
            counteveyear= hydroncount/nyear
        elseif(nyear==1)then
            counteveyear=hydroncount
        endif
        if (totalSolution(1).runtype == 2 .or. totalSolution(1).runtype == 3 ) then
            NAnalySeries = hydronCount - totalSolution(1).NTimeStepInit
            if(allocated(TotalAnalyRunoff))deallocate(TotalAnalyRunoff)
            allocate(TotalAnalyRunoff(NAllSeries,2,WaterShed.NParamRange))
            if(allocated(AnalyRunoff))deallocate(AnalyRunoff)
            allocate(AnalyRunoff(NAllSeries,2))
            
        elseif(totalSolution(1).runtype == 8.or.totalSolution(1).runtype ==31)then
            NAnalySeries = hydroncount - totalSolution(1).NTimeStepInit
            if(allocated(TotalAnalyRunoff))deallocate(TotalAnalyRunoff)
            allocate(TotalAnalyRunoff(NAllSeries,2,WaterShed.NParamRange))
            if(allocated(AnalyRunoff))deallocate(AnalyRunoff)
            allocate(AnalyRunoff(NAllSeries,2))
            if (totalSolution(1).Rtmdy .ne. 0) then
                if(allocated(TotalAdjustRunoff))deallocate(TotalAdjustRunoff)
                allocate(TotalAdjustRunoff(NAllSeries,3,WaterShed.NParamRange))
                if(allocated(AdjustRunoff))deallocate(AdjustRunoff)
                allocate(AdjustRunoff(NAllSeries,4))
            endif
            if(totalSolution(1).NRunoffGenType > 0) then
                if(allocated(TotalAllRunoff))deallocate(TotalAllRunoff)
                allocate(TotalAllRunoff(NAllSeries,5,WaterShed.NParamRange))
                if(allocated(AllRunoff))deallocate(AllRunoff)
                allocate(AllRunoff(NAllSeries,5))
            endif
        else
            if (totalSolution(1).Rtmdy .ne. 0) then
                if(allocated(TotalAdjustRunoff))deallocate(TotalAdjustRunoff)
                allocate(TotalAdjustRunoff(NAllSeries,3,WaterShed.NParamRange))
                if(allocated(AdjustRunoff))deallocate(AdjustRunoff)
                allocate(AdjustRunoff(NAllSeries,4))
            endif
            if(totalSolution(1).NRunoffGenType > 0) then
                if(allocated(TotalAllRunoff))deallocate(TotalAllRunoff)
                allocate(TotalAllRunoff(NAllSeries,5,WaterShed.NParamRange))
                if(allocated(AllRunoff))deallocate(AllRunoff)
                allocate(AllRunoff(NAllSeries,5))
            endif

        endif
        do ipid=1,npid
!            totalSolution(ipid)%IParamRange=hydropid(ipid)
            if (ParamRanges(totalSolution(ipid)%IParamRange)%NUpStreamParamRange > 0) then
                if(allocated(TotalUpHydroFinal(totalSolution(ipid)%IParamRange)%UpHydroFinal))deallocate(TotalUpHydroFinal(totalSolution(ipid)%IParamRange)%UpHydroFinal)
                allocate(TotalUpHydroFinal(totalSolution(ipid)%IParamRange)%UpHydroFinal(ParamRanges(totalSolution(ipid)%IParamRange)%NUpStreamParamRange))
                do j = 1,ParamRanges(totalSolution(ipid)%IParamRange)%NUpStreamParamRange
                    TotalUpHydroFinal(totalSolution(ipid)%IParamRange)%UpHydroFinal(j).UpReachId = ParamRanges(ParamRanges(totalSolution(ipid)%IParamRange)%UpstreamParamRanges(j)).SubId 
                    if (ParamRanges(totalSolution(ipid)%IParamRange)%NUpStreamParamRange > 0) then
                        if(allocated(TotalUpHydroFinal(totalSolution(ipid)%IParamRange)%UpHydroFinal(j)%UpQFinal)) then
                            deallocate(TotalUpHydroFinal(totalSolution(ipid)%IParamRange)%UpHydroFinal(j).UpQFinal)                        
                        endif
                        allocate(TotalUpHydroFinal(totalSolution(ipid)%IParamRange)%UpHydroFinal(j).UpQFinal(hydronCount))   
                    endif
                enddo    
            endif

            do j = 1,ParamRanges(totalSolution(ipid)%IParamRange)%NPartSubbasin                    
                subid = ParamRanges(totalSolution(ipid)%IParamRange)%PartSubbasins(j)
                if (totalSolution(1).Iweather > 0) then
                    if(allocated(s_Weather(subid)%Rain))deallocate(s_Weather(subid)%Rain)
                    allocate(s_Weather(subid)%Rain(hydroncount)) 
                else if (totalSolution(1).Iweather == 0) then
                    if(allocated(s_Weather(subid)%Rain))deallocate(s_Weather(subid)%Rain)
                    allocate(s_Weather(subid)%Rain(weatherdatacount)) 
                endif  
                if(allocated(s_Weather(subid)%Wind))deallocate(s_Weather(subid)%Wind)
                allocate(s_Weather(subid)%Wind(weatherdatacount))   
                if(allocated(s_Weather(subid)%Tmax))deallocate(s_Weather(subid)%Tmax)
                allocate(s_Weather(subid)%Tmax(weatherdatacount))   
                if(allocated(s_Weather(subid)%Tmin))deallocate(s_Weather(subid)%Tmin)
                allocate(s_Weather(subid)%Tmin(weatherdatacount))   
                if(allocated(s_Weather(subid)%Tavg))deallocate(s_Weather(subid)%Tavg)
                allocate(s_Weather(subid)%Tavg(weatherdatacount))   
                if(allocated(s_Weather(subid)%Hmdt))deallocate(s_Weather(subid)%Hmdt)
                allocate(s_Weather(subid)%Hmdt(weatherdatacount))   
                if(allocated(s_Weather(subid)%Slr))deallocate(s_Weather(subid)%Slr)
                allocate(s_Weather(subid)%Slr(weatherdatacount))   
                if(allocated(s_Weather(subid)%PET))deallocate(s_Weather(subid)%PET)
                allocate(s_Weather(subid)%PET(weatherdatacount)) 
!                if(allocated(s_Weather(subid)%E))deallocate(s_Weather(subid)%E)
!                allocate(s_Weather(subid)%E(hydroncount)) 

            enddo 

            if(allocated(totalp_Weather(totalSolution(ipid)%IParamRange)%Rain))deallocate(totalp_Weather(totalSolution(ipid)%IParamRange)%Rain)
            allocate(totalp_Weather(totalSolution(ipid)%IParamRange)%Rain(hydroncount))
            if(allocated(totalp_Weather(totalSolution(ipid)%IParamRange)%PET))deallocate(totalp_Weather(totalSolution(ipid)%IParamRange)%PET)
            allocate(totalp_Weather(totalSolution(ipid)%IParamRange)%PET(hydroncount))
            if(allocated(totalp_Weather(totalSolution(ipid)%IParamRange)%E))deallocate(totalp_Weather(totalSolution(ipid)%IParamRange)%E)
            allocate(totalp_Weather(totalSolution(ipid)%IParamRange)%E(hydroncount))

            do i = 1,hydronCount  
                if(ParamRanges( totalSolution(ipid)%IParamRange)%Itype .eq. 1)then
                    HydroObserveds(i)%QObserved(ParamRanges(totalSolution(ipid).IParamRange).HydroId) = max(fQ((ipid-1)*hydroncount+i),0.001)
                     if (totalSolution(ipid).runtype == 2 .or. totalSolution(ipid).runtype == 3 ) then
    !                    if (i >  totalSolution(ipid).NTimeStepInit)
                        TotalAnalyRunoff(i,1,hydropid(ipid)) = max(fQ((ipid-1)*hydroncount+i),0.001)
    !                     AnalyRunoff(i - totalSolution(ipid).NTimeStepInit,1)
                    elseif( totalSolution(ipid).runtype == 8.or. totalSolution(1).runtype == 31)then
    !                    if (i >  totalSolution(ipid).NTimeStepInit) AnalyRunoff(i - totalSolution(ipid).NTimeStepInit,1) = fQ((ipid-1)*hydroncount+i)
    !                    if (totalSolution(ipid).Rtmdy .ne. 0) AdjustRunoff(i,1) = fQ((ipid-1)*hydroncount+i)
    !                    if (totalSolution(ipid).NRunoffGenType > 0) AllRunoff(i,5) = fQ((ipid-1)*hydroncount+i)
                        TotalAnalyRunoff(i,1,hydropid(ipid)) = max(fQ((ipid-1)*hydroncount+i),0.001)
                        TotalAdjustRunoff(i,1,hydropid(ipid))= max(fQ((ipid-1)*hydroncount+i),0.001)
                        TotalAllRunoff(i,5,hydropid(ipid)) = max(fQ((ipid-1)*hydroncount+i),0.001)

                    else
    !                    if (totalSolution(ipid).Rtmdy .ne. 0) AdjustRunoff(i,1) = fQ((ipid-1)*hydroncount+i)
    !                    if (totalSolution(ipid).NRunoffGenType > 0) AllRunoff(i,5) = fQ((ipid-1)*hydroncount+i)
                        TotalAdjustRunoff(i,1,hydropid(ipid))= max(fQ((ipid-1)*hydroncount+i),0.001)
                        TotalAllRunoff(i,5,hydropid(ipid)) = max(fQ((ipid-1)*hydroncount+i),0.001)
                    endif
                elseif(ParamRanges( totalSolution(ipid)%IParamRange)%Itype .eq. 0)then
                    if (i == 1) then
                        iupres = iupres + 1
                    endif     
                    HydroObserveds(i)%QObserved(watershed.nhydrostation+ParamRanges(totalSolution(ipid).IParamRange).HydroId) = max(fQ1((iupres-1)*hydroncount+i),0.001)
                     if (totalSolution(ipid).runtype == 2 .or. totalSolution(ipid).runtype == 3 ) then
    !                    if (i >  totalSolution(ipid).NTimeStepInit)
                        TotalAnalyRunoff(i,1,hydropid(ipid)) = max(fQ1((iupres-1)*hydroncount+i),0.001)
    !                     AnalyRunoff(i - totalSolution(ipid).NTimeStepInit,1)
                    elseif( totalSolution(ipid).runtype == 8.or. totalSolution(1).runtype == 31)then
    !                    if (i >  totalSolution(ipid).NTimeStepInit) AnalyRunoff(i - totalSolution(ipid).NTimeStepInit,1) = fQ((ipid-1)*hydroncount+i)
    !                    if (totalSolution(ipid).Rtmdy .ne. 0) AdjustRunoff(i,1) = fQ((ipid-1)*hydroncount+i)
    !                    if (totalSolution(ipid).NRunoffGenType > 0) AllRunoff(i,5) = fQ((ipid-1)*hydroncount+i)
                        TotalAnalyRunoff(i,1,hydropid(ipid)) = max(fQ1((iupres-1)*hydroncount+i),0.001)
                        TotalAdjustRunoff(i,1,hydropid(ipid))= max(fQ1((iupres-1)*hydroncount+i),0.001)
                        TotalAllRunoff(i,5,hydropid(ipid)) = max(fQ1((iupres-1)*hydroncount+i),0.001)

                    else
    !                    if (totalSolution(ipid).Rtmdy .ne. 0) AdjustRunoff(i,1) = fQ((ipid-1)*hydroncount+i)
    !                    if (totalSolution(ipid).NRunoffGenType > 0) AllRunoff(i,5) = fQ((ipid-1)*hydroncount+i)
                        TotalAdjustRunoff(i,1,hydropid(ipid))= max(fQ1((iupres-1)*hydroncount+i),0.001)
                        TotalAllRunoff(i,5,hydropid(ipid)) = max(fQ1((iupres-1)*hydroncount+i),0.001)

                    endif 
!                elseif(ParamRanges( totalSolution(ipid)%IParamRange)%Itype .eq. 2)then
!                    HydroObserveds(i)%QObserved(ParamRanges(totalSolution(ipid).IParamRange).HydroId) = max(fQ((ipid-1)*hydroncount+i),0.001)
!                     if (totalSolution(ipid).runtype == 2 .or. totalSolution(ipid).runtype == 3 ) then
!    !                    if (i >  totalSolution(ipid).NTimeStepInit)
!                        TotalAnalyRunoff(i,1,hydropid(ipid)) = max(fQ((ipid-1)*hydroncount+i),0.001)
!    !                     AnalyRunoff(i - totalSolution(ipid).NTimeStepInit,1)
!                    elseif( totalSolution(ipid).runtype == 8.or. totalSolution(1).runtype == 31)then
!    !                    if (i >  totalSolution(ipid).NTimeStepInit) AnalyRunoff(i - totalSolution(ipid).NTimeStepInit,1) = fQ((ipid-1)*hydroncount+i)
!    !                    if (totalSolution(ipid).Rtmdy .ne. 0) AdjustRunoff(i,1) = fQ((ipid-1)*hydroncount+i)
!    !                    if (totalSolution(ipid).NRunoffGenType > 0) AllRunoff(i,5) = fQ((ipid-1)*hydroncount+i)
!                        TotalAnalyRunoff(i,1,hydropid(ipid)) = max(fQ((ipid-1)*hydroncount+i),0.001)
!                        TotalAdjustRunoff(i,1,hydropid(ipid))= max(fQ((ipid-1)*hydroncount+i),0.001)
!                        TotalAllRunoff(i,5,hydropid(ipid)) = max(fQ((ipid-1)*hydroncount+i),0.001)
!
!                    else
!    !                    if (totalSolution(ipid).Rtmdy .ne. 0) AdjustRunoff(i,1) = fQ((ipid-1)*hydroncount+i)
!    !                    if (totalSolution(ipid).NRunoffGenType > 0) AllRunoff(i,5) = fQ((ipid-1)*hydroncount+i)
!                        TotalAdjustRunoff(i,1,hydropid(ipid))= max(fQ((ipid-1)*hydroncount+i),0.001)
!                        TotalAllRunoff(i,5,hydropid(ipid)) = max(fQ((ipid-1)*hydroncount+i),0.001)
!                    endif
                endif
                Upallrain(ipid,i) = Updrp((ipid-1)*hydroncount+i)

                iyear=ceiling(real(i)/counteveyear)            
                        
                if (nyear > 1 )then 
                    iyear=ceiling((real(i)-0.1)/counteveyear)
                    IF (iyear > nyear ) then 
                    iyear = nyear
                    endif
                else
                    iyear = 1
                endif    
                
                if (totalSolution(1).Iweather == 1) then
                    do j = 1,ParamRanges(totalSolution(ipid)%IParamRange)%NPartSubbasin
                        subid = ParamRanges(totalSolution(ipid)%IParamRange)%PartSubbasins(j)
                        s_Weather(subid)%Rain(i) = 0
                        do ii = 1,totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%nstcd0
                            s_Weather(subid)%Rain(i)  = s_Weather(subid)%Rain(i)  + fPPtn(ii,(ipid-1)*hydroncount+i)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight0(ii,iyear)
                        enddo
                    enddo  
                elseif(totalSolution(1).Iweather == 2) then
                    do j = 1,ParamRanges(totalSolution(ipid)%IParamRange)%NPartSubbasin
                        subid = ParamRanges(totalSolution(ipid)%IParamRange)%PartSubbasins(j)
                        s_Weather(subid)%Rain(i) = fPPtn(j,(ipid-1)*hydroncount+i)
                    enddo
                endif
            end do

            do i = 1,weatherdatacount
                if (nyear > 1 )then 
                    iyear=ceiling((real(i)-0.1)/122)
                    IF (iyear > nyear ) then 
                    iyear = nyear
                    endif
                else
                    iyear = 1
                endif    
                if (totalSolution(1).Iweather == 0) then               
                    do j = 1,ParamRanges(totalSolution(ipid)%IParamRange)%NPartSubbasin
                        subid = ParamRanges(totalSolution(ipid)%IParamRange)%PartSubbasins(j)
                        s_Weather(subid)%Rain(i) = 0
                        s_Weather(subid)%Wind(i) = 0
                        s_Weather(subid)%Tmax(i) = 0
                        s_Weather(subid)%Tmin(i) = 0
                        s_Weather(subid)%Tavg(i) = 0
                        s_Weather(subid)%Hmdt(i) = 0
                        s_Weather(subid)%Slr(i)  = 0
                        do ii = 1,totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%nstcd1
                            
                            s_Weather(subid)%Hmdt(i)  = s_Weather(subid)%Hmdt(i)  + fhmdt(ii,(ipid-1)*weatherdatacount+i)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
                            s_Weather(subid)%Slr(i)   = s_Weather(subid)%Slr(i)   + fIslr(ii,(ipid-1)*weatherdatacount+i)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
                            s_Weather(subid)%Tavg(i)  = s_Weather(subid)%Tavg(i)  + fTavg(ii,(ipid-1)*weatherdatacount+i)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
                            s_Weather(subid)%Tmax(i)  = s_Weather(subid)%Tmax(i)  + fTmaxt(ii,(ipid-1)*weatherdatacount+i)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
                            s_Weather(subid)%Tmin(i)  = s_Weather(subid)%Tmin(i)  + fTmin(ii,(ipid-1)*weatherdatacount+i)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
                            s_Weather(subid)%Wind(i)  = s_Weather(subid)%Wind(i)  + fWsws(ii,(ipid-1)*weatherdatacount+i)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
!                            if(iyear==1)then
!                            itemp=(i-1)*nyear+iyear
!                            else
!                            itemp=(i-1-(iyear-1)*counteveyearweather)*nyear+iyear
!                            endif
!                            s_Weather(subid)%Hmdt(i)  = s_Weather(subid)%Hmdt(i)  + fhmdt(ii,itemp)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
!                            s_Weather(subid)%Slr(i)   = s_Weather(subid)%Slr(i)   + fIslr(ii,itemp)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
!                            s_Weather(subid)%Tavg(i)  = s_Weather(subid)%Tavg(i)  + fTavg(ii,itemp)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
!                            s_Weather(subid)%Tmax(i)  = s_Weather(subid)%Tmax(i)  + fTmaxt(ii,itemp)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
!                            s_Weather(subid)%Tmin(i)  = s_Weather(subid)%Tmin(i)  + fTmin(ii,itemp)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
!                            s_Weather(subid)%Wind(i)  = s_Weather(subid)%Wind(i)  + fWsws(ii,itemp)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
 
                    
                        enddo
                        do ii = 1,totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%nstcd0
                            s_Weather(subid)%Rain(i)  = s_Weather(subid)%Rain(i)  + fPPtn(ii,(ipid-1)*weatherdatacount+i)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight0(ii,iyear)
                        enddo
                    enddo
                else
                    do j = 1,ParamRanges(totalSolution(ipid)%IParamRange)%NPartSubbasin
                        subid = ParamRanges(totalSolution(ipid)%IParamRange)%PartSubbasins(j)
                        s_Weather(subid)%Wind(i) = 0
                        s_Weather(subid)%Tmax(i) = 0
                        s_Weather(subid)%Tmin(i) = 0
                        s_Weather(subid)%Tavg(i) = 0
                        s_Weather(subid)%Hmdt(i) = 0
                        s_Weather(subid)%Slr(i)  = 0
                        do ii = 1,totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%nstcd1
                            s_Weather(subid)%Hmdt(i)  = s_Weather(subid)%Hmdt(i)  + fhmdt(ii,(ipid-1)*weatherdatacount+i)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
                            s_Weather(subid)%Slr(i)   = s_Weather(subid)%Slr(i)   + fIslr(ii,(ipid-1)*weatherdatacount+i)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
                            s_Weather(subid)%Tavg(i)  = s_Weather(subid)%Tavg(i)  + fTavg(ii,(ipid-1)*weatherdatacount+i)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
                            s_Weather(subid)%Tmax(i)  = s_Weather(subid)%Tmax(i)  + fTmaxt(ii,(ipid-1)*weatherdatacount+i)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
                            s_Weather(subid)%Tmin(i)  = s_Weather(subid)%Tmin(i)  + fTmin(ii,(ipid-1)*weatherdatacount+i)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
                            s_Weather(subid)%Wind(i)  = s_Weather(subid)%Wind(i)  + fWsws(ii,(ipid-1)*weatherdatacount+i)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
!                            if(iyear==1)then
!                            itemp=(i-1)*nyear+iyear
!                            else
!                            itemp=(i-1-(iyear-1)*counteveyearweather)*nyear+iyear
!                            endif
!                            s_Weather(subid)%Hmdt(i)  = s_Weather(subid)%Hmdt(i)  + fhmdt(ii,itemp)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
!                            s_Weather(subid)%Slr(i)   = s_Weather(subid)%Slr(i)   + fIslr(ii,itemp)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
!                            s_Weather(subid)%Tavg(i)  = s_Weather(subid)%Tavg(i)  + fTavg(ii,itemp)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
!                            s_Weather(subid)%Tmax(i)  = s_Weather(subid)%Tmax(i)  + fTmaxt(ii,itemp)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
!                            s_Weather(subid)%Tmin(i)  = s_Weather(subid)%Tmin(i)  + fTmin(ii,itemp)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
!                            s_Weather(subid)%Wind(i)  = s_Weather(subid)%Wind(i)  + fWsws(ii,itemp)*totalWeatherInfos(totalSolution(ipid)%IParamRange)%WeatherInfos(j)%weight1(ii,iyear)
 
                        enddo
                    enddo
                  
                endif
            end do
        enddo        

    endsubroutine

    subroutine OptFloodInfo(optfloodid,Nlines,Npoints,IForcast,Qlimit,QLow,QHigh,Qchar)
        integer ::optfloodid,Nlines,Npoints,IForcast,count
        real Qlimit,QLow,QHigh
        character (100) Qchar
        FloodChartemp.IForcast = IForcast
        FloodChartemp.Nlines   = Nlines
        FloodChartemp.Npoints  = Npoints
        FloodChartemp.Qlimit   = Qlimit
        FloodChartemp.QLow   = QLow
        FloodChartemp.QHigh  = QHigh
        FloodChartemp.Stbu=10
        FloodChartemp.Stbl=1
        FloodChartemp.Locbu=1
        FloodChartemp.Locbl=0
        if(allocated(FloodChartemp.Qmax)) deallocate(FloodChartemp.Qmax)
        allocate(FloodChartemp.Qmax(Nlines+2))  
        
    end subroutine

    subroutine OptFloodInfochar(Qchar,idx,len)
        integer idx,i,count,len
        character(len) Qchar
        character(len) strTemp
        strTemp = Qchar
        if(idx==1)then
            call GetStrSplitterCount(strTemp,100,count ,',')  
            read(strTemp,*)    (FloodChartemp.Qmax(i),i=2,FloodChartemp.Nlines+1,1)            
            FloodChartemp.Qmax(1)=FloodChartemp.QLow
            FloodChartemp.Qmax(FloodChartemp.Nlines+2)=FloodChartemp.QHigh
        endif
    end subroutine

    subroutine ResOptSolution(resvars,NResCK)
        implicit none
        integer NResCK,ipid,iNResCK,i
        real resvars(NResCK,14)
        
        if(allocated(restype))deallocate(restype)
        allocate(restype(NResCK))

        if(allocated(floodcharpoint)) deallocate(floodcharpoint)
        allocate(floodcharpoint(WaterShed.NParamRange))

        do i=1,NResCK
           restypetemp.IParamRange = resvars(i,1) 
           restypetemp.ResDHMID = resvars(i,2) 
           restypetemp.ResID = resvars(i,3)
           restypetemp.StoreType = resvars(i,4) 
!           restypetemp.StoreMod = resvars(i,5) 
           restypetemp.OptFloodID = resvars(i,6) 
!           restypetemp.Qout = resvars(i,7) 
!           restypetemp.LocMod = resvars(i,7) 
           restypetemp.Qlimit = resvars(i,8) 
!           restypetemp.NormStorage = resvars(i,9)*10000*restypetemp.StoreMod
!           restypetemp.Storage = resvars(i,10)*10000*restypetemp.StoreMod
!           restypetemp.DeadStorage = resvars(i,11)*10000*restypetemp.StoreMod
           restypetemp.NormSt = resvars(i,9)*10000
           restypetemp.St = resvars(i,10)*10000
           restypetemp.DeadSt = resvars(i,11)*10000     
           if(restypetemp.DeadSt<0.1*restypetemp.NormSt) then
               restypetemp.DeadSt = 0.1*restypetemp.NormSt
           endif
           restypetemp.Area = resvars(i,12)*1000000
           restypetemp.StoreMod = resvars(i,13)
           restypetemp.LocMod = resvars(i,14)
           restypetemp.floodchar.IForcast=FloodChartemp.IForcast
           restypetemp.floodchar.Nlines   = FloodChartemp.Nlines
           restypetemp.floodchar.Npoints  = FloodChartemp.Npoints
           restypetemp%floodchar.Qlimit   =  FloodChartemp.Qlimit*restypetemp.Qlimit
           restypetemp.floodchar.QLow=(FloodChartemp.QLow)*restypetemp.Qlimit
           restypetemp.floodchar.QHigh=(FloodChartemp.QHigh)*restypetemp.Qlimit
            if(allocated( restype(i).floodchar.Qmax)) deallocate( restype(i).floodchar.Qmax)
            allocate( restype(i).floodchar.Qmax(FloodChartemp.Nlines+2))
            if(allocated( restypetemp.floodchar.Qmax)) deallocate( restypetemp.floodchar.Qmax)
            allocate( restypetemp.floodchar.Qmax(FloodChartemp.Nlines+2))
!           restypetemp.floodchar.Qmax=FloodChartemp.Qmax*restypetemp.Qout
            restypetemp.floodchar.Qmax=FloodChartemp.Qmax
           
            restype(i) = restypetemp
            if(allocated( restype(i).ResIn1)) deallocate( restype(i).ResIn1)
            allocate( restype(i).ResIn1(NAllSeries))
            if(allocated( restype(i).ResIn2)) deallocate( restype(i).ResIn2)
            allocate( restype(i).ResIn2(NAllSeries)) 
            if(allocated( restype(i).ResIn3)) deallocate( restype(i).ResIn3)
            allocate( restype(i).ResIn3(NAllSeries))
            restype(i).ResIn1=0
            restype(i).ResIn2=0
            restype(i).ResIn3=0
            if(restype(i).StoreType==1)then
                if(allocated(floodcharpoint(restypetemp.IParamRange).resx1)) then
                    cycle
                else
                    allocate(floodcharpoint(restypetemp.IParamRange).resx1(restype(i).floodchar.Nlines,restype(i).floodchar.Npoints))
                    allocate(floodcharpoint(restypetemp.IParamRange).resy1(restype(i).floodchar.Nlines,restype(i).floodchar.Npoints))

                endif 
            endif
            if(restype(i).StoreType==2)then
                if(allocated(floodcharpoint(restypetemp.IParamRange).resx2)) then
                    cycle
                else
                    allocate(floodcharpoint(restypetemp.IParamRange).resx2(restype(i).floodchar.Nlines,restype(i).floodchar.Npoints))
                    allocate(floodcharpoint(restypetemp.IParamRange).resy2(restype(i).floodchar.Nlines,restype(i).floodchar.Npoints))

                endif 
            endif
            if(restype(i).StoreType==3)then
                if(allocated(floodcharpoint(restypetemp.IParamRange).resx3)) then
                    cycle
                else
                    allocate(floodcharpoint(restypetemp.IParamRange).resx3(restype(i).floodchar.Nlines,restype(i).floodchar.Npoints))
                    allocate(floodcharpoint(restypetemp.IParamRange).resy3(restype(i).floodchar.Nlines,restype(i).floodchar.Npoints))

                endif 
            endif

        enddo
        NResOptSolution=NResCK  
!        do i=1,NResCK
!            if(restype(i).StoreType==1)then
!                if(allocated(resx1)) deallocate(resx1)
!                if(allocated(resy1)) deallocate(resy1)
!                if(allocated(resxx1)) deallocate(resxx1)
!                allocate(resx1(restype(i).floodchar.Nlines,restype(i).floodchar.Npoints))
!                allocate(resy1(restype(i).floodchar.Nlines,restype(i).floodchar.Npoints))
!                allocate(resxx1(restype(i).floodchar.Nlines,restype(i).floodchar.Npoints))
!                exit
!            endif
!        enddo
!        do i=1,NResCK
!            if(restype(i).StoreType==2)then
!                if(allocated(resx2)) deallocate(resx2)
!                if(allocated(resy2)) deallocate(resy2)
!                if(allocated(resxx2)) deallocate(resxx2)
!                allocate(resx2(restype(i).floodchar.Nlines,restype(i).floodchar.Npoints))
!                allocate(resy2(restype(i).floodchar.Nlines,restype(i).floodchar.Npoints))
!                allocate(resxx2(restype(i).floodchar.Nlines,restype(i).floodchar.Npoints))
!                exit
!            endif
!        enddo
!        do i=1,NResCK
!            if(restype(i).StoreType==3)then
!                if(allocated(resx3)) deallocate(resx3)
!                if(allocated(resy3)) deallocate(resy3)
!                if(allocated(resxx3)) deallocate(resxx3)
!                allocate(resx3(restype(i).floodchar.Nlines,restype(i).floodchar.Npoints))
!                allocate(resy3(restype(i).floodchar.Nlines,restype(i).floodchar.Npoints))
!                allocate(resxx3(restype(i).floodchar.Nlines,restype(i).floodchar.Npoints))
!                exit
!            endif
!        enddo
    end subroutine

    subroutine OptFloodchar(floodparvars,nfloodpar,nline,npoint,ntotalcount)
        implicit none
        integer ntotalcount,i,j,k,nfloodpar
        integer nline(nfloodpar,2),npoint(nfloodpar,2)
        real floodparvars(ntotalcount,6)

        do i=1,ntotalcount
            if(floodparvars(i,1)==1)then
!                do j=1,floodparvars(i,2)
!                    if(floodparvars(i,2)==j)then
!                        do k=1,floodparvars(i,3)
!                            if(floodparvars(i,3)==k)then
!                                resx1(j,k)=floodparvars(i,4)
!                                resy1(j,k)=floodparvars(i,5)
!                            endif
!                        enddo
!                    endif
                if(allocated(floodcharpoint(restypetemp.IParamRange).resx1)) then

                    floodcharpoint(floodparvars(i,6)).resx1(floodparvars(i,2),floodparvars(i,3))=floodparvars(i,4)
                    floodcharpoint(floodparvars(i,6)).resy1(floodparvars(i,2),floodparvars(i,3))=floodparvars(i,5)
                else
                    cycle
                endif

!                enddo
            elseif(floodparvars(i,1)==2)then
!                do j=1,floodparvars(i,2)
!                    if(floodparvars(i,2)==j)then
!                        do k=1,floodparvars(i,3)
!                            if(floodparvars(i,3)==k)then
!                                resx2(j,k)=floodparva)rs(i,4
!                                resy2(j,k)=floodparvars(i,5)
!                            endif
!                        enddo
!                    endif
!                enddo
                if(allocated(floodcharpoint(restypetemp.IParamRange).resx2)) then
                    floodcharpoint(floodparvars(i,6)).resx2(floodparvars(i,2),floodparvars(i,3))=floodparvars(i,4)
                    floodcharpoint(floodparvars(i,6)).resy2(floodparvars(i,2),floodparvars(i,3))=floodparvars(i,5)
                else
                    cycle
                endif
            elseif(floodparvars(i,1)==3)then
!                do j=1,floodparvars(i,2)
!                    if(floodparvars(i,2)==j)then
!                        do k=1,floodparvars(i,3)
!                            if(floodparvars(i,3)==k)then
!                                resx3(j,k)=floodparvars(i,4)
!                                resy3(j,k)=floodparvars(i,5)
!                            endif
!                        enddo
!                    endif
!                enddo
                if(allocated(floodcharpoint(restypetemp.IParamRange).resx3)) then

                    floodcharpoint(floodparvars(i,6)).resx3(floodparvars(i,2),floodparvars(i,3))=floodparvars(i,4)
                    floodcharpoint(floodparvars(i,6)).resy3(floodparvars(i,2),floodparvars(i,3))=floodparvars(i,5)
                else
                   cycle
                endif
            endif
        enddo
    end subroutine
end module