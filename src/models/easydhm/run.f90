subroutine run(RunType,CurRunoffGenType1,ipid)
	
		use SolutionMod
		use TimeInfoMod
		use OutputPRMod
		use HydroDataMod
		use HydroObservedMod
		use WaterShedMod
        use ParamRangeMod
        use HydroInfoMod
        use AutoTimes
        use ST_PR_OutMod
        implicit none
        	
		integer RunType,CurRunoffGenType1,ipid
		
		! 各产流模型产汇流参数读入

		call CalModelPara(CurRunoffGenType1)	

	    call RunLoop(Runtype,CurRunoffGenType1,ipid)
        
		return
		
		contains

           subroutine RunLoop(Runtype,CurRunoffGenType1,ipid)
                use ReachVarMod
                use EasyDHMVarMod
                use ResVarMod

                character(30) ::dataTemp
                character(300)::PostString
                integer dateDay,dateMonth,dateYear,hour
                integer IDayOld,IYearOld
                integer i,ipid
                integer Runtype,CurRunoffGenType1,iflood,dataTempint,year,yeartemp
                integer IHydro,IParamrange,IX,j
                real Qhydro,Qinit,Vinit

                jdt = jdt0	

                IUpdateWeather = 1
                iwLoop = 1

                do i = 1,NAllSeries
                    if(mod(i,150)==0)then
                        continue
                    endif
                    if (i <= totalsolution(ipid)%TimesALL(1))then
                        Cursolution%DT = totalSolution(ipid)%dtold(1)
                        else if(i <= totalsolution(ipid)%TimesALL(2))then
                        Cursolution%DT = totalSolution(ipid)%dtold(2)
                        else
                        Cursolution%DT = totalSolution(ipid)%dtold(3)
                    endif
                    dataTemp = HydroObserveds(i)%DateTime
                    dateYear=0
                    dateMonth=0
                    dateDay=0
                    call CstrToint(dataTemp,dateYear,dateMonth,dateDay)
                    if (i==1) then
                        IDayOld  = dateDay
	                    IYearOld = dateYear
                    endif
                    
                    if (i.ne.1) then
                        if (dateDay .ne. IDayOld) then
                            jdt = jdt + 1
                            IUpdateWeather = 1
                            IDayOld = dateDay
                            iwLoop = iwLoop + 1
                        else
                            IupdateWeather = 0
                        end if
                    endif
                    
                    if(dateYear.ne.IYearOld) then
                        jdt = 1
                        IDayOld = 1
                        IYearOld = dateYear
                    endif                    
                    iLoop = i

                    call ChangeStringInt(dataTemp,len(dataTemp),dataTempint)
                    year=int(dataTempint/1000000)
                    year=year*1000000
                    yeartemp=dataTempint-year
                    hour = dataTempint - dateYear*1000000 - dateMonth*10000 - dateDay * 100
                    if(dataTempint==2012072808)then
                    continue
                    endif
                    if(yeartemp==40108)then
                        call InitDHMVars
                        call InitReachVars(CurRunoffGenType1)
		                call InitResVars
                    endif
                    
                    call InitStatVars(CurRunoffGenType1)

	                 ! 产流模拟	                
				    call LandSurfaceSim(CurRunoffGenType1)

			        ! 子流域内汇流 + 子流域间汇流
		            call ConfluxSim(CurRunoffGenType1,dateYear,dateMonth,dateDay,ipid,i)

        			!----------------------参数分区输出1------------------------------------ 
        			if (Cursolution%Runtype /= 8 .and. Cursolution%Runtype /= 3) then
        			
		                if (Runtype == 1 .or. Runtype == 5) then
		                    ! 计算结果输出
	                        call OutputRangePR(CurRunoffGenType1,i,dataTemp,ipid)
	                        write(*,*) dateYear,dateMonth,dateDay,'finished'
                        endif
                    endif
                    !------------------------------------------------------------------ 
                    nProcessLocal = i

                    
	            enddo  

                if (CurSolution%runtype == 1 .or. CurSolution%runtype == 5) then
                    if (Cursolution%Rtmdy .ne. 0) then
                    !----------------------校正+参数分区输出2------------------------------------
                        Call AutoAdjustRunoff(CurRunoffGenType1,ipid)
	                !------------------------------------------------------------------                         
                    endif
                endif

           end subroutine
            
           subroutine CstrToint(ctmp1,orgInt1,orgInt2,orgInt)
                integer::orgInt,orgInt1,orgInt2
                integer ::IW,ibegin,iend,nCount,IW2,ibegin1,iend1
                character (30) ctmp1
                character(4) cc
                IW2=1 
                nCount = 0  
                ibegin = 0             
                DO WHILE (.TRUE.)
                    if(ctmp1(IW2:IW2) .eq. ' ')EXIT
                    if(ctmp1(IW2:IW2) .eq. '-' .or.ctmp1(IW2:IW2) .eq. '/')then
                        nCount = nCount + 1
                    endif 
                    if((ctmp1(IW2:IW2) .eq. '-' .or.ctmp1(IW2:IW2) .eq. '/').and. nCount .eq. 1)then
                        ibegin1 = IW2 +1       
                    endif
                    if((ctmp1(IW2:IW2) .eq. '-' .or.ctmp1(IW2:IW2) .eq. '/').and. nCount .eq. 2)then
                        ibegin = IW2 +1       
                    endif
                    IW2 = IW2 + 1
                END DO  
                iend = IW2 -1 
                iend1 = ibegin - 2
                cc=ctmp1(1:4)

                read(cc,*) orgInt1
                read(ctmp1(ibegin1:iend1),*) orgInt2
                read(ctmp1(ibegin:iend),*) orgInt
                
                return
           end subroutine

           subroutine cstr_find_s(ctmp1)
                integer :: IW2
                character(4000),intent (inout):: ctmp1
                IW2=1            
                DO WHILE (.TRUE.)
                    if(ctmp1(IW2:IW2) .eq. ';')then
                        ctmp1(IW2:IW2) = ""
                    endif
                    IW2 = IW2 + 1
                    if(IW2 .gt.4000)exit
                END DO
            end subroutine
	end 
			
		subroutine AllocateOuttable (CurRunoffGenType1,npid)
		    use ST_PR_OutMod
		    use ParamRangeMod
		    use HydroDataMod
            use reachinfoMod
		        
		    integer CurRunoffGenType1,NSub,NUnit,npid,ipid
            integer NStore,ix
		        
		    NSub=0
            NStore = 0
		    do ipid=1,npid
		        NSub=NSub+ParamRanges(totalSolution(ipid)%IParamRange)%NPartSubbasin
                do i = 1,ParamRanges(totalSolution(ipid)%IParamRange)%NPartSubbasin
                    ix = ParamRanges(totalSolution(ipid)%IParamRange)%PartSubbasins(i)
                    if (Reachs(IX)%IStore == 1) then
                        NStore = NStore + 1
                    endif
                enddo
		    enddo

 		    NUnit = NSub
		        
            if(allocated(outST_PR)) deallocate(outST_PR)
            allocate(outST_PR(NAllSeries*npid))
                
            ST_PRNCK = 0
            outIdx = 1
		end subroutine