 ! 接口函数，根据SolutionId，查找数据库中方案的其他信息，并进行该方案的计算
    module EasyDHMMod
        
        use SolutionMod
        
        use WaterShedMod
        use HydroDataMod
        use ParamRangeMod
        use ResVarMod
!        use DBSQLLinkMod
        
        implicit none
        
        integer i,j
        integer NRunoffGenType, IRunoffGenType(5),CurRunoffGenType
        integer RunType, LstRunType ,IParamYear
        integer IParamRange, JParamRange
        
        contains
            subroutine EasyDHM(SolutionID,npid)                 !!easyDHM的Fortran程序起点
                integer SolutionID,npid,ipid
                
                 ! 从CurSolution中赋值
                 call AllocateOuttable(totalSolution(1).IRunoffGenType(1),npid)

                 do ipid = 1,npid
                 
                    IParamYear     = totalSolution(ipid).IParamYear
                    Runtype        = totalSolution(ipid).Runtype
                    IParamRange    = totalSolution(ipid).IParamRange
                    LstRunType     = Runtype
                    NRunoffGenType = totalSolution(ipid).NRunoffGenType
                    do i = 1 , NRunoffGenType
                        IRunoffGenType(i) = totalSolution(ipid).IRunoffGenType(i)
                    enddo
                    Cursolution = totalSolution(ipid)
                    Cursolution.ithpid = ipid
                    ! 水库信息、单元信息及调度图信息读取，暂时放在这里。 lwh
    !                call InitDBSQLLink(iRetLinkSuc)
    !                call ResOptSolution(SolutionId,IParamRange)



 			        select case (RunType)
! 			            case(1)
! 			                !write(*,*) '**************按默认参数运行EasyDHM模型...  '
! 			                call RunDefaultPara(npid)
! 			            case(2)
! 			                !write(*,*) '**************参数敏感性分析...   '
! 			                call RunSenseAnalyst(IParamYear,npid,SolutionId)
!  			            case(21)
! 			                !write(*,*) '**************参数敏感性分析...   '
! 			                call RunSenseAnalyst(IParamYear,npid,SolutionId)
! 			            case(22)
! 			                !write(*,*) '**************参数敏感性分析...   '
! 			                call RunSenseAnalyst(IParamYear,npid,SolutionId)
!			            case(3)
! 			                !write(*,*) '**************参数优化...   '
! 			                call RunParamOptimize(IParamYear,npid,SolutionId)
! 			            case(4)
! 			                !write(*,*) '**************参数不确定性分析...   '
! 			                call RunUncertainty()
 			            case(5)
 			                !write(*,*) '**************运行最优参数...   '
 			                call RunBestPara(IParamYear,npid,SolutionId)
! 			            case(6)
! 			                !write(*,*) '**************运行所有相关参数分区...   '
! 			                call UpallParamRange()
!    !  			        case(7)
!    ! 			            !write(*,*) '**************提取气象数据...   '
!    ! 			            call ExtractRDSData()
! 			            case(8)
! 			                !write(*,*) '**************参数敏感性分析+优化...   '
! 			                call RunParamAnalyst(IParamYear,npid,SolutionId)
!                        case(51)
!                            call RunfloodOpt(IParamYear,npid,SolutionId)
!                        case(52)
!                            call RunBestPara(IParamYear,npid,SolutionId)
!                        case(31)
!                            call RunParamOptimize(IParamYear,npid,SolutionId)
 			        end select
                enddo
 			    
                
            end subroutine
 			
! 			subroutine RunDefaultPara(npid)
! 			    integer npid,ipid
!                do i=1,NRunoffGenType
! 			        do ipid=1,npid
!		                CurSolution=totalSolution(ipid)                    
!                        CurRunoffGenType = totalSolution(ipid).IRunoffGenType(i)                    	
!                        AdjustRunoff(1:NAllSeries,1)=TotalAdjustRunoff(1:NAllSeries,1,totalSolution(ipid)%IParamRange)
!	                    call run(RunType,CurRunoffGenType,ipid)
!                    enddo	                
!                enddo
!                if (NRunoffGenType .gt. 1) then
!                
!                    call Multi_ModelForecast(ipid)
!                    
!                endif
!		        return
!		        
!		    end subroutine
!		    
!		    subroutine RunSenseAnalyst(SolutionID,npid,sid)
!                integer SolutionID,npid,ipid,sid
!		        LstRunType = RunType
!		        
!		        Runtype = 1
!!		        call RunDefaultPara(npid)
!		        RunType = LstRunType
!                CurRunoffGenType = totalSolution(1).IRunoffGenType(1)
!		        call automet(RunType,CurRunoffGenType,SolutionID,npid,sid)	
!                                
!    			return	
!            end subroutine          
!            
!            subroutine RunParamOptimize(SolutionID,npid,sid)
!                integer SolutionID,npid,ipid,sid
!		        LstRunType = RunType
!		        
!		        Runtype = 1
!!		        call RunDefaultPara(npid)
!		        RunType = LstRunType
!                CurRunoffGenType = totalSolution(1).IRunoffGenType(1)
!		        call automet(RunType,CurRunoffGenType,SolutionID,npid,sid)	
!		        
!		        return
!		    end  subroutine  
!		    
!		    subroutine RunUncertainty
!		        
!		        LstRunType = RunType
!		        
!!		        RunType = 7
!!		        call ExtractRDSData()	
!		        
!		        Runtype = 6
!		        call UpallParamRange()
!		        
!		        CurRunoffGenType = IRunoffGenType(1)
!!                call getAnalysisPath()
!!		        call automet(RunType,CurRunoffGenType)
!		        
!		        return
!		    end subroutine
		    
		    subroutine RunBestPara(SolutionID,npid,Sid)
                integer SolutionID,npid,sid,ipid
                do i=1,NRunoffGenType
                    CurRunoffGenType = IRunoffGenType(i)
                	
		            call automet(RunType,CurRunoffGenType,SolutionID,npid,sid)
                enddo

!                if (NRunoffGenType .gt. 1) then
!                    call RunGroupForecast
!                endif
			    
		        return		        
		    end subroutine

!		    subroutine RunfloodOpt(SolutionID,npid,Sid)
!                integer SolutionID,npid,sid
!                do i=1,NRunoffGenType
!                    CurRunoffGenType = IRunoffGenType(i)
!                	
!		            call automet(RunType,CurRunoffGenType,SolutionID,npid,sid)
!                enddo
!
!!                if (NRunoffGenType .gt. 1) then
!!                    call RunGroupForecast
!!                endif
!			    
!		        return		        
!		    end subroutine
!
!
!		    
!		    subroutine UpallParamRange()
!		        
!		        LstRunType = RunType
!		        RunType = 5
!        		
!		        JparamRange = IParamRange
!        		
!		        do i = 1,WaterShed.NParamRange
!		            if (ParamRangeUpStreamAll(IParamRange,i).eq.1.or.i == IparamRange) then
!		                CurSolution.IparamRange   =  i
!        		        
!		                !write(*,*) "----------------------------------------------------"
!	                    !write(*,*) "按顺序计算至第",i,"号参数分区"
!                        
!                        NRunoffGenType = Cursolution.NRunoffGenType
!                        do j = 1 , NRunoffGenType
!                            IRunoffGenType(j) = Cursolution.IRunoffGenType(j)
!                        enddo   
!                                             
!                        do j=1,NRunoffGenType
!                            CurRunoffGenType = IRunoffGenType(j)
!                        	
!!		                    call automet(RunType,CurRunoffGenType)
!                        enddo
!
!!                        if (NRunoffGenType .gt. 1) then
!!                            call RunGroupForecast
!!                        endif
!                        
!                        !write(*,*) "第",i,"号参数分区计算结束"
!                    endif
!                enddo
!                
!                CurSolution.IParamRange = JParamRange
!
!		        RunType = LstRunType
!
!		        return		    
!		    end subroutine
!		    
!		    subroutine RunParamAnalyst(SolutionID,npid,sid)
!		    
!                integer SolutionID,npid,sid
!		        LstRunType = RunType
!                do i=1,NRunoffGenType
!                CurRunoffGenType = IRunoffGenType(i)
!		        Runtype = 1
!!		        call RunDefaultPara(npid)
!		        
!		        RunType = 8
!    		    
!		        call automet(RunType,CurRunoffGenType,SolutionID,npid,sid)	
!    			enddo
!    			return	
!		    
!		    end subroutine 
    
    end module