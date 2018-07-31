module AutoTimes
    real StartTime,EndTime,AutoTime
    integer ::nProcessLocal = -1 
    integer ::nProcessCount = -1
    contains
    subroutine GetEndTime()
	    call gettim(ihr1,imin1,isec1,i100th1)
	    EndTime = (ihr1*3600 + imin1*60 + isec1) + i100th1/100.0
	    AutoTime = EndTime - StartTime
    endsubroutine

    ! 获得开始时间的秒数
    subroutine GetStartTime()
	    call gettim(ihr1,imin1,isec1,i100th1)
	    StartTime = (ihr1*3600 + imin1*60 + isec1) + i100th1/100.0
	    AutoTime = 0.0
	    EndTime = 0.0
    endsubroutine
end module