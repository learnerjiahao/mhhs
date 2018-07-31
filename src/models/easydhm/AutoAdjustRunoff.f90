subroutine AutoAdjustRunoff(CurRunoffGenType,ipid)
        use SolutionMod
        use HydroDataMod
        use HydroObservedMod
!        use DBSQLLinkMod
        use ST_PR_OutMod
        use ParamRangeMod

        implicit none
        integer CurRunoffGenType,ipid
        integer i,j,m_qms,hydate
        character (200) :: PostString
!        INTEGER (SQLHANDLE) :: hSQLStmt
        real Qsimavg,Qobsavg
        integer Nsim,Nobs
        

        !dcb1
!        iRet=SQLALLocHandle(SQL_HANDLE_Stmt,hSQLDbc,hSQLStmt)
!        Qsimavg = 0.
!        Qobsavg = 0.
!        Nsim = 0 
!        Nobs = 0
!        do i = 1,NAllSeries
!            if(NAllSeries >7) then
!                if(i>6) then
!                    if(AdjustRunoff(i,2) > 0.001) then
!                        if(i>(NAllSeries-4) .and. AdjustRunoff(i,1)<0.01 .and. Nobs>0) then
!                            if((AdjustRunoff(i-1,2) -AdjustRunoff(i,2)) > (AdjustRunoff(i-2,2) - AdjustRunoff(i-1,2))) then
!                                AdjustRunoff(i,2) = AdjustRunoff(i-1,2) - AdjustRunoff(i-1,2) * (AdjustRunoff(i-2,2) - AdjustRunoff(i-1,2))/ AdjustRunoff(i-2,2)
!                            endif
!                        endif
!                        Qsimavg = Qsimavg + AdjustRunoff(i,2)
!
!                        Nsim = Nsim + 1
!                    endif
!                    if(AdjustRunoff(i,1) > 0.001) then
!                        Qobsavg = Qobsavg + AdjustRunoff(i,1)
!                        Nobs= Nobs + 1
!                    endif
!                endif
!            endif
!        enddo 
!        Qsimavg = Qsimavg / Nsim
!        Qobsavg = Qobsavg / Nobs

!        if(Qobsavg >20) then
!            do i = 1,NAllSeries
!                AdjustRunoff(i,2) = max(30.0,AdjustRunoff(i,2) + Qobsavg - Qsimavg)
!            enddo 
!        endif
        if (CurSolution.RtmdyType == 2) then
            call RtMdyPar_AR(AdjustRunoff(1:NAllSeries,1:3),CurSolution.AR_num,NAllSeries,Cursolution.RtMdy)
        endif   
            
        do i=1,NAllSeries
!            write(PostString,100) "update ST_PR set [Q(m3/s)]=",AdjustRunoff(i,3)," where sid = ",Cursolution.SolutionID,     &
!                                   " and IRunoffGenType = ",CurRunoffGenType," and pid = ",Cursolution.IParamRange," and TM = '",trim(adjustl(HydroObserveds(i).datetime)),"'",char(0)
!            iRet = SQLExecDirect( hSQLStmt, PostString , SQL_NTSL )
!            write(PostString,100) "update ST_PR set [Q(m3/s)]=",AdjustRunoff(i,3)," where sid = ",Cursolution.SolutionID,     &
!                                   " and IRunoffGenType = ",0," and pid = ",Cursolution.IParamRange," and TM = '",trim(adjustl(HydroObserveds(i).datetime)),"'",char(0)
!            iRet = SQLExecDirect( hSQLStmt, PostString , SQL_NTSL )
            j =i+(ipid-1)*NAllSeries
!            if(NAllSeries > 5) then
!                if(i>(NAllSeries-4) .and. AdjustRunoff(i,1)<0.01) then
!                    if(AdjustRunoff(i,3) > AdjustRunoff(i-1,3)) AdjustRunoff(i,3) = AdjustRunoff(i-1,3)*0.98
!                    
!                endif
!            endif
            outST_PR(j).Qms = AdjustRunoff(i,3)
            outST_PR(j).PR_Rsimms = AdjustRunoff(i,2)
            outST_PR(j).PR_Rsimmm =  AdjustRunoff(i,3)*Cursolution.dt*1000*3600/ ParamRanges(Cursolution.IParamRange).UpArea
!            if(AdjustRunoff(i,1)<=0.001) then
            outST_PR(j).QFinal = AdjustRunoff(i,3)
!            endif
        enddo
!100     Format(a,F10.4,a,i,a,i,a,i,a,a19,a,a)  
! 	    iRet = SQLFreeHandle( SQL_HANDLE_STMT, hSQLStmt)  

    end subroutine
    
    
	subroutine RtMdyPar_AR(QAll,AR_num,n,RtMdy)
        real ErrorAR_Sim,Qerr1, QerrForecast
        integer AR_num,n,RtMdy
        integer ::L(AR_num)
        real Qobs(n),Qfore(n),Qobs2(n),Qerr(n),Rtotal(n),RR(AR_num),Q(n),r(AR_num),A(AR_num,AR_num),S(AR_num),X(AR_num),QerrT(AR_num),QAll(n,3)
        
        Qerr1=0
        QerrForecast=0
        Qobs = QAll(:,1)
        Qfore = QAll(:,2)
        
        !求解AR模型校正系数
        
        do i=1,n
            if (Qobs(i) .le.0.001) then
                Qerr(i)=0.
            else
                Qerr(i)=Qobs(i)-Qfore(i)
            endif
        enddo

        Rtotal=0
        RR=0
        r=0 

        do i=1,n
            Qerr1=Qerr1+Qerr(i)**2
            E=Qerr1/n
        enddo
        
        if (E == 0) then
            X = 0
            goto  1111
        endif  

        do j=1,AR_num 
            do i=1,n-j                
                Rtotal(j)=Rtotal(j)+Qerr(i)*Qerr(i+j)
                RR(j)=Rtotal(j)/(n-j) 
                r(j)=RR(j)/E      
            enddo    
        enddo

        do i=1,AR_num
            do j=i,AR_num
                if (i==j) then
                    A(i,j)=1
                else 
                    A(i,j)=r(j-i)
                endif
            enddo      
        enddo

        do j=1,AR_num
            do i=j,AR_num
                A(i,j)=A(j,i)
            enddo
        enddo

        CALL TSTGAUS(AR_num,A,AR_num,L,S,r,X) 
 
 1111   continue       
        do i=1,n
            Q(i)=Qfore(i)
        enddo
        
        do i=1,n
            if (Qobs(i) .le.0.001) then
                Qerr(i)=Q(i)-Qfore(i)
            else
                Qerr(i)=Qobs(i)-Qfore(i)
            endif
        enddo    
                 
        do i=AR_num+1,n                
            do j=1,AR_num
                !用上一时段的校正值代替作为实测值
                if (Qobs(i-j) .le.0.001) then
                    if (i-j-1>=1) then
                        if (Qobs(i-j-1) .gt.0.001) then
!                            QerrForecast=Q(i-j-1)-Qfore(i-j-1)
                            QerrForecast=Qobs(i-j-1)-Qfore(i-j)
                        else
                            QerrForecast=Q(i-j)-Qfore(i-j)
                        endif
                    endif
                    QerrT(j)=QerrForecast
                
                !直接用实测值
                else
                    QerrT(j)=Qerr(i-j)                
                endif
            enddo
            if(abs(ErrorAR_Sim(AR_num,X,QerrT))>Q(i)) then
                if(Qobs(i) .le. 0.001) then
                    Q(i)=Q(i)
                else
                    Q(i)=Qobs(i)
                endif
            else
                Q(i)=max(Q(i)+ErrorAR_Sim(AR_num,X,QerrT),0.0)   
            endif
            if(i>1) then
                if((Q(i) - Q(i-1)) > (Qfore(i) - Q(i-1))) then
                    Q(i) = Qfore(i)
                endif
                if(Qobs(i) .le. 0.001) then
                    if((Qfore(i) - Qfore(i-1))*(Q(i) - Q(i-1))<0) then
                        if(Qfore(i) > 0) then
                            Q(i) = Q(i-1) * Qfore(i) / Qfore(i-1)
                        else
                            Q(i) = Q(i-1) * 0.98 
                        endif
                    endif
                endif
            endif     
        enddo
        
        call SmoothQ(n, Q)
        call SmoothQ(n, Q)
        QAll(:,3) = Q
        return      
    end
    
    real function ErrorAR_Sim(m,X,QerrT)
                 
        integer m,j
        DIMENSION  QerrT(m),X(m)
        
        ret=0
        do j=1,m
            ret=ret+X(j)*QerrT(j)
        enddo
        
        ErrorAR_Sim = ret
        return
    end
    
    subroutine SmoothQ(n, Q)
        integer n, i, j, ns, ns2
        real qtmp
        dimension Q(n)
        
        ! 光滑阶数，一定为奇数
        ns=3
        ns2= (ns-1)/2
        
        do i=1,n
            if (i>=3 .and. i<=n-3) then
                qtmp=0
                do j=i-ns2, i+ns2
                    qtmp=qtmp+Q(j)
                enddo
                Q(i)=qtmp/ns
            else
                Q(i)=Q(i)
            endif
        enddo
        return
    end
    
      SUBROUTINE TSTGAUS(N,A,IA,L,S,B,X)
      DIMENSION  A(IA,N),B(N),X(N),S(N),L(N)
!      PRINT 10,((A(I,J),J=1,N),I=1,N) 
!      PRINT 10,(B(I),I=1,N) 
      CALL GAUSS(N,A,IA,L,S)
      CALL SOLVE(N,A,IA,L,B,X)
!      PRINT 10,(X(I),I=1,N) 
      RETURN
10    FORMAT(5X,5(F10.5,2X))
      END 
  
      SUBROUTINE GAUSS(N,A,IA,L,S)    
      DIMENSION  A(IA,N),L(N),S(N)    
      DO 3 I = 1,N
        L(I) = I  
        SMAX = 0.0
        DO 2 J = 1,N
          SMAX = AMAX1(SMAX,ABS(A(I,J)))
   2    CONTINUE  
        S(I) = SMAX 
   3  CONTINUE    
      DO 7 K = 1,N-1
        RMAX = 0.0
        DO 4 I = K,N
          R = ABS(A(L(I),K))/S(L(I))  
          IF(R .LE. RMAX)  GO TO 4    
          J = I   
          RMAX = R
   4    CONTINUE  
        LK = L(J) 
        L(J) = L(K) 
        L(K) = LK 
        DO 6 I = K+1,N      
          XMULT = A(L(I),K)/A(LK,K)   
          DO 5 J = K+1,N    
            A(L(I),J) = A(L(I),J) - XMULT*A(LK,J) 
   5      CONTINUE
          A(L(I),K) = XMULT 
   6    CONTINUE  
   7  CONTINUE    
      RETURN      
      END 
  
      SUBROUTINE SOLVE(N,A,IA,L,B,X)  
      DIMENSION  A(IA,N),L(N),B(N),X(N) 
      DO 3 K = 1,N-1
        DO 2 I = K+1,N      
          B(L(I)) = B(L(I)) - A(L(I),K)*B(L(K)) 
   2    CONTINUE
   3  CONTINUE    
      X(N) = B(L(N))/A(L(N),N)
      DO 5 I = N-1,1,-1     
        SUM = B(L(I))       
        DO 4 J = I+1,N      
          SUM = SUM - A(L(I),J)*X(J)  
   4    CONTINUE  
        X(I) = SUM/A(L(I),I)
   5  CONTINUE    
      RETURN      
      END 