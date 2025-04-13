       IDENTIFICATION DIVISION.                         
       PROGRAM-ID.    A20PGM1.                          
       ENVIRONMENT DIVISION.                            
       DATA DIVISION.                                   
       WORKING-STORAGE SECTION.                         
       COPY DFHAID.                                     
       COPY A20MPS1.                                    
      *                                                 
       77 WS-ABS-TIME     PIC S9(15) COMP.              
       77 WS-SEP          PIC X(01) VALUE '-'. 
       77 WS-TIME-SEP     PIC X(01) VALUE ':'. 
       77 WS-SQLCODE      PIC -9(04).
       01 WS-COMM.                                      
         05 WS-DATA          PIC X(10).                 
         05 WS-DATE          PIC X(10). 
         05 WS-TIME          PIC X(08).
      *                                                 
       LINKAGE SECTION.                                 
       01 DFHCOMMAREA.                                  
          05 LK-COMM.                                       
            10 LK-DATA       PIC X(10).                     
            10 LK-DATE       PIC X(10). 
            10 LK-TIME       PIC X(08).
      *                                                     
       PROCEDURE DIVISION.                                  
       0000-MAIN-PARA.                                                        
            IF EIBCALEN = 0                                 
              MOVE LOW-VALUES TO PRODINO                     
              PERFORM 1000-SEND-PARA                   
              MOVE 'LOGIN' TO WS-DATA                       
              PERFORM 4000-RETURN-TRANS-PARA                
            ELSE                                            
               MOVE LK-COMM TO WS-COMM                      
               PERFORM 1100-KEY-CHECK-MENU-PARA             
            END-IF.                                         
       1000-GET-DATE.                                 
            EXEC CICS                                 
               ASKTIME                                
               ABSTIME(WS-ABS-TIME)                   
            END-EXEC.                                 
            EXEC CICS ASKTTIME                      
                 ABSTIME(WS-ABS-TIME)
            END-EXEC.
            EXEC CICS FORMATTIME
                 ABSTIME(WS-ABS-TIME)
                 DATESEP(WS-SEP)                      
                 YYYYMMDD(WS-DATE) 
                 TIMESEP(WS-TIME-SEP)
                 TIME(WS-TIME)                       
            END-EXEC.                                 
       1000-SEND-PARA.                           
            PERFORM 1000-GET-DATE.
            MOVE WS-DATE TO PRODATEO
            MOVE WS-TIME TO PROTIMEO
            EXEC CICS                                 
                SEND MAP('PRODIN')                     
               MAPSET('A20MPS1')                      
               ERASE                                  
            END-EXEC.                                  
       2000-RECV-PARA.                            
            EXEC CICS                                  
               RECEIVE MAP('PRODIN')                    
                       MAPSET('A20MPS1')               
            END-EXEC.                                  
      *                                                
       1100-KEY-CHECK-MENU-PARA.                       
            EVALUATE EIBAID                            
              WHEN DFHENTER                              
                  PERFORM 2000-RECV-PARA             
              WHEN DFHPF1
                  EXEC CICS
                      XCTL PROGRAM('A20PGM2')
                  END-EXEC                             
              WHEN DFHPF2        
                  EXEC CICS
                      XCTL PROGRAM('A20PGM3')
                  END-EXEC                                                           
              WHEN DFHPF4  
                  EXEC CICS
                      XCTL PROGRAM('A20PGM4')
                  END-EXEC                                                   
              WHEN DFHPF6
                  EXEC CICS
                      XCTL PROGRAM('A20PGM7')
                  END-EXEC 
              WHEN DFHPF3
                MOVE 'LOW-VALUES' TO PRODINO
                MOVE 'YOU ARE LOGGED OUT' TO PROMSO   
                PERFORM 1000-SEND-MENU-PARA            
                PERFORM 9999-RETURN-PARA   
              WHEN DFHPF5
                MOVE 'LOW-VALUES' TO PRODINO
                PERFORM 1000-SEND-MENU-PARA            
                PERFORM 9999-RETURN-TRANS-PARA                  
              WHEN OTHER                               
                MOVE 'INVALID KEY PRESSED' TO PROMSO  
                PERFORM 1000-SEND-PARA            
                PERFORM 4000-RETURN-TRANS-PARA         
            END-EVALUATE.                              
       4000-RETURN-TRANS-PARA.                         
            EXEC CICS                                  
                 RETURN                                
                 TRANSID('A201')                       
                 COMMAREA(WS-COMM)                     
            END-EXEC.                                                                
       9999-RETURN-PARA.     
           EXEC CICS         
               RETURN        
           END-EXEC.        
