       IDENTIFICATION DIVISION.                                       
       PROGRAM-ID.    A20PGM5.                                        
      * PGM FOR SCREENS                                               
       ENVIRONMENT DIVISION.                                          
       DATA DIVISION.                                                 
       WORKING-STORAGE SECTION.                                       
       COPY DFHAID.                                                   
       COPY A20MPS1.                                                  
      *                                                               
           EXEC SQL                                                   
             INCLUDE SQLCA                                            
           END-EXEC.                                                  
      *                                                               
           EXEC SQL                                                   
             INCLUDE PROD1                                            
           END-EXEC.                                                  
       77 WS-ABS-TIME     PIC S9(15) COMP.                            
       77 WS-SEP          PIC X(01) VALUE '-'.                        
       77 WS-TSEP         PIC X(01) VALUE ':'.                        
       77 WS-SQLCODE      PIC -9(04).                                 
       01 WS-COMM.                                                    
         05 WS-DATA       PIC X(10).                                  
         05 WS-DATE       PIC X(10).                                  
         05 WS-TIME       PIC X(08).                                                             
         05 WS-QUANTITY   PIC 9(03).                                   
         05 WS-PROCODE    PIC X(06).  
         05 WS-CUSTID     PIC X(06).                           
       01 WS-PROD          PIC X(6).                                   
       01 WS-NAME          PIC X(10). 
       01 WS-STOCK-AVAILABILITY PIC 9(03).      
      *                                                                
       LINKAGE SECTION.                                                
       01 DFHCOMMAREA.                                                 
          05 LK-COMM.                                                  
            10 LK-DATA       PIC X(10).                                
            10 LK-DATE       PIC X(10).                                
            10 LK-TIME       PIC X(08).                                                       
            10 LK-QUANTITY   PIC 9(03).                                
            10 LK-PROCODE    PIC X(06).                                
            10 LK-CUSTID     PIC X(06).                     
      *                                                                
       PROCEDURE DIVISION.                                             
       0000-MAIN-PARA.                                                 
            IF EIBCALEN = 0                                            
              MOVE LOW-VALUES TO ORDINO                                
              PERFORM 1000-GET-DATE                                    
              PERFORM 1000-SEND-SCREEN1-PARA                           
              MOVE 'ORDER' TO WS-DATA                                  
              PERFORM 4000-RETURN-TRANS-PARA                           
            ELSE                                                       
               MOVE LK-COMM TO WS-COMM                                 
               PERFORM 1100-KEY-CHECK-MENU-PARA                        
            END-IF.                                                    
      *                                                                
       1000-GET-DATE.                                                  
            EXEC CICS                                                  
               ASKTIME                                                 
               ABSTIME(WS-ABS-TIME)                                    
            END-EXEC.                                                  
      *                                                                
            EXEC CICS FORMATTIME                                       
                 ABSTIME(WS-ABS-TIME)                                  
                 DATESEP(WS-SEP)                                       
                 YYYYMMDD(WS-DATE)                                     
                 TIME(WS-TIME)                                         
                 TIMESEP(WS-TSEP)                                      
            END-EXEC.                                                  
      *                                                                
       1000-SEND-SCREEN1-PARA.                                         
            MOVE WS-DATE TO ORDDATEO.                                  
            MOVE WS-TIME TO ORDTIMEO.                                  
            EXEC CICS                                                  
                SEND MAP('ORDIN')                                      
                MAPSET('A20MPS1')                                      
                ERASE                                                  
            END-EXEC.                                                  
       2000-RECV-SCREEN1-PARA.                                         
            EXEC CICS                                                  
               RECEIVE MAP('ORDIN')                                    
                       MAPSET('A20MPS1')                               
            END-EXEC.                                                  
       1100-KEY-CHECK-MENU-PARA.                                       
            EVALUATE EIBAID                                            
              WHEN DFHENTER                                            
               PERFORM 2000-RECV-SCREEN1-PARA                          
               PERFORM 3000-READ-PARA   
              WHEN DFHPF2
                MOVE ORDQUANI  TO WS-QUANTITY
                MOVE ORDPRCDI  TO WS-PROCODE
                EXEC CICS
                   XCTL PROGRAM('A20PGM6')
                   COMMAREA(WS-COMM)
                END-EXEC
              WHEN DFHPF3                                              
               MOVE LOW-VALUES TO ORDINO                               
               MOVE 'YOU ARE LOGGED OUT   ' TO ORDMSG4O                
               PERFORM 1000-SEND-SCREEN1-PARA                          
               PERFORM 9999-RETURN-PARA                                                                            
              WHEN DFHPF5                                              
               MOVE LOW-VALUES TO ORDINO                               
               PERFORM 1000-SEND-SCREEN1-PARA                          
               PERFORM 4000-RETURN-TRANS-PARA                          
              WHEN OTHER                                               
               MOVE 'INVALID KEY PRESSED' TO ORDMSG4O                  
               PERFORM 1000-SEND-SCREEN1-PARA                          
               PERFORM 4000-RETURN-TRANS-PARA                          
            END-EVALUATE.                                              
       3000-READ-PARA.                                                 
      *                                                                
           INITIALIZE DCLPRODUCT1                                      
           IF   ORDPRCDI NOT = SPACES                                  
                MOVE SPACES   TO  WS-PROD                              
             ELSE                                                      
                MOVE ORDPRCDI TO WS-PROD                               
           END-IF                                                      
           MOVE ORDPRCDI TO PRODUCT-CODE-TEXT                          
           MOVE LENGTH OF ORDPRCDI TO PRODUCT-CODE-LEN                 
           MOVE ORDNAMEI TO PRODUCT-NAME-TEXT                          
           MOVE LENGTH OF ORDNAMEI TO PRODUCT-NAME-LEN                 
           EXEC SQL                                                    
             SELECT PRODUCT_CODE,                                      
                    PRODUCT_NAME,                                      
                    STOCK_AVAILABILITY                                 
              INTO :PRODUCT-CODE,                                      
                   :PRODUCT-NAME,                                      
                   :STOCK-AVAILABILITY                                 
              FROM PRODUCT1                                            
              WHERE PRODUCT_CODE = :PRODUCT-CODE                       
                     OR PRODUCT_NAME = :PRODUCT-NAME                   
           END-EXEC.                                                   
           EVALUATE SQLCODE                                            
           WHEN 0                                                      
             MOVE PRODUCT-CODE-TEXT(1:PRODUCT-CODE-LEN) TO ORDPRCDO    
             MOVE PRODUCT-NAME-TEXT(1:PRODUCT-NAME-LEN) TO ORDNAMEO    
             MOVE SPACES TO ORDMSG4O                                   
             PERFORM 4000-VALIDATE-PARA                                
             PERFORM 4000-VALIDATE1-PARA                               
           WHEN 100                                                    
             MOVE 'NO PRODUCT-CODE EXISTS' TO ORDMSG4O                 
           WHEN OTHER                                                  
             MOVE SQLCODE TO WS-SQLCODE                                
             STRING 'SQL ERROR' DELIMITED BY SIZE                      
                    WS-SQLCODE  DELIMITED BY SIZE                      
             INTO ORDMSG4O                                             
           END-EVALUATE.                                               
           PERFORM 1000-SEND-SCREEN1-PARA.                             
           PERFORM 4000-RETURN-TRANS-PARA.                             
       4000-VALIDATE-PARA.                                             
           IF ORDPRCDO(1:1) = 'P' AND ORDPRCDO(2:4) IS NUMERIC         
              MOVE 'PRODUCT-CODE EXISTS,ENTER QUANTITY' TO ORDMSG4O    
              PERFORM 1000-SEND-SCREEN1-PARA                           
              PERFORM 6666-CUS-PARA                                    
           ELSE                                                        
              MOVE 'ENTER VALID CODE' TO ORDMSG4O                      
              PERFORM 1000-SEND-SCREEN1-PARA                           
              PERFORM 4000-RETURN-TRANS-PARA                           
           END-IF.                                                     
       4000-VALIDATE1-PARA.                                            
           IF ORDNAMEO IS ALPHABETIC                                   
              MOVE 'PRODUCT-CODE EXISTS,ENTER QUANTITY' TO ORDMSG4O    
              PERFORM 1000-SEND-SCREEN1-PARA                           
              PERFORM 6666-CUS-PARA                                                                 
           ELSE                                                        
              MOVE 'ENTER VALID NAME' TO ORDMSG4O                      
              PERFORM 1000-SEND-SCREEN1-PARA                           
              PERFORM 4000-RETURN-TRANS-PARA                           
           END-IF.                                                     
      *                                                                
       6666-CUS-PARA.                                                  
             MOVE STOCK-AVAILABILITY TO WS-STOCK-AVAILABILITY          
             IF WS-STOCK-AVAILABILITY >= ORDQUANI                      
                MOVE 'STOCK IS AVAILABLE' TO ORDMSG4O                  
             ELSE                                                      
                MOVE 'STOCK NOT AVAILABLE' TO ORDMSG4O                 
             END-IF                                                    
             PERFORM 1000-SEND-SCREEN1-PARA                            
             PERFORM 4000-RETURN-TRANS-PARA.                                                                                              
       4000-RETURN-TRANS-PARA.                                         
            EXEC CICS                                                  
                 RETURN                                                
                 TRANSID('A205')                                       
                 COMMAREA(WS-COMM)                                     
            END-EXEC.                                                  
       9999-RETURN-PARA.                                           
            EXEC CICS                                              
                 RETURN                                            
            END-EXEC.        
