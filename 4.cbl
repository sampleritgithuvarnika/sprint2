       IDENTIFICATION DIVISION.                   
       PROGRAM-ID.     A20PGM4                    
       ENVIRONMENT DIVISION.                      
       DATA DIVISION.                             
       WORKING-STORAGE SECTION.                   
       COPY A20MPS1.                              
       COPY DFHAID.                               
      *                                           
           EXEC SQL                               
             INCLUDE CUS1                       
           END-EXEC.                              
      *                                           
           EXEC SQL                               
             INCLUDE SQLCA                        
           END-EXEC.                              
       77 WS-ABS-TIME      PIC S9(15) COMP.     
       77 WS-SEP           PIC X VALUE '-'.     
       77 WS-TSEP          PIC X VALUE ':'.     
       77 WS-SQLCODE       PIC -9(04).          
       01 WS-DATA          PIC X(6).            
       01 WS-COMM-AREA.                         
          05 WS-DATA          PIC X(10).        
          05 WS-DATE          PIC X(10).        
          05 WS-TIME          PIC X(08).                
          05 WS-QUANTITY      PIC 9(03).        
          05 WS-PROCODE       PIC X(06).  
          05 WS-CUSTID        PIC X(06).        
      *                                         
       LINKAGE SECTION.                         
       01 DFHCOMMAREA.                          
          05 LK-COMM-AREA.                      
            10 LK-DATA          PIC X(10).         
            10 LK-DATE          PIC X(10).         
            10 LK-TIME          PIC X(08).                  
            10 LK-QUANTITY      PIC 9(03).         
            10 LK-PROCODE       PIC X(6).   
            10 LK-CUSTID        PIC X(6).         
       PROCEDURE DIVISION.                         
       MAIN-PARA.                                  
           IF EIBCALEN = 0                         
              MOVE LOW-VALUES TO  ORDIN3O        
              PERFORM 2000-SEND-PARA               
              PERFORM 8888-RETURN-TRANS-PARA       
           ELSE                                    
              MOVE LK-COMM-AREA TO WS-COMM-AREA    
              PERFORM 7777-KEY-CHECK-PARA          
           END-IF.                                 
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
       2000-SEND-PARA.   
           MOVE WS-DATE TO ORDD3O
           MOVE WS-TIME TO ORDT3O
           PERFORM 1000-GET-DATE                         
           EXEC CICS                                     
                SEND MAP('ORDIN3') MAPSET('A20MPS1')    
                ERASE                                   
           END-EXEC.                                    
      *                                                 
       7777-KEY-CHECK-PARA.                             
           EVALUATE EIBAID                              
            WHEN DFHENTER 
              MOVE LOW-VALUES TO ORDIN3O
              PERFORM 3000-RECIEVE-PARA                 
              PERFORM 4000-VALIDATE-PARA                
            WHEN DFHPF1               
              MOVE LOW-VALUES TO ORDIN3O
              PERFORM 3000-RECIEVE-PARA                 
              PERFORM 1200-PRODUCT-CODE-CHECK-PARA      
            WHEN DFHPF3                                 
              MOVE LOW-VALUES TO  ORDIN3O            
              MOVE 'YOU ARE LOGGED OUT' TO ORDM1O  
              PERFORM 2000-SEND-PARA                    
              PERFORM 9999-RETURN-PARA                                  
            WHEN DFHPF5                                   
              MOVE LOW-VALUES TO ORDIN3O                
              PERFORM 2000-SEND-PARA                      
              PERFORM 8888-RETURN-TRANS-PARA              
            WHEN OTHER                                    
              PERFORM 3000-RECIEVE-PARA                   
              MOVE ' ENTER VALID KEY' TO ORDM1O        
              PERFORM 2000-SEND-PARA                      
              PERFORM 8888-RETURN-TRANS-PARA              
           END-EVALUATE.                                  
       3000-RECIEVE-PARA.                                 
           EXEC CICS                                      
                RECEIVE MAP('ORDIN3') MAPSET('A20MPS1')  
           END-EXEC.                                      
      *                                                   
       4000-VALIDATE-PARA.                                
           IF ORDNM3L > 0                                      
             IF (  ORDNM3I(1:1) NOT ALPHABETIC AND             
                 ORDNM3I(2:5) NOT  NUMERIC )                   
                  MOVE 'ENTER VALID CUSTOMER ID' TO ORDM1O    
                  PERFORM 2000-SEND-PARA                       
             ELSE                                              
                 PERFORM 4100-GET-CUSTOMER-PARA                
           END-IF.                                             
       4100-GET-CUSTOMER-PARA.                                 
      *    INITIALIZE  DCLACCOUNT                              
           MOVE ORDNM3I TO CUSTOMER-ID-TEXT                    
           MOVE LENGTH OF ORDNM3I TO CUSTOMER-ID-LEN           
           EXEC  SQL                                           
              SELECT CUSTOMER_ID,                              
                     CUSTOMER_NAME,                            
                     CONTACT,                                  
                     ADDRESS1                              
              INTO  :CUSTOMER-ID,                                 
                    :CUSTOMER-NAME,                               
                    :CONTACT,                                     
                    :ADDRESS1                                 
              FROM CUSTOMER1                                      
              WHERE CUSTOMER_ID = :CUSTOMER-ID                    
           END-EXEC                                               
           EVALUATE  SQLCODE                                      
             WHEN 0                                               
                MOVE   CUSTOMER-ID-TEXT(1:CUSTOMER-ID-LEN)        
                                  TO ORDNM3O, WS-CUSTID                     
                MOVE   CUSTOMER-NAME-TEXT(1:CUSTOMER-NAME-LEN)    
                                   TO ORDNAO                      
                MOVE   CONTACT     TO ORDNUMO                     
                MOVE   ADDRESS1-TEXT(1:ADDRESS1-LEN)      
                                   TO ORDADDO                     
             WHEN 100                                             
                MOVE 'NO CUSTOMER FOUND '   TO ORDM1O    
             WHEN OTHER                                   
                MOVE  SQLCODE  TO WS-SQLCODE              
                STRING 'SQL ERROR '   DELIMITED BY SIZE   
                       WS-SQLCODE     DELIMITED BY SIZE   
                       INTO  ORDM1O                     
           END-EVALUATE.                                  
           PERFORM 2000-SEND-PARA                         
           PERFORM 8888-RETURN-TRANS-PARA.                
       1200-PRODUCT-CODE-CHECK-PARA.                      
                        MOVE ORDNM3I TO WS-CUSTID         
                        EXEC CICS                         
                           XCTL PROGRAM('A20PGM5')        
                           COMMAREA (WS-COMM-AREA)        
                        END-EXEC .                        
       8888-RETURN-TRANS-PARA.                            
           EXEC CICS                                      
                RETURN                         
                TRANSID('A204')                
                COMMAREA(WS-COMM-AREA)         
           END-EXEC.                                              
       9999-RETURN-PARA.                       
           EXEC CICS                           
                RETURN                         
           END-EXEC.        
