       IDENTIFICATION DIVISION.                                       
       PROGRAM-ID.     A20PGM3.                                       
      *UPDATE RECORDS IN DB2                                          
       ENVIRONMENT DIVISION.                                          
       DATA DIVISION.                                                 
       WORKING-STORAGE SECTION.                                       
       COPY A20MPS1.                                                  
       COPY DFHAID.                                                   
      *                                                               
           EXEC SQL                                                   
             INCLUDE PROD1                                            
           END-EXEC.                                                  
      *                                                               
           EXEC SQL                                                   
             INCLUDE SQLCA                                            
           END-EXEC.                                                  
       77 WS-ABS-TIME      PIC S9(15) COMP.                           
       77 WS-SEP           PIC X VALUE '-'.                           
       77 WS-TIME-SEP      PIC X(01) VALUE ':'.                        
       77 WS-SQLCODE       PIC -9(04).                                
       01 WS-AMT-NUM       PIC 9(4)V9(2).                             
       01 WS-COMM-AREA.                                               
          05 WS-DATA       PIC X(05).                                 
          05 WS-DATE       PIC X(10).
          05 WS-TIME       PIC X(08).
          05 WS-PRODID     PIC X(06).                                
      *                                                              
       LINKAGE SECTION.                                              
       01 DFHCOMMAREA.                                               
          05 LK-COMM-AREA.                                           
            10 LK-DATA     PIC X(05).                                
            10 LK-DATE     PIC X(10). 
            10 LK-TIME     PIC X(08). 
            10 LK-PRODID   PIC X(06).                                
       PROCEDURE DIVISION.                                           
       MAIN-PARA.                                                    
           IF EIBCALEN = 0                                           
              MOVE LOW-VALUES TO  PRODIN2O                           
              PERFORM 2000-SEND-PARA                                 
              MOVE 'UPDATE ' TO WS-DATA                              
              PERFORM 8888-RETURN-TRANS-PARA                         
      *                                                              
           ELSE                                                      
              MOVE LK-COMM-AREA TO WS-COMM-AREA                      
              PERFORM 7777-KEY-CHECK-PARA                            
           END-IF.                                                   
      *                                                              
       1000-GET-DATE.                                                
            EXEC CICS                                                
               ASKTIME                                                 
               ABSTIME(WS-ABS-TIME)                                    
            END-EXEC.                                                  
            EXEC CICS FORMATTIME                                       
                 ABSTIME(WS-ABS-TIME)                                  
                 DATESEP(WS-SEP)                                       
                 YYYYMMDD(WS-DATE)                                     
                 TIME(WS-TIME)                                        
                 TIMESEP(WS-TIME-SEP)                                  
            END-EXEC.                                                  
       2000-SEND-PARA. 
           MOVE WS-DATE TO PRODTEO
           MOVE WS-TIME TO PROTMEO
           PERFORM 1000-GET-DATE.                                      
           EXEC CICS                                                   
                SEND MAP('PRODIN2') MAPSET('A20MPS1')                  
                ERASE                                                  
           END-EXEC.                                                   
       7777-KEY-CHECK-PARA.                                            
           EVALUATE EIBAID                                             
            WHEN DFHENTER                                              
              PERFORM 3000-RECIEVE-PARA                                
              PERFORM 4000-VALIDATE-PARA                               
            WHEN DFHPF2                                                
              PERFORM 3000-RECIEVE-PARA
              PERFORM 4010-VALIDATE1-PARA
            WHEN DFHPF4
                EXEC CICS
                    XCTL PROGRAM('A20PGM1')
                END-EXEC
            WHEN DFHPF6
                EXEC CICS
                    XCTL PROGRAM('A20PGM4')
                END-EXEC
            WHEN DFHPF3
              MOVE LOW-VALUES TO PRODIN2O 
              MOVE 'YOU ARE LOGGED OUT' TO PROMSGO 
              PERFORM 2000-SEND-PARA                           
              PERFORM 9999-RETURN-PARA                                
            WHEN DFHPF5                                               
              MOVE LOW-VALUES TO PRODIN2O                             
              PERFORM 2000-SEND-PARA                                  
              PERFORM 8888-RETURN-TRANS-PARA                          
            WHEN OTHER                                                
              PERFORM 3000-RECIEVE-PARA                               
              MOVE ' ENTER VALID KEY' TO PROMSGO                      
              PERFORM 2000-SEND-PARA                                  
              PERFORM 8888-RETURN-TRANS-PARA                          
           END-EVALUATE.                                              
       3000-RECIEVE-PARA.                                             
           EXEC CICS                                                  
                RECEIVE MAP('PRODIN2') MAPSET('A20MPS1')              
           END-EXEC.                                                  
       4000-VALIDATE-PARA.                                            
           IF PROCODEI(1:1) = 'P' AND PROCODEI(2:5) IS NUMERIC        
             PERFORM 4100-GET-CUSTOMER-PARA                           
           ELSE                                                       
             MOVE 'PRODUCT ID IS NOT VALID   ' TO PROMSGO             
             PERFORM 2000-SEND-PARA                                   
             PERFORM 8888-RETURN-TRANS-PARA                           
           END-IF.                                                    
      * 
       4100-GET-CUSTOMER-PARA.                                        
            MOVE PROCODEI TO PRODUCT-CODE-TEXT                        
            MOVE LENGTH  OF PROCODEI TO PRODUCT-CODE-LEN              
           EXEC  SQL                                                  
              SELECT PRODUCT_CODE,                                    
                     PRODUCT_NAME,                                    
                     PRODUCT_DESC,                                    
                     PRODUCT_PRICE,                                   
                     STOCK_AVAILABILITY,                              
                     REGISTER_DATE,                                   
                     PROD_STATUS                                      
              INTO  :PRODUCT-CODE,                                    
                    :PRODUCT-NAME,                                    
                    :PRODUCT-DESC,                                    
                    :PRODUCT-PRICE,                                   
                    :STOCK-AVAILABILITY,                              
                    :REGISTER-DATE,                                   
                    :PROD-STATUS                                      
              FROM PRODUCT1                                           
              WHERE PRODUCT_CODE = :PRODUCT-CODE                      
           END-EXEC                        
           EVALUATE  SQLCODE                                          
             WHEN 0                                                   
                MOVE   PRODUCT-CODE-TEXT (1:PRODUCT-CODE-LEN)         
                                     TO  PROCODEO                     
                MOVE   PRODUCT-NAME-TEXT(1:PRODUCT-NAME-LEN)          
                                     TO PRONAMEO                      
                MOVE   PRODUCT-DESC-TEXT(1:PRODUCT-DESC-LEN)          
                                     TO PRODESCO                      
                MOVE   PRODUCT-PRICE    TO PROPRCEO                   
                MOVE   STOCK-AVAILABILITY      TO PROSTAVO            
                MOVE   REGISTER-DATE-TEXT (1:REGISTER-DATE-LEN)       
                                       TO PRORGDTO                    
                MOVE   PROD-STATUS-TEXT (1:PROD-STATUS-LEN)           
                                     TO PROSTATO                      
             WHEN 100                                                 
                MOVE 'PRODUCT NOT FOUND '   TO PROMSGO  
             WHEN OTHER                                               
                MOVE  SQLCODE  TO WS-SQLCODE                          
                STRING 'SQL ERROR '   DELIMITED BY SIZE               
                       WS-SQLCODE     DELIMITED BY SIZE               
                       INTO  PROMSGO                                  
           END-EVALUATE.                                              
           PERFORM 2000-SEND-PARA                                     
           PERFORM 8888-RETURN-TRANS-PARA.   
       4010-VALIDATE1-PARA.         
           IF PRONAMEI IS ALPHABETIC AND PRONAMEI NOT = SPACES                                 
             CONTINUE                          
           ELSE                                                       
             MOVE 'PRODUCT NAME IS NOT VALID   ' TO PROMSGO           
             PERFORM 2000-SEND-PARA                                   
             PERFORM 8888-RETURN-TRANS-PARA                           
           END-IF.                                                    
      *                                                               
           IF PRODESCI IS ALPHABETIC AND PRODESCI NOT = SPACES                                 
             CONTINUE                          
           ELSE                                                       
             MOVE 'PRODUCT DESC IS NOT VALID   ' TO PROMSGO           
             PERFORM 2000-SEND-PARA                                   
             PERFORM 8888-RETURN-TRANS-PARA                           
           END-IF.                                                    
      *                                                               
           IF PROPRCEI IS NOT = SPACES                                    
             CONTINUE                       
            ELSE                                                      
              MOVE 'PRODUCT PRICE IS NOT VALID ' TO PROMSGO           
             PERFORM 2000-SEND-PARA                                   
             PERFORM 8888-RETURN-TRANS-PARA                           
      *                                                               
           IF PROSTAVI IS NUMERIC                                     
             CONTINUE                  
            ELSE                                                      
              MOVE 'PRODUCT STOCK IS NOT VALID ' TO PROMSGO           
             PERFORM 2000-SEND-PARA                                   
             PERFORM 8888-RETURN-TRANS-PARA                           
      *                                                               
           IF PROSTATI = 'ACTIVE' OR PROSTATI = 'INACTIVE'            
             CONTINUE                   
            ELSE                                                      
              MOVE 'PRODUCT STATUS IS NOT VALID ' TO PROMSGO          
             PERFORM 2000-SEND-PARA                                   
             PERFORM 8888-RETURN-TRANS-PARA                           
      *                                                               
           IF PRORGDTI(1:4) IS NUMERIC AND PRORGDTI(5:1) = '-' AND
              PRORGDTI(6:2) IS NUMERIC AND PRORGDTI(8:1) = '-' AND
              PRORGDTI(9:2) IS NUMERIC
              PERFORM 6000-UPDATE-PARA                           
            ELSE                                                      
              MOVE 'REGISTRATION DATE IS NOT VALID ' TO PROMSGO       
             PERFORM 2000-SEND-PARA                                   
             PERFORM 8888-RETURN-TRANS-PARA.                                                  
       6000-UPDATE-PARA.                                              
      *    INITIALIZE  DCLPRODUCT1                                    
           COMPUTE WS-AMT-NUM = FUNCTION NUMVAL-C(PROPRCEI)           
           MOVE PRONAMEI TO  PRODUCT-NAME-TEXT                        
           MOVE LENGTH OF PRONAMEI TO PRODUCT-NAME-LEN                
           MOVE PRODESCI TO PRODUCT-DESC-TEXT                         
           MOVE LENGTH OF PRODESCI  TO PRODUCT-DESC-LEN               
           MOVE WS-AMT-NUM  TO PRODUCT-PRICE                           
           MOVE PROSTAVI  TO  STOCK-AVAILABILITY                       
           MOVE PROSTATI  TO  PROD-STATUS-TEXT                         
           MOVE LENGTH OF PROSTATI TO PROD-STATUS-LEN                  
           MOVE PROCODEI TO PRODUCT-CODE-TEXT                          
           MOVE LENGTH  OF PROCODEI TO PRODUCT-CODE-LEN                
           EXEC  SQL                                                   
                UPDATE PRODUCT1                                        
                    SET                                                
                    PRODUCT_NAME  = :PRODUCT-NAME,                     
                    PRODUCT_DESC  = :PRODUCT-DESC,                     
                    PRODUCT_PRICE = :PRODUCT-PRICE,                    
                    STOCK_AVAILABILITY   = :STOCK-AVAILABILITY,        
                    PROD_STATUS = :PROD-STATUS                         
                    WHERE PRODUCT_CODE = :PRODUCT-CODE                 
           END-EXEC.                                                   
           EVALUATE  SQLCODE                                           
             WHEN 0                                                    
                MOVE 'UPDATED'   TO PROMSGO                            
             WHEN 100                                                  
                MOVE 'NO RECORD FOUND'   TO PROMSGO                    
             WHEN OTHER                                                
                MOVE  SQLCODE  TO WS-SQLCODE                           
                STRING 'UPDATE  ERROR' DELIMITED BY SIZE               
                       WS-SQLCODE     DELIMITED BY SIZE               
                       INTO  PROMSGO                                  
           END-EVALUATE                                               
           PERFORM 2000-SEND-PARA                                     
           PERFORM 8888-RETURN-TRANS-PARA.                            
       8888-RETURN-TRANS-PARA.                                        
           EXEC CICS                                                  
                RETURN                                                
                TRANSID('A203')                                       
                COMMAREA(WS-COMM-AREA)                                
           END-EXEC.                                                                                             
       9999-RETURN-PARA.                                              
           EXEC CICS                                                  
                RETURN                                                
           END-EXEC.    
