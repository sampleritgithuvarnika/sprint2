       IDENTIFICATION DIVISION.                             
       PROGRAM-ID.     A20PGM2.                             
      *INSERT RECORDS IN DB2                                
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
       77 WS-SEP           PIC X(01) VALUE '-'.        
       77 WS-TIME-SEP      PIC X(01) VALUE ':'.              
       77 WS-SQLCODE       PIC -9(04).             
       01 WS-COMM-AREA.                            
          05 WS-DATA       PIC X(05).              
          05 WS-DATE       PIC X(10).
          05 WS-TIME       PIC X(08).
          05 WS-PRODID     PIC X(06).  
       01 WS-AMOUNT-NUM    PIC 9(04)V9(02).  
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
              MOVE 'INSERT ' TO WS-DATA             
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
                 TIMESEP(WS-TIME-SEP)
                 TIME(WS-TIME)                                 
            END-EXEC.                                           
       2000-SEND-PARA.                                          
           PERFORM 1000-GET-DATE.
           MOVE WS-DATE TO PRODTEO
           MOVE WS-TIME TO PROTMEO
           EXEC CICS                                            
                SEND MAP('PRODIN2') MAPSET('A20MPS1')            
                ERASE                                           
           END-EXEC.                                            
       7777-KEY-CHECK-PARA.                                     
           EVALUATE EIBAID                                      
            WHEN DFHENTER                                       
              PERFORM 3000-RECIEVE-PARA                         
      *       PERFORM 4000-VALIDATE-PARA    
            WHEN DFHPF1                                         
              MOVE LOW-VALUES TO  PROMSGO 
              PERFORM 3000-RECEIVE-PARA
              PERFORM 6000-INSERT-PARA
              PERFORM 2000-SEND-PARA                     
              PERFORM 8888-RETURN-TRANS-PARA 
            WHEN DFHPF4
                EXEC CICS
                    XCTL PROGRAM('A20PGM1')
                END-EXEC
            WHEN DFHPF3                                         
              MOVE LOW-VALUES TO  PRODIN2O                
              MOVE 'YOU ARE LOGGED OUT' TO PROMSGO       
              PERFORM 2000-SEND-PARA                     
              PERFORM 9999-RETURN-PARA                   
            WHEN DFHPF5
              MOVE LOW-VALUES TO PROMSGO
              PERFORM 2000-SEND-PARA                  
              PERFORM 8888-RETURN-TRANS-PARA                              
            WHEN OTHER                                   
              PERFORM 3000-RECIEVE-PARA                  
              MOVE ' ENTER VALID KEY' TO PROMSGO         
              PERFORM 2000-SEND-PARA                     
              PERFORM 8888-RETURN-TRANS-PARA             
           END-EVALUATE.                                 
      *                                                         
       3000-RECIEVE-PARA.                                       
           EXEC CICS                                            
                RECEIVE MAP('PRODIN2') MAPSET('A20MPS1')         
           END-EXEC.                                            
       4000-VALIDATE-PARA.                                                                                           
           IF PROCODEI(1:1) = 'P' AND PROCODEI(2:5) IS NUMERIC                              
             PERFORM 4100-GET-CUSTOMER-PARA                     
           ELSE                                                 
             MOVE 'CUST ID IS NOT NUMBER   ' TO PROMSGO         
             PERFORM 2000-SEND-PARA                             
                                                                
             PERFORM 8888-RETURN-TRANS-PARA                     
           END-IF.                                              
       4100-GET-CUSTOMER-PARA.                                  
      *    INITIALIZE  DCLACCOUNT                               
           MOVE PROCODEI   TO WS-PRODID          
           MOVE WS-PRODID   TO PRODUCT-CODE         
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
                MOVE   REGISTER-DATE     TO PRORGDTO               
                MOVE   PROD-STATUS-TEXT (1:PROD-STATUS-LEN)    
                                     TO PROSTATO               
             WHEN 100                                          
                MOVE 'NO CUSTOMER FOUND '   TO PROMSGO       
             WHEN OTHER                                      
                MOVE  SQLCODE  TO WS-SQLCODE                 
                STRING 'SQL ERROR '   DELIMITED BY SIZE      
                       WS-SQLCODE     DELIMITED BY SIZE      
                       INTO  PROMSGO                         
           END-EVALUATE.                                     
           PERFORM 2000-SEND-PARA                            
           PERFORM 8888-RETURN-TRANS-PARA.                   
       6000-INSERT-PARA.                                     
      *    INITIALIZE  DCLACCOUNT
           COMPUTE WS-AMOUNT-NUM = FUNCTION NUMVAL(PROPRCEI)
           MOVE PROCODEI TO PRODUCT-CODE-TEXT                   
           MOVE LENGTH  OF PROCODEI TO PRODUCT-CODE-LEN         
           MOVE PRONAMEI TO  PRODUCT-NAME-TEXT                  
           MOVE LENGTH OF PRONAMEI TO PRODUCT-NAME-LEN          
           MOVE PRODESCI TO PRODUCT-DESC-TEXT                   
           MOVE LENGTH OF PRODESCI  TO PRODUCT-DESC-LEN         
           MOVE WS-AMOUNT-NUM  TO PRODUCT-PRICE                  
           MOVE PROSTAVI  TO  STOCK-AVAILABILITY                   
           MOVE PRORGDTI  TO  REGISTER-DATE-TEXT
           MOVE LENGTH OF PRORGDTI  TO  REGISTER-DATE-LEN
           MOVE PROSTATI  TO  PROD-STATUS-TEXT           
           MOVE LENGTH OF PROSTATI TO PROD-STATUS-LEN    
           EXEC  SQL                                     
              INSERT INTO PRODUCT1 VALUES(                
                    :PRODUCT-CODE,                          
                    :PRODUCT-NAME,                          
                    :PRODUCT-DESC,                          
                    :PRODUCT-PRICE,                         
                    :STOCK-AVAILABILITY,                           
                    :REGISTER-DATE,                          
                    :PROD-STATUS)                        
           END-EXEC                                      
           EVALUATE  SQLCODE                             
             WHEN 0                                      
                MOVE 'INSERTED'   TO PROMSGO                  
             WHEN -803                                        
                MOVE 'DUPLICATE KEY'   TO PROMSGO             
             WHEN -180                                        
                MOVE 'INVALID DATE'   TO PROMSGO              
             WHEN -181                                        
                MOVE 'INVALID DATE'   TO PROMSGO               
             WHEN OTHER                                       
                MOVE  SQLCODE  TO WS-SQLCODE                  
                STRING 'INSERT ERROR' DELIMITED BY SIZE       
                       WS-SQLCODE     DELIMITED BY SIZE       
                       INTO  PROMSGO                           
           END-EVALUATE                                       
           PERFORM 2000-SEND-PARA                             
           PERFORM 8888-RETURN-TRANS-PARA.                    
       8888-RETURN-TRANS-PARA.                                
           EXEC CICS                                          
                RETURN                      
                TRANSID('A202')             
                COMMAREA(WS-COMM-AREA)      
           END-EXEC.                        
      *                                     
       9999-RETURN-PARA.                    
           EXEC CICS                        
                RETURN                      
           END-EXEC.      
