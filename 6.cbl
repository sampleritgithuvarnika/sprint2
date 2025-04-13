       IDENTIFICATION DIVISION.                    
       PROGRAM-ID.    A20PGM6.                     
      * AUTO GENERATE AMOUNT                       
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
             INCLUDE ORD1                           
           END-EXEC.                               
           EXEC SQL                                             
             INCLUDE PROD1                                     
           END-EXEC. 
           EXEC SQL                                             
             INCLUDE CUS1                                     
           END-EXEC. 
       77 WS-ABS-TIME     PIC S9(15) COMP.                      
       77 WS-SEP          PIC X(01) VALUE '-'.                  
       77 WS-TSEP         PIC X(01) VALUE ':'.                  
       77 WS-SQLCODE      PIC -9(04).                           
       01 WS-TOTAL-AMOUNT PIC S9(10)V9(2) COMP-3.                       
       77 WS-PRICE        PIC S9(04)V9(2) COMP-3.                       
       01 WS-STATUS       PIC X(10) VALUE 'CONFIRMED'.          
       01 WS-MAP-NUM       PIC 9(08)V9(02).                      
      *                                                         
       01 WS-FILL-COMMS      PIC X(10) VALUE 'COMM START'.      
      *                                                         
       01 WS-COMM.                                              
         05 WS-DATA          PIC X(10).                         
         05 WS-DATE          PIC X(10).                         
         05 WS-TIME          PIC X(08).                                     
         05 WS-QUANTITY      PIC 9(03).                   
         05 WS-PROCODE       PIC X(6).
         05 WS-CUSTID        PIC X(6).                    
       01 WS-FILL-COMME      PIC X(10) VALUE 'COMM ENDS'.                                            
       01 WS-ORD-ID          PIC X(06).                   
       01 WS-MAX-ID          PIC X(06).                   
       01 WS-MAX-NUM         PIC 9(05).                   
       01 WS-MAX-NUM-FORMATTED  PIC ZZZZZZ.               
       01 WS-ORDER-DATE      PIC X(10).                   
       01 WS-NUM             PIC 9(03).                   
       01 WS-DATE-FORMATTED  PIC X(10).                   
       01 WS-DATE1           PIC X(10).                   
       LINKAGE SECTION.                                   
       01 DFHCOMMAREA.                                    
          05 LK-COMM.                                  
            10 LK-DATA          PIC X(10).             
            10 LK-DATE          PIC X(10).             
            10 LK-TIME          PIC X(08).                             
            10 LK-QUANTITY      PIC 9(03).                
            10 LK-PROCODE       PIC X(6).                 
            10 LK-CUSTID        PIC X(6).          
      *                                                
       PROCEDURE DIVISION.                             
       0000-MAIN-PARA.                                 
              IF  EIBTRNID = 'A205'                    
              MOVE LOW-VALUES TO ORDIN2O             
              PERFORM 1000-GET-DATE 
              MOVE WS-DATE1  TO ORDDTE2O
              MOVE LK-COMM TO WS-COMM                  
              PERFORM 5050-SET-ORDER-ID-PARA           
              PERFORM 3000-CALCULATE-AMOUNT                         
             PERFORM 1000-SEND-SCREEN1-PARA          
             MOVE 'PLACE' TO WS-DATA                 
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
            EXEC CICS FORMATTIME                     
                 ABSTIME(WS-ABS-TIME)                
                 DATESEP(WS-SEP)                     
                 YYYYMMDD(WS-DATE1)                  
                 TIME(WS-TIME)                       
                 TIMESEP(WS-TSEP)                    
            END-EXEC.                                
      *                                              
       1000-SEND-SCREEN1-PARA. 
            MOVE WS-TIME TO ORDTMEO
            MOVE WS-DATE1 TO ORDDTEO                                  
            EXEC CICS                                
                SEND MAP('ORDIN2')                    
                MAPSET('A20MPS1')                           
                ERASE                                       
            END-EXEC.                                       
       2000-RECV-SCREEN1-PARA.                              
            EXEC CICS                                       
               RECEIVE MAP('ORDIN2')                         
                       MAPSET('A20MPS1')                    
                       INTO(ORDIN2I)                         
            END-EXEC.                                       
       1100-KEY-CHECK-MENU-PARA.                            
            EVALUATE EIBAID 
              WHEN DFHPF2                                    
               PERFORM  2000-RECV-SCREEN1-PARA 
               PERFORM  6666-INSERT-PARA
              WHEN DFHPF4
                  EXEC CICS
                      XCTL PROGRAM('A20PGM1')
                  END-EXEC
              WHEN DFHPF3                                   
               MOVE LOW-VALUES TO ORDIN2O                    
               MOVE 'YOU ARE LOGGED OUT' TO ORDMSG5O         
               PERFORM 1000-SEND-SCREEN1-PARA               
               PERFORM 9999-RETURN-PARA                                 
              WHEN DFHPF5                               
               MOVE LOW-VALUES TO ORDIN2O               
               PERFORM 1000-SEND-SCREEN1-PARA           
               PERFORM 4000-RETURN-TRANS-PARA           
              WHEN OTHER                                
               PERFORM 2000-RECV-SCREEN1-PARA           
               MOVE 'INVALID KEY PRESSED' TO ORDMSG5O   
               PERFORM 1000-SEND-SCREEN1-PARA           
               PERFORM 4000-RETURN-TRANS-PARA           
            END-EVALUATE.                               
       4000-RETURN-TRANS-PARA.                          
            EXEC CICS                                   
                 RETURN                                 
                 TRANSID('A206')                                      
                 COMMAREA(WS-COMM)                                    
            END-EXEC.                                                                                       
       5050-SET-ORDER-ID-PARA.                                        
            EXEC SQL                                                  
               SELECT MAX(ORDER_ID) INTO :ORDER-ID                    
               FROM ORDER1                                            
            END-EXEC.                                                 
           IF SQLCODE = 0                                             
              MOVE ORDER-ID-TEXT(1:ORDER-ID-LEN) TO WS-MAX-ID         
              COMPUTE WS-MAX-NUM = FUNCTION NUMVAL(ORDER-ID-TEXT(2:5))
              ADD 1 TO WS-MAX-NUM                                     
           STRING 'O'  DELIMITED BY SIZE                              
                WS-MAX-NUM DELIMITED BY SIZE                           
                    INTO WS-ORD-ID                                   
           MOVE WS-ORDER-ID TO ORDIDO                                  
           END-IF.                                                     
       3000-CALCULATE-AMOUNT.                                          
           MOVE WS-PROCODE  TO PRODUCT-CODE-TEXT OF DCLPRODUCT1               
           MOVE LENGTH OF WS-PROCODE TO PRODUCT-CODE-LEN  OF DCLPRODUCT1      
           EXEC SQL                                                    
               SELECT PRODUCT_CODE,                               
                      PRODUCT_PRICE                                       
               INTO :DCLPRODUCT1.PRODUCT-CODE,                             
                    :PRODUCT-PRICE                                        
               FROM PRODUCT1                                            
               WHERE PRODUCT_CODE = :DCLPRODUCT1.PRODUCT-CODE                 
           END-EXEC.
           MOVE SQLCODE TO WS-SQLCODE
           EVALUATE SQLCODE
           WHEN 0
            MOVE PRODUCT-PRICE TO WS-PRICE
            MULTIPLY WS-PRICE  BY WS-QUANTITY GIVING WS-TOTAL-AMOUNT
            MOVE WS-TOTAL-AMOUNT TO ORDAMTO
            PERFORM 1000-SEND-SCREEN1-PARA
            PERFORM 4000-RETURN-TRANS-PARA
           WHEN 100 
             MOVE 0 TO WS-TOTAL-AMOUNT
           WHEN OTHER
             MOVE WS-SQLCODE TO ORDMSG5O
             PERFORM 1000-SEND-SCREEN1-PARA
             PERFORM 4000-RETURN-TRANS-PARA
           END-EVALUATE.                       
       6666-INSERT-PARA.                                             
           COMPUTE WS-MAP-NUM = FUNCTION NUMVAL-C(ORDAMTI)           
           MOVE ORDIDI  TO ORDER-ID-TEXT                         
           MOVE LENGTH OF ORDER-ID-TEXT TO ORDER-ID-LEN                
           MOVE WS-CUSTID TO CUSTOMER-ID-TEXT OF DCLORDER1                              
           MOVE LENGTH OF CUSTOMER-ID-TEXT OF DCLORDER1 TO
           CUSTOMER-ID-LEN OF DCLORDER1
           MOVE WS-PROCODE TO PRODUCT-CODE-TEXT OF DCLORDER1
           MOVE LENGTH OF PRODUCT-CODE-TEXT OF DCLORDER1 TO
           PRODUCT-CODE-LEN OF DCLORDER1
           MOVE ORDDTE2I TO ORDER-DATE 
           MOVE WS-QUANTITY TO QUANTITY
           MOVE WS-STATUS TO PROD-STATUS-TEXT OF DCLORDER1
           MOVE LENGTH OF WS-STATUS OF PROD-STATUS-TEXT OF DCLORDER1
           MOVE WS-MAP-NUM TO TOTAL-AMOUNT                          
           EXEC SQL                                                    
               INSERT INTO ORDER1 VALUES(                              
                      :ORDER-ID,                                       
                      :DCLORDER1.CUSTOMER-ID,                                    
                      :DCLORDER1.PRODUCT-CODE,                            
                      :ORDER-DATE,                                     
                      :QUANTITY,                                   
                      :DCLORDER1.PROD-STATUS,                                     
                      :TOTAL-AMOUNT)                                   
           END-EXEC                                                    
           EVALUATE  SQLCODE                                           
             WHEN 0                                                    
                MOVE WS-PROCODE TO PRODUCT-CODE-TEXT OF DCLPRODUCT1          
                MOVE LENGTH OF WS-PROCODE TO PRODUCT-CODE-LEN  OF DCLPRODUCT1
                EXEC SQL                                           
                   UPDATE PRODUCT1                                  
                   SET STOCK_AVAILABILITY = STOCK_AVAILABILITY - :QUANTITY         
                   WHERE PRODUCT_CODE = :DCLPRODUCT1.PRODUCT-CODE         
                END-EXEC                                           
                IF SQLCODE = 0 THEN                                
                   MOVE 'ORDER PLACED SUCCESSFULLY' TO ORDMSG5O     
                ELSE                                               
                   MOVE  SQLCODE  TO WS-SQLCODE                    
                   STRING 'UPDATE ERROR' DELIMITED BY SIZE         
                       WS-SQLCODE     DELIMITED BY SIZE            
                       INTO  ORDMSG5O                               
                END-IF                                             
             WHEN OTHER                                            
                MOVE  SQLCODE  TO WS-SQLCODE                       
                STRING 'INSERT ERROR' DELIMITED BY SIZE            
                       WS-SQLCODE     DELIMITED BY SIZE            
                       INTO  ORDMSG5O        
           END-EVALUATE                     
           PERFORM 1000-SEND-SCREEN1-PARA.  
           PERFORM  4000-RETURN-TRANS-PARA.               
       9999-RETURN-PARA.                 
            EXEC CICS                    
                 RETURN                  
            END-EXEC.        
