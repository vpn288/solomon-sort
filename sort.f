( S" lib/ext/case.f" INCLUDED )



1 VALUE temp
VARIABLE temp2 0 temp2 !
VARIABLE on_this_index
VARIABLE on_index_max
VARIABLE min
VARIABLE max
VARIABLE input_file 0 , 
VARIABLE indexes_file 0 ,
VARIABLE tempnumber
VARIABLE tempindex
VARIABLE counter2
VARIABLE delta
VARIABLE sec_delta
VARIABLE buf_cnt
VARIABLE n_zero
VARIABLE n_one
VARIABLE n_two
VARIABLE n_three
VARIABLE n_four
VARIABLE n_another

0 n_zero ! 0 n_one ! 0 n_two !
0 VALUE fid
32 ALLOCATE DROP CONSTANT buf 
128 ALLOCATE DROP CONSTANT stack_buf

VARIABLE counter 
VARIABLE counter1
VARIABLE 32cnt

STARTLOG

: +buf CELL buf_cnt +! ;
: -buf CELL NEGATE buf_cnt +! ; 
: buf! stack_buf buf_cnt @ + ! +buf ;
: buf@ -buf stack_buf buf_cnt @ + @ ; 

: minn  min ! BEGIN DUP min @ < IF min @ >R  min ! THEN             -1 counter2 +! counter2 @ 0= UNTIL ;

: depth. ." Depth:"  DEPTH . ;
: 32cnt-    32cnt @ 1- DUP 32cnt ! ;
: temp+     fid CELL+ 1+! ;
: counter-   counter @  1- counter ! counter @  ;
: cut-entry-spaces   32 32cnt ! BEGIN DUP C@  BL <> IF  1 32cnt ! ELSE   1+ temp+ THEN  32cnt- 0= UNTIL ;

: bl_word  ( addr -- addr u )  cut-entry-spaces  32 32cnt !  
                               BEGIN DUP C@ BL = IF  1 32cnt ! ELSE temp+ THEN   1+  32cnt- 0= UNTIL DROP  ;



: get_number   ( file_id_adr -- number )

              buf 32  fid @ READ-FILE DROP DROP ( ." IORr:" . DROP  CR  buf 32 TYPE CR )
              buf  bl_word fid CELL+ @ temp2 +! 
              buf fid CELL+ @ EVALUATE  ( DUP . ) 1 fid CELL+ !
              temp2 @ 0 fid @ REPOSITION-FILE DROP ;

S" input.txt" R/O OPEN-FILE DROP  input_file ! 
S" indexes.txt" R/W CREATE-FILE DROP  indexes_file !


: fill_by_zero   0 0 indexes_file @ REPOSITION-FILE DROP 
                BEGIN 0 0 (D.) DROP  32 indexes_file @ WRITE-FILE DROP counter- 0= UNTIL ;

input_file TO fid  get_number DUP counter1 ! counter !  counter @ . .( numbers in file.) CR

get_number DUP min ! max !

counter1 @ 2* counter !
.( filling ) 
fill_by_zero  
 CR .( filled ) CR
counter1 @ 1-  counter !

: (min)      >R R@ min @ > IF  R> buf!  ELSE  min @ buf! R> min !  THEN  ; 

: (min-max)  >R R@ min @ < IF R@   min !  THEN R@ max @ > IF R@  max !  THEN R> ; 

: min_max     BEGIN  get_number  (min-max) DROP  counter-  0=  UNTIL   ; 

: min-max   BEGIN get_number (min-max) DROP  counter- 0= UNTIL   ; 

min_max 


min @ .( Min:) . max @ .( Max:) .


max @ min @ - counter1 @ / 1+ DUP .( Delta:) . delta !

0. fid @ REPOSITION-FILE DROP
 
1 fid CELL+ ! 0 temp2 !

get_number counter ! 

: get_on_index  
              32 * S>D fid @ REPOSITION-FILE DROP  buf 32 fid @ READ-FILE DROP DROP ( ." Buffer:"  buf  32 TYPE  depth.   )    buf  bl_word   buf fid CELL+ @ EVALUATE  1 fid CELL+ ! ( DUP ." Eval:" . ) ;

: indexes  
           BEGIN input_file TO fid get_number tempnumber ! tempnumber @ min @ - delta @ / tempindex ! 
                 tempindex @   indexes_file TO fid get_on_index 1+  on_this_index ! on_this_index @ on_index_max @ MAX on_index_max !
                 tempindex @  32 * S>D fid @ REPOSITION-FILE DROP 
                 on_this_index @   S>D (D.)  fid @ WRITE-FILE DROP 
                 on_this_index @ counter1 @ * tempindex @ + 32 * S>D fid @ REPOSITION-FILE DROP  
                 tempnumber @  S>D (D.)  fid @ WRITE-FILE DROP 
 counter- 0= UNTIL ;
CR
  
indexes 
: get_nth_number 32 * tempindex @ + S>D fid @ REPOSITION-FILE DROP 
                get_number
;

: on_one        CR ." one     "   counter1 @ get_nth_number .   DROP
                 ;
: range_two    2DUP  < IF  SWAP THEN ;

: on_two        CR ." two     " 
                counter1 @ get_nth_number  
                counter1 @ 2* get_nth_number  
                range_two . . DROP ;

: on_three      CR ." three   "
                counter1 @     get_nth_number
                counter1 @ 2*  get_nth_number
                counter1 @ 3 * get_nth_number
                min ! (min) (min)  buf@ buf@ range_two min @ . . . DROP ;

: on_four       CR ." four    " 
                counter1 @     get_nth_number
                counter1 @ 2*  get_nth_number
                counter1 @ 3 * get_nth_number
                counter1 @ 4 * get_nth_number
                min ! (min) (min) (min) min @ . buf@ buf@ buf@ min ! (min) (min) buf@ buf@ range_two min @ . . . DROP ;


on_index_max 1+!
0 tempindex !
counter1 @ counter !
indexes_file TO fid 
on_index_max @ 16 * ALLOCATE DROP CONSTANT secondary_buf

: gather2    0  >R BEGIN  secondary_buf on_index_max @ CELL * + R@ CELL * + @ 
                    DUP 0 = IF   DROP ELSE  
                    DUP 1 = IF   DROP  on_index_max @ 2*  CELL *   R@ CELL * + secondary_buf +  @ .    ELSE 
                    DUP 2 = IF   DROP  on_index_max @ 2*   CELL *   R@ CELL * + secondary_buf +  @
                                       on_index_max @ 3 *  CELL *   R@ CELL * + secondary_buf +  @     
                                       range_two . .   ELSE 
                    DUP 3 = IF   ." sec_three "
                                 DROP on_index_max @ 2*   CELL *   R@ CELL * + secondary_buf +  @
                                      on_index_max @ 3 *  CELL *   R@ CELL * + secondary_buf +  @   
                                      on_index_max @ 4 *  CELL *   R@ CELL * + secondary_buf +  @ 
                                      min ! (min) (min)  buf@ buf@ range_two min @ . . . ELSE 
                    DUP 4 = IF  ." sec_four " DROP 
                                      on_index_max @ 2*   CELL *   R@ CELL * + secondary_buf +  @
                                      on_index_max @ 3 *  CELL *   R@ CELL * + secondary_buf +  @   
                                      on_index_max @ 4 *  CELL *   R@ CELL * + secondary_buf +  @   
                                      on_index_max @ 5 *  CELL *   R@ CELL * + secondary_buf +  @
    min ! (min) (min) (min) min @ . buf@ buf@ buf@ min ! (min) (min) buf@ buf@ range_two min @ . . . 
 ELSE  ." sec_more " DROP  THEN THEN THEN THEN THEN


       R> 1+ >R R@ counter2 @ = UNTIL RDROP  
;

: secondary_indexes ( находим индекс для числа ) counter2 @ >R 
                   BEGIN secondary_buf  R@  CELL *  +  @ DUP  min @  - sec_delta @ / >R
                    ( инкрементируем индекс  )   ( индесы лежат в сeкондарибуф начиная с ониндексмакс*селлс )
                    secondary_buf on_index_max @ CELL * +  R>  CELL * +  DUP DUP 1+!  @ 
                    ( получбуфили значение индекса, умножаем его на ониндексмакс, на селл и прибавляем к началу индексов )           on_index_max @  *  CELL *   +   !  ( а теперь надо записать число по этому адресу) 
                    R> 1- >R R@ 0= UNTIL RDROP                     
     
;

: min_and_max      counter2 @ >R   secondary_buf CELL+ @ DUP min !  max !    
                   BEGIN secondary_buf R@  CELL * + @ (min-max)  DROP    R> 1- >R R@ 0= UNTIL RDROP 
                  (  min @ . max @ . ." -- " ) max @ min @ - counter2 @ / 1+  sec_delta !    
  ( нашли минимальное и максимальное, вычислили дельту ) 
                  secondary_indexes gather2
                 
;

: on_another    CR ." another " counter2 ! counter2 @ DUP >R >R ( counter2 - количество чисел во вторичном массиве )
                BEGIN R@ counter1 @ * get_nth_number  R@ CELL * secondary_buf + !   R> 1- >R R@ 0= UNTIL RDROP ( заполнили вторичный массив )

                min_and_max RDROP
               ( BEGIN R@ CELL * secondary_buf + @ .    R> 1- >R R@ 0= UNTIL  RDROP ) ;

: gathering  BEGIN  tempindex @ S>D fid @ REPOSITION-FILE DROP get_number  
                    DUP 0 = IF ( CR ." zero  " ) n_zero 1+! DROP ELSE  
                    DUP 1 = IF on_one     n_one 1+!       ELSE 
                    DUP 2 = IF on_two     n_two 1+!       ELSE 
                    DUP 3 = IF on_three   n_three 1+!     ELSE 
                    DUP 4 = IF on_four    n_four 1+!      ELSE secondary_buf on_index_max @ 16 * 0 FILL
 on_another  n_another 1+!  THEN THEN THEN THEN THEN
                                          
                           32 tempindex +!              
                                 
             counter- 0= UNTIL ;

0. fid @ REPOSITION-FILE DROP
 max @ min @ max ! min ! 

gathering
on_index_max @ 1- CR
.( max in index:) .  n_zero @ .( Zeroes:) . n_one @ .( Ones:) . n_two @ .( Twos:) . n_three @ .( Threes:)  . n_four .( Fours:) @ . n_another @ .( More_than_four:) . CR CR
