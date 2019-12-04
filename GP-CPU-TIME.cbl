       ID Division.
       PROGRAM-ID GPCPUtime.
      * ****************************************************************
      *                                                                *
      *    This program retrieves specific job-related data from z/OS  *
      *    control blocks and moves it to Working-storage.             *
      *                                                                *
      *    CPU time is meassured in microseconds:                      *
      *    1.000.000 microseconds = 1 second                           *
      *                                                                *
      * ****************************************************************
       Environment Division.
       Configuration Section.
       Source-Computer. IBM-zOS.
       Object-Computer. IBM-zOS.
       Special-names.
           Decimal-point is comma.
       Input-Output Section.
       File-control.
           Select TimeData assign to TimeData.


       Data Division.
       File Section.
       FD  TimeData
           Record 41 recording mode is f label record omitted.
         01 TimeDataOut       PIC X(41).


       Working-Storage Section.
         01 DCB-Results.
           05 job-name        PIC X(8).
           05 micro-seconds   PIC S9(15) Packed-Decimal.
           05 batch-or-cics   PIC X(5).
              88 Batch Value "BATCH".
              88 CICS  Value "CICS ".

         01 eight-bytes.
           05 double-word     PIC S9(18) Binary.

         01 Schalter          PIC  9(1)  Binary Value Zero.
         01 LastUsed          PIC S9(15) Packed-Decimal.
         01 CPUusage          PIC S9(15) Packed-Decimal.

         01 Unstring-Felder.
            05 Datensatz-OUT  PIC X(41).
            05 Datensatz-TEMP PIC X(41).
            05 P1             PIC 9(04).
            05 P2             PIC 9(04).
            05 P-MAX          PIC 9(04)  VALUE 41.
            05 ANZ-CHAR       PIC 999    VALUE 0.
            05 ANZ-SUBSTRING  PIC 999    VALUE 0.

         01 Datensatz.
           05 Jobname         PIC X(8)   Value Spaces.
           05 Filler          PIC X(1)   value ";".
           05 Laufdatum       PIC X(8)   Value Spaces.
           05 Filler          PIC X(1)   value ";".
           05 CPU-Zeit        PIC 9(15)  Value Zero usage Display.
           05 Filler          PIC X(1)   value ";".
           05 Programmversion PIC X(7)   Value "1.0.0  ".



       Linkage Section.
         01 cb1.
            05 ptr1 Pointer Occurs 256.
         01 cb2.
            05 ptr2 Pointer Occurs 256.



       Procedure Division
           entry "ELLV0127".
       Main section.
           evaluate OP-CODE
              when "OPEN"
                 open output TimeData
                 goback
              when "CLOSE"
                 close TimeData
                 goback
           end-evaluate.

           perform Read-Data-Control-Blocks.
           if Schalter = 0
              move 0 to LastUsed
              move micro-seconds to LastUsed
              move 1 to Schalter
           else
              compute CPUusage = micro-seconds - LastUsed
              perform Write-Data
              move 0 to Schalter
           end-if.
           goback.


       Read-Data-Control-Blocks Section.
           set address of cb1 to null.                                  PSA
           set address of cb1 to ptr1(136).                             TCB
           move cb1(317:8) to eight-bytes.
           compute micro-seconds = double-word / 4096.
           set address of cb2 to ptr1(4).                               TIOT
           move cb2(1:8) to job-name.
           if cb2(21:4) = low-values then                               CAUF
             set Batch to true
           else
             set CICS to true
           end-if.


       Write-Data Section.
           move job-name to Jobname.
           move function CURRENT-DATE (1:8) to Laufdatum.
           move CPUusage to CPU-Zeit.
           perform ElimLeerzeichen.
           write TimeDataOut from Datensatz-OUT.


       ElimLeerzeichen Section.
           move 1 to P1 P2.
           move spaces to Datensatz-OUT Datensatz-TEMP.
           perform until P1 > P-MAX
               unstring Datensatz delimited by all spaces
                   into Datensatz-TEMP count in ANZ-CHAR
                       with pointer P1 tallying in ANZ-SUBSTRING
                       on overflow continue
      *                more data to process
                       not on overflow continue
               end-unstring
               string Datensatz-TEMP delimited by spaces
                   into Datensatz-OUT
                       with pointer P2
                       on overflow continue
                       not on overflow continue
               end-string
           end-perform.
