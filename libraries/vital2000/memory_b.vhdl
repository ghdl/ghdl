-- ----------------------------------------------------------------------------
-- Title        : Standard VITAL Memory Package
--              :
-- Library      : Vital_Memory
--              :
-- Developers   : IEEE DASC Timing Working Group (TWG), PAR 1076.4
--              : Ekambaram Balaji, LSI Logic Corporation
--              : Jose De Castro, Consultant
--              : Prakash Bare, GDA Technologies
--              : William Yam, LSI Logic Corporation
--              : Dennis Brophy, Model Technology
--              :
-- Purpose      : This packages defines standard types, constants, functions
--              : and procedures for use in developing ASIC memory models.
--              :  
-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------
-- Modification History : 
-- ----------------------------------------------------------------------------
-- Ver:|Auth:| Date:| Changes Made:
-- 0.1 | eb  |071796| First prototye as part of VITAL memory proposal
-- 0.2 | jdc |012897| Initial prototyping with proposed MTM scheme 
-- 0.3 | jdc |090297| Extensive updates for TAG review (functional)
-- 0.4 | eb  |091597| Changed naming conventions for VitalMemoryTable 
--     |     |      | Added interface of VitalMemoryCrossPorts() & 
--     |     |      | VitalMemoryViolation().
-- 0.5 | jdc |092997| Completed naming changes thoughout package body.
--     |     |      | Testing with simgle port test model looks ok.
-- 0.6 | jdc |121797| Major updates to the packages:
--     |     |      | - Implement VitalMemoryCrossPorts()
--     |     |      |   - Use new VitalAddressValueType
--     |     |      |   - Use new VitalCrossPortModeType enum
--     |     |      |   - Overloading without SamePort args
--     |     |      |   - Honor erroneous address values
--     |     |      |   - Honor ports disabled with 'Z'
--     |     |      | - Implement implicit read 'M' table symbol
--     |     |      | - Cleanup buses to use (H DOWNTO L)
--     |     |      | - Message control via MsgOn,HeaderMsg,PortName
--     |     |      | - Tested with 1P1RW,2P2RW,4P2R2W,4P4RW cases
-- 0.7 | jdc |052698| Bug fixes to the packages:
--     |     |      | - Fix failure with negative Address values
--     |     |      | - Added debug messages for VMT table search
--     |     |      | - Remove 'S' for action column (only 's')
--     |     |      | - Remove 's' for response column (only 'S')
--     |     |      | - Remove 'X' for action and response columns
-- 0.8 | jdc |061298| Implemented VitalMemoryViolation()
--     |     |      | - Minimal functionality violation tables
--     |     |      | - Missing:
--     |     |      |   - Cannot handle wide violation variables
--     |     |      |   - Cannot handle sub-word cases
--     |     |      | Fixed IIC version of MemoryMatch
--     |     |      | Fixed 'M' vs 'm' switched on debug output
--     |     |      | TO BE DONE:
--     |     |      | - Implement 'd' corrupting a single bit
--     |     |      | - Implement 'D' corrupting a single bit
-- 0.9 |eb/sc|080498| Added UNDEF value for VitalPortFlagType
-- 0.10|eb/sc|080798| Added CORRUPT value for VitalPortFlagType
-- 0.11|eb/sc|081798| Added overloaded function interface for
--     |     |      |       VitalDeclareMemory
-- 0.14| jdc |113198| Merging of memory functionality and version
--     |     |      | 1.4 9/17/98 of timing package from Prakash
-- 0.15| jdc |120198| Major development of VMV functionality
-- 0.16| jdc |120298| Complete VMV functionlality for initial testing
--     |     |      | - New ViolationTableCorruptMask() procedure
--     |     |      | - New MemoryTableCorruptMask() procedure
--     |     |      | - HandleMemoryAction():
--     |     |      |   - Removed DataOutBus bogus output
--     |     |      |   - Replaced DataOutTmp with DataInTmp
--     |     |      |   - Added CorruptMask input handling
--     |     |      |   - Implemented 'd','D' using CorruptMask
--     |     |      |   - CorruptMask on 'd','C','L','D','E'
--     |     |      |   - CorruptMask ignored on 'c','l','e'
--     |     |      |   - Changed 'l','d','e' to set PortFlag to CORRUPT
--     |     |      |   - Changed 'L','D','E' to set PortFlag to CORRUPT
--     |     |      |   - Changed 'c','l','d','e' to ignore HighBit, LowBit
--     |     |      |   - Changed 'C','L','D','E' to use HighBit, LowBit
--     |     |      | - HandleDataAction():
--     |     |      |   - Added CorruptMask input handling
--     |     |      |   - Implemented 'd','D' using CorruptMask
--     |     |      |   - CorruptMask on 'd','C','L','D','E'
--     |     |      |   - CorruptMask ignored on 'l','e'
--     |     |      |   - Changed 'l','d','e' to set PortFlag to CORRUPT
--     |     |      |   - Changed 'L','D','E' to set PortFlag to CORRUPT
--     |     |      |   - Changed 'l','d','e' to ignore HighBit, LowBit
--     |     |      |   - Changed 'L','D','E' to use HighBit, LowBit
--     |     |      | - MemoryTableLookUp():
--     |     |      |   - Added MsgOn table debug output
--     |     |      |   - Uses new MemoryTableCorruptMask()
--     |     |      | - ViolationTableLookUp():
--     |     |      |   - Uses new ViolationTableCorruptMask()
-- 0.17| jdc |120898| - Added VitalMemoryViolationSymbolType,
--     |     |      |   VitalMemoryViolationTableType data 
--     |     |      |   types but not used yet (need to discuss)
--     |     |      | - Added overload for VitalMemoryViolation()
--     |     |      |   which does not have array flags
--     |     |      | - Bug fixes for VMV functionality:
--     |     |      |   - ViolationTableLookUp() not handling '-' in
--     |     |      |     scalar violation matching
--     |     |      |   - VitalMemoryViolation() now normalizes
--     |     |      |     VFlagArrayTmp'LEFT as LSB before calling
--     |     |      |     ViolationTableLookUp() for proper scanning
--     |     |      |   - ViolationTableCorruptMask() had to remove 
--     |     |      |     normalization of CorruptMaskTmp and
--     |     |      |     ViolMaskTmp for proper MSB:LSB corruption
--     |     |      |   - HandleMemoryAction(), HandleDataAction() 
--     |     |      |     - Removed 'D','E' since not being used
--     |     |      |     - Use XOR instead of OR for corrupt masks
--     |     |      |     - Now 'd' is sensitive to HighBit, LowBit
--     |     |      |   - Fixed LowBit overflow in bit writeable case
--     |     |      |     - MemoryTableCorruptMask()
--     |     |      |     - ViolationTableCorruptMask()
--     |     |      |     - VitalMemoryTable()
--     |     |      |     - VitalMemoryCrossPorts()
--     |     |      |   - Fixed VitalMemoryViolation() failing on
--     |     |      |     error AddressValue from earlier VMT()
--     |     |      | - Minor cleanup of code formatting
-- 0.18| jdc |032599| - In VitalDeclareMemory()
--     |     |      |   - Added BinaryLoadFile formal arg and 
--     |     |      |     modified LoadMemory() to handle bin
--     |     |      | - Added NOCHANGE to VitalPortFlagType
--     |     |      | - For VitalCrossPortModeType
--     |     |      |   - Added CpContention enum
--     |     |      | - In HandleDataAction()
--     |     |      |   - Set PortFlag := NOCHANGE for 'S' 
--     |     |      | - In HandleMemoryAction()
--     |     |      |   - Set PortFlag := NOCHANGE for 's' 
--     |     |      | - In VitalMemoryTable() and
--     |     |      |   VitalMemoryViolation()
--     |     |      |   - Honor PortFlag = NOCHANGE returned 
--     |     |      |     from HandleMemoryAction()
--     |     |      | - In VitalMemoryCrossPorts()
--     |     |      |   - Fixed Address = AddressJ for all
--     |     |      |     conditions of DoWrCont & DoCpRead 
--     |     |      |   - Handle CpContention like WrContOnly
--     |     |      |     under CpReadOnly conditions, with
--     |     |      |     associated memory message changes
--     |     |      |   - Handle PortFlag = NOCHANGE like
--     |     |      |     PortFlag = READ for actions
--     |     |      | - Modeling change:
--     |     |      |   - Need to init PortFlag every delta
--     |     |      |     PortFlag_A := (OTHES => UNDEF);
--     |     |      | - Updated InternalTimingCheck code
-- 0.19| jdc |042599| - Fixes for bit-writeable cases
--     |     |      | - Check PortFlag after HandleDataAction
--     |     |      |   in VitalMemoryViolation()
-- 0.20| jdc |042599| - Merge PortFlag changes from Prakash
--     |     |      |   and Willian:
--     |     |      |     VitalMemorySchedulePathDelay()
--     |     |      |     VitalMemoryExpandPortFlag()
-- 0.21| jdc |072199| - Changed VitalCrossPortModeType enums,
--     |     |      |   added new CpReadAndReadContention.
--     |     |      | - Fixed VitalMemoryCrossPorts() parameter
--     |     |      |   SamePortFlag to INOUT so that it can
--     |     |      |   set CORRUPT or READ value.
--     |     |      | - Fixed VitalMemoryTable() where PortFlag
--     |     |      |   setting by HandleDataAction() is being
--     |     |      |   ignored when HandleMemoryAction() sets
--     |     |      |   PortFlagTmp to NOCHANGE.
--     |     |      | - Fixed VitalMemoryViolation() to set
--     |     |      |   all bits of PortFlag when violating.
-- 0.22| jdc |072399| - Added HIGHZ to PortFlagType. HandleData
--     |     |      |   checks whether the previous state is HIGHZ.
--     |     |      |   If yes then portFlag should be NOCHANGE
--     |     |      |   for VMPD to ignore IORetain corruption.
--     |     |      |   The idea is that the first Z should be
--     |     |      |   propagated but later ones should be ignored.
--     |     |      |
-- 0.23| jdc |100499| - Took code checked in by Dennis 09/28/99
--     |     |      | - Changed VitalPortFlagType to record of
--     |     |      |   new VitalPortStateType to hold current,
--     |     |      |   previous values and separate disable.
--     |     |      |   Also created VitalDefaultPortFlag const.
--     |     |      |   Removed usage of PortFlag NOCHANGE
--     |     |      | - VitalMemoryTable() changes:
--     |     |      |   Optimized return when all curr = prev
--     |     |      |   AddressValue is now INOUT to optimize
--     |     |      |   Transfer PF.MemoryCurrent to MemoryPrevious
--     |     |      |   Transfer PF.DataCurrent to DataPrevious
--     |     |      |   Reset PF.OutputDisable to FALSE
--     |     |      |   Expects PortFlag init in declaration
--     |     |      |   No need to init PortFlag every delta
--     |     |      | - VitalMemorySchedulePathDelay() changes:
--     |     |      |   Initialize with VitalDefaultPortFlag
--     |     |      |   Check PortFlag.OutputDisable
--     |     |      | - HandleMemoryAction() changes:
--     |     |      |   Set value of PortFlag.MemoryCurrent
--     |     |      |   Never set PortFlag.OutputDisable
--     |     |      | - HandleDataAction() changes:
--     |     |      |   Set value of PortFlag.DataCurrent
--     |     |      |   Set PortFlag.DataCurrent for HIGHZ
--     |     |      | - VitalMemoryCrossPorts() changes:
--     |     |      |   Check/set value of PF.MemoryCurrent
--     |     |      |   Check value of PF.OutputDisable
--     |     |      | - VitalMemoryViolation() changes:
--     |     |      |   Fixed bug - not reading inout PF value
--     |     |      |   Clean up setting of PortFlag
-- 0.24| jdc |100899| - Modified update of PF.OutputDisable
--     |     |      |   to correctly accomodate 2P1W1R case:
--     |     |      |   the read port should not exhibit
--     |     |      |   IO retain corrupt when reading
--     |     |      |   addr unrelated to addr being written.
-- 0.25| jdc |100999| - VitalMemoryViolation() change:
--     |     |      |   Fixed bug with RDNWR mode incorrectly
--     |     |      |   updating the PF.OutputDisable
-- 0.26| jdc |100999| - VitalMemoryCrossPorts() change:
--     |     |      |   Fixed bugs with update of PF
-- 0.27| jdc |101499| - VitalMemoryCrossPorts() change:
--     |     |      |   Added DoRdWrCont message (ErrMcpRdWrCo,
--     |     |      |   Memory cross port read/write data only
--     |     |      |   contention)
--     |     |      | - VitalMemoryTable() change:
--     |     |      |   Set PF.OutputDisable := TRUE for the
--     |     |      |   optimized cases.
-- 0.28| pb  |112399| - Added 8 VMPD procedures for vector 
--     |     |      |   PathCondition support. Now the total
--     |     |      |   number of overloadings for VMPD is 24.
--     |     |      | - Number of overloadings for SetupHold
--     |     |      |   procedures increased to 5. Scalar violations
--     |     |      |   are not supported anymore. Vector checkEnabled
--     |     |      |   support is provided through the new overloading
-- 0.29| jdc |120999| - HandleMemoryAction() HandleDataAction()
--     |     |      |   Reinstated 'D' and 'E' actions but
--     |     |      |   with new PortFlagType
--     |     |      | - Updated file handling syntax, must compile
--     |     |      |   with -93 syntax now.
-- 0.30| jdc |022300| - Formated for 80 column max width
-- ----------------------------------------------------------------------------

LIBRARY IEEE;
USE     IEEE.STD_LOGIC_1164.ALL;
USE     IEEE.Vital_Timing.all;
USE     IEEE.Vital_Primitives.all;

LIBRARY STD;
USE     STD.TEXTIO.ALL;

-- ----------------------------------------------------------------------------
PACKAGE BODY Vital_Memory IS

-- ----------------------------------------------------------------------------
-- Timing Section
-- ----------------------------------------------------------------------------

FILE LogFile : TEXT OPEN write_mode IS "delayLog";
FILE Output  : TEXT OPEN write_mode IS "STD_OUTPUT";

-- Added for turning off the debug msg..
CONSTANT PrintDebugMsg : STD_ULOGIC := '0';
                         -- '0' - don't print in STD OUTPUT
                         -- '1' - print in STD OUTPUT

-- Type and constant definitions for type conversion.
TYPE MVL9_TO_CHAR_TBL IS ARRAY (STD_ULOGIC) OF character;

--constant MVL9_to_char: MVL9_TO_CHAR_TBL := "UX01ZWLH-";
CONSTANT MVL9_to_char: MVL9_TO_CHAR_TBL := "XX01ZX010";

-- ----------------------------------------------------------------------------
-- STD_LOGIC WRITE UTILITIES
-- ----------------------------------------------------------------------------
PROCEDURE WRITE(
  l       : INOUT line;
  val     : IN std_logic_vector;
  justify : IN side := right; 
  field   : IN width := 0
) IS
  VARIABLE invect : std_logic_vector(val'LENGTH DOWNTO 1);
  VARIABLE ins    : STRING(val'LENGTH DOWNTO 1);
BEGIN
  invect := val;
  FOR I IN invect'length DOWNTO 1 LOOP
     ins(I) := MVL9_to_char(invect(I));
  END LOOP;
  WRITE(L, ins, justify, field);
END;

PROCEDURE WRITE(
  l       : INOUT line; 
  val     : IN std_ulogic;
  justify : IN side := right;
  field   : in width := 0
) IS
  VARIABLE ins : CHARACTER;
BEGIN
  ins := MVL9_to_char(val);
  WRITE(L, ins, justify, field);
END;

-- ----------------------------------------------------------------------------
PROCEDURE DelayValue(
  InputTime : IN TIME ;
  outline   : INOUT LINE
) IS
  CONSTANT header : STRING := "TIME'HIGH";
BEGIN
  IF(InputTime = TIME'HIGH) THEN
    WRITE(outline, header);
  ELSE
    WRITE(outline, InputTime);
  END IF;
END DelayValue;

-- ----------------------------------------------------------------------------
PROCEDURE PrintScheduleDataArray (
  ScheduleDataArray : IN VitalMemoryScheduleDataVectorType
) IS
  VARIABLE outline1 : LINE;
  VARIABLE outline2 : LINE;
  VARIABLE value    : TIME;
  CONSTANT empty    : STRING := "      ";
  CONSTANT header1  : STRING := "i    Age     PropDly  RetainDly"; 
  CONSTANT header2  : STRING := "i  Sc.Value  Output  Lastvalue  Sc.Time";
BEGIN
  WRITE (outline1, empty);
  WRITE (outline1, NOW);
  outline2 := outline1; 
  WRITELINE (LogFile, outline1);
  IF (PrintDebugMsg = '1') THEN
    WRITELINE (output, outline2);
  END IF;
  WRITE (outline1, header1);
  outline2 := outline1; 
  WRITELINE (LogFile, outline1);
  IF (PrintDebugMsg = '1') THEN
    WRITELINE (output, outline2);
  END IF;
  FOR i IN ScheduleDataArray'RANGE LOOP
    WRITE (outline1, i );
    WRITE (outline1, empty);
    DelayValue(ScheduleDataArray(i).InputAge, outline1);  
    WRITE (outline1, empty);
    DelayValue(ScheduleDataArray(i).PropDelay, outline1);  
    WRITE (outline1, empty);
    DelayValue(ScheduleDataArray(i).OutputRetainDelay, outline1);  
    outline2 := outline1; 
    WRITELINE (LogFile, outline1);
    IF (PrintDebugMsg = '1') THEN
      WRITELINE (output, outline2);
    END IF;
  END LOOP;
  WRITE (outline1, header2);
  outline2 := outline1; 
  WRITELINE (LogFile, outline1);
  IF (PrintDebugMsg = '1') THEN
    WRITELINE (output, outline2);
  END IF;
  FOR i IN ScheduleDataArray'RANGE LOOP
    WRITE (outline1, i ); 
    WRITE (outline1, empty);
    WRITE (outline1, ScheduleDataArray(i).ScheduleValue); 
    WRITE (outline1, empty);
    WRITE (outline1, ScheduleDataArray(i).OutputData);
    WRITE (outline1, empty);
    WRITE (outline1, ScheduleDataArray(i).LastOutputValue );
    WRITE (outline1, empty);
    DelayValue(ScheduleDataArray(i).ScheduleTime, outline1);
    outline2 := outline1; 
    WRITELINE (LogFile, outline1);
    IF (PrintDebugMsg = '1') THEN
      WRITELINE (output, outline2);
    END IF;
  END LOOP;
  WRITE (outline1, empty);
  WRITE (outline2, empty);
  WRITELINE (LogFile, outline1);
  IF (PrintDebugMsg = '1') THEN
    WRITELINE (Output, outline2);
  END IF;
END PrintScheduleDataArray;

-- ----------------------------------------------------------------------------
PROCEDURE PrintArcType (
  ArcType : IN VitalMemoryArcType
) IS
  VARIABLE outline1, outline2 : LINE;
  CONSTANT empty : STRING := " ";
  CONSTANT cross : STRING := "CrossArc";
  CONSTANT para : STRING := "ParallelArc";
  CONSTANT sub : STRING := "SubWordArc";
  CONSTANT Header1 : STRING := "Path considered @ ";
  CONSTANT Header2 : STRING := " is  ";
BEGIN
  WRITELINE (LogFile, outline1);
  WRITE (outline1, header1);
  WRITE (outline1, NOW);
  WRITE (outline1, empty);
  WRITE (outline1, header2);
  WRITE (outline1, empty);
  case ArcType is
  WHEN CrossArc =>
    WRITE (outline1, cross);
  WHEN ParallelArc =>
    WRITE (outline1, para);
  WHEN SubwordArc =>
    WRITE (outline1, sub);
  END CASE;
  outline2 := outline1 ;
  -- Appears on STD OUT
  IF (PrintDebugMsg = '1') THEN
    WRITELINE (Output, outline1);
  END IF;
  WRITELINE (LogFile, outline2);
END PrintArcType;

-- ----------------------------------------------------------------------------
-- This returns the value picked from the delay array
-- ----------------------------------------------------------------------------
PROCEDURE PrintDelay (
  outbitpos         : IN INTEGER;
  InputArrayLow     : IN INTEGER; 
  InputArrayHigh    : IN INTEGER;
  debugprop         : IN VitalTimeArrayT;
  debugretain       : IN VitalTimeArrayT
) IS
  VARIABLE outline1 : LINE;
  VARIABLE outline2 : LINE;
  VARIABLE outline3 : LINE;
  VARIABLE outline4 : LINE;
  VARIABLE outline5 : LINE;
  VARIABLE outline6 : LINE;
  CONSTANT empty    : STRING := "  ";
  CONSTANT empty5   : STRING := "  ";
  CONSTANT header1  : STRING := "Prop. delays : ";
  CONSTANT header2  : STRING := "Retain delays : ";
  CONSTANT header3  : STRING := "output bit :  ";
BEGIN
  WRITE(outline1, header3);
  WRITE(outline1, outbitpos);
  outline2 := outline1;
  WRITELINE(LogFile, outline1);
  IF (PrintDebugMsg = '1') THEN
    WRITELINE(output, outline2);
  END IF;
  WRITE(outline1, header1);
  WRITE (outline1, empty5);
  FOR i IN InputArrayHigh DOWNTO InputArrayLow LOOP
    DelayValue(debugprop(i), outline1);
    WRITE(outline1, empty);
  END LOOP;
  outline2 := outline1;
  WRITELINE(LogFile, outline1);
  IF (PrintDebugMsg = '1') THEN
    WRITELINE(output, outline2);
  END IF;
  WRITE(outline1, header2);
  WRITE (outline1, empty5);
  FOR i in InputArrayHigh DOWNTO InputArrayLow LOOP
    DelayValue(debugretain(i), outline1);
    WRITE(outline1, empty);
  END LOOP;
  outline2 := outline1;
  WRITELINE(LogFile, outline1);
  IF (PrintDebugMsg = '1') THEN
    WRITELINE(output, outline2);
  END IF;
END PrintDelay;

-- ----------------------------------------------------------------------------
PROCEDURE DebugMsg1 IS
    CONSTANT header1:STRING:= "******************************************";
    CONSTANT header2 :STRING:="Entering the process because of an i/p change";
    variable outline1, outline2 : LINE;
BEGIN
    WRITE(outline1, header1);
    outline2 := outline1;
    WRITELINE (Logfile, outline1); 
    IF (PrintDebugMsg = '1') THEN
       WRITELINE (output, outline2); 
    END IF;
    WRITE(outline1, header2);
    outline2 := outline1;
    WRITELINE (Logfile, outline1); 
    IF (PrintDebugMsg = '1') THEN 
       WRITELINE (output, outline2); 
    END IF;
    WRITE(outline1, header1);
    outline2 := outline1;
    WRITELINE (Logfile, outline1);
    IF (PrintDebugMsg = '1') THEN 
       WRITELINE (output, outline2); 
    END IF;
END DebugMsg1;

-- ----------------------------------------------------------------------------
PROCEDURE ScheduleDebugMsg IS
  CONSTANT header1  : STRING := "******************************************";
  CONSTANT header2  : STRING := "Finished executing all the procedures";
  VARIABLE outline1 : LINE;
  VARIABLE outline2 : LINE;
BEGIN
  WRITE(outline1, header1);
  outline2 := outline1;
  IF (PrintDebugMsg = '1') THEN 
    WRITELINE (output, outline2); 
  END IF;
  WRITELINE (Logfile, outline1); 
  WRITE(outline1, header2);
  outline2 := outline1;
  IF (PrintDebugMsg = '1') THEN 
    WRITELINE (output, outline2);
  END IF; 
  WRITELINE (Logfile, outline1);    
  WRITE(outline1, header1);
  outline2 := outline1;
  IF (PrintDebugMsg = '1') THEN 
    WRITELINE (output, outline2); 
  END IF;
  WRITELINE (Logfile, outline1);
END ScheduleDebugMsg;   

-- ----------------------------------------------------------------------------
PROCEDURE PrintInputName(
  InputSignalName : IN STRING
) IS
  VARIABLE outline1 : LINE;
  VARIABLE outline2 : LINE;
  CONSTANT header1  : STRING := "***Changing input is ";
  CONSTANT header2  : STRING := "(";
  CONSTANT header3  : STRING := ")";
  CONSTANT header4  : STRING :=  "****";
  CONSTANT header5  : STRING := "******************************************";
  CONSTANT header6  : STRING:="Entering the process because of an i/p change";
  CONSTANT empty    : STRING := "  ";
BEGIN
  WRITE(outline1, header5);
  outline2 := outline1;
  WRITELINE (output, outline1); 
  WRITELINE (Logfile, outline2); 
  WRITE(outline1, header6);
  outline2 := outline1;
  WRITELINE (output, outline1); 
  WRITELINE (Logfile, outline2); 
  WRITE(outline1, header5);
  outline2 := outline1;
  WRITELINE (output, outline1); 
  WRITELINE (Logfile, outline2); 
  WRITE(outline1, header1);
  WRITE(outline1, InputSignalName); 
  WRITE(outline1, empty); 
  WRITE(outline1, now);
  WRITE(outline1, empty); 
  WRITE(outline1, header4);
  WRITELINE (output, outline1); 
  WRITELINE (Logfile, outline2);  
END PrintInputName;

-- ----------------------------------------------------------------------------
PROCEDURE PrintInputChangeTime(
  ChangeTimeArray : IN VitalTimeArrayT
) IS
  VARIABLE outline1 : LINE;
  VARIABLE outline2 : LINE;
  CONSTANT header5  : STRING := "*************************************";
  CONSTANT header6  : STRING:="ChangeTime Array : ";
  CONSTANT empty    : STRING := "  ";
BEGIN
  WRITE(outline1, header5);
  outline2 := outline1;
  IF (PrintDebugMsg = '1') THEN 
    WRITELINE (output, outline2); 
  END IF;
  WRITELINE (Logfile, outline1); 
  WRITE(outline1, header6);
  FOR i in ChangeTimeArray'range LOOP
    WRITE(outline1, ChangeTimeArray(i));
    WRITE(outline1, empty);
  END LOOP;
  outline2 := outline1;
  IF (PrintDebugMsg = '1') THEN  
    WRITELINE (output, outline2); 
  END IF;
  WRITELINE (Logfile, outline1); 
  WRITE(outline1, header5);
  outline2 := outline1;
  IF (PrintDebugMsg = '1') THEN  
    WRITELINE (output, outline2); 
  END IF;
  WRITELINE (Logfile, outline1);  
END PrintInputChangeTime;

-- ----------------------------------------------------------------------------
PROCEDURE PrintInputChangeTime(
  ChangeTime : IN Time
) IS
  VARIABLE ChangeTimeArray : VitalTimeArrayT(0 DOWNTO 0);
BEGIN
  ChangeTimeArray(0) := ChangeTime;
  PrintInputChangeTime(ChangeTimeArray);
END PrintInputChangeTime;

-- ----------------------------------------------------------------------------
-- for debug purpose
CONSTANT MaxNoInputBits : INTEGER := 1000;

TYPE VitalMemoryDelayType IS RECORD
  PropDelay         : TIME;
  OutputRetainDelay : TIME;
END RECORD;

-- ----------------------------------------------------------------------------
-- PROCEDURE:   IntToStr
--
-- PARAMETERS:  InputInt     - Integer to be converted to String.
--              ResultStr    - String buffer for converted Integer
--              AppendPos    - Position in buffer to place result
--
-- DESCRIPTION: This procedure is used to convert an input integer
--              into a string representation. The converted string
--              may be placed at a specific position in the result
--              buffer.
--
-- ----------------------------------------------------------------------------

PROCEDURE IntToStr ( 
  InputInt    : IN INTEGER ;
  ResultStr   : INOUT STRING ( 1 TO 256) ;
  AppendPos   : INOUT NATURAL
) IS
  -- Look-up table.  Given an int, we can get the character.
  TYPE     integer_table_type IS  ARRAY (0 TO 9) OF CHARACTER ;
  CONSTANT integer_table : integer_table_type :=
    ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9') ;
  --  Local variables used in this function.
  VARIABLE   inpVal      : INTEGER := inputInt ;
  VARIABLE   divisor     : INTEGER := 10 ;
  VARIABLE   tmpStrIndex : INTEGER := 1 ;
  VARIABLE   tmpStr      : STRING ( 1 TO 256 ) ;
BEGIN
  IF ( inpVal = 0 ) THEN
    tmpStr(tmpStrIndex) := integer_table ( 0 ) ;
    tmpStrIndex := tmpStrIndex + 1 ;
  ELSE
    WHILE ( inpVal > 0 )  LOOP
      tmpStr(tmpStrIndex) := integer_table (inpVal mod divisor);
      tmpStrIndex := tmpStrIndex + 1 ;
      inpVal := inpVal / divisor ;
    END LOOP ;
  END IF ;
  IF (appendPos /= 1 ) THEN
    resultStr(appendPos)  :=  ',' ;
    appendPos := appendPos + 1 ;
  END IF ;

  FOR i IN tmpStrIndex-1 DOWNTO 1  LOOP
    resultStr(appendPos)  :=  tmpStr(i) ;
    appendPos := appendPos + 1 ;
  END LOOP ;
END IntToStr ;

-- ----------------------------------------------------------------------------
TYPE CheckType IS (
  SetupCheck,
  HoldCheck,
  RecoveryCheck,
  RemovalCheck,
  PulseWidCheck,
  PeriodCheck 
);

TYPE CheckInfoType IS RECORD
  Violation : BOOLEAN;
  CheckKind : CheckType;
  ObsTime   : TIME;
  ExpTime   : TIME;
  DetTime   : TIME;
  State     : X01;
END RECORD;

TYPE LogicCvtTableType IS ARRAY (std_ulogic) OF CHARACTER; 
TYPE HiLoStrType IS ARRAY (std_ulogic RANGE 'X' TO '1') OF STRING(1 TO 4); 

CONSTANT LogicCvtTable : LogicCvtTableType 
               := ( 'U', 'X', '0', '1', 'Z', 'W', 'L', 'H', '-'); 
CONSTANT HiLoStr     : HiLoStrType := ("  X ", " Low", "High" ); 

TYPE EdgeSymbolMatchType IS ARRAY (X01,X01,VitalEdgeSymbolType) OF BOOLEAN;

-- last value, present value, edge symbol
CONSTANT EdgeSymbolMatch : EdgeSymbolMatchType :=  
  (
  'X' =>
    ( 'X'=>(                                 OTHERS => FALSE),
      '0'=>('N'|'F'|'v'|'E'|'D'|'*' => TRUE, OTHERS => FALSE ),
      '1'=>('P'|'R'|'^'|'E'|'A'|'*' => TRUE, OTHERS => FALSE ) 
    ),
  '0' =>
    ( 'X'=>(    'r'|'p'|'R'|'A'|'*' => TRUE, OTHERS => FALSE ),
      '0'=>(                                 OTHERS => FALSE ),
      '1'=>(    '/'|'P'|'p'|'R'|'*' => TRUE, OTHERS => FALSE ) 
    ),
  '1' =>
    ( 'X'=>(    'f'|'n'|'F'|'D'|'*' => TRUE, OTHERS => FALSE ),
      '0'=>(    '\'|'N'|'n'|'F'|'*' => TRUE, OTHERS => FALSE ),
      '1'=>(                                 OTHERS => FALSE ) 
    ) 
  );

-- ----------------------------------------------------------------------------
FUNCTION Minimum (
  CONSTANT t1, t2 : IN TIME 
) RETURN TIME IS
BEGIN
  IF (t1 < t2) THEN RETURN (t1); ELSE RETURN (t2); END IF;
END Minimum;

-- ----------------------------------------------------------------------------
FUNCTION Maximum (
  CONSTANT t1, t2 : IN TIME
) RETURN TIME IS
BEGIN
  IF (t1 < t2) THEN RETURN (t2); ELSE RETURN (t1); END IF;
END Maximum;

-- ----------------------------------------------------------------------------
-- FUNCTION:    VitalMemoryCalcDelay
-- Description: Select Transition dependent Delay. 
--              Used internally by VitalMemorySelectDelay.        
-- ----------------------------------------------------------------------------
FUNCTION VitalMemoryCalcDelay (
  CONSTANT NewVal : IN STD_ULOGIC := 'X';
  CONSTANT OldVal : IN STD_ULOGIC := 'X';
  CONSTANT Delay  : IN VitalDelayType01ZX
) RETURN VitalMemoryDelayType IS
  VARIABLE Result : VitalMemoryDelayType;
BEGIN
  CASE Oldval IS
  WHEN '0' | 'L' =>
    CASE Newval IS
      WHEN '0' | 'L' =>
        Result.PropDelay := Delay(tr10);
      WHEN '1' | 'H' =>
        Result.PropDelay := Delay(tr01);
      WHEN 'Z' =>
        Result.PropDelay := Delay(tr0Z);
      WHEN OTHERS =>
        Result.PropDelay := Minimum(Delay(tr01), Delay(tr0Z));
    END CASE;
    Result.OutputRetainDelay := Delay(tr0X);
  WHEN '1' | 'H' =>
    CASE Newval IS
      WHEN '0' | 'L' =>
        Result.PropDelay := Delay(tr10);
      WHEN '1' | 'H' =>
        Result.PropDelay := Delay(tr01);
      WHEN 'Z' =>
        Result.PropDelay := Delay(tr1Z);
      WHEN OTHERS =>
        Result.PropDelay := Minimum(Delay(tr10), Delay(tr1Z));
    END CASE;
    Result.OutputRetainDelay := Delay(tr1X);
  WHEN 'Z' =>
    CASE Newval IS
      WHEN '0' | 'L' =>
        Result.PropDelay := Delay(trZ0);
      WHEN '1' | 'H' =>
        Result.PropDelay := Delay(trZ1);
      WHEN 'Z' =>
        Result.PropDelay := Maximum(Delay(tr1Z), Delay(tr0Z));
      WHEN OTHERS =>
        Result.PropDelay := Minimum(Delay(trZ1), Delay(trZ0));
      END CASE;
      Result.OutputRetainDelay := Delay(trZX);
  WHEN OTHERS =>
    CASE Newval IS
      WHEN '0' | 'L' =>
        Result.PropDelay := Maximum(Delay(tr10), Delay(trZ0));
      WHEN '1' | 'H' =>
        Result.PropDelay := Maximum(Delay(tr01), Delay(trZ1));
      WHEN 'Z' =>
        Result.PropDelay := Maximum(Delay(tr1Z), Delay(tr0Z));
      WHEN OTHERS => 
        Result.PropDelay := Maximum(Delay(tr10), Delay(tr01));
    END CASE;
    Result.OutputRetainDelay := Minimum(Delay(tr1X), Delay(tr0X));
  END CASE;
  RETURN Result;
END VitalMemoryCalcDelay;

-- ----------------------------------------------------------------------------
FUNCTION VitalMemoryCalcDelay (
  CONSTANT NewVal : IN STD_ULOGIC   := 'X';
  CONSTANT OldVal : IN STD_ULOGIC   := 'X';
  CONSTANT Delay  : IN VitalDelayType01Z
) RETURN VitalMemoryDelayType IS
  VARIABLE Result : VitalMemoryDelayType;
BEGIN
CASE Oldval IS
  WHEN '0' | 'L' =>
    CASE Newval IS
      WHEN '0' | 'L' => Result.PropDelay := Delay(tr10);
      WHEN '1' | 'H' => Result.PropDelay := Delay(tr01);
      WHEN OTHERS =>
        Result.PropDelay := Minimum(Delay(tr01), Delay(tr10));
    END CASE;
    Result.OutputRetainDelay := Delay(tr0Z);
  WHEN '1' | 'H' =>
    CASE Newval IS
      WHEN '0' | 'L' => Result.PropDelay := Delay(tr10);
      WHEN '1' | 'H' => Result.PropDelay := Delay(tr01);
      WHEN OTHERS => 
        Result.PropDelay := Minimum(Delay(tr10), Delay(tr01));
    END CASE;
    Result.OutputRetainDelay := Delay(tr1Z);
  WHEN OTHERS =>
    Result.PropDelay := Maximum(Delay(tr10),Delay(tr01));
    Result.OutputRetainDelay := Minimum(Delay(tr1Z),Delay(tr0Z));
  END CASE;
  RETURN Result;
END VitalMemoryCalcDelay;

-- ----------------------------------------------------------------------------
PROCEDURE VitalMemoryUpdateInputChangeTime (
  VARIABLE InputChangeTimeArray : INOUT VitalTimeArrayT;
  SIGNAL   InputSignal          : IN STD_LOGIC_VECTOR;
  VARIABLE NumBitsPerSubword    : INTEGER
) IS
  VARIABLE LastInputValue : STD_LOGIC_VECTOR(InputSignal'LENGTH-1 downto 0);
  VARIABLE InSignalNorm   : STD_LOGIC_VECTOR(InputSignal'LENGTH-1 downto 0);
  VARIABLE ChangeTimeNorm : VitalTimeArrayT(InputSignal'LENGTH-1 downto 0);
  VARIABLE BitsPerWord    : INTEGER;
BEGIN
  LastInputValue := InputSignal'LAST_VALUE;
  IF NumBitsPerSubword = DefaultNumBitsPerSubword THEN
    BitsPerWord := InputSignal'LENGTH;
  ELSE
    BitsPerWord := NumBitsPerSubword;
  END IF;

  FOR i IN InSignalNorm'RANGE LOOP
    IF (InSignalNorm(i) /= LastInputValue(i)) THEN
      ChangeTimeNorm(i/BitsPerWord) := NOW - InputSignal'LAST_EVENT;
    ELSE
      ChangeTimeNorm(i/BitsPerWord) := InputChangeTimeArray(i);
    END IF;
  END LOOP;

  FOR i IN ChangeTimeNorm'RANGE LOOP
    ChangeTimeNorm(i) := ChangeTimeNorm(i/BitsPerword);
  END LOOP;

  InputChangeTimeArray := ChangeTimeNorm;

  -- for debug purpose only    
  PrintInputChangeTime(InputChangeTimeArray);
END VitalMemoryUpdateInputChangeTime;

-- ----------------------------------------------------------------------------
-- Procedure:   VitalMemoryUpdateInputChangeTime
-- Description: Time since previous event for each bit of the input
-- ----------------------------------------------------------------------------
PROCEDURE VitalMemoryUpdateInputChangeTime (
  VARIABLE InputChangeTimeArray : INOUT VitalTimeArrayT;
  SIGNAL   InputSignal          : IN STD_LOGIC_VECTOR
) IS
  VARIABLE LastInputValue  : STD_LOGIC_VECTOR(InputSignal'RANGE) ;
BEGIN
  LastInputValue := InputSignal'LAST_VALUE;        
  FOR i IN InputSignal'RANGE LOOP
    IF (InputSignal(i) /= LastInputValue(i)) THEN
      InputChangeTimeArray(i) := NOW - InputSignal'LAST_EVENT;
    END IF;
  END LOOP;
  -- for debug purpose only    
  PrintInputChangeTime(InputChangeTimeArray);
END VitalMemoryUpdateInputChangeTime;

-- ----------------------------------------------------------------------------
PROCEDURE VitalMemoryUpdateInputChangeTime (
  VARIABLE InputChangeTime      : INOUT TIME;
  SIGNAL   InputSignal          : IN STD_ULOGIC
) IS
BEGIN
  InputChangeTime := NOW - InputSignal'LAST_EVENT;
  -- for debug purpose only    
  PrintInputChangeTime(InputChangeTime);
END VitalMemoryUpdateInputChangeTime;

-- ----------------------------------------------------------------------------
PROCEDURE VitalMemoryExpandPortFlag (
  CONSTANT PortFlag             : IN  VitalPortFlagVectorType;
  CONSTANT NumBitsPerSubword    : IN  INTEGER;
  VARIABLE ExpandedPortFlag       : OUT VitalPortFlagVectorType
) IS
  VARIABLE PortFlagNorm : VitalPortFlagVectorType(
            PortFlag'LENGTH-1 downto 0) := PortFlag;
  VARIABLE ExpandedPortFlagNorm : VitalPortFlagVectorType(
            ExpandedPortFlag'LENGTH-1 downto 0);
  VARIABLE SubwordIndex : INTEGER;
BEGIN
  FOR Index IN INTEGER RANGE 0 to ExpandedPortFlag'LENGTH-1 LOOP
    IF NumBitsPerSubword = DefaultNumBitsPerSubword THEN
      SubwordIndex := 0;
    ELSE
      SubwordIndex := Index / NumBitsPerSubword;
    END IF;
    ExpandedPortFlagNorm(Index) := PortFlagNorm(SubWordIndex);
  END LOOP;
  ExpandedPortFlag := ExpandedPortFlagNorm;                               
END VitalMemoryExpandPortFlag;

-- ----------------------------------------------------------------------------
-- Procedure: VitalMemorySelectDelay
-- Description : Select Propagation Delay. Used internally by 
--               VitalMemoryAddPathDelay.
-- ----------------------------------------------------------------------------

-- ----------------------------------------------------------------------------
-- VitalDelayArrayType01ZX
-- ----------------------------------------------------------------------------
PROCEDURE VitalMemorySelectDelay (
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType;
  VARIABLE InputChangeTimeArray : IN VitalTimeArrayT;
  CONSTANT OutputSignalName     : IN STRING :="";
  CONSTANT PathDelayArray       : IN VitalDelayArrayType01ZX;
  CONSTANT ArcType              : IN VitalMemoryArcType;
  CONSTANT PathConditionArray   : IN VitalBoolArrayT;
  CONSTANT OutputRetainFlag     : IN BOOLEAN
) IS
  VARIABLE InputArrayLow      : INTEGER := 0;
  VARIABLE InputArrayHigh     : INTEGER := 0;
  VARIABLE DelayArrayIndex    : INTEGER := 0;
  VARIABLE NumBitsPerSubWord  : INTEGER := DefaultNumBitsPerSubword;
  VARIABLE NewValue           : STD_ULOGIC;
  VARIABLE OldValue           : STD_ULOGIC;
  VARIABLE OutputLength       : INTEGER := 0;
  VARIABLE OutArrayIndex      : INTEGER;
  VARIABLE PropDelay          : TIME;
  VARIABLE RetainDelay        : TIME;
  VARIABLE CurPropDelay       : TIME;
  VARIABLE CurRetainDelay     : TIME;
  VARIABLE InputAge           : TIME;
  VARIABLE CurInputAge        : TIME;
  VARIABLE InputChangeTimeNorm  : VitalTimeArrayT(
     InputChangeTimeArray'LENGTH-1 downto 0):=InputChangeTimeArray;
  VARIABLE DelayArrayNorm  : VitalDelayArrayType01ZX(
     PathDelayArray'LENGTH-1 downto 0):= PathDelayArray;
  VARIABLE ScheduleDataArrayNorm : VitalMemoryScheduleDatavectorType
     (ScheduleDataArray'LENGTH-1 downto 0):= ScheduleDataArray;
   
  -- for debug purpose
  VARIABLE debugprop : VitalTimeArrayT(MaxNoInputBits-1 downto 0);
  VARIABLE debugretain : VitalTimeArrayT(MaxNoInputBits-1 downto 0);

BEGIN

  -- for debug purpose
  PrintArcType(ArcType);

  OutputLength := ScheduleDataArray'LENGTH;        
  FOR OutBitPos IN 0 to (OutputLength -1) LOOP
    NEXT WHEN PathConditionArray(OutBitPos) = FALSE;

    NEXT WHEN ((ScheduleDataArrayNorm(OutBitPos).ScheduleValue 
      = ScheduleDataArrayNorm(OutBitPos).OutputData) AND 
      (ScheduleDataArrayNorm(OutBitPos).ScheduleTime <= NOW) AND
       (OutputRetainFlag = FALSE ));

    NewValue := ScheduleDataArrayNorm(OutBitPos).OutputData;
    OldValue := ScheduleDataArrayNorm(OutBitPos).LastOutputValue;
    PropDelay :=ScheduleDataArrayNorm(OutBitPos).PropDelay;
    InputAge := ScheduleDataArrayNorm(OutBitPos).InputAge;
    RetainDelay:=ScheduleDataArrayNorm(OutBitPos).OutputRetainDelay;
    NumBitsPerSubWord:=ScheduleDataArrayNorm(OutBitPos).NumBitsPerSubWord;

    CASE ArcType IS
      WHEN ParallelArc =>
        InputArrayLow := OutBitPos;
        InputArrayHigh :=  OutBitPos;
        DelayArrayIndex :=  OutBitPos;
      WHEN CrossArc =>
        InputArrayLow := 0;
        InputArrayHigh := InputChangeTimeArray'LENGTH - 1 ;              
        DelayArrayIndex := OutBitPos; 
      WHEN SubwordArc =>
        InputArrayLow := OutBitPos / NumBitsPerSubWord;
        InputArrayHigh := OutBitPos / NumBitsPerSubWord;
        DelayArrayIndex := OutBitPos + 
            (OutputLength * (OutBitPos / NumBitsPerSubWord));  
    END CASE;

    FOR i IN InputArrayLow TO InputArrayHigh LOOP
      (CurPropDelay,CurRetainDelay) := 
        VitalMemoryCalcDelay (
          NewValue, OldValue, DelayArrayNorm(DelayArrayIndex)
        );
      IF (OutputRetainFlag = FALSE) THEN
        CurRetainDelay := TIME'HIGH;
      END IF;              

      -- for debug purpose
      debugprop(i) := CurPropDelay;
      debugretain(i) := CurRetainDelay;

      IF ArcType = CrossArc THEN
        DelayArrayIndex := DelayArrayIndex + OutputLength;
      END IF;

      -- If there is one input change at a time, then choose the
      -- delay from that input. If there is simultaneous input 
      -- change, then choose the minimum of propagation delays 
 
      IF (InputChangeTimeNorm(i) < 0 ns)THEN
        CurInputAge := TIME'HIGH;
      ELSE
        CurInputAge := NOW - InputChangeTimeNorm(i);
      END IF;            

      IF (CurInputAge  < InputAge)THEN
        PropDelay := CurPropDelay;
        RetainDelay := CurRetainDelay;
        InputAge := CurInputAge;
      ELSIF (CurInputAge = InputAge)THEN
        IF (CurPropDelay < PropDelay) THEN
          PropDelay := CurPropDelay;
        END IF;
        IF (OutputRetainFlag = TRUE) THEN
          IF (CurRetainDelay < RetainDelay) THEN
            RetainDelay := CurRetainDelay;
          END IF;
        END IF;
      END IF;
    END LOOP; 

    -- Store it back to data strucutre
    ScheduleDataArrayNorm(OutBitPos).PropDelay := PropDelay;
    ScheduleDataArrayNorm(OutBitPos).OutputRetainDelay:= RetainDelay;
    ScheduleDataArrayNorm(OutBitPos).InputAge := InputAge;

    -- for debug purpose 
    PrintDelay(outbitPos,InputArrayLow, InputArrayHigh,
      debugprop, debugretain);
  END LOOP;

  ScheduleDataArray := ScheduleDataArrayNorm;

END VitalMemorySelectDelay;

-- ----------------------------------------------------------------------------
-- VitalDelayArrayType01Z
-- ----------------------------------------------------------------------------
PROCEDURE VitalMemorySelectDelay (
  VARIABLE ScheduleDataArray    : INOUT VitalMemoryScheduleDataVectorType;
  VARIABLE InputChangeTimeArray : IN VitalTimeArrayT;
  CONSTANT OutputSignalName     : IN STRING :="";
  CONSTANT PathDelayArray       : IN VitalDelayArrayType01Z;
  CONSTANT ArcType              : IN VitalMemoryArcType;
  CONSTANT PathConditionArray   : IN VitalBoolArrayT;
  CONSTANT OutputRetainFlag     : IN BOOLEAN
) IS
  VARIABLE InputArrayLow      : INTEGER := 0;
  VARIABLE InputArrayHigh     : INTEGER := 0;
  VARIABLE DelayArrayIndex    : INTEGER := 0;
  VARIABLE NumBitsPerSubWord  : INTEGER := DefaultNumBitsPerSubword;
  VARIABLE NewValue           : STD_ULOGIC;
  VARIABLE OldValue           : STD_ULOGIC;
  VARIABLE OutputLength       : INTEGER := 0;
  VARIABLE OutArrayIndex      : INTEGER;
  VARIABLE PropDelay          : TIME;
  VARIABLE RetainDelay        : TIME;
  VARIABLE CurPropDelay       : TIME;
  VARIABLE CurRetainDelay     : TIME;
  VARIABLE InputAge           : TIME;
  VARIABLE CurInputAge        : TIME;
  VARIABLE InputChangeTimeNorm  : VitalTimeArrayT(
     InputChangeTimeArray'LENGTH-1 downto 0):=InputChangeTimeArray;
  VARIABLE DelayArrayNorm  : VitalDelayArrayType01Z(
     PathDelayArray'LENGTH-1 downto 0):= PathDelayArray;
  VARIABLE ScheduleDataArrayNorm : VitalMemoryScheduleDatavectorType
     (ScheduleDataArray'LENGTH-1 downto 0):=ScheduleDataArray;
   
  -- for debug purpose
  VARIABLE debugprop : VitalTimeArrayT(MaxNoInputBits-1 downto 0);
  VARIABLE debugretain : VitalTimeArrayT(MaxNoInputBits-1 downto 0);
BEGIN

  -- for debug purpose
  PrintArcType(ArcType);

  OutputLength := ScheduleDataArray'LENGTH;  
  FOR OutBitPos IN 0 to (OutputLength -1) LOOP
    NEXT WHEN PathConditionArray(OutBitPos) = FALSE;

    NEXT WHEN ((ScheduleDataArrayNorm(OutBitPos).ScheduleValue 
      = ScheduleDataArrayNorm(OutBitPos).OutputData) AND 
        (ScheduleDataArrayNorm(OutBitPos).ScheduleTime <= NOW) AND
        (OutputRetainFlag = FALSE));

    NewValue := ScheduleDataArrayNorm(OutBitPos).OutputData;
    OldValue := ScheduleDataArrayNorm(OutBitPos).LastOutputValue;
    PropDelay :=ScheduleDataArrayNorm(OutBitPos).PropDelay;
    InputAge := ScheduleDataArrayNorm(OutBitPos).InputAge;
    RetainDelay:=ScheduleDataArrayNorm(OutBitPos).OutputRetainDelay;
    NumBitsPerSubWord:=ScheduleDataArrayNorm(OutBitPos).NumBitsPerSubWord;

    CASE ArcType IS
      WHEN ParallelArc =>
        InputArrayLow := OutBitPos;
        InputArrayHigh :=  OutBitPos;
        DelayArrayIndex :=  OutBitPos;
      WHEN CrossArc =>
        InputArrayLow := 0;
        InputArrayHigh := InputChangeTimeArray'LENGTH-1;
        DelayArrayIndex := OutBitPos; 
      WHEN SubwordArc =>
        InputArrayLow := OutBitPos / NumBitsPerSubWord;
        InputArrayHigh := OutBitPos / NumBitsPerSubWord;
        DelayArrayIndex := OutBitPos + 
          (OutputLength * (OutBitPos / NumBitsPerSubWord));  
    END CASE;

    FOR i IN InputArrayLow TO InputArrayHigh LOOP
      (CurPropDelay, CurRetainDelay) := 
        VitalMemoryCalcDelay (
          NewValue, OldValue, DelayArrayNorm(DelayArrayIndex)
        );
      IF (OutputRetainFlag = FALSE) THEN
        CurRetainDelay := TIME'HIGH;
      END IF;
        
      -- for debug purpose
      debugprop(i) := CurPropDelay;
      debugretain(i) := CurRetainDelay;
                       
      IF (ArcType = CrossArc) THEN
        DelayArrayIndex := DelayArrayIndex + OutputLength;
      END IF;

      -- If there is one input change at a time, then choose the
      -- delay from that input. If there is simultaneous input 
      -- change, then choose the minimum of propagation delays 

      IF (InputChangeTimeNorm(i) < 0 ns) THEN
        CurInputAge := TIME'HIGH;
      ELSE
        CurInputAge := NOW - InputChangeTimeNorm(i);
      END IF;            

      IF (CurInputAge  < InputAge) THEN
        PropDelay := CurPropDelay;
        RetainDelay := CurRetainDelay;
        InputAge := CurInputAge;
      ELSIF (CurInputAge = InputAge) THEN
        IF (CurPropDelay < PropDelay) THEN
          PropDelay := CurPropDelay;
        END IF;
        IF (OutputRetainFlag = TRUE) THEN
          IF (CurRetainDelay < RetainDelay) THEN
            RetainDelay := CurRetainDelay;
          END IF;
        END IF;
      END IF;
    END LOOP; 

    -- Store it back to data strucutre
    ScheduleDataArrayNorm(OutBitPos).PropDelay := PropDelay;
    ScheduleDataArrayNorm(OutBitPos).OutputRetainDelay:= RetainDelay;
    ScheduleDataArrayNorm(OutBitPos).InputAge := InputAge;

    -- for debug purpose 
    PrintDelay(outbitPos, InputArrayLow, InputArrayHigh,
       debugprop, debugretain);
  END LOOP;

  ScheduleDataArray := ScheduleDataArrayNorm;

END VitalMemorySelectDelay;

-- ----------------------------------------------------------------------------
-- VitalDelayArrayType01
-- ----------------------------------------------------------------------------
PROCEDURE VitalMemorySelectDelay (
  VARIABLE ScheduleDataArray    : INOUT VitalMemoryScheduleDataVectorType;
  VARIABLE InputChangeTimeArray : IN    VitalTimeArrayT;
  CONSTANT OutputSignalName     : IN    STRING :="";
  CONSTANT PathDelayArray       : IN    VitalDelayArrayType01;
  CONSTANT ArcType              : IN    VitalMemoryArcType;
  CONSTANT PathConditionArray   : IN VitalBoolArrayT
) IS
  VARIABLE CurPathDelay       : VitalMemoryDelayType;
  VARIABLE InputArrayLow      : INTEGER := 0;
  VARIABLE InputArrayHigh     : INTEGER := 0;
  VARIABLE DelayArrayIndex    : INTEGER := 0;
  VARIABLE NumBitsPerSubWord  : INTEGER := DefaultNumBitsPerSubword;
  VARIABLE NewValue           : STD_ULOGIC;
  VARIABLE OldValue           : STD_ULOGIC;
  VARIABLE OutputLength       : INTEGER := 0;
  VARIABLE OutArrayIndex      : INTEGER;
  VARIABLE PropDelay          : TIME;
  VARIABLE CurPropDelay       : TIME;
  VARIABLE InputAge           : TIME;
  VARIABLE CurInputAge        : TIME;
  VARIABLE InputChangeTimeNorm  : VitalTimeArrayT(
     InputChangeTimeArray'LENGTH-1 downto 0):= InputChangeTimeArray;
  VARIABLE DelayArrayNorm  : VitalDelayArrayType01(
     PathDelayArray'LENGTH-1 downto 0):= PathDelayArray;
  VARIABLE ScheduleDataArrayNorm : VitalMemoryScheduleDatavectorType
     (ScheduleDataArray'LENGTH-1 downto 0):=ScheduleDataArray;
   
  -- for debug purpose
  VARIABLE debugprop : VitalTimeArrayT(MaxNoInputBits-1 downto 0);
  VARIABLE debugretain : VitalTimeArrayT(MaxNoInputBits-1 downto 0);
BEGIN

  -- for debug purpose
  PrintArcType(ArcType);

  OutputLength := ScheduleDataArray'LENGTH;
  FOR OutBitPos IN 0 to (OutputLength -1) LOOP
    NEXT WHEN PathConditionArray(OutBitPos) = FALSE;

    NEXT WHEN ((ScheduleDataArrayNorm(OutBitPos).ScheduleValue 
    = ScheduleDataArrayNorm(OutBitPos).OutputData) AND 
    (ScheduleDataArrayNorm(OutBitPos).ScheduleTime <= NOW));

    NewValue := ScheduleDataArrayNorm(OutBitPos).OutputData;
    OldValue := ScheduleDataArrayNorm(OutBitPos).LastOutputValue;
    PropDelay :=ScheduleDataArrayNorm(OutBitPos).PropDelay;
    InputAge := ScheduleDataArrayNorm(OutBitPos).InputAge;
    NumBitsPerSubWord:=ScheduleDataArrayNorm(OutBitPos).NumBitsPerSubWord;

    CASE ArcType IS
      WHEN ParallelArc =>
        InputArrayLow := OutBitPos;
        InputArrayHigh :=  OutBitPos;
        DelayArrayIndex :=  OutBitPos;
      WHEN CrossArc =>
        InputArrayLow := 0;
        InputArrayHigh := InputChangeTimeArray'LENGTH-1;
        DelayArrayIndex := OutBitPos; 
      WHEN SubwordArc =>
        InputArrayLow := OutBitPos / NumBitsPerSubWord;
        InputArrayHigh := OutBitPos / NumBitsPerSubWord;
        DelayArrayIndex := OutBitPos + 
          (OutputLength * (OutBitPos / NumBitsPerSubWord));  
    END CASE;

    FOR i IN InputArrayLow TO InputArrayHigh LOOP
      CurPropDelay:= VitalCalcDelay (NewValue, 
                  OldValue, DelayArrayNorm(DelayArrayIndex));

      -- for debug purpose
      debugprop(i) := CurPropDelay;
      debugretain(i) := TIME'HIGH;
                       
      IF (ArcType = CrossArc) THEN
        DelayArrayIndex := DelayArrayIndex + OutputLength;
      END IF;

      -- If there is one input change at a time, then choose the
      -- delay from that input. If there is simultaneous input 
      -- change, then choose the minimum of propagation delays 

      IF (InputChangeTimeNorm(i) < 0 ns) THEN
        CurInputAge := TIME'HIGH;
      ELSE
        CurInputAge := NOW - InputChangeTimeNorm(i);
      END IF;            
      IF (CurInputAge  < InputAge) THEN
        PropDelay := CurPropDelay;
        InputAge := CurInputAge;
      ELSIF (CurInputAge = InputAge) THEN
        IF (CurPropDelay < PropDelay) THEN
          PropDelay := CurPropDelay;
        END IF;
      END IF;
    END LOOP;  

    -- Store it back to data strucutre
    ScheduleDataArrayNorm(OutBitPos).PropDelay := PropDelay;
    ScheduleDataArrayNorm(OutBitPos).InputAge := InputAge;

    -- for debug purpose 
    PrintDelay(outbitPos, InputArrayLow, InputArrayHigh,
      debugprop, debugretain);
  END LOOP;

  ScheduleDataArray := ScheduleDataArrayNorm;

END VitalMemorySelectDelay;

-- ----------------------------------------------------------------------------
-- VitalDelayArrayType
-- ----------------------------------------------------------------------------
PROCEDURE VitalMemorySelectDelay (
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType;
  VARIABLE InputChangeTimeArray : IN VitalTimeArrayT;
  CONSTANT OutputSignalName     : IN STRING :="";
  CONSTANT PathDelayArray       : IN VitalDelayArrayType;
  CONSTANT ArcType              : IN VitalMemoryArcType;
  CONSTANT PathConditionArray   : IN VitalBoolArrayT
) IS
  VARIABLE InputArrayLow      : INTEGER := 0;
  VARIABLE InputArrayHigh     : INTEGER := 0;
  VARIABLE DelayArrayIndex    : INTEGER := 0;
  VARIABLE NumBitsPerSubWord  : INTEGER := DefaultNumBitsPerSubword;
  VARIABLE NewValue           : STD_ULOGIC;
  VARIABLE OldValue           : STD_ULOGIC;
  VARIABLE OutputLength       : INTEGER := 0;
  VARIABLE OutArrayIndex      : INTEGER;
  VARIABLE PropDelay          : TIME;
  VARIABLE CurPropDelay       : TIME;
  VARIABLE InputAge           : TIME;
  VARIABLE CurInputAge        : TIME;
  VARIABLE InputChangeTimeNorm  : VitalTimeArrayT(
     InputChangeTimeArray'LENGTH-1 downto 0) := InputChangeTimeArray;
  VARIABLE DelayArrayNorm  : VitalDelayArrayType(
     PathDelayArray'LENGTH-1 downto 0) := PathDelayArray;
  VARIABLE ScheduleDataArrayNorm : VitalMemoryScheduleDatavectorType
     (ScheduleDataArray'LENGTH-1 downto 0) := ScheduleDataArray;
   
  -- for debug purpose
  VARIABLE debugprop : VitalTimeArrayT(MaxNoInputBits-1 downto 0);
  VARIABLE debugretain : VitalTimeArrayT(MaxNoInputBits-1 downto 0);
BEGIN
  
  -- for debug purpose
  PrintArcType(ArcType);

  OutputLength := ScheduleDataArray'LENGTH;
  FOR OutBitPos IN 0 to (OutputLength -1) LOOP
    NEXT WHEN PathConditionArray(OutBitPos) = FALSE;

    NEXT WHEN ((ScheduleDataArrayNorm(OutBitPos).ScheduleValue 
      = ScheduleDataArrayNorm(OutBitPos).OutputData) AND 
        (ScheduleDataArrayNorm(OutBitPos).ScheduleTime <= NOW));

    NewValue := ScheduleDataArrayNorm(OutBitPos).OutputData;
    OldValue := ScheduleDataArrayNorm(OutBitPos).LastOutputValue;
    PropDelay :=ScheduleDataArrayNorm(OutBitPos).PropDelay;
    InputAge := ScheduleDataArrayNorm(OutBitPos).InputAge;
    NumBitsPerSubWord:=ScheduleDataArrayNorm(OutBitPos).NumBitsPerSubWord;

    CASE ArcType IS
      WHEN ParallelArc =>
        InputArrayLow := OutBitPos;
        InputArrayHigh :=  OutBitPos;
        DelayArrayIndex :=  OutBitPos;
      WHEN CrossArc =>
        InputArrayLow := 0;
        InputArrayHigh := InputChangeTimeArray'LENGTH-1;
        DelayArrayIndex := OutBitPos; 
      WHEN SubwordArc =>
        InputArrayLow := OutBitPos / NumBitsPerSubWord;
        InputArrayHigh := OutBitPos / NumBitsPerSubWord;
        DelayArrayIndex := OutBitPos + 
          (OutputLength * (OutBitPos / NumBitsPerSubWord));  
    END CASE;

    FOR i IN InputArrayLow TO InputArrayHigh LOOP
      CurPropDelay := VitalCalcDelay (NewValue, 
        OldValue, DelayArrayNorm(DelayArrayIndex)); 

      -- for debug purpose
      debugprop(i) := CurPropDelay;
      debugretain(i) := TIME'HIGH;
                   
      IF (ArcType = CrossArc) THEN
        DelayArrayIndex := DelayArrayIndex + OutputLength;
      END IF;

      -- If there is one input change at a time, then choose the
      -- delay from that input. If there is simultaneous input 
      -- change, then choose the minimum of propagation delays 

      IF (InputChangeTimeNorm(i) < 0 ns) THEN
        CurInputAge := TIME'HIGH;
      ELSE
        CurInputAge := NOW - InputChangeTimeNorm(i);
      END IF;            

      IF (CurInputAge  < InputAge) THEN
        PropDelay := CurPropDelay;
        InputAge := CurInputAge;
      ELSIF (CurInputAge = InputAge) THEN
        IF (CurPropDelay < PropDelay) THEN
          PropDelay := CurPropDelay;
        END IF;
      END IF;
    END LOOP;  

    -- Store it back to data strucutre
    ScheduleDataArrayNorm(OutBitPos).PropDelay := PropDelay;
    ScheduleDataArrayNorm(OutBitPos).InputAge := InputAge;

    -- for debug purpose 
    PrintDelay(outbitPos, InputArrayLow, InputArrayHigh,
      debugprop, debugretain);
  END LOOP;

  ScheduleDataArray := ScheduleDataArrayNorm;

END VitalMemorySelectDelay;

-- ----------------------------------------------------------------------------
-- Procedure:   VitalMemoryInitPathDelay
-- Description: To initialize Schedule Data structure for an
--              output.
-- ----------------------------------------------------------------------------
PROCEDURE VitalMemoryInitPathDelay (
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType;
  VARIABLE OutputDataArray   : IN STD_LOGIC_VECTOR;
  CONSTANT NumBitsPerSubWord : IN INTEGER := DefaultNumBitsPerSubword
) IS
BEGIN
  -- Initialize the ScheduleData Structure.
  FOR i IN OutputDataArray'RANGE LOOP        
    ScheduleDataArray(i).OutputData    := OutputDataArray(i); 
    ScheduleDataArray(i).PropDelay     := TIME'HIGH;
    ScheduleDataArray(i).OutputRetainDelay := TIME'HIGH;
    ScheduleDataArray(i).InputAge      := TIME'HIGH;
    ScheduleDataArray(i).NumBitsPerSubWord := NumBitsPerSubWord;

    -- Update LastOutputValue of Output if the Output has 
    -- already been scheduled.
    IF ((ScheduleDataArray(i).ScheduleValue /= OutputDataArray(i)) AND
        (ScheduleDataArray(i).ScheduleTime <= NOW)) THEN
       ScheduleDataArray(i).LastOutputValue 
                := ScheduleDataArray(i).ScheduleValue;
    END IF;                                   
  END LOOP; 

  -- for debug purpose
  DebugMsg1;
  PrintScheduleDataArray(ScheduleDataArray);

END VitalMemoryInitPathDelay;

-- ----------------------------------------------------------------------------
PROCEDURE VitalMemoryInitPathDelay (
  VARIABLE ScheduleData : INOUT VitalMemoryScheduleDataType;
  VARIABLE OutputData   : IN STD_ULOGIC
) IS
  VARIABLE ScheduledataArray: VitalMemoryScheduleDataVectorType
                                        (0 downto 0);
  VARIABLE OutputDataArray  : STD_LOGIC_VECTOR(0 downto 0);
BEGIN
  ScheduledataArray(0) := ScheduleData;
  OutputDataArray(0)   := OutputData;
  VitalMemoryInitPathDelay (
    ScheduleDataArray => ScheduleDataArray,
    OutputDataArray   => OutputDataArray,
    NumBitsPerSubWord => DefaultNumBitsPerSubword
  );

  -- for debug purpose
  DebugMsg1;
  PrintScheduleDataArray( ScheduleDataArray);

END VitalMemoryInitPathDelay;

-- ----------------------------------------------------------------------------
-- Procedure:   VitalMemoryAddPathDelay
-- Description: Declare a path for one scalar/vector input to 
--              the output for which Schedule Data has been
--              initialized previously.
-- ----------------------------------------------------------------------------

-- ----------------------------------------------------------------------------
-- #1
-- DelayType - VitalMemoryDelayType
-- Input     - Scalar 
-- Output    - Scalar 
-- Delay     - Scalar 
-- Condition - Scalar 
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleData      : INOUT VitalMemoryScheduleDataType;
  SIGNAL   InputSignal       : IN STD_ULOGIC;
  CONSTANT OutputSignalName  : IN STRING :="";
  VARIABLE InputChangeTime   : INOUT TIME;
  CONSTANT PathDelay         : IN VitalDelayType;
  CONSTANT ArcType           : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathCondition     : IN BOOLEAN := TRUE
) IS
  VARIABLE ScheduleDataArray : 
              VitalMemoryScheduleDataVectorType(0 downto 0);
  VARIABLE PathDelayArray : VitalDelayArrayType(0 downto 0);
  VARIABLE InputChangeTimeArray : VitalTimeArrayT(0 downto 0); 
  VARIABLE PathConditionArray : VitalBoolArrayT(0 downto 0); 
BEGIN
  PathConditionArray(0) := PathCondition;
  ScheduleDataArray(0) := ScheduleData;
  PathDelayArray(0) := PathDelay;
  VitalMemoryUpdateInputChangeTime(InputChangeTime, InputSignal);
  InputChangeTimeArray(0) := InputChangeTime;          
   
  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray,
    OutputSignalName, PathDelayArray, 
    ArcType, PathConditionArray);
END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- #2
-- DelayType - VitalMemoryDelayType
-- Input     - Scalar 
-- Output    - Vector 
-- Delay     - Vector 
-- Condition - Scalar 
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray  : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal        : IN STD_ULOGIC;
  CONSTANT OutputSignalName   : IN STRING :="";
  VARIABLE InputChangeTime    : INOUT TIME;
  CONSTANT PathDelayArray     : IN VitalDelayArrayType;
  CONSTANT ArcType            : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathCondition      : IN BOOLEAN := TRUE
) IS
  VARIABLE InputChangeTimeArray : VitalTimeArrayT(0 downto 0);
  VARIABLE PathConditionArray :
      VitalBoolArrayT(ScheduleDataArray'LENGTH-1 downto 0);
BEGIN
  FOR i IN PathConditionArray'RANGE LOOP
    PathConditionArray(i) := PathCondition;
  END LOOP;

  VitalMemoryUpdateInputChangeTime(InputChangeTime, InputSignal);
  InputChangeTimeArray(0) := InputChangeTime; 

  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray, 
    OutputSignalName, PathDelayArray, 
    ArcType, PathConditionArray
  );
END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- #3
-- DelayType - VitalMemoryDelayType
-- Input     - Scalar
-- Output    - Vector
-- Delay     - Vector
-- Condition - Vector
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray  : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal        : IN STD_ULOGIC;
  CONSTANT OutputSignalName   : IN STRING :="";
  VARIABLE InputChangeTime    : INOUT TIME;
  CONSTANT PathDelayArray     : IN VitalDelayArrayType;
  CONSTANT ArcType            : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathConditionArray : IN VitalBoolArrayT
) IS
  VARIABLE InputChangeTimeArray : VitalTimeArrayT(0 downto 0);
  VARIABLE NumBitsPerSubword : INTEGER;
  VARIABLE PathConditionArrayNorm : 
      VitalBoolArrayT(PathConditionArray'LENGTH-1 downto 0) := PathConditionArray; -- IR Mem400 
  VARIABLE PathConditionArrayExp : 
      VitalBoolArrayT(ScheduleDataArray'LENGTH-1 downto 0); 
BEGIN
  NumBitsPerSubword := 
    ScheduleDataArray(ScheduleDataArray'RIGHT).NumBitsPerSubword;
  FOR i IN PathConditionArrayExp'RANGE LOOP
    PathConditionArrayExp(i) := PathConditionArrayNorm(i/NumBitsPerSubword);
  END LOOP;

  VitalMemoryUpdateInputChangeTime(InputChangeTime, InputSignal);
  InputChangeTimeArray(0) := InputChangeTime;

  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray,
    OutputSignalName, PathDelayArray, 
    ArcType, PathConditionArrayExp);
END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- #4
-- DelayType - VitalMemoryDelayType
-- Input     - Vector
-- Output    - Scalar 
-- Delay     - Vector 
-- Condition - Scalar 
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleData         : INOUT VitalMemoryScheduleDataType;
  SIGNAL   InputSignal          : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName     : IN STRING :="";
  VARIABLE InputChangeTimeArray : INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray       : IN VitalDelayArrayType;
  CONSTANT ArcType              : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathCondition        : IN BOOLEAN := TRUE
) IS
  VARIABLE ScheduleDataArray : VitalMemoryScheduleDataVectorType(0 downto 0);
  VARIABLE PathConditionArray : VitalBoolArrayT(0 downto 0); 
BEGIN
  PathConditionArray(0) := PathCondition;

  ScheduleDataArray(0) := ScheduleData;
  VitalMemoryUpdateInputChangeTime(InputChangeTimeArray, InputSignal);
    
  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray, 
    OutputSignalName, PathDelayArray, 
    ArcType, PathConditionArray);
END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- #5
-- DelayType - VitalMemoryDelayType
-- Input     - Vector
-- Output    - Vector
-- Delay     - Vector 
-- Condition - Scalar 
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray  : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal        : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName   : IN STRING :="";
  VARIABLE InputChangeTimeArray  : INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray        : IN VitalDelayArrayType;
  CONSTANT ArcType               : IN VitalMemoryArcType := CrossArc; 
  CONSTANT PathCondition         : IN BOOLEAN := TRUE
) IS
  VARIABLE PathConditionArray : 
      VitalBoolArrayT(ScheduleDataArray'LENGTH-1 downto 0); 
BEGIN
  FOR i IN PathConditionArray'RANGE LOOP
    PathConditionArray(i) := PathCondition;
  END LOOP;

  VitalMemoryUpdateInputChangeTime(InputChangeTimeArray, InputSignal); 

  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray, 
    OutputSignalName, PathDelayArray,
    ArcType, PathConditionArray);
END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- #6
-- DelayType - VitalMemoryDelayType
-- Input     - Vector
-- Output    - Vector
-- Delay     - Vector
-- Condition - Vector
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray  : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal        : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName   : IN STRING :="";
  VARIABLE InputChangeTimeArray  : INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray        : IN VitalDelayArrayType;
  CONSTANT ArcType               : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathConditionArray    : IN VitalBoolArrayT
) IS
  VARIABLE NumBitsPerSubword : INTEGER;
  VARIABLE PathConditionArrayNorm : 
      VitalBoolArrayT(PathConditionArray'LENGTH-1 downto 0) := PathConditionArray; -- IR MEM400; 
  VARIABLE PathConditionArrayExp : 
      VitalBoolArrayT(ScheduleDataArray'LENGTH-1 downto 0); 
BEGIN
  NumBitsPerSubword := 
      ScheduleDataArray(ScheduleDataArray'RIGHT).NumBitsPerSubword;
  FOR i IN PathConditionArrayExp'RANGE LOOP
    PathConditionArrayExp(i) := PathConditionArrayNorm(i/NumBitsPerSubword);
  END LOOP;

  VitalMemoryUpdateInputChangeTime(InputChangeTimeArray, InputSignal);

  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray,
    OutputSignalName, PathDelayArray, 
    ArcType, PathConditionArrayExp);
END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- #7
-- DelayType - VitalMemoryDelayType01
-- Input     - Scalar
-- Output    - Scalar
-- Delay     - Scalar 
-- Condition - Scalar 
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleData      : INOUT VitalMemoryScheduleDataType;
  SIGNAL   InputSignal       : IN STD_ULOGIC;
  CONSTANT OutputSignalName  : IN STRING :="";
  VARIABLE InputChangeTime   : INOUT TIME;
  CONSTANT PathDelay         : IN VitalDelayType01;
  CONSTANT ArcType           : IN VitalMemoryArcType := CrossArc; 
  CONSTANT PathCondition     : IN BOOLEAN := TRUE
) IS
  VARIABLE ScheduleDataArray : 
           VitalMemoryScheduleDataVectorType(0 downto 0);
  VARIABLE PathDelayArray : VitalDelayArrayType01(0 downto 0);
  VARIABLE InputChangeTimeArray : VitalTimeArrayT(0 downto 0); 
  VARIABLE PathConditionArray : VitalBoolArrayT(0 downto 0); 
BEGIN
  PathConditionArray(0) := PathCondition;
  ScheduleDataArray(0) := ScheduleData;
  PathDelayArray(0) := PathDelay;
  VitalMemoryUpdateInputChangeTime(InputChangeTime, InputSignal);
  InputChangeTimeArray(0) := InputChangeTime;

  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray, 
    OutputSignalName, PathDelayArray, 
    ArcType, PathConditionArray);
END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- #8
-- DelayType - VitalMemoryDelayType01
-- Input     - Scalar
-- Output    - Vector
-- Delay     - Vector
-- Condition - Scalar 
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_ULOGIC;
  CONSTANT OutputSignalName  : IN STRING :="";
  VARIABLE InputChangeTime   : INOUT TIME;
  CONSTANT PathDelayArray    : IN VitalDelayArrayType01;
  CONSTANT ArcType           : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathCondition     : IN BOOLEAN := TRUE
) IS
  VARIABLE InputChangeTimeArray : VitalTimeArrayT(0 downto 0);
  VARIABLE PathConditionArray : 
      VitalBoolArrayT(ScheduleDataArray'LENGTH-1 downto 0); 
BEGIN
  FOR i IN PathConditionArray'RANGE LOOP
    PathConditionArray(i) := PathCondition;
  END LOOP;

  VitalMemoryUpdateInputChangeTime(InputChangeTime, InputSignal);
  InputChangeTimeArray(0) := InputChangeTime;

  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray, 
    OutputSignalName, PathDelayArray, 
    ArcType, PathConditionArray);
END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- #9
-- DelayType - VitalMemoryDelayType01
-- Input     - Scalar
-- Output    - Vector
-- Delay     - Vector
-- Condition - Vector
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_ULOGIC;
  CONSTANT OutputSignalName  : IN STRING :="";
  VARIABLE InputChangeTime   : INOUT TIME;
  CONSTANT PathDelayArray    : IN VitalDelayArrayType01;
  CONSTANT ArcType           : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathConditionArray: IN VitalBoolArrayT
) IS
  VARIABLE InputChangeTimeArray : VitalTimeArrayT(0 downto 0);
  VARIABLE NumBitsPerSubword : INTEGER;
  VARIABLE PathConditionArrayNorm : 
      VitalBoolArrayT(PathConditionArray'LENGTH-1 downto 0) := PathConditionArray; -- IR MEM400; 
  VARIABLE PathConditionArrayExp : 
      VitalBoolArrayT(ScheduleDataArray'LENGTH-1 downto 0); 
BEGIN
  NumBitsPerSubword := 
      ScheduleDataArray(ScheduleDataArray'RIGHT).NumBitsPerSubword;
  FOR i IN PathConditionArrayExp'RANGE LOOP
    PathConditionArrayExp(i) := PathConditionArrayNorm(i/NumBitsPerSubword);
  END LOOP;

  VitalMemoryUpdateInputChangeTime(InputChangeTime, InputSignal);
  InputChangeTimeArray(0) := InputChangeTime;

  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray,
    OutputSignalName, PathDelayArray, 
    ArcType, PathConditionArrayExp);
END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- #10
-- DelayType - VitalMemoryDelayType01
-- Input     - Vector
-- Output    - Scalar
-- Delay     - Vector
-- Condition - Scalar 
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleData        : INOUT VitalMemoryScheduleDataType;
  SIGNAL   InputSignal         : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName    : IN STRING :="";
  VARIABLE InputChangeTimeArray: INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray      : IN VitalDelayArrayType01;
  CONSTANT ArcType             : IN VitalMemoryArcType := CrossArc; 
  CONSTANT PathCondition       : IN BOOLEAN := TRUE
)IS 
  VARIABLE ScheduleDataArray : 
      VitalMemoryScheduleDataVectorType(0 downto 0);
  VARIABLE PathConditionArray : VitalBoolArrayT(0 downto 0); 
BEGIN
  PathConditionArray(0) := PathCondition;
  ScheduleDataArray(0) := ScheduleData;
  VitalMemoryUpdateInputChangeTime(InputChangeTimeArray, InputSignal); 
                                         
  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray, 
    OutputSignalName, PathDelayArray, 
    ArcType, PathConditionArray); 
END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- #11
-- DelayType - VitalMemoryDelayType01
-- Input     - Vector
-- Output    - Vector
-- Delay     - Vector
-- Condition - Scalar 
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray  : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal        : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName   : IN STRING :="";
  VARIABLE InputChangeTimeArray : INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray       : IN VitalDelayArrayType01;
  CONSTANT ArcType              : IN VitalMemoryArcType  := CrossArc;
  CONSTANT PathCondition        : IN BOOLEAN := TRUE
) IS
  VARIABLE PathConditionArray : 
      VitalBoolArrayT(ScheduleDataArray'LENGTH-1 downto 0); 
BEGIN
  FOR i IN PathConditionArray'RANGE LOOP
    PathConditionArray(i) := PathCondition;
  END LOOP;

  VitalMemoryUpdateInputChangeTime(InputChangeTimeArray, InputSignal); 

  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray, 
    OutputSignalName, PathDelayArray, 
    ArcType, PathConditionArray);
END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- #12
-- DelayType - VitalMemoryDelayType01
-- Input     - Vector
-- Output    - Vector
-- Delay     - Vector
-- Condition - Vector
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray  : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName  : IN STRING :="";
  VARIABLE InputChangeTimeArray : INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray       : IN VitalDelayArrayType01;
  CONSTANT ArcType              : IN VitalMemoryArcType  := CrossArc;
  CONSTANT PathConditionArray   : IN VitalBoolArrayT
) IS
  VARIABLE NumBitsPerSubword : INTEGER;
  VARIABLE PathConditionArrayNorm : 
      VitalBoolArrayT(PathConditionArray'LENGTH-1 downto 0) := PathConditionArray; -- IR MEM400; 
  VARIABLE PathConditionArrayExp : 
      VitalBoolArrayT(ScheduleDataArray'LENGTH-1 downto 0); 
BEGIN
  NumBitsPerSubword := 
      ScheduleDataArray(ScheduleDataArray'RIGHT).NumBitsPerSubword;
  FOR i IN PathConditionArrayExp'RANGE LOOP
    PathConditionArrayExp(i) := PathConditionArrayNorm(i/NumBitsPerSubword);
  END LOOP;

  VitalMemoryUpdateInputChangeTime(InputChangeTimeArray, InputSignal);

  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray,
    OutputSignalName, PathDelayArray, 
    ArcType, PathConditionArrayExp);
END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- #13
-- DelayType - VitalMemoryDelayType01Z
-- Input     - Scalar
-- Output    - Scalar
-- Delay     - Scalar
-- Condition - Scalar 
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleData     : INOUT VitalMemoryScheduleDataType;
  SIGNAL   InputSignal      : IN STD_ULOGIC;
  CONSTANT OutputSignalName : IN STRING :="";
  VARIABLE InputChangeTime  : INOUT TIME;
  CONSTANT PathDelay        : IN VitalDelayType01Z;
  CONSTANT ArcType          : IN VitalMemoryArcType := CrossArc; 
  CONSTANT PathCondition    : IN BOOLEAN := TRUE;
  CONSTANT OutputRetainFlag : IN BOOLEAN := FALSE
) IS
  VARIABLE ScheduleDataArray : 
           VitalMemoryScheduleDataVectorType(0 downto 0);
  VARIABLE PathDelayArray : VitalDelayArrayType01Z(0 downto 0);
  VARIABLE InputChangeTimeArray : VitalTimeArrayT(0 downto 0);
  VARIABLE PathConditionArray : VitalBoolArrayT(0 downto 0); 
BEGIN
  PathConditionArray(0) := PathCondition;
  ScheduleDataArray(0) := ScheduleData;
  PathDelayArray(0) := PathDelay;
  VitalMemoryUpdateInputChangeTime(InputChangeTime, InputSignal);
  InputChangeTimeArray(0) := InputChangeTime;

  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray,
    OutputSignalName, PathDelayArray, 
    ArcType, PathConditionArray, OutputRetainFlag);
END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- #14
-- DelayType - VitalMemoryDelayType01Z
-- Input     - Scalar
-- Output    - Vector
-- Delay     - Vector
-- Condition - Scalar 
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_ULOGIC;
  CONSTANT OutputSignalName  : IN STRING :="";
  VARIABLE InputChangeTime   : INOUT TIME;
  CONSTANT PathDelayArray    : IN VitalDelayArrayType01Z;
  CONSTANT ArcType           : IN VitalMemoryArcType  := CrossArc;
  CONSTANT PathCondition     : IN BOOLEAN := TRUE;
  CONSTANT OutputRetainFlag  : IN BOOLEAN := FALSE
) IS
  VARIABLE InputChangeTimeArray : VitalTimeArrayT(0 downto 0);
  VARIABLE PathConditionArray : 
      VitalBoolArrayT(ScheduleDataArray'LENGTH-1 downto 0); 
BEGIN
  FOR i IN PathConditionArray'RANGE LOOP
    PathConditionArray(i) := PathCondition;
  END LOOP;

  VitalMemoryUpdateInputChangeTime(InputChangeTime, InputSignal);
  InputChangeTimeArray(0) := InputChangeTime; 

  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray, 
    OutputSignalName, PathDelayArray, 
    ArcType, PathConditionArray, OutputRetainFlag);

END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- #15
-- DelayType - VitalMemoryDelayType01Z
-- Input     - Scalar
-- Output    - Vector
-- Delay     - Vector
-- Condition - Vector
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_ULOGIC;
  CONSTANT OutputSignalName  : IN STRING :="";
  VARIABLE InputChangeTime   : INOUT TIME;
  CONSTANT PathDelayArray    : IN VitalDelayArrayType01Z;
  CONSTANT ArcType           : IN VitalMemoryArcType  := CrossArc;
  CONSTANT PathConditionArray: IN VitalBoolArrayT;
  CONSTANT OutputRetainFlag  : IN BOOLEAN := FALSE
) IS
  VARIABLE InputChangeTimeArray : VitalTimeArrayT(0 downto 0);
  VARIABLE NumBitsPerSubword : INTEGER;
  VARIABLE PathConditionArrayNorm : VitalBoolArrayT(PathConditionArray'LENGTH-1 downto 0); 
  VARIABLE PathConditionArrayExp : VitalBoolArrayT(ScheduleDataArray'LENGTH-1 downto 0); 
BEGIN
  NumBitsPerSubword := ScheduleDataArray(ScheduleDataArray'RIGHT).NumBitsPerSubword;
  FOR i IN PathConditionArrayExp'RANGE LOOP
    PathConditionArrayExp(i) := PathConditionArrayNorm(i/NumBitsPerSubword);
  END LOOP;

  VitalMemoryUpdateInputChangeTime(InputChangeTime, InputSignal);
  InputChangeTimeArray(0) := InputChangeTime;

  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray,
    OutputSignalName, PathDelayArray, 
    ArcType, PathConditionArrayExp, OutputRetainFlag);
END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- #16
-- DelayType - VitalMemoryDelayType01Z
-- Input     - Vector
-- Output    - Scalar
-- Delay     - Vector
-- Condition - Scalar 
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleData         : INOUT VitalMemoryScheduleDataType;
  SIGNAL   InputSignal          : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName     : IN STRING :="";
  VARIABLE InputChangeTimeArray : INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray       : IN VitalDelayArrayType01Z;
  CONSTANT ArcType              : IN VitalMemoryArcType  := CrossArc;
  CONSTANT PathCondition        : IN BOOLEAN := TRUE;
  CONSTANT OutputRetainFlag     : IN BOOLEAN := FALSE;
  CONSTANT OutputRetainBehavior : IN OutputRetainBehaviorType := BitCorrupt
) IS
  VARIABLE ScheduleDataArray : 
              VitalMemoryScheduleDataVectorType(0 downto 0);
  VARIABLE NumBitsPerSubword : INTEGER;
  VARIABLE PathConditionArray : VitalBoolArrayT(0 downto 0); 
BEGIN
  PathConditionArray(0) := PathCondition;
  ScheduleDataArray(0) := ScheduleData;
  NumBitsPerSubword := ScheduleDataArray(0).NumBitsPerSubword;
  IF (OutputRetainBehavior = WordCorrupt AND 
      ArcType = ParallelArc AND
      OutputRetainFlag = TRUE) THEN
    VitalMemoryUpdateInputChangeTime(
      InputChangeTimeArray, 
      InputSignal,
      NumBitsPerSubword
    ); 
  ELSE
    VitalMemoryUpdateInputChangeTime(InputChangeTimeArray, InputSignal); 
  END IF;

  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray, 
    OutputSignalName, PathDelayArray, 
    ArcType, PathConditionArray, OutputRetainFlag);
END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- #17
-- DelayType - VitalMemoryDelayType01Z
-- Input     - Vector
-- Output    - Vector
-- Delay     - Vector  
-- Condition - Scalar 
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName  : IN STRING :="";
  VARIABLE InputChangeTimeArray : INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray       : IN VitalDelayArrayType01Z;
  CONSTANT ArcType              : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathCondition        : IN BOOLEAN := TRUE;
  CONSTANT OutputRetainFlag     : IN BOOLEAN := FALSE;
  CONSTANT OutputRetainBehavior : IN OutputRetainBehaviorType := BitCorrupt
) IS
  VARIABLE NumBitsPerSubword  : INTEGER;
  VARIABLE PathConditionArray : 
      VitalBoolArrayT(ScheduleDataArray'LENGTH-1 downto 0); 
BEGIN
  FOR i IN PathConditionArray'RANGE LOOP
    PathConditionArray(i) := PathCondition;
  END LOOP;

  NumBitsPerSubword := 
      ScheduleDataArray(ScheduleDataArray'LEFT).NumBitsPerSubword;
  IF (OutputRetainBehavior = WordCorrupt AND 
      ArcType = ParallelArc AND
      OutputRetainFlag = TRUE) THEN
    VitalMemoryUpdateInputChangeTime(
      InputChangeTimeArray, 
      InputSignal,
      NumBitsPerSubword
    ); 
  ELSE
    VitalMemoryUpdateInputChangeTime(InputChangeTimeArray, InputSignal); 
  END IF;

  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray, 
    OutputSignalName, PathDelayArray, 
    ArcType, PathConditionArray, OutputRetainFlag);
END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- #18
-- DelayType - VitalMemoryDelayType01Z
-- Input     - Vector
-- Output    - Vector
-- Delay     - Vector 
-- Condition - Vector
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName  : IN STRING :="";
  VARIABLE InputChangeTimeArray : INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray       : IN VitalDelayArrayType01Z;
  CONSTANT ArcType              : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathConditionArray   : IN VitalBoolArrayT;
  CONSTANT OutputRetainFlag     : IN BOOLEAN := FALSE;
  CONSTANT OutputRetainBehavior : IN OutputRetainBehaviorType := BitCorrupt
) IS
VARIABLE NumBitsPerSubword : INTEGER;
VARIABLE PathConditionArrayNorm : 
    VitalBoolArrayT(PathConditionArray'LENGTH-1 downto 0); 
VARIABLE PathConditionArrayExp : 
    VitalBoolArrayT(ScheduleDataArray'LENGTH-1 downto 0); 
BEGIN
  NumBitsPerSubword := ScheduleDataArray(ScheduleDataArray'RIGHT).NumBitsPerSubword;
  FOR i IN PathConditionArrayExp'RANGE LOOP
    PathConditionArrayExp(i) := PathConditionArrayNorm(i/NumBitsPerSubword);
  END LOOP;

  IF (OutputRetainBehavior = WordCorrupt AND 
      ArcType = ParallelArc AND
      OutputRetainFlag = TRUE) THEN
    VitalMemoryUpdateInputChangeTime(
        InputChangeTimeArray, InputSignal,
        NumBitsPerSubword);
  ELSE
    VitalMemoryUpdateInputChangeTime(InputChangeTimeArray, InputSignal);
  END IF;

  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray,
    OutputSignalName, PathDelayArray, 
    ArcType, PathConditionArrayExp, OutputRetainFlag);
END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- #19
-- DelayType - VitalMemoryDelayType01XZ
-- Input     - Scalar
-- Output    - Scalar
-- Delay     - Scalar
-- Condition - Scalar 
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleData      : INOUT  VitalMemoryScheduleDataType;
  SIGNAL   InputSignal       : IN STD_ULOGIC;
  CONSTANT OutputSignalName  : IN STRING :="";
  VARIABLE InputChangeTime   : INOUT TIME;
  CONSTANT PathDelay         : IN VitalDelayType01ZX;
  CONSTANT ArcType           : IN VitalMemoryArcType := CrossArc; 
  CONSTANT PathCondition     : IN BOOLEAN := TRUE;
  CONSTANT OutputRetainFlag  : IN BOOLEAN := FALSE
) IS
  VARIABLE ScheduleDataArray : 
         VitalMemoryScheduleDataVectorType(0 downto 0);
  VARIABLE PathDelayArray : VitalDelayArrayType01ZX(0 downto 0);
  VARIABLE InputChangeTimeArray : VitalTimeArrayT(0 downto 0);
  VARIABLE PathConditionArray : VitalBoolArrayT(0 downto 0); 
BEGIN
  PathConditionArray(0) := PathCondition;
  ScheduleDataArray(0) := ScheduleData;
  PathDelayArray(0) := PathDelay;
  VitalMemoryUpdateInputChangeTime(InputChangeTime, InputSignal);
  InputChangeTimeArray(0) := InputChangeTime;

  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray, 
    OutputSignalName, PathDelayArray, 
    ArcType, PathConditionArray, OutputRetainFlag); 
END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- #20
-- DelayType - VitalMemoryDelayType01XZ
-- Input     - Scalar
-- Output    - Vector
-- Delay     - Vector
-- Condition - Scalar 
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray :INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_ULOGIC;
  CONSTANT OutputSignalName  : IN STRING :="";
  VARIABLE InputChangeTime   : INOUT TIME;
  CONSTANT PathDelayArray    : IN VitalDelayArrayType01ZX;
  CONSTANT ArcType           : IN VitalMemoryArcType := CrossArc; 
  CONSTANT PathCondition     : IN BOOLEAN := TRUE;
  CONSTANT OutputRetainFlag  : IN BOOLEAN := FALSE
) IS
  VARIABLE InputChangeTimeArray : VitalTimeArrayT(0 downto 0);
  VARIABLE PathConditionArray : 
      VitalBoolArrayT(ScheduleDataArray'LENGTH-1 downto 0); 
BEGIN
  FOR i IN PathConditionArray'RANGE LOOP
    PathConditionArray(i) := PathCondition;
  END LOOP;

  VitalMemoryUpdateInputChangeTime(InputChangeTime, InputSignal);
  InputChangeTimeArray(0) := InputChangeTime; 

  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray,  
    OutputSignalName, PathDelayArray, 
    ArcType, PathConditionArray, OutputRetainFlag); 
END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- #21
-- DelayType - VitalMemoryDelayType01XZ
-- Input     - Scalar
-- Output    - Vector
-- Delay     - Vector
-- Condition - Vector
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray :INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_ULOGIC;
  CONSTANT OutputSignalName  : IN STRING :="";
  VARIABLE InputChangeTime   : INOUT TIME;
  CONSTANT PathDelayArray    : IN VitalDelayArrayType01ZX;
  CONSTANT ArcType           : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathConditionArray: IN VitalBoolArrayT;
  CONSTANT OutputRetainFlag  : IN BOOLEAN := FALSE
) IS
  VARIABLE InputChangeTimeArray : VitalTimeArrayT(0 downto 0);
  VARIABLE NumBitsPerSubword : INTEGER;
  VARIABLE PathConditionArrayNorm : 
      VitalBoolArrayT(PathConditionArray'LENGTH-1 downto 0) := PathConditionArray; -- IR MEM400; 
  VARIABLE PathConditionArrayExp : 
      VitalBoolArrayT(ScheduleDataArray'LENGTH-1 downto 0); 
BEGIN
  NumBitsPerSubword := 
      ScheduleDataArray(ScheduleDataArray'RIGHT).NumBitsPerSubword;
  FOR i IN PathConditionArrayExp'RANGE LOOP
    PathConditionArrayExp(i) := PathConditionArrayNorm(i/NumBitsPerSubword);
  END LOOP;

  VitalMemoryUpdateInputChangeTime(InputChangeTime, InputSignal);
  InputChangeTimeArray(0) := InputChangeTime;

  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray,
    OutputSignalName, PathDelayArray, 
    ArcType, PathConditionArrayExp, OutputRetainFlag);
END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- #22
-- DelayType - VitalMemoryDelayType01XZ
-- Input     - Vector
-- Output    - Scalar
-- Delay     - Vector
-- Condition - Scalar 
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleData         : INOUT  VitalMemoryScheduleDataType;
  SIGNAL   InputSignal          : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName     : IN STRING :="";
  VARIABLE InputChangeTimeArray : INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray       : IN VitalDelayArrayType01ZX;
  CONSTANT ArcType              : IN VitalMemoryArcType := CrossArc; 
  CONSTANT PathCondition        : IN BOOLEAN := TRUE;
  CONSTANT OutputRetainFlag     : IN BOOLEAN := FALSE;
  CONSTANT OutputRetainBehavior : IN OutputRetainBehaviorType := BitCorrupt
) IS 
  VARIABLE ScheduleDataArray : 
    VitalMemoryScheduleDataVectorType(0 downto 0);
  VARIABLE NumBitsPerSubword : INTEGER;
  VARIABLE PathConditionArray : VitalBoolArrayT(0 downto 0); 
BEGIN
  PathConditionArray(0) := PathCondition;
  ScheduleDataArray(0) := ScheduleData;
  NumBitsPerSubword := 
      ScheduleDataArray(ScheduleDataArray'LEFT).NumBitsPerSubword;
  IF (OutputRetainBehavior = WordCorrupt AND 
      ArcType = ParallelArc AND
      OutputRetainFlag = TRUE) THEN
    VitalMemoryUpdateInputChangeTime(
      InputChangeTimeArray, InputSignal,
      NumBitsPerSubword); 
  ELSE
    VitalMemoryUpdateInputChangeTime(InputChangeTimeArray, InputSignal); 
  END IF;

  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray, 
    OutputSignalName, PathDelayArray, 
    ArcType, PathConditionArray, OutputRetainFlag);
END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- #23
-- DelayType - VitalMemoryDelayType01XZ
-- Input     - Vector
-- Output    - Vector
-- Delay     - Vector
-- Condition - Scalar 
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName  : IN STRING :="";
  VARIABLE InputChangeTimeArray : INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray       : IN VitalDelayArrayType01ZX;
  CONSTANT ArcType              : IN VitalMemoryArcType  := CrossArc;
  CONSTANT PathCondition        : IN BOOLEAN := TRUE;
  CONSTANT OutputRetainFlag     : IN BOOLEAN := FALSE;
  CONSTANT OutputRetainBehavior : IN OutputRetainBehaviorType := BitCorrupt
) IS
  VARIABLE NumBitsPerSubword  : INTEGER;
  VARIABLE PathConditionArray : 
      VitalBoolArrayT(ScheduleDataArray'LENGTH-1 downto 0); 
BEGIN
  FOR i IN PathConditionArray'RANGE LOOP
    PathConditionArray(i) := PathCondition;
  END LOOP;

  NumBitsPerSubword := 
      ScheduleDataArray(ScheduleDataArray'LEFT).NumBitsPerSubword;
  IF (OutputRetainBehavior = WordCorrupt AND 
      ArcType = ParallelArc AND
      OutputRetainFlag = TRUE) THEN
    VitalMemoryUpdateInputChangeTime(
      InputChangeTimeArray, InputSignal,
      NumBitsPerSubword); 
  ELSE
    VitalMemoryUpdateInputChangeTime(InputChangeTimeArray, InputSignal); 
  END IF;

  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray,
    OutputSignalName, PathDelayArray, 
    ArcType, PathConditionArray, OutputRetainFlag); 
END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- #24
-- DelayType - VitalMemoryDelayType01XZ
-- Input     - Vector
-- Output    - Vector
-- Delay     - Vector
-- Condition - Vector
PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName  : IN STRING :="";
  VARIABLE InputChangeTimeArray : INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray       : IN VitalDelayArrayType01ZX;
  CONSTANT ArcType              : IN VitalMemoryArcType  := CrossArc;
  CONSTANT PathConditionArray   : IN VitalBoolArrayT;
  CONSTANT OutputRetainFlag     : IN BOOLEAN := FALSE;
  CONSTANT OutputRetainBehavior : IN OutputRetainBehaviorType := BitCorrupt
) IS
  VARIABLE NumBitsPerSubword : INTEGER;
  VARIABLE PathConditionArrayNorm : 
      VitalBoolArrayT(PathConditionArray'LENGTH-1 downto 0) := PathConditionArray; -- IR MEM400; 
  VARIABLE PathConditionArrayExp : 
      VitalBoolArrayT(ScheduleDataArray'LENGTH-1 downto 0); 
BEGIN
  NumBitsPerSubword := 
      ScheduleDataArray(ScheduleDataArray'RIGHT).NumBitsPerSubword;
  FOR i IN PathConditionArrayExp'RANGE LOOP
    PathConditionArrayExp(i) := PathConditionArrayNorm(i/NumBitsPerSubword);
  END LOOP;

  IF (OutputRetainBehavior = WordCorrupt AND 
      ArcType = ParallelArc AND
      OutputRetainFlag = TRUE) THEN
    VitalMemoryUpdateInputChangeTime(
      InputChangeTimeArray, InputSignal,
      NumBitsPerSubword);
  ELSE
    VitalMemoryUpdateInputChangeTime(InputChangeTimeArray, InputSignal);
  END IF;

  VitalMemorySelectDelay(
    ScheduleDataArray, InputChangeTimeArray,
    OutputSignalName, PathDelayArray, 
    ArcType, PathConditionArrayExp, OutputRetainFlag);
END VitalMemoryAddPathDelay;

-- ----------------------------------------------------------------------------
-- Procedure:   VitalMemorySchedulePathDelay
-- Description: Schedule Output after Propagation Delay selected
--              by checking all the paths added thru' 
--              VitalMemoryAddPathDelay.
-- ----------------------------------------------------------------------------
PROCEDURE VitalMemorySchedulePathDelay (
  SIGNAL   OutSignal        : OUT STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName : IN STRING :="";
  CONSTANT PortFlag         : IN VitalPortFlagType := VitalDefaultPortFlag;
  CONSTANT OutputMap        : IN VitalOutputMapType:= VitalDefaultOutputMap;
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType
) IS
  VARIABLE Age               : TIME;
  VARIABLE PropDelay         : TIME;
  VARIABLE RetainDelay       : TIME;
  VARIABLE Data              : STD_ULOGIC;
BEGIN
  IF (PortFlag.OutputDisable /= TRUE) THEN
    FOR i IN ScheduleDataArray'RANGE LOOP
      PropDelay := ScheduleDataArray(i).PropDelay;
      RetainDelay := ScheduleDataArray(i).OutputRetainDelay;   

      NEXT WHEN PropDelay = TIME'HIGH;

      Age := ScheduleDataArray(i).InputAge; 
      Data := ScheduleDataArray(i).OutputData;

      IF (Age < RetainDelay and RetainDelay < PropDelay) THEN
        OutSignal(i) <= TRANSPORT 'X' AFTER (RetainDelay - Age);
      END IF;

      IF (Age <= PropDelay) THEN
        OutSignal(i)<= TRANSPORT OutputMap(Data)AFTER (PropDelay-Age);
        ScheduleDataArray(i).ScheduleValue := Data;
        ScheduleDataArray(i).ScheduleTime := NOW + PropDelay - Age;
      END IF;
    END LOOP;
  END IF;

  -- for debug purpose
  PrintScheduleDataArray(ScheduleDataArray);

  -- for debug purpose
  ScheduleDebugMsg;
END VitalMemorySchedulePathDelay;

-- ----------------------------------------------------------------------------
-- Procedure:   VitalMemorySchedulePathDelay
-- Description: Schedule Output after Propagation Delay selected
--              by checking all the paths added thru' 
--              VitalMemoryAddPathDelay.
-- ----------------------------------------------------------------------------
PROCEDURE VitalMemorySchedulePathDelay (
  SIGNAL   OutSignal        : OUT STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName : IN STRING :="";
  CONSTANT PortFlag         : IN VitalPortFlagVectorType;
  CONSTANT OutputMap        : IN VitalOutputMapType:= VitalDefaultOutputMap;
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType
) IS
  VARIABLE Age               : TIME;
  VARIABLE PropDelay         : TIME;
  VARIABLE RetainDelay       : TIME;
  VARIABLE Data              : STD_ULOGIC;
  VARIABLE ExpandedPortFlag  : 
      VitalPortFlagVectorType(ScheduleDataArray'RANGE);
  VARIABLE NumBitsPerSubword : INTEGER;
BEGIN
  NumBitsPerSubword := 
      ScheduleDataArray(ScheduleDataArray'LEFT).NumBitsPerSubword;
  VitalMemoryExpandPortFlag( PortFlag, NumBitsPerSubword, ExpandedPortFlag );
  FOR i IN ScheduleDataArray'RANGE LOOP
    NEXT WHEN ExpandedPortFlag(i).OutputDisable = TRUE;

    PropDelay := ScheduleDataArray(i).PropDelay;
    RetainDelay := ScheduleDataArray(i).OutputRetainDelay;   
    
    NEXT WHEN PropDelay = TIME'HIGH;

    Age  := ScheduleDataArray(i).InputAge; 
    Data := ScheduleDataArray(i).OutputData;

    IF (Age < RetainDelay and RetainDelay < PropDelay) THEN
      OutSignal(i) <= TRANSPORT 'X' AFTER (RetainDelay - Age);
    END IF;

    IF (Age <= PropDelay) THEN
      OutSignal(i)<= TRANSPORT OutputMap(Data)AFTER (PropDelay-Age);
      ScheduleDataArray(i).ScheduleValue := Data;
      ScheduleDataArray(i).ScheduleTime := NOW + PropDelay - Age;
    END IF;
  END LOOP;

  -- for debug purpose
  PrintScheduleDataArray(ScheduleDataArray);

  -- for debug purpose
  ScheduleDebugMsg;
END VitalMemorySchedulePathDelay;

-- ----------------------------------------------------------------------------
PROCEDURE VitalMemorySchedulePathDelay (
  SIGNAL   OutSignal       : OUT STD_ULOGIC;
  CONSTANT OutputSignalName: IN STRING :="";
  CONSTANT PortFlag        : IN VitalPortFlagType := VitalDefaultPortFlag;
  CONSTANT OutputMap       : IN VitalOutputMapType := VitalDefaultOutputMap;
  VARIABLE ScheduleData    : INOUT VitalMemoryScheduleDataType
) IS
  VARIABLE Age               : TIME;
  VARIABLE PropDelay         : TIME;
  VARIABLE RetainDelay       : TIME;
  VARIABLE Data              : STD_ULOGIC;
  VARIABLE ScheduleDataArray : VitalMemoryScheduleDataVectorType (0 downto 0);
BEGIN
  IF (PortFlag.OutputDisable /= TRUE) THEN
    ScheduledataArray(0) := ScheduleData;
    PropDelay := ScheduleDataArray(0).PropDelay;
    RetainDelay := ScheduleDataArray(0).OutputRetainDelay;
    Age := ScheduleDataArray(0).InputAge; 
    Data := ScheduleDataArray(0).OutputData;

    IF (Age < RetainDelay and RetainDelay < PropDelay) THEN
      OutSignal <= TRANSPORT 'X' AFTER (RetainDelay - Age);
    END IF;

    IF (Age <= PropDelay and PropDelay /= TIME'HIGH) THEN
      OutSignal <= TRANSPORT OutputMap(Data)  AFTER (PropDelay - Age);
      ScheduleDataArray(0).ScheduleValue := Data;
      ScheduleDataArray(0).ScheduleTime := NOW + PropDelay - Age;
    END IF;
  END IF;

  -- for debug purpose
  PrintScheduleDataArray(ScheduleDataArray);

  -- for debug purpose
  ScheduleDebugMsg;

END VitalMemorySchedulePathDelay;

-- ----------------------------------------------------------------------------
-- Procedure  : InternalTimingCheck
-- ----------------------------------------------------------------------------
PROCEDURE InternalTimingCheck (
  CONSTANT TestSignal    : IN     std_ulogic;
  CONSTANT RefSignal     : IN     std_ulogic;
  CONSTANT TestDelay     : IN     TIME := 0 ns;
  CONSTANT RefDelay      : IN     TIME := 0 ns;
  CONSTANT SetupHigh     : IN     TIME := 0 ns;
  CONSTANT SetupLow      : IN     TIME := 0 ns;
  CONSTANT HoldHigh      : IN     TIME := 0 ns;
  CONSTANT HoldLow       : IN     TIME := 0 ns;
  VARIABLE RefTime       : IN     TIME;
  VARIABLE RefEdge       : IN     BOOLEAN;
  VARIABLE TestTime      : IN     TIME;
  VARIABLE TestEvent     : IN     BOOLEAN;
  VARIABLE SetupEn       : INOUT  BOOLEAN;
  VARIABLE HoldEn        : INOUT  BOOLEAN;
  VARIABLE CheckInfo     : INOUT  CheckInfoType;
  CONSTANT MsgOn         : IN     BOOLEAN
) IS
  VARIABLE bias           : TIME;
  VARIABLE actualObsTime  : TIME;
  VARIABLE BC             : TIME;
  VARIABLE Message        :LINE;
BEGIN
  -- Check SETUP constraint
  IF (RefEdge) THEN
    IF (SetupEn) THEN
      CheckInfo.ObsTime   := RefTime - TestTime;
      CheckInfo.State     := To_X01(TestSignal);
      CASE CheckInfo.State IS
        WHEN '0' =>
          CheckInfo.ExpTime := SetupLow;
          -- start of new code IR245-246
          BC := HoldHigh;
          -- end of new code IR245-246
        WHEN '1' =>
          CheckInfo.ExpTime := SetupHigh;
          -- start of new code IR245-246
          BC := HoldLow;
          -- end of new code IR245-246
        WHEN 'X' => 
          CheckInfo.ExpTime := Maximum(SetupHigh,SetupLow);
          -- start of new code IR245-246
          BC := Maximum(HoldHigh,HoldLow);
          -- end of new code IR245-246
      END CASE;
      -- added the second condition for IR 245-246
      CheckInfo.Violation := 
          ((CheckInfo.ObsTime < CheckInfo.ExpTime) 
            AND ( NOT ((CheckInfo.ObsTime = BC) and (BC = 0 ns))));
      -- start of new code IR245-246
      IF (CheckInfo.ExpTime = 0 ns) THEN
        CheckInfo.CheckKind := HoldCheck;
      ELSE
        CheckInfo.CheckKind := SetupCheck;
      END IF;
      -- end of new code IR245-246
      SetupEn  := FALSE;
    ELSE
      CheckInfo.Violation := FALSE;
    END IF;

  -- Check HOLD constraint
  ELSIF (TestEvent) THEN
    IF HoldEn THEN
      CheckInfo.ObsTime := TestTime - RefTime;
      CheckInfo.State := To_X01(TestSignal);
      CASE CheckInfo.State IS
        WHEN '0' =>
          CheckInfo.ExpTime := HoldHigh;
          -- new code for unnamed IR 
          CheckInfo.State := '1';
          -- start of new code IR245-246
          BC := SetupLow;
          -- end of new code IR245-246
        WHEN '1' =>
          CheckInfo.ExpTime := HoldLow;
          -- new code for unnamed IR 
          CheckInfo.State := '0';
          -- start of new code IR245-246
          BC := SetupHigh;
          -- end of new code IR245-246
        WHEN 'X' =>
          CheckInfo.ExpTime := Maximum(HoldHigh,HoldLow);
          -- start of new code IR245-246
          BC := Maximum(SetupHigh,SetupLow);
          -- end of new code IR245-246
      END CASE;
      -- added the second condition for IR 245-246
      CheckInfo.Violation := 
        ((CheckInfo.ObsTime < CheckInfo.ExpTime) 
          AND ( NOT ((CheckInfo.ObsTime = BC) and (BC = 0 ns))));
      -- start of new code IR245-246
      IF (CheckInfo.ExpTime = 0 ns) THEN
        CheckInfo.CheckKind := SetupCheck;
      ELSE
        CheckInfo.CheckKind := HoldCheck;
      END IF;
      -- end of new code IR245-246
      HoldEn := NOT CheckInfo.Violation;
    ELSE
      CheckInfo.Violation := FALSE;
    END IF;
  ELSE
    CheckInfo.Violation := FALSE;
  END IF;

  -- Adjust report values to account for internal model delays
  -- Note: TestDelay, RefDelay, TestTime, RefTime are non-negative
  -- Note: bias may be negative or positive
  IF MsgOn AND CheckInfo.Violation THEN
    -- modified the code for correct reporting of violation in case of 
    -- order of signals being reversed because of internal delays
    -- new variable 
    actualObsTime := (TestTime-TestDelay)-(RefTime-RefDelay);
    bias := TestDelay - RefDelay;
    IF (actualObsTime < 0 ns) THEN -- It should be a setup check
      IF ( CheckInfo.CheckKind = HoldCheck) THEN
        CheckInfo.CheckKind := SetupCheck;
        CASE CheckInfo.State IS
          WHEN '0' => CheckInfo.ExpTime := SetupLow; 
          WHEN '1' => CheckInfo.ExpTime := SetupHigh;
          WHEN 'X' => CheckInfo.ExpTime := Maximum(SetupHigh,SetupLow);
        END CASE;	       	  
      END IF;
      CheckInfo.ObsTime := -actualObsTime;
      CheckInfo.ExpTime := CheckInfo.ExpTime + bias;
      CheckInfo.DetTime := RefTime - RefDelay;
    ELSE -- It should be a hold check
      IF (CheckInfo.CheckKind = SetupCheck) THEN
        CheckInfo.CheckKind :=  HoldCheck;
        CASE CheckInfo.State IS
          WHEN '0' =>
            CheckInfo.ExpTime := HoldHigh;
            CheckInfo.State   := '1';
          WHEN '1' =>
            CheckInfo.ExpTime := HoldLow;
            CheckInfo.State   := '0';
          WHEN 'X' =>
            CheckInfo.ExpTime := Maximum(HoldHigh,HoldLow);
        END CASE;
      END IF;
      CheckInfo.ObsTime := actualObsTime;
      CheckInfo.ExpTime := CheckInfo.ExpTime - bias;
      CheckInfo.DetTime := TestTime - TestDelay;
    END IF;
  END IF;
END InternalTimingCheck;


-- ----------------------------------------------------------------------------
-- Setup and Hold Time Check Routine 
-- ----------------------------------------------------------------------------
PROCEDURE TimingArrayIndex (
  SIGNAL InputSignal      : IN Std_logic_vector;
  CONSTANT ArrayIndexNorm : IN INTEGER;
  VARIABLE Index          : OUT INTEGER
) IS
BEGIN
  IF (InputSignal'LEFT > InputSignal'RIGHT) THEN
    Index  := ArrayIndexNorm + InputSignal'RIGHT;
  ELSE
    Index  := InputSignal'RIGHT - ArrayIndexNorm;
  END IF;
END TimingArrayIndex;

-- ----------------------------------------------------------------------------
PROCEDURE VitalMemoryReportViolation (
  CONSTANT TestSignalName : IN STRING := "";
  CONSTANT RefSignalName  : IN STRING := "";
  CONSTANT HeaderMsg      : IN STRING := " ";
  CONSTANT CheckInfo      : IN CheckInfoType;
  CONSTANT MsgSeverity    : IN SEVERITY_LEVEL := WARNING
) IS
  VARIABLE Message : LINE;
BEGIN
  IF (NOT CheckInfo.Violation) THEN
    RETURN;
  END IF;
  Write ( Message, HeaderMsg );
  CASE CheckInfo.CheckKind IS
    WHEN    SetupCheck => Write ( Message, STRING'(" SETUP ")      );
    WHEN     HoldCheck => Write ( Message, STRING'(" HOLD ")       );
    WHEN RecoveryCheck => Write ( Message, STRING'(" RECOVERY ")   );
    WHEN  RemovalCheck => Write ( Message, STRING'(" REMOVAL ")    );
    WHEN PulseWidCheck => Write ( Message, STRING'(" PULSE WIDTH "));
    WHEN   PeriodCheck => Write ( Message, STRING'(" PERIOD ")     );
  END CASE;
  Write ( Message, HiLoStr(CheckInfo.State) );
  Write ( Message, STRING'(" VIOLATION ON ") );
  Write ( Message, TestSignalName );
  IF (RefSignalName'LENGTH > 0) THEN
    Write ( Message, STRING'(" WITH RESPECT TO ") );
    Write ( Message, RefSignalName );
  END IF;
  Write ( Message, ';' & LF );
  Write ( Message, STRING'("  Expected := ")  );
  Write ( Message, CheckInfo.ExpTime);
  Write ( Message, STRING'("; Observed := ")  );
  Write ( Message, CheckInfo.ObsTime);
  Write ( Message, STRING'("; At : ")         );
  Write ( Message, CheckInfo.DetTime);
  ASSERT FALSE REPORT Message.ALL SEVERITY MsgSeverity;
  DEALLOCATE (Message);
END VitalMemoryReportViolation;

-- ----------------------------------------------------------------------------
PROCEDURE VitalMemoryReportViolation (
  CONSTANT TestSignalName : IN STRING := "";
  CONSTANT RefSignalName  : IN STRING := "";
  CONSTANT TestArrayIndex : IN INTEGER;
  CONSTANT RefArrayIndex  : IN INTEGER;
  SIGNAL TestSignal       : IN std_logic_vector;
  SIGNAL RefSignal        : IN std_logic_vector;
  CONSTANT HeaderMsg      : IN STRING := " ";
  CONSTANT CheckInfo      : IN CheckInfoType;
  CONSTANT MsgFormat      : IN VitalMemoryMsgFormatType;
  CONSTANT MsgSeverity    : IN SEVERITY_LEVEL := WARNING
) IS
  VARIABLE Message : LINE;
  VARIABLE i, j : INTEGER;
BEGIN
  IF (NOT CheckInfo.Violation) THEN 
    RETURN; 
  END IF;

  Write ( Message, HeaderMsg );
  CASE CheckInfo.CheckKind IS
    WHEN    SetupCheck => Write ( Message, STRING'(" SETUP ")      );
    WHEN     HoldCheck => Write ( Message, STRING'(" HOLD ")       );
    WHEN PulseWidCheck => Write ( Message, STRING'(" PULSE WIDTH "));
    WHEN   PeriodCheck => Write ( Message, STRING'(" PERIOD ")     );
    WHEN OTHERS        => Write ( Message, STRING'(" UNKNOWN ")     );
  END CASE;
  Write ( Message, HiLoStr(CheckInfo.State) );
  Write ( Message, STRING'(" VIOLATION ON ") );
  Write ( Message, TestSignalName );
  TimingArrayIndex(TestSignal, TestArrayIndex, i);
  CASE MsgFormat IS
    WHEN Scalar => 
      NULL;
    WHEN VectorEnum =>
      Write ( Message, '_');
      Write ( Message, i);  
    WHEN Vector =>
      Write ( Message, '(');
      Write ( Message, i); 
      Write ( Message, ')'); 
  END CASE;

  IF (RefSignalName'LENGTH > 0) THEN
    Write ( Message, STRING'(" WITH RESPECT TO ") );
    Write ( Message, RefSignalName );
  END IF;

  IF(RefSignal'LENGTH > 0) THEN
    TimingArrayIndex(RefSignal, RefArrayIndex, j);
    CASE MsgFormat IS
      WHEN Scalar => 
        NULL;
      WHEN VectorEnum =>
        Write ( Message, '_');
        Write ( Message, j);  
      WHEN Vector =>
        Write ( Message, '(');
        Write ( Message, j); 
        Write ( Message, ')');
    END CASE;
  END IF;

  Write ( Message, ';' & LF );
  Write ( Message, STRING'("  Expected := ")  );
  Write ( Message, CheckInfo.ExpTime);
  Write ( Message, STRING'("; Observed := ")  );
  Write ( Message, CheckInfo.ObsTime);
  Write ( Message, STRING'("; At : ")         );
  Write ( Message, CheckInfo.DetTime);

  ASSERT FALSE REPORT Message.ALL SEVERITY MsgSeverity;

  DEALLOCATE (Message);
END VitalMemoryReportViolation;

-- ----------------------------------------------------------------------------
PROCEDURE VitalMemoryReportViolation (
  CONSTANT TestSignalName : IN STRING := "";
  CONSTANT RefSignalName  : IN STRING := "";
  CONSTANT TestArrayIndex : IN INTEGER;
  CONSTANT HeaderMsg      : IN STRING := " ";
  CONSTANT CheckInfo      : IN CheckInfoType;
  CONSTANT MsgFormat      : IN VitalMemoryMsgFormatType;
  CONSTANT MsgSeverity    : IN SEVERITY_LEVEL := WARNING
) IS
  VARIABLE Message : LINE;
BEGIN
  IF (NOT CheckInfo.Violation) THEN 
    RETURN; 
  END IF;

  Write ( Message, HeaderMsg );
  CASE CheckInfo.CheckKind IS
    WHEN    SetupCheck => Write ( Message, STRING'(" SETUP ")      );
    WHEN     HoldCheck => Write ( Message, STRING'(" HOLD ")       );
    WHEN PulseWidCheck => Write ( Message, STRING'(" PULSE WIDTH "));
    WHEN   PeriodCheck => Write ( Message, STRING'(" PERIOD ")     );
    WHEN        OTHERS => Write ( Message, STRING'(" UNKNOWN ")     );
  END CASE;

  Write ( Message, HiLoStr(CheckInfo.State) );
  Write ( Message, STRING'(" VIOLATION ON ") );
  Write ( Message, TestSignalName );

  CASE MsgFormat IS
    WHEN Scalar => 
      NULL;
    WHEN VectorEnum =>
      Write ( Message, '_');
      Write ( Message, TestArrayIndex);  
    WHEN Vector =>
      Write ( Message, '(');
      Write ( Message, TestArrayIndex); 
      Write ( Message, ')');
  END CASE;

  IF (RefSignalName'LENGTH > 0) THEN
    Write ( Message, STRING'(" WITH RESPECT TO ") );
    Write ( Message, RefSignalName );
  END IF;

  Write ( Message, ';' & LF );
  Write ( Message, STRING'("  Expected := ")  );
  Write ( Message, CheckInfo.ExpTime);
  Write ( Message, STRING'("; Observed := ")  );
  Write ( Message, CheckInfo.ObsTime);
  Write ( Message, STRING'("; At : ")         );
  Write ( Message, CheckInfo.DetTime);

  ASSERT FALSE REPORT Message.ALL SEVERITY MsgSeverity;

  DEALLOCATE (Message);
END VitalMemoryReportViolation;

-- ----------------------------------------------------------------------------
FUNCTION VitalMemoryTimingDataInit 
RETURN VitalMemoryTimingDataType IS
BEGIN
  RETURN (FALSE, 'X', 0 ns, FALSE, 'X', 0 ns, FALSE, 
          NULL, NULL, NULL, NULL, NULL, NULL);
END;

-- ----------------------------------------------------------------------------
-- Procedure:   VitalSetupHoldCheck
-- ----------------------------------------------------------------------------
PROCEDURE VitalMemorySetupHoldCheck (
  VARIABLE Violation     : OUT    X01ArrayT;
  VARIABLE TimingData    : INOUT  VitalMemoryTimingDataType;
  SIGNAL   TestSignal    : IN     std_ulogic;
  CONSTANT TestSignalName: IN     STRING := "";
  CONSTANT TestDelay     : IN     TIME := 0 ns;
  SIGNAL   RefSignal     : IN     std_ulogic;
  CONSTANT RefSignalName : IN     STRING := "";
  CONSTANT RefDelay      : IN     TIME := 0 ns;
  CONSTANT SetupHigh     : IN     VitalDelayType;
  CONSTANT SetupLow      : IN     VitalDelayType;
  CONSTANT HoldHigh      : IN     VitalDelayType;
  CONSTANT HoldLow       : IN     VitalDelayType;
  CONSTANT CheckEnabled  : IN     VitalBoolArrayT;
  CONSTANT RefTransition : IN     VitalEdgeSymbolType;
  CONSTANT HeaderMsg     : IN     STRING := " ";
  CONSTANT XOn           : IN     BOOLEAN := TRUE;
  CONSTANT MsgOn         : IN     BOOLEAN := TRUE;
  CONSTANT MsgSeverity   : IN     SEVERITY_LEVEL := WARNING;
  --IR252 3/23/98
  CONSTANT EnableSetupOnTest : IN   BOOLEAN := TRUE; 
  CONSTANT EnableSetupOnRef  : IN   BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnRef   : IN   BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnTest  : IN   BOOLEAN := TRUE  
) IS
  VARIABLE CheckInfo       : CheckInfoType;
  VARIABLE CheckEnScalar   : BOOLEAN := FALSE;
  VARIABLE ViolationInt    : X01ArrayT(CheckEnabled'RANGE);
  VARIABLE RefEdge         : BOOLEAN;
  VARIABLE TestEvent       : BOOLEAN;
  VARIABLE TestDly         : TIME := Maximum(0 ns, TestDelay);
  VARIABLE RefDly          : TIME := Maximum(0 ns, RefDelay);
  VARIABLE bias            : TIME;
BEGIN

  -- Initialization of working area. 
  IF (TimingData.NotFirstFlag = FALSE) THEN
    TimingData.TestLast     := To_X01(TestSignal);
    TimingData.RefLast      := To_X01(RefSignal);
    TimingData.NotFirstFlag := TRUE;
  END IF;

  -- Detect reference edges and record the time of the last edge
  RefEdge := EdgeSymbolMatch(TimingData.RefLast, To_X01(RefSignal),
               RefTransition);
  TimingData.RefLast := To_X01(RefSignal);
  IF (RefEdge) THEN
    TimingData.RefTime     := NOW;
    --TimingData.HoldEnA.all := (TestSignal'RANGE=>TRUE);
    --IR252 3/23/98
    TimingData.SetupEn := TimingData.SetupEn AND EnableSetupOnRef; 
    TimingData.HoldEn  := EnableHoldOnRef; 
  END IF;

  -- Detect test (data) changes and record the time of the last change
  TestEvent := TimingData.TestLast /= To_X01Z(TestSignal);
  TimingData.TestLast := To_X01Z(TestSignal);
  IF TestEvent THEN
    TimingData.SetupEn  := EnableSetupOnTest ; --IR252 3/23/98
    TimingData.HoldEn := TimingData.HoldEn AND EnableHoldOnTest ;
                                                --IR252 3/23/98
    TimingData.TestTime := NOW;
  END IF;

  FOR i IN CheckEnabled'RANGE LOOP
    IF CheckEnabled(i) = TRUE THEN
      CheckEnScalar := TRUE;
    END IF;
    ViolationInt(i) := '0'; 
  END LOOP;

  IF (CheckEnScalar) THEN
    InternalTimingCheck (
      TestSignal   => TestSignal,
      RefSignal    => RefSignal,
      TestDelay    => TestDly,
      RefDelay     => RefDly,
      SetupHigh    => SetupHigh,
      SetupLow     => SetupLow,
      HoldHigh     => HoldHigh,
      HoldLow      => HoldLow,
      RefTime      => TimingData.RefTime,
      RefEdge      => RefEdge,
      TestTime     => TimingData.TestTime,
      TestEvent    => TestEvent,
      SetupEn      => TimingData.SetupEn,
      HoldEn       => TimingData.HoldEn,
      CheckInfo    => CheckInfo,
      MsgOn        => MsgOn 
    );

    -- Report any detected violations and set return violation flag
    IF CheckInfo.Violation THEN
      IF (MsgOn) THEN
        VitalMemoryReportViolation (TestSignalName, RefSignalName,
          HeaderMsg, CheckInfo, MsgSeverity );
      END IF;
      IF (XOn) THEN 
        FOR i IN CheckEnabled'RANGE LOOP
          IF CheckEnabled(i) = TRUE THEN
              ViolationInt(i) := 'X'; 
          END IF;
        END LOOP;
      END IF;
    END IF;
  END IF;
  Violation := ViolationInt;
END VitalMemorySetupHoldCheck;

-- ----------------------------------------------------------------------------
PROCEDURE VitalMemorySetupHoldCheck (
  VARIABLE Violation     : OUT    X01ArrayT;
  VARIABLE TimingData    : INOUT  VitalMemoryTimingDataType;
  SIGNAL   TestSignal    : IN     std_logic_vector;
  CONSTANT TestSignalName: IN     STRING := "";
  CONSTANT TestDelay     : IN     VitalDelayArraytype;
  SIGNAL   RefSignal     : IN     std_ulogic;
  CONSTANT RefSignalName : IN     STRING := "";
  CONSTANT RefDelay      : IN     TIME := 0 ns;
  CONSTANT SetupHigh     : IN     VitalDelayArraytype;
  CONSTANT SetupLow      : IN     VitalDelayArraytype;
  CONSTANT HoldHigh      : IN     VitalDelayArraytype;
  CONSTANT HoldLow       : IN     VitalDelayArraytype;
  CONSTANT CheckEnabled  : IN     BOOLEAN := TRUE;
  CONSTANT RefTransition : IN     VitalEdgeSymbolType;
  CONSTANT HeaderMsg     : IN     STRING := " ";
  CONSTANT XOn           : IN     BOOLEAN := TRUE;
  CONSTANT MsgOn         : IN     BOOLEAN := TRUE;
  CONSTANT MsgSeverity   : IN     SEVERITY_LEVEL := WARNING;
  CONSTANT MsgFormat     : IN     VitalMemoryMsgFormatType;
  --IR252 3/23/98
  CONSTANT EnableSetupOnTest : IN   BOOLEAN := TRUE; 
  CONSTANT EnableSetupOnRef  : IN   BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnRef   : IN   BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnTest  : IN   BOOLEAN := TRUE  
) IS
  VARIABLE CheckInfo     : CheckInfoType;
  VARIABLE RefEdge       : BOOLEAN;
  VARIABLE TestEvent     : VitalBoolArrayT(TestSignal'RANGE);
  VARIABLE TestDly       : TIME;
  VARIABLE RefDly        : TIME := Maximum(0 ns, RefDelay);
  VARIABLE bias          : TIME;
BEGIN

  -- Initialization of working area. 
  IF (TimingData.NotFirstFlag = FALSE) THEN
    TimingData.TestLastA := NEW std_logic_vector(TestSignal'RANGE);
    TimingData.TestTimeA := NEW VitalTimeArrayT(TestSignal'RANGE);
    TimingData.HoldEnA   := NEW VitalBoolArrayT(TestSignal'RANGE);
    TimingData.SetupEnA  := NEW VitalBoolArrayT(TestSignal'RANGE);
    FOR i IN TestSignal'RANGE LOOP
      TimingData.TestLastA(i) := To_X01(TestSignal(i));
    END LOOP;
    TimingData.RefLast := To_X01(RefSignal);
    TimingData.NotFirstFlag := TRUE;
  END IF;

  -- Detect reference edges and record the time of the last edge
  RefEdge := EdgeSymbolMatch(TimingData.RefLast, To_X01(RefSignal),
               RefTransition);
  TimingData.RefLast := To_X01(RefSignal);
  IF (RefEdge) THEN
    TimingData.RefTime     := NOW;
    --TimingData.HoldEnA.all := (TestSignal'RANGE=>TRUE);
    --IR252 3/23/98
    FOR i IN TestSignal'RANGE LOOP
      TimingData.SetupEnA(i) 
        := TimingData.SetupEnA(i) AND EnableSetupOnRef; 
      TimingData.HoldEnA(i)  := EnableHoldOnRef; 
    END LOOP;
  END IF;

  -- Detect test (data) changes and record the time of the last change
  FOR i IN TestSignal'RANGE LOOP
    TestEvent(i) := TimingData.TestLastA(i) /= To_X01Z(TestSignal(i));
    TimingData.TestLastA(i) := To_X01Z(TestSignal(i));
    IF TestEvent(i) THEN
      TimingData.SetupEnA(i)  := EnableSetupOnTest ; --IR252 3/23/98
      TimingData.HoldEnA(i) := TimingData.HoldEnA(i) AND EnableHoldOnTest ;
                                                  --IR252 3/23/98
      TimingData.TestTimeA(i) := NOW;
      --TimingData.SetupEnA(i)  := TRUE;
      TimingData.TestTime := NOW;
    END IF;
  END LOOP;

  FOR i IN TestSignal'RANGE LOOP
    Violation(i) := '0';

    IF (CheckEnabled) THEN
      TestDly := Maximum(0 ns, TestDelay(i));
      InternalTimingCheck (
        TestSignal   => TestSignal(i),
        RefSignal    => RefSignal,
        TestDelay    => TestDly,
        RefDelay     => RefDly,
        SetupHigh    => SetupHigh(i),
        SetupLow     => SetupLow(i),
        HoldHigh     => HoldHigh(i),
        HoldLow      => HoldLow(i),
        RefTime      => TimingData.RefTime,
        RefEdge      => RefEdge,
        TestTime     => TimingData.TestTimeA(i),
        TestEvent    => TestEvent(i),
        SetupEn      => TimingData.SetupEnA(i),
        HoldEn       => TimingData.HoldEnA(i),
        CheckInfo    => CheckInfo,
        MsgOn        => MsgOn 
      );

      -- Report any detected violations and set return violation flag
      IF CheckInfo.Violation THEN
        IF (MsgOn) THEN
          VitalMemoryReportViolation (TestSignalName, RefSignalName, i ,
            HeaderMsg, CheckInfo, MsgFormat, MsgSeverity );
        END IF;
        IF (XOn) THEN
          Violation(i) := 'X';
        END IF;
      END IF;
    END IF;
  END LOOP;

END VitalMemorySetupHoldCheck;

-- ----------------------------------------------------------------------------
PROCEDURE VitalMemorySetupHoldCheck (
  VARIABLE Violation     : OUT    X01ArrayT;
  VARIABLE TimingData    : INOUT  VitalMemoryTimingDataType;
  SIGNAL   TestSignal    : IN     std_logic_vector;
  CONSTANT TestSignalName: IN     STRING := "";
  CONSTANT TestDelay     : IN     VitalDelayArraytype;
  SIGNAL   RefSignal     : IN     std_ulogic;
  CONSTANT RefSignalName : IN     STRING := "";
  CONSTANT RefDelay      : IN     TIME := 0 ns;
  CONSTANT SetupHigh     : IN     VitalDelayArraytype;
  CONSTANT SetupLow      : IN     VitalDelayArraytype;
  CONSTANT HoldHigh      : IN     VitalDelayArraytype;
  CONSTANT HoldLow       : IN     VitalDelayArraytype;
  CONSTANT CheckEnabled  : IN     VitalBoolArrayT;
  CONSTANT RefTransition : IN     VitalEdgeSymbolType;
  CONSTANT ArcType       : IN     VitalMemoryArcType := CrossArc;
  CONSTANT NumBitsPerSubWord : IN INTEGER := 1;
  CONSTANT HeaderMsg     : IN     STRING := " ";
  CONSTANT XOn           : IN     BOOLEAN := TRUE;
  CONSTANT MsgOn         : IN     BOOLEAN := TRUE;
  CONSTANT MsgSeverity   : IN     SEVERITY_LEVEL := WARNING;
  CONSTANT MsgFormat     : IN     VitalMemoryMsgFormatType;
  --IR252 3/23/98
  CONSTANT EnableSetupOnTest : IN   BOOLEAN := TRUE; 
  CONSTANT EnableSetupOnRef  : IN   BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnRef   : IN   BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnTest  : IN   BOOLEAN := TRUE  
) IS
  VARIABLE CheckInfo       : CheckInfoType;
  VARIABLE ViolationInt    : X01ArrayT(TestSignal'RANGE);
  VARIABLE ViolationIntNorm: X01ArrayT(TestSignal'LENGTH-1 downto 0);
  VARIABLE ViolationNorm   : X01ArrayT(Violation'LENGTH-1 downto 0);
  VARIABLE CheckEnInt      : VitalBoolArrayT(TestSignal'RANGE);
  VARIABLE CheckEnIntNorm  : VitalBoolArrayT(TestSignal'LENGTH-1 downto 0);
  VARIABLE CheckEnScalar   : BOOLEAN := FALSE; --Mem IR 401
  VARIABLE CheckEnabledNorm: VitalBoolArrayT(CheckEnabled'LENGTH-1 downto 0);
  VARIABLE RefEdge         : BOOLEAN;
  VARIABLE TestEvent       : VitalBoolArrayT(TestSignal'RANGE);
  VARIABLE TestDly         : TIME;
  VARIABLE RefDly          : TIME := Maximum(0 ns, RefDelay);
  VARIABLE bias            : TIME;
BEGIN

  -- Initialization of working area. 
  IF (TimingData.NotFirstFlag = FALSE) THEN
    TimingData.TestLastA := NEW std_logic_vector(TestSignal'RANGE);
    TimingData.TestTimeA := NEW VitalTimeArrayT(TestSignal'RANGE);
    TimingData.HoldEnA   := NEW VitalBoolArrayT(TestSignal'RANGE);
    TimingData.SetupEnA  := NEW VitalBoolArrayT(TestSignal'RANGE);
    FOR i IN TestSignal'RANGE LOOP
      TimingData.TestLastA(i) := To_X01(TestSignal(i));
    END LOOP;
    TimingData.RefLast := To_X01(RefSignal);
    TimingData.NotFirstFlag := TRUE;
  END IF;

  -- Detect reference edges and record the time of the last edge
  RefEdge := EdgeSymbolMatch(TimingData.RefLast, To_X01(RefSignal),
               RefTransition);
  TimingData.RefLast := To_X01(RefSignal);
  IF RefEdge THEN
    TimingData.RefTime     := NOW;
    --TimingData.HoldEnA.all := (TestSignal'RANGE=>TRUE);
    --IR252 3/23/98
    FOR i IN TestSignal'RANGE LOOP
      TimingData.SetupEnA(i) 
        := TimingData.SetupEnA(i) AND EnableSetupOnRef; 
      TimingData.HoldEnA(i)  := EnableHoldOnRef; 
    END LOOP;
  END IF;

  -- Detect test (data) changes and record the time of the last change
  FOR i IN TestSignal'RANGE LOOP
    TestEvent(i) := TimingData.TestLastA(i) /= To_X01Z(TestSignal(i));
    TimingData.TestLastA(i) := To_X01Z(TestSignal(i));
    IF TestEvent(i) THEN
      TimingData.SetupEnA(i)  := EnableSetupOnTest ; --IR252 3/23/98
      TimingData.HoldEnA(i) := TimingData.HoldEnA(i) AND EnableHoldOnTest ;
                                                  --IR252 3/23/98
      TimingData.TestTimeA(i) := NOW;
      --TimingData.SetupEnA(i)  := TRUE;
      TimingData.TestTime := NOW;
    END IF;
  END LOOP;

  IF ArcType = CrossArc THEN
    CheckEnScalar := FALSE;
    FOR i IN CheckEnabled'RANGE LOOP
      IF CheckEnabled(i) = TRUE THEN
        CheckEnScalar := TRUE;
      END IF;
    END LOOP;
    FOR i IN CheckEnInt'RANGE LOOP
      CheckEnInt(i) := CheckEnScalar;
    END LOOP;
  ELSE
    FOR i IN CheckEnIntNorm'RANGE LOOP
      CheckEnIntNorm(i) := CheckEnabledNorm(i / NumBitsPerSubWord );
    END LOOP;
    CheckEnInt := CheckEnIntNorm;
  END IF;

  FOR i IN TestSignal'RANGE LOOP
    ViolationInt(i) := '0';

    IF (CheckEnInt(i)) THEN
      TestDly := Maximum(0 ns, TestDelay(i));
      InternalTimingCheck (
        TestSignal   => TestSignal(i),
        RefSignal    => RefSignal,
        TestDelay    => TestDly,
        RefDelay     => RefDly,
        SetupHigh    => SetupHigh(i),
        SetupLow     => SetupLow(i),
        HoldHigh     => HoldHigh(i),
        HoldLow      => HoldLow(i),
        RefTime      => TimingData.RefTime,
        RefEdge      => RefEdge,
        TestTime     => TimingData.TestTimeA(i),
        TestEvent    => TestEvent(i),
        SetupEn      => TimingData.SetupEnA(i),
        HoldEn       => TimingData.HoldEnA(i),
        CheckInfo    => CheckInfo,
        MsgOn        => MsgOn 
      );

      -- Report any detected violations and set return violation flag
      IF CheckInfo.Violation THEN
        IF (MsgOn) THEN
          VitalMemoryReportViolation (TestSignalName, RefSignalName, i ,
            HeaderMsg, CheckInfo, MsgFormat, MsgSeverity );
        END IF;
        IF (XOn) THEN
          ViolationInt(i) := 'X';
        END IF;
      END IF;
    END IF;
  END LOOP;

  IF (ViolationInt'LENGTH = Violation'LENGTH) THEN
    Violation := ViolationInt;
  ELSE
    ViolationIntNorm := ViolationInt;
    FOR i IN ViolationNorm'RANGE LOOP
      ViolationNorm(i) := '0';
    END LOOP;
    FOR i IN ViolationIntNorm'RANGE LOOP
      IF (ViolationIntNorm(i) = 'X') THEN
        ViolationNorm(i / NumBitsPerSubWord) := 'X';
      END IF;
    END LOOP;
    Violation := ViolationNorm;
  END IF;

END VitalMemorySetupHoldCheck;

-- ----------------------------------------------------------------------------
PROCEDURE VitalMemorySetupHoldCheck (
  VARIABLE Violation     : OUT    X01ArrayT;
  VARIABLE TimingData    : INOUT  VitalMemoryTimingDataType;
  SIGNAL   TestSignal    : IN     std_logic_vector;
  CONSTANT TestSignalName: IN     STRING := "";
  CONSTANT TestDelay     : IN     VitalDelayArraytype;
  SIGNAL   RefSignal     : IN     std_logic_vector;
  CONSTANT RefSignalName : IN     STRING := "";
  CONSTANT RefDelay      : IN     VitalDelayArraytype;
  CONSTANT SetupHigh     : IN     VitalDelayArraytype;
  CONSTANT SetupLow      : IN     VitalDelayArraytype;
  CONSTANT HoldHigh      : IN     VitalDelayArraytype;
  CONSTANT HoldLow       : IN     VitalDelayArraytype;
  CONSTANT CheckEnabled  : IN     BOOLEAN := TRUE;
  CONSTANT RefTransition : IN     VitalEdgeSymbolType;
  CONSTANT ArcType       : IN     VitalMemoryArcType := CrossArc;
  CONSTANT NumBitsPerSubWord : IN INTEGER := 1;
  CONSTANT HeaderMsg     : IN     STRING := " ";
  CONSTANT XOn           : IN     BOOLEAN := TRUE;
  CONSTANT MsgOn         : IN     BOOLEAN := TRUE;
  CONSTANT MsgSeverity   : IN     SEVERITY_LEVEL := WARNING;
  CONSTANT MsgFormat     : IN     VitalMemoryMsgFormatType;
  --IR252 3/23/98
  CONSTANT EnableSetupOnTest : IN   BOOLEAN := TRUE; 
  CONSTANT EnableSetupOnRef  : IN   BOOLEAN := TRUE;
  CONSTANT EnableHoldOnRef   : IN   BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnTest  : IN   BOOLEAN := TRUE 
) IS
  VARIABLE CheckInfo       : CheckInfoType;
  VARIABLE RefEdge         : VitalBoolArrayT(RefSignal'LENGTH-1 downto 0);
  VARIABLE TestEvent       : VitalBoolArrayT(TestSignal'LENGTH-1 downto 0);
  VARIABLE TestDly         : TIME;
  VARIABLE RefDly          : TIME;
  VARIABLE bias            : TIME;
  VARIABLE NumTestBits     : NATURAL := TestSignal'LENGTH;
  VARIABLE NumRefBits      : NATURAL := RefSignal'LENGTH;
  VARIABLE NumChecks       : NATURAL;

  VARIABLE ViolationTest   : X01ArrayT(NumTestBits-1 downto 0);
  VARIABLE ViolationRef    : X01ArrayT(NumRefBits-1 downto 0);

  VARIABLE TestSignalNorm  : std_logic_vector(NumTestBits-1 downto 0)
                              := TestSignal;
  VARIABLE TestDelayNorm   : VitalDelayArraytype(NumTestBits-1 downto 0)
                              := TestDelay;
  VARIABLE RefSignalNorm   : std_logic_vector(NumRefBits-1 downto 0)
                              := RefSignal;
  VARIABLE RefDelayNorm    : VitalDelayArraytype(NumRefBits-1 downto 0)
                              := RefDelay;
  VARIABLE SetupHighNorm   : VitalDelayArraytype(SetupHigh'LENGTH-1 downto 0)
                              := SetupHigh;
  VARIABLE SetupLowNorm    : VitalDelayArraytype(SetupLow'LENGTH-1 downto 0)
                              := SetupLow;
  VARIABLE HoldHighNorm    : VitalDelayArraytype(HoldHigh'LENGTH-1 downto 0)
                              := HoldHigh;
  VARIABLE HoldLowNorm     : VitalDelayArraytype(HoldLow'LENGTH-1 downto 0)
                              := HoldLow;

  VARIABLE RefBitLow       : NATURAL;
  VARIABLE RefBitHigh      : NATURAL;
  VARIABLE EnArrayIndex    : NATURAL;
  VARIABLE TimingArrayIndex: NATURAL;
BEGIN

  -- Initialization of working area. 
  IF (TimingData.NotFirstFlag = FALSE) THEN
    TimingData.TestLastA := NEW std_logic_vector(NumTestBits-1 downto 0);
    TimingData.TestTimeA := NEW VitalTimeArrayT(NumTestBits-1 downto 0);
    TimingData.RefTimeA  := NEW VitalTimeArrayT(NumRefBits-1 downto 0);
    TimingData.RefLastA  := NEW X01ArrayT(NumRefBits-1 downto 0);
    IF (ArcType = CrossArc) THEN
      NumChecks := RefSignal'LENGTH * TestSignal'LENGTH;
    ELSE
      NumChecks := TestSignal'LENGTH;
    END IF;
    TimingData.HoldEnA   := NEW VitalBoolArrayT(NumChecks-1 downto 0);
    TimingData.SetupEnA  := NEW VitalBoolArrayT(NumChecks-1 downto 0);

    FOR i IN TestSignalNorm'RANGE LOOP
      TimingData.TestLastA(i) := To_X01(TestSignalNorm(i));
    END LOOP;

    FOR i IN RefSignalNorm'RANGE LOOP
      TimingData.RefLastA(i) := To_X01(RefSignalNorm(i));
    END LOOP;
    TimingData.NotFirstFlag := TRUE;
  END IF;

  -- Detect reference edges and record the time of the last edge
  FOR i IN RefSignalNorm'RANGE LOOP
    RefEdge(i) := EdgeSymbolMatch(TimingData.RefLastA(i), 
                  To_X01(RefSignalNorm(i)), RefTransition);
    TimingData.RefLastA(i) := To_X01(RefSignalNorm(i));
    IF (RefEdge(i)) THEN
      TimingData.RefTimeA(i)  := NOW;
    END IF;
  END LOOP;

  -- Detect test (data) changes and record the time of the last change
  FOR i IN TestSignalNorm'RANGE LOOP
    TestEvent(i) := TimingData.TestLastA(i) /= To_X01Z(TestSignalNorm(i));
    TimingData.TestLastA(i) := To_X01Z(TestSignalNorm(i));
    IF (TestEvent(i)) THEN
      TimingData.TestTimeA(i) := NOW;
    END IF;
  END LOOP;

  FOR i IN ViolationTest'RANGE LOOP
    ViolationTest(i) := '0'; 
  END LOOP;
  FOR i IN ViolationRef'RANGE LOOP
    ViolationRef(i) := '0'; 
  END LOOP;

  FOR i IN TestSignalNorm'RANGE LOOP
    IF (ArcType = CrossArc) THEN
      FOR j IN RefSignalNorm'RANGE LOOP
        IF (TestEvent(i)) THEN
          --TimingData.SetupEnA(i*NumRefBits+j) := TRUE;
          --IR252
          TimingData.SetupEnA(i*NumRefBits+j) := EnableSetupOnTest;
          TimingData.HoldEnA(i*NumRefBits+j) 
               := TimingData.HoldEnA(i*NumRefBits+j) AND EnableHoldOnTest;
        END IF;
        IF (RefEdge(j)) THEN
          --TimingData.HoldEnA(i*NumRefBits+j) := TRUE;
          --IR252 
          TimingData.HoldEnA(i*NumRefBits+j) := EnableHoldOnRef; 
          TimingData.SetupEnA(i*NumRefBits+j) 
               := TimingData.SetupEnA(i*NumRefBits+j) AND EnableSetupOnRef;
        END IF;
      END LOOP;
      RefBitLow := 0;
      RefBitHigh := NumRefBits-1;
      TimingArrayIndex := i;
    ELSE
      IF ArcType = SubwordArc THEN
        RefBitLow  := i / NumBitsPerSubWord;
        TimingArrayIndex := i + NumTestBits * RefBitLow;
      ELSE
        RefBitLow  := i;
        TimingArrayIndex := i;
      END IF;
      RefBitHigh := RefBitLow;
      IF TestEvent(i) THEN
        --TimingData.SetupEnA(i) := TRUE;
        --IR252
        TimingData.SetupEnA(i) := EnableSetupOnTest;
        TimingData.HoldEnA(i) := TimingData.HoldEnA(i) AND EnableHoldOnTest;
      END IF;
      IF RefEdge(RefBitLow) THEN
        --TimingData.HoldEnA(i) := TRUE;
        --IR252
        TimingData.HoldEnA(i) := EnableHoldOnRef;
        TimingData.SetupEnA(i) := TimingData.SetupEnA(i) AND EnableSetupOnRef;
      END IF;
    END IF;

    EnArrayIndex := i;
    FOR j IN RefBitLow to RefBitHigh LOOP

      IF (CheckEnabled) THEN
        TestDly := Maximum(0 ns, TestDelayNorm(i));
        RefDly  := Maximum(0 ns, RefDelayNorm(j));

        InternalTimingCheck (
          TestSignal   => TestSignalNorm(i),
          RefSignal    => RefSignalNorm(j),
          TestDelay    => TestDly,
          RefDelay     => RefDly,
          SetupHigh    => SetupHighNorm(TimingArrayIndex),
          SetupLow     => SetupLowNorm(TimingArrayIndex),
          HoldHigh     => HoldHighNorm(TimingArrayIndex),
          HoldLow      => HoldLowNorm(TimingArrayIndex),
          RefTime      => TimingData.RefTimeA(j),
          RefEdge      => RefEdge(j),
          TestTime     => TimingData.TestTimeA(i),
          TestEvent    => TestEvent(i),
          SetupEn      => TimingData.SetupEnA(EnArrayIndex),
          HoldEn       => TimingData.HoldEnA(EnArrayIndex),
          CheckInfo    => CheckInfo,
          MsgOn        => MsgOn 
        );

        -- Report any detected violations and set return violation flag
        IF (CheckInfo.Violation) THEN
          IF (MsgOn) THEN
             VitalMemoryReportViolation (TestSignalName, RefSignalName, i, j,
                    TestSignal, RefSignal, HeaderMsg, CheckInfo, 
                    MsgFormat, MsgSeverity );
          END IF;
          IF (XOn) THEN
              ViolationTest(i) := 'X'; 
              ViolationRef(j) := 'X'; 
          END IF;
        END IF;
      END IF;

      TimingArrayIndex := TimingArrayIndex + NumRefBits;
      EnArrayIndex    := EnArrayIndex + NumRefBits;

    END LOOP;
  END LOOP;

  IF (ArcType = CrossArc) THEN
    Violation := ViolationRef;
  ELSE
    IF (Violation'LENGTH = ViolationRef'LENGTH) THEN
      Violation := ViolationRef;
    ELSE
      Violation := ViolationTest;
    END IF;
  END IF;

END VitalMemorySetupHoldCheck;

-- ----------------------------------------------------------------------------
PROCEDURE VitalMemorySetupHoldCheck (
  VARIABLE Violation     : OUT    X01ArrayT;
  VARIABLE TimingData    : INOUT  VitalMemoryTimingDataType;
  SIGNAL   TestSignal    : IN     std_logic_vector;
  CONSTANT TestSignalName: IN     STRING := "";
  CONSTANT TestDelay     : IN     VitalDelayArraytype;
  SIGNAL   RefSignal     : IN     std_logic_vector;
  CONSTANT RefSignalName : IN     STRING := "";
  CONSTANT RefDelay      : IN     VitalDelayArraytype;
  CONSTANT SetupHigh     : IN     VitalDelayArraytype;
  CONSTANT SetupLow      : IN     VitalDelayArraytype;
  CONSTANT HoldHigh      : IN     VitalDelayArraytype;
  CONSTANT HoldLow       : IN     VitalDelayArraytype;
  CONSTANT CheckEnabled  : IN     VitalBoolArrayT;
  CONSTANT RefTransition : IN     VitalEdgeSymbolType;
  CONSTANT ArcType       : IN     VitalMemoryArcType := CrossArc;
  CONSTANT NumBitsPerSubWord : IN INTEGER := 1;
  CONSTANT HeaderMsg     : IN     STRING := " ";
  CONSTANT XOn           : IN     BOOLEAN := TRUE;
  CONSTANT MsgOn         : IN     BOOLEAN := TRUE;
  CONSTANT MsgSeverity   : IN     SEVERITY_LEVEL := WARNING;
  CONSTANT MsgFormat     : IN     VitalMemoryMsgFormatType;
  --IR252 3/23/98
  CONSTANT EnableSetupOnTest : IN   BOOLEAN := TRUE; 
  CONSTANT EnableSetupOnRef  : IN   BOOLEAN := TRUE;
  CONSTANT EnableHoldOnRef   : IN   BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnTest  : IN   BOOLEAN := TRUE 
) IS

  VARIABLE CheckInfo       : CheckInfoType;
  VARIABLE RefEdge         : VitalBoolArrayT(RefSignal'LENGTH-1 downto 0);
  VARIABLE TestEvent       : VitalBoolArrayT(TestSignal'LENGTH-1 downto 0);
  VARIABLE TestDly         : TIME;
  VARIABLE RefDly          : TIME;
  VARIABLE bias            : TIME;
  VARIABLE NumTestBits     : NATURAL := TestSignal'LENGTH;
  VARIABLE NumRefBits      : NATURAL := RefSignal'LENGTH;
  VARIABLE NumChecks       : NATURAL;

  VARIABLE ViolationTest   : X01ArrayT(NumTestBits-1 downto 0);
  VARIABLE ViolationRef    : X01ArrayT(NumRefBits-1 downto 0);

  VARIABLE TestSignalNorm  : std_logic_vector(NumTestBits-1 downto 0)
                              := TestSignal;
  VARIABLE TestDelayNorm   : VitalDelayArraytype(NumTestBits-1 downto 0)
                              := TestDelay;
  VARIABLE RefSignalNorm   : std_logic_vector(NumRefBits-1 downto 0)
                              := RefSignal;
  VARIABLE RefDelayNorm    : VitalDelayArraytype(NumRefBits-1 downto 0)
                              := RefDelay;
  VARIABLE CheckEnNorm     : VitalBoolArrayT(NumRefBits-1 downto 0)
                              := CheckEnabled;
  VARIABLE SetupHighNorm   : VitalDelayArraytype(SetupHigh'LENGTH-1 downto 0)
                              := SetupHigh;
  VARIABLE SetupLowNorm    : VitalDelayArraytype(SetupLow'LENGTH-1 downto 0)
                              := SetupLow;
  VARIABLE HoldHighNorm    : VitalDelayArraytype(HoldHigh'LENGTH-1 downto 0)
                              := HoldHigh;
  VARIABLE HoldLowNorm     : VitalDelayArraytype(HoldLow'LENGTH-1 downto 0)
                              := HoldLow;

  VARIABLE RefBitLow       : NATURAL;
  VARIABLE RefBitHigh      : NATURAL;
  VARIABLE EnArrayIndex    : NATURAL;
  VARIABLE TimingArrayIndex: NATURAL;
BEGIN

  -- Initialization of working area. 
  IF (TimingData.NotFirstFlag = FALSE) THEN
    TimingData.TestLastA := NEW std_logic_vector(NumTestBits-1 downto 0);
    TimingData.TestTimeA := NEW VitalTimeArrayT(NumTestBits-1 downto 0);
    TimingData.RefTimeA  := NEW VitalTimeArrayT(NumRefBits-1 downto 0);
    TimingData.RefLastA  := NEW X01ArrayT(NumRefBits-1 downto 0);
    IF ArcType = CrossArc THEN
      NumChecks := RefSignal'LENGTH * TestSignal'LENGTH;
    ELSE
      NumChecks := TestSignal'LENGTH;
    END IF;
    TimingData.HoldEnA   := NEW VitalBoolArrayT(NumChecks-1 downto 0);
    TimingData.SetupEnA  := NEW VitalBoolArrayT(NumChecks-1 downto 0);

    FOR i IN TestSignalNorm'RANGE LOOP
      TimingData.TestLastA(i) := To_X01(TestSignalNorm(i));
    END LOOP;

    FOR i IN RefSignalNorm'RANGE LOOP
      TimingData.RefLastA(i) := To_X01(RefSignalNorm(i));
    END LOOP;
    TimingData.NotFirstFlag := TRUE;
  END IF;

  -- Detect reference edges and record the time of the last edge
  FOR i IN RefSignalNorm'RANGE LOOP
    RefEdge(i) := EdgeSymbolMatch(TimingData.RefLastA(i), 
                  To_X01(RefSignalNorm(i)), RefTransition);
    TimingData.RefLastA(i) := To_X01(RefSignalNorm(i));
    IF RefEdge(i) THEN
      TimingData.RefTimeA(i)  := NOW;
    END IF;
  END LOOP;

  -- Detect test (data) changes and record the time of the last change
  FOR i IN TestSignalNorm'RANGE LOOP
    TestEvent(i) := TimingData.TestLastA(i) /= To_X01Z(TestSignalNorm(i));
    TimingData.TestLastA(i) := To_X01Z(TestSignalNorm(i));
    IF TestEvent(i) THEN
      TimingData.TestTimeA(i) := NOW;
    END IF;
  END LOOP;

  FOR i IN ViolationTest'RANGE LOOP
    ViolationTest(i) := '0'; 
  END LOOP;
  FOR i IN ViolationRef'RANGE LOOP
    ViolationRef(i) := '0'; 
  END LOOP;

  FOR i IN TestSignalNorm'RANGE LOOP
    IF (ArcType = CrossArc) THEN
      FOR j IN RefSignalNorm'RANGE LOOP
        IF (TestEvent(i)) THEN
          --TimingData.SetupEnA(i*NumRefBits+j) := TRUE;
          --IR252
          TimingData.SetupEnA(i*NumRefBits+j) := EnableSetupOnTest;
          TimingData.HoldEnA(i*NumRefBits+j) 
               := TimingData.HoldEnA(i*NumRefBits+j) AND EnableHoldOnTest;
        END IF;
        IF (RefEdge(j)) THEN
          --TimingData.HoldEnA(i*NumRefBits+j) := TRUE;
          --IR252 
          TimingData.HoldEnA(i*NumRefBits+j) := EnableHoldOnRef; 
          TimingData.SetupEnA(i*NumRefBits+j) 
               := TimingData.SetupEnA(i*NumRefBits+j) AND EnableSetupOnRef;
        END IF;
      END LOOP;
      RefBitLow := 0;
      RefBitHigh := NumRefBits-1;
      TimingArrayIndex := i;
    ELSE
      IF (ArcType = SubwordArc) THEN
        RefBitLow  := i / NumBitsPerSubWord;
        TimingArrayIndex := i + NumTestBits * RefBitLow;
      ELSE
        RefBitLow  := i;
        TimingArrayIndex := i;
      END IF;
      RefBitHigh := RefBitLow;
      IF (TestEvent(i)) THEN
        --TimingData.SetupEnA(i) := TRUE;
        --IR252
        TimingData.SetupEnA(i) := EnableSetupOnTest;
        TimingData.HoldEnA(i) := TimingData.HoldEnA(i) AND EnableHoldOnTest;
      END IF;
      IF (RefEdge(RefBitLow)) THEN
        --TimingData.HoldEnA(i) := TRUE;
        --IR252
        TimingData.HoldEnA(i) := EnableHoldOnRef;
        TimingData.SetupEnA(i) := TimingData.SetupEnA(i) AND EnableSetupOnRef;
      END IF;
    END IF;

    EnArrayIndex := i;
    FOR j IN RefBitLow to RefBitHigh LOOP
      IF (CheckEnNorm(j)) THEN
        TestDly := Maximum(0 ns, TestDelayNorm(i));
        RefDly  := Maximum(0 ns, RefDelayNorm(j));

        InternalTimingCheck (
          TestSignal   => TestSignalNorm(i),
          RefSignal    => RefSignalNorm(j),
          TestDelay    => TestDly,
          RefDelay     => RefDly,
          SetupHigh    => SetupHighNorm(TimingArrayIndex),
          SetupLow     => SetupLowNorm(TimingArrayIndex),
          HoldHigh     => HoldHighNorm(TimingArrayIndex),
          HoldLow      => HoldLowNorm(TimingArrayIndex),
          RefTime      => TimingData.RefTimeA(j),
          RefEdge      => RefEdge(j),
          TestTime     => TimingData.TestTimeA(i),
          TestEvent    => TestEvent(i),
          SetupEn      => TimingData.SetupEnA(EnArrayIndex),
          HoldEn       => TimingData.HoldEnA(EnArrayIndex),
          CheckInfo    => CheckInfo,
          MsgOn        => MsgOn 
        );

        -- Report any detected violations and set return violation flag
        IF (CheckInfo.Violation) THEN
          IF (MsgOn) THEN
            VitalMemoryReportViolation (TestSignalName, RefSignalName, i, j,
                    TestSignal, RefSignal, HeaderMsg, CheckInfo, 
                    MsgFormat, MsgSeverity );
          END IF;

          IF (XOn) THEN
            ViolationTest(i) := 'X'; 
            ViolationRef(j) := 'X'; 
          END IF;
        END IF;
      END IF;

      TimingArrayIndex := TimingArrayIndex + NumRefBits;
      EnArrayIndex    := EnArrayIndex + NumRefBits;
    END LOOP;
  END LOOP;

  IF (ArcType = CrossArc) THEN
    Violation := ViolationRef;
  ELSE
    IF (Violation'LENGTH = ViolationRef'LENGTH) THEN
      Violation := ViolationRef;
    ELSE
      Violation := ViolationTest;
    END IF;
  END IF;

END VitalMemorySetupHoldCheck;

-- ----------------------------------------------------------------------------
-- scalar violations not needed 
-- ----------------------------------------------------------------------------
PROCEDURE VitalMemorySetupHoldCheck (
  VARIABLE Violation     : OUT    X01;
  VARIABLE TimingData    : INOUT  VitalMemoryTimingDataType;
  SIGNAL   TestSignal    : IN     std_logic_vector;
  CONSTANT TestSignalName: IN     STRING := "";
  CONSTANT TestDelay     : IN     VitalDelayArraytype;
  SIGNAL   RefSignal     : IN     std_ulogic;
  CONSTANT RefSignalName : IN     STRING := "";
  CONSTANT RefDelay      : IN     TIME := 0 ns;
  CONSTANT SetupHigh     : IN     VitalDelayArraytype;
  CONSTANT SetupLow      : IN     VitalDelayArraytype;
  CONSTANT HoldHigh      : IN     VitalDelayArraytype;
  CONSTANT HoldLow       : IN     VitalDelayArraytype;
  CONSTANT CheckEnabled  : IN     BOOLEAN := TRUE;
  CONSTANT RefTransition : IN     VitalEdgeSymbolType;
  CONSTANT HeaderMsg     : IN     STRING := " ";
  CONSTANT XOn           : IN     BOOLEAN := TRUE;
  CONSTANT MsgOn         : IN     BOOLEAN := TRUE;
  CONSTANT MsgSeverity   : IN     SEVERITY_LEVEL := WARNING;
  CONSTANT MsgFormat     : IN     VitalMemoryMsgFormatType;
  --IR252 3/23/98
  CONSTANT EnableSetupOnTest : IN   BOOLEAN := TRUE; 
  CONSTANT EnableSetupOnRef  : IN   BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnRef   : IN   BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnTest  : IN   BOOLEAN := TRUE  
) IS
  VARIABLE CheckInfo     : CheckInfoType;
  VARIABLE RefEdge       : BOOLEAN;
  VARIABLE TestEvent     : VitalBoolArrayT(TestSignal'RANGE);
  VARIABLE TestDly       : TIME;
  VARIABLE RefDly        : TIME := Maximum(0 ns, RefDelay);
  VARIABLE bias          : TIME;

BEGIN

  -- Initialization of working area. 
  IF (TimingData.NotFirstFlag = FALSE) THEN
    TimingData.TestLastA := NEW std_logic_vector(TestSignal'RANGE);
    TimingData.TestTimeA := NEW VitalTimeArrayT(TestSignal'RANGE);
    TimingData.HoldEnA   := NEW VitalBoolArrayT(TestSignal'RANGE);
    TimingData.SetupEnA  := NEW VitalBoolArrayT(TestSignal'RANGE);
    FOR i IN TestSignal'RANGE LOOP
      TimingData.TestLastA(i) := To_X01(TestSignal(i));
    END LOOP;
    TimingData.RefLast := To_X01(RefSignal);
    TimingData.NotFirstFlag := TRUE;
  END IF;

  -- Detect reference edges and record the time of the last edge
  RefEdge := EdgeSymbolMatch(TimingData.RefLast, To_X01(RefSignal),
               RefTransition);
  TimingData.RefLast := To_X01(RefSignal);
  IF (RefEdge) THEN
    TimingData.RefTime     := NOW;
    --TimingData.HoldEnA.all := (TestSignal'RANGE=>TRUE);
    --IR252 3/23/98
    FOR i IN TestSignal'RANGE LOOP
      TimingData.SetupEnA(i) 
        := TimingData.SetupEnA(i) AND EnableSetupOnRef; 
      TimingData.HoldEnA(i)  := EnableHoldOnRef; 
    END LOOP;
  END IF;

  -- Detect test (data) changes and record the time of the last change
  FOR i IN TestSignal'RANGE LOOP
    TestEvent(i) := TimingData.TestLastA(i) /= To_X01Z(TestSignal(i));
    TimingData.TestLastA(i) := To_X01Z(TestSignal(i));
    IF TestEvent(i) THEN
      TimingData.SetupEnA(i)  := EnableSetupOnTest ; --IR252 3/23/98
      TimingData.HoldEnA(i) := TimingData.HoldEnA(i) AND EnableHoldOnTest ;
                                                  --IR252 3/23/98
      TimingData.TestTimeA(i) := NOW;
      --TimingData.SetupEnA(i)  := TRUE;
      TimingData.TestTime := NOW;
    END IF;
  END LOOP;

  Violation := '0';
  FOR i IN TestSignal'RANGE LOOP
    IF (CheckEnabled) THEN
      TestDly := Maximum(0 ns, TestDelay(i));
      InternalTimingCheck (
        TestSignal   => TestSignal(i),
        RefSignal    => RefSignal,
        TestDelay    => TestDly,
        RefDelay     => RefDly,
        SetupHigh    => SetupHigh(i),
        SetupLow     => SetupLow(i),
        HoldHigh     => HoldHigh(i),
        HoldLow      => HoldLow(i),
        RefTime      => TimingData.RefTime,
        RefEdge      => RefEdge,
        TestTime     => TimingData.TestTimeA(i),
        TestEvent    => TestEvent(i),
        SetupEn      => TimingData.SetupEnA(i),
        HoldEn       => TimingData.HoldEnA(i),
        CheckInfo    => CheckInfo,
        MsgOn        => MsgOn 
      );

      -- Report any detected violations and set return violation flag
      IF CheckInfo.Violation THEN
        IF (MsgOn) THEN
          VitalMemoryReportViolation (TestSignalName, RefSignalName, i ,
                  HeaderMsg, CheckInfo, MsgFormat, MsgSeverity );
        END IF;
        IF (XOn) THEN
          Violation := 'X';
        END IF;
      END IF;
    END IF;
  END LOOP;

END VitalMemorySetupHoldCheck;

-- ----------------------------------------------------------------------------
PROCEDURE VitalMemorySetupHoldCheck (
  VARIABLE Violation     : OUT    X01;
  VARIABLE TimingData    : INOUT  VitalMemoryTimingDataType;            
  SIGNAL   TestSignal    : IN     std_logic_vector;
  CONSTANT TestSignalName: IN     STRING := "";
  CONSTANT TestDelay     : IN     VitalDelayArraytype;
  SIGNAL   RefSignal     : IN     std_logic_vector;
  CONSTANT RefSignalName : IN     STRING := "";
  CONSTANT RefDelay      : IN     VitalDelayArraytype;
  CONSTANT SetupHigh     : IN     VitalDelayArraytype;
  CONSTANT SetupLow      : IN     VitalDelayArraytype;
  CONSTANT HoldHigh      : IN     VitalDelayArraytype;
  CONSTANT HoldLow       : IN     VitalDelayArraytype;
  CONSTANT CheckEnabled  : IN     BOOLEAN := TRUE;
  CONSTANT RefTransition : IN     VitalEdgeSymbolType;
  CONSTANT HeaderMsg     : IN     STRING := " ";
  CONSTANT XOn           : IN     BOOLEAN := TRUE;
  CONSTANT MsgOn         : IN     BOOLEAN := TRUE;
  CONSTANT MsgSeverity   : IN     SEVERITY_LEVEL := WARNING;
  CONSTANT ArcType       : IN     VitalMemoryArcType := CrossArc;
  CONSTANT NumBitsPerSubWord : IN INTEGER := 1;
  CONSTANT MsgFormat     : IN     VitalMemoryMsgFormatType;
  --IR252 3/23/98
  CONSTANT EnableSetupOnTest : IN   BOOLEAN := TRUE; 
  CONSTANT EnableSetupOnRef  : IN   BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnRef   : IN   BOOLEAN := TRUE;
  CONSTANT EnableHoldOnTest  : IN   BOOLEAN := TRUE 
) IS
  VARIABLE CheckInfo       : CheckInfoType;
  VARIABLE RefEdge         : VitalBoolArrayT(RefSignal'LENGTH-1 downto 0);
  VARIABLE TestEvent       : VitalBoolArrayT(TestSignal'LENGTH-1 downto 0);
  VARIABLE TestDly         : TIME;
  VARIABLE RefDly          : TIME;
  VARIABLE bias            : TIME;
  VARIABLE NumTestBits     : NATURAL := TestSignal'LENGTH;
  VARIABLE NumRefBits      : NATURAL := RefSignal'LENGTH;
  VARIABLE NumChecks       : NATURAL;

  VARIABLE TestSignalNorm  : std_logic_vector(NumTestBits-1 downto 0)
                              := TestSignal;
  VARIABLE TestDelayNorm   : VitalDelayArraytype(NumTestBits-1 downto 0)
                              := TestDelay;
  VARIABLE RefSignalNorm   : std_logic_vector(NumRefBits-1 downto 0)
                              := RefSignal;
  VARIABLE RefDelayNorm    : VitalDelayArraytype(NumRefBits-1 downto 0)
                              := RefDelay;
  VARIABLE SetupHighNorm   : VitalDelayArraytype(SetupHigh'LENGTH-1 downto 0)
                              := SetupHigh;
  VARIABLE SetupLowNorm    : VitalDelayArraytype(SetupLow'LENGTH-1 downto 0)
                              := SetupLow;
  VARIABLE HoldHighNorm    : VitalDelayArraytype(HoldHigh'LENGTH-1 downto 0)
                              := HoldHigh;
  VARIABLE HoldLowNorm     : VitalDelayArraytype(HoldLow'LENGTH-1 downto 0)
                              := HoldLow;

  VARIABLE RefBitLow       : NATURAL;
  VARIABLE RefBitHigh      : NATURAL;
  VARIABLE EnArrayIndex    : NATURAL;
  VARIABLE TimingArrayIndex: NATURAL;
BEGIN

  -- Initialization of working area. 
  IF (TimingData.NotFirstFlag = FALSE) THEN
    TimingData.TestLastA := NEW std_logic_vector(NumTestBits-1 downto 0);
    TimingData.TestTimeA := NEW VitalTimeArrayT(NumTestBits-1 downto 0);
    TimingData.RefTimeA  := NEW VitalTimeArrayT(NumRefBits-1 downto 0);
    TimingData.RefLastA  := NEW X01ArrayT(NumRefBits-1 downto 0);
    IF (ArcType = CrossArc) THEN
      NumChecks := RefSignal'LENGTH * TestSignal'LENGTH;
    ELSE
      NumChecks := TestSignal'LENGTH;
    END IF;
    TimingData.HoldEnA   := NEW VitalBoolArrayT(NumChecks-1 downto 0);
    TimingData.SetupEnA  := NEW VitalBoolArrayT(NumChecks-1 downto 0);

    FOR i IN TestSignalNorm'RANGE LOOP
      TimingData.TestLastA(i) := To_X01(TestSignalNorm(i));
    END LOOP;

    FOR i IN RefSignalNorm'RANGE LOOP
      TimingData.RefLastA(i) := To_X01(RefSignalNorm(i));
    END LOOP;
    TimingData.NotFirstFlag := TRUE;
  END IF;

  -- Detect reference edges and record the time of the last edge
  FOR i IN RefSignalNorm'RANGE LOOP
    RefEdge(i) := EdgeSymbolMatch(TimingData.RefLastA(i), 
                  To_X01(RefSignalNorm(i)), RefTransition);
    TimingData.RefLastA(i) := To_X01(RefSignalNorm(i));
    IF (RefEdge(i)) THEN
      TimingData.RefTimeA(i)  := NOW;
    END IF;
  END LOOP;

  -- Detect test (data) changes and record the time of the last change
  FOR i IN TestSignalNorm'RANGE LOOP
    TestEvent(i) := TimingData.TestLastA(i) /= To_X01Z(TestSignalNorm(i));
    TimingData.TestLastA(i) := To_X01Z(TestSignalNorm(i));
    IF (TestEvent(i)) THEN
      TimingData.TestTimeA(i) := NOW;
    END IF;
  END LOOP;

  FOR i IN TestSignalNorm'RANGE LOOP
    IF (ArcType = CrossArc) THEN
      FOR j IN RefSignalNorm'RANGE LOOP
        IF (TestEvent(i)) THEN
          --TimingData.SetupEnA(i*NumRefBits+j) := TRUE;
          --IR252
          TimingData.SetupEnA(i*NumRefBits+j) := EnableSetupOnTest;
          TimingData.HoldEnA(i*NumRefBits+j) 
                 := TimingData.HoldEnA(i*NumRefBits+j) AND EnableHoldOnTest;
        END IF;
        IF (RefEdge(j)) THEN
          --TimingData.HoldEnA(i*NumRefBits+j) := TRUE;
          --IR252 
          TimingData.HoldEnA(i*NumRefBits+j) := EnableHoldOnRef; 
          TimingData.SetupEnA(i*NumRefBits+j) 
                 := TimingData.SetupEnA(i*NumRefBits+j) AND EnableSetupOnRef;
        END IF;
      END LOOP;
      RefBitLow := 0;
      RefBitHigh := NumRefBits-1;
      TimingArrayIndex := i;
    ELSE
      IF (ArcType = SubwordArc) THEN
        RefBitLow  := i / NumBitsPerSubWord;
        TimingArrayIndex := i + NumTestBits * RefBitLow;
      ELSE
        RefBitLow  := i;
        TimingArrayIndex := i;
      END IF;
      RefBitHigh := RefBitLow;
      IF (TestEvent(i)) THEN
        --TimingData.SetupEnA(i) := TRUE;
        --IR252
        TimingData.SetupEnA(i) := EnableSetupOnTest;
        TimingData.HoldEnA(i) := TimingData.HoldEnA(i) AND EnableHoldOnTest;
      END IF;
      IF (RefEdge(RefBitLow)) THEN
        --TimingData.HoldEnA(i) := TRUE;
        --IR252
        TimingData.HoldEnA(i) := EnableHoldOnRef;
        TimingData.SetupEnA(i) := TimingData.SetupEnA(i) AND EnableSetupOnRef;
      END IF;
    END IF;

    EnArrayIndex := i;
    Violation := '0';
    FOR j IN RefBitLow to RefBitHigh LOOP

      IF (CheckEnabled) THEN
        TestDly := Maximum(0 ns, TestDelayNorm(i));
        RefDly  := Maximum(0 ns, RefDelayNorm(j));

        InternalTimingCheck (
          TestSignal   => TestSignalNorm(i),
          RefSignal    => RefSignalNorm(j),
          TestDelay    => TestDly,
          RefDelay     => RefDly,
          SetupHigh    => SetupHighNorm(TimingArrayIndex),
          SetupLow     => SetupLowNorm(TimingArrayIndex),
          HoldHigh     => HoldHighNorm(TimingArrayIndex),
          HoldLow      => HoldLowNorm(TimingArrayIndex),
          RefTime      => TimingData.RefTimeA(j),
          RefEdge      => RefEdge(j),
          TestTime     => TimingData.TestTimeA(i),
          TestEvent    => TestEvent(i),
          SetupEn      => TimingData.SetupEnA(EnArrayIndex),
          HoldEn       => TimingData.HoldEnA(EnArrayIndex),
          CheckInfo    => CheckInfo,
          MsgOn        => MsgOn 
        );

        -- Report any detected violations and set return violation flag
        IF (CheckInfo.Violation) THEN
          IF (MsgOn) THEN
            VitalMemoryReportViolation (TestSignalName, RefSignalName, i, j,
                    TestSignal, RefSignal, HeaderMsg, CheckInfo, 
                    MsgFormat, MsgSeverity );
          END IF;

          IF (XOn) THEN
            Violation := 'X'; 
          END IF;
        END IF;
      END IF;

      TimingArrayIndex := TimingArrayIndex + NumRefBits;
      EnArrayIndex    := EnArrayIndex + NumRefBits;

    END LOOP;
  END LOOP;

END VitalMemorySetupHoldCheck;

-- ----------------------------------------------------------------------------
PROCEDURE  VitalMemoryPeriodPulseCheck  (
  VARIABLE Violation      : OUT   X01; 
  VARIABLE PeriodData     : INOUT VitalPeriodDataArrayType;
  SIGNAL   TestSignal     : IN    std_logic_vector;
  CONSTANT TestSignalName : IN    STRING := "";
  CONSTANT TestDelay      : IN    VitalDelayArraytype;
  CONSTANT Period         : IN    VitalDelayArraytype;
  CONSTANT PulseWidthHigh : IN    VitalDelayArraytype;
  CONSTANT PulseWidthLow  : IN    VitalDelayArraytype;
  CONSTANT CheckEnabled   : IN    BOOLEAN := TRUE;
  CONSTANT HeaderMsg      : IN    STRING := " ";
  CONSTANT XOn            : IN    BOOLEAN := TRUE;
  CONSTANT MsgOn          : IN    BOOLEAN := TRUE;
  CONSTANT MsgSeverity    : IN    SEVERITY_LEVEL := WARNING;
  CONSTANT MsgFormat      : IN    VitalMemoryMsgFormatType
) IS
  VARIABLE TestDly   : VitalDelayType;
  VARIABLE CheckInfo : CheckInfoType;
  VARIABLE PeriodObs : VitalDelayType;
  VARIABLE PulseTest : BOOLEAN;
  VARIABLE PeriodTest: BOOLEAN;
  VARIABLE TestValue : X01;
BEGIN

    -- Initialize for no violation
    Violation := '0';  --MEM IR 402

  FOR i IN TestSignal'RANGE LOOP
    TestDly := Maximum(0 ns, TestDelay(i));
    TestValue := To_X01(TestSignal(i));

    IF (PeriodData(i).NotFirstFlag = FALSE) THEN
      PeriodData(i).Rise := -Maximum(Period(i), 
          Maximum(PulseWidthHigh(i),PulseWidthLow(i)));
      PeriodData(i).Fall := -Maximum(Period(i), 
          Maximum(PulseWidthHigh(i),PulseWidthLow(i)));
      PeriodData(i).Last := TestValue;
      PeriodData(i).NotFirstFlag := TRUE;
    END IF;

    -- Initialize for no violation
  --  Violation := '0'; --Mem IR 402

    -- No violation possible if no test signal change
    NEXT WHEN (PeriodData(i).Last = TestValue);

    -- record starting pulse times
    IF (EdgeSymbolMatch(PeriodData(i).Last, TestValue, 'P')) THEN
      -- Compute period times, then record the High Rise Time
      PeriodObs := NOW - PeriodData(i).Rise;
      PeriodData(i).Rise := NOW;
      PeriodTest := TRUE;
    ELSIF (EdgeSymbolMatch(PeriodData(i).Last, TestValue, 'N')) THEN
      -- Compute period times, then record the Low Fall Time
      PeriodObs := NOW - PeriodData(i).Fall;
      PeriodData(i).Fall := NOW;
      PeriodTest := TRUE;
    ELSE
      PeriodTest := FALSE;
    END IF;

    -- do checks on pulse ends
    IF (EdgeSymbolMatch(PeriodData(i).Last, TestValue, 'p')) THEN
      -- Compute pulse times
      CheckInfo.ObsTime := NOW - PeriodData(i).Fall;
      CheckInfo.ExpTime := PulseWidthLow(i);
      PulseTest := TRUE;
    ELSIF (EdgeSymbolMatch(PeriodData(i).Last, TestValue, 'n')) THEN
      -- Compute pulse times
      CheckInfo.ObsTime := NOW - PeriodData(i).Rise;
      CheckInfo.ExpTime := PulseWidthHigh(i);
      PulseTest := TRUE;
    ELSE
      PulseTest := FALSE;
    END IF;

    IF (PulseTest AND CheckEnabled) THEN
      -- Verify Pulse Width [ignore 1st edge]
      IF (CheckInfo.ObsTime < CheckInfo.ExpTime) THEN
        IF (XOn) THEN 
         Violation := 'X';
        END IF;
        IF (MsgOn) THEN
          CheckInfo.Violation := TRUE;
          CheckInfo.CheckKind := PulseWidCheck;
          CheckInfo.DetTime   := NOW - TestDly;
          CheckInfo.State     := PeriodData(i).Last;
          VitalMemoryReportViolation (TestSignalName, "", i, 
               HeaderMsg, CheckInfo, MsgFormat, MsgSeverity );
        END IF; -- MsgOn
      END IF;
    END IF;

    IF (PeriodTest AND CheckEnabled) THEN
      -- Verify the Period [ignore 1st edge]
      CheckInfo.ObsTime := PeriodObs;
      CheckInfo.ExpTime := Period(i);
      IF ( CheckInfo.ObsTime < CheckInfo.ExpTime ) THEN
        IF (XOn) THEN 
          Violation := 'X'; 
        END IF;
        IF (MsgOn) THEN
          CheckInfo.Violation := TRUE;
          CheckInfo.CheckKind := PeriodCheck;
          CheckInfo.DetTime   := NOW - TestDly;
          CheckInfo.State     := TestValue;
          VitalMemoryReportViolation (TestSignalName, "", i,
               HeaderMsg, CheckInfo, MsgFormat, MsgSeverity );
        END IF; -- MsgOn
      END IF;
    END IF;

    PeriodData(i).Last := TestValue;
  END LOOP;

END VitalMemoryPeriodPulseCheck;

-- ----------------------------------------------------------------------------
PROCEDURE  VitalMemoryPeriodPulseCheck  (
  VARIABLE Violation      : OUT   X01ArrayT; 
  VARIABLE PeriodData     : INOUT VitalPeriodDataArrayType; 
  SIGNAL   TestSignal     : IN    std_logic_vector;
  CONSTANT TestSignalName : IN    STRING := "";
  CONSTANT TestDelay      : IN    VitalDelayArraytype;
  CONSTANT Period         : IN    VitalDelayArraytype;
  CONSTANT PulseWidthHigh : IN    VitalDelayArraytype;
  CONSTANT PulseWidthLow  : IN    VitalDelayArraytype;
  CONSTANT CheckEnabled   : IN    BOOLEAN := TRUE;
  CONSTANT HeaderMsg      : IN    STRING := " ";
  CONSTANT XOn            : IN    BOOLEAN := TRUE;
  CONSTANT MsgOn          : IN    BOOLEAN := TRUE;
  CONSTANT MsgSeverity    : IN    SEVERITY_LEVEL := WARNING;
  CONSTANT MsgFormat      : IN    VitalMemoryMsgFormatType
)IS
  VARIABLE TestDly   : VitalDelayType;
  VARIABLE CheckInfo : CheckInfoType;
  VARIABLE PeriodObs : VitalDelayType;
  VARIABLE PulseTest : BOOLEAN;
  VARIABLE PeriodTest: BOOLEAN;
  VARIABLE TestValue : X01;
BEGIN

  FOR i IN TestSignal'RANGE LOOP
    TestDly := Maximum(0 ns, TestDelay(i));
    TestValue := To_X01(TestSignal(i));

    IF (PeriodData(i).NotFirstFlag = FALSE) THEN
      PeriodData(i).Rise := -Maximum(Period(i), 
          Maximum(PulseWidthHigh(i),PulseWidthLow(i)));
      PeriodData(i).Fall := -Maximum(Period(i), 
          Maximum(PulseWidthHigh(i),PulseWidthLow(i)));
      PeriodData(i).Last := TestValue;
      PeriodData(i).NotFirstFlag := TRUE;
    END IF;

    -- Initialize for no violation
    Violation(i) := '0';

    -- No violation possible if no test signal change
    NEXT WHEN (PeriodData(i).Last = TestValue);

    -- record starting pulse times
    IF (EdgeSymbolMatch(PeriodData(i).Last, TestValue, 'P')) THEN
      -- Compute period times, then record the High Rise Time
      PeriodObs := NOW - PeriodData(i).Rise;
      PeriodData(i).Rise := NOW;
      PeriodTest := TRUE;
    ELSIF (EdgeSymbolMatch(PeriodData(i).Last, TestValue, 'N')) THEN
      -- Compute period times, then record the Low Fall Time
      PeriodObs := NOW - PeriodData(i).Fall;
      PeriodData(i).Fall := NOW;
      PeriodTest := TRUE;
    ELSE
      PeriodTest := FALSE;
    END IF;

    -- do checks on pulse ends
    IF (EdgeSymbolMatch(PeriodData(i).Last, TestValue, 'p')) THEN
      -- Compute pulse times
      CheckInfo.ObsTime := NOW - PeriodData(i).Fall;
      CheckInfo.ExpTime := PulseWidthLow(i);
      PulseTest := TRUE;
    ELSIF (EdgeSymbolMatch(PeriodData(i).Last, TestValue, 'n')) THEN
      -- Compute pulse times
      CheckInfo.ObsTime := NOW - PeriodData(i).Rise;
      CheckInfo.ExpTime := PulseWidthHigh(i);
      PulseTest := TRUE;
    ELSE
      PulseTest := FALSE;
    END IF;

    IF (PulseTest AND CheckEnabled) THEN
      -- Verify Pulse Width [ignore 1st edge]
      IF (CheckInfo.ObsTime < CheckInfo.ExpTime) THEN
        IF (XOn) THEN 
          Violation(i) := 'X';
        END IF;
        IF (MsgOn) THEN
          CheckInfo.Violation := TRUE;
          CheckInfo.CheckKind := PulseWidCheck;
          CheckInfo.DetTime   := NOW - TestDly;
          CheckInfo.State     := PeriodData(i).Last;
          VitalMemoryReportViolation (TestSignalName, "", i, 
              HeaderMsg, CheckInfo, MsgFormat, MsgSeverity );
        END IF; -- MsgOn
      END IF;
    END IF;

    IF (PeriodTest AND CheckEnabled) THEN
      -- Verify the Period [ignore 1st edge]
      CheckInfo.ObsTime := PeriodObs;
      CheckInfo.ExpTime := Period(i);
      IF ( CheckInfo.ObsTime < CheckInfo.ExpTime ) THEN
        IF (XOn) THEN 
          Violation(i) := 'X'; 
        END IF;
        IF (MsgOn) THEN
          CheckInfo.Violation := TRUE;
          CheckInfo.CheckKind := PeriodCheck;
          CheckInfo.DetTime   := NOW - TestDly;
          CheckInfo.State     := TestValue;
          VitalMemoryReportViolation (TestSignalName, "", i,
              HeaderMsg, CheckInfo, MsgFOrmat, MsgSeverity );
        END IF; -- MsgOn
      END IF;
    END IF;

    PeriodData(i).Last := TestValue;
  END LOOP;

END VitalMemoryPeriodPulseCheck;

-- ----------------------------------------------------------------------------
-- Functionality Section
-- ----------------------------------------------------------------------------

-- Look-up table.  Given an int, we can get the 4-bit bit_vector.
TYPE HexToBitvTableType IS  ARRAY (NATURAL RANGE <>) OF 
  std_logic_vector(3 DOWNTO 0) ;

CONSTANT HexToBitvTable : HexToBitvTableType (0 TO 15) :=
  (
    "0000", "0001", "0010", "0011",
    "0100", "0101", "0110", "0111",
    "1000", "1001", "1010", "1011",
    "1100", "1101", "1110", "1111"
  ) ;

-- ----------------------------------------------------------------------------
-- Misc Utilities Local Utilities
-- ----------------------------------------------------------------------------

-- ----------------------------------------------------------------------------
-- Procedure:   IsSpace
-- Parameters:  ch      -- input character
-- Description: Returns TRUE or FALSE depending on the input character
--              being white space or not.
-- ----------------------------------------------------------------------------
FUNCTION IsSpace (ch : character) 
RETURN boolean IS
BEGIN
  RETURN ((ch = ' ') OR (ch = CR) OR (ch = HT) OR (ch = NUL));
END IsSpace;

-- ----------------------------------------------------------------------------
-- Procedure:   LenOfString
-- Parameters:  Str         -- input string
-- Description: Returns the NATURAL length of the input string.
--              as terminated by the first NUL character.
-- ----------------------------------------------------------------------------
FUNCTION LenOfString (Str : STRING) 
RETURN NATURAL IS
  VARIABLE StrRight : NATURAL;
BEGIN
  StrRight := Str'RIGHT;
  FOR i IN Str'RANGE LOOP
    IF (Str(i) = NUL) THEN
      StrRight := i - 1;
      EXIT;
    END IF;
  END LOOP;
  RETURN (StrRight);
END LenOfString;

-- ----------------------------------------------------------------------------
-- Procedure:   HexToInt
-- Parameters:  Hex         -- input character or string
-- Description: Converts input character or string interpreted as a
--              hexadecimal representation to integer value.
-- ----------------------------------------------------------------------------
FUNCTION HexToInt(Hex : CHARACTER) RETURN INTEGER IS
  CONSTANT HexChars : STRING := "0123456789ABCDEFabcdef";
  CONSTANT XHiChar  : CHARACTER := 'X';
  CONSTANT XLoChar  : CHARACTER := 'x';
BEGIN
  IF (Hex = XLoChar OR Hex = XHiChar) THEN
    RETURN (23);
  END IF;
  FOR i IN 1 TO 16 LOOP
    IF(Hex = HexChars(i)) THEN
      RETURN (i-1);
    END IF;
  END LOOP;
  FOR i IN 17 TO 22 LOOP
    IF (Hex = HexChars(i)) THEN
      RETURN (i-7);
    END IF;
  END LOOP;
  ASSERT FALSE REPORT
    "Invalid character received by HexToInt function"
    SEVERITY WARNING;
  RETURN (0);
END HexToInt;

-- ----------------------------------------------------------------------------
FUNCTION HexToInt (Hex : STRING) RETURN INTEGER IS
  VARIABLE Value  : INTEGER := 0;
  VARIABLE Length : INTEGER;
BEGIN
  Length := LenOfString(hex);
  IF (Length > 8) THEN
    ASSERT FALSE REPORT
      "Invalid string length received by HexToInt function"
      SEVERITY WARNING;
  ELSE
    FOR i IN 1 TO Length LOOP
      Value := Value + HexToInt(Hex(i)) * 16 ** (Length - i);
    END LOOP;
  END IF;
  RETURN (Value);
END HexToInt;

-- ----------------------------------------------------------------------------
-- Procedure:   HexToBitv
-- Parameters:  Hex         -- Input hex string
-- Description: Converts input hex string to a std_logic_vector 
-- ----------------------------------------------------------------------------
FUNCTION HexToBitv( 
  Hex     : STRING
) RETURN std_logic_vector is
  VARIABLE   Index       : INTEGER := 0 ;
  VARIABLE   ValHexToInt : INTEGER ;
  VARIABLE   BitsPerHex  : INTEGER := 4 ; -- Denotes no. of bits per hex char.
  VARIABLE   HexLen      : NATURAL := (BitsPerHex * LenOfString(Hex)) ;
  VARIABLE   TableVal    : std_logic_vector(3 DOWNTO 0) ;  
  VARIABLE   Result      : std_logic_vector(HexLen-1 DOWNTO 0) ;
BEGIN
  --  Assign 4-bit wide bit vector to result directly from a look-up table.
  Index := 0 ;
  WHILE ( Index < HexLen )  LOOP
    ValHexToInt := HexToInt( Hex((HexLen - Index)/BitsPerHex ) );
    IF ( ValHexToInt = 23 ) THEN
      TableVal := "XXXX";
    ELSE
      -- Look up from the table.
      TableVal := HexToBitvTable( ValHexToInt ) ;
    END IF;
    -- Assign now.
    Result(Index+3 DOWNTO Index) := TableVal ;
    -- Get ready for next block of 4-bits.
    Index := Index + 4 ;
  END LOOP ;
  RETURN Result ;
END HexToBitv ;

-- ----------------------------------------------------------------------------
-- Procedure:   BinToBitv
-- Parameters:  Bin         -- Input bin string
-- Description: Converts input bin string to a std_logic_vector 
-- ----------------------------------------------------------------------------
FUNCTION BinToBitv( 
  Bin     : STRING
) RETURN std_logic_vector is
  VARIABLE   Index        : INTEGER := 0 ;
  VARIABLE   Length       : NATURAL := LenOfString(Bin);
  VARIABLE   BitVal       : std_ulogic;
  VARIABLE   Result       : std_logic_vector(Length-1 DOWNTO 0) ;  
BEGIN
  Index := 0 ;
  WHILE ( Index < Length )  LOOP
    IF (Bin(Length-Index) = '0') THEN
      BitVal := '0';
    ELSIF (Bin(Length-Index) = '1') THEN
      BitVal := '1';
    ELSE
      BitVal := 'X';
    END IF ;
    -- Assign now.
    Result(Index) := BitVal ;
    Index := Index + 1 ;
  END LOOP ;
  RETURN Result ;
END BinToBitv ;

-- ----------------------------------------------------------------------------
-- For Memory Table Modeling
-- ----------------------------------------------------------------------------

TYPE To_MemoryCharType IS ARRAY (VitalMemorySymbolType) OF CHARACTER;
CONSTANT To_MemoryChar : To_MemoryCharType :=
  ( '/', '\', 'P', 'N', 'r', 'f', 'p', 'n', 'R', 'F', '^', 'v',
    'E', 'A', 'D', '*', 'X', '0', '1', '-', 'B', 'Z', 'S',
    'g', 'u', 'i', 'G', 'U', 'I', 
    'w', 's',
    'c', 'l', 'd', 'e', 'C', 'L',
    'M', 'm', 't' );

TYPE ValidMemoryTableInputType IS ARRAY (VitalMemorySymbolType) OF BOOLEAN;
CONSTANT ValidMemoryTableInput : ValidMemoryTableInputType :=
    -- '/',   '\',   'P',   'N',   'r',   'f',
  (   TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
    -- 'p',   'n',   'R',   'F',   '^',   'v',
      TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
    -- 'E',   'A',    'D',  '*',
      TRUE,  TRUE,  TRUE,  TRUE,
    -- 'X',   '0',   '1',   '-',   'B',   'Z',
      TRUE,  TRUE,  TRUE,  TRUE,  TRUE, FALSE,
    -- 'S',
      TRUE, 
    -- 'g',   'u',   'i',   'G',   'U',   'I', 
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    -- 'w',   's',
      FALSE, FALSE, 
    -- 'c',   'l',   'd',   'e',   'C',   'L',  
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
  -- 'M',   'm',   't' 
      FALSE, FALSE, FALSE);

TYPE MemoryTableMatchType IS ARRAY (X01,X01,VitalMemorySymbolType) OF BOOLEAN;
-- last value, present value, table symbol
CONSTANT MemoryTableMatch : MemoryTableMatchType :=  (
  ( -- X (lastvalue)
    -- /     \     P     N     r     f
    -- p     n     R     F     ^     v
    -- E     A     D     *
    -- X     0     1     -     B     Z     S
    -- g     u     i     G     U     I  
    -- w     s
    -- c     l     d     e,    C     L
    -- m     t 
    ( FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,FALSE,FALSE,
      TRUE, FALSE,FALSE,TRUE, FALSE,FALSE,FALSE,
      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,
      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,FALSE),
    ( FALSE,FALSE,FALSE,TRUE, FALSE,FALSE,
      FALSE,FALSE,FALSE,TRUE, FALSE,TRUE,
      TRUE, FALSE,TRUE, TRUE,
      FALSE,TRUE, FALSE,TRUE, TRUE, FALSE,FALSE,
      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,
      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,FALSE),
    ( FALSE,FALSE,TRUE, FALSE,FALSE,FALSE,
      FALSE,FALSE,TRUE, FALSE,TRUE, FALSE,
      TRUE, TRUE, FALSE,TRUE,
      FALSE,FALSE,TRUE, TRUE, TRUE, FALSE,FALSE,
      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,
      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,FALSE)
    ),

    (-- 0 (lastvalue)
    -- /     \     P     N     r     f
    -- p     n     R     F     ^     v
    -- E     A     D     *
    -- X     0     1     -     B     Z     S
    -- g     u     i     G     U     I  
    -- w     s
    -- c     l     d     e,    C     L 
    -- m     t 
    ( FALSE,FALSE,FALSE,FALSE,TRUE, FALSE,
      TRUE, FALSE,TRUE, FALSE,FALSE,FALSE,
      FALSE,TRUE, FALSE,TRUE,
      TRUE, FALSE,FALSE,TRUE, FALSE,FALSE,FALSE,
      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,
      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,FALSE),
    ( FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,FALSE,FALSE,
      FALSE,TRUE, FALSE,TRUE, TRUE, FALSE,TRUE,
      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,
      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,FALSE),
    ( TRUE, FALSE,TRUE, FALSE,FALSE,FALSE,
      TRUE, FALSE,TRUE, FALSE,FALSE,FALSE,
      FALSE,FALSE,FALSE,TRUE,
      FALSE,FALSE,TRUE, TRUE, TRUE, FALSE,FALSE,
      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,
      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,FALSE)
    ),

    (-- 1 (lastvalue)
    -- /     \     P     N     r     f
    -- p     n     R     F     ^     v
    -- E     A     D     *
    -- X     0     1     -     B     Z     S
    -- g     u     i     G     U     I  
    -- w     s
    -- c     l     d     e,    C     L
    -- m     t 
    ( FALSE,FALSE,FALSE,FALSE,FALSE,TRUE ,
      FALSE,TRUE, FALSE,TRUE, FALSE,FALSE,
      FALSE,FALSE,TRUE, TRUE,
      TRUE, FALSE,FALSE,TRUE, FALSE,FALSE,FALSE,
      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,
      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,FALSE),
    ( FALSE,TRUE, FALSE,TRUE, FALSE,FALSE,
      FALSE,TRUE, FALSE,TRUE, FALSE,FALSE,
      FALSE,FALSE,FALSE,TRUE,
      FALSE,TRUE, FALSE,TRUE, TRUE, FALSE,FALSE,
      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,
      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,FALSE),
    ( FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,TRUE, TRUE, TRUE, FALSE,TRUE,
      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,
      FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
      FALSE,FALSE,FALSE)
    )
  );


-- ----------------------------------------------------------------------------
-- Error Message Types and Tables
-- ----------------------------------------------------------------------------

TYPE VitalMemoryErrorType IS (
  ErrGoodAddr,   -- 'g' Good address (no transition)
  ErrUnknAddr,   -- 'u' 'X' levels in address (no transition)
  ErrInvaAddr,   -- 'i' Invalid address (no transition)
  ErrGoodTrAddr, -- 'G' Good address (with transition)
  ErrUnknTrAddr, -- 'U' 'X' levels in address (with transition)
  ErrInvaTrAddr, -- 'I' Invalid address (with transition)
  ErrWrDatMem,   -- 'w' Writing data to memory
  ErrNoChgMem,   -- 's' Retaining previous memory contents
  ErrCrAllMem,   -- 'c' Corrupting entire memory with 'X'
  ErrCrWrdMem,   -- 'l' Corrupting a word in memory with 'X'
  ErrCrBitMem,   -- 'd' Corrupting a single bit in memory with 'X'
  ErrCrDatMem,   -- 'e' Corrupting a word with 'X' based on data in
  ErrCrAllSubMem,-- 'C' Corrupting a sub-word entire memory with 'X'
  ErrCrWrdSubMem,-- 'L' Corrupting a sub-word in memory with 'X'
  ErrCrBitSubMem,-- 'D' Corrupting a single bit of a memory sub-word with 'X'
  ErrCrDatSubMem,-- 'E' Corrupting a sub-word with 'X' based on data in
  ErrCrWrdOut,   -- 'l' Corrupting data out with 'X'
  ErrCrBitOut,   -- 'd' Corrupting a single bit of data out with 'X'
  ErrCrDatOut,   -- 'e' Corrupting data out with 'X' based on data in
  ErrCrWrdSubOut,-- 'L' Corrupting data out sub-word with 'X'
  ErrCrBitSubOut,-- 'D' Corrupting a single bit of data out sub-word with 'X'
  ErrCrDatSubOut,-- 'E' Corrupting data out sub-word with 'X' based on data in
  ErrImplOut,    -- 'M' Implicit read from memory to data out
  ErrReadOut,    -- 'm' Reading data from memory to data out
  ErrAssgOut,    -- 't' Transferring from data in to data out
  ErrAsgXOut,    -- 'X' Assigning unknown level to data out
  ErrAsg0Out,    -- '0' Assigning low level to data out
  ErrAsg1Out,    -- '1' Assigning high level to data out
  ErrAsgZOut,    -- 'Z' Assigning high impedence to data out
  ErrAsgSOut,    -- 'S' Keeping data out at steady value
  ErrAsgXMem,    -- 'X' Assigning unknown level to memory location
  ErrAsg0Mem,    -- '0' Assigning low level to memory location
  ErrAsg1Mem,    -- '1' Assigning high level to memory location
  ErrAsgZMem,    -- 'Z' Assigning high impedence to memory location
  ErrDefMemAct,  --     No memory table match, using default action
  ErrInitMem,    --     Initialize memory contents
  ErrMcpWrCont,  --     Memory cross port to same port write contention
  ErrMcpCpCont,  --     Memory cross port read/write data/memory contention
  ErrMcpCpRead,  --     Memory cross port read to same port
  ErrMcpRdWrCo,  --     Memory cross port read/write data only contention
  ErrMcpCpWrCont,--     Memory cross port to cross port write contention
  ErrUnknMemDo,  --     Unknown memory action
  ErrUnknDatDo,  --     Unknown data action
  ErrUnknSymbol, --     Illegal memory symbol
  ErrLdIlgArg,
  ErrLdAddrRng,
  ErrLdMemInfo,
  ErrLdFileEmpty,
  ErrPrintString
);

TYPE VitalMemoryErrorSeverityType IS
ARRAY (VitalMemoryErrorType) OF SEVERITY_LEVEL;
CONSTANT VitalMemoryErrorSeverity : 
  VitalMemoryErrorSeverityType := (
    ErrGoodAddr     => NOTE,
    ErrUnknAddr     => WARNING,
    ErrInvaAddr     => WARNING,
    ErrGoodTrAddr   => NOTE,
    ErrUnknTrAddr   => WARNING,
    ErrInvaTrAddr   => WARNING,
    ErrWrDatMem     => NOTE,
    ErrNoChgMem     => NOTE,
    ErrCrAllMem     => WARNING,
    ErrCrWrdMem     => WARNING,
    ErrCrBitMem     => WARNING,
    ErrCrDatMem     => WARNING,
    ErrCrAllSubMem  => WARNING,
    ErrCrWrdSubMem  => WARNING,
    ErrCrBitSubMem  => WARNING,
    ErrCrDatSubMem  => WARNING,
    ErrCrWrdOut     => WARNING,
    ErrCrBitOut     => WARNING,
    ErrCrDatOut     => WARNING,
    ErrCrWrdSubOut  => WARNING,
    ErrCrBitSubOut  => WARNING,
    ErrCrDatSubOut  => WARNING,
    ErrImplOut      => NOTE,
    ErrReadOut      => NOTE,
    ErrAssgOut      => NOTE,
    ErrAsgXOut      => NOTE,
    ErrAsg0Out      => NOTE,
    ErrAsg1Out      => NOTE,
    ErrAsgZOut      => NOTE,
    ErrAsgSOut      => NOTE,
    ErrAsgXMem      => NOTE,
    ErrAsg0Mem      => NOTE,
    ErrAsg1Mem      => NOTE,
    ErrAsgZMem      => NOTE,
    ErrDefMemAct    => NOTE,
    ErrInitMem      => NOTE,
    ErrMcpWrCont    => WARNING,
    ErrMcpCpCont    => WARNING,
    ErrMcpCpRead    => WARNING,
    ErrMcpRdWrCo    => WARNING,
    ErrMcpCpWrCont  => WARNING,
    ErrUnknMemDo    => ERROR,
    ErrUnknDatDo    => ERROR,
    ErrUnknSymbol   => ERROR,
    ErrLdIlgArg     => ERROR,
    ErrLdAddrRng    => WARNING,
    ErrLdMemInfo    => NOTE,
    ErrLdFileEmpty  => ERROR,
    ErrPrintString  => WARNING
  );

-- ----------------------------------------------------------------------------
CONSTANT MsgGoodAddr    : STRING
          := "Good address (no transition)";
CONSTANT MsgUnknAddr    : STRING
          := "Unknown address (no transition)";
CONSTANT MsgInvaAddr    : STRING
          := "Invalid address (no transition)";
CONSTANT MsgGoodTrAddr  : STRING
          := "Good address (with transition)"; 
CONSTANT MsgUnknTrAddr  : STRING
          := "Unknown address (with transition)"; 
CONSTANT MsgInvaTrAddr  : STRING
          := "Invalid address (with transition)"; 
CONSTANT MsgNoChgMem    : STRING
          := "Retaining previous memory contents";
CONSTANT MsgWrDatMem    : STRING
          := "Writing data to memory";
CONSTANT MsgCrAllMem    : STRING
          := "Corrupting entire memory with 'X'";
CONSTANT MsgCrWrdMem    : STRING
          := "Corrupting a word in memory with 'X'";
CONSTANT MsgCrBitMem    : STRING
          := "Corrupting a single bit in memory with 'X'";
CONSTANT MsgCrDatMem    : STRING
          := "Corrupting a word with 'X' based on data in";
CONSTANT MsgCrAllSubMem : STRING
          := "Corrupting a sub-word entire memory with 'X'";
CONSTANT MsgCrWrdSubMem : STRING
          := "Corrupting a sub-word in memory with 'X'";
CONSTANT MsgCrBitSubMem : STRING
          := "Corrupting a single bit of a sub-word with 'X'";
CONSTANT MsgCrDatSubMem : STRING
          := "Corrupting a sub-word with 'X' based on data in";
CONSTANT MsgCrWrdOut    : STRING
          := "Corrupting data out with 'X'";
CONSTANT MsgCrBitOut    : STRING
          := "Corrupting a single bit of data out with 'X'";
CONSTANT MsgCrDatOut    : STRING
          := "Corrupting data out with 'X' based on data in";
CONSTANT MsgCrWrdSubOut : STRING
          := "Corrupting data out sub-word with 'X'";
CONSTANT MsgCrBitSubOut : STRING
          := "Corrupting a single bit of data out sub-word with 'X'";
CONSTANT MsgCrDatSubOut : STRING
          := "Corrupting data out sub-word with 'X' based on data in";
CONSTANT MsgImplOut     : STRING
          := "Implicit read from memory to data out";
CONSTANT MsgReadOut     : STRING
          := "Reading data from memory to data out";
CONSTANT MsgAssgOut     : STRING
          := "Transferring from data in to data out";
CONSTANT MsgAsgXOut     : STRING
          := "Assigning unknown level to data out";
CONSTANT MsgAsg0Out     : STRING
          := "Assigning low level to data out";
CONSTANT MsgAsg1Out     : STRING
          := "Assigning high level to data out";
CONSTANT MsgAsgZOut     : STRING
          := "Assigning high impedance to data out";
CONSTANT MsgAsgSOut     : STRING
          := "Keeping data out at steady value";
CONSTANT MsgAsgXMem     : STRING
          := "Assigning unknown level to memory location";
CONSTANT MsgAsg0Mem     : STRING
          := "Assigning low level to memory location";
CONSTANT MsgAsg1Mem     : STRING
          := "Assigning high level to memory location";
CONSTANT MsgAsgZMem     : STRING
          := "Assigning high impedance to memory location";
CONSTANT MsgDefMemAct   : STRING
          := "No memory table match, using default action";
CONSTANT MsgInitMem     : STRING
          := "Initializing memory contents";
CONSTANT MsgMcpWrCont   : STRING
          := "Same port write contention";
CONSTANT MsgMcpCpCont   : STRING
          := "Cross port read/write data/memory contention";
CONSTANT MsgMcpCpRead   : STRING
          := "Cross port read to same port";
CONSTANT MsgMcpRdWrCo   : STRING
          := "Cross port read/write data only contention";
CONSTANT MsgMcpCpWrCont : STRING
          := "Cross port write contention";
CONSTANT MsgUnknMemDo   : STRING
          := "Unknown memory action";
CONSTANT MsgUnknDatDo   : STRING
          := "Unknown data action";
CONSTANT MsgUnknSymbol  : STRING
          := "Illegal memory symbol";

CONSTANT MsgLdIlgArg    : STRING
          := "Illegal bit arguments while loading memory.";
CONSTANT MsgLdMemInfo   : STRING
          := "Loading data from the file into memory.";
CONSTANT MsgLdAddrRng   : STRING
          := "Address out of range while loading memory.";
CONSTANT MsgLdFileEmpty : STRING
          := "Memory load file is empty.";
CONSTANT MsgPrintString : STRING
          := "";

CONSTANT MsgUnknown     : STRING
          := "Unknown error message.";

CONSTANT MsgVMT         : STRING
          := "VitalMemoryTable";
CONSTANT MsgVMV         : STRING
          := "VitalMemoryViolation";
CONSTANT MsgVDM         : STRING
          := "VitalDeclareMemory";
CONSTANT MsgVMCP        : STRING
          := "VitalMemoryCrossPorts";

-- ----------------------------------------------------------------------------
-- LOCAL Utilities
-- ----------------------------------------------------------------------------

-- ----------------------------------------------------------------------------
-- Procedure:   MemoryMessage
-- Parameters:  ErrorId     -- Input error code
-- Description: This function looks up the input error code and returns
--              the string value of the associated message.
-- ----------------------------------------------------------------------------

FUNCTION MemoryMessage (
  CONSTANT ErrorId : IN VitalMemoryErrorType
) RETURN STRING IS
BEGIN
  CASE ErrorId IS
    WHEN ErrGoodAddr      => RETURN MsgGoodAddr   ;
    WHEN ErrUnknAddr      => RETURN MsgUnknAddr   ;
    WHEN ErrInvaAddr      => RETURN MsgInvaAddr   ;
    WHEN ErrGoodTrAddr    => RETURN MsgGoodTrAddr ;
    WHEN ErrUnknTrAddr    => RETURN MsgUnknTrAddr ;
    WHEN ErrInvaTrAddr    => RETURN MsgInvaTrAddr ;
    WHEN ErrWrDatMem      => RETURN MsgWrDatMem   ;
    WHEN ErrNoChgMem      => RETURN MsgNoChgMem   ;
    WHEN ErrCrAllMem      => RETURN MsgCrAllMem   ;
    WHEN ErrCrWrdMem      => RETURN MsgCrWrdMem   ;
    WHEN ErrCrBitMem      => RETURN MsgCrBitMem   ;
    WHEN ErrCrDatMem      => RETURN MsgCrDatMem   ;
    WHEN ErrCrAllSubMem   => RETURN MsgCrAllSubMem;
    WHEN ErrCrWrdSubMem   => RETURN MsgCrWrdSubMem;
    WHEN ErrCrBitSubMem   => RETURN MsgCrBitSubMem;
    WHEN ErrCrDatSubMem   => RETURN MsgCrDatSubMem;
    WHEN ErrCrWrdOut      => RETURN MsgCrWrdOut   ;
    WHEN ErrCrBitOut      => RETURN MsgCrBitOut   ;
    WHEN ErrCrDatOut      => RETURN MsgCrDatOut   ;
    WHEN ErrCrWrdSubOut   => RETURN MsgCrWrdSubOut;
    WHEN ErrCrBitSubOut   => RETURN MsgCrBitSubOut;
    WHEN ErrCrDatSubOut   => RETURN MsgCrDatSubOut;
    WHEN ErrImplOut       => RETURN MsgImplOut    ;
    WHEN ErrReadOut       => RETURN MsgReadOut    ;
    WHEN ErrAssgOut       => RETURN MsgAssgOut    ;
    WHEN ErrAsgXOut       => RETURN MsgAsgXOut    ;
    WHEN ErrAsg0Out       => RETURN MsgAsg0Out    ;
    WHEN ErrAsg1Out       => RETURN MsgAsg1Out    ;
    WHEN ErrAsgZOut       => RETURN MsgAsgZOut    ;
    WHEN ErrAsgSOut       => RETURN MsgAsgSOut    ;
    WHEN ErrAsgXMem       => RETURN MsgAsgXMem    ;
    WHEN ErrAsg0Mem       => RETURN MsgAsg0Mem    ;
    WHEN ErrAsg1Mem       => RETURN MsgAsg1Mem    ;
    WHEN ErrAsgZMem       => RETURN MsgAsgZMem    ;
    WHEN ErrDefMemAct     => RETURN MsgDefMemAct  ;
    WHEN ErrInitMem       => RETURN MsgInitMem    ;
    WHEN ErrMcpWrCont     => RETURN MsgMcpWrCont  ;
    WHEN ErrMcpCpCont     => RETURN MsgMcpCpCont  ;
    WHEN ErrMcpCpRead     => RETURN MsgMcpCpRead  ;
    WHEN ErrMcpRdWrCo     => RETURN MsgMcpRdWrCo  ;
    WHEN ErrMcpCpWrCont   => RETURN MsgMcpCpWrCont;
    WHEN ErrUnknMemDo     => RETURN MsgUnknMemDo  ;
    WHEN ErrUnknDatDo     => RETURN MsgUnknDatDo  ;
    WHEN ErrUnknSymbol    => RETURN MsgUnknSymbol ;
    WHEN ErrLdIlgArg      => RETURN MsgLdIlgArg   ;
    WHEN ErrLdAddrRng     => RETURN MsgLdAddrRng  ;
    WHEN ErrLdMemInfo     => RETURN MsgLdMemInfo  ;
    WHEN ErrLdFileEmpty   => RETURN MsgLdFileEmpty;
    WHEN ErrPrintString   => RETURN MsgPrintString;
    WHEN OTHERS           => RETURN MsgUnknown    ;
  END CASE;
END;

-- ----------------------------------------------------------------------------
-- Procedure:   PrintMemoryMessage
-- Parameters:  Routine     -- String identifying the calling routine
--                  ErrorId     -- Input error code for message lookup
--                  Info        -- Output string or character
--                  InfoStr     -- Additional output string
--                  Info1       -- Additional output integer
--                  Info2       -- Additional output integer
--                  Info3       -- Additional output integer
-- Description: This procedure prints out a memory status message
--              given the input error id and other status information.
-- ----------------------------------------------------------------------------
PROCEDURE PrintMemoryMessage (
  CONSTANT Routine : IN STRING;
  CONSTANT ErrorId : IN VitalMemoryErrorType
) IS
BEGIN
  ASSERT FALSE
    REPORT Routine & ": " & MemoryMessage(ErrorId)
    SEVERITY VitalMemoryErrorSeverity(ErrorId);
END;

-- ----------------------------------------------------------------------------
PROCEDURE PrintMemoryMessage (
  CONSTANT Routine : IN STRING;
  CONSTANT ErrorId : IN VitalMemoryErrorType;
  CONSTANT Info    : IN STRING
) IS
BEGIN
  ASSERT FALSE
    REPORT Routine & ": " & MemoryMessage(ErrorId) & " " & Info
    SEVERITY VitalMemoryErrorSeverity(ErrorId);
END;

-- ----------------------------------------------------------------------------
PROCEDURE PrintMemoryMessage (
  CONSTANT Routine : IN STRING;
  CONSTANT ErrorId : IN VitalMemoryErrorType;
  CONSTANT Info1   : IN STRING;
  CONSTANT Info2   : IN STRING
) IS
BEGIN
  ASSERT FALSE
    REPORT Routine & ": " & MemoryMessage(ErrorId) & " " & Info1 & " " & Info2
    SEVERITY VitalMemoryErrorSeverity(ErrorId);
END;

-- ----------------------------------------------------------------------------
PROCEDURE PrintMemoryMessage (
  CONSTANT Routine : IN STRING;
  CONSTANT ErrorId : IN VitalMemoryErrorType;
  CONSTANT Info    : IN CHARACTER
) IS
BEGIN
  ASSERT FALSE
    REPORT Routine & ": " & MemoryMessage(ErrorId) & " " & Info
    SEVERITY VitalMemoryErrorSeverity(ErrorId);
END;

-- ----------------------------------------------------------------------------
PROCEDURE PrintMemoryMessage (
  CONSTANT Routine    : IN STRING;
  CONSTANT ErrorId    : IN VitalMemoryErrorType;
  CONSTANT InfoStr    : IN STRING;
  CONSTANT Info1      : IN NATURAL
) IS
  VARIABLE TmpStr         : STRING ( 1 TO 256 ) ;
  VARIABLE TmpInt         : INTEGER := 1;
BEGIN
  IntToStr(Info1,TmpStr,TmpInt);
  ASSERT FALSE
    REPORT Routine & ": " & MemoryMessage(ErrorId) & " " & InfoStr & " " & TmpStr
    SEVERITY VitalMemoryErrorSeverity(ErrorId);
END;

-- ----------------------------------------------------------------------------
PROCEDURE PrintMemoryMessage (
  CONSTANT Routine    : IN STRING;
  CONSTANT ErrorId    : IN VitalMemoryErrorType;
  CONSTANT InfoStr    : IN STRING;
  CONSTANT Info1      : IN NATURAL;
  CONSTANT Info2      : IN NATURAL
) IS
  VARIABLE TmpStr         : STRING ( 1 TO 256 ) ;
  VARIABLE TmpInt         : INTEGER := 1;
BEGIN
  IntToStr(Info1,TmpStr,TmpInt);
  IntToStr(Info2,TmpStr,TmpInt);
  ASSERT FALSE
    REPORT Routine & ": " & MemoryMessage(ErrorId) & " " & InfoStr & " " & TmpStr
    SEVERITY VitalMemoryErrorSeverity(ErrorId);
END;

-- ----------------------------------------------------------------------------
PROCEDURE PrintMemoryMessage (
  CONSTANT Routine    : IN STRING;
  CONSTANT ErrorId    : IN VitalMemoryErrorType;
  CONSTANT InfoStr    : IN STRING;
  CONSTANT Info1      : IN NATURAL;
  CONSTANT Info2      : IN NATURAL;
  CONSTANT Info3      : IN NATURAL
) IS
  VARIABLE TmpStr         : STRING ( 1 TO 256 ) ;
  VARIABLE TmpInt         : INTEGER := 1;
BEGIN
  IntToStr(Info1,TmpStr,TmpInt);
  IntToStr(Info2,TmpStr,TmpInt);
  IntToStr(Info3,TmpStr,TmpInt);
  ASSERT FALSE
    REPORT Routine & ": " & MemoryMessage(ErrorId) & " " & InfoStr & " " & TmpStr
    SEVERITY VitalMemoryErrorSeverity(ErrorId);
END;

-- ----------------------------------------------------------------------------
PROCEDURE PrintMemoryMessage (
  CONSTANT Routine    : IN STRING;
  CONSTANT Table      : IN VitalMemoryTableType;
  CONSTANT Index      : IN INTEGER;
  CONSTANT InfoStr    : IN STRING
) IS
  CONSTANT TableEntries   : INTEGER := Table'LENGTH(1);
  CONSTANT TableWidth     : INTEGER := Table'LENGTH(2);
  VARIABLE TmpStr         : STRING ( 1 TO 256 ) ;
  VARIABLE TmpInt         : INTEGER := 1;
BEGIN
  IF (Index < 0 AND Index > TableEntries-1) THEN 
    ASSERT FALSE
      REPORT Routine & ": Memory table search failure"
      SEVERITY ERROR;
  END IF;
  ColLoop:
  FOR i IN 0 TO TableWidth-1 LOOP
    IF (i >= 64) THEN
      TmpStr(TmpInt) := '.';
      TmpInt := TmpInt + 1;
      TmpStr(TmpInt) := '.';
      TmpInt := TmpInt + 1;
      TmpStr(TmpInt) := '.';
      TmpInt := TmpInt + 1;
      EXIT ColLoop;
    END IF;
    TmpStr(TmpInt) := ''';
    TmpInt := TmpInt + 1;
    TmpStr(TmpInt) := To_MemoryChar(Table(Index,i));
    TmpInt := TmpInt + 1;
    TmpStr(TmpInt) := ''';
    TmpInt := TmpInt + 1;
    IF (i < TableWidth-1) THEN
      TmpStr(TmpInt) := ',';
      TmpInt := TmpInt + 1;
    END IF;
  END LOOP;
  ASSERT FALSE
    REPORT Routine & ": Port=" & InfoStr & " TableRow=" & TmpStr
    SEVERITY NOTE;
END;

-- ----------------------------------------------------------------------------
-- Procedure:   DecodeAddress
-- Parameters:  Address        - Converted address.
--              AddrFlag       - Flag to indicte address match
--              MemoryData     - Information about memory characteristics
--              PrevAddressBus - Previous input address value
--              AddressBus     - Input address value.
-- Description: This procedure is used for transforming a valid
--              address value to an integer in order to access memory.
--              It performs address bound checking as well.
--              Sets Address to -1 for unknowns
--              Sets Address to -2 for out of range
-- ----------------------------------------------------------------------------

PROCEDURE DecodeAddress (
  VARIABLE Address        : INOUT INTEGER;
  VARIABLE AddrFlag       : INOUT VitalMemorySymbolType;
  VARIABLE MemoryData     : IN VitalMemoryDataType;
  CONSTANT PrevAddressBus : IN std_logic_vector;
  CONSTANT AddressBus     : IN std_logic_vector
) IS
  VARIABLE Power      : NATURAL;
  VARIABLE AddrUnkn   : BOOLEAN;
BEGIN
  Power := 0;
  AddrUnkn := FALSE;
  -- It is assumed that always Address'LEFT represents the Most significant bit.   
  FOR i IN AddressBus'RANGE LOOP
    Power := Power * 2;
    IF (AddressBus(i) /= '1' AND AddressBus(i) /= '0') THEN
      AddrUnkn := TRUE;
      Power := 0;
      EXIT;
    ELSIF (AddressBus(i) = '1') THEN
      Power := Power + 1;
    END IF;
  END LOOP;
  Address := Power;
  AddrFlag := 'g';
  IF (AddrUnkn) THEN
    AddrFlag := 'u';    -- unknown addr
    Address := -1;
  END IF;
  IF ( Power > (MemoryData.NoOfWords - 1)) THEN
    AddrFlag := 'i';    -- invalid addr
    Address := -2;
  END IF;
  IF (PrevAddressBus /= AddressBus) THEN
    CASE AddrFlag IS
      WHEN 'g'    => AddrFlag := 'G';
      WHEN 'u'    => AddrFlag := 'U';
      WHEN 'i'    => AddrFlag := 'I';
      WHEN OTHERS => 
        ASSERT FALSE REPORT
          "DecodeAddress: Internal error. [AddrFlag]=" 
          & To_MemoryChar(AddrFlag)
          SEVERITY ERROR;
    END CASE;
  END IF;
END DecodeAddress;

-- ----------------------------------------------------------------------------
-- Procedure:   DecodeData
-- Parameters:  DataFlag      - Flag to indicte data match
--              PrevDataInBus - Previous input data value
--              DataInBus     - Input data value.
--              HighBit       - High bit offset value.
--              LowBit        - Low  bit offset value.
-- Description: This procedure is used for interpreting the input data
--              as a data flag for subsequent table matching.
-- ----------------------------------------------------------------------------
PROCEDURE DecodeData (
  VARIABLE DataFlag       : INOUT VitalMemorySymbolType;
  CONSTANT PrevDataInBus  : IN std_logic_vector;
  CONSTANT DataInBus      : IN std_logic_vector;
  CONSTANT HighBit        : IN    NATURAL;
  CONSTANT LowBit         : IN    NATURAL
) IS
  VARIABLE DataUnkn   : BOOLEAN := FALSE;
BEGIN
  FOR i IN LowBit TO HighBit LOOP
    IF DataInBus(i) /= '1' AND DataInBus(i) /= '0' THEN
      DataUnkn := TRUE;
      EXIT;
    END IF;
  END LOOP;
  DataFlag := 'g';
  IF (DataUnkn) THEN
    DataFlag := 'u';    -- unknown addr
  END IF;
  IF (PrevDataInBus(HighBit DOWNTO LowBit) /= 
      DataInBus(HighBit DOWNTO LowBit)) THEN
    CASE DataFlag IS
      WHEN 'g'    => DataFlag := 'G';
      WHEN 'u'    => DataFlag := 'U';
      WHEN OTHERS => 
        ASSERT FALSE REPORT
          "DecodeData: Internal error. [DataFlag]=" 
          & To_MemoryChar(DataFlag)
          SEVERITY ERROR;
    END CASE;
  END IF;
END DecodeData;

-- ----------------------------------------------------------------------------
-- Procedure:   WriteMemory
-- Parameters:  MemoryPtr   - Pointer to the memory array.
--              DataInBus   - Input Data to be written.
--              Address     - Address of the memory location.
--              BitPosition - Position of bit in memory location.
--              HighBit     - High bit offset value.
--              LowBit      - Low  bit offset value.
-- Description: This procedure is used to write to a memory location
--              on a bit/byte/word basis. 
--              The high bit and low bit offset are used for byte write
--              operations.These parameters specify the data byte for write.
--              In the case of word write the complete memory word is used.
--              This procedure is overloaded for bit,byte and word write 
--              memory operations.The number of parameters may vary.
-- ----------------------------------------------------------------------------
PROCEDURE WriteMemory (
  VARIABLE MemoryPtr   : INOUT VitalMemoryDataType;
  CONSTANT DataInBus   : IN    std_logic_vector;
  CONSTANT Address     : IN    INTEGER;
  CONSTANT HighBit     : IN    NATURAL;
  CONSTANT LowBit      : IN    NATURAL
) IS
  VARIABLE TmpData : std_logic_vector(DataInBus'LENGTH - 1 DOWNTO 0);
BEGIN
  -- Address bound checking.
  IF ( Address < 0 OR Address > (MemoryPtr.NoOfWords - 1)) THEN
    PrintMemoryMessage ( "WriteMemory", ErrPrintString, 
      "Aborting write operation as address is out of range.") ;
    RETURN;
  END IF;
  TmpData  := To_UX01(DataInBus);
  FOR i in LowBit to HighBit LOOP
    MemoryPtr.MemoryArrayPtr(Address).all(i) := TmpData(i);
  END LOOP;
END WriteMemory;

-- ----------------------------------------------------------------------------
PROCEDURE WriteMemory (
  VARIABLE MemoryPtr      : INOUT VitalMemoryDataType;
  CONSTANT DataInBus      : IN    std_logic_vector;
  CONSTANT Address        : IN    INTEGER;
  CONSTANT BitPosition    : IN    NATURAL
) IS
  VARIABLE HighBit      : NATURAL;
  VARIABLE LowBit       : NATURAL;
BEGIN
  HighBit      := BitPosition;
  LowBit       := BitPosition;
  WriteMemory (MemoryPtr, DataInBus, Address, HighBit, LowBit);
END WriteMemory;

-- ----------------------------------------------------------------------------
PROCEDURE WriteMemory   (
  VARIABLE MemoryPtr   : INOUT VitalMemoryDataType;
  CONSTANT DataInBus   : IN    std_logic_vector;
  CONSTANT Address     : IN    INTEGER
) IS
  VARIABLE HighBit      : NATURAL;
  VARIABLE LowBit       : NATURAL;
BEGIN
  HighBit      := MemoryPtr.NoOfBitsPerWord - 1;
  LowBit       := 0;
  WriteMemory (MemoryPtr, DataInBus, Address, HighBit, LowBit);
END WriteMemory;

-- ----------------------------------------------------------------------------
-- Procedure:   ReadMemory
-- Parameters:  MemoryPtr    - Pointer to the memory array.
--              DataOut      - Output Data to be read in this.
--              Address      - Address of the memory location.
--              BitPosition  - Position of bit in memory location.
--              HighBit      - High bit offset value.
--              LowBit       - Low  bit offset value.
-- Description: This procedure is used to read from a memory location
--              on a bit/byte/word basis. 
--              The high bit and low bit offset are used for byte write
--              operations.These parameters specify the data byte for 
--              read.In the case of word write the complete memory word 
--              is used.This procedure is overloaded for bit,byte and 
--              word write memory operations.The number of parameters 
--              may vary.
-- ----------------------------------------------------------------------------
PROCEDURE ReadMemory (
  VARIABLE MemoryPtr  : INOUT VitalMemoryDataType;
  VARIABLE DataOut    : OUT   std_logic_vector;
  CONSTANT Address    : IN    INTEGER;
  CONSTANT HighBit    : IN    NATURAL;
  CONSTANT LowBit     : IN    NATURAL
) IS
  VARIABLE DataOutTmp : std_logic_vector(MemoryPtr.NoOfBitsPerWord-1 DOWNTO 0);
  VARIABLE length     : NATURAL := (HighBit - LowBit + 1);
BEGIN
  -- Address bound checking.
  IF ( Address > (MemoryPtr.NoOfWords - 1)) THEN
    PrintMemoryMessage ( 
      "ReadMemory",ErrInvaAddr,
      "[Address,NoOfWords]=",Address,MemoryPtr.NoOfWords 
    );
    FOR i in LowBit to HighBit LOOP
      DataOutTmp(i) := 'X';
    END LOOP;
  ELSE
    FOR i in LowBit to HighBit LOOP
      DataOutTmp(i) := MemoryPtr.MemoryArrayPtr (Address).all(i);
    END LOOP;
  END IF;
  DataOut :=  DataOutTmp;
END ReadMemory;

-- ----------------------------------------------------------------------------
PROCEDURE ReadMemory (
  VARIABLE MemoryPtr   : INOUT VitalMemoryDataType;
  VARIABLE DataOut     : OUT   std_logic_vector;
  CONSTANT Address     : IN    INTEGER;
  CONSTANT BitPosition : IN    NATURAL
) IS
  VARIABLE HighBit    : NATURAL;
  VARIABLE LowBit     : NATURAL;
BEGIN
  HighBit      := BitPosition;
  LowBit       := BitPosition;
  ReadMemory (MemoryPtr, DataOut, Address, HighBit, LowBit);
END ReadMemory;

-- ----------------------------------------------------------------------------
PROCEDURE ReadMemory (
  VARIABLE MemoryPtr  : INOUT VitalMemoryDataType;
  VARIABLE DataOut    : OUT   std_logic_vector;
  CONSTANT Address    : IN    INTEGER
) IS
  VARIABLE HighBit    : NATURAL;
  VARIABLE LowBit     : NATURAL;
BEGIN
  HighBit      := MemoryPtr.NoOfBitsPerWord - 1;
  LowBit       := 0;
  ReadMemory (MemoryPtr, DataOut, Address, HighBit, LowBit);
END ReadMemory;


-- ----------------------------------------------------------------------------
-- Procedure:   LoadMemory
-- Parameters:  MemoryPtr   - Pointer to the memory array.
--              FileName    - Name of the output file.
--              HighBit     - High bit offset value.
--              LowBit      - Low  bit offset value.
-- Description: This procedure is used to load the contents of the memory
--              from a specified input file.
--              The high bit and low bit offset are used so that same task
--              can be used for all bit/byte/word write operations.
--              In the case of a bit write RAM the HighBit and LowBit have
--              the same value.
--              This procedure is overloaded for word write operations.
-- ----------------------------------------------------------------------------
PROCEDURE LoadMemory (
  VARIABLE MemoryPtr  : INOUT VitalMemoryDataType;
  CONSTANT FileName   : IN    STRING;
  CONSTANT BinaryFile : IN BOOLEAN := FALSE
) IS
  FILE     Fptr      : TEXT OPEN read_mode IS FileName;
  VARIABLE OneLine   : LINE;
  VARIABLE Ignore    : CHARACTER;
  VARIABLE Index     : NATURAL  := 1;
  VARIABLE LineNo    : NATURAL  := 0;
  VARIABLE Address   : INTEGER  := 0;
  VARIABLE DataInBus : std_logic_vector(MemoryPtr.NoOfBitsPerWord-1  DOWNTO 0);
  VARIABLE AddrStr   : STRING(1 TO 80) ;    
  VARIABLE DataInStr : STRING(1 TO 255) ;    
BEGIN
  IF (ENDFILE(fptr)) THEN
    PrintMemoryMessage (MsgVDM, ErrLdFileEmpty, 
      "[FileName]="&FileName);
    RETURN;
  END IF ;
  PrintMemoryMessage (
    MsgVDM,ErrLdMemInfo, "[FileName]="&FileName
  );
  WHILE (NOT ENDFILE(fptr)) LOOP
    ReadLine(Fptr, OneLine);
    LineNo := LineNo + 1 ;
    --  First ignoring leading spaces.  
    WHILE (OneLine'LENGTH /= 0 and IsSpace(OneLine(1))) LOOP
      READ (OneLine, Ignore) ;     -- Ignoring the space character.
    END LOOP ;
    --  Note that, by now oneline has been "stripped" of its leading spaces.
    IF ( OneLine(1) = '@' ) THEN
      READ (OneLine, Ignore);  -- Ignore the '@' character and read the string.
      -- Now strip off spaces, if any, between '@' and Address string.
      WHILE (OneLine'LENGTH /= 0 and IsSpace(OneLine(1))) LOOP
        READ (OneLine, Ignore) ;     -- Ignoring the space character.
      END LOOP ;
      -- Now get the string which represents the address into string variable.
      Index := 1;
      WHILE (OneLine'LENGTH /= 0 AND (NOT(IsSpace(OneLine(1))))) LOOP
        READ(OneLine, AddrStr(Index));
        Index := Index + 1;
      END LOOP ;
      AddrStr(Index) := NUL;
      -- Now convert the hex string into a hex integer
      Address := HexToInt(AddrStr) ;
    ELSE
      IF ( LineNo /= 1 ) THEN
        Address := Address + 1;
      END IF;
    END IF ;
    IF ( Address > (MemoryPtr.NoOfWords - 1) ) THEN
      PrintMemoryMessage (MsgVDM, ErrLdAddrRng, 
      "[Address,lineno]=", Address, LineNo) ;
      EXIT ;
    END IF;
    -- Now strip off spaces, between Address string and DataInBus string.
    WHILE (OneLine'LENGTH /= 0 AND IsSpace(OneLine(1))) LOOP
      READ (OneLine, Ignore) ;     -- Ignoring the space character.
    END LOOP ;
    Index := 1;
    WHILE (OneLine'LENGTH /= 0 AND (NOT(IsSpace(OneLine(1))))) LOOP
      READ(OneLine, DataInStr(Index));
      Index := Index + 1;
    END LOOP ;
    DataInStr(Index) := NUL;
    IF (BinaryFile) THEN
      DataInBus := BinToBitv (DataInStr);
    ELSE
      DataInBus := HexToBitv (DataInStr);
    END IF ;
    WriteMemory (MemoryPtr, DataInBus, Address);
  END LOOP ;
END LoadMemory;

-- ----------------------------------------------------------------------------
-- Procedure:   MemoryMatch
-- Parameters:  Symbol       - Symbol from memory table
--              TestFlag     - Interpreted data or address symbol
--              In2          - input from VitalMemoryTable procedure
--                             to memory table
--              In2LastValue - Previous value of input
--              Err          - TRUE if symbol is not a valid input symbol
--              ReturnValue  - TRUE if match occurred
-- Description: This procedure sets ReturnValue to true if in2 matches
--              symbol (from the memory table).  If symbol is an edge
--              value edge is set to true and in2 and in2LastValue are
--              checked against symbol.  Err is set to true if symbol
--              is an invalid value for the input portion of the memory
--              table.
-- ----------------------------------------------------------------------------
PROCEDURE MemoryMatch (
  CONSTANT Symbol       : IN VitalMemorySymbolType;
  CONSTANT In2          : IN std_ulogic;
  CONSTANT In2LastValue : IN std_ulogic;
  VARIABLE Err          : OUT BOOLEAN;
  VARIABLE ReturnValue  : OUT BOOLEAN
) IS
BEGIN
  IF (NOT ValidMemoryTableInput(Symbol) ) THEN
    PrintMemoryMessage(MsgVMT,ErrUnknSymbol,To_MemoryChar(Symbol));
    Err := TRUE;
    ReturnValue := FALSE;
  ELSE
    ReturnValue := MemoryTableMatch(To_X01(In2LastValue), To_X01(In2), Symbol);
    Err := FALSE;
  END IF;
END;

-- ----------------------------------------------------------------------------
PROCEDURE MemoryMatch (
  CONSTANT Symbol       : IN VitalMemorySymbolType;
  CONSTANT TestFlag     : IN VitalMemorySymbolType;
  VARIABLE Err          : OUT BOOLEAN;
  VARIABLE ReturnValue  : OUT BOOLEAN
) IS
BEGIN
  Err := FALSE;
  ReturnValue := FALSE;
  CASE Symbol IS
    WHEN 'g'|'u'|'i'|'G'|'U'|'I'|'-'|'*'|'S' =>
      IF (Symbol = TestFlag) THEN
        ReturnValue := TRUE;
      ELSE
        CASE Symbol IS
          WHEN '-'    =>
            ReturnValue := TRUE;
            Err := FALSE;
          WHEN '*'    =>
            IF (TestFlag = 'G' OR 
                TestFlag = 'U' OR 
                TestFlag = 'I') THEN
              ReturnValue := TRUE;
              Err := FALSE;
            END IF;
          WHEN 'S'    =>
            IF (TestFlag = 'g' OR 
                TestFlag = 'u' OR 
                TestFlag = 'i') THEN
                ReturnValue := TRUE;
              Err := FALSE;
            END IF;
          WHEN OTHERS =>
            ReturnValue := FALSE;
        END CASE;
      END IF;
    WHEN OTHERS =>
      Err := TRUE;
      RETURN;
  END CASE;
END;

-- ----------------------------------------------------------------------------
-- Procedure:   MemoryTableCorruptMask
-- Description: Compute memory and data corruption masks for memory table
-- ----------------------------------------------------------------------------
PROCEDURE MemoryTableCorruptMask (
  VARIABLE CorruptMask            : OUT std_logic_vector;
  CONSTANT Action                 : IN VitalMemorySymbolType;
  CONSTANT EnableIndex            : IN INTEGER;
  CONSTANT BitsPerWord            : IN INTEGER;
  CONSTANT BitsPerSubWord         : IN INTEGER;
  CONSTANT BitsPerEnable          : IN INTEGER
) IS
  VARIABLE CorruptMaskTmp : std_logic_vector (CorruptMask'RANGE)
                          := (OTHERS => '0');
  VARIABLE ViolFlAryPosn  : INTEGER;
  VARIABLE HighBit        : INTEGER;
  VARIABLE LowBit         : INTEGER;
BEGIN
  CASE (Action) IS
    WHEN 'c'|'l'|'e'  =>
      -- Corrupt whole word
      CorruptMaskTmp := (OTHERS => 'X');
      CorruptMask := CorruptMaskTmp;
      RETURN;
    WHEN 'd'|'C'|'L'|'D'|'E' =>
      -- Process corruption below
    WHEN OTHERS =>
      -- No data or memory corruption
      CorruptMaskTmp := (OTHERS => '0');
      CorruptMask := CorruptMaskTmp;
      RETURN;
  END CASE;
  IF (Action = 'd') THEN
    CorruptMaskTmp := (OTHERS => 'X');
    CorruptMask := CorruptMaskTmp;
    RETURN;
  END IF;
  -- Remaining are subword cases 'C', 'L', 'D', 'E'
  CorruptMaskTmp := (OTHERS => '0');
  LowBit := 0;
  HighBit := BitsPerSubWord-1;
  SubWordLoop:
  FOR i IN 0 TO BitsPerEnable-1 LOOP
    IF (i = EnableIndex) THEN
      FOR j IN HighBit TO LowBit LOOP
        CorruptMaskTmp(j) := 'X';
      END LOOP;
    END IF;
    -- Calculate HighBit and LowBit
    LowBit := LowBit + BitsPerSubWord;
    IF (LowBit > BitsPerWord) THEN
      LowBit := BitsPerWord;
    END IF;
    HighBit := LowBit + BitsPerSubWord;
    IF (HighBit > BitsPerWord) THEN
      HighBit := BitsPerWord;
    ELSE
      HighBit := HighBit - 1;
    END IF;
  END LOOP;
  CorruptMask := CorruptMaskTmp;
  RETURN;
END;

-- ----------------------------------------------------------------------------
PROCEDURE MemoryTableCorruptMask (
  VARIABLE CorruptMask            : OUT std_logic_vector;
  CONSTANT Action                 : IN VitalMemorySymbolType
) IS
  VARIABLE CorruptMaskTmp : std_logic_vector (0 TO CorruptMask'LENGTH-1)
                          := (OTHERS => '0');
  VARIABLE ViolFlAryPosn  : INTEGER;
  VARIABLE HighBit        : INTEGER;
  VARIABLE LowBit         : INTEGER;
BEGIN
  CASE (Action) IS
    WHEN 'c'|'l'|'d'|'e'|'C'|'L'|'D'|'E' =>
      -- Corrupt whole word
      CorruptMaskTmp := (OTHERS => 'X');
      CorruptMask := CorruptMaskTmp;
      RETURN;
    WHEN OTHERS =>
      -- No data or memory corruption
      CorruptMaskTmp := (OTHERS => '0');
      CorruptMask := CorruptMaskTmp;
      RETURN;
  END CASE;
  RETURN;
END;

-- ----------------------------------------------------------------------------
-- Procedure:   MemoryTableCorruptMask
-- Description: Compute memory and data corruption masks for violation table
-- ----------------------------------------------------------------------------
PROCEDURE ViolationTableCorruptMask (
  VARIABLE CorruptMask            : OUT std_logic_vector;
  CONSTANT Action                 : IN VitalMemorySymbolType;
  CONSTANT ViolationFlags         : IN std_logic_vector;
  CONSTANT ViolationFlagsArray    : IN std_logic_vector;
  CONSTANT ViolationSizesArray    : IN VitalMemoryViolFlagSizeType;
  CONSTANT ViolationTable         : IN VitalMemoryTableType;
  CONSTANT TableIndex             : IN INTEGER;
  CONSTANT BitsPerWord            : IN INTEGER;
  CONSTANT BitsPerSubWord         : IN INTEGER;
  CONSTANT BitsPerEnable          : IN INTEGER
) IS
  VARIABLE CorruptMaskTmp : std_logic_vector (CorruptMask'RANGE)
                          := (OTHERS => '0');
  VARIABLE ViolMaskTmp    : std_logic_vector (CorruptMask'RANGE)
                          := (OTHERS => '0');
  VARIABLE ViolFlAryPosn  : INTEGER;
  VARIABLE HighBit        : INTEGER;
  VARIABLE LowBit         : INTEGER;
  CONSTANT ViolFlagsSize  : INTEGER := ViolationFlags'LENGTH;
  CONSTANT ViolFlArySize  : INTEGER := ViolationFlagsArray'LENGTH;
  CONSTANT TableEntries   : INTEGER := ViolationTable'LENGTH(1);
  CONSTANT TableWidth     : INTEGER := ViolationTable'LENGTH(2);
  CONSTANT DatActionNdx   : INTEGER := TableWidth - 1;
  CONSTANT MemActionNdx   : INTEGER := TableWidth - 2;
BEGIN
  CASE (Action) IS
    WHEN 'c'|'l'|'e'  =>
      -- Corrupt whole word
      CorruptMaskTmp := (OTHERS => 'X');
      CorruptMask := CorruptMaskTmp;
      RETURN;
    WHEN 'd'|'C'|'L'|'D'|'E' =>
      -- Process corruption below
    WHEN OTHERS =>
      -- No data or memory corruption
      CorruptMaskTmp := (OTHERS => '0');
      CorruptMask := CorruptMaskTmp;
      RETURN;
  END CASE;
  RowLoop: -- Check each element of the ViolationFlags
  FOR j IN 0 TO ViolFlagsSize LOOP
    IF (j = ViolFlagsSize) THEN
      ViolFlAryPosn := 0;
      RowLoop2: -- Check relevant elements of the ViolationFlagsArray
      FOR k IN 0 TO MemActionNdx - ViolFlagsSize - 1 LOOP
        IF (ViolationTable(TableIndex, k + ViolFlagsSize) = 'X') THEN
          MaskLoop: -- Set the 'X' bits in the violation mask
          FOR m IN INTEGER RANGE 0 TO CorruptMask'LENGTH-1 LOOP
            IF (m <= ViolationSizesArray(k)-1) THEN
              ViolMaskTmp(m) := ViolMaskTmp(m) XOR 
              ViolationFlagsArray(ViolFlAryPosn+m);
            ELSE
              EXIT MaskLoop;
            END IF;
          END LOOP;
        END IF;
        ViolFlAryPosn := ViolFlAryPosn + ViolationSizesArray(k);
      END LOOP;
    ELSE
      IF (ViolationTable(TableIndex, j) = 'X') THEN
        ViolMaskTmp(0) := ViolMaskTmp(0) XOR ViolationFlags(j);
      END IF;
    END IF;
  END LOOP;
  IF (Action = 'd') THEN
    CorruptMask := ViolMaskTmp;
    RETURN;
  END IF;
  -- Remaining are subword cases 'C', 'L', 'D', 'E'
  CorruptMaskTmp := (OTHERS => '0');
  LowBit := 0;
  HighBit := BitsPerSubWord-1;
  SubWordLoop:
  FOR i IN 0 TO BitsPerEnable-1 LOOP
    IF (ViolMaskTmp(i) = 'X') THEN
      FOR j IN HighBit TO LowBit LOOP
        CorruptMaskTmp(j) := 'X';
      END LOOP;
    END IF;
    -- Calculate HighBit and LowBit
    LowBit := LowBit + BitsPerSubWord;
    IF (LowBit > BitsPerWord) THEN
      LowBit := BitsPerWord;
    END IF;
    HighBit := LowBit + BitsPerSubWord;
    IF (HighBit > BitsPerWord) THEN
      HighBit := BitsPerWord;
    ELSE
      HighBit := HighBit - 1;
    END IF;
  END LOOP;
  CorruptMask := CorruptMaskTmp;
  RETURN;
END;

-- ----------------------------------------------------------------------------
-- Procedure:   MemoryTableLookUp
-- Parameters:  MemoryAction  - Output memory action to be performed
--              DataAction    - Output data action to be performed
--              PrevControls  - Previous data in for edge detection
--              PrevEnableBus - Previous enables for edge detection
--              Controls      - Agregate of scalar control lines
--              EnableBus     - Concatenation of vector control lines
--              EnableIndex   - Current slice of vector control lines
--              AddrFlag      - Matching symbol from address decoding
--              DataFlag      - Matching symbol from data decoding
--              MemoryTable   - Input memory action table
--              PortName      - Port name string for messages
--              HeaderMsg     - Header string for messages
--              MsgOn         - Control message output
--
-- Description: This function is used to find the output of the
--              MemoryTable corresponding to a given set of inputs.
--
-- ----------------------------------------------------------------------------
PROCEDURE MemoryTableLookUp (
  VARIABLE MemoryAction       : OUT VitalMemorySymbolType;
  VARIABLE DataAction         : OUT VitalMemorySymbolType;
  VARIABLE MemoryCorruptMask  : OUT std_logic_vector;
  VARIABLE DataCorruptMask    : OUT std_logic_vector;
  CONSTANT PrevControls       : IN std_logic_vector;
  CONSTANT Controls           : IN std_logic_vector;
  CONSTANT AddrFlag           : IN VitalMemorySymbolType;
  CONSTANT DataFlag           : IN VitalMemorySymbolType;
  CONSTANT MemoryTable        : IN VitalMemoryTableType;
  CONSTANT PortName           : IN STRING := "";
  CONSTANT HeaderMsg          : IN STRING := "";
  CONSTANT MsgOn              : IN BOOLEAN := TRUE
) IS
  CONSTANT ControlsSize   : INTEGER := Controls'LENGTH;
  CONSTANT TableEntries   : INTEGER := MemoryTable'LENGTH(1);
  CONSTANT TableWidth     : INTEGER := MemoryTable'LENGTH(2);
  CONSTANT DatActionNdx   : INTEGER := TableWidth - 1;
  CONSTANT MemActionNdx   : INTEGER := TableWidth - 2;
  CONSTANT DataInBusNdx   : INTEGER := TableWidth - 3;
  CONSTANT AddressBusNdx  : INTEGER := TableWidth - 4;
  VARIABLE AddrFlagTable  : VitalMemorySymbolType;
  VARIABLE Match  : BOOLEAN;
  VARIABLE Err    : BOOLEAN := FALSE;
  VARIABLE TableAlias     : VitalMemoryTableType(
                              0 TO TableEntries - 1,
                              0 TO TableWidth - 1)
                              := MemoryTable;
BEGIN
  ColLoop: -- Compare each entry in the table
  FOR i IN TableAlias'RANGE(1) LOOP
    RowLoop: -- Check each element of the Controls
    FOR j IN 0 TO ControlsSize LOOP
      IF (j = ControlsSize) THEN
        -- a match occurred, now check AddrFlag, DataFlag
        MemoryMatch(TableAlias(i,AddressBusNdx),AddrFlag,Err,Match);
        IF (Match) THEN
          MemoryMatch(TableAlias(i,DataInBusNdx),DataFlag,Err,Match);
          IF (Match) THEN
            MemoryTableCorruptMask (
              CorruptMask     => MemoryCorruptMask    ,
              Action          => TableAlias(i, MemActionNdx)
            );
            MemoryTableCorruptMask (
              CorruptMask     => DataCorruptMask      ,
              Action          => TableAlias(i, DatActionNdx)
            );
            -- get the return memory and data actions
            MemoryAction := TableAlias(i, MemActionNdx);
            DataAction   := TableAlias(i, DatActionNdx);
            -- DEBUG: The lines below report table search
            IF (MsgOn) THEN
              PrintMemoryMessage(MsgVMT,TableAlias,i,PortName);
            END IF;
            -- DEBUG: The lines above report table search
            RETURN;
          END IF;
        END IF;
      ELSE
        -- Match memory table inputs
        MemoryMatch (   TableAlias(i,j),
        Controls(j), PrevControls(j),
        Err, Match);
      END IF;
      EXIT RowLoop WHEN NOT(Match);
      EXIT ColLoop WHEN Err;
    END LOOP RowLoop;
  END LOOP ColLoop;
  -- no match found, return default action
  MemoryAction := 's';   -- no change to memory
  DataAction   := 'S';   -- no change to dataout
  IF (MsgOn) THEN
  PrintMemoryMessage(MsgVMT,ErrDefMemAct,HeaderMsg,PortName);
  END IF;
  RETURN;
END;

-- ----------------------------------------------------------------------------
PROCEDURE MemoryTableLookUp (
  VARIABLE MemoryAction       : OUT VitalMemorySymbolType;
  VARIABLE DataAction         : OUT VitalMemorySymbolType;
  VARIABLE MemoryCorruptMask  : OUT std_logic_vector;
  VARIABLE DataCorruptMask    : OUT std_logic_vector;
  CONSTANT PrevControls       : IN std_logic_vector;
  CONSTANT PrevEnableBus      : IN std_logic_vector;
  CONSTANT Controls           : IN std_logic_vector;
  CONSTANT EnableBus          : IN std_logic_vector;
  CONSTANT EnableIndex        : IN INTEGER;
  CONSTANT BitsPerWord        : IN INTEGER;
  CONSTANT BitsPerSubWord     : IN INTEGER;
  CONSTANT BitsPerEnable      : IN INTEGER;
  CONSTANT AddrFlag           : IN VitalMemorySymbolType;
  CONSTANT DataFlag           : IN VitalMemorySymbolType;
  CONSTANT MemoryTable        : IN VitalMemoryTableType;
  CONSTANT PortName           : IN STRING := "";
  CONSTANT HeaderMsg          : IN STRING := "";
  CONSTANT MsgOn              : IN BOOLEAN := TRUE
) IS
  CONSTANT ControlsSize   : INTEGER := Controls'LENGTH;
  CONSTANT TableEntries   : INTEGER := MemoryTable'LENGTH(1);
  CONSTANT TableWidth     : INTEGER := MemoryTable'LENGTH(2);
  CONSTANT DatActionNdx   : INTEGER := TableWidth - 1;
  CONSTANT MemActionNdx   : INTEGER := TableWidth - 2;
  CONSTANT DataInBusNdx   : INTEGER := TableWidth - 3;
  CONSTANT AddressBusNdx  : INTEGER := TableWidth - 4;
  VARIABLE AddrFlagTable  : VitalMemorySymbolType;
  VARIABLE Match  : BOOLEAN;
  VARIABLE Err    : BOOLEAN := FALSE;
  VARIABLE TableAlias     : VitalMemoryTableType(
                              0 TO TableEntries - 1,
                              0 TO TableWidth - 1)
                                := MemoryTable;
BEGIN
  ColLoop: -- Compare each entry in the table
  FOR i IN TableAlias'RANGE(1) LOOP
    RowLoop: -- Check each element of the Controls
    FOR j IN 0 TO ControlsSize LOOP
      IF (j = ControlsSize) THEN
        -- a match occurred, now check EnableBus, AddrFlag, DataFlag
        IF (EnableIndex >= 0) THEN
          RowLoop2: -- Check relevant elements of the EnableBus
          FOR k IN 0 TO AddressBusNdx - ControlsSize - 1 LOOP
            MemoryMatch (   TableAlias(i,k + ControlsSize),
                            EnableBus(k * BitsPerEnable + EnableIndex),
                            PrevEnableBus(k * BitsPerEnable + EnableIndex),
                            Err, Match);
            EXIT RowLoop2 WHEN NOT(Match);
          END LOOP;
        END IF;
        IF (Match) THEN
          MemoryMatch(TableAlias(i,AddressBusNdx),AddrFlag,Err,Match);
          IF (Match) THEN
            MemoryMatch(TableAlias(i,DataInBusNdx),DataFlag,Err,Match);
            IF (Match) THEN
              MemoryTableCorruptMask (
                CorruptMask     => MemoryCorruptMask    ,
                Action          => TableAlias(i, MemActionNdx),
                EnableIndex     => EnableIndex          ,
                BitsPerWord     => BitsPerWord          ,
                BitsPerSubWord  => BitsPerSubWord       ,
                BitsPerEnable   => BitsPerEnable
              );
              MemoryTableCorruptMask (
                CorruptMask     => DataCorruptMask      ,
                Action          => TableAlias(i, DatActionNdx),
                EnableIndex     => EnableIndex          ,
                BitsPerWord     => BitsPerWord          ,
                BitsPerSubWord  => BitsPerSubWord       ,
                BitsPerEnable   => BitsPerEnable
              );
              -- get the return memory and data actions
              MemoryAction := TableAlias(i, MemActionNdx);
              DataAction   := TableAlias(i, DatActionNdx);
              -- DEBUG: The lines below report table search
              IF (MsgOn) THEN
                PrintMemoryMessage(MsgVMT,TableAlias,i,PortName);
              END IF;
              -- DEBUG: The lines above report table search
              RETURN;
            END IF;
          END IF;
        END IF;
      ELSE
        -- Match memory table inputs
        MemoryMatch (   TableAlias(i,j),
                        Controls(j), PrevControls(j),
                        Err, Match);
      END IF;
      EXIT RowLoop WHEN NOT(Match);
      EXIT ColLoop WHEN Err;
    END LOOP RowLoop;
  END LOOP ColLoop;
  -- no match found, return default action
  MemoryAction := 's';   -- no change to memory
  DataAction   := 'S';   -- no change to dataout
  IF (MsgOn) THEN
    PrintMemoryMessage(MsgVMT,ErrDefMemAct,HeaderMsg,PortName);
  END IF;
  RETURN;
END;

-- ----------------------------------------------------------------------------
-- Procedure:   ViolationTableLookUp
-- Parameters:  MemoryAction  - Output memory action to be performed
--              DataAction    - Output data action to be performed
--              TimingDataArray   - This is currently not used (comment out) 
--              ViolationArray    - Aggregation of violation variables
--              ViolationTable    - Input memory violation table
--              PortName      - Port name string for messages
--              HeaderMsg     - Header string for messages
--              MsgOn         - Control message output
-- Description: This function is used to find the output of the
--              ViolationTable corresponding to a given set of inputs.
-- ----------------------------------------------------------------------------
PROCEDURE ViolationTableLookUp (
  VARIABLE MemoryAction           : OUT VitalMemorySymbolType;
  VARIABLE DataAction             : OUT VitalMemorySymbolType;
  VARIABLE MemoryCorruptMask      : OUT std_logic_vector;
  VARIABLE DataCorruptMask        : OUT std_logic_vector;
  CONSTANT ViolationFlags         : IN std_logic_vector;
  CONSTANT ViolationFlagsArray    : IN std_logic_vector;
  CONSTANT ViolationSizesArray    : IN VitalMemoryViolFlagSizeType;
  CONSTANT ViolationTable         : IN VitalMemoryTableType;
  CONSTANT BitsPerWord            : IN INTEGER;
  CONSTANT BitsPerSubWord         : IN INTEGER;
  CONSTANT BitsPerEnable          : IN INTEGER;
  CONSTANT PortName               : IN STRING := "";
  CONSTANT HeaderMsg              : IN STRING := "";
  CONSTANT MsgOn                  : IN BOOLEAN := TRUE
) IS
  CONSTANT ViolFlagsSize  : INTEGER := ViolationFlags'LENGTH;
  CONSTANT ViolFlArySize  : INTEGER := ViolationFlagsArray'LENGTH;
  VARIABLE ViolFlAryPosn  : INTEGER;
  VARIABLE ViolFlAryItem  : std_ulogic;
  CONSTANT ViolSzArySize  : INTEGER := ViolationSizesArray'LENGTH;
  CONSTANT TableEntries   : INTEGER := ViolationTable'LENGTH(1);
  CONSTANT TableWidth     : INTEGER := ViolationTable'LENGTH(2);
  CONSTANT DatActionNdx   : INTEGER := TableWidth - 1;
  CONSTANT MemActionNdx   : INTEGER := TableWidth - 2;
  VARIABLE HighBit        : NATURAL := 0;
  VARIABLE LowBit         : NATURAL := 0;
  VARIABLE Match  : BOOLEAN;
  VARIABLE Err    : BOOLEAN := FALSE;
  VARIABLE TableAlias     : VitalMemoryTableType(
                              0 TO TableEntries - 1,
                              0 TO TableWidth - 1)
                                := ViolationTable;
BEGIN
  ColLoop: -- Compare each entry in the table
  FOR i IN TableAlias'RANGE(1) LOOP
    RowLoop: -- Check each element of the ViolationFlags
    FOR j IN 0 TO ViolFlagsSize LOOP
      IF (j = ViolFlagsSize) THEN
        ViolFlAryPosn := 0;
        RowLoop2: -- Check relevant elements of the ViolationFlagsArray
        FOR k IN 0 TO MemActionNdx - ViolFlagsSize - 1 LOOP
          ViolFlAryItem := '0';
          SubwordLoop: -- Check for 'X' in ViolationFlagsArray chunk
          FOR s IN ViolFlAryPosn TO ViolFlAryPosn+ViolationSizesArray(k)-1 LOOP
            IF (ViolationFlagsArray(s) = 'X') THEN
              ViolFlAryItem := 'X';
              EXIT SubwordLoop;
            END IF;
          END LOOP;
          MemoryMatch (   TableAlias(i,k + ViolFlagsSize),
          ViolFlAryItem,ViolFlAryItem,
          Err, Match);
          ViolFlAryPosn := ViolFlAryPosn + ViolationSizesArray(k);
          EXIT RowLoop2 WHEN NOT(Match);
        END LOOP;
        IF (Match) THEN
          -- Compute memory and data corruption masks
          ViolationTableCorruptMask(
            CorruptMask             => MemoryCorruptMask    ,
            Action                  => TableAlias(i, MemActionNdx),
            ViolationFlags          => ViolationFlags       ,
            ViolationFlagsArray     => ViolationFlagsArray  ,
            ViolationSizesArray     => ViolationSizesArray  ,
            ViolationTable          => ViolationTable       ,
            TableIndex              => i                    ,
            BitsPerWord             => BitsPerWord          ,
            BitsPerSubWord          => BitsPerSubWord       ,
            BitsPerEnable           => BitsPerEnable
          );
          ViolationTableCorruptMask(
            CorruptMask             => DataCorruptMask      ,
            Action                  => TableAlias(i, DatActionNdx),
            ViolationFlags          => ViolationFlags       ,
            ViolationFlagsArray     => ViolationFlagsArray  ,
            ViolationSizesArray     => ViolationSizesArray  ,
            ViolationTable          => ViolationTable       ,
            TableIndex              => i                    ,
            BitsPerWord             => BitsPerWord          ,
            BitsPerSubWord          => BitsPerSubWord       ,
            BitsPerEnable           => BitsPerEnable
          );
          -- get the return memory and data actions
          MemoryAction := TableAlias(i, MemActionNdx);
          DataAction   := TableAlias(i, DatActionNdx);
          -- DEBUG: The lines below report table search
          IF (MsgOn) THEN
            PrintMemoryMessage(MsgVMV,TableAlias,i,PortName);
          END IF;
          -- DEBUG: The lines above report table search
          RETURN;
        END IF;
      ELSE
        -- Match violation table inputs
        Err := FALSE;
        Match := FALSE;
        IF (TableAlias(i,j) /= 'X' AND 
            TableAlias(i,j) /= '0' AND
            TableAlias(i,j) /= '-') THEN
          Err := TRUE;
        ELSIF (TableAlias(i,j) = '-' OR
            (TableAlias(i,j) = 'X' AND ViolationFlags(j) = 'X') OR
            (TableAlias(i,j) = '0' AND ViolationFlags(j) = '0')) THEN
          Match := TRUE;
        END IF;
      END IF;
      EXIT RowLoop WHEN NOT(Match);
      EXIT ColLoop WHEN Err;
    END LOOP RowLoop;
  END LOOP ColLoop;
  -- no match found, return default action
  MemoryAction := 's';   -- no change to memory
  DataAction   := 'S';   -- no change to dataout
  IF (MsgOn) THEN
    PrintMemoryMessage(MsgVMV,ErrDefMemAct,HeaderMsg,PortName);
  END IF;
  RETURN;
END;

-- ----------------------------------------------------------------------------
-- Procedure:   HandleMemoryAction
-- Parameters:  MemoryData   - Pointer to memory data structure
--              PortFlag     - Indicates read/write mode of port
--              CorruptMask  - XOR'ed with DataInBus when corrupting
--              DataInBus    - Current data bus in
--              Address      - Current address integer
--              HighBit      - Current address high bit
--              LowBit       - Current address low bit
--              MemoryTable  - Input memory action table
--              MemoryAction - Memory action to be performed
--              PortName     - Port name string for messages
--              HeaderMsg    - Header string for messages
--              MsgOn        - Control message output
-- Description: This procedure performs the specified memory action on
--              the input memory data structure.
-- ----------------------------------------------------------------------------
PROCEDURE HandleMemoryAction (
  VARIABLE MemoryData     : INOUT VitalMemoryDataType;
  VARIABLE PortFlag       : INOUT VitalPortFlagType;
  CONSTANT CorruptMask    : IN std_logic_vector;
  CONSTANT DataInBus      : IN std_logic_vector;
  CONSTANT Address        : IN INTEGER;
  CONSTANT HighBit        : IN NATURAL;
  CONSTANT LowBit         : IN NATURAL;
  CONSTANT MemoryTable    : IN VitalMemoryTableType;
  CONSTANT MemoryAction   : IN VitalMemorySymbolType;
  CONSTANT CallerName     : IN STRING;
  CONSTANT PortName       : IN STRING := "";
  CONSTANT HeaderMsg      : IN STRING := "";
  CONSTANT MsgOn          : IN BOOLEAN := TRUE
) IS
  VARIABLE DataInTmp : std_logic_vector(DataInBus'RANGE)
                        := DataInBus;
  BEGIN

  -- Handle the memory action
  CASE MemoryAction IS

  WHEN 'w'    =>
    -- Writing data to memory
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrWrDatMem,HeaderMsg,PortName);
    END IF;
    WriteMemory(MemoryData,DataInBus,Address,HighBit,LowBit);
    PortFlag.MemoryCurrent := WRITE;

  WHEN 's'    =>
    -- Retaining previous memory contents
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrNoChgMem,HeaderMsg,PortName);
    END IF;
    -- Set memory current to quiet state
    PortFlag.MemoryCurrent := READ;

  WHEN 'c'    =>
    -- Corrupting entire memory with 'X'
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrCrAllMem,HeaderMsg,PortName);
    END IF;
    DataInTmp := (OTHERS => 'X');
    -- No need to CorruptMask
    FOR i IN 0 TO MemoryData.NoOfWords-1 LOOP
      WriteMemory(MemoryData,DataInTmp,i);
    END LOOP;
    PortFlag.MemoryCurrent := CORRUPT;

  WHEN 'l'    =>
    -- Corrupting a word in memory with 'X'
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrCrWrdMem,HeaderMsg,PortName);
    END IF;
    DataInTmp := (OTHERS => 'X');
    -- No need to CorruptMask
    WriteMemory(MemoryData,DataInTmp,Address);
    PortFlag.MemoryCurrent := CORRUPT;

  WHEN 'd'    =>
    -- Corrupting a single bit in memory with 'X'
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrCrBitMem,HeaderMsg,PortName);
    END IF;
    ReadMemory(MemoryData,DataInTmp,Address);
    DataInTmp := DataInTmp XOR CorruptMask;
    WriteMemory(MemoryData,DataInTmp,Address,HighBit,LowBit);
    PortFlag.MemoryCurrent := CORRUPT;

  WHEN 'e'    =>
    -- Corrupting a word with 'X' based on data in
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrCrDatMem,HeaderMsg,PortName);
    END IF;
    ReadMemory(MemoryData,DataInTmp,Address);
    IF (DataInTmp /= DataInBus) THEN
      DataInTmp := (OTHERS => 'X');
      -- No need to CorruptMask
      WriteMemory(MemoryData,DataInTmp,Address);
    END IF;
    PortFlag.MemoryCurrent := CORRUPT;

  WHEN 'C'    =>
    -- Corrupting a sub-word entire memory with 'X'
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrCrAllSubMem,HeaderMsg,PortName);
    END IF;
    FOR i IN 0 TO MemoryData.NoOfWords-1 LOOP
      ReadMemory(MemoryData,DataInTmp,i);
      DataInTmp := DataInTmp XOR CorruptMask;
      WriteMemory(MemoryData,DataInTmp,i,HighBit,LowBit);
    END LOOP;
    PortFlag.MemoryCurrent := CORRUPT;

  WHEN 'L'    =>
    -- Corrupting a sub-word in memory with 'X'
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrCrWrdSubMem,HeaderMsg,PortName);
    END IF;
    ReadMemory(MemoryData,DataInTmp,Address);
    DataInTmp := DataInTmp XOR CorruptMask;
    WriteMemory(MemoryData,DataInTmp,Address,HighBit,LowBit);
    PortFlag.MemoryCurrent := CORRUPT;

  WHEN 'D'    =>
    -- Corrupting a single bit of a memory sub-word with 'X'
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrCrBitSubMem,HeaderMsg,PortName);
    END IF;
    ReadMemory(MemoryData,DataInTmp,Address);
    DataInTmp := DataInTmp XOR CorruptMask;
    WriteMemory(MemoryData,DataInTmp,Address,HighBit,LowBit);
    PortFlag.MemoryCurrent := CORRUPT;

  WHEN 'E'    =>
    -- Corrupting a sub-word with 'X' based on data in
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrCrDatSubMem,HeaderMsg,PortName);
    END IF;
    ReadMemory(MemoryData,DataInTmp,Address);
    IF (DataInBus(HighBit DOWNTO LowBit) /= 
      DataInTmp(HighBit DOWNTO LowBit)) THEN
      DataInTmp(HighBit DOWNTO LowBit) := (OTHERS => 'X');
      WriteMemory(MemoryData,DataInTmp,Address,HighBit,LowBit);
    END IF;
    --PortFlag := WRITE;
    PortFlag.MemoryCurrent := CORRUPT;

  WHEN '0'    =>
    -- Assigning low level to memory location
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrAsg0Mem,HeaderMsg,PortName);
    END IF;
    DataInTmp := (OTHERS => '0');
    WriteMemory(MemoryData,DataInTmp,Address, HighBit, LowBit);
    PortFlag.MemoryCurrent := WRITE;

  WHEN '1'    =>
    -- Assigning high level to memory location
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrAsg1Mem,HeaderMsg,PortName);
    END IF;
    DataInTmp := (OTHERS => '1');
    WriteMemory(MemoryData,DataInTmp,Address, HighBit, LowBit);
    PortFlag.MemoryCurrent := WRITE;

  WHEN 'Z'    =>
    -- Assigning high impedence to memory location
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrAsgZMem,HeaderMsg,PortName);
    END IF;
    DataInTmp := (OTHERS => 'Z');
    WriteMemory(MemoryData,DataInTmp,Address, HighBit, LowBit);
    PortFlag.MemoryCurrent := WRITE;

  WHEN OTHERS =>
    -- Unknown memory action
    PortFlag.MemoryCurrent := UNDEF;
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrUnknMemDo,HeaderMsg,PortName);
    END IF;

  END CASE;

  -- Note: HandleMemoryAction does not change the PortFlag.OutputDisable
END;

-- ----------------------------------------------------------------------------
-- Procedure:   HandleDataAction
-- Parameters:  DataOutBus  - Output result of the data action
--              MemoryData  - Input pointer to memory data structure
--              PortFlag    - Indicates read/write mode of port
--              CorruptMask - XOR'ed with DataInBus when corrupting
--              DataInBus   - Current data bus in
--              Address     - Current address integer
--              HighBit     - Current address high bit
--              LowBit      - Current address low bit
--              MemoryTable - Input memory action table
--              DataAction  - Data action to be performed
--              PortName    - Port name string for messages
--              HeaderMsg   - Header string for messages
--              MsgOn       - Control message output
-- Description: This procedure performs the specified data action based
--              on the input memory data structure.  Checks whether
--              the previous state is HighZ. If yes then portFlag
--              should be NOCHANGE for VMPD to ignore IORetain
--              corruption. The idea is that the first Z should be
--              propagated but later ones should be ignored.
-- ----------------------------------------------------------------------------
PROCEDURE HandleDataAction (
  VARIABLE DataOutBus     : INOUT std_logic_vector;
  VARIABLE MemoryData     : INOUT VitalMemoryDataType;
  VARIABLE PortFlag       : INOUT VitalPortFlagType;
  CONSTANT CorruptMask    : IN std_logic_vector;
  CONSTANT DataInBus      : IN std_logic_vector;
  CONSTANT Address        : IN INTEGER;
  CONSTANT HighBit        : IN NATURAL;
  CONSTANT LowBit         : IN NATURAL;
  CONSTANT MemoryTable    : IN VitalMemoryTableType;
  CONSTANT DataAction     : IN VitalMemorySymbolType;
  CONSTANT CallerName     : IN STRING;
  CONSTANT PortName       : IN STRING := "";
  CONSTANT HeaderMsg      : IN STRING := "";
  CONSTANT MsgOn          : IN BOOLEAN := TRUE
) IS

  VARIABLE DataOutTmp : std_logic_vector(DataOutBus'RANGE)
                          := DataOutBus;

BEGIN

  -- Handle the data action
  CASE DataAction IS

  WHEN 'l'    =>
    -- Corrupting data out with 'X'
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrCrWrdOut,HeaderMsg,PortName);
    END IF;
    DataOutTmp := (OTHERS => 'X');
    -- No need to CorruptMask
    PortFlag.DataCurrent := CORRUPT;

  WHEN 'd'    =>
    -- Corrupting a single bit of data out with 'X'
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrCrBitOut,HeaderMsg,PortName);
    END IF;
    DataOutTmp(HighBit DOWNTO LowBit) := 
      DataOutTmp(HighBit DOWNTO LowBit) XOR 
      CorruptMask(HighBit DOWNTO LowBit);
    PortFlag.DataCurrent := CORRUPT;

  WHEN 'e'    =>
    -- Corrupting data out with 'X' based on data in
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrCrDatOut,HeaderMsg,PortName);
    END IF;
    ReadMemory(MemoryData,DataOutTmp,Address);
    IF (DataOutTmp /= DataInBus) THEN
      DataOutTmp := (OTHERS => 'X');
      -- No need to CorruptMask
    END IF;
    PortFlag.DataCurrent := CORRUPT;

  WHEN 'L'    =>
    -- Corrupting data out sub-word with 'X'
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrCrWrdSubOut,HeaderMsg,PortName);
    END IF;
    ReadMemory(MemoryData,DataOutTmp,Address);
    DataOutTmp(HighBit DOWNTO LowBit) := 
      DataOutTmp(HighBit DOWNTO LowBit) XOR 
      CorruptMask(HighBit DOWNTO LowBit);
    PortFlag.DataCurrent := CORRUPT;

  WHEN 'D'    =>
    -- Corrupting a single bit of data out sub-word with 'X'
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrCrBitSubOut,HeaderMsg,PortName);
    END IF;
    DataOutTmp(HighBit DOWNTO LowBit) := 
      DataOutTmp(HighBit DOWNTO LowBit) XOR 
      CorruptMask(HighBit DOWNTO LowBit);
    PortFlag.DataCurrent := CORRUPT;

  WHEN 'E'    =>
    -- Corrupting data out sub-word with 'X' based on data in
    IF (MsgOn) THEN
    PrintMemoryMessage(CallerName,ErrCrDatSubOut,HeaderMsg,PortName);
    END IF;
    ReadMemory(MemoryData,DataOutTmp,Address);
    IF (DataInBus(HighBit DOWNTO LowBit) /= 
        DataOutTmp(HighBit DOWNTO LowBit)) THEN
      DataOutTmp(HighBit DOWNTO LowBit) := (OTHERS => 'X');
      -- No need to CorruptMask
    END IF;
    PortFlag.DataCurrent := CORRUPT;

  WHEN 'M'    =>
    -- Implicit read from memory to data out
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrImplOut,HeaderMsg,PortName);
    END IF;
    PortFlag.DataCurrent := READ;

  WHEN 'm'    =>
    -- Reading data from memory to data out
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrReadOut,HeaderMsg,PortName);
    END IF;
    ReadMemory(MemoryData,DataOutTmp,Address);
    PortFlag.DataCurrent := READ;

  WHEN 't'    =>
    -- Transferring from data in to data out
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrAssgOut,HeaderMsg,PortName);
    END IF;
    DataOutTmp := DataInBus;
    PortFlag.DataCurrent := READ;

  WHEN '0'    =>
    -- Assigning low level to data out
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrAsg0Out,HeaderMsg,PortName);
    END IF;
    DataOutTmp := (OTHERS => '0');
    PortFlag.DataCurrent := READ;

  WHEN '1'    =>
    -- Assigning high level to data out
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrAsg1Out,HeaderMsg,PortName);
    END IF;
    DataOutTmp := (OTHERS => '1');
    PortFlag.DataCurrent := READ;

  WHEN 'Z'    =>
    -- Assigning high impedence to data out
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrAsgZOut,HeaderMsg,PortName);
    END IF;
    DataOutTmp := (OTHERS => 'Z');
    PortFlag.DataCurrent := HIGHZ;

  WHEN 'S'    =>
    -- Keeping data out at steady value
    PortFlag.OutputDisable := TRUE;
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrAsgSOut,HeaderMsg,PortName);
    END IF;

  WHEN OTHERS =>
    -- Unknown data action
    PortFlag.DataCurrent := UNDEF;
    IF (MsgOn) THEN
      PrintMemoryMessage(CallerName,ErrUnknDatDo,HeaderMsg,PortName);
    END IF;

  END CASE;

  DataOutBus(HighBit DOWNTO LowBit) := DataOutTmp(HighBit DOWNTO LowBit);

END;


-- ----------------------------------------------------------------------------
-- Memory Table Modeling Primitives
-- ----------------------------------------------------------------------------

-- ----------------------------------------------------------------------------
-- Procedure:   VitalDeclareMemory
-- Parameters:  NoOfWords          - Number of words in the memory
--              NoOfBitsPerWord    - Number of bits per word in memory
--              NoOfBitsPerSubWord - Number of bits per sub word
--              MemoryLoadFile     - Name of data file to load
-- Description: This function is intended to be used to initialize
--              memory data declarations, i.e. to be executed duing
--              simulation elaboration time.  Handles the allocation
--              and initialization of memory for the memory data.
--              Default NoOfBitsPerSubWord is NoOfBitsPerWord.
-- ----------------------------------------------------------------------------
IMPURE FUNCTION VitalDeclareMemory (
  CONSTANT NoOfWords          : IN POSITIVE;
  CONSTANT NoOfBitsPerWord    : IN POSITIVE;
  CONSTANT MemoryLoadFile     : IN string := "";
  CONSTANT BinaryLoadFile     : IN BOOLEAN := FALSE
) RETURN VitalMemoryDataType IS
  VARIABLE MemoryPtr      : VitalMemoryDataType;
BEGIN
  MemoryPtr := VitalDeclareMemory(
    NoOfWords           => NoOfWords,
    NoOfBitsPerWord     => NoOfBitsPerWord,
    NoOfBitsPerSubWord  => NoOfBitsPerWord,
    MemoryLoadFile      => MemoryLoadFile,
    BinaryLoadFile      => BinaryLoadFile
  );
  RETURN MemoryPtr;
END;

-- ----------------------------------------------------------------------------
IMPURE FUNCTION VitalDeclareMemory (
  CONSTANT NoOfWords          : IN POSITIVE;
  CONSTANT NoOfBitsPerWord    : IN POSITIVE;
  CONSTANT NoOfBitsPerSubWord : IN POSITIVE;
  CONSTANT MemoryLoadFile     : IN string := "";
  CONSTANT BinaryLoadFile     : IN BOOLEAN := FALSE
) RETURN VitalMemoryDataType IS
  VARIABLE MemoryPtr      : VitalMemoryDataType;
  VARIABLE BitsPerEnable  : NATURAL 
                            := ((NoOfBitsPerWord-1)
                                /NoOfBitsPerSubWord)+1;
BEGIN
  PrintMemoryMessage(MsgVDM,ErrInitMem);
  MemoryPtr := new VitalMemoryArrayRecType '( 
      NoOfWords           => NoOfWords,
      NoOfBitsPerWord     => NoOfBitsPerWord,
      NoOfBitsPerSubWord  => NoOfBitsPerSubWord,
      NoOfBitsPerEnable   => BitsPerEnable,
      MemoryArrayPtr      => NULL 
    );
  MemoryPtr.MemoryArrayPtr 
    := new MemoryArrayType (0 to MemoryPtr.NoOfWords - 1);
  FOR i IN 0 TO MemoryPtr.NoOfWords - 1 LOOP
    MemoryPtr.MemoryArrayPtr(i) 
      := new MemoryWordType (MemoryPtr.NoOfBitsPerWord - 1 DOWNTO 0);
  END LOOP;
  IF (MemoryLoadFile /= "") THEN
    LoadMemory (MemoryPtr, MemoryLoadFile, BinaryLoadFile);
  END IF;
  RETURN MemoryPtr;
END;

-- ----------------------------------------------------------------------------
-- Procedure:   VitalMemoryTable
-- Parameters:  DataOutBus     - Output candidate zero delay data bus out
--              MemoryData     - Pointer to memory data structure
--              PrevControls   - Previous data in for edge detection
--              PrevEnableBus  - Previous enables for edge detection
--              PrevDataInBus  - Previous data bus for edge detection
--              PrevAddressBus - Previous address bus for edge detection
--              PortFlag       - Indicates port operating mode
--              PortFlagArray  - Vector form of PortFlag for sub-word
--              Controls       - Agregate of scalar control lines
--              EnableBus      - Concatenation of vector control lines
--              DataInBus      - Input value of data bus in
--              AddressBus     - Input value of address bus in
--              AddressValue   - Decoded value of the AddressBus
--              MemoryTable    - Input memory action table
--              PortType       - The type of port (currently not used)
--              PortName       - Port name string for messages
--              HeaderMsg      - Header string for messages
--              MsgOn          - Control the generation of messages
--              MsgSeverity    - Control level of message generation
-- Description: This procedure implements the majority of the memory 
--              modeling functionality via lookup of the memory action
--              tables and performing the specified actions if matches
--              are found, or the default actions otherwise.  The
--              overloadings are provided for the word and sub-word
--              (using the EnableBus and PortFlagArray arguments) addressing
--              cases.
-- ----------------------------------------------------------------------------
PROCEDURE VitalMemoryTable (
  VARIABLE DataOutBus     : INOUT std_logic_vector;
  VARIABLE MemoryData     : INOUT VitalMemoryDataType;
  VARIABLE PrevControls   : INOUT std_logic_vector;
  VARIABLE PrevDataInBus  : INOUT std_logic_vector;
  VARIABLE PrevAddressBus : INOUT std_logic_vector;
  VARIABLE PortFlag       : INOUT VitalPortFlagVectorType;
  CONSTANT Controls       : IN std_logic_vector;
  CONSTANT DataInBus      : IN std_logic_vector;
  CONSTANT AddressBus     : IN std_logic_vector;
  VARIABLE AddressValue   : INOUT VitalAddressValueType;
  CONSTANT MemoryTable    : IN VitalMemoryTableType;
  CONSTANT PortType       : IN VitalPortType := UNDEF;
  CONSTANT PortName       : IN STRING := "";
  CONSTANT HeaderMsg      : IN STRING := "";
  CONSTANT MsgOn          : IN BOOLEAN := TRUE;
  CONSTANT MsgSeverity    : IN SEVERITY_LEVEL := WARNING
) IS

  VARIABLE DataOutTmp     : std_logic_vector(DataOutBus'RANGE)
                            := DataOutBus;
  VARIABLE MemoryAction   : VitalMemorySymbolType;
  VARIABLE DataAction     : VitalMemorySymbolType;
  VARIABLE HighBit        : NATURAL := MemoryData.NoOfBitsPerWord-1;
  VARIABLE LowBit         : NATURAL := 0;
  VARIABLE Address        : INTEGER := 0;
  VARIABLE PortFlagTmp    : VitalPortFlagType;
  VARIABLE AddrFlag       : VitalMemorySymbolType := 'g';  -- good addr
  VARIABLE DataFlag       : VitalMemorySymbolType := 'g';  -- good data
  VARIABLE MemCorruptMask : std_logic_vector (DataOutBus'RANGE);
  VARIABLE DatCorruptMask : std_logic_vector (DataOutBus'RANGE);

BEGIN

  -- Optimize for case when all current inputs are same as previous
  IF (PrevDataInBus = DataInBus 
      AND PrevAddressBus = AddressBus 
      AND PrevControls = Controls
      AND PortFlag(0).MemoryCurrent = PortFlag(0).MemoryPrevious
      AND PortFlag(0).DataCurrent = PortFlag(0).DataPrevious) THEN
    PortFlag(0).OutputDisable := TRUE;
    RETURN;
  END IF;

  PortFlag(0).DataPrevious := PortFlag(0).DataCurrent;
  PortFlag(0).MemoryPrevious := PortFlag(0).MemoryCurrent;
  PortFlag(0).OutputDisable := FALSE;
  PortFlagTmp := PortFlag(0);

  -- Convert address bus to integer value and table lookup flag
  DecodeAddress(
    Address         => Address             ,
    AddrFlag        => AddrFlag            ,
    MemoryData      => MemoryData          ,
    PrevAddressBus  => PrevAddressBus      ,
    AddressBus      => AddressBus          
  );

  -- Interpret data bus as a table lookup flag
  DecodeData (
    DataFlag        => DataFlag           ,
    PrevDataInBus   => PrevDataInBus      ,
    DataInBus       => DataInBus          ,
    HighBit         => HighBit            ,
    LowBit          => LowBit             
  );

  -- Lookup memory and data actions
  MemoryTableLookUp(  
    MemoryAction        => MemoryAction     ,
    DataAction          => DataAction       ,
    MemoryCorruptMask   => MemCorruptMask   ,
    DataCorruptMask     => DatCorruptMask   ,
    PrevControls        => PrevControls     ,
    Controls            => Controls         ,
    AddrFlag            => AddrFlag         ,
    DataFlag            => DataFlag         ,
    MemoryTable         => MemoryTable      ,
    PortName            => PortName         ,
    HeaderMsg           => HeaderMsg        ,
    MsgOn               => MsgOn            
  );

  -- Handle data action before memory action
  -- This allows reading previous memory contents
  HandleDataAction(
    DataOutBus      => DataOutTmp       ,
    MemoryData      => MemoryData       ,
    PortFlag        => PortFlagTmp      ,
    CorruptMask     => DatCorruptMask   ,
    DataInBus       => DataInBus        ,
    Address         => Address          ,
    HighBit         => HighBit          ,
    LowBit          => LowBit           ,
    MemoryTable     => MemoryTable      ,
    DataAction      => DataAction       ,
    CallerName      => MsgVMT           ,
    PortName        => PortName         ,
    HeaderMsg       => HeaderMsg        ,
    MsgOn           => MsgOn            
  );

  HandleMemoryAction(
    MemoryData      => MemoryData       ,
    PortFlag        => PortFlagTmp      ,
    CorruptMask     => MemCorruptMask   ,
    DataInBus       => DataInBus        ,
    Address         => Address          ,
    HighBit         => HighBit          ,
    LowBit          => LowBit           ,
    MemoryTable     => MemoryTable      ,
    MemoryAction    => MemoryAction     ,
    CallerName      => MsgVMT           ,
    PortName        => PortName         ,
    HeaderMsg       => HeaderMsg        ,
    MsgOn           => MsgOn            
  );

  -- Set the output PortFlag(0) value 
  IF (DataAction = 'S') THEN
    PortFlagTmp.OutputDisable := TRUE;
  END IF;
  IF (PortFlagTmp.DataCurrent = PortFlagTmp.DataPrevious
      AND PortFlagTmp.DataCurrent = HIGHZ) THEN
    PortFlagTmp.OutputDisable := TRUE;
  END IF;
  PortFlag(0) := PortFlagTmp;

  -- Set previous values for subsequent edge detection
  PrevControls := Controls;
  PrevDataInBus := DataInBus;
  PrevAddressBus := AddressBus;

  -- Set the candidate zero delay return value
  DataOutBus := DataOutTmp;

  -- Set the output AddressValue for VitalMemoryCrossPorts
  AddressValue := Address;

END VitalMemoryTable;

-- ----------------------------------------------------------------------------
PROCEDURE VitalMemoryTable (
  VARIABLE DataOutBus     : INOUT std_logic_vector;
  VARIABLE MemoryData     : INOUT VitalMemoryDataType;
  VARIABLE PrevControls   : INOUT std_logic_vector;
  VARIABLE PrevEnableBus  : INOUT std_logic_vector;
  VARIABLE PrevDataInBus  : INOUT std_logic_vector;
  VARIABLE PrevAddressBus : INOUT std_logic_vector;
  VARIABLE PortFlagArray  : INOUT VitalPortFlagVectorType;
  CONSTANT Controls       : IN std_logic_vector;
  CONSTANT EnableBus      : IN std_logic_vector;
  CONSTANT DataInBus      : IN std_logic_vector;
  CONSTANT AddressBus     : IN std_logic_vector;
  VARIABLE AddressValue   : INOUT VitalAddressValueType;
  CONSTANT MemoryTable    : IN VitalMemoryTableType;
  CONSTANT PortType       : IN VitalPortType := UNDEF;
  CONSTANT PortName       : IN STRING := "";
  CONSTANT HeaderMsg      : IN STRING := "";
  CONSTANT MsgOn          : IN BOOLEAN := TRUE;
  CONSTANT MsgSeverity    : IN SEVERITY_LEVEL := WARNING
) IS

  VARIABLE BitsPerWord    : NATURAL := MemoryData.NoOfBitsPerWord;
  VARIABLE BitsPerSubWord : NATURAL := MemoryData.NoOfBitsPerSubWord;
  VARIABLE BitsPerEnable  : NATURAL := MemoryData.NoOfBitsPerEnable;
  VARIABLE DataOutTmp     : std_logic_vector(DataOutBus'RANGE)
  := DataOutBus;
  VARIABLE MemoryAction   : VitalMemorySymbolType;
  VARIABLE DataAction     : VitalMemorySymbolType;
  VARIABLE HighBit        : NATURAL := BitsPerSubWord-1;
  VARIABLE LowBit         : NATURAL := 0;
  VARIABLE Address        : INTEGER := 0;
  VARIABLE PortFlagTmp    : VitalPortFlagType;
  VARIABLE AddrFlag       : VitalMemorySymbolType := 'g';  -- good addr
  VARIABLE DataFlag       : VitalMemorySymbolType := 'g';  -- good data
  VARIABLE MemCorruptMask : std_logic_vector (DataOutBus'RANGE);
  VARIABLE DatCorruptMask : std_logic_vector (DataOutBus'RANGE);

BEGIN

  -- Optimize for case when all current inputs are same as previous
  IF (PrevDataInBus = DataInBus 
      AND PrevAddressBus = AddressBus 
      AND PrevControls = Controls) THEN
    CheckFlags:
    FOR i IN 0 TO BitsPerEnable-1 LOOP
      IF (PortFlagArray(i).MemoryCurrent /= PortFlagArray(i).MemoryPrevious
          OR  PortFlagArray(i).DataCurrent /= PortFlagArray(i).DataPrevious) THEN
        EXIT CheckFlags;
      END IF;
      IF (i = BitsPerEnable-1) THEN
        FOR j IN 0 TO BitsPerEnable-1 LOOP
          PortFlagArray(j).OutputDisable := TRUE;
        END LOOP;
        RETURN;
      END IF;
    END LOOP;
  END IF;

  -- Convert address bus to integer value and table lookup flag
  DecodeAddress(
    Address         => Address,
    AddrFlag        => AddrFlag,
    MemoryData      => MemoryData,
    PrevAddressBus  => PrevAddressBus,
    AddressBus      => AddressBus     
  );

  -- Perform independent operations for each sub-word
  FOR i IN 0 TO BitsPerEnable-1 LOOP

    -- Set the output PortFlag(i) value
    PortFlagArray(i).DataPrevious := PortFlagArray(i).DataCurrent;
    PortFlagArray(i).MemoryPrevious := PortFlagArray(i).MemoryCurrent;
    PortFlagArray(i).OutputDisable := FALSE;
    PortFlagTmp := PortFlagArray(i);

    -- Interpret data bus as a table lookup flag
    DecodeData (
      DataFlag        => DataFlag         ,
      PrevDataInBus   => PrevDataInBus    ,
      DataInBus       => DataInBus        ,
      HighBit         => HighBit          ,
      LowBit          => LowBit           
    );

    -- Lookup memory and data actions
    MemoryTableLookUp(  
      MemoryAction        => MemoryAction     ,
      DataAction          => DataAction       ,
      MemoryCorruptMask   => MemCorruptMask   ,
      DataCorruptMask     => DatCorruptMask   ,
      PrevControls        => PrevControls     ,
      PrevEnableBus       => PrevEnableBus    ,
      Controls            => Controls         ,
      EnableBus           => EnableBus        ,
      EnableIndex         => i                ,
      BitsPerWord         => BitsPerWord      ,
      BitsPerSubWord      => BitsPerSubWord   ,
      BitsPerEnable       => BitsPerEnable    ,
      AddrFlag            => AddrFlag         ,
      DataFlag            => DataFlag         ,
      MemoryTable         => MemoryTable      ,
      PortName            => PortName         ,
      HeaderMsg           => HeaderMsg        ,
      MsgOn               => MsgOn            
    );

    -- Handle data action before memory action
    -- This allows reading previous memory contents
    HandleDataAction(
      DataOutBus      => DataOutTmp       ,
      MemoryData      => MemoryData       ,
      PortFlag        => PortFlagTmp      ,
      CorruptMask     => DatCorruptMask   ,
      DataInBus       => DataInBus        ,
      Address         => Address          ,
      HighBit         => HighBit          ,
      LowBit          => LowBit           ,
      MemoryTable     => MemoryTable      ,
      DataAction      => DataAction       ,
      CallerName      => MsgVMT           ,
      PortName        => PortName         ,
      HeaderMsg       => HeaderMsg        ,
      MsgOn           => MsgOn            
    );

    HandleMemoryAction(
      MemoryData      => MemoryData       ,
      PortFlag        => PortFlagTmp      ,
      CorruptMask     => MemCorruptMask   ,
      DataInBus       => DataInBus        ,
      Address         => Address          ,
      HighBit         => HighBit          ,
      LowBit          => LowBit           ,
      MemoryTable     => MemoryTable      ,
      MemoryAction    => MemoryAction     ,
      CallerName      => MsgVMT           ,
      PortName        => PortName         ,
      HeaderMsg       => HeaderMsg        ,
      MsgOn           => MsgOn            
    );

    -- Set the output PortFlag(i) value 
    IF (DataAction = 'S') THEN
      PortFlagTmp.OutputDisable := TRUE;
    END IF;
    IF (PortFlagTmp.DataCurrent = PortFlagTmp.DataPrevious
        AND PortFlagTmp.DataCurrent = HIGHZ) THEN
      PortFlagTmp.OutputDisable := TRUE;
    END IF;
    PortFlagArray(i) := PortFlagTmp;

    IF (i < BitsPerEnable-1) THEN
      -- Calculate HighBit and LowBit
      LowBit := LowBit + BitsPerSubWord;
      IF (LowBit > BitsPerWord) THEN
        LowBit := BitsPerWord;
      END IF;
      HighBit := LowBit + BitsPerSubWord;
      IF (HighBit > BitsPerWord) THEN
        HighBit := BitsPerWord;
      ELSE
        HighBit := HighBit - 1;
      END IF;
    END IF;

  END LOOP;

  -- Set previous values for subsequent edge detection
  PrevControls := Controls;
  PrevEnableBus := EnableBus;
  PrevDataInBus := DataInBus;
  PrevAddressBus := AddressBus;

  -- Set the candidate zero delay return value
  DataOutBus := DataOutTmp;

  -- Set the output AddressValue for VitalMemoryCrossPorts
  AddressValue := Address;

END VitalMemoryTable;

-- ----------------------------------------------------------------------------
-- Procedure:   VitalMemoryCrossPorts
-- Parameters:  DataOutBus     - Output candidate zero delay data bus out
--              MemoryData     - Pointer to memory data structure
--              SamePortFlag   - Operating mode for same port
--              SamePortAddressValue  - Operating modes for cross ports
--              CrossPortAddressArray - Decoded AddressBus for cross ports
--              CrossPortMode  - Write contention and crossport read control
--              PortName       - Port name string for messages
--              HeaderMsg      - Header string for messages
--              MsgOn          - Control the generation of messages
-- Description: These procedures control the effect of memory operations
--              on a given port due to operations on other ports in a
--              multi-port memory.
--              This includes data write through when reading and writing 
--              to the same address, as well as write contention when
--              there are multiple write to the same address.
--              If addresses do not match then data bus is unchanged. 
--              The DataOutBus can be diabled with 'Z' value.
--              If the WritePortFlag is 'CORRUPT', that would mean
--              that the whole memory is corrupted. So, for corrupting
--              the Read port, the Addresses need not be compared. 
--
--     CrossPortMode Enum            Description
-- 1.  CpRead                        Allows Cross Port Read Only
--                                   No contention checking.
-- 2.  WriteContention               Allows for write contention checks 
--                                   only between multiple write ports
-- 3.  ReadWriteContention           Allows contention between read and 
--                                   write ports. The action is to corrupt 
--                                   the memory and the output bus.
-- 4.  CpReadAndWriteContention      Is a combination of 1 & 2
-- 5.  CpReadAndReadContention       Allows contention between read and
--                                   write ports. The action is to corrupt 
--                                   the dataout bus only. The cp read is 
--                                   performed if not contending.
-- ----------------------------------------------------------------------------
PROCEDURE VitalMemoryCrossPorts (
  VARIABLE DataOutBus             : INOUT std_logic_vector;
  VARIABLE MemoryData             : INOUT VitalMemoryDataType;
  VARIABLE SamePortFlag           : INOUT VitalPortFlagVectorType;
  CONSTANT SamePortAddressValue   : IN VitalAddressValueType;
  CONSTANT CrossPortFlagArray     : IN VitalPortFlagVectorType;
  CONSTANT CrossPortAddressArray  : IN VitalAddressValueVectorType;
  CONSTANT CrossPortMode          : IN VitalCrossPortModeType 
                                        := CpReadAndWriteContention;
  CONSTANT PortName               : IN STRING := "";
  CONSTANT HeaderMsg              : IN STRING := "";
  CONSTANT MsgOn                  : IN BOOLEAN := TRUE
) IS

  VARIABLE BitsPerWord    : NATURAL := MemoryData.NoOfBitsPerWord;
  VARIABLE BitsPerSubWord : NATURAL := MemoryData.NoOfBitsPerSubWord;
  VARIABLE BitsPerEnable  : NATURAL := MemoryData.NoOfBitsPerEnable;
  VARIABLE DataOutTmp : std_logic_vector(DataOutBus'RANGE) := (OTHERS => 'Z');
  VARIABLE MemoryTmp  : std_logic_vector(DataOutBus'RANGE);
  VARIABLE CrossPorts : NATURAL := CrossPortAddressArray'LENGTH;
  VARIABLE LowBit     : NATURAL := 0;
  VARIABLE HighBit    : NATURAL := BitsPerSubWord-1;
  VARIABLE Address    : VitalAddressValueType := SamePortAddressValue;
  VARIABLE AddressJ   : VitalAddressValueType;
  VARIABLE AddressK   : VitalAddressValueType;
  VARIABLE PortFlagI  : VitalPortFlagType;
  VARIABLE PortFlagIJ : VitalPortFlagType;
  VARIABLE PortFlagIK : VitalPortFlagType;
  VARIABLE DoCpRead   : BOOLEAN := FALSE;
  VARIABLE DoWrCont   : BOOLEAN := FALSE;
  VARIABLE DoCpCont   : BOOLEAN := FALSE;
  VARIABLE DoRdWrCont : BOOLEAN := FALSE;
  VARIABLE CpWrCont   : BOOLEAN := FALSE;
  VARIABLE ModeWrCont : BOOLEAN := 
                          (CrossPortMode=WriteContention) OR  
                          (CrossPortMode=CpReadAndWriteContention);
  VARIABLE ModeCpRead : BOOLEAN := 
                          (CrossPortMode=CpRead) OR   
                          (CrossPortMode=CpReadAndWriteContention);
  VARIABLE ModeCpCont : BOOLEAN := (CrossPortMode=ReadWriteContention);
  VARIABLE ModeRdWrCont : BOOLEAN := (CrossPortMode=CpReadAndReadContention);

BEGIN

  -- Check for disabled port (i.e. OTHERS => 'Z')
  IF (DataOutBus = DataOutTmp) THEN
    RETURN;
  ELSE
    DataOutTmp := DataOutBus;
  END IF;

  -- Check for error in address
  IF (Address < 0) THEN
    RETURN;
  END IF;

  ReadMemory(MemoryData,MemoryTmp,Address);

  SubWordLoop: -- For each slice of the sub-word I
  FOR i IN 0 TO BitsPerEnable-1 LOOP
    PortFlagI := SamePortFlag(i);

    -- For each cross port J: check with same port address
    FOR j IN 0 TO CrossPorts-1 LOOP
      PortFlagIJ := CrossPortFlagArray(i+j*BitsPerEnable);
      AddressJ := CrossPortAddressArray(j);
      IF (AddressJ < 0) THEN
        NEXT;
      END IF;
      DoWrCont :=  (Address    = AddressJ)  AND
                   (ModeWrCont = TRUE)      AND
                  ((PortFlagI.MemoryCurrent  = WRITE)     OR 
                   (PortFlagI.MemoryCurrent  = CORRUPT))  AND
                  ((PortFlagIJ.MemoryCurrent = WRITE)     OR
                   (PortFlagIJ.MemoryCurrent = CORRUPT))  ;
      DoCpRead :=  (Address    = AddressJ)  AND
                   (ModeCpRead = TRUE)      AND
                  ((PortFlagI.MemoryCurrent  = READ)      OR 
                   (PortFlagI.OutputDisable  = TRUE))     AND 
                  ((PortFlagIJ.MemoryCurrent = WRITE)     OR
                   (PortFlagIJ.MemoryCurrent = CORRUPT))  ;
      DoCpCont :=  (Address    = AddressJ)  AND
                   (ModeCpCont = TRUE)      AND
                  ((PortFlagI.MemoryCurrent  = READ)      OR 
                   (PortFlagI.OutputDisable  = TRUE))     AND 
                  ((PortFlagIJ.MemoryCurrent = WRITE)     OR
                   (PortFlagIJ.MemoryCurrent = CORRUPT))  ;
      DoRdWrCont:= (Address    = AddressJ)  AND
                   (ModeRdWrCont = TRUE)    AND
                  ((PortFlagI.MemoryCurrent  = READ)      OR 
                   (PortFlagI.OutputDisable  = TRUE))     AND 
                  ((PortFlagIJ.MemoryCurrent = WRITE)     OR
                   (PortFlagIJ.MemoryCurrent = CORRUPT))  ;
      IF (DoWrCont OR DoCpCont) THEN
        -- Corrupt dataout and memory
        MemoryTmp(HighBit DOWNTO LowBit) := (OTHERS => 'X');
        DataOutTmp(HighBit DOWNTO LowBit) := (OTHERS => 'X');
        SamePortFlag(i).MemoryCurrent := CORRUPT;
        SamePortFlag(i).DataCurrent := CORRUPT;
        SamePortFlag(i).OutputDisable := FALSE;
        EXIT;
      END IF;
      IF (DoCpRead) THEN
        -- Update dataout with memory
        DataOutTmp(HighBit DOWNTO LowBit) :=
        MemoryTmp(HighBit DOWNTO LowBit);
        SamePortFlag(i).MemoryCurrent := READ;
        SamePortFlag(i).DataCurrent := READ;
        SamePortFlag(i).OutputDisable := FALSE;
        EXIT;
      END IF;
      IF (DoRdWrCont) THEN
        -- Corrupt dataout only
        DataOutTmp(HighBit DOWNTO LowBit) := (OTHERS => 'X');
        SamePortFlag(i).DataCurrent := CORRUPT;
        SamePortFlag(i).OutputDisable := FALSE;
        EXIT;
      END IF;
    END LOOP;

    IF (i < BitsPerEnable-1) THEN
      -- Calculate HighBit and LowBit
      LowBit := LowBit + BitsPerSubWord;
      IF (LowBit > BitsPerWord) THEN
        LowBit := BitsPerWord;
      END IF;
      HighBit := LowBit + BitsPerSubWord;
      IF (HighBit > BitsPerWord) THEN
        HighBit := BitsPerWord;
      ELSE
        HighBit := HighBit - 1;
      END IF;
    END IF;

  END LOOP;   -- SubWordLoop

  DataOutBus := DataOutTmp;

  IF (DoWrCont) THEN
    IF (MsgOn) THEN
      PrintMemoryMessage(MsgVMCP,ErrMcpWrCont,HeaderMsg,PortName);
    END IF;
    WriteMemory(MemoryData,MemoryTmp,Address);
  END IF;

  IF (DoCpCont) THEN
    IF (MsgOn) THEN
      PrintMemoryMessage(MsgVMCP,ErrMcpCpCont,HeaderMsg,PortName);
    END IF;
    WriteMemory(MemoryData,MemoryTmp,Address);
  END IF;

  IF (DoCpRead) THEN
    IF (MsgOn) THEN
      PrintMemoryMessage(MsgVMCP,ErrMcpCpRead,HeaderMsg,PortName);
    END IF;
  END IF;

  IF (DoRdWrCont) THEN
    IF (MsgOn) THEN
      PrintMemoryMessage(MsgVMCP,ErrMcpRdWrCo,HeaderMsg,PortName);
    END IF;
  END IF;

END VitalMemoryCrossPorts;

-- ----------------------------------------------------------------------------
PROCEDURE VitalMemoryCrossPorts (
  VARIABLE MemoryData             : INOUT VitalMemoryDataType;
  CONSTANT CrossPortFlagArray     : IN VitalPortFlagVectorType;
  CONSTANT CrossPortAddressArray  : IN VitalAddressValueVectorType;
  CONSTANT HeaderMsg              : IN STRING := "";
  CONSTANT MsgOn                  : IN BOOLEAN := TRUE
) IS

  VARIABLE BitsPerWord    : NATURAL := MemoryData.NoOfBitsPerWord;
  VARIABLE BitsPerSubWord : NATURAL := MemoryData.NoOfBitsPerSubWord;
  VARIABLE BitsPerEnable  : NATURAL := MemoryData.NoOfBitsPerEnable;
  VARIABLE MemoryTmp  : std_logic_vector(BitsPerWord-1 DOWNTO 0);
  VARIABLE CrossPorts : NATURAL := CrossPortAddressArray'LENGTH;
  VARIABLE LowBit     : NATURAL := 0;
  VARIABLE HighBit    : NATURAL := BitsPerSubWord-1;
  VARIABLE AddressJ   : VitalAddressValueType;
  VARIABLE AddressK   : VitalAddressValueType;
  VARIABLE PortFlagIJ : VitalPortFlagType;
  VARIABLE PortFlagIK : VitalPortFlagType;
  VARIABLE CpWrCont   : BOOLEAN := FALSE;

BEGIN

  SubWordLoop: -- For each slice of the sub-word I
  FOR i IN 0 TO BitsPerEnable-1 LOOP

    -- For each cross port J: check with each cross port K
    FOR j IN 0 TO CrossPorts-1 LOOP
      PortFlagIJ := CrossPortFlagArray(i+j*BitsPerEnable);
      AddressJ := CrossPortAddressArray(j);
      -- Check for error in address
      IF (AddressJ < 0) THEN
        NEXT;
      END IF;
      ReadMemory(MemoryData,MemoryTmp,AddressJ);
      -- For each cross port K
      FOR k IN 0 TO CrossPorts-1 LOOP
        IF (k <= j) THEN
          NEXT;
        END IF;
        PortFlagIK := CrossPortFlagArray(i+k*BitsPerEnable);
        AddressK := CrossPortAddressArray(k);
        -- Check for error in address
        IF (AddressK < 0) THEN
          NEXT;
        END IF;
        CpWrCont := ( (AddressJ   = AddressK) AND
                      (PortFlagIJ.MemoryCurrent = WRITE)    AND 
                      (PortFlagIK.MemoryCurrent = WRITE) ) OR
                    ( (PortFlagIJ.MemoryCurrent = WRITE)    AND
                      (PortFlagIK.MemoryCurrent = CORRUPT) ) OR
                    ( (PortFlagIJ.MemoryCurrent = CORRUPT) AND
                      (PortFlagIK.MemoryCurrent = WRITE) ) OR
                    ( (PortFlagIJ.MemoryCurrent = CORRUPT) AND
                      (PortFlagIK.MemoryCurrent = CORRUPT) ) ;
        IF (CpWrCont) THEN
          -- Corrupt memory only
          MemoryTmp(HighBit DOWNTO LowBit) := (OTHERS => 'X');
          EXIT;
        END IF;
      END LOOP;   -- FOR k IN 0 TO CrossPorts-1 LOOP
      IF (CpWrCont = TRUE) THEN
        IF (MsgOn) THEN
          PrintMemoryMessage(MsgVMCP,ErrMcpCpWrCont,HeaderMsg);
        END IF;
        WriteMemory(MemoryData,MemoryTmp,AddressJ);
      END IF;
    END LOOP;   -- FOR j IN 0 TO CrossPorts-1 LOOP

    IF (i < BitsPerEnable-1) THEN
      -- Calculate HighBit and LowBit
      LowBit := LowBit + BitsPerSubWord;
      IF (LowBit > BitsPerWord) THEN
        LowBit := BitsPerWord;
      END IF;
      HighBit := LowBit + BitsPerSubWord;
      IF (HighBit > BitsPerWord) THEN
        HighBit := BitsPerWord;
      ELSE
        HighBit := HighBit - 1;
      END IF;
    END IF;
  END LOOP;   -- SubWordLoop

END VitalMemoryCrossPorts;


-- ----------------------------------------------------------------------------
-- Procedure:   VitalMemoryViolation
-- Parameters:  DataOutBus        - Output zero delay data bus out
--              MemoryData        - Pointer to memory data structure
--              PortFlag          - Indicates port operating mode
--              TimingDataArray   - This is currently not used (comment out) 
--              ViolationArray    - Aggregation of violation variables
--              DataInBus         - Input value of data bus in
--              AddressBus        - Input value of address bus in
--              AddressValue      - Decoded value of the AddressBus
--              ViolationTable    - Input memory violation table
--              PortName          - Port name string for messages
--              HeaderMsg         - Header string for messages
--              MsgOn             - Control the generation of messages
--              MsgSeverity       - Control level of message generation
-- Description: This procedure is intended to implement all actions on the
--              memory contents and data out bus as a result of timing viols.
--              It uses the memory action table to perform various corruption
--              policies specified by the user. 
-- ----------------------------------------------------------------------------

PROCEDURE VitalMemoryViolation (
  VARIABLE DataOutBus           : INOUT std_logic_vector;
  VARIABLE MemoryData           : INOUT VitalMemoryDataType;
  VARIABLE PortFlag             : INOUT VitalPortFlagVectorType;
  CONSTANT DataInBus            : IN std_logic_vector;
  CONSTANT AddressValue         : IN VitalAddressValueType;
  CONSTANT ViolationFlags       : IN std_logic_vector;
  CONSTANT ViolationFlagsArray  : IN X01ArrayT;
  CONSTANT ViolationSizesArray  : IN VitalMemoryViolFlagSizeType;
  CONSTANT ViolationTable       : IN VitalMemoryTableType;
  CONSTANT PortType             : IN VitalPortType; 
  CONSTANT PortName             : IN STRING := "";
  CONSTANT HeaderMsg            : IN STRING := "";
  CONSTANT MsgOn                : IN BOOLEAN := TRUE;
  CONSTANT MsgSeverity          : IN SEVERITY_LEVEL := WARNING
) IS

  VARIABLE BitsPerWord    : NATURAL := MemoryData.NoOfBitsPerWord;
  VARIABLE BitsPerSubWord : NATURAL := MemoryData.NoOfBitsPerSubWord;
  VARIABLE BitsPerEnable  : NATURAL := MemoryData.NoOfBitsPerEnable;
  VARIABLE DataOutTmp     : std_logic_vector(DataOutBus'RANGE)
                            := DataOutBus;
  VARIABLE MemoryAction   : VitalMemorySymbolType;
  VARIABLE DataAction     : VitalMemorySymbolType;
  -- VMT relies on the corrupt masks so HighBit/LowBit are full word
  VARIABLE HighBit        : NATURAL := BitsPerWord-1;
  VARIABLE LowBit         : NATURAL := 0;
  VARIABLE PortFlagTmp    : VitalPortFlagType;
  VARIABLE VFlagArrayTmp  : std_logic_vector 
                            (0 TO ViolationFlagsArray'LENGTH-1);
  VARIABLE MemCorruptMask : std_logic_vector (DataOutBus'RANGE);
  VARIABLE DatCorruptMask : std_logic_vector (DataOutBus'RANGE);

BEGIN

  -- Don't do anything if given an error address
  IF (AddressValue < 0) THEN
    RETURN;
  END IF;

  FOR i IN ViolationFlagsArray'RANGE LOOP
    VFlagArrayTmp(i) := ViolationFlagsArray(i);
  END LOOP;

  -- Lookup memory and data actions
  ViolationTableLookUp(  
    MemoryAction        => MemoryAction         ,
    DataAction          => DataAction           ,
    MemoryCorruptMask   => MemCorruptMask       ,
    DataCorruptMask     => DatCorruptMask       ,
    ViolationFlags      => ViolationFlags       ,
    ViolationFlagsArray => VFlagArrayTmp        ,
    ViolationSizesArray => ViolationSizesArray  ,
    ViolationTable      => ViolationTable       ,
    BitsPerWord         => BitsPerWord          ,
    BitsPerSubWord      => BitsPerSubWord       ,
    BitsPerEnable       => BitsPerEnable        ,
    PortName            => PortName             ,
    HeaderMsg           => HeaderMsg            ,
    MsgOn               => MsgOn            
  );

  -- Need to read incoming PF value (was not before)
  PortFlagTmp := PortFlag(0);

  IF (PortType = READ OR PortType = RDNWR) THEN
    -- Handle data action before memory action
    -- This allows reading previous memory contents
    HandleDataAction(
      DataOutBus      => DataOutTmp       ,
      MemoryData      => MemoryData       ,
      PortFlag        => PortFlagTmp      ,
      CorruptMask     => DatCorruptMask   ,
      DataInBus       => DataInBus        ,
      Address         => AddressValue     ,
      HighBit         => HighBit          ,
      LowBit          => LowBit           ,
      MemoryTable     => ViolationTable   ,
      DataAction      => DataAction       ,
      CallerName      => MsgVMV           ,
      PortName        => PortName         ,
      HeaderMsg       => HeaderMsg        ,
      MsgOn           => MsgOn            
    );
  END IF;

  IF (PortType = WRITE OR PortType = RDNWR) THEN
    HandleMemoryAction(
      MemoryData      => MemoryData       ,
      PortFlag        => PortFlagTmp      ,
      CorruptMask     => MemCorruptMask   ,
      DataInBus       => DataInBus        ,
      Address         => AddressValue     ,
      HighBit         => HighBit          ,
      LowBit          => LowBit           ,
      MemoryTable     => ViolationTable   ,
      MemoryAction    => MemoryAction     ,
      CallerName      => MsgVMV           ,
      PortName        => PortName         ,
      HeaderMsg       => HeaderMsg        ,
      MsgOn           => MsgOn            
    );
  END IF;

  -- Check if we need to turn off PF.OutputDisable
  IF (DataAction /= 'S') THEN
    PortFlagTmp.OutputDisable := FALSE;
    -- Set the output PortFlag(0) value 
    -- Note that all bits of PortFlag get PortFlagTmp
    FOR i IN PortFlag'RANGE LOOP
      PortFlag(i) := PortFlagTmp;
    END LOOP;
  END IF;

  -- Set the candidate zero delay return value
  DataOutBus := DataOutTmp;

END;

PROCEDURE VitalMemoryViolation (
  VARIABLE DataOutBus           : INOUT std_logic_vector;
  VARIABLE MemoryData           : INOUT VitalMemoryDataType;
  VARIABLE PortFlag             : INOUT VitalPortFlagVectorType;
  CONSTANT DataInBus            : IN std_logic_vector;
  CONSTANT AddressValue         : IN VitalAddressValueType;
  CONSTANT ViolationFlags       : IN std_logic_vector;
  CONSTANT ViolationTable       : IN VitalMemoryTableType;
  CONSTANT PortType             : IN VitalPortType; 
  CONSTANT PortName             : IN STRING := "";
  CONSTANT HeaderMsg            : IN STRING := "";
  CONSTANT MsgOn                : IN BOOLEAN := TRUE;
  CONSTANT MsgSeverity          : IN SEVERITY_LEVEL := WARNING
) IS

  VARIABLE VFlagArrayTmp  : X01ArrayT (0 TO 0);

BEGIN

  VitalMemoryViolation (
    DataOutBus              => DataOutBus       ,
    MemoryData              => MemoryData       ,
    PortFlag                => PortFlag         ,
    DataInBus               => DataInBus        ,
    AddressValue            => AddressValue     ,
    ViolationFlags          => ViolationFlags   ,
    ViolationFlagsArray     => VFlagArrayTmp    ,
    ViolationSizesArray     => ( 0 => 0 )       ,
    ViolationTable          => ViolationTable   ,
    PortType                => PortType         ,
    PortName                => PortName         ,
    HeaderMsg               => HeaderMsg        ,
    MsgOn                   => MsgOn            ,
    MsgSeverity             => MsgSeverity
  );

END;

END Vital_Memory ;
