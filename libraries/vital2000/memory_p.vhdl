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
USE     IEEE.Vital_Timing.ALL;
USE     IEEE.Vital_Primitives.ALL;

LIBRARY STD;
USE     STD.TEXTIO.ALL;

PACKAGE Vital_Memory IS

-- ----------------------------------------------------------------------------
-- Timing Section
-- ----------------------------------------------------------------------------

-- ----------------------------------------------------------------------------
-- Types and constants for Memory timing procedures
-- ----------------------------------------------------------------------------
TYPE VitalMemoryArcType       IS (ParallelArc, CrossArc, SubwordArc);
TYPE OutputRetainBehaviorType IS (BitCorrupt, WordCorrupt);
TYPE VitalMemoryMsgFormatType IS (Vector, Scalar, VectorEnum);
TYPE X01ArrayT IS ARRAY (NATURAL RANGE <> ) OF X01;
TYPE X01ArrayPT IS ACCESS X01ArrayT;
TYPE VitalMemoryViolationType IS ACCESS X01ArrayT;
CONSTANT DefaultNumBitsPerSubword : INTEGER := -1;


-- Data type storing path delay and schedule information for output bits
TYPE VitalMemoryScheduleDataType IS RECORD
  OutputData        : std_ulogic;
  NumBitsPerSubWord : INTEGER;
  ScheduleTime      : TIME;
  ScheduleValue     : std_ulogic;
  LastOutputValue   : std_ulogic;
  PropDelay         : TIME;
  OutputRetainDelay : TIME; 
  InputAge          : TIME;
END RECORD;

TYPE VitalMemoryTimingDataType IS RECORD
  NotFirstFlag : BOOLEAN;
  RefLast      : X01;
  RefTime      : TIME;
  HoldEn       : BOOLEAN;
  TestLast     : std_ulogic;
  TestTime     : TIME;
  SetupEn      : BOOLEAN;
  TestLastA    : VitalLogicArrayPT;
  TestTimeA    : VitalTimeArrayPT;
  RefLastA     : X01ArrayPT; 
  RefTimeA     : VitalTimeArrayPT;
  HoldEnA      : VitalBoolArrayPT;
  SetupEnA     : VitalBoolArrayPT;
END RECORD;

TYPE VitalPeriodDataArrayType IS ARRAY (NATURAL RANGE <>) OF 
  VitalPeriodDataType;

-- Data type storing path delay and schedule information for output 
-- vectors
TYPE VitalMemoryScheduleDataVectorType IS ARRAY (NATURAL RANGE <> ) OF 
  VitalMemoryScheduleDataType;

-- VitalPortFlagType records runtime mode of port sub-word slices
--    TYPE VitalPortFlagType IS (
--        UNDEF,
--        READ,
--        WRITE,
--        CORRUPT,
--        HIGHZ,
--        NOCHANGE
--    );

-- VitalPortFlagType records runtime mode of port sub-word slices
TYPE VitalPortStateType IS (
  UNDEF,
  READ,
  WRITE,
  CORRUPT,
  HIGHZ
);

TYPE VitalPortFlagType IS RECORD
  MemoryCurrent   : VitalPortStateType;
  MemoryPrevious  : VitalPortStateType;
  DataCurrent     : VitalPortStateType;
  DataPrevious    : VitalPortStateType;
  OutputDisable   : BOOLEAN;
END RECORD;

CONSTANT VitalDefaultPortFlag : VitalPortFlagType := (
  MemoryCurrent   => READ,
  MemoryPrevious  => UNDEF,
  DataCurrent     => READ,
  DataPrevious    => UNDEF,
  OutputDisable   => FALSE
);

-- VitalPortFlagVectorType to be same width i as enables of a port
-- or j multiples thereof, where j is the number of cross ports
TYPE VitalPortFlagVectorType IS 
  ARRAY (NATURAL RANGE <>) OF VitalPortFlagType;

-- ----------------------------------------------------------------------------
-- Functions  : VitalMemory path delay procedures 
--                - VitalMemoryInitPathDelay
--                - VitalMemoryAddPathDelay
--                - VitalMemorySchedulePathDelay
--
-- Description: VitalMemoryInitPathDelay, VitalMemoryAddPathDelay and
--              VitalMemorySchedulePathDelay are Level 1 routines used 
--              for selecting the propagation delay paths based on
--              path condition, transition type and delay values and
--              schedule a new output value.
--
--              Following features are implemented in these procedures:
--              o condition dependent path selection
--              o Transition dependent delay selection
--              o shortest delay path selection from multiple 
--                candidate paths
--              o Scheduling of the computed values on the specified 
--                signal.
--              o output retain behavior if outputRetain flag is set
--              o output mapping to alternate strengths to model 
--                pull-up, pull-down etc. 
--                
--              <More details to be added here>
--
-- Following is information on overloading of the procedures.
--
-- VitalMemoryInitPathDelay is overloaded for ScheduleDataArray and 
-- OutputDataArray
--
-- ----------------------------------------------------------------------------
--           ScheduleDataArray        OutputDataArray
-- ----------------------------------------------------------------------------
--              Scalar                    Scalar
--              Vector                    Vector
-- ----------------------------------------------------------------------------
-- 
--
-- VitalMemoryAddPathDelay is overloaded for ScheduleDataArray,
-- PathDelayArray, InputSignal and delaytype.
--
-- ----------------------------------------------------------------------------
--   DelayType        InputSignal    ScheduleData     PathDelay
--                                      Array          Array
-- ----------------------------------------------------------------------------
--  VitalDelayType      Scalar         Scalar          Scalar
--  VitalDelayType      Scalar         Vector          Vector
--  VitalDelayType      Vector         Scalar          Vector      
--  VitalDelayType      Vector         Vector          Vector
--  VitalDelayType01    Scalar         Scalar          Scalar
--  VitalDelayType01    Scalar         Vector          Vector
--  VitalDelayType01    Vector         Scalar          Vector
--  VitalDelayType01    Vector         Vector          Vector
--  VitalDelayType01Z   Scalar         Scalar          Scalar
--  VitalDelayType01Z   Scalar         Vector          Vector
--  VitalDelayType01Z   Vector         Scalar          Vector
--  VitalDelayType01Z   Vector         Vector          Vector
--  VitalDelayType01XZ  Scalar         Scalar          Scalar
--  VitalDelayType01XZ  Scalar         Vector          Vector
--  VitalDelayType01XZ  Vector         Scalar          Vector
--  VitalDelayType01XZ  Vector         Vector          Vector
-- ----------------------------------------------------------------------------
--  
--
--  VitalMemorySchedulePathDelay is overloaded for ScheduleDataArray,
--  and OutSignal
--
-- ----------------------------------------------------------------------------
--           OutSignal                 ScheduleDataArray     
-- ----------------------------------------------------------------------------
--             Scalar                      Scalar
--             Vector                      Vector
-- ----------------------------------------------------------------------------
--
-- Procedure Declarations:
--
--
-- Function     :   VitalMemoryInitPathDelay
--
-- Arguments:
--
--  INOUT               Type             Description
--
--  ScheduleDataArray/  VitalMemoryScheduleDataVectorType/        
--  ScheduleData        VitalMemoryScheduleDataType   
--                                       Internal data variable for 
--                                       storing delay and schedule
--                                       information for each output bit
--                                            
--
--  IN
--
--  OutputDataArray/    STD_LOGIC_VECTOR/Array containing current output
--  OutputData          STD_ULOGIC       value
--
--
--  NumBitsPerSubWord   INTEGER          Number of bits per subword.
--                                       Default value of this argument
--                                       is DefaultNumBitsPerSubword 
--                                       which is interpreted as no 
--                                       subwords
--
-- ----------------------------------------------------------------------------
--
-- 
-- ScheduleDataArray - Vector
-- OutputDataArray - Vector
--
PROCEDURE VitalMemoryInitPathDelay (
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType;
  VARIABLE OutputDataArray   : IN STD_LOGIC_VECTOR;
  CONSTANT NumBitsPerSubWord : IN INTEGER := DefaultNumBitsPerSubword
);
-- 
-- ScheduleDataArray - Scalar
-- OutputDataArray - Scalar
--
PROCEDURE VitalMemoryInitPathDelay (
  VARIABLE ScheduleData      : INOUT VitalMemoryScheduleDataType;
  VARIABLE OutputData        : IN STD_ULOGIC
);

-- ----------------------------------------------------------------------------
--
-- Function     :   VitalMemoryAddPathDelay
--
-- Arguments
--
--  INOUT                 Type            Description
--
--  ScheduleDataArray/    VitalMemoryScheduleDataVectorType/          
--  ScheduleData          VitalMemoryScheduleDataType         
--                                        Internal data variable for 
--                                        storing delay and schedule
--                                        information for each output bit
--
--  InputChangeTimeArray/ VitaltimeArrayT/Time     
--  InputChangeTime                       Holds the time since the last 
--                                        input change
--
--  IN
--
--  InputSignal           STD_LOGIC_VECTOR
--                        STD_ULOGIC/     Array holding the input value
--
--  OutputSignalName      STRING          The output signal name
--
--  PathDelayArray/       VitalDelayArrayType01ZX, 
--  PathDelay             VitalDelayArrayType01Z,
--                        VitalDelayArrayType01,     
--                        VitalDelayArrayType/
--                        VitalDelayType01ZX,
--                        VitalDelayType01Z,
--                        VitalDelayType01,
--                        VitalDelayType  Array of delay values
--  
--  ArcType               VitalMemoryArcType        
--                                        Indicates the Path type. This
--                                        can be SubwordArc, CrossArc or
--                                        ParallelArc
--
--  PathCondition         BOOLEAN         If True, the transition in
--                                        the corresponding input signal
--                                        is considered while 
--                                        caluculating the prop. delay
--                                        else the transition is ignored.
--
--  OutputRetainFlag      BOOLEAN         If specified TRUE,output retain
--                                        (hold) behavior is implemented.
--
-- ----------------------------------------------------------------------------
--
-- #1
-- DelayType - VitalDelayType
-- Input     - Scalar 
-- Output    - Scalar 
-- Delay     - Scalar
-- Condition - Scalar

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleData         : INOUT VitalMemoryScheduleDataType;
  SIGNAL   InputSignal          : IN STD_ULOGIC;
  CONSTANT OutputSignalName     : IN STRING := "";
  VARIABLE InputChangeTime      : INOUT Time;
  CONSTANT PathDelay            : IN VitalDelayType;
  CONSTANT ArcType              : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathCondition        : IN BOOLEAN := TRUE
);

-- #2
-- DelayType - VitalDelayType
-- Input     - Scalar 
-- Output    - Vector 
-- Delay     - Vector 
-- Condition - Scalar

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_ULOGIC;
  CONSTANT OutputSignalName  : IN STRING := "";
  VARIABLE InputChangeTime   : INOUT Time;
  CONSTANT PathDelayArray    : IN VitalDelayArrayType;
  CONSTANT ArcType           : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathCondition     : IN BOOLEAN := TRUE
);

-- #3
-- DelayType - VitalDelayType
-- Input     - Scalar 
-- Output    - Vector 
-- Delay     - Vector 
-- Condition - Vector

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_ULOGIC;
  CONSTANT OutputSignalName  : IN STRING := "";
  VARIABLE InputChangeTime   : INOUT Time;
  CONSTANT PathDelayArray    : IN VitalDelayArrayType;
  CONSTANT ArcType           : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathConditionArray: IN VitalBoolArrayT
);

-- #4
-- DelayType - VitalDelayType
-- Input     - Vector
-- Output    - Scalar 
-- Delay     - Vector
-- Condition - Scalar

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleData         : INOUT VitalMemoryScheduleDataType;
  SIGNAL   InputSignal          : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName     : IN STRING := "";
  VARIABLE InputChangeTimeArray : INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray       : IN VitalDelayArrayType;
  CONSTANT ArcType              : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathCondition        : IN BOOLEAN := TRUE
);

-- #5
-- DelayType - VitalDelayType
-- Input     - Vector
-- Output    - Vector
-- Delay     - Vector
-- Condition - Scalar

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray    : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal          : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName     : IN STRING := "";
  VARIABLE InputChangeTimeArray : INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray       : IN VitalDelayArrayType;
  CONSTANT ArcType              : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathCondition        : IN BOOLEAN := TRUE
);

-- #6
-- DelayType - VitalDelayType
-- Input     - Vector
-- Output    - Vector
-- Delay     - Vector
-- Condition - Vector

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray    : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal          : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName     : IN STRING := "";
  VARIABLE InputChangeTimeArray : INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray       : IN VitalDelayArrayType;
  CONSTANT ArcType              : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathConditionArray   : IN VitalBoolArrayT
);

-- #7
-- DelayType - VitalDelayType01
-- Input     - Scalar
-- Output    - Scalar
-- Delay     - Scalar 
-- Condition - Scalar

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleData         : INOUT VitalMemoryScheduleDataType;
  SIGNAL   InputSignal          : IN STD_ULOGIC;
  CONSTANT OutputSignalName     : IN STRING := "";
  VARIABLE InputChangeTime      : INOUT Time;
  CONSTANT PathDelay            : IN VitalDelayType01;
  CONSTANT ArcType              : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathCondition        : IN BOOLEAN := TRUE
);

-- #8
-- DelayType - VitalDelayType01
-- Input     - Scalar
-- Output    - Vector
-- Delay     - Vector
-- Condition - Scalar

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray  : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_ULOGIC;
  CONSTANT OutputSignalName  : IN STRING := "";
  VARIABLE InputChangeTime   : INOUT Time;
  CONSTANT PathDelayArray    : IN VitalDelayArrayType01;
  CONSTANT ArcType           : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathCondition     : IN BOOLEAN := TRUE
);

-- #9
-- DelayType - VitalDelayType01
-- Input     - Scalar
-- Output    - Vector
-- Delay     - Vector
-- Condition - Vector

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray  : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_ULOGIC;
  CONSTANT OutputSignalName  : IN STRING := "";
  VARIABLE InputChangeTime   : INOUT Time;
  CONSTANT PathDelayArray    : IN VitalDelayArrayType01;
  CONSTANT ArcType           : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathConditionArray: IN VitalBoolArrayT
);

-- #10
-- DelayType - VitalDelayType01
-- Input     - Vector
-- Output    - Scalar
-- Delay     - Vector
-- Condition - Scalar

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleData         : INOUT VitalMemoryScheduleDataType;
  SIGNAL   InputSignal          : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName     : IN STRING := "";
  VARIABLE InputChangeTimeArray : INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray       : IN VitalDelayArrayType01;
  CONSTANT ArcType              : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathCondition        : IN BOOLEAN := TRUE
);

-- #11
-- DelayType - VitalDelayType01
-- Input     - Vector
-- Output    - Vector
-- Delay     - Vector
-- Condition - Scalar

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray  : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName  : IN STRING := "";
  VARIABLE InputChangeTimeArray : INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray       : IN VitalDelayArrayType01;
  CONSTANT ArcType              : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathCondition        : IN BOOLEAN := TRUE
);

-- #12
-- DelayType - VitalDelayType01
-- Input     - Vector
-- Output    - Vector
-- Delay     - Vector
-- Condition - Vector

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray  : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName  : IN STRING := "";
  VARIABLE InputChangeTimeArray : INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray       : IN VitalDelayArrayType01;
  CONSTANT ArcType              : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathConditionArray   : IN VitalBoolArrayT
);

-- #13
-- DelayType - VitalDelayType01Z
-- Input     - Scalar
-- Output    - Scalar
-- Delay     - Scalar
-- Condition - Scalar

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleData         : INOUT VitalMemoryScheduleDataType;
  SIGNAL   InputSignal          : IN STD_ULOGIC;
  CONSTANT OutputSignalName     : IN STRING := "";
  VARIABLE InputChangeTime      : INOUT Time;
  CONSTANT PathDelay            : IN VitalDelayType01Z;
  CONSTANT ArcType              : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathCondition        : IN BOOLEAN := TRUE;
  CONSTANT OutputRetainFlag     : IN BOOLEAN := FALSE
);

-- #14
-- DelayType - VitalDelayType01Z
-- Input     - Scalar
-- Output    - Vector
-- Delay     - Vector
-- Condition - Scalar

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_ULOGIC;
  CONSTANT OutputSignalName  : IN STRING := "";
  VARIABLE InputChangeTime   : INOUT Time;
  CONSTANT PathDelayArray    : IN VitalDelayArrayType01Z;
  CONSTANT ArcType           : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathCondition     : IN BOOLEAN := TRUE;
  CONSTANT OutputRetainFlag  : IN BOOLEAN := FALSE
);

-- #15
-- DelayType - VitalDelayType01Z
-- Input     - Scalar
-- Output    - Vector
-- Delay     - Vector
-- Condition - Vector

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_ULOGIC;
  CONSTANT OutputSignalName  : IN STRING := "";
  VARIABLE InputChangeTime   : INOUT Time;
  CONSTANT PathDelayArray    : IN VitalDelayArrayType01Z;
  CONSTANT ArcType           : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathConditionArray: IN VitalBoolArrayT;
  CONSTANT OutputRetainFlag  : IN BOOLEAN := FALSE
);

-- #16
-- DelayType - VitalDelayType01Z
-- Input     - Vector
-- Output    - Scalar
-- Delay     - Vector
-- Condition - Scalar

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleData         : INOUT VitalMemoryScheduleDataType;
  SIGNAL   InputSignal          : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName     : IN STRING := "";
  VARIABLE InputChangeTimeArray : INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray       : IN VitalDelayArrayType01Z;
  CONSTANT ArcType              : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathCondition        : IN BOOLEAN := TRUE;
  CONSTANT OutputRetainFlag     : IN BOOLEAN := FALSE;
  CONSTANT OutputRetainBehavior : IN OutputRetainBehaviorType := BitCorrupt
);

-- #17
-- DelayType - VitalDelayType01Z
-- Input     - Vector
-- Output    - Vector
-- Delay     - Vector 
-- Condition - Scalar

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName  : IN STRING := "";
  VARIABLE InputChangeTimeArray : INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray       : IN VitalDelayArrayType01Z;
  CONSTANT ArcType              : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathCondition        : IN BOOLEAN := TRUE;
  CONSTANT OutputRetainFlag     : IN BOOLEAN := FALSE;
  CONSTANT OutputRetainBehavior : IN OutputRetainBehaviorType := BitCorrupt
);

-- #18
-- DelayType - VitalDelayType01Z
-- Input     - Vector
-- Output    - Vector
-- Delay     - Vector 
-- Condition - Vector

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName  : IN STRING := "";
  VARIABLE InputChangeTimeArray : INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray       : IN VitalDelayArrayType01Z;
  CONSTANT ArcType              : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathConditionArray   : IN VitalBoolArrayT;
  CONSTANT OutputRetainFlag     : IN BOOLEAN := FALSE;
  CONSTANT OutputRetainBehavior : IN OutputRetainBehaviorType := BitCorrupt
);

-- #19
-- DelayType - VitalDelayType01ZX
-- Input     - Scalar
-- Output    - Scalar
-- Delay     - Scalar
-- Condition - Scalar

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleData         : INOUT VitalMemoryScheduleDataType;
  SIGNAL   InputSignal          : IN STD_ULOGIC;
  CONSTANT OutputSignalName     : IN STRING := "";
  VARIABLE InputChangeTime      : INOUT Time;
  CONSTANT PathDelay            : IN VitalDelayType01ZX;
  CONSTANT ArcType              : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathCondition        : IN BOOLEAN := TRUE;
  CONSTANT OutputRetainFlag     : IN BOOLEAN := FALSE
);

-- #20
-- DelayType - VitalDelayType01ZX
-- Input     - Scalar
-- Output    - Vector
-- Delay     - Vector
-- Condition - Scalar

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_ULOGIC;
  CONSTANT OutputSignalName  : IN STRING := "";
  VARIABLE InputChangeTime   : INOUT Time;
  CONSTANT PathDelayArray    : IN VitalDelayArrayType01ZX;
  CONSTANT ArcType           : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathCondition     : IN BOOLEAN := TRUE;
  CONSTANT OutputRetainFlag  : IN BOOLEAN := FALSE
);

-- #21
-- DelayType - VitalDelayType01ZX
-- Input     - Scalar
-- Output    - Vector
-- Delay     - Vector
-- Condition - Vector

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_ULOGIC;
  CONSTANT OutputSignalName  : IN STRING := "";
  VARIABLE InputChangeTime   : INOUT Time;
  CONSTANT PathDelayArray    : IN VitalDelayArrayType01ZX;
  CONSTANT ArcType           : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathConditionArray: IN VitalBoolArrayT;
  CONSTANT OutputRetainFlag  : IN BOOLEAN := FALSE
);

-- #22
-- DelayType - VitalDelayType01ZX
-- Input     - Vector
-- Output    - Scalar
-- Delay     - Vector
-- Condition - Scalar

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleData         : INOUT VitalMemoryScheduleDataType;
  SIGNAL   InputSignal          : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName     : IN STRING := "";
  VARIABLE InputChangeTimeArray : INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray       : IN VitalDelayArrayType01ZX;
  CONSTANT ArcType              : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathCondition        : IN BOOLEAN := TRUE;
  CONSTANT OutputRetainFlag     : IN BOOLEAN := FALSE;
  CONSTANT OutputRetainBehavior : IN OutputRetainBehaviorType := BitCorrupt
);

-- #23
-- DelayType - VitalDelayType01ZX
-- Input     - Vector
-- Output    - Vector
-- Delay     - Vector
-- Condition - Scalar

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName  : IN STRING := "";
  VARIABLE InputChangeTimeArray : INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray       : IN VitalDelayArrayType01ZX;
  CONSTANT ArcType              : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathCondition        : IN BOOLEAN := TRUE;
  CONSTANT OutputRetainFlag     : IN BOOLEAN := FALSE;
  CONSTANT OutputRetainBehavior : IN OutputRetainBehaviorType := BitCorrupt
);

-- #24
-- DelayType - VitalDelayType01ZX
-- Input     - Vector
-- Output    - Vector
-- Delay     - Vector
-- Condition - Vector

PROCEDURE VitalMemoryAddPathDelay (
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType;
  SIGNAL   InputSignal       : IN STD_LOGIC_VECTOR;
  CONSTANT OutputSignalName  : IN STRING := "";
  VARIABLE InputChangeTimeArray : INOUT VitalTimeArrayT;
  CONSTANT PathDelayArray       : IN VitalDelayArrayType01ZX;
  CONSTANT ArcType              : IN VitalMemoryArcType := CrossArc;
  CONSTANT PathConditionArray   : IN VitalBoolArrayT;
  CONSTANT OutputRetainFlag     : IN BOOLEAN := FALSE;
  CONSTANT OutputRetainBehavior : IN OutputRetainBehaviorType := BitCorrupt
);

-- ----------------------------------------------------------------------------
--
-- Function     :   VitalMemorySchedulePathDelay
--
-- Arguments:
--
--  OUT                Type                 Description
--  OutSignal          STD_LOGIC_VECTOR/    The output signal for 
--                     STD_ULOGIC           scheduling
--
--  IN
--  OutputSignalName   STRING               The name of the output signal
--
--  IN
--  PortFlag           VitalPortFlagType    Port flag variable from 
--                                          functional procedures
--
--  IN
--  OutputMap          VitalOutputMapType   For VitalPathDelay01Z, the 
--                                          output can be mapped to 
--                                          alternate strengths to model
--                                          tri-state devices, pull-ups
--                                          and pull-downs.
--
--  INOUT
--  ScheduleDataArray/ VitalMemoryScheduleDataVectorType/        
--  ScheduleData       VitalMemoryScheduleDataType           
--                                          Internal data variable for 
--                                          storing delay and schedule
--                                          information for each 
--                                          output bit
--                                       
-- ----------------------------------------------------------------------------
--
-- ScheduleDataArray - Vector
-- OutputSignal - Vector
--
PROCEDURE VitalMemorySchedulePathDelay (
  SIGNAL   OutSignal         : OUT std_logic_vector;
  CONSTANT OutputSignalName  : IN STRING := "";       
  CONSTANT PortFlag          : IN VitalPortFlagType := VitalDefaultPortFlag;
  CONSTANT OutputMap : IN VitalOutputMapType := VitalDefaultOutputMap;
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType
);
--
-- ScheduleDataArray - Vector
-- OutputSignal - Vector
--
PROCEDURE VitalMemorySchedulePathDelay (
  SIGNAL   OutSignal         : OUT std_logic_vector;
  CONSTANT OutputSignalName  : IN STRING := ""; 
  CONSTANT PortFlag          : IN VitalPortFlagVectorType;
  CONSTANT OutputMap : IN VitalOutputMapType := VitalDefaultOutputMap;
  VARIABLE ScheduleDataArray : INOUT VitalMemoryScheduleDataVectorType
);
--
-- ScheduleDataArray - Scalar
-- OutputSignal - Scalar
--
PROCEDURE VitalMemorySchedulePathDelay (
  SIGNAL   OutSignal         : OUT std_ulogic;
  CONSTANT OutputSignalName  : IN STRING := "";       
  CONSTANT PortFlag          : IN VitalPortFlagType := VitalDefaultPortFlag;
  CONSTANT OutputMap  : IN VitalOutputMapType := VitalDefaultOutputMap;
  VARIABLE ScheduleData      : INOUT VitalMemoryScheduleDataType
);

-- ----------------------------------------------------------------------------
FUNCTION VitalMemoryTimingDataInit RETURN VitalMemoryTimingDataType;

-- ----------------------------------------------------------------------------
--
-- Function Name:  VitalMemorySetupHoldCheck
--
-- Description:    The VitalMemorySetupHoldCheck procedure detects a setup or a 
--                 hold violation on the input test signal with respect
--                 to the corresponding input reference signal.  The timing 
--                 constraints are specified through parameters 
--                 representing the high and low values for the setup and
--                 hold values for the setup and hold times.  This 
--                 procedure assumes non-negative values for setup and hold 
--                 timing constraints. 
--
--                 It is assumed that negative timing constraints
--                 are handled by internally delaying the test or
--                 reference signals.  Negative setup times result in
--                 a delayed reference signal.  Negative hold times
--                 result in a delayed test signal.  Furthermore, the
--                 delays and constraints associated with these and
--                 other signals may need to be appropriately
--                 adjusted so that all constraint intervals overlap
--                 the delayed reference signals and all constraint
--                 values (with respect to the delayed signals) are
--                 non-negative.
--
--                 This function is overloaded based on the input
--                 TestSignal and reference signals. Parallel, Subword and
--                 Cross Arc relationships between test and reference
--                 signals are supported.
--                  
-- TestSignal XXXXXXXXXXXX____________________________XXXXXXXXXXXXXXXXXXXXXX
--            :
--            :        -->|       error region       |<--
--            :
--            _______________________________
-- RefSignal                                 \______________________________
--            :           |                  |       |
--            :           |               -->|       |<-- thold
--            :        -->|     tsetup       |<--
--
-- Arguments:         
--
--  IN                Type                Description    
--   TestSignal       std_logic_vector    Value of test signal
--   TestSignalName   STRING              Name of test signal
--   TestDelay        VitalDelayArrayType Model's internal delay associated
--                                        with TestSignal
--   RefSignal        std_ulogic          Value of reference signal
--                    std_logic_vector
--   RefSignalName    STRING              Name of reference signal
--   RefDelay         TIME                Model's internal delay associated
--                    VitalDelayArrayType with RefSignal
--   SetupHigh        VitalDelayArrayType Absolute minimum time duration 
--                                        before the transition of RefSignal
--                                        for which transitions of 
--                                        TestSignal are allowed to proceed
--                                        to the "1" state without causing
--                                        a setup violation.      
--   SetupLow         VitalDelayArrayType Absolute minimum time duration 
--                                        before the transition of RefSignal
--                                        for which transitions of 
--                                        TestSignal are allowed to proceed 
--                                        to the "0" state without causing
--                                        a setup violation.      
--   HoldHigh         VitalDelayArrayType Absolute minimum time duration 
--                                        after the transition of RefSignal
--                                        for which transitions of 
--                                        TestSignal are allowed to
--                                        proceed to the "1" state without
--                                        causing a hold violation.      
--   HoldLow          VitalDelayArrayType Absolute minimum time duration
--                                        after the transition of RefSignal
--                                        for which transitions of 
--                                        TestSignal are allowed to
--                                        proceed to the "0" state without
--                                        causing a hold violation.      
--   CheckEnabled     BOOLEAN             Check performed if TRUE.
--   RefTransition    VitalEdgeSymbolType
--                                        Reference edge specified. Events 
--                                        on the RefSignal which match the
--                                        edge spec. are used as reference 
--                                        edges.      
--   ArcType          VitalMemoryArcType
--   NumBitsPerSubWord INTEGER
--   HeaderMsg        STRING              String that will accompany any
--                                        assertion messages produced.      
--   XOn              BOOLEAN             If TRUE, Violation output 
--                                        parameter is set to "X". 
--                                        Otherwise, Violation is always
--                                        set to "0."      
--   MsgOn            BOOLEAN             If TRUE, set and hold violation 
--                                        message will be generated.
--                                        Otherwise, no messages are
--                                        generated, even upon violations.  
--   MsgSeverity      SEVERITY_LEVEL      Severity level for the assertion. 
--   MsgFormat        VitalMemoryMsgFormatType
--                                        Format of the Test/Reference 
--                                        signals in violation messages.
--              
--  INOUT
--   TimingData       VitalMemoryTimingDataType  
--                                        VitalMemorySetupHoldCheck information
--                                        storage area.  This is used
--                                        internally to detect reference 
--                                        edges and record the time of the 
--                                        last edge.
--
--  OUT
--   Violation        X01              This is the violation flag returned. 
--                    X01ArrayT        Overloaded for array type.
--
--
-- ----------------------------------------------------------------------------

PROCEDURE VitalMemorySetupHoldCheck (
  VARIABLE Violation         : OUT    X01ArrayT;
  VARIABLE TimingData        : INOUT  VitalMemoryTimingDataType;
  SIGNAL   TestSignal        : IN     std_ulogic;
  CONSTANT TestSignalName    : IN     STRING := "";
  CONSTANT TestDelay         : IN     TIME := 0 ns;
  SIGNAL   RefSignal         : IN     std_ulogic;
  CONSTANT RefSignalName     : IN     STRING := "";
  CONSTANT RefDelay          : IN     TIME := 0 ns;
  CONSTANT SetupHigh         : IN     VitalDelayType;
  CONSTANT SetupLow          : IN     VitalDelayType;
  CONSTANT HoldHigh          : IN     VitalDelayType;
  CONSTANT HoldLow           : IN     VitalDelayType;
  CONSTANT CheckEnabled      : IN     VitalBoolArrayT;
  CONSTANT RefTransition     : IN     VitalEdgeSymbolType;
  CONSTANT HeaderMsg         : IN     STRING := " ";
  CONSTANT XOn               : IN     BOOLEAN := TRUE;
  CONSTANT MsgOn             : IN     BOOLEAN := TRUE;
  CONSTANT MsgSeverity       : IN     SEVERITY_LEVEL := WARNING;
  CONSTANT EnableSetupOnTest : IN     BOOLEAN := TRUE;
  CONSTANT EnableSetupOnRef  : IN     BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnRef   : IN     BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnTest  : IN     BOOLEAN := TRUE  
); 

PROCEDURE VitalMemorySetupHoldCheck (
  VARIABLE Violation         : OUT    X01ArrayT;
  VARIABLE TimingData        : INOUT  VitalMemoryTimingDataType;
  SIGNAL   TestSignal        : IN     std_logic_vector;
  CONSTANT TestSignalName    : IN     STRING := "";
  CONSTANT TestDelay         : IN     VitalDelayArrayType;
  SIGNAL   RefSignal         : IN     std_ulogic;
  CONSTANT RefSignalName     : IN     STRING := "";
  CONSTANT RefDelay          : IN     TIME := 0 ns;
  CONSTANT SetupHigh         : IN     VitalDelayArrayType;
  CONSTANT SetupLow          : IN     VitalDelayArrayType;
  CONSTANT HoldHigh          : IN     VitalDelayArrayType;
  CONSTANT HoldLow           : IN     VitalDelayArrayType;
  CONSTANT CheckEnabled      : IN     BOOLEAN := TRUE;
  CONSTANT RefTransition     : IN     VitalEdgeSymbolType;
  CONSTANT HeaderMsg         : IN     STRING := " ";
  CONSTANT XOn               : IN     BOOLEAN := TRUE;
  CONSTANT MsgOn             : IN     BOOLEAN := TRUE;
  CONSTANT MsgSeverity       : IN     SEVERITY_LEVEL := WARNING;
  CONSTANT MsgFormat         : IN     VitalMemoryMsgFormatType;
  CONSTANT EnableSetupOnTest : IN     BOOLEAN := TRUE;
  CONSTANT EnableSetupOnRef  : IN     BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnRef   : IN     BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnTest  : IN     BOOLEAN := TRUE  
); 

PROCEDURE VitalMemorySetupHoldCheck (
  VARIABLE Violation         : OUT    X01ArrayT;
  VARIABLE TimingData        : INOUT  VitalMemoryTimingDataType;
  SIGNAL   TestSignal        : IN     std_logic_vector;
  CONSTANT TestSignalName    : IN     STRING := "";
  CONSTANT TestDelay         : IN     VitalDelayArrayType;
  SIGNAL   RefSignal         : IN     std_ulogic;
  CONSTANT RefSignalName     : IN     STRING := "";
  CONSTANT RefDelay          : IN     TIME := 0 ns;
  CONSTANT SetupHigh         : IN     VitalDelayArrayType;
  CONSTANT SetupLow          : IN     VitalDelayArrayType;
  CONSTANT HoldHigh          : IN     VitalDelayArrayType;
  CONSTANT HoldLow           : IN     VitalDelayArrayType;
  CONSTANT CheckEnabled      : IN     VitalBoolArrayT;
  CONSTANT RefTransition     : IN     VitalEdgeSymbolType;
  CONSTANT ArcType           : IN     VitalMemoryArcType := CrossArc;
  CONSTANT NumBitsPerSubWord : IN     INTEGER := 1;
  CONSTANT HeaderMsg         : IN     STRING := " ";
  CONSTANT XOn               : IN     BOOLEAN := TRUE;
  CONSTANT MsgOn             : IN     BOOLEAN := TRUE;
  CONSTANT MsgSeverity       : IN     SEVERITY_LEVEL := WARNING;
  CONSTANT MsgFormat         : IN     VitalMemoryMsgFormatType;
  CONSTANT EnableSetupOnTest : IN     BOOLEAN := TRUE;
  CONSTANT EnableSetupOnRef  : IN     BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnRef   : IN     BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnTest  : IN     BOOLEAN := TRUE  
); 

PROCEDURE VitalMemorySetupHoldCheck (
  VARIABLE Violation         : OUT    X01ArrayT;
  VARIABLE TimingData        : INOUT  VitalMemoryTimingDataType;            
  SIGNAL   TestSignal        : IN     std_logic_vector;
  CONSTANT TestSignalName    : IN     STRING := "";
  CONSTANT TestDelay         : IN     VitalDelayArrayType;
  SIGNAL   RefSignal         : IN     std_logic_vector;
  CONSTANT RefSignalName     : IN     STRING := "";
  CONSTANT RefDelay          : IN     VitalDelayArrayType;
  CONSTANT SetupHigh         : IN     VitalDelayArrayType;
  CONSTANT SetupLow          : IN     VitalDelayArrayType;
  CONSTANT HoldHigh          : IN     VitalDelayArrayType;
  CONSTANT HoldLow           : IN     VitalDelayArrayType;
  CONSTANT CheckEnabled      : IN     BOOLEAN := TRUE;
  CONSTANT RefTransition     : IN     VitalEdgeSymbolType;
  CONSTANT ArcType           : IN     VitalMemoryArcType := CrossArc;
  CONSTANT NumBitsPerSubWord : IN     INTEGER := 1;
  CONSTANT HeaderMsg         : IN     STRING := " ";
  CONSTANT XOn               : IN     BOOLEAN := TRUE;
  CONSTANT MsgOn             : IN     BOOLEAN := TRUE;
  CONSTANT MsgSeverity       : IN     SEVERITY_LEVEL := WARNING;
  CONSTANT MsgFormat         : IN     VitalMemoryMsgFormatType;
  CONSTANT EnableSetupOnTest : IN     BOOLEAN := TRUE; 
  CONSTANT EnableSetupOnRef  : IN     BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnRef   : IN     BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnTest  : IN     BOOLEAN := TRUE  
);     

PROCEDURE VitalMemorySetupHoldCheck (
  VARIABLE Violation         : OUT    X01ArrayT;
  VARIABLE TimingData        : INOUT  VitalMemoryTimingDataType;            
  SIGNAL   TestSignal        : IN     std_logic_vector;
  CONSTANT TestSignalName    : IN     STRING := "";
  CONSTANT TestDelay         : IN     VitalDelayArrayType;
  SIGNAL   RefSignal         : IN     std_logic_vector;
  CONSTANT RefSignalName     : IN     STRING := "";
  CONSTANT RefDelay          : IN     VitalDelayArrayType;
  CONSTANT SetupHigh         : IN     VitalDelayArrayType;
  CONSTANT SetupLow          : IN     VitalDelayArrayType;
  CONSTANT HoldHigh          : IN     VitalDelayArrayType;
  CONSTANT HoldLow           : IN     VitalDelayArrayType;
  CONSTANT CheckEnabled      : IN     VitalBoolArrayT;
  CONSTANT RefTransition     : IN     VitalEdgeSymbolType;
  CONSTANT ArcType           : IN     VitalMemoryArcType := CrossArc;
  CONSTANT NumBitsPerSubWord : IN     INTEGER := 1;
  CONSTANT HeaderMsg         : IN     STRING := " ";
  CONSTANT XOn               : IN     BOOLEAN := TRUE;
  CONSTANT MsgOn             : IN     BOOLEAN := TRUE;
  CONSTANT MsgSeverity       : IN     SEVERITY_LEVEL := WARNING;
  CONSTANT MsgFormat         : IN     VitalMemoryMsgFormatType;
  CONSTANT EnableSetupOnTest : IN     BOOLEAN := TRUE; 
  CONSTANT EnableSetupOnRef  : IN     BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnRef   : IN     BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnTest  : IN     BOOLEAN := TRUE  
);     

--------------- following are not needed --------------------------

PROCEDURE VitalMemorySetupHoldCheck (
  VARIABLE Violation         : OUT    X01;
  VARIABLE TimingData        : INOUT  VitalMemoryTimingDataType;
  SIGNAL   TestSignal        : IN     std_logic_vector;
  CONSTANT TestSignalName    : IN     STRING := "";
  CONSTANT TestDelay         : IN     VitalDelayArrayType;
  SIGNAL   RefSignal         : IN     std_ulogic;
  CONSTANT RefSignalName     : IN     STRING := "";
  CONSTANT RefDelay          : IN     TIME := 0 ns;
  CONSTANT SetupHigh         : IN     VitalDelayArrayType;
  CONSTANT SetupLow          : IN     VitalDelayArrayType;
  CONSTANT HoldHigh          : IN     VitalDelayArrayType;
  CONSTANT HoldLow           : IN     VitalDelayArrayType;
  CONSTANT CheckEnabled      : IN     BOOLEAN := TRUE;
  CONSTANT RefTransition     : IN     VitalEdgeSymbolType;
  CONSTANT HeaderMsg         : IN     STRING := " ";
  CONSTANT XOn               : IN     BOOLEAN := TRUE;
  CONSTANT MsgOn             : IN     BOOLEAN := TRUE;
  CONSTANT MsgSeverity       : IN     SEVERITY_LEVEL := WARNING;
  CONSTANT MsgFormat         : IN     VitalMemoryMsgFormatType;
  CONSTANT EnableSetupOnTest : IN     BOOLEAN := TRUE; 
  CONSTANT EnableSetupOnRef  : IN     BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnRef   : IN     BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnTest  : IN     BOOLEAN := TRUE  
);     

PROCEDURE VitalMemorySetupHoldCheck (
  VARIABLE Violation         : OUT    X01;
  VARIABLE TimingData        : INOUT  VitalMemoryTimingDataType;            
  SIGNAL   TestSignal        : IN     std_logic_vector;
  CONSTANT TestSignalName    : IN     STRING := "";
  CONSTANT TestDelay         : IN     VitalDelayArrayType;
  SIGNAL   RefSignal         : IN     std_logic_vector;
  CONSTANT RefSignalName     : IN     STRING := "";
  CONSTANT RefDelay          : IN     VitalDelayArrayType;
  CONSTANT SetupHigh         : IN     VitalDelayArrayType;
  CONSTANT SetupLow          : IN     VitalDelayArrayType;
  CONSTANT HoldHigh          : IN     VitalDelayArrayType;
  CONSTANT HoldLow           : IN     VitalDelayArrayType;
  CONSTANT CheckEnabled      : IN     BOOLEAN := TRUE;
  CONSTANT RefTransition     : IN     VitalEdgeSymbolType;
  CONSTANT HeaderMsg         : IN     STRING := " ";
  CONSTANT XOn               : IN     BOOLEAN := TRUE;
  CONSTANT MsgOn             : IN     BOOLEAN := TRUE;
  CONSTANT MsgSeverity       : IN     SEVERITY_LEVEL := WARNING;
  CONSTANT ArcType           : IN     VitalMemoryArcType := CrossArc;
  CONSTANT NumBitsPerSubWord : IN     INTEGER := 1;
  CONSTANT MsgFormat         : IN     VitalMemoryMsgFormatType;
  CONSTANT EnableSetupOnTest : IN     BOOLEAN := TRUE; 
  CONSTANT EnableSetupOnRef  : IN     BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnRef   : IN     BOOLEAN := TRUE; 
  CONSTANT EnableHoldOnTest  : IN     BOOLEAN := TRUE  
);         


-- ----------------------------------------------------------------------------
--
-- Function Name:  VitalPeriodPulseCheck
--
-- Description:    VitalPeriodPulseCheck checks for minimum and maximum
--                 periodicity and pulse width for "1" and "0" values of
--                 the input test signal.  The timing constraint is 
--                 specified through parameters representing the minimal
--                 period between successive rising and falling edges of
--                 the input test signal and the minimum pulse widths 
--                 associated with high and low values.  
--                 
--                 VitalPeriodCheck's accepts rising and falling edges
--                 from 1 and 0 as well as transitions to and from 'X.'
--                 
--                    _______________         __________
--       ____________|               |_______|
--
--                   |<--- pw_hi --->|
--                   |<-------- period ----->|
--                                -->| pw_lo |<--
--                 
-- Arguments:
--    IN             Type              Description
--    TestSignal     std_logic_vector  Value of test signal  
--    TestSignalName STRING            Name of the test signal
--    TestDelay      VitalDelayArrayType 
--                                     Model's internal delay associated
--                                     with TestSignal
--    Period         VitalDelayArrayType 
--                                     Minimum period allowed between
--                                     consecutive rising ('P') or 
--                                     falling ('F') transitions.
--    PulseWidthHigh VitalDelayArrayType 
--                                     Minimum time allowed for a high
--                                     pulse ('1' or 'H')
--    PulseWidthLow  VitalDelayArrayType 
--                                     Minimum time allowed for a low
--                                     pulse ('0' or 'L')
--    CheckEnabled   BOOLEAN           Check performed if TRUE.
--    HeaderMsg      STRING            String that will accompany any
--                                     assertion messages produced.   
--    XOn            BOOLEAN           If TRUE, Violation output parameter
--                                     is set to "X". Otherwise, Violation
--                                     is always set to "0." 
--    MsgOn          BOOLEAN           If TRUE, period/pulse violation
--                                     message will be generated.
--                                     Otherwise, no messages are generated,
--                                     even though a violation is detected. 
--    MsgSeverity    SEVERITY_LEVEL    Severity level for the assertion.
--    MsgFormat      VitalMemoryMsgFormatType
--                                     Format of the Test/Reference signals
--                                     in violation messages.
--         
--    INOUT
--    PeriodData     VitalPeriodDataArrayType 
--                                     VitalPeriodPulseCheck information
--                                     storage area.  This is used
--                                     internally to detect reference edges
--                                     and record the pulse and period
--                                     times.
--    OUT
--    Violation      X01               This is the violation flag returned. 
--                   X01ArrayT         Overloaded for array type.
--
-- ----------------------------------------------------------------------------
PROCEDURE  VitalMemoryPeriodPulseCheck  (
  VARIABLE Violation      : OUT   X01ArrayT; 
  VARIABLE PeriodData     : INOUT VitalPeriodDataArrayType; 
  SIGNAL   TestSignal     : IN    std_logic_vector;
  CONSTANT TestSignalName : IN    STRING := "";
  CONSTANT TestDelay      : IN    VitalDelayArrayType;
  CONSTANT Period         : IN    VitalDelayArrayType;
  CONSTANT PulseWidthHigh : IN    VitalDelayArrayType;
  CONSTANT PulseWidthLow  : IN    VitalDelayArrayType;
  CONSTANT CheckEnabled   : IN    BOOLEAN := TRUE;
  CONSTANT HeaderMsg      : IN    STRING := " ";
  CONSTANT XOn            : IN    BOOLEAN := TRUE;
  CONSTANT MsgOn          : IN    BOOLEAN := TRUE;
  CONSTANT MsgSeverity    : IN    SEVERITY_LEVEL := WARNING;
  CONSTANT MsgFormat      : IN    VitalMemoryMsgFormatType
);

PROCEDURE  VitalMemoryPeriodPulseCheck  (
  VARIABLE Violation      : OUT   X01; 
  VARIABLE PeriodData     : INOUT VitalPeriodDataArrayType;
  SIGNAL   TestSignal     : IN    std_logic_vector;
  CONSTANT TestSignalName : IN    STRING := "";
  CONSTANT TestDelay      : IN    VitalDelayArrayType;
  CONSTANT Period         : IN    VitalDelayArrayType;
  CONSTANT PulseWidthHigh : IN    VitalDelayArrayType;
  CONSTANT PulseWidthLow  : IN    VitalDelayArrayType;
  CONSTANT CheckEnabled   : IN    BOOLEAN := TRUE;
  CONSTANT HeaderMsg      : IN    STRING := " ";
  CONSTANT XOn            : IN    BOOLEAN := TRUE;
  CONSTANT MsgOn          : IN    BOOLEAN := TRUE;
  CONSTANT MsgSeverity    : IN    SEVERITY_LEVEL := WARNING;
  CONSTANT MsgFormat      : IN    VitalMemoryMsgFormatType
);

-- ----------------------------------------------------------------------------
-- Functionality Section
-- ----------------------------------------------------------------------------

-- ----------------------------------------------------------------------------
-- All Memory Types and Record definitions.
-- ----------------------------------------------------------------------------
TYPE MemoryWordType        IS ARRAY (NATURAL RANGE <>) OF UX01;
TYPE MemoryWordPtr         IS ACCESS MemoryWordType;

TYPE MemoryArrayType       IS ARRAY (NATURAL RANGE <>) OF MemoryWordPtr;
TYPE MemoryArrayPtrType    IS ACCESS MemoryArrayType;

TYPE VitalMemoryArrayRecType IS 
RECORD
NoOfWords           : POSITIVE;
NoOfBitsPerWord     : POSITIVE;
NoOfBitsPerSubWord  : POSITIVE;
NoOfBitsPerEnable   : POSITIVE;
MemoryArrayPtr      : MemoryArrayPtrType;
END RECORD;

TYPE VitalMemoryDataType    IS ACCESS VitalMemoryArrayRecType;

TYPE VitalTimingDataVectorType IS 
ARRAY (NATURAL RANGE <>) OF VitalTimingDataType;

TYPE VitalMemoryViolFlagSizeType IS ARRAY (NATURAL RANGE <>) OF INTEGER;

-- ----------------------------------------------------------------------------
-- Symbol Literals used for Memory Table Modeling
-- ----------------------------------------------------------------------------

-- Symbol literals from '/' to 'S' are closely related to MemoryTableMatch
-- lookup matching and the order cannot be arbitrarily changed.
-- The remaining symbol literals are interpreted directly and matchting is
-- handled in the MemoryMatch procedure itself.

TYPE VitalMemorySymbolType IS (
  '/',      -- 0 -> 1
  '\',      -- 1 -> 0
  'P',      -- Union of '/' and '^' (any edge to 1)
  'N',      -- Union of '\' and 'v' (any edge to 0)
  'r',      -- 0 -> X
  'f',      -- 1 -> X
  'p',      -- Union of '/' and 'r' (any edge from 0)
  'n',      -- Union of '\' and 'f' (any edge from 1)
  'R',      -- Union of '^' and 'p' (any possible rising edge)
  'F',      -- Union of 'v' and 'n' (any possible falling edge)
  '^',      -- X -> 1
  'v',      -- X -> 0
  'E',      -- Union of 'v' and '^' (any edge from X)
  'A',      -- Union of 'r' and '^' (rising edge to or from 'X')

  'D',      -- Union of 'f' and 'v' (falling edge to or from 'X')

  '*',      -- Union of 'R' and 'F' (any edge)
  'X',      -- Unknown level
  '0',      -- low level
  '1',      -- high level
  '-',      -- don't care
  'B',      -- 0 or 1
  'Z',      -- High Impedance
  'S',      -- steady value

  'g',      -- Good address (no transition)
  'u',      -- Unknown address (no transition)
  'i',      -- Invalid address (no transition)
  'G',      -- Good address (with transition)
  'U',      -- Unknown address (with transition)
  'I',      -- Invalid address (with transition)

  'w',      -- Write data to memory
  's',      -- Retain previous memory contents

  'c',      -- Corrupt entire memory with 'X'
  'l',      -- Corrupt a word in memory with 'X'
  'd',      -- Corrupt a single bit in memory with 'X'
  'e',      -- Corrupt a word with 'X' based on data in 
  'C',      -- Corrupt a sub-word entire memory with 'X' 
  'L',      -- Corrupt a sub-word in memory with 'X' 

  -- The following entries are commented since their 
  -- interpretation overlap with existing definitions.

  --      'D',      -- Corrupt a single bit of a sub-word with 'X'
  --      'E',      -- Corrupt a sub-word with 'X' based on datain

  'M',      -- Implicit read data from memory
  'm',      -- Read data from memory
  't'       -- Immediate assign/transfer data in

);

TYPE VitalMemoryTableType IS ARRAY ( NATURAL RANGE <>, NATURAL RANGE <> )
  OF VitalMemorySymbolType;

TYPE VitalMemoryViolationSymbolType IS (
  'X',      -- Unknown level
  '0',      -- low level
  '-'       -- don't care
);

TYPE VitalMemoryViolationTableType IS 
  ARRAY ( NATURAL RANGE <>, NATURAL RANGE <> )
  OF VitalMemoryViolationSymbolType;

TYPE VitalPortType IS (
  UNDEF,
  READ,
  WRITE,
  RDNWR
);

TYPE VitalCrossPortModeType IS (
  CpRead,                         --  CpReadOnly,
  WriteContention,                --  WrContOnly,
  ReadWriteContention,            --  CpContention
  CpReadAndWriteContention,       --  WrContAndCpRead,
  CpReadAndReadContention
);

SUBTYPE VitalAddressValueType IS INTEGER;
TYPE VitalAddressValueVectorType IS 
  ARRAY (NATURAL RANGE <>) OF VitalAddressValueType;

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
--              Default NoOfBitsPerSubWord is NoOfBits.
-- ----------------------------------------------------------------------------

IMPURE FUNCTION VitalDeclareMemory (
  CONSTANT NoOfWords          : IN POSITIVE;
  CONSTANT NoOfBitsPerWord    : IN POSITIVE;
  CONSTANT NoOfBitsPerSubWord : IN POSITIVE;
  CONSTANT MemoryLoadFile     : IN string := "";
  CONSTANT BinaryLoadFile     : IN BOOLEAN := FALSE
) RETURN VitalMemoryDataType;

IMPURE FUNCTION VitalDeclareMemory (
  CONSTANT NoOfWords          : IN POSITIVE;
  CONSTANT NoOfBitsPerWord    : IN POSITIVE;
  CONSTANT MemoryLoadFile     : IN string := "";
  CONSTANT BinaryLoadFile     : IN BOOLEAN := FALSE
) RETURN VitalMemoryDataType;


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
);

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
);

-- ----------------------------------------------------------------------------
-- Procedure:   VitalMemoryCrossPorts
-- Parameters:  DataOutBus     - Output candidate zero delay data bus out
--              MemoryData     - Pointer to memory data structure
--              SamePortFlag   - Operating mode for same port
--              SamePortAddressValue  - Decoded AddressBus for same port
--              CrossPortFlagArray    - Operating modes for cross ports
--              CrossPortAddressArray - Decoded AddressBus for cross ports
--              CrossPortMode  - Write contention and crossport read control
--              PortName       - Port name string for messages
--              HeaderMsg      - Header string for messages
--              MsgOn          - Control the generation of messages
--
-- Description: These procedures control the effect of memory operations
--              on a given port due to operations on other ports in a
--              multi-port memory.
--              This includes data write through when reading and writing 
--              to the same address, as well as write contention when
--              there are multiple write to the same address.
--              If addresses do not match then data bus is unchanged. 
--              The DataOutBus can be diabled with 'Z' value.
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
) ;

PROCEDURE VitalMemoryCrossPorts (
  VARIABLE MemoryData             : INOUT VitalMemoryDataType;
  CONSTANT CrossPortFlagArray     : IN VitalPortFlagVectorType;
  CONSTANT CrossPortAddressArray  : IN VitalAddressValueVectorType;
  CONSTANT HeaderMsg              : IN STRING := "";
  CONSTANT MsgOn                  : IN BOOLEAN := TRUE
) ;

-- ----------------------------------------------------------------------------
-- Procedure:   VitalMemoryViolation
-- Parameters:  DataOutBus          - Output zero delay data bus out
--              MemoryData          - Pointer to memory data structure
--              PortFlag            - Indicates port operating mode
--              DataInBus           - Input value of data bus in
--              AddressValue        - Decoded value of the AddressBus
--              ViolationFlags      - Aggregate of scalar violation vars
--              ViolationFlagsArray - Concatenation of vector violation vars
--              ViolationTable      - Input memory violation table
--              PortType            - The type of port (currently not used)
--              PortName            - Port name string for messages
--              HeaderMsg           - Header string for messages
--              MsgOn               - Control the generation of messages
--              MsgSeverity         - Control level of message generation
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
) ;

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
) ;

END Vital_Memory;
