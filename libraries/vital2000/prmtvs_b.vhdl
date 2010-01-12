-------------------------------------------------------------------------------
-- Title        : Standard VITAL_Primitives Package
--              : $Revision$
--              :
-- Library      : VITAL
--              :
-- Developers   : IEEE DASC Timing Working Group (TWG), PAR 1076.4
--              :
-- Purpose      : This packages defines standard types, constants, functions
--              : and procedures for use in developing ASIC models.
--              : Specifically a set of logic primitives are defined.
--              :  
-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------
-- Modification History : 
-- ----------------------------------------------------------------------------
-- Version No:|Auth:| Mod.Date:| Changes Made:
--   v95.0 A  |     | 06/02/95 | Initial ballot draft 1995
--   v95.1    |     | 08/31/95 | #204 - glitch detection prior to OutputMap
-- ----------------------------------------------------------------------------
--   v95.2    | ddl | 09/14/96 | #223 - single input prmtvs use on-detect
--            |     |          |        instead of glitch-on-event behavior
--   v95.3    | ddl | 09/24/96 | #236 - VitalTruthTable DataIn should be of
--            |     |          |        of class SIGNAL
--   v95.4    | ddl | 01/16/97 | #243 - index constraint error in nbit xor/xnor
--   v99.1    | dbb | 03/31/99 | Updated for VHDL 93
-- ----------------------------------------------------------------------------

LIBRARY STD;
USE STD.TEXTIO.ALL;

PACKAGE BODY VITAL_Primitives IS
    -- ------------------------------------------------------------------------
    --  Default values for Primitives
    -- ------------------------------------------------------------------------
    --  default values for delay parameters
    CONSTANT VitalDefDelay01  : VitalDelayType01  := VitalZeroDelay01;
    CONSTANT VitalDefDelay01Z : VitalDelayType01Z := VitalZeroDelay01Z;

    TYPE VitalTimeArray IS ARRAY (NATURAL RANGE <>) OF TIME;    

    --  default primitive model operation parameters
    --  Glitch detection/reporting
    TYPE VitalGlitchModeType IS ( MessagePlusX, MessageOnly, XOnly, NoGlitch);
    CONSTANT PrimGlitchMode : VitalGlitchModeType   := XOnly;

    -- ------------------------------------------------------------------------
    -- Local Type and Subtype Declarations
    -- ------------------------------------------------------------------------
    ---------------------------------------------------------------------------
    -- enumeration value representing the transition or level of the signal.
    --  See function 'GetEdge'
    ---------------------------------------------------------------------------
    TYPE EdgeType IS ( 'U',   -- Uninitialized level
                       'X',   -- Unknown level
                       '0',   -- low level
                       '1',   -- high level
                       '\',   -- 1 to 0 falling edge
                       '/',   -- 0 to 1 rising  edge
                       'F',   -- * to 0 falling edge
                       'R',   -- * to 1 rising  edge
                       'f',   -- rising  to X edge
                       'r',   -- falling to X edge
                       'x',   -- Unknown edge (ie U->X)
                       'V'    -- Timing violation edge
                     );
    TYPE EdgeArray  IS ARRAY ( NATURAL RANGE <> ) OF EdgeType;

    TYPE EdgeX1Table IS ARRAY ( EdgeType                          ) OF EdgeType;
    TYPE EdgeX2Table IS ARRAY ( EdgeType, EdgeType                ) OF EdgeType;
    TYPE EdgeX3Table IS ARRAY ( EdgeType, EdgeType, EdgeType      ) OF EdgeType;
    TYPE EdgeX4Table IS ARRAY (EdgeType,EdgeType,EdgeType,EdgeType) OF EdgeType;

    TYPE LogicToEdgeT  IS ARRAY(std_ulogic, std_ulogic) OF EdgeType;
    TYPE LogicToLevelT IS ARRAY(std_ulogic ) OF EdgeType;

    TYPE GlitchDataType IS 
      RECORD 
        SchedTime    : TIME; 
        GlitchTime   : TIME; 
        SchedValue   : std_ulogic;
        CurrentValue    : std_ulogic; 
      END RECORD; 
    TYPE GlitchDataArrayType IS ARRAY (NATURAL RANGE <>) 
         OF GlitchDataType;
 
    -- Enumerated type used in selection of output path delays
    TYPE SchedType  IS
      RECORD
        inp0  : TIME;   -- time (abs) of output change due to input change to 0
        inp1  : TIME;   -- time (abs) of output change due to input change to 1
        InpX  : TIME;   -- time (abs) of output change due to input change to X
        Glch0 : TIME;   -- time (abs) of output glitch due to input change to 0
        Glch1 : TIME;   -- time (abs) of output glitch due to input change to 0
      END RECORD;

    TYPE SchedArray  IS ARRAY ( NATURAL RANGE <> ) OF SchedType;
    CONSTANT DefSchedType : SchedType := (TIME'HIGH, TIME'HIGH, 0 ns,0 ns,0 ns);
    CONSTANT DefSchedAnd  : SchedType := (TIME'HIGH, 0 ns,0 ns, TIME'HIGH,0 ns);

    -- Constrained array declarations (common sizes used by primitives)
    SUBTYPE SchedArray2 IS SchedArray(1 DOWNTO 0);
    SUBTYPE SchedArray3 IS SchedArray(2 DOWNTO 0);
    SUBTYPE SchedArray4 IS SchedArray(3 DOWNTO 0);
    SUBTYPE SchedArray8 IS SchedArray(7 DOWNTO 0);

    SUBTYPE TimeArray2 IS VitalTimeArray(1 DOWNTO 0);
    SUBTYPE TimeArray3 IS VitalTimeArray(2 DOWNTO 0);
    SUBTYPE TimeArray4 IS VitalTimeArray(3 DOWNTO 0);
    SUBTYPE TimeArray8 IS VitalTimeArray(7 DOWNTO 0);

    SUBTYPE GlitchArray2 IS GlitchDataArrayType(1 DOWNTO 0);
    SUBTYPE GlitchArray3 IS GlitchDataArrayType(2 DOWNTO 0);
    SUBTYPE GlitchArray4 IS GlitchDataArrayType(3 DOWNTO 0);
    SUBTYPE GlitchArray8 IS GlitchDataArrayType(7 DOWNTO 0);

    SUBTYPE EdgeArray2 IS EdgeArray(1 DOWNTO 0);
    SUBTYPE EdgeArray3 IS EdgeArray(2 DOWNTO 0);
    SUBTYPE EdgeArray4 IS EdgeArray(3 DOWNTO 0);
    SUBTYPE EdgeArray8 IS EdgeArray(7 DOWNTO 0);

    CONSTANT DefSchedArray2 : SchedArray2 :=
                             (OTHERS=> (0 ns, 0 ns, 0 ns, 0 ns, 0 ns));

    TYPE stdlogic_table IS ARRAY(std_ulogic, std_ulogic) OF std_ulogic;

    CONSTANT InitialEdge : LogicToLevelT := (
            '1'|'H' => 'R',
            '0'|'L' => 'F',
            OTHERS  => 'x'
     );

    CONSTANT LogicToEdge  : LogicToEdgeT  := (  -- previous, current
    --  old \ new: U    X    0    1    Z    W    L    H    -
        'U' =>  ( 'U', 'x', 'F', 'R', 'x', 'x', 'F', 'R', 'x' ),
        'X' =>  ( 'x', 'X', 'F', 'R', 'x', 'X', 'F', 'R', 'X' ),
        '0' =>  ( 'r', 'r', '0', '/', 'r', 'r', '0', '/', 'r' ),
        '1' =>  ( 'f', 'f', '\', '1', 'f', 'f', '\', '1', 'f' ),
        'Z' =>  ( 'x', 'X', 'F', 'R', 'X', 'x', 'F', 'R', 'x' ),
        'W' =>  ( 'x', 'X', 'F', 'R', 'x', 'X', 'F', 'R', 'X' ),
        'L' =>  ( 'r', 'r', '0', '/', 'r', 'r', '0', '/', 'r' ),
        'H' =>  ( 'f', 'f', '\', '1', 'f', 'f', '\', '1', 'f' ),
        '-' =>  ( 'x', 'X', 'F', 'R', 'x', 'X', 'F', 'R', 'X' )
    );
    CONSTANT LogicToLevel : LogicToLevelT := (
            '1'|'H' => '1',
            '0'|'L' => '0',
            'U'     => 'U',
            OTHERS  => 'X'
     );

    -- -----------------------------------
    -- 3-state logic tables
    -- -----------------------------------
    CONSTANT BufIf0_Table : stdlogic_table :=
        -- enable        data       value
        ( '1'|'H'   => ( OTHERS  => 'Z' ),
          '0'|'L'   => ( '1'|'H' => '1',
                         '0'|'L' => '0',
                         'U'     => 'U',
                         OTHERS  => 'X' ),
          'U'       => ( OTHERS  => 'U' ),
          OTHERS    => ( OTHERS  => 'X' ) );
    CONSTANT BufIf1_Table : stdlogic_table :=
        -- enable        data       value
        ( '0'|'L'   => ( OTHERS  => 'Z' ),
          '1'|'H'   => ( '1'|'H' => '1',
                         '0'|'L' => '0',
                         'U'     => 'U',
                         OTHERS  => 'X' ),
          'U'       => ( OTHERS  => 'U' ),
          OTHERS    => ( OTHERS  => 'X' ) );
    CONSTANT InvIf0_Table : stdlogic_table :=
        -- enable        data       value
        ( '1'|'H'   => ( OTHERS  => 'Z' ),
          '0'|'L'   => ( '1'|'H' => '0',
                         '0'|'L' => '1',
                         'U'     => 'U',
                         OTHERS  => 'X' ),
          'U'       => ( OTHERS  => 'U' ),
          OTHERS    => ( OTHERS  => 'X' ) );
    CONSTANT InvIf1_Table : stdlogic_table :=
        -- enable        data       value
        ( '0'|'L'   => ( OTHERS  => 'Z' ),
          '1'|'H'   => ( '1'|'H' => '0',
                         '0'|'L' => '1',
                         'U'     => 'U',
                         OTHERS  => 'X' ),
          'U'       => ( OTHERS  => 'U' ),
          OTHERS    => ( OTHERS  => 'X' ) );


    TYPE To_StateCharType IS ARRAY (VitalStateSymbolType) OF CHARACTER;
    CONSTANT To_StateChar : To_StateCharType :=
     ( '/', '\', 'P', 'N', 'r', 'f', 'p', 'n', 'R', 'F', '^', 'v',
       'E', 'A', 'D', '*', 'X', '0', '1', '-', 'B', 'Z', 'S' );
    TYPE To_TruthCharType IS ARRAY (VitalTruthSymbolType) OF CHARACTER;
    CONSTANT To_TruthChar : To_TruthCharType :=
     ( 'X', '0', '1', '-', 'B', 'Z' );

    TYPE TruthTableOutMapType IS ARRAY (VitalTruthSymbolType) OF std_ulogic;
    CONSTANT TruthTableOutMap : TruthTableOutMapType :=
       --  'X', '0', '1', '-', 'B', 'Z'
         ( 'X', '0', '1', 'X', '-', 'Z' );

    TYPE StateTableOutMapType IS ARRAY (VitalStateSymbolType) OF std_ulogic;
    -- does conversion to X01Z or '-' if invalid
    CONSTANT StateTableOutMap : StateTableOutMapType :=
     -- '/' '\' 'P' 'N' 'r' 'f' 'p' 'n' 'R' 'F' '^' 'v'
     -- 'E' 'A' 'D' '*' 'X' '0' '1' '-' 'B' 'Z' 'S'
      ( '-','-','-','-','-','-','-','-','-','-','-','-',
        '-','-','-','-','X','0','1','X','-','Z','W');

    -- ------------------------------------------------------------------------
    TYPE ValidTruthTableInputType IS ARRAY (VitalTruthSymbolType) OF BOOLEAN;
    -- checks if a symbol IS valid for the stimulus portion of a truth table
    CONSTANT ValidTruthTableInput : ValidTruthTableInputType :=
       -- 'X'    '0'    '1'    '-'    'B'    'Z'
       (  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  FALSE );

    TYPE TruthTableMatchType IS ARRAY (X01, VitalTruthSymbolType) OF BOOLEAN;
    -- checks if an input matches th corresponding truth table symbol
    -- use: TruthTableMatch(input_converted_to_X01, truth_table_stimulus_symbol)
    CONSTANT TruthTableMatch : TruthTableMatchType  :=  (
       -- X,     0,     1,     -      B      Z
       (  TRUE,  FALSE, FALSE, TRUE,  FALSE, FALSE  ),  -- X
       (  FALSE, TRUE,  FALSE, TRUE,  TRUE,  FALSE  ),  -- 0
       (  FALSE, FALSE, TRUE,  TRUE,  TRUE,  FALSE  )   -- 1
    );

    -- ------------------------------------------------------------------------
    TYPE ValidStateTableInputType IS ARRAY (VitalStateSymbolType) OF BOOLEAN;
    CONSTANT ValidStateTableInput : ValidStateTableInputType :=
       -- '/',   '\',   'P',   'N',   'r',   'f',
      (   TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
       -- 'p',   'n',   'R',   'F',   '^',   'v',
          TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
       -- 'E',   'A',    'D',  '*',
          TRUE,  TRUE,  TRUE,  TRUE,
       -- 'X',   '0',   '1',   '-',   'B',   'Z',
          TRUE,  TRUE,  TRUE,  TRUE,  TRUE, FALSE,
       -- 'S'
          TRUE );

    CONSTANT ValidStateTableState : ValidStateTableInputType :=
       -- '/',   '\',   'P',   'N',   'r',   'f',
      (   FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
       -- 'p',   'n',   'R',   'F',   '^',   'v',
          FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
       -- 'E',   'A',    'D',  '*',
          FALSE, FALSE, FALSE, FALSE,
       -- 'X',   '0',   '1',   '-',   'B',   'Z',
          TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  FALSE,
       -- 'S'
          FALSE );

    TYPE StateTableMatchType IS ARRAY (X01,X01,VitalStateSymbolType) OF BOOLEAN;
    -- last value, present value, table symbol
    CONSTANT StateTableMatch : StateTableMatchType :=  (
      ( -- X (lastvalue)
     -- /     \     P     N     r     f
     -- p     n     R     F     ^     v
     -- E     A     D     *
     -- X     0     1     -     B     Z     S
      (FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
       FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
       FALSE,FALSE,FALSE,FALSE,
       TRUE, FALSE,FALSE,TRUE, FALSE,FALSE,FALSE),
      (FALSE,FALSE,FALSE,TRUE, FALSE,FALSE,
       FALSE,FALSE,FALSE,TRUE, FALSE,TRUE,
       TRUE, FALSE,TRUE, TRUE,
       FALSE,TRUE, FALSE,TRUE, TRUE, FALSE,FALSE),
      (FALSE,FALSE,TRUE, FALSE,FALSE,FALSE,
       FALSE,FALSE,TRUE, FALSE,TRUE, FALSE,
       TRUE, TRUE, FALSE,TRUE,
       FALSE,FALSE,TRUE, TRUE, TRUE, FALSE,FALSE)
      ),

      (-- 0 (lastvalue)
     -- /     \     P     N     r     f
     -- p     n     R     F     ^     v
     -- E     A     D     *
     -- X     0     1     -     B     Z     S
      (FALSE,FALSE,FALSE,FALSE,TRUE, FALSE,
       TRUE, FALSE,TRUE, FALSE,FALSE,FALSE,
       FALSE,TRUE, FALSE,TRUE,
       TRUE, FALSE,FALSE,TRUE, FALSE,FALSE,FALSE),
      (FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
       FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
       FALSE,FALSE,FALSE,FALSE,
       FALSE,TRUE, FALSE,TRUE, TRUE, FALSE,TRUE ),
      (TRUE, FALSE,TRUE, FALSE,FALSE,FALSE,
       TRUE, FALSE,TRUE, FALSE,FALSE,FALSE,
       FALSE,FALSE,FALSE,TRUE,
       FALSE,FALSE,TRUE, TRUE, TRUE, FALSE,FALSE)
      ),

      (-- 1 (lastvalue)
     -- /     \     P     N     r     f
     -- p     n     R     F     ^     v
     -- E     A     D     *
     -- X     0     1     -     B     Z     S
      (FALSE,FALSE,FALSE,FALSE,FALSE,TRUE ,
       FALSE,TRUE, FALSE,TRUE, FALSE,FALSE,
       FALSE,FALSE,TRUE, TRUE,
       TRUE, FALSE,FALSE,TRUE, FALSE,FALSE,FALSE),
      (FALSE,TRUE, FALSE,TRUE, FALSE,FALSE,
       FALSE,TRUE, FALSE,TRUE, FALSE,FALSE,
       FALSE,FALSE,FALSE,TRUE,
       FALSE,TRUE, FALSE,TRUE, TRUE, FALSE,FALSE),
      (FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
       FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
       FALSE,FALSE,FALSE,FALSE,
       FALSE,FALSE,TRUE, TRUE, TRUE, FALSE,TRUE )
      )
      );

    TYPE Logic_UX01Z_Table IS ARRAY (std_ulogic) OF UX01Z;
    ----------------------------------------------------------
    -- table name : cvt_to_x01z
    -- parameters :  std_ulogic  -- some logic value
    -- returns    :  UX01Z       -- state value of logic value
    -- purpose    :  to convert state-strength to state only
    ----------------------------------------------------------
    CONSTANT cvt_to_ux01z : Logic_UX01Z_Table :=
                                ('U','X','0','1','Z','X','0','1','X' );

    TYPE LogicCvtTableType IS ARRAY (std_ulogic) OF CHARACTER; 
    CONSTANT LogicCvtTable : LogicCvtTableType 
                     := ( 'U', 'X', '0', '1', 'Z', 'W', 'L', 'H', '-'); 

    --------------------------------------------------------------------
    -- LOCAL Utilities
    --------------------------------------------------------------------
    -- ------------------------------------------------------------------------
    --  FUNCTION  NAME  :  MINIMUM
    --
    --  PARAMETERS      :  in1, in2  - integer, time
    --
    --  DESCRIPTION     :  return smaller of in1 and in2
    -- ------------------------------------------------------------------------
    FUNCTION Minimum (
            CONSTANT in1, in2 : INTEGER
          ) RETURN INTEGER IS
    BEGIN
       IF (in1 < in2) THEN
          RETURN in1;
       END IF;
       RETURN in2;
    END;
    -- ------------------------------------------------------------------------
    FUNCTION Minimum (
            CONSTANT t1,t2 : IN TIME
          ) RETURN TIME IS
    BEGIN
        IF ( t1 < t2 ) THEN RETURN (t1); ELSE RETURN (t2); END IF;
    END Minimum;

    -- ------------------------------------------------------------------------
    --  FUNCTION  NAME  :  MAXIMUM
    --
    --  PARAMETERS      :  in1, in2  - integer, time
    --
    --  DESCRIPTION     :  return larger of in1 and in2
    -- ------------------------------------------------------------------------
    FUNCTION Maximum (
            CONSTANT in1, in2 : INTEGER
          ) RETURN INTEGER IS
    BEGIN
       IF (in1 > in2) THEN
          RETURN in1;
       END IF;
       RETURN in2;
    END;
    -----------------------------------------------------------------------
    FUNCTION Maximum (
            CONSTANT t1,t2 : IN TIME
          ) RETURN TIME IS
    BEGIN
        IF ( t1 > t2 ) THEN RETURN (t1); ELSE RETURN (t2); END IF;
    END Maximum;

    -----------------------------------------------------------------------
    FUNCTION GlitchMinTime (
            CONSTANT Time1, Time2 : IN TIME
          ) RETURN TIME IS
    BEGIN
        IF ( Time1 >= NOW ) THEN
                IF ( Time2 >= NOW ) THEN
                  RETURN Minimum ( Time1, Time2);
                ELSE
                  RETURN Time1;
                END IF;
        ELSE
                IF ( Time2 >= NOW ) THEN
                   RETURN Time2;
                ELSE
                   RETURN 0 ns;
                END IF;
        END IF;
    END;

    --------------------------------------------------------------------
    -- Error Message Types and Tables
    --------------------------------------------------------------------
    TYPE VitalErrorType IS (
        ErrNegDel,
        ErrInpSym,
        ErrOutSym,
        ErrStaSym,
        ErrVctLng,
        ErrTabWidSml,
        ErrTabWidLrg,
        ErrTabResSml,
        ErrTabResLrg
    );

    TYPE VitalErrorSeverityType IS ARRAY (VitalErrorType) OF SEVERITY_LEVEL;
    CONSTANT VitalErrorSeverity : VitalErrorSeverityType := (
        ErrNegDel    => WARNING,
        ErrInpSym    => ERROR,
        ErrOutSym    => ERROR,
        ErrStaSym    => ERROR,
        ErrVctLng    => ERROR,
        ErrTabWidSml => ERROR,
        ErrTabWidLrg => WARNING,
        ErrTabResSml => WARNING,
        ErrTabResLrg => WARNING
    );

    CONSTANT MsgNegDel : STRING :=
      "Negative delay. New output value not scheduled. Output signal is: ";
    CONSTANT MsgInpSym : STRING :=
      "Illegal symbol in the input portion of a Truth/State table.";
    CONSTANT MsgOutSym : STRING :=
      "Illegal symbol in the output portion of a Truth/State table.";
    CONSTANT MsgStaSym : STRING :=
      "Illegal symbol in the state portion of a State table.";
    CONSTANT MsgVctLng : STRING :=
      "Vector (array) lengths not equal. ";
    CONSTANT MsgTabWidSml : STRING :=
      "Width of the Truth/State table is too small.";
    CONSTANT MsgTabWidLrg : STRING :=
      "Width of Truth/State table is too large. Extra elements are ignored.";
    CONSTANT MsgTabResSml : STRING :=
      "Result of Truth/State table has too many elements.";
    CONSTANT MsgTabResLrg : STRING :=
      "Result of Truth/State table has too few elements.";

    CONSTANT MsgUnknown : STRING :=
      "Unknown error message.";

    --------------------------------------------------------------------
    -- LOCAL Utilities
    --------------------------------------------------------------------
    FUNCTION VitalMessage (
            CONSTANT ErrorId : IN VitalErrorType
          ) RETURN STRING IS
    BEGIN
        CASE ErrorId IS
            WHEN ErrNegDel    => RETURN MsgNegDel;
            WHEN ErrInpSym    => RETURN MsgInpSym;
            WHEN ErrOutSym    => RETURN MsgOutSym;
            WHEN ErrStaSym    => RETURN MsgStaSym;
            WHEN ErrVctLng    => RETURN MsgVctLng;
            WHEN ErrTabWidSml => RETURN MsgTabWidSml;
            WHEN ErrTabWidLrg => RETURN MsgTabWidLrg;
            WHEN ErrTabResSml => RETURN MsgTabResSml;
            WHEN ErrTabResLrg => RETURN MsgTabResLrg;
            WHEN OTHERS       => RETURN MsgUnknown;
        END CASE;
    END;

    PROCEDURE VitalError (
            CONSTANT Routine : IN STRING;
            CONSTANT ErrorId : IN VitalErrorType
    ) IS
    BEGIN
        ASSERT FALSE
          REPORT Routine & ": " & VitalMessage(ErrorId)
          SEVERITY VitalErrorSeverity(ErrorId);
    END;

    PROCEDURE VitalError (
            CONSTANT Routine : IN STRING;
            CONSTANT ErrorId : IN VitalErrorType;
            CONSTANT Info    : IN STRING
    ) IS
    BEGIN
        ASSERT FALSE
          REPORT Routine & ": " & VitalMessage(ErrorId) & Info
          SEVERITY VitalErrorSeverity(ErrorId);
    END;

    PROCEDURE VitalError (
            CONSTANT Routine : IN STRING;
            CONSTANT ErrorId : IN VitalErrorType;
            CONSTANT Info    : IN CHARACTER
    ) IS
    BEGIN
        ASSERT FALSE
          REPORT Routine & ": " & VitalMessage(ErrorId) & Info
          SEVERITY VitalErrorSeverity(ErrorId);
    END;

    ---------------------------------------------------------------------------
    PROCEDURE ReportGlitch ( 
            CONSTANT GlitchRoutine  : IN  STRING;
            CONSTANT OutSignalName  : IN  STRING;
            CONSTANT PreemptedTime  : IN  TIME;
            CONSTANT PreemptedValue : IN  std_ulogic;
            CONSTANT NewTime        : IN  TIME;
            CONSTANT NewValue       : IN  std_ulogic;
            CONSTANT Index          : IN  INTEGER := 0;
            CONSTANT IsArraySignal  : IN  BOOLEAN := FALSE;
            CONSTANT MsgSeverity    : IN  SEVERITY_LEVEL := WARNING
    ) IS

        VARIABLE StrPtr1, StrPtr2, StrPtr3, StrPtr4, StrPtr5 : LINE;
    BEGIN

        Write (StrPtr1, PreemptedTime );
        Write (StrPtr2, NewTime);
        Write (StrPtr3, LogicCvtTable(PreemptedValue));
        Write (StrPtr4, LogicCvtTable(NewValue));
        IF IsArraySignal THEN
            Write (StrPtr5, STRING'( "(" ) );
            Write (StrPtr5, Index);
            Write (StrPtr5, STRING'( ")" ) );
        ELSE
            Write (StrPtr5, STRING'( " " ) );
        END IF;

        -- Issue Report only if Preemted value has not been
        --  removed from event queue
        ASSERT PreemptedTime >  NewTime
          REPORT GlitchRoutine & ": GLITCH Detected on port " & 
                 OutSignalName & StrPtr5.ALL &
                 "; Preempted Future Value := " & StrPtr3.ALL &
                 " @ " & StrPtr1.ALL &
                 "; Newly Scheduled Value := " & StrPtr4.ALL &
                 " @ " & StrPtr2.ALL &
                 ";"
          SEVERITY MsgSeverity;

        DEALLOCATE(StrPtr1);
        DEALLOCATE(StrPtr2);
        DEALLOCATE(StrPtr3);
        DEALLOCATE(StrPtr4); 
        DEALLOCATE(StrPtr5); 
        RETURN;
    END ReportGlitch;

    ---------------------------------------------------------------------------
    -- Procedure  : VitalGlitchOnEvent
    --            :
    -- Parameters : OutSignal ........ signal being driven
    --            : OutSignalName..... name of the driven signal
    --            : GlitchData........ internal data required by the procedure
    --            : NewValue.......... new value being assigned
    --            : NewDelay.......... Delay accompanying the assignment
    --            :                    (Note: for vectors, this is an array)
    --            : GlitchMode........ Glitch generation mode
    --            :                     MessagePlusX, MessageOnly, 
    --            :                     XOnly, NoGlitch )
    --            : GlitchDelay....... if <= 0 ns , then there will be no Glitch
    --            :                    if >  NewDelay, then there is no Glitch,
    --            :                    otherwise, this is the time when a FORCED
    --            :                    generation of a glitch will occur. 
    ----------------------------------------------------------------------------
    PROCEDURE VitalGlitchOnEvent (
            SIGNAL   OutSignal        : OUT   std_logic;
            CONSTANT OutSignalName    : IN    STRING;
            VARIABLE GlitchData       : INOUT GlitchDataType;
            CONSTANT NewValue         : IN    std_logic;
            CONSTANT NewDelay         : IN    TIME := 0 ns;
            CONSTANT GlitchMode       : IN    VitalGlitchModeType := MessagePlusX;
            CONSTANT GlitchDelay      : IN    TIME := -1 ns;  -- IR#223
            CONSTANT MsgSeverity      : IN    SEVERITY_LEVEL := WARNING
    ) IS
    -- ------------------------------------------------------------------------
        VARIABLE NoGlitchDet  : BOOLEAN := FALSE;
        VARIABLE OldGlitch    : BOOLEAN := FALSE;
        VARIABLE Dly          : TIME    := NewDelay;

    BEGIN
        -- If nothing to schedule, just return
        IF NewDelay < 0 ns THEN
            IF (NewValue /= GlitchData.SchedValue) THEN
                VitalError ( "VitalGlitchOnEvent", ErrNegDel, OutSignalName );
            END IF;

        ELSE 
            -- If nothing currently scheduled
            IF GlitchData.SchedTime <= NOW THEN
                GlitchData.CurrentValue := GlitchData.SchedValue;
                IF (GlitchDelay <= 0 ns) THEN
                    IF (NewValue = GlitchData.SchedValue) THEN RETURN; END IF;
                    NoGlitchDet := TRUE;
                END IF;
     
            -- Transaction currently scheduled - if glitch already happened
            ELSIF GlitchData.GlitchTime <= NOW THEN
                GlitchData.CurrentValue := 'X';
                OldGlitch := TRUE;
                IF (GlitchData.SchedValue = NewValue) THEN
                    dly := Minimum( GlitchData.SchedTime-NOW, NewDelay );
                END IF;
     
            -- Transaction currently scheduled (no glitch if same value)
            ELSIF (GlitchData.SchedValue = NewValue) AND
                  (GlitchData.SchedTime = GlitchData.GlitchTime) AND
                  (GlitchDelay <= 0 ns) THEN
                NoGlitchDet := TRUE;
                Dly := Minimum( GlitchData.SchedTime-NOW, NewDelay );

            END IF;
     
            GlitchData.SchedTime := NOW+Dly;
            IF OldGlitch THEN
                OutSignal <= NewValue AFTER Dly;

            ELSIF NoGlitchDet THEN
                GlitchData.GlitchTime := NOW+Dly;
                OutSignal <= NewValue AFTER Dly;

            ELSE -- new glitch
                GlitchData.GlitchTime := GlitchMinTime ( GlitchData.GlitchTime, 
                                                         NOW+GlitchDelay );

                IF (GlitchMode = MessagePlusX) OR
                   (GlitchMode = MessageOnly) THEN
                    ReportGlitch ( "VitalGlitchOnEvent", OutSignalName,
                                   GlitchData.GlitchTime, GlitchData.SchedValue,
                                   (Dly + NOW), NewValue,
                                   MsgSeverity=>MsgSeverity );
                END IF;

                IF (GlitchMode = MessagePlusX) OR (GlitchMode = XOnly) THEN
                    OutSignal <= 'X' AFTER GlitchData.GlitchTime-NOW;
                    OutSignal <=  TRANSPORT NewValue AFTER Dly;
                ELSE
                    OutSignal <= NewValue AFTER Dly;
                END IF;
            END IF;

            GlitchData.SchedValue := NewValue;
        END IF;

        RETURN;
    END;
 
    ----------------------------------------------------------------------------
    PROCEDURE VitalGlitchOnEvent (
            SIGNAL   OutSignal        : OUT   std_logic_vector;
            CONSTANT OutSignalName    : IN    STRING;
            VARIABLE GlitchData       : INOUT GlitchDataArrayType;
            CONSTANT NewValue         : IN    std_logic_vector;
            CONSTANT NewDelay         : IN    VitalTimeArray;
            CONSTANT GlitchMode       : IN    VitalGlitchModeType := MessagePlusX;
            CONSTANT GlitchDelay      : IN    VitalTimeArray;
            CONSTANT MsgSeverity      : IN    SEVERITY_LEVEL := WARNING
    ) IS

        ALIAS GlDataAlias  : GlitchDataArrayType(1 TO GlitchData'LENGTH) 
                                 IS GlitchData;
        ALIAS NewValAlias  : std_logic_vector(1 TO NewValue'LENGTH) IS NewValue;
        ALIAS GlDelayAlias : VitalTimeArray(1 TO GlitchDelay'LENGTH)
              IS GlitchDelay;
        ALIAS NewDelAlias  : VitalTimeArray(1 TO NewDelay'LENGTH) IS NewDelay;
 
        VARIABLE Index       : INTEGER := OutSignal'LEFT;
        VARIABLE Direction   : INTEGER;
        VARIABLE NoGlitchDet : BOOLEAN;
        VARIABLE OldGlitch   : BOOLEAN;
        VARIABLE Dly, GlDly  : TIME;

    BEGIN
        IF (OutSignal'LEFT > OutSignal'RIGHT) THEN
            Direction := -1;
        ELSE
            Direction := 1;
        END IF;

        IF ( (OutSignal'LENGTH /=  GlitchData'LENGTH) OR
             (OutSignal'LENGTH /=    NewValue'LENGTH) OR
             (OutSignal'LENGTH /=    NewDelay'LENGTH) OR
             (OutSignal'LENGTH /= GlitchDelay'LENGTH) ) THEN
          VitalError ( "VitalGlitchOnEvent", ErrVctLng, OutSignalName );
          RETURN;
        END IF;

        -- a call to the scalar function cannot be made since the actual 
        -- name associated with a signal parameter must be locally static
      FOR n IN 1 TO OutSignal'LENGTH LOOP

        NoGlitchDet := FALSE;
        OldGlitch   := FALSE;
        Dly := NewDelAlias(n);

        -- If nothing to schedule, just skip to next loop iteration
        IF NewDelAlias(n) < 0 ns THEN
            IF (NewValAlias(n) /=  GlDataAlias(n).SchedValue) THEN
                VitalError ( "VitalGlitchOnEvent", ErrNegDel, OutSignalName );
            END IF;
        ELSE
            -- If nothing currently scheduled (i.e. last scheduled 
            -- transaction already occurred)
            IF GlDataAlias(n).SchedTime <= NOW THEN
                GlDataAlias(n).CurrentValue := GlDataAlias(n).SchedValue;
                IF (GlDelayAlias(n) <= 0 ns) THEN
                    -- Next iteration if no change in value
                    IF (NewValAlias(n) = GlDataAlias(n).SchedValue) THEN 
                        Index := Index + Direction;
                        NEXT; 
                    END IF; 
                    -- since last transaction already occurred there is no glitch
                    NoGlitchDet := TRUE;
                END IF;

            -- Transaction currently scheduled - if glitch already happened
            ELSIF GlDataAlias(n).GlitchTime <= NOW THEN
                GlDataAlias(n).CurrentValue := 'X';
                OldGlitch := TRUE;
                IF (GlDataAlias(n).SchedValue = NewValAlias(n)) THEN
                    dly := Minimum( GlDataAlias(n).SchedTime-NOW,
                                    NewDelAlias(n) );
                END IF;

            -- Transaction currently scheduled
            ELSIF (GlDataAlias(n).SchedValue = NewValAlias(n)) AND
                  (GlDataAlias(n).SchedTime = GlDataAlias(n).GlitchTime) AND
                  (GlDelayAlias(n) <= 0 ns) THEN
                  NoGlitchDet := TRUE;
                  Dly := Minimum( GlDataAlias(n).SchedTime-NOW, 
                                  NewDelAlias(n) );
            END IF;

            -- update last scheduled transaction
            GlDataAlias(n).SchedTime := NOW+Dly;

            IF OldGlitch THEN
                OutSignal(Index) <= NewValAlias(n) AFTER Dly;
            ELSIF NoGlitchDet THEN
                -- if no glitch then update last glitch time 
                -- and OutSignal(actual_index)
                GlDataAlias(n).GlitchTime := NOW+Dly;
                OutSignal(Index) <= NewValAlias(n) AFTER Dly;
            ELSE   -- new glitch
                GlDataAlias(n).GlitchTime := GlitchMinTime ( 
                                                  GlDataAlias(n).GlitchTime,
                                                  NOW+GlDelayAlias(n) );

                IF (GlitchMode = MessagePlusX) OR
                   (GlitchMode = MessageOnly) THEN
                    ReportGlitch ( "VitalGlitchOnEvent", OutSignalName,
                                   GlDataAlias(n).GlitchTime, 
                                   GlDataAlias(n).SchedValue,
                                   (Dly + NOW), NewValAlias(n), 
                                   Index, TRUE, MsgSeverity );
                END IF;

                IF (GlitchMode = MessagePlusX) OR (GlitchMode = XOnly) THEN
                    GlDly := GlDataAlias(n).GlitchTime - NOW;
                    OutSignal(Index) <= 'X' AFTER GlDly;
                    OutSignal(Index) <= TRANSPORT NewValAlias(n) AFTER Dly;
                ELSE
                    OutSignal(Index) <= NewValAlias(n) AFTER Dly;
                END IF;

            END IF; -- glitch / no-glitch
            GlDataAlias(n).SchedValue := NewValAlias(n);

        END IF; -- NewDelAlias(n) < 0 ns
        Index := Index + Direction;
      END LOOP;

        RETURN;
    END;

    ---------------------------------------------------------------------------
    -- ------------------------------------------------------------------------
    --  PROCEDURE NAME  :  TruthOutputX01Z
    --
    --  PARAMETERS      :  table_out - output of table
    --                     X01Zout   - output converted to X01Z
    --                     err       - true if illegal character is encountered
    --
    --
    --  DESCRIPTION     :  converts the output of a truth table to a valid
    --                     std_ulogic
    -- ------------------------------------------------------------------------
    PROCEDURE TruthOutputX01Z (
            CONSTANT TableOut : IN VitalTruthSymbolType;
            VARIABLE X01Zout   : OUT std_ulogic;
            VARIABLE Err       : OUT BOOLEAN
    ) IS
        VARIABLE TempOut : std_ulogic;
    BEGIN
        Err := FALSE;
        TempOut := TruthTableOutMap(TableOut);
        IF (TempOut = '-') THEN
            Err := TRUE;
            TempOut := 'X';
            VitalError ( "VitalTruthTable", ErrOutSym, To_TruthChar(TableOut));
        END IF;
        X01Zout := TempOut;
    END;

    -- ------------------------------------------------------------------------
    --  PROCEDURE NAME  :  StateOutputX01Z
    --
    --  PARAMETERS      :  table_out - output of table
    --                     prev_out  - previous output value
    --                     X01Zout   - output cojnverted to X01Z
    --                     err       - true if illegal character is encountered
    --
    --  DESCRIPTION     :  converts the output of a state table to a
    --                     valid std_ulogic
    -- ------------------------------------------------------------------------
    PROCEDURE StateOutputX01Z (
            CONSTANT TableOut : IN VitalStateSymbolType;
            CONSTANT PrevOut  : IN std_ulogic;
            VARIABLE X01Zout   : OUT std_ulogic;
            VARIABLE Err       : OUT BOOLEAN
    ) IS
        VARIABLE TempOut : std_ulogic;
    BEGIN
        Err := FALSE;
        TempOut := StateTableOutMap(TableOut);
        IF (TempOut = '-') THEN
            Err := TRUE;
            TempOut := 'X';
            VitalError ( "VitalStateTable", ErrOutSym, To_StateChar(TableOut));
        ELSIF (TempOut = 'W') THEN
            TempOut := To_X01Z(PrevOut);
        END IF;
        X01Zout := TempOut;
    END;

    -- ------------------------------------------------------------------------
    -- PROCEDURE NAME:  StateMatch
    --
    -- PARAMETERS    :  symbol       - symbol from state table
    --                  in2          - input from VitalStateTble procedure
    --                                 to state table
    --                  in2LastValue - previous value of input
    --                  state        - false if the symbol is from the input
    --                                  portion of the table,
    --                                 true if the symbol is from the state
    --                                  portion of the table
    --                  Err          - true if symbol is not a valid input symbol
    --                  ReturnValue  - true if match occurred
    --
    -- DESCRIPTION   :  This procedure sets ReturnValue to true if in2 matches
    --                  symbol (from the state table).  If symbol is an edge
    --                  value edge is set to true and in2 and in2LastValue are
    --                  checked against symbol.  Err is set to true if symbol
    --                  is an invalid value for the input portion of the state
    --                  table.
    --
    -- ------------------------------------------------------------------------
    PROCEDURE StateMatch (
            CONSTANT Symbol       : IN VitalStateSymbolType;
            CONSTANT in2          : IN std_ulogic;
            CONSTANT in2LastValue : IN std_ulogic;
            CONSTANT State        : IN BOOLEAN;
            VARIABLE Err          : OUT BOOLEAN;
            VARIABLE ReturnValue  : OUT BOOLEAN
    ) IS
    BEGIN
        IF (State) THEN
            IF (NOT ValidStateTableState(Symbol)) THEN
                VitalError ( "VitalStateTable", ErrStaSym, To_StateChar(Symbol));
                Err := TRUE;
                ReturnValue := FALSE;
            ELSE
                Err := FALSE;
                ReturnValue := StateTableMatch(in2LastValue, in2, Symbol);
            END IF;
        ELSE
            IF (NOT ValidStateTableInput(Symbol) ) THEN
                VitalError ( "VitalStateTable", ErrInpSym, To_StateChar(Symbol));
                Err := TRUE;
                ReturnValue := FALSE;
            ELSE
                ReturnValue := StateTableMatch(in2LastValue, in2, Symbol);
                Err := FALSE;
            END IF;
        END IF;
    END;

    -- -----------------------------------------------------------------------
    -- FUNCTION NAME:  StateTableLookUp
    --
    -- PARAMETERS   :  StateTable     - state table
    --                 PresentDataIn  - current inputs
    --                 PreviousDataIn - previous inputs and states
    --                 NumStates      - number of state variables
    --                 PresentOutputs - current state and current outputs
    --
    -- DESCRIPTION  :  This function is used to find the output of the
    --                 StateTable corresponding to a given set of inputs.
    --
    -- ------------------------------------------------------------------------
    FUNCTION StateTableLookUp (
            CONSTANT StateTable     : VitalStateTableType;
            CONSTANT PresentDataIn  : std_logic_vector;
            CONSTANT PreviousDataIn : std_logic_vector;
            CONSTANT NumStates      : NATURAL;
            CONSTANT PresentOutputs : std_logic_vector
          ) RETURN std_logic_vector IS

        CONSTANT InputSize    : INTEGER := PresentDataIn'LENGTH;
        CONSTANT NumInputs    : INTEGER := InputSize + NumStates - 1;
        CONSTANT TableEntries : INTEGER := StateTable'LENGTH(1);
        CONSTANT TableWidth   : INTEGER := StateTable'LENGTH(2);
        CONSTANT OutSize      : INTEGER := TableWidth - InputSize - NumStates;
        VARIABLE Inputs       : std_logic_vector(0 TO NumInputs);
        VARIABLE PrevInputs   : std_logic_vector(0 TO NumInputs)
                                := (OTHERS => 'X');
        VARIABLE ReturnValue  : std_logic_vector(0 TO (OutSize-1))
                                := (OTHERS => 'X');
        VARIABLE Temp   : std_ulogic;
        VARIABLE Match  : BOOLEAN;
        VARIABLE Err    : BOOLEAN := FALSE;

        -- This needs to be done since the TableLookup arrays must be
        -- ascending starting with 0
        VARIABLE TableAlias   : VitalStateTableType(0 TO TableEntries - 1,
                                                    0 TO TableWidth - 1)
                                := StateTable;

    BEGIN
        Inputs(0 TO InputSize-1) := PresentDataIn;
        Inputs(InputSize TO NumInputs) := PresentOutputs(0 TO NumStates - 1);
        PrevInputs(0 TO InputSize - 1) := PreviousDataIn(0 TO InputSize - 1);

      ColLoop: -- Compare each entry in the table
        FOR i IN TableAlias'RANGE(1) LOOP

        RowLoop: -- Check each element of the entry
          FOR j IN 0 TO InputSize + NumStates  LOOP

            IF (j = InputSize + NumStates) THEN        -- a match occurred
                FOR k IN 0 TO Minimum(OutSize, PresentOutputs'LENGTH)-1  LOOP
                    StateOutputX01Z (
                            TableAlias(i, TableWidth - k - 1),
                            PresentOutputs(PresentOutputs'LENGTH - k - 1),
                            Temp, Err);
                    ReturnValue(OutSize - k - 1) := Temp;
                    IF (Err) THEN
                        ReturnValue := (OTHERS => 'X');
                        RETURN ReturnValue;
                    END IF;
                END LOOP;
                RETURN ReturnValue;
            END IF;

            StateMatch ( TableAlias(i,j),
                         Inputs(j), PrevInputs(j),
                         j >= InputSize, Err, Match);
            EXIT RowLoop WHEN NOT(Match);
            EXIT ColLoop WHEN Err;
          END LOOP RowLoop;
        END LOOP ColLoop;

        ReturnValue := (OTHERS => 'X');
        RETURN ReturnValue;
    END;

    --------------------------------------------------------------------
    -- to_ux01z
    -------------------------------------------------------------------
    FUNCTION To_UX01Z  ( s : std_ulogic
          ) RETURN  UX01Z IS
    BEGIN
        RETURN cvt_to_ux01z (s);
    END;

    ---------------------------------------------------------------------------
    -- Function  : GetEdge
    -- Purpose   : Converts transitions on a given input signal into a
    --             enumeration value representing the transition or level
    --             of the signal.
    --
    --    previous "value"   current "value"     :=   "edge"
    --   ---------------------------------------------------------
    --     '1' | 'H'          '1' | 'H'                 '1'    level, no edge
    --     '0' | 'L'          '1' | 'H'                 '/'    rising edge
    --      others            '1' | 'H'                 'R'    rising from X
    --
    --     '1' | 'H'          '0' | 'L'                 '\'    falling egde
    --     '0' | 'L'          '0' | 'L'                 '0'    level, no edge
    --      others            '0' | 'L'                 'F'    falling from X
    --
    --     'X' | 'W' | '-'    'X' | 'W' | '-'           'X'    unknown (X) level
    --     'Z'                'Z'                       'X'    unknown (X) level
    --     'U'                'U'                       'U'    'U' level
    --
    --     '1' | 'H'           others                   'f'    falling to X
    --     '0' | 'L'           others                   'r'    rising to X
    --     'X' | 'W' | '-'    'U' | 'Z'                 'x'    unknown (X) edge
    --     'Z'                'X' | 'W' | '-' | 'U'     'x'    unknown (X) edge
    --     'U'                'X' | 'W' | '-' | 'Z'     'x'    unknown (X) edge
    --
    ---------------------------------------------------------------------------
    FUNCTION GetEdge (
            SIGNAL      s : IN    std_logic
          ) RETURN EdgeType IS
    BEGIN
        IF (s'EVENT)
            THEN RETURN LogicToEdge  ( s'LAST_VALUE, s );
            ELSE RETURN LogicToLevel ( s );
        END IF;
    END;

    ---------------------------------------------------------------------------
    PROCEDURE GetEdge (
            SIGNAL       s : IN    std_logic_vector;
            VARIABLE LastS : INOUT std_logic_vector;
            VARIABLE  Edge :   OUT EdgeArray ) IS

        ALIAS     sAlias : std_logic_vector ( 1 TO     s'LENGTH ) IS s;
        ALIAS LastSAlias : std_logic_vector ( 1 TO LastS'LENGTH ) IS LastS;
        ALIAS  EdgeAlias : EdgeArray ( 1 TO  Edge'LENGTH ) IS Edge;
    BEGIN
        IF s'LENGTH /= LastS'LENGTH OR
           s'LENGTH /=  Edge'LENGTH THEN
            VitalError ( "GetEdge", ErrVctLng, "s, LastS, Edge" );
        END IF;

        FOR n IN 1 TO s'LENGTH LOOP
            EdgeAlias(n)  := LogicToEdge( LastSAlias(n), sAlias(n) );
            LastSAlias(n) := sAlias(n);
        END LOOP;
    END;

    ---------------------------------------------------------------------------
    FUNCTION  ToEdge     ( Value         : IN std_logic
          ) RETURN EdgeType IS
    BEGIN
        RETURN LogicToLevel( Value );
    END;

    -- Note: This function will likely be replaced by S'DRIVING_VALUE in VHDL'92
    ----------------------------------------------------------------------------
    IMPURE FUNCTION CurValue (
            CONSTANT GlitchData : IN  GlitchDataType
          ) RETURN std_logic IS
    BEGIN
        IF NOW >= GlitchData.SchedTime THEN
            RETURN GlitchData.SchedValue;
        ELSIF NOW >= GlitchData.GlitchTime THEN
            RETURN 'X';
        ELSE
            RETURN GlitchData.CurrentValue;
        END IF;
    END;
    ---------------------------------------------------------------------------
    IMPURE FUNCTION CurValue (
            CONSTANT GlitchData : IN  GlitchDataArrayType
          ) RETURN std_logic_vector IS
        VARIABLE Result : std_logic_vector(GlitchData'RANGE);
    BEGIN
        FOR n IN GlitchData'RANGE LOOP
            IF NOW >= GlitchData(n).SchedTime THEN
                Result(n) := GlitchData(n).SchedValue;
            ELSIF NOW >= GlitchData(n).GlitchTime THEN
                Result(n) := 'X';
            ELSE
                Result(n) := GlitchData(n).CurrentValue;
            END IF;
        END LOOP;
        RETURN Result;
    END;

    ---------------------------------------------------------------------------
    -- function calculation utilities
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- Function   : VitalSame
    -- Returns    : VitalSame compares the state (UX01) of two logic value. A
    --              value of 'X' is returned if the values are different.  The
    --              common value is returned if the values are equal.
    -- Purpose    : When the result of a logic model may be either of two
    --              separate input values (eg. when the select on a MUX is 'X'),
    --              VitalSame may be used to determine if the result needs to
    --              be 'X'.
    -- Arguments  : See the declarations below...
    ---------------------------------------------------------------------------
    FUNCTION VitalSame (
            CONSTANT a, b : IN std_ulogic
          ) RETURN std_ulogic IS
    BEGIN
        IF To_UX01(a) = To_UX01(b)
            THEN RETURN To_UX01(a);
            ELSE RETURN 'X';
        END IF;
    END;

    ---------------------------------------------------------------------------
    -- delay selection utilities
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- Procedure  : BufPath, InvPath
    --
    -- Purpose    : BufPath and InvPath compute output change times, based on
    --              a change on an input port. The computed output change times
    --              returned in the composite parameter 'schd'.
    --
    --              BufPath and InpPath are used together with the delay path
    --              selection functions (GetSchedDelay, VitalAND, VitalOR... )
    --              The 'schd' value from each of the input ports of a model are
    --              combined by the delay selection functions (VitalAND,
    --              VitalOR, ...). The GetSchedDelay procedure converts the
    --              combined output changes times to the single delay (delta
    --              time) value for scheduling the output change (passed to
    --              VitalGlitchOnEvent).
    --
    --              The values in 'schd' are: (absolute times)
    --                inp0  :  time of output change due to input change to 0
    --                inp1  :  time of output change due to input change to 1
    --                inpX  :  time of output change due to input change to X
    --                glch0 :  time of output glitch due to input change to 0
    --                glch1 :  time of output glitch due to input change to 1
    --
    --              The output times are computed from the model INPUT value
    --              and not the final value.  For this reason, 'BufPath' should
    --              be used to compute the output times for a non-inverting
    --              delay paths and 'InvPath' should be used to compute the
    --              ouput times for inverting delay paths. Delay paths which
    --              include both non-inverting and paths require usage of both
    --              'BufPath' and 'InvPath'. (IE this is needed for the
    --              select->output path of a MUX -- See the VitalMUX model).
    --
    --
    -- Parameters : schd....... Computed output result times. (INOUT parameter
    --                          modified only on input edges)
    --              Iedg....... Input port edge/level value.
    --               tpd....... Propagation delays from this input
    --
    ---------------------------------------------------------------------------

    PROCEDURE BufPath (
            VARIABLE Schd : INOUT SchedType;
            CONSTANT Iedg : IN    EdgeType;
            CONSTANT  tpd : IN    VitalDelayType01
    ) IS
    BEGIN
      CASE Iedg IS
        WHEN '0'|'1' => NULL;                   -- no edge: no timing update
        WHEN '/'|'R' => Schd.inp0 := TIME'HIGH;
                        Schd.inp1 := NOW + tpd(tr01);  Schd.Glch1 := Schd.inp1;
                        Schd.InpX := Schd.inp1;
        WHEN '\'|'F' => Schd.inp1 := TIME'HIGH;
                        Schd.inp0 := NOW + tpd(tr10);  Schd.Glch0 := Schd.inp0;
                        Schd.InpX := Schd.inp0;
        WHEN 'r'     => Schd.inp1 := TIME'HIGH;
                        Schd.inp0 := TIME'HIGH;
                        Schd.InpX := NOW + tpd(tr01);
        WHEN 'f'     => Schd.inp0 := TIME'HIGH;
                        Schd.inp1 := TIME'HIGH;
                        Schd.InpX := NOW + tpd(tr10);
        WHEN 'x'     => Schd.inp1 := TIME'HIGH;
                        Schd.inp0 := TIME'HIGH;
                        -- update for X->X change
                        Schd.InpX := NOW + Minimum(tpd(tr10),tpd(tr01));
        WHEN OTHERS  => NULL;                   -- no timing change
      END CASE;
    END;

    PROCEDURE BufPath (
            VARIABLE Schd : INOUT SchedArray;
            CONSTANT Iedg : IN    EdgeArray;
            CONSTANT  tpd : IN    VitalDelayArrayType01
    ) IS
    BEGIN
      FOR n IN Schd'RANGE LOOP
        CASE Iedg(n) IS
          WHEN '0'|'1' => NULL;                   -- no edge: no timing update
          WHEN '/'|'R' => Schd(n).inp0 := TIME'HIGH;
                          Schd(n).inp1 := NOW + tpd(n)(tr01);
                          Schd(n).Glch1 := Schd(n).inp1;
                          Schd(n).InpX := Schd(n).inp1;
          WHEN '\'|'F' => Schd(n).inp1 := TIME'HIGH;
                          Schd(n).inp0 := NOW + tpd(n)(tr10);
                          Schd(n).Glch0 := Schd(n).inp0;
                          Schd(n).InpX := Schd(n).inp0;
          WHEN 'r'     => Schd(n).inp1 := TIME'HIGH;
                          Schd(n).inp0 := TIME'HIGH;
                          Schd(n).InpX := NOW + tpd(n)(tr01);
          WHEN 'f'     => Schd(n).inp0 := TIME'HIGH;
                          Schd(n).inp1 := TIME'HIGH;
                          Schd(n).InpX := NOW + tpd(n)(tr10);
          WHEN 'x'     => Schd(n).inp1 := TIME'HIGH;
                          Schd(n).inp0 := TIME'HIGH;
                          -- update for X->X change
                          Schd(n).InpX := NOW + Minimum ( tpd(n)(tr10),
                                                          tpd(n)(tr01) );
          WHEN OTHERS  => NULL;                   -- no timing change
        END CASE;
      END LOOP;
    END;

    PROCEDURE InvPath (
            VARIABLE Schd : INOUT SchedType;
            CONSTANT Iedg : IN    EdgeType;
            CONSTANT  tpd : IN    VitalDelayType01
    ) IS
    BEGIN
      CASE Iedg IS
        WHEN '0'|'1' => NULL;                   -- no edge: no timing update
        WHEN '/'|'R' => Schd.inp0 := TIME'HIGH;
                        Schd.inp1 := NOW + tpd(tr10);  Schd.Glch1 := Schd.inp1;
                        Schd.InpX := Schd.inp1;
        WHEN '\'|'F' => Schd.inp1 := TIME'HIGH;
                        Schd.inp0 := NOW + tpd(tr01);  Schd.Glch0 := Schd.inp0;
                        Schd.InpX := Schd.inp0;
        WHEN 'r'     => Schd.inp1 := TIME'HIGH;
                        Schd.inp0 := TIME'HIGH;
                        Schd.InpX := NOW + tpd(tr10);
        WHEN 'f'     => Schd.inp0 := TIME'HIGH;
                        Schd.inp1 := TIME'HIGH;
                        Schd.InpX := NOW + tpd(tr01);
        WHEN 'x'     => Schd.inp1 := TIME'HIGH;
                        Schd.inp0 := TIME'HIGH;
                        -- update for X->X change
                        Schd.InpX := NOW + Minimum(tpd(tr10),tpd(tr01));
        WHEN OTHERS  => NULL;                   -- no timing change
      END CASE;
    END;

    PROCEDURE InvPath (
            VARIABLE Schd : INOUT SchedArray;
            CONSTANT Iedg : IN    EdgeArray;
            CONSTANT  tpd : IN    VitalDelayArrayType01
    ) IS
    BEGIN
      FOR n IN Schd'RANGE LOOP
        CASE Iedg(n) IS
          WHEN '0'|'1' => NULL;                   -- no edge: no timing update
          WHEN '/'|'R' => Schd(n).inp0 := TIME'HIGH;
                          Schd(n).inp1 := NOW + tpd(n)(tr10);
                          Schd(n).Glch1 := Schd(n).inp1;
                          Schd(n).InpX := Schd(n).inp1;
          WHEN '\'|'F' => Schd(n).inp1 := TIME'HIGH;
                          Schd(n).inp0 := NOW + tpd(n)(tr01);
                          Schd(n).Glch0 := Schd(n).inp0;
                          Schd(n).InpX := Schd(n).inp0;
          WHEN 'r'     => Schd(n).inp1 := TIME'HIGH;
                          Schd(n).inp0 := TIME'HIGH;
                          Schd(n).InpX := NOW + tpd(n)(tr10);
          WHEN 'f'     => Schd(n).inp0 := TIME'HIGH;
                          Schd(n).inp1 := TIME'HIGH;
                          Schd(n).InpX := NOW + tpd(n)(tr01);
          WHEN 'x'     => Schd(n).inp1 := TIME'HIGH;
                          Schd(n).inp0 := TIME'HIGH;
                          -- update for X->X change
                          Schd(n).InpX := NOW + Minimum ( tpd(n)(tr10),
                                                          tpd(n)(tr01) );
          WHEN OTHERS  => NULL;                   -- no timing change
        END CASE;
      END LOOP;
    END;

    ---------------------------------------------------------------------------
    -- Procedure  : BufEnab, InvEnab
    --
    -- Purpose    : BufEnab and InvEnab compute output change times, from a
    --              change on an input enable port for a 3-state driver. The
    --              computed output change times are returned in the composite
    --              parameters 'schd1', 'schd0'.
    --
    --              BufEnab and InpEnab are used together with the delay path
    --              selection functions (GetSchedDelay, VitalAND, VitalOR... )
    --              The 'schd' value from each of the non-enable input ports of
    --              a model (See BufPath, InvPath) are combined using the delay
    --              selection functions (VitalAND,  VitalOR, ...). The
    --              GetSchedDelay procedure combines the output times on the
    --              enable path with the output times from the data path(s) and
    --              computes the single delay (delta time) value for scheduling
    --              the output change (passed to VitalGlitchOnEvent)
    --
    --              The values in 'schd*' are: (absolute times)
    --                inp0  :  time of output change due to input change to 0
    --                inp1  :  time of output change due to input change to 1
    --                inpX  :  time of output change due to input change to X
    --                glch0 :  time of output glitch due to input change to 0
    --                glch1 :  time of output glitch due to input change to 1
    --
    --              'schd1' contains output times for 1->Z, Z->1 transitions.
    --              'schd0' contains output times for 0->Z, Z->0 transitions.
    --
    --              'BufEnab' is used for computing the output times for an
    --              high asserted enable (output 'Z' for enable='0').
    --              'InvEnab' is used for computing the output times for an
    --              low asserted enable (output 'Z' for enable='1').
    --
    --              Note: separate 'schd1', 'schd0' parameters are generated
    --                    so that the combination of the delay paths from
    --                    multiple enable signals may be combined using the
    --                    same functions/operators used in combining separate
    --                    data paths. (See exampe 2 below)
    --
    --
    -- Parameters : schd1...... Computed output result times for 1->Z, Z->1
    --                          transitions. This parameter is modified only on
    --                          input edge values (events).
    --              schd0...... Computed output result times for 0->Z, 0->1
    --                          transitions. This parameter is modified only on
    --                          input edge values (events).
    --              Iedg....... Input port edge/level value.
    --               tpd....... Propagation delays for the enable -> output path.
    --
    ---------------------------------------------------------------------------
    PROCEDURE BufEnab (
            VARIABLE Schd1 : INOUT SchedType;
            VARIABLE Schd0 : INOUT SchedType;
            CONSTANT  Iedg : IN    EdgeType;
            CONSTANT   tpd : IN    VitalDelayType01Z
    ) IS
    BEGIN
      CASE Iedg IS
        WHEN '0'|'1' => NULL;                   -- no edge: no timing update
        WHEN '/'|'R' => Schd1.inp0 := TIME'HIGH;
                        Schd1.inp1 := NOW + tpd(trz1);
                        Schd1.Glch1 := Schd1.inp1;
                        Schd1.InpX := Schd1.inp1;
                        Schd0.inp0 := TIME'HIGH;
                        Schd0.inp1 := NOW + tpd(trz0);
                        Schd0.Glch1 := Schd0.inp1;
                        Schd0.InpX := Schd0.inp1;
        WHEN '\'|'F' => Schd1.inp1 := TIME'HIGH;
                        Schd1.inp0 := NOW + tpd(tr1z);
                        Schd1.Glch0 := Schd1.inp0;
                        Schd1.InpX := Schd1.inp0;
                        Schd0.inp1 := TIME'HIGH;
                        Schd0.inp0 := NOW + tpd(tr0z);
                        Schd0.Glch0 := Schd0.inp0;
                        Schd0.InpX := Schd0.inp0;
        WHEN 'r'     => Schd1.inp1 := TIME'HIGH;
                        Schd1.inp0 := TIME'HIGH;
                        Schd1.InpX := NOW + tpd(trz1);
                        Schd0.inp1 := TIME'HIGH;
                        Schd0.inp0 := TIME'HIGH;
                        Schd0.InpX := NOW + tpd(trz0);
        WHEN 'f'     => Schd1.inp0 := TIME'HIGH;
                        Schd1.inp1 := TIME'HIGH;
                        Schd1.InpX := NOW + tpd(tr1z);
                        Schd0.inp0 := TIME'HIGH;
                        Schd0.inp1 := TIME'HIGH;
                        Schd0.InpX := NOW + tpd(tr0z);
        WHEN 'x'     => Schd1.inp0 := TIME'HIGH;
                        Schd1.inp1 := TIME'HIGH;
                        Schd1.InpX := NOW + Minimum(tpd(tr10),tpd(tr01));
                        Schd0.inp0 := TIME'HIGH;
                        Schd0.inp1 := TIME'HIGH;
                        Schd0.InpX := NOW + Minimum(tpd(tr10),tpd(tr01));
        WHEN OTHERS  => NULL;                   -- no timing change
      END CASE;
    END;

    PROCEDURE InvEnab (
            VARIABLE Schd1 : INOUT SchedType;
            VARIABLE Schd0 : INOUT SchedType;
            CONSTANT  Iedg : IN    EdgeType;
            CONSTANT   tpd : IN    VitalDelayType01Z
    ) IS
    BEGIN
      CASE Iedg IS
        WHEN '0'|'1' => NULL;                   -- no edge: no timing update
        WHEN '/'|'R' => Schd1.inp0 := TIME'HIGH;
                        Schd1.inp1 := NOW + tpd(tr1z);
                        Schd1.Glch1 := Schd1.inp1;
                        Schd1.InpX := Schd1.inp1;
                        Schd0.inp0 := TIME'HIGH;
                        Schd0.inp1 := NOW + tpd(tr0z);
                        Schd0.Glch1 := Schd0.inp1;
                        Schd0.InpX := Schd0.inp1;
        WHEN '\'|'F' => Schd1.inp1 := TIME'HIGH;
                        Schd1.inp0 := NOW + tpd(trz1);
                        Schd1.Glch0 := Schd1.inp0;
                        Schd1.InpX := Schd1.inp0;
                        Schd0.inp1 := TIME'HIGH;
                        Schd0.inp0 := NOW + tpd(trz0);
                        Schd0.Glch0 := Schd0.inp0;
                        Schd0.InpX := Schd0.inp0;
        WHEN 'r'     => Schd1.inp1 := TIME'HIGH;
                        Schd1.inp0 := TIME'HIGH;
                        Schd1.InpX := NOW + tpd(tr1z);
                        Schd0.inp1 := TIME'HIGH;
                        Schd0.inp0 := TIME'HIGH;
                        Schd0.InpX := NOW + tpd(tr0z);
        WHEN 'f'     => Schd1.inp0 := TIME'HIGH;
                        Schd1.inp1 := TIME'HIGH;
                        Schd1.InpX := NOW + tpd(trz1);
                        Schd0.inp0 := TIME'HIGH;
                        Schd0.inp1 := TIME'HIGH;
                        Schd0.InpX := NOW + tpd(trz0);
        WHEN 'x'     => Schd1.inp0 := TIME'HIGH;
                        Schd1.inp1 := TIME'HIGH;
                        Schd1.InpX := NOW + Minimum(tpd(tr10),tpd(tr01));
                        Schd0.inp0 := TIME'HIGH;
                        Schd0.inp1 := TIME'HIGH;
                        Schd0.InpX := NOW + Minimum(tpd(tr10),tpd(tr01));
        WHEN OTHERS  => NULL;                   -- no timing change
      END CASE;
    END;

    ---------------------------------------------------------------------------
    -- Procedure  : GetSchedDelay
    --
    -- Purpose    : GetSchedDelay computes the final delay (incremental) for
    --              for scheduling an output signal.  The delay is computed
    --              from the absolute output times in the 'NewSched' parameter.
    --              (See BufPath, InvPath).
    --
    --              Computation of the output delay for non-3_state outputs
    --              consists of selection the appropriate output time based
    --              on the new output value 'NewValue' and subtracting 'NOW'
    --              to convert to an incremental delay value.
    --
    --              The Computation of the output delay for 3_state output
    --              also includes combination of the enable path delay with
    --              the date path delay.
    --
    -- Parameters : NewDelay... Returned output delay value.
    --              GlchDelay.. Returned output delay for the start of a glitch.
    --              NewValue... New output value.
    --              CurValue... Current value of the output.
    --              NewSched... Composite containing the combined absolute
    --                          output times from the data inputs.
    --              EnSched1... Composite containing the combined absolute
    --                          output times from the enable input(s).
    --                          (for a 3_state output transitions 1->Z, Z->1)
    --              EnSched0... Composite containing the combined absolute
    --                          output times from the enable input(s).
    --                          (for a 3_state output transitions 0->Z, Z->0)
    --
    ---------------------------------------------------------------------------
    PROCEDURE GetSchedDelay (
            VARIABLE   NewDelay : OUT TIME;
            VARIABLE  GlchDelay : OUT TIME;
            CONSTANT   NewValue : IN  std_ulogic;
            CONSTANT   CurValue : IN  std_ulogic;
            CONSTANT   NewSched : IN  SchedType
    ) IS
        VARIABLE Tim, Glch : TIME;
    BEGIN

        CASE To_UX01(NewValue) IS
          WHEN '0'    => Tim  := NewSched.inp0;
                         Glch := NewSched.Glch1;
          WHEN '1'    => Tim  := NewSched.inp1;
                         Glch := NewSched.Glch0;
          WHEN OTHERS => Tim  := NewSched.InpX;
                         Glch := -1 ns;
        END CASE;
        IF (CurValue /= NewValue)
          THEN Glch := -1 ns;
        END IF;

        NewDelay  := Tim  - NOW;
        IF Glch < 0 ns
            THEN GlchDelay := Glch;
            ELSE GlchDelay := Glch - NOW;
        END IF; -- glch < 0 ns
    END;

    PROCEDURE GetSchedDelay (
            VARIABLE   NewDelay : OUT VitalTimeArray;
            VARIABLE  GlchDelay : OUT VitalTimeArray;
            CONSTANT   NewValue : IN  std_logic_vector;
            CONSTANT   CurValue : IN  std_logic_vector;
            CONSTANT   NewSched : IN  SchedArray
    ) IS
        VARIABLE Tim, Glch : TIME;
        ALIAS  NewDelayAlias : VitalTimeArray( NewDelay'LENGTH DOWNTO 1)
               IS NewDelay;
        ALIAS GlchDelayAlias : VitalTimeArray(GlchDelay'LENGTH DOWNTO 1)
               IS GlchDelay;
        ALIAS  NewSchedAlias : SchedArray( NewSched'LENGTH DOWNTO 1)
               IS NewSched;
        ALIAS  NewValueAlias : std_logic_vector (  NewValue'LENGTH DOWNTO 1 )
                                IS  NewValue;
        ALIAS  CurValueAlias : std_logic_vector (  CurValue'LENGTH DOWNTO 1 )
                                IS  CurValue;
    BEGIN
      FOR n IN NewDelay'LENGTH DOWNTO 1 LOOP
        CASE To_UX01(NewValueAlias(n)) IS
          WHEN '0'    => Tim  := NewSchedAlias(n).inp0;
                         Glch := NewSchedAlias(n).Glch1;
          WHEN '1'    => Tim  := NewSchedAlias(n).inp1;
                         Glch := NewSchedAlias(n).Glch0;
          WHEN OTHERS => Tim  := NewSchedAlias(n).InpX;
                         Glch := -1 ns;
        END CASE;
        IF (CurValueAlias(n) /= NewValueAlias(n))
          THEN Glch := -1 ns;
        END IF;

        NewDelayAlias(n) := Tim  - NOW;
        IF Glch < 0 ns
            THEN GlchDelayAlias(n) := Glch;
            ELSE GlchDelayAlias(n) := Glch - NOW;
        END IF; -- glch < 0 ns
      END LOOP;
      RETURN;
    END;

    PROCEDURE GetSchedDelay (
            VARIABLE   NewDelay : OUT TIME;
            VARIABLE  GlchDelay : OUT TIME;
            CONSTANT   NewValue : IN  std_ulogic;
            CONSTANT   CurValue : IN  std_ulogic;
            CONSTANT   NewSched : IN  SchedType;
            CONSTANT   EnSched1 : IN  SchedType;
            CONSTANT   EnSched0 : IN  SchedType
    ) IS
        SUBTYPE v2 IS std_logic_vector(0 TO 1);
        VARIABLE Tim, Glch : TIME;
    BEGIN

        CASE v2'(To_X01Z(CurValue) & To_X01Z(NewValue)) IS
          WHEN "00"    => Tim  := Maximum (NewSched.inp0, EnSched0.inp1);
                          Glch := GlitchMinTime(NewSched.Glch1,EnSched0.Glch0);
          WHEN "01"    => Tim  := Maximum (NewSched.inp1, EnSched1.inp1);
                          Glch := EnSched1.Glch0;
          WHEN "0Z"    => Tim  := EnSched0.inp0;
                          Glch := NewSched.Glch1;
          WHEN "0X"    => Tim  := Maximum (NewSched.InpX, EnSched1.InpX);
                          Glch := 0 ns;
          WHEN "10"    => Tim  := Maximum (NewSched.inp0, EnSched0.inp1);
                          Glch := EnSched0.Glch0;
          WHEN "11"    => Tim  := Maximum (NewSched.inp1, EnSched1.inp1);
                          Glch := GlitchMinTime(NewSched.Glch0,EnSched1.Glch0);
          WHEN "1Z"    => Tim  := EnSched1.inp0;
                          Glch := NewSched.Glch0;
          WHEN "1X"    => Tim  := Maximum (NewSched.InpX, EnSched0.InpX);
                          Glch := 0 ns;
          WHEN "Z0"    => Tim  := Maximum (NewSched.inp0, EnSched0.inp1);
                          IF NewSched.Glch0 > NOW
                            THEN Glch := Maximum(NewSched.Glch1,EnSched1.inp1);
                            ELSE Glch := 0 ns;
                          END IF;
          WHEN "Z1"    => Tim  := Maximum (NewSched.inp1, EnSched1.inp1);
                          IF NewSched.Glch1 > NOW
                            THEN Glch := Maximum(NewSched.Glch0,EnSched0.inp1);
                            ELSE Glch := 0 ns;
                          END IF;
          WHEN "ZX"    => Tim  := Maximum (NewSched.InpX, EnSched1.InpX);
                          Glch := 0 ns;
          WHEN "ZZ"    => Tim  := Maximum (EnSched1.InpX, EnSched0.InpX);
                          Glch := 0 ns;
          WHEN "X0"    => Tim  := Maximum (NewSched.inp0, EnSched0.inp1);
                          Glch := 0 ns;
          WHEN "X1"    => Tim  := Maximum (NewSched.inp1, EnSched1.inp1);
                          Glch := 0 ns;
          WHEN "XZ"    => Tim  := Maximum (EnSched1.InpX, EnSched0.InpX);
                          Glch := 0 ns;
          WHEN OTHERS  => Tim  := Maximum (NewSched.InpX, EnSched1.InpX);
                          Glch := 0 ns;

        END CASE;
        NewDelay  := Tim  - NOW;
        IF Glch < 0 ns
            THEN GlchDelay := Glch;
            ELSE GlchDelay := Glch - NOW;
        END IF; -- glch < 0 ns
    END;

    ---------------------------------------------------------------------------
    -- Operators and Functions for combination (selection) of path delays
    -- > These functions support selection of the "appripriate" path delay
    --   dependent on the logic function.
    -- > These functions only "select" from the possable output times. No
    --   calculation (addition) of delays is performed.
    -- > See description of 'BufPath', 'InvPath' and 'GetSchedDelay'
    -- > See primitive PROCEDURE models for examples.
    ---------------------------------------------------------------------------

    FUNCTION "not"  (
            CONSTANT a : IN SchedType
          ) RETURN SchedType IS
        VARIABLE z : SchedType;
    BEGIN
        z.inp1  := a.inp0 ;
        z.inp0  := a.inp1 ;
        z.InpX  := a.InpX ;
        z.Glch1 := a.Glch0;
        z.Glch0 := a.Glch1;
        RETURN (z);
    END;

    FUNCTION "and"  (
            CONSTANT a, b : IN SchedType
          ) RETURN SchedType IS
        VARIABLE z : SchedType;
    BEGIN
        z.inp1  := Maximum   ( a.inp1 , b.inp1  );
        z.inp0  := Minimum   ( a.inp0 , b.inp0  );
        z.InpX  := GlitchMinTime ( a.InpX , b.InpX  );
        z.Glch1 := Maximum   ( a.Glch1, b.Glch1 );
        z.Glch0 := GlitchMinTime ( a.Glch0, b.Glch0 );
        RETURN (z);
    END;

    FUNCTION "or"   (
            CONSTANT a, b : IN SchedType
          ) RETURN SchedType IS
        VARIABLE z : SchedType;
    BEGIN
        z.inp0  := Maximum   ( a.inp0 , b.inp0  );
        z.inp1  := Minimum   ( a.inp1 , b.inp1  );
        z.InpX  := GlitchMinTime ( a.InpX , b.InpX  );
        z.Glch0 := Maximum   ( a.Glch0, b.Glch0 );
        z.Glch1 := GlitchMinTime ( a.Glch1, b.Glch1 );
        RETURN (z);
    END;

    IMPURE FUNCTION "nand" (
            CONSTANT a, b : IN SchedType
          ) RETURN SchedType IS
        VARIABLE z : SchedType;
    BEGIN
        z.inp0  := Maximum   ( a.inp1 , b.inp1  );
        z.inp1  := Minimum   ( a.inp0 , b.inp0  );
        z.InpX  := GlitchMinTime ( a.InpX , b.InpX  );
        z.Glch0 := Maximum   ( a.Glch1, b.Glch1 );
        z.Glch1 := GlitchMinTime ( a.Glch0, b.Glch0 );
        RETURN (z);
    END;

    IMPURE FUNCTION "nor"  (
            CONSTANT a, b : IN SchedType
          ) RETURN SchedType IS
        VARIABLE z : SchedType;
    BEGIN
        z.inp1  := Maximum   ( a.inp0 , b.inp0  );
        z.inp0  := Minimum   ( a.inp1 , b.inp1  );
        z.InpX  := GlitchMinTime ( a.InpX , b.InpX  );
        z.Glch1 := Maximum   ( a.Glch0, b.Glch0 );
        z.Glch0 := GlitchMinTime ( a.Glch1, b.Glch1 );
        RETURN (z);
    END;

    -- ------------------------------------------------------------------------
    -- Delay Calculation for 2-bit Logical gates.
    -- ------------------------------------------------------------------------
    IMPURE FUNCTION VitalXOR2   (
            CONSTANT ab,ai, bb,bi : IN SchedType
          ) RETURN SchedType IS
        VARIABLE z : SchedType;
    BEGIN
        -- z = (a AND b) NOR (a NOR b)
        z.inp1  :=   Maximum (  Minimum (ai.inp0 , bi.inp0 ),
                                Minimum (ab.inp1 , bb.inp1 ) );
        z.inp0  :=   Minimum (  Maximum (ai.inp1 , bi.inp1 ),
                                Maximum (ab.inp0 , bb.inp0 ) );
        z.InpX  :=   Maximum (  Maximum (ai.InpX , bi.InpX ),
                                Maximum (ab.InpX , bb.InpX ) );
        z.Glch1 :=   Maximum (GlitchMinTime (ai.Glch0, bi.Glch0),
                              GlitchMinTime (ab.Glch1, bb.Glch1) );
        z.Glch0 := GlitchMinTime (  Maximum (ai.Glch1, bi.Glch1),
                                Maximum (ab.Glch0, bb.Glch0) );
        RETURN (z);
    END;

    IMPURE FUNCTION VitalXNOR2  (
            CONSTANT ab,ai, bb,bi : IN SchedType
          ) RETURN SchedType IS
        VARIABLE z : SchedType;
    BEGIN
        -- z = (a AND b) OR (a NOR b)
        z.inp0  :=   Maximum (  Minimum (ab.inp0 , bb.inp0 ),
                                Minimum (ai.inp1 , bi.inp1 ) );
        z.inp1  :=   Minimum (  Maximum (ab.inp1 , bb.inp1 ),
                                Maximum (ai.inp0 , bi.inp0 ) );
        z.InpX  :=   Maximum (  Maximum (ab.InpX , bb.InpX ),
                                Maximum (ai.InpX , bi.InpX ) );
        z.Glch0 :=   Maximum (GlitchMinTime (ab.Glch0, bb.Glch0),
                              GlitchMinTime (ai.Glch1, bi.Glch1) );
        z.Glch1 := GlitchMinTime (  Maximum (ab.Glch1, bb.Glch1),
                                Maximum (ai.Glch0, bi.Glch0) );
        RETURN (z);
    END;

    -- ------------------------------------------------------------------------
    -- Delay Calculation for 3-bit Logical gates.
    -- ------------------------------------------------------------------------
    IMPURE FUNCTION VitalXOR3   (
            CONSTANT ab,ai, bb,bi, cb,ci : IN SchedType )
      RETURN SchedType IS
    BEGIN
        RETURN VitalXOR2 ( VitalXOR2 (ab,ai, bb,bi),
                           VitalXOR2 (ai,ab, bi,bb),
                           cb, ci );
    END;

    IMPURE FUNCTION VitalXNOR3  (
            CONSTANT ab,ai, bb,bi, cb,ci : IN SchedType )
      RETURN SchedType IS
    BEGIN
        RETURN VitalXNOR2 ( VitalXOR2 ( ab,ai, bb,bi ),
                            VitalXOR2 ( ai,ab, bi,bb ),
                            cb, ci );
    END;

    -- ------------------------------------------------------------------------
    -- Delay Calculation for 4-bit Logical gates.
    -- ------------------------------------------------------------------------
    IMPURE FUNCTION VitalXOR4   (
            CONSTANT ab,ai, bb,bi, cb,ci, db,di : IN SchedType )
      RETURN SchedType IS
    BEGIN
        RETURN VitalXOR2 ( VitalXOR2 ( ab,ai, bb,bi ),
                           VitalXOR2 ( ai,ab, bi,bb ),
                           VitalXOR2 ( cb,ci, db,di ),
                           VitalXOR2 ( ci,cb, di,db ) );
    END;

    IMPURE FUNCTION VitalXNOR4  (
            CONSTANT ab,ai, bb,bi, cb,ci, db,di : IN SchedType )
      RETURN SchedType IS
    BEGIN
        RETURN VitalXNOR2 ( VitalXOR2 ( ab,ai, bb,bi ),
                            VitalXOR2 ( ai,ab, bi,bb ),
                            VitalXOR2 ( cb,ci, db,di ),
                            VitalXOR2 ( ci,cb, di,db ) );
    END;

    -- ------------------------------------------------------------------------
    -- Delay Calculation for N-bit Logical gates.
    -- ------------------------------------------------------------------------
    -- Note: index range on datab,datai assumed to be 1 TO length.
    --       This is enforced by internal only usage of this Function
    IMPURE FUNCTION VitalXOR   (
            CONSTANT DataB, DataI : IN SchedArray
          ) RETURN SchedType IS
            CONSTANT Leng : INTEGER := DataB'LENGTH;
    BEGIN
        IF Leng = 2 THEN
            RETURN VitalXOR2 ( DataB(1),DataI(1), DataB(2),DataI(2) );
        ELSE
            RETURN VitalXOR2 ( VitalXOR ( DataB(1 TO Leng-1),
                                          DataI(1 TO Leng-1) ),
                               VitalXOR ( DataI(1 TO Leng-1),
                                          DataB(1 TO Leng-1) ),
                               DataB(Leng),DataI(Leng) );
        END IF;
    END;

    -- Note: index range on datab,datai assumed to be 1 TO length.
    --       This is enforced by internal only usage of this Function
    IMPURE FUNCTION VitalXNOR  (
            CONSTANT DataB, DataI : IN SchedArray
          ) RETURN SchedType IS
            CONSTANT Leng : INTEGER := DataB'LENGTH;
    BEGIN
        IF Leng = 2 THEN
            RETURN VitalXNOR2 ( DataB(1),DataI(1), DataB(2),DataI(2) );
        ELSE
            RETURN VitalXNOR2 ( VitalXOR ( DataB(1 TO Leng-1),
                                           DataI(1 TO Leng-1) ),
                                VitalXOR ( DataI(1 TO Leng-1),
                                           DataB(1 TO Leng-1) ),
                                DataB(Leng),DataI(Leng) );
        END IF;
    END;

    -- ------------------------------------------------------------------------
    -- Multiplexor
    --   MUX   .......... result := data(dselect)
    --   MUX2  .......... 2-input mux; result := data0 when (dselect = '0'),
    --                                           data1 when (dselect = '1'),
    --                        'X' when (dselect = 'X') and (data0 /= data1)
    --   MUX4  .......... 4-input mux; result := data(dselect)
    --   MUX8  .......... 8-input mux; result := data(dselect)
    -- ------------------------------------------------------------------------
    FUNCTION VitalMUX2  (
            CONSTANT d1, d0 : IN SchedType;
            CONSTANT sb, SI : IN SchedType
          ) RETURN SchedType IS
    BEGIN
        RETURN (d1 AND sb) OR (d0 AND (NOT SI) );
    END;
--
    FUNCTION VitalMUX4  (
            CONSTANT Data : IN SchedArray4;
            CONSTANT sb   : IN SchedArray2;
            CONSTANT SI   : IN SchedArray2
          ) RETURN SchedType IS
    BEGIN
        RETURN    (      sb(1)  AND VitalMUX2(Data(3),Data(2), sb(0), SI(0)) )
               OR ( (NOT SI(1)) AND VitalMUX2(Data(1),Data(0), sb(0), SI(0)) );
    END;

    FUNCTION VitalMUX8  (
            CONSTANT Data : IN SchedArray8;
            CONSTANT sb   : IN SchedArray3;
            CONSTANT SI   : IN SchedArray3
          ) RETURN SchedType IS
    BEGIN
        RETURN    ( (    sb(2)) AND VitalMUX4 (Data(7 DOWNTO 4),
                                           sb(1 DOWNTO 0), SI(1 DOWNTO 0) ) )
               OR ( (NOT SI(2)) AND VitalMUX4 (Data(3 DOWNTO 0),
                                           sb(1 DOWNTO 0), SI(1 DOWNTO 0) ) );
    END;
--
    FUNCTION VInterMux   (
            CONSTANT Data : IN SchedArray;
            CONSTANT sb   : IN SchedArray;
            CONSTANT SI   : IN SchedArray
          ) RETURN SchedType IS
        CONSTANT sMsb : INTEGER := sb'LENGTH;
        CONSTANT dMsbHigh : INTEGER := Data'LENGTH;
        CONSTANT dMsbLow  : INTEGER := Data'LENGTH/2;
    BEGIN
        IF sb'LENGTH = 1 THEN
          RETURN VitalMUX2( Data(2), Data(1), sb(1), SI(1) );
        ELSIF sb'LENGTH = 2 THEN
          RETURN VitalMUX4( Data, sb, SI );
        ELSIF sb'LENGTH = 3 THEN
          RETURN VitalMUX8( Data, sb, SI );
        ELSIF sb'LENGTH > 3 THEN
          RETURN ((    sb(sMsb)) AND VInterMux( Data(dMsbLow  DOWNTO  1),
                                                  sb(sMsb-1 DOWNTO 1),
                                                  SI(sMsb-1 DOWNTO 1) ))
              OR ((NOT SI(sMsb)) AND VInterMux( Data(dMsbHigh DOWNTO dMsbLow+1),
                                                  sb(sMsb-1 DOWNTO 1),
                                                  SI(sMsb-1 DOWNTO 1) ));
        ELSE
          RETURN (0 ns, 0 ns, 0 ns, 0 ns, 0 ns); -- dselect'LENGTH < 1
        END IF;
    END;
--
    FUNCTION VitalMUX   (
            CONSTANT Data : IN SchedArray;
            CONSTANT sb   : IN SchedArray;
            CONSTANT SI   : IN SchedArray
          ) RETURN SchedType IS
        CONSTANT msb : INTEGER := 2**sb'LENGTH;
        VARIABLE    lDat : SchedArray(msb DOWNTO 1);
        ALIAS DataAlias : SchedArray ( Data'LENGTH DOWNTO 1 ) IS Data;
        ALIAS   sbAlias : SchedArray (   sb'LENGTH DOWNTO 1 ) IS sb;
        ALIAS   siAlias : SchedArray (   SI'LENGTH DOWNTO 1 ) IS SI;
    BEGIN
        IF Data'LENGTH <= msb THEN
            FOR i IN Data'LENGTH DOWNTO 1 LOOP
                lDat(i) := DataAlias(i);
            END LOOP;
            FOR i IN msb DOWNTO Data'LENGTH+1 LOOP
                lDat(i) := DefSchedAnd;
            END LOOP;
        ELSE
            FOR i IN msb DOWNTO 1 LOOP
                lDat(i) := DataAlias(i);
            END LOOP;
        END IF;
        RETURN VInterMux( lDat, sbAlias, siAlias );
    END;

    -- ------------------------------------------------------------------------
    -- Decoder
    --          General Algorithm :
    --              (a) Result(...) := '0' when (enable = '0')
    --              (b) Result(data) := '1'; all other subelements = '0'
    --              ... Result array is decending (n-1 downto 0)
    --
    --          DECODERn  .......... n:2**n decoder
    -- ------------------------------------------------------------------------
    FUNCTION VitalDECODER2  (
            CONSTANT DataB  : IN SchedType;
            CONSTANT DataI  : IN SchedType;
            CONSTANT Enable : IN SchedType
          ) RETURN SchedArray IS
        VARIABLE Result : SchedArray2;
    BEGIN
        Result(1) := Enable AND (    DataB);
        Result(0) := Enable AND (NOT DataI);
        RETURN Result;
    END;

    FUNCTION VitalDECODER4  (
            CONSTANT DataB  : IN SchedArray2;
            CONSTANT DataI  : IN SchedArray2;
            CONSTANT Enable : IN SchedType
          ) RETURN SchedArray IS
        VARIABLE Result : SchedArray4;
    BEGIN
        Result(3) := Enable AND (    DataB(1)) AND (    DataB(0));
        Result(2) := Enable AND (    DataB(1)) AND (NOT DataI(0));
        Result(1) := Enable AND (NOT DataI(1)) AND (    DataB(0));
        Result(0) := Enable AND (NOT DataI(1)) AND (NOT DataI(0));
        RETURN Result;
    END;

    FUNCTION VitalDECODER8  (
            CONSTANT DataB  : IN SchedArray3;
            CONSTANT DataI  : IN SchedArray3;
            CONSTANT Enable : IN SchedType
          ) RETURN SchedArray IS
        VARIABLE Result : SchedArray8;
    BEGIN
        Result(7):= Enable AND (    DataB(2))AND(    DataB(1))AND(    DataB(0));
        Result(6):= Enable AND (    DataB(2))AND(    DataB(1))AND(NOT DataI(0));
        Result(5):= Enable AND (    DataB(2))AND(NOT DataI(1))AND(    DataB(0));
        Result(4):= Enable AND (    DataB(2))AND(NOT DataI(1))AND(NOT DataI(0));
        Result(3):= Enable AND (NOT DataI(2))AND(    DataB(1))AND(    DataB(0));
        Result(2):= Enable AND (NOT DataI(2))AND(    DataB(1))AND(NOT DataI(0));
        Result(1):= Enable AND (NOT DataI(2))AND(NOT DataI(1))AND(    DataB(0));
        Result(0):= Enable AND (NOT DataI(2))AND(NOT DataI(1))AND(NOT DataI(0));
        RETURN Result;
    END;


    FUNCTION VitalDECODER   (
            CONSTANT DataB  : IN SchedArray;
            CONSTANT DataI  : IN SchedArray;
            CONSTANT Enable : IN SchedType
          ) RETURN SchedArray IS
        CONSTANT DMsb : INTEGER := DataB'LENGTH - 1;
        ALIAS DataBAlias : SchedArray ( DMsb DOWNTO 0 ) IS DataB;
        ALIAS DataIAlias : SchedArray ( DMsb DOWNTO 0 ) IS DataI;
    BEGIN
        IF DataB'LENGTH = 1 THEN
            RETURN  VitalDECODER2 ( DataBAlias(    0     ),
                                    DataIAlias(    0     ), Enable );
        ELSIF DataB'LENGTH = 2 THEN
            RETURN  VitalDECODER4 ( DataBAlias(1 DOWNTO 0),
                                    DataIAlias(1 DOWNTO 0), Enable );
        ELSIF DataB'LENGTH = 3 THEN
            RETURN  VitalDECODER8 ( DataBAlias(2 DOWNTO 0),
                                    DataIAlias(2 DOWNTO 0), Enable );
        ELSIF DataB'LENGTH > 3 THEN
            RETURN  VitalDECODER  ( DataBAlias(DMsb-1 DOWNTO 0),
                                    DataIAlias(DMsb-1 DOWNTO 0),
                                    Enable AND (    DataBAlias(DMsb)) )
                  & VitalDECODER  ( DataBAlias(DMsb-1 DOWNTO 0),
                                    DataIAlias(DMsb-1 DOWNTO 0),
                                    Enable AND (NOT DataIAlias(DMsb)) );
        ELSE
            RETURN DefSchedArray2;
        END IF;
    END;


-------------------------------------------------------------------------------
-- PRIMITIVES
-------------------------------------------------------------------------------
    -- ------------------------------------------------------------------------
    -- N-bit wide Logical gates.
    -- ------------------------------------------------------------------------
    FUNCTION VitalAND    (
            CONSTANT       Data :  IN std_logic_vector;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
        VARIABLE Result : UX01;
    BEGIN
        Result := '1';
        FOR i IN Data'RANGE LOOP
            Result := Result AND Data(i);
        END LOOP;
        RETURN ResultMap(Result);
    END;
--
    FUNCTION VitalOR     (
            CONSTANT       Data :  IN std_logic_vector;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
        VARIABLE Result : UX01;
    BEGIN
        Result := '0';
        FOR i IN Data'RANGE LOOP
            Result := Result OR Data(i);
        END LOOP;
        RETURN ResultMap(Result);
    END;
--
    FUNCTION VitalXOR    (
            CONSTANT       Data :  IN std_logic_vector;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
        VARIABLE Result : UX01;
    BEGIN
        Result := '0';
        FOR i IN Data'RANGE LOOP
            Result := Result XOR Data(i);
        END LOOP;
        RETURN ResultMap(Result);
    END;
--
    FUNCTION VitalNAND   (
            CONSTANT       Data :  IN std_logic_vector;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
        VARIABLE Result : UX01;
    BEGIN
        Result := '1';
        FOR i IN Data'RANGE LOOP
            Result := Result AND Data(i);
        END LOOP;
        RETURN ResultMap(NOT Result);
    END;
--
    FUNCTION VitalNOR    (
            CONSTANT       Data :  IN std_logic_vector;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
        VARIABLE Result : UX01;
    BEGIN
        Result := '0';
        FOR i IN Data'RANGE LOOP
            Result := Result OR Data(i);
        END LOOP;
        RETURN ResultMap(NOT Result);
    END;
--
    FUNCTION VitalXNOR   (
            CONSTANT       Data :  IN std_logic_vector;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
        VARIABLE Result : UX01;
    BEGIN
        Result := '0';
        FOR i IN Data'RANGE LOOP
            Result := Result XOR Data(i);
        END LOOP;
        RETURN ResultMap(NOT Result);
    END;

    -- ------------------------------------------------------------------------
    -- Commonly used 2-bit Logical gates.
    -- ------------------------------------------------------------------------
    FUNCTION VitalAND2   (
            CONSTANT       a, b :  IN std_ulogic;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(a AND b);
    END;
--
    FUNCTION VitalOR2    (
            CONSTANT       a, b :  IN std_ulogic;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(a OR b);
    END;
--
    FUNCTION VitalXOR2   (
            CONSTANT       a, b :  IN std_ulogic;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(a XOR b);
    END;
--
    FUNCTION VitalNAND2  (
            CONSTANT       a, b :  IN std_ulogic;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(a NAND b);
    END;
--
    FUNCTION VitalNOR2   (
            CONSTANT       a, b :  IN std_ulogic;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(a NOR b);
    END;
--
    FUNCTION VitalXNOR2  (
            CONSTANT       a, b :  IN std_ulogic;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(NOT (a XOR b));
    END;
--
    -- ------------------------------------------------------------------------
    -- Commonly used 3-bit Logical gates.
    -- ------------------------------------------------------------------------
    FUNCTION VitalAND3   (
            CONSTANT    a, b, c :  IN std_ulogic;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(a AND b AND c);
    END;
--
    FUNCTION VitalOR3    (
            CONSTANT    a, b, c :  IN std_ulogic;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(a OR b OR c);
    END;
--
    FUNCTION VitalXOR3   (
            CONSTANT    a, b, c :  IN std_ulogic;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(a XOR b XOR c);
    END;
--
    FUNCTION VitalNAND3  (
            CONSTANT    a, b, c :  IN std_ulogic;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(NOT (a AND b AND c));
    END;
--
    FUNCTION VitalNOR3   (
            CONSTANT    a, b, c :  IN std_ulogic;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(NOT (a OR b OR c));
    END;
--
    FUNCTION VitalXNOR3  (
            CONSTANT    a, b, c :  IN std_ulogic;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(NOT (a XOR b XOR c));
    END;

    -- ---------------------------------------------------------------------------
    -- Commonly used 4-bit Logical gates.
    -- ---------------------------------------------------------------------------
    FUNCTION VitalAND4   (
            CONSTANT a, b, c, d :  IN std_ulogic;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(a AND b AND c AND d);
    END;
--
    FUNCTION VitalOR4    (
            CONSTANT a, b, c, d :  IN std_ulogic;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(a OR b OR c OR d);
    END;
--
    FUNCTION VitalXOR4   (
            CONSTANT a, b, c, d :  IN std_ulogic;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(a XOR b XOR c XOR d);
    END;
--
    FUNCTION VitalNAND4  (
            CONSTANT a, b, c, d :  IN std_ulogic;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(NOT (a AND b AND c AND d));
    END;
--
    FUNCTION VitalNOR4   (
            CONSTANT a, b, c, d :  IN std_ulogic;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(NOT (a OR b OR c OR d));
    END;
--
    FUNCTION VitalXNOR4  (
            CONSTANT a, b, c, d :  IN std_ulogic;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                      := VitalDefaultResultMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(NOT (a XOR b XOR c XOR d));
    END;

    -- ------------------------------------------------------------------------
    -- Buffers
    --   BUF    ....... standard non-inverting buffer
    --   BUFIF0 ....... non-inverting buffer Data passes thru if (Enable = '0')
    --   BUFIF1 ....... non-inverting buffer Data passes thru if (Enable = '1')
    -- ------------------------------------------------------------------------
    FUNCTION VitalBUF    (
            CONSTANT         Data :  IN std_ulogic;
            CONSTANT    ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(To_UX01(Data));
    END;
--
    FUNCTION VitalBUFIF0 (
            CONSTANT Data, Enable :  IN std_ulogic;
            CONSTANT    ResultMap :  IN VitalResultZMapType
                                        := VitalDefaultResultZMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(BufIf0_Table(Enable,Data));
    END;
--
    FUNCTION VitalBUFIF1 (
            CONSTANT Data, Enable :  IN std_ulogic;
            CONSTANT    ResultMap :  IN VitalResultZMapType
                                        := VitalDefaultResultZMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(BufIf1_Table(Enable,Data));
    END;
    FUNCTION VitalIDENT  (
            CONSTANT         Data :  IN std_ulogic;
            CONSTANT    ResultMap :  IN VitalResultZMapType
                                        := VitalDefaultResultZMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(To_UX01Z(Data));
    END;

    -- ------------------------------------------------------------------------
    -- Invertors
    --   INV    ......... standard inverting buffer
    --   INVIF0 ......... inverting buffer Data passes thru if (Enable = '0')
    --   INVIF1 ......... inverting buffer Data passes thru if (Enable = '1')
    -- ------------------------------------------------------------------------
    FUNCTION VitalINV    (
            CONSTANT         Data :  IN std_ulogic;
            CONSTANT    ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(NOT Data);
    END;
--
    FUNCTION VitalINVIF0 (
            CONSTANT Data, Enable :  IN std_ulogic;
            CONSTANT    ResultMap :  IN VitalResultZMapType
                                        := VitalDefaultResultZMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(InvIf0_Table(Enable,Data));
    END;
--
    FUNCTION VitalINVIF1 (
            CONSTANT Data, Enable :  IN std_ulogic;
            CONSTANT    ResultMap :  IN VitalResultZMapType
                                        := VitalDefaultResultZMap
          ) RETURN std_ulogic IS
    BEGIN
        RETURN ResultMap(InvIf1_Table(Enable,Data));
    END;

    -- ------------------------------------------------------------------------
    -- Multiplexor
    --   MUX   .......... result := data(dselect)
    --   MUX2  .......... 2-input mux; result := data0 when (dselect = '0'),
    --                                           data1 when (dselect = '1'),
    --                        'X' when (dselect = 'X') and (data0 /= data1)
    --   MUX4  .......... 4-input mux; result := data(dselect)
    --   MUX8  .......... 8-input mux; result := data(dselect)
    -- ------------------------------------------------------------------------
    FUNCTION VitalMUX2  (
            CONSTANT Data1, Data0 :  IN std_ulogic;
            CONSTANT      dSelect :  IN std_ulogic;
            CONSTANT    ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
          ) RETURN std_ulogic IS
        VARIABLE Result : UX01;
    BEGIN
        CASE To_X01(dSelect) IS
          WHEN '0'    => Result := To_UX01(Data0);
          WHEN '1'    => Result := To_UX01(Data1);
          WHEN OTHERS => Result := VitalSame( Data1, Data0 );
        END CASE;
        RETURN ResultMap(Result);
    END;
--
    FUNCTION VitalMUX4  (
            CONSTANT       Data :  IN std_logic_vector4;
            CONSTANT    dSelect :  IN std_logic_vector2;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
          ) RETURN std_ulogic IS
        VARIABLE Slct : std_logic_vector2;
        VARIABLE Result : UX01;
    BEGIN
        Slct := To_X01(dSelect);
        CASE Slct IS
          WHEN "00"   => Result := To_UX01(Data(0));
          WHEN "01"   => Result := To_UX01(Data(1));
          WHEN "10"   => Result := To_UX01(Data(2));
          WHEN "11"   => Result := To_UX01(Data(3));
          WHEN "0X"   => Result := VitalSame( Data(1), Data(0) );
          WHEN "1X"   => Result := VitalSame( Data(2), Data(3) );
          WHEN "X0"   => Result := VitalSame( Data(2), Data(0) );
          WHEN "X1"   => Result := VitalSame( Data(3), Data(1) );
          WHEN OTHERS => Result := VitalSame( VitalSame(Data(3),Data(2)),
                                              VitalSame(Data(1),Data(0)));
        END CASE;
        RETURN ResultMap(Result);
    END;
--
    FUNCTION VitalMUX8  (
            CONSTANT       Data :  IN std_logic_vector8;
            CONSTANT    dSelect :  IN std_logic_vector3;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
          ) RETURN std_ulogic IS
        VARIABLE Result : UX01;
    BEGIN
        CASE To_X01(dSelect(2)) IS
          WHEN '0'    => Result := VitalMUX4( Data(3 DOWNTO 0),
                                              dSelect(1 DOWNTO 0));
          WHEN '1'    => Result := VitalMUX4( Data(7 DOWNTO 4),
                                              dSelect(1 DOWNTO 0));
          WHEN OTHERS => Result := VitalSame( VitalMUX4( Data(3 DOWNTO 0),
                                                         dSelect(1 DOWNTO 0)),
                                              VitalMUX4( Data(7 DOWNTO 4),
                                                         dSelect(1 DOWNTO 0)));
        END CASE;
        RETURN ResultMap(Result);
    END;
--
    FUNCTION VInterMux    (
            CONSTANT Data    : IN std_logic_vector;
            CONSTANT dSelect : IN std_logic_vector
          ) RETURN std_ulogic IS

        CONSTANT sMsb     : INTEGER := dSelect'LENGTH;
        CONSTANT dMsbHigh : INTEGER := Data'LENGTH;
        CONSTANT dMsbLow  : INTEGER := Data'LENGTH/2;
        ALIAS DataAlias : std_logic_vector (   Data'LENGTH DOWNTO 1) IS Data;
        ALIAS dSelAlias : std_logic_vector (dSelect'LENGTH DOWNTO 1) IS dSelect;

        VARIABLE Result : UX01;
    BEGIN
        IF dSelect'LENGTH = 1 THEN
            Result := VitalMUX2( DataAlias(2), DataAlias(1), dSelAlias(1) );
        ELSIF dSelect'LENGTH = 2 THEN
            Result := VitalMUX4( DataAlias, dSelAlias );
        ELSIF dSelect'LENGTH > 2 THEN
          CASE To_X01(dSelect(sMsb)) IS
            WHEN '0'    =>
              Result := VInterMux( DataAlias(dMsbLow  DOWNTO          1),
                                   dSelAlias(sMsb-1 DOWNTO 1) );
            WHEN '1'    =>
              Result := VInterMux( DataAlias(dMsbHigh DOWNTO dMsbLow+1),
                                   dSelAlias(sMsb-1 DOWNTO 1) );
            WHEN OTHERS =>
              Result := VitalSame(
                              VInterMux( DataAlias(dMsbLow  DOWNTO          1),
                                         dSelAlias(sMsb-1 DOWNTO 1) ),
                              VInterMux( DataAlias(dMsbHigh DOWNTO dMsbLow+1),
                                         dSelAlias(sMsb-1 DOWNTO 1) )
                              );
          END CASE;
        ELSE
          Result := 'X'; -- dselect'LENGTH < 1
        END IF;
        RETURN Result;
    END;
--
    FUNCTION VitalMUX   (
            CONSTANT       Data :  IN std_logic_vector;
            CONSTANT    dSelect :  IN std_logic_vector;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
          ) RETURN std_ulogic IS
        CONSTANT msb    : INTEGER := 2**dSelect'LENGTH;
        ALIAS DataAlias : std_logic_vector (   Data'LENGTH DOWNTO 1) IS Data;
        ALIAS dSelAlias : std_logic_vector (dSelect'LENGTH DOWNTO 1) IS dSelect;
        VARIABLE lDat   : std_logic_vector(msb DOWNTO 1) := (OTHERS=>'X');
        VARIABLE Result : UX01;
    BEGIN
        IF Data'LENGTH <= msb THEN
            FOR i IN Data'LENGTH DOWNTO 1 LOOP
                lDat(i) := DataAlias(i);
            END LOOP;
        ELSE
            FOR i IN msb DOWNTO 1 LOOP
                lDat(i) := DataAlias(i);
            END LOOP;
        END IF;
        Result := VInterMux( lDat, dSelAlias );
        RETURN ResultMap(Result);
    END;

    -- ------------------------------------------------------------------------
    -- Decoder
    --          General Algorithm :
    --              (a) Result(...) := '0' when (enable = '0')
    --              (b) Result(data) := '1'; all other subelements = '0'
    --              ... Result array is decending (n-1 downto 0)
    --
    --          DECODERn  .......... n:2**n decoder
    -- ------------------------------------------------------------------------
    FUNCTION VitalDECODER2  (
            CONSTANT       Data :  IN std_ulogic;
            CONSTANT     Enable :  IN std_ulogic;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
          ) RETURN std_logic_vector2 IS
        VARIABLE Result : std_logic_vector2;
    BEGIN
        Result(1) := ResultMap(Enable AND (    Data));
        Result(0) := ResultMap(Enable AND (NOT Data));
        RETURN Result;
    END;
--
    FUNCTION VitalDECODER4  (
            CONSTANT       Data :  IN std_logic_vector2;
            CONSTANT     Enable :  IN std_ulogic;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
          ) RETURN std_logic_vector4 IS
        VARIABLE Result : std_logic_vector4;
    BEGIN
        Result(3) := ResultMap(Enable AND (    Data(1)) AND (    Data(0)));
        Result(2) := ResultMap(Enable AND (    Data(1)) AND (NOT Data(0)));
        Result(1) := ResultMap(Enable AND (NOT Data(1)) AND (    Data(0)));
        Result(0) := ResultMap(Enable AND (NOT Data(1)) AND (NOT Data(0)));
        RETURN Result;
    END;
--
    FUNCTION VitalDECODER8  (
            CONSTANT       Data :  IN std_logic_vector3;
            CONSTANT     Enable :  IN std_ulogic;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
          ) RETURN std_logic_vector8 IS
        VARIABLE Result : std_logic_vector8;
    BEGIN
        Result(7) := (    Data(2)) AND (    Data(1)) AND (    Data(0));
        Result(6) := (    Data(2)) AND (    Data(1)) AND (NOT Data(0));
        Result(5) := (    Data(2)) AND (NOT Data(1)) AND (    Data(0));
        Result(4) := (    Data(2)) AND (NOT Data(1)) AND (NOT Data(0));
        Result(3) := (NOT Data(2)) AND (    Data(1)) AND (    Data(0));
        Result(2) := (NOT Data(2)) AND (    Data(1)) AND (NOT Data(0));
        Result(1) := (NOT Data(2)) AND (NOT Data(1)) AND (    Data(0));
        Result(0) := (NOT Data(2)) AND (NOT Data(1)) AND (NOT Data(0));

        Result(0) := ResultMap ( Enable AND Result(0) );
        Result(1) := ResultMap ( Enable AND Result(1) );
        Result(2) := ResultMap ( Enable AND Result(2) );
        Result(3) := ResultMap ( Enable AND Result(3) );
        Result(4) := ResultMap ( Enable AND Result(4) );
        Result(5) := ResultMap ( Enable AND Result(5) );
        Result(6) := ResultMap ( Enable AND Result(6) );
        Result(7) := ResultMap ( Enable AND Result(7) );

        RETURN Result;
    END;
--
    FUNCTION VitalDECODER   (
            CONSTANT       Data :  IN std_logic_vector;
            CONSTANT     Enable :  IN std_ulogic;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
          ) RETURN std_logic_vector IS

        CONSTANT DMsb : INTEGER := Data'LENGTH - 1;
        ALIAS DataAlias : std_logic_vector ( DMsb DOWNTO 0 ) IS Data;
    BEGIN
        IF    Data'LENGTH = 1 THEN
            RETURN VitalDECODER2 (DataAlias(    0     ), Enable, ResultMap );
        ELSIF Data'LENGTH = 2 THEN
            RETURN VitalDECODER4 (DataAlias(1 DOWNTO 0), Enable, ResultMap );
        ELSIF Data'LENGTH = 3 THEN
            RETURN VitalDECODER8 (DataAlias(2 DOWNTO 0), Enable, ResultMap );
        ELSIF Data'LENGTH > 3 THEN
            RETURN VitalDECODER  (DataAlias(DMsb-1 DOWNTO 0),
                                  Enable AND (    DataAlias(DMsb)), ResultMap )
                 & VitalDECODER  (DataAlias(DMsb-1 DOWNTO 0),
                                  Enable AND (NOT DataAlias(DMsb)), ResultMap );
        ELSE RETURN "X";
        END IF;
    END;

    -- ------------------------------------------------------------------------
    -- N-bit wide Logical gates.
    -- ------------------------------------------------------------------------
    PROCEDURE VitalAND   (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL         Data :  IN std_logic_vector;
            CONSTANT tpd_data_q :  IN VitalDelayArrayType01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE LastData  : std_logic_vector(Data'RANGE) := (OTHERS=>'U');
        VARIABLE Data_Edge :  EdgeArray(Data'RANGE);
        VARIABLE Data_Schd : SchedArray(Data'RANGE);
        VARIABLE NewValue     : UX01;
        VARIABLE Glitch_Data : GlitchDataType;
        VARIABLE new_schd    : SchedType;
        VARIABLE Dly, Glch   : TIME;
        ALIAS Atpd_data_q : VitalDelayArrayType01(Data'RANGE) IS tpd_data_q;
        VARIABLE AllZeroDelay  : BOOLEAN := TRUE; --SN
    BEGIN
      -- ------------------------------------------------------------------------
      --  Check if ALL zero delay paths, use simple model
      --   ( No delay selection, glitch detection required )
      -- ------------------------------------------------------------------------
      FOR i IN Data'RANGE LOOP
          IF (Atpd_data_q(i) /= VitalZeroDelay01) THEN
              AllZeroDelay := FALSE;
              EXIT;
          END IF;
      END LOOP;
      IF (AllZeroDelay) THEN LOOP
          q <= VitalAND(Data, ResultMap);
          WAIT ON Data;
      END LOOP;
      ELSE

        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        FOR n IN Data'RANGE LOOP
            BufPath ( Data_Schd(n), InitialEdge(Data(n)), Atpd_data_q(n) );
        END LOOP;

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        GetEdge ( Data, LastData, Data_Edge );
        BufPath ( Data_Schd, Data_Edge, Atpd_data_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue := '1';
        new_schd := Data_Schd(Data_Schd'LEFT);
        FOR i IN Data'RANGE LOOP
            NewValue  := NewValue  AND Data(i);
            new_schd := new_schd AND Data_Schd(i);
        END LOOP;

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON Data;
      END LOOP;
      END IF; --SN
    END;
--
    PROCEDURE VitalOR    (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL         Data :  IN std_logic_vector;
            CONSTANT tpd_data_q :  IN VitalDelayArrayType01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE LastData  : std_logic_vector(Data'RANGE) := (OTHERS=>'U');
        VARIABLE Data_Edge :  EdgeArray(Data'RANGE);
        VARIABLE Data_Schd : SchedArray(Data'RANGE);
        VARIABLE NewValue     : UX01;
        VARIABLE Glitch_Data : GlitchDataType;
        VARIABLE new_schd    : SchedType;
        VARIABLE Dly, Glch   : TIME;
        ALIAS Atpd_data_q : VitalDelayArrayType01(Data'RANGE) IS tpd_data_q;
        VARIABLE AllZeroDelay  : BOOLEAN := TRUE; --SN
    BEGIN
      -- ------------------------------------------------------------------------
      --  Check if ALL zero delay paths, use simple model
      --   ( No delay selection, glitch detection required )
      -- ------------------------------------------------------------------------
      FOR i IN Data'RANGE LOOP
          IF (Atpd_data_q(i) /= VitalZeroDelay01) THEN
              AllZeroDelay := FALSE;
              EXIT;
          END IF;
      END LOOP;
      IF (AllZeroDelay) THEN LOOP
          q <= VitalOR(Data, ResultMap);
          WAIT ON Data;
      END LOOP;
      ELSE

        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        FOR n IN Data'RANGE LOOP
            BufPath ( Data_Schd(n), InitialEdge(Data(n)), Atpd_data_q(n) );
        END LOOP;

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        GetEdge ( Data, LastData, Data_Edge );
        BufPath ( Data_Schd, Data_Edge, Atpd_data_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue := '0';
        new_schd := Data_Schd(Data_Schd'LEFT);
        FOR i IN Data'RANGE LOOP
            NewValue  := NewValue  OR Data(i);
            new_schd := new_schd OR Data_Schd(i);
        END LOOP;

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON Data;
      END LOOP;
      END IF; --SN
    END;
--
    PROCEDURE VitalXOR   (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL         Data :  IN std_logic_vector;
            CONSTANT tpd_data_q :  IN VitalDelayArrayType01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE LastData  : std_logic_vector(Data'RANGE) := (OTHERS=>'U');
        VARIABLE Data_Edge   :  EdgeArray(Data'RANGE);
        VARIABLE DataB_Schd  : SchedArray(1 TO Data'LENGTH);
        VARIABLE DataI_Schd  : SchedArray(1 TO Data'LENGTH);
        VARIABLE NewValue     : UX01;
        VARIABLE Glitch_Data : GlitchDataType;
        VARIABLE new_schd    : SchedType;
        VARIABLE Dly, Glch   : TIME;
        ALIAS Atpd_data_q : VitalDelayArrayType01(Data'RANGE) IS tpd_data_q;
        ALIAS ADataB_Schd : SchedArray(Data'RANGE) IS DataB_Schd;
        ALIAS ADataI_Schd : SchedArray(Data'RANGE) IS DataI_Schd;
        VARIABLE AllZeroDelay  : BOOLEAN := TRUE; --SN
    BEGIN
      -- ------------------------------------------------------------------------
      --  Check if ALL zero delay paths, use simple model
      --   ( No delay selection, glitch detection required )
      -- ------------------------------------------------------------------------
      FOR i IN Data'RANGE LOOP
          IF (Atpd_data_q(i) /= VitalZeroDelay01) THEN
              AllZeroDelay := FALSE;
              EXIT;
          END IF;
      END LOOP;
      IF (AllZeroDelay) THEN LOOP
          q <= VitalXOR(Data, ResultMap);
          WAIT ON Data;
      END LOOP;
      ELSE

        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        FOR n IN Data'RANGE LOOP
            BufPath ( ADataB_Schd(n), InitialEdge(Data(n)), Atpd_data_q(n) );
            InvPath ( ADataI_Schd(n), InitialEdge(Data(n)), Atpd_data_q(n) );
        END LOOP;

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        GetEdge ( Data, LastData, Data_Edge );
        BufPath ( ADataB_Schd, Data_Edge, Atpd_data_q );
        InvPath ( ADataI_Schd, Data_Edge, Atpd_data_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue  := VitalXOR ( Data );
        new_schd := VitalXOR ( DataB_Schd, DataI_Schd );

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON Data;
      END LOOP;
      END IF; --SN
    END;
--
    PROCEDURE VitalNAND  (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL         Data :  IN std_logic_vector;
            CONSTANT tpd_data_q :  IN VitalDelayArrayType01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE LastData  : std_logic_vector(Data'RANGE) := (OTHERS=>'U');
        VARIABLE Data_Edge :  EdgeArray(Data'RANGE);
        VARIABLE Data_Schd : SchedArray(Data'RANGE);
        VARIABLE NewValue     : UX01;
        VARIABLE Glitch_Data : GlitchDataType;
        VARIABLE new_schd    : SchedType;
        VARIABLE Dly, Glch   : TIME;
        ALIAS Atpd_data_q : VitalDelayArrayType01(Data'RANGE) IS tpd_data_q;
        VARIABLE AllZeroDelay  : BOOLEAN := TRUE; --SN
    BEGIN
      -- ------------------------------------------------------------------------
      --  Check if ALL zero delay paths, use simple model
      --   ( No delay selection, glitch detection required )
      -- ------------------------------------------------------------------------
      FOR i IN Data'RANGE LOOP
          IF (Atpd_data_q(i) /= VitalZeroDelay01) THEN
              AllZeroDelay := FALSE;
              EXIT;
          END IF;
      END LOOP;
      IF (AllZeroDelay) THEN LOOP
          q <= VitalNAND(Data, ResultMap);
          WAIT ON Data;
      END LOOP;
      ELSE

        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        FOR n IN Data'RANGE LOOP
            InvPath ( Data_Schd(n), InitialEdge(Data(n)), Atpd_data_q(n) );
        END LOOP;

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        GetEdge ( Data, LastData, Data_Edge );
        InvPath ( Data_Schd, Data_Edge, Atpd_data_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue := '1';
        new_schd := Data_Schd(Data_Schd'LEFT);
        FOR i IN Data'RANGE LOOP
            NewValue  := NewValue  AND Data(i);
            new_schd := new_schd AND Data_Schd(i);
        END LOOP;
        NewValue  := NOT NewValue;
        new_schd := NOT new_schd;

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON Data;
      END LOOP;
      END IF;
    END;
--
    PROCEDURE VitalNOR   (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL         Data :  IN std_logic_vector;
            CONSTANT tpd_data_q :  IN VitalDelayArrayType01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE LastData  : std_logic_vector(Data'RANGE) := (OTHERS=>'U');
        VARIABLE Data_Edge :  EdgeArray(Data'RANGE);
        VARIABLE Data_Schd : SchedArray(Data'RANGE);
        VARIABLE NewValue     : UX01;
        VARIABLE Glitch_Data : GlitchDataType;
        VARIABLE new_schd    : SchedType;
        VARIABLE Dly, Glch   : TIME;
        ALIAS Atpd_data_q : VitalDelayArrayType01(Data'RANGE) IS tpd_data_q;
        VARIABLE AllZeroDelay  : BOOLEAN := TRUE; --SN
    BEGIN
      -- ------------------------------------------------------------------------
      --  Check if ALL zero delay paths, use simple model
      --   ( No delay selection, glitch detection required )
      -- ------------------------------------------------------------------------
      FOR i IN Data'RANGE LOOP
          IF (Atpd_data_q(i) /= VitalZeroDelay01) THEN
              AllZeroDelay := FALSE;
              EXIT;
          END IF;
      END LOOP;
      IF (AllZeroDelay) THEN LOOP
          q <= VitalNOR(Data, ResultMap);
          WAIT ON Data;
      END LOOP;
      ELSE

        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        FOR n IN Data'RANGE LOOP
            InvPath ( Data_Schd(n), InitialEdge(Data(n)), Atpd_data_q(n) );
        END LOOP;

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        GetEdge ( Data, LastData, Data_Edge );
        InvPath ( Data_Schd, Data_Edge, Atpd_data_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue := '0';
        new_schd := Data_Schd(Data_Schd'LEFT);
        FOR i IN Data'RANGE LOOP
            NewValue  := NewValue  OR Data(i);
            new_schd := new_schd OR Data_Schd(i);
        END LOOP;
        NewValue  := NOT NewValue;
        new_schd := NOT new_schd;

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON Data;
      END LOOP;
      END IF; --SN
    END;
--
    PROCEDURE VitalXNOR  (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL         Data :  IN std_logic_vector;
            CONSTANT tpd_data_q :  IN VitalDelayArrayType01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE LastData    : std_logic_vector(Data'RANGE) := (OTHERS=>'U');
        VARIABLE Data_Edge   :  EdgeArray(Data'RANGE);
        VARIABLE DataB_Schd  : SchedArray(1 TO Data'LENGTH);
        VARIABLE DataI_Schd  : SchedArray(1 TO Data'LENGTH);
        VARIABLE NewValue     : UX01;
        VARIABLE Glitch_Data : GlitchDataType;
        VARIABLE new_schd    : SchedType;
        VARIABLE Dly, Glch   : TIME;
        ALIAS Atpd_data_q : VitalDelayArrayType01(Data'RANGE) IS tpd_data_q;
        ALIAS ADataB_Schd : SchedArray(Data'RANGE) IS DataB_Schd;
        ALIAS ADataI_Schd : SchedArray(Data'RANGE) IS DataI_Schd;
        VARIABLE AllZeroDelay  : BOOLEAN := TRUE; --SN
    BEGIN
      -- ------------------------------------------------------------------------
      --  Check if ALL zero delay paths, use simple model
      --   ( No delay selection, glitch detection required )
      -- ------------------------------------------------------------------------
      FOR i IN Data'RANGE LOOP
          IF (Atpd_data_q(i) /= VitalZeroDelay01) THEN
              AllZeroDelay := FALSE;
              EXIT;
          END IF;
      END LOOP;
      IF (AllZeroDelay) THEN LOOP
          q <= VitalXNOR(Data, ResultMap);
          WAIT ON Data;
      END LOOP;
      ELSE

        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        FOR n IN Data'RANGE LOOP
            BufPath ( ADataB_Schd(n), InitialEdge(Data(n)), Atpd_data_q(n) );
            InvPath ( ADataI_Schd(n), InitialEdge(Data(n)), Atpd_data_q(n) );
        END LOOP;

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        GetEdge ( Data, LastData, Data_Edge );
        BufPath ( ADataB_Schd, Data_Edge, Atpd_data_q );
        InvPath ( ADataI_Schd, Data_Edge, Atpd_data_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue  := VitalXNOR ( Data );
        new_schd := VitalXNOR ( DataB_Schd, DataI_Schd );

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON Data;
      END LOOP;
      END IF; --SN
    END;
--

    -- ------------------------------------------------------------------------
    -- Commonly used 2-bit Logical gates.
    -- ------------------------------------------------------------------------
    PROCEDURE VitalAND2  (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL         a, b :  IN std_ulogic   ;
            CONSTANT    tpd_a_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_b_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE a_schd, b_schd : SchedType;
        VARIABLE NewValue        : UX01;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE new_schd       : SchedType;
        VARIABLE Dly, Glch      : TIME;
    BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF ((tpd_a_q = VitalZeroDelay01) AND (tpd_b_q = VitalZeroDelay01)) THEN
      LOOP
        q <= VitalAND2 ( a, b, ResultMap );
        WAIT ON a, b;
      END LOOP;

    ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        BufPath ( a_schd, InitialEdge(a), tpd_a_q );
        BufPath ( b_schd, InitialEdge(b), tpd_b_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        BufPath ( a_schd, GetEdge(a), tpd_a_q );
        BufPath ( b_schd, GetEdge(b), tpd_b_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue := a AND b;
        new_schd := a_schd AND b_schd;

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON a, b;
      END LOOP;
    END IF;
    END;
--
    PROCEDURE VitalOR2   (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL         a, b :  IN std_ulogic   ;
            CONSTANT    tpd_a_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_b_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE a_schd, b_schd : SchedType;
        VARIABLE NewValue        : UX01;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE new_schd       : SchedType;
        VARIABLE Dly, Glch      : TIME;
    BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF ((tpd_a_q = VitalZeroDelay01) AND (tpd_b_q = VitalZeroDelay01)) THEN
      LOOP
        q <= VitalOR2 ( a, b, ResultMap );
        WAIT ON a, b;
      END LOOP;

    ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        BufPath ( a_schd, InitialEdge(a), tpd_a_q );
        BufPath ( b_schd, InitialEdge(b), tpd_b_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        BufPath ( a_schd, GetEdge(a), tpd_a_q );
        BufPath ( b_schd, GetEdge(b), tpd_b_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue := a OR b;
        new_schd := a_schd OR b_schd;

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON a, b;
      END LOOP;
    END IF;
    END;
--
    PROCEDURE VitalNAND2 (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL         a, b :  IN std_ulogic   ;
            CONSTANT    tpd_a_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_b_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE a_schd, b_schd : SchedType;
        VARIABLE NewValue        : UX01;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE new_schd       : SchedType;
        VARIABLE Dly, Glch      : TIME;
    BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF ((tpd_a_q = VitalZeroDelay01) AND (tpd_b_q = VitalZeroDelay01)) THEN
      LOOP
        q <= VitalNAND2 ( a, b, ResultMap );
        WAIT ON a, b;
      END LOOP;

    ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        InvPath ( a_schd, InitialEdge(a), tpd_a_q );
        InvPath ( b_schd, InitialEdge(b), tpd_b_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        InvPath ( a_schd, GetEdge(a), tpd_a_q );
        InvPath ( b_schd, GetEdge(b), tpd_b_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue := a NAND b;
        new_schd := a_schd NAND b_schd;

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON a, b;
      END LOOP;
    END IF;
    END;
--
    PROCEDURE VitalNOR2  (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL         a, b :  IN std_ulogic   ;
            CONSTANT    tpd_a_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_b_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE a_schd, b_schd : SchedType;
        VARIABLE NewValue        : UX01;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE new_schd       : SchedType;
        VARIABLE Dly, Glch      : TIME;
    BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF ((tpd_a_q = VitalZeroDelay01) AND (tpd_b_q = VitalZeroDelay01)) THEN
      LOOP
        q <= VitalNOR2 ( a, b, ResultMap );
        WAIT ON a, b;
      END LOOP;

    ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        InvPath ( a_schd, InitialEdge(a), tpd_a_q );
        InvPath ( b_schd, InitialEdge(b), tpd_b_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        InvPath ( a_schd, GetEdge(a), tpd_a_q );
        InvPath ( b_schd, GetEdge(b), tpd_b_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue := a NOR b;
        new_schd := a_schd NOR b_schd;

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON a, b;
      END LOOP;
    END IF;
    END;
--
    PROCEDURE VitalXOR2  (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL         a, b :  IN std_ulogic   ;
            CONSTANT    tpd_a_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_b_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE ab_schd, bb_schd : SchedType;
        VARIABLE ai_schd, bi_schd : SchedType;
        VARIABLE NewValue        : UX01;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE new_schd       : SchedType;
        VARIABLE Dly, Glch      : TIME;
    BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF ((tpd_a_q = VitalZeroDelay01) AND (tpd_b_q = VitalZeroDelay01)) THEN
      LOOP
        q <= VitalXOR2 ( a, b, ResultMap );
        WAIT ON a, b;
      END LOOP;

    ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        BufPath ( ab_schd, InitialEdge(a), tpd_a_q );
        InvPath ( ai_schd, InitialEdge(a), tpd_a_q );
        BufPath ( bb_schd, InitialEdge(b), tpd_b_q );
        InvPath ( bi_schd, InitialEdge(b), tpd_b_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        BufPath ( ab_schd, GetEdge(a), tpd_a_q );
        InvPath ( ai_schd, GetEdge(a), tpd_a_q );

        BufPath ( bb_schd, GetEdge(b), tpd_b_q );
        InvPath ( bi_schd, GetEdge(b), tpd_b_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue  := a XOR b;
        new_schd := VitalXOR2 ( ab_schd,ai_schd, bb_schd,bi_schd );

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON a, b;
      END LOOP;
    END IF;
    END;
--
    PROCEDURE VitalXNOR2 (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL         a, b :  IN std_ulogic   ;
            CONSTANT    tpd_a_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_b_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE ab_schd, bb_schd : SchedType;
        VARIABLE ai_schd, bi_schd : SchedType;
        VARIABLE NewValue        : UX01;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE new_schd       : SchedType;
        VARIABLE Dly, Glch      : TIME;
    BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF ((tpd_a_q = VitalZeroDelay01) AND (tpd_b_q = VitalZeroDelay01)) THEN
      LOOP
        q <= VitalXNOR2 ( a, b, ResultMap );
        WAIT ON a, b;
      END LOOP;

    ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        BufPath ( ab_schd, InitialEdge(a), tpd_a_q );
        InvPath ( ai_schd, InitialEdge(a), tpd_a_q );
        BufPath ( bb_schd, InitialEdge(b), tpd_b_q );
        InvPath ( bi_schd, InitialEdge(b), tpd_b_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        BufPath ( ab_schd, GetEdge(a), tpd_a_q );
        InvPath ( ai_schd, GetEdge(a), tpd_a_q );

        BufPath ( bb_schd, GetEdge(b), tpd_b_q );
        InvPath ( bi_schd, GetEdge(b), tpd_b_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue  := NOT (a XOR b);
        new_schd := VitalXNOR2 ( ab_schd,ai_schd, bb_schd,bi_schd );

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON a, b;
      END LOOP;
    END IF;
    END;

    -- ------------------------------------------------------------------------
    -- Commonly used 3-bit Logical gates.
    -- ------------------------------------------------------------------------
    PROCEDURE VitalAND3  (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL      a, b, c :  IN std_ulogic   ;
            CONSTANT    tpd_a_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_b_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_c_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE a_schd, b_schd, c_schd : SchedType;
        VARIABLE NewValue        : UX01;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE new_schd       : SchedType;
        VARIABLE Dly, Glch      : TIME;
    BEGIN
--
    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF (     (tpd_a_q = VitalZeroDelay01)
         AND (tpd_b_q = VitalZeroDelay01)
         AND (tpd_c_q = VitalZeroDelay01)) THEN
      LOOP
        q <= VitalAND3 ( a, b, c, ResultMap );
        WAIT ON a, b, c;
      END LOOP;

    ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        BufPath ( a_schd, InitialEdge(a), tpd_a_q );
        BufPath ( b_schd, InitialEdge(b), tpd_b_q );
        BufPath ( c_schd, InitialEdge(c), tpd_c_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        BufPath ( a_schd, GetEdge(a), tpd_a_q );
        BufPath ( b_schd, GetEdge(b), tpd_b_q );
        BufPath ( c_schd, GetEdge(c), tpd_c_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue  := a AND b AND c;
        new_schd := a_schd AND b_schd AND c_schd;

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON a, b, c;
      END LOOP;
    END IF;
    END;
--
    PROCEDURE VitalOR3   (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL      a, b, c :  IN std_ulogic   ;
            CONSTANT    tpd_a_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_b_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_c_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE a_schd, b_schd, c_schd : SchedType;
        VARIABLE NewValue        : UX01;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE new_schd       : SchedType;
        VARIABLE Dly, Glch      : TIME;
    BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF (     (tpd_a_q = VitalZeroDelay01)
         AND (tpd_b_q = VitalZeroDelay01)
         AND (tpd_c_q = VitalZeroDelay01)) THEN
      LOOP
        q <= VitalOR3 ( a, b, c, ResultMap );
        WAIT ON a, b, c;
      END LOOP;

    ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        BufPath ( a_schd, InitialEdge(a), tpd_a_q );
        BufPath ( b_schd, InitialEdge(b), tpd_b_q );
        BufPath ( c_schd, InitialEdge(c), tpd_c_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        BufPath ( a_schd, GetEdge(a), tpd_a_q );
        BufPath ( b_schd, GetEdge(b), tpd_b_q );
        BufPath ( c_schd, GetEdge(c), tpd_c_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue  := a OR b OR c;
        new_schd := a_schd OR b_schd OR c_schd;

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON a, b, c;
      END LOOP;
    END IF;
    END;
--
    PROCEDURE VitalNAND3 (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL      a, b, c :  IN std_ulogic   ;
            CONSTANT    tpd_a_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_b_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_c_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE a_schd, b_schd, c_schd : SchedType;
        VARIABLE NewValue        : UX01;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE new_schd       : SchedType;
        VARIABLE Dly, Glch      : TIME;
    BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF (     (tpd_a_q = VitalZeroDelay01)
         AND (tpd_b_q = VitalZeroDelay01)
         AND (tpd_c_q = VitalZeroDelay01)) THEN
      LOOP
        q <= VitalNAND3 ( a, b, c, ResultMap );
        WAIT ON a, b, c;
      END LOOP;

    ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        InvPath ( a_schd, InitialEdge(a), tpd_a_q );
        InvPath ( b_schd, InitialEdge(b), tpd_b_q );
        InvPath ( c_schd, InitialEdge(c), tpd_c_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        InvPath ( a_schd, GetEdge(a), tpd_a_q );
        InvPath ( b_schd, GetEdge(b), tpd_b_q );
        InvPath ( c_schd, GetEdge(c), tpd_c_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue  := (a AND b) NAND c;
        new_schd := (a_schd AND b_schd) NAND c_schd;

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON a, b, c;
      END LOOP;
    END IF;
    END;
--
    PROCEDURE VitalNOR3  (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL      a, b, c :  IN std_ulogic   ;
            CONSTANT    tpd_a_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_b_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_c_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE a_schd, b_schd, c_schd : SchedType;
        VARIABLE NewValue        : UX01;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE new_schd       : SchedType;
        VARIABLE Dly, Glch      : TIME;
    BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF (     (tpd_a_q = VitalZeroDelay01)
         AND (tpd_b_q = VitalZeroDelay01)
         AND (tpd_c_q = VitalZeroDelay01)) THEN
      LOOP
        q <= VitalNOR3 ( a, b, c, ResultMap );
        WAIT ON a, b, c;
      END LOOP;

    ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        InvPath ( a_schd, InitialEdge(a), tpd_a_q );
        InvPath ( b_schd, InitialEdge(b), tpd_b_q );
        InvPath ( c_schd, InitialEdge(c), tpd_c_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        InvPath ( a_schd, GetEdge(a), tpd_a_q );
        InvPath ( b_schd, GetEdge(b), tpd_b_q );
        InvPath ( c_schd, GetEdge(c), tpd_c_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue  := (a OR b) NOR c;
        new_schd := (a_schd OR b_schd) NOR c_schd;

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON a, b, c;
      END LOOP;
    END IF;
    END;
--
    PROCEDURE VitalXOR3  (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL      a, b, c :  IN std_ulogic   ;
            CONSTANT    tpd_a_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_b_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_c_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE ab_schd, bb_schd, cb_schd : SchedType;
        VARIABLE ai_schd, bi_schd, ci_schd : SchedType;
        VARIABLE NewValue        : UX01;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE new_schd       : SchedType;
        VARIABLE Dly, Glch      : TIME;
    BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF (     (tpd_a_q = VitalZeroDelay01)
         AND (tpd_b_q = VitalZeroDelay01)
         AND (tpd_c_q = VitalZeroDelay01)) THEN
      LOOP
        q <= VitalXOR3 ( a, b, c, ResultMap );
        WAIT ON a, b, c;
      END LOOP;

    ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        BufPath ( ab_schd, InitialEdge(a), tpd_a_q );
        InvPath ( ai_schd, InitialEdge(a), tpd_a_q );
        BufPath ( bb_schd, InitialEdge(b), tpd_b_q );
        InvPath ( bi_schd, InitialEdge(b), tpd_b_q );
        BufPath ( cb_schd, InitialEdge(c), tpd_c_q );
        InvPath ( ci_schd, InitialEdge(c), tpd_c_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        BufPath ( ab_schd, GetEdge(a), tpd_a_q );
        InvPath ( ai_schd, GetEdge(a), tpd_a_q );

        BufPath ( bb_schd, GetEdge(b), tpd_b_q );
        InvPath ( bi_schd, GetEdge(b), tpd_b_q );

        BufPath ( cb_schd, GetEdge(c), tpd_c_q );
        InvPath ( ci_schd, GetEdge(c), tpd_c_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue  := a XOR b XOR c;
        new_schd := VitalXOR3 ( ab_schd,ai_schd,
                                bb_schd,bi_schd,
                                cb_schd,ci_schd );

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON a, b, c;
      END LOOP;
    END IF;
    END;
--
    PROCEDURE VitalXNOR3 (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL      a, b, c :  IN std_ulogic   ;
            CONSTANT    tpd_a_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_b_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_c_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE ab_schd, bb_schd, cb_schd : SchedType;
        VARIABLE ai_schd, bi_schd, ci_schd : SchedType;
        VARIABLE NewValue        : UX01;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE new_schd       : SchedType;
        VARIABLE Dly, Glch      : TIME;
    BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF (     (tpd_a_q = VitalZeroDelay01)
         AND (tpd_b_q = VitalZeroDelay01)
         AND (tpd_c_q = VitalZeroDelay01)) THEN
      LOOP
        q <= VitalXNOR3 ( a, b, c, ResultMap );
        WAIT ON a, b, c;
      END LOOP;

    ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        BufPath ( ab_schd, InitialEdge(a), tpd_a_q );
        InvPath ( ai_schd, InitialEdge(a), tpd_a_q );
        BufPath ( bb_schd, InitialEdge(b), tpd_b_q );
        InvPath ( bi_schd, InitialEdge(b), tpd_b_q );
        BufPath ( cb_schd, InitialEdge(c), tpd_c_q );
        InvPath ( ci_schd, InitialEdge(c), tpd_c_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        BufPath ( ab_schd, GetEdge(a), tpd_a_q );
        InvPath ( ai_schd, GetEdge(a), tpd_a_q );

        BufPath ( bb_schd, GetEdge(b), tpd_b_q );
        InvPath ( bi_schd, GetEdge(b), tpd_b_q );

        BufPath ( cb_schd, GetEdge(c), tpd_c_q );
        InvPath ( ci_schd, GetEdge(c), tpd_c_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue  := NOT (a XOR b XOR c);
        new_schd := VitalXNOR3 ( ab_schd, ai_schd,
                                 bb_schd, bi_schd,
                                 cb_schd, ci_schd );

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON a, b, c;
      END LOOP;
    END IF;
    END;

    -- ------------------------------------------------------------------------
    -- Commonly used 4-bit Logical gates.
    -- ------------------------------------------------------------------------
    PROCEDURE VitalAND4  (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL   a, b, c, d :  IN std_ulogic   ;
            CONSTANT    tpd_a_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_b_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_c_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_d_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE a_schd, b_schd, c_schd, d_Schd : SchedType;
        VARIABLE NewValue        : UX01;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE new_schd       : SchedType;
        VARIABLE Dly, Glch      : TIME;
    BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF (     (tpd_a_q = VitalZeroDelay01)
         AND (tpd_b_q = VitalZeroDelay01)
         AND (tpd_c_q = VitalZeroDelay01)
         AND (tpd_d_q = VitalZeroDelay01)) THEN
      LOOP
        q <= VitalAND4 ( a, b, c, d, ResultMap );
        WAIT ON a, b, c, d;
      END LOOP;

    ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        BufPath ( a_schd, InitialEdge(a), tpd_a_q );
        BufPath ( b_schd, InitialEdge(b), tpd_b_q );
        BufPath ( c_schd, InitialEdge(c), tpd_c_q );
        BufPath ( d_Schd, InitialEdge(d), tpd_d_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        BufPath ( a_schd, GetEdge(a), tpd_a_q );
        BufPath ( b_schd, GetEdge(b), tpd_b_q );
        BufPath ( c_schd, GetEdge(c), tpd_c_q );
        BufPath ( d_Schd, GetEdge(d), tpd_d_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue  := a AND b AND c AND d;
        new_schd := a_schd AND b_schd AND c_schd AND d_Schd;

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON a, b, c, d;
      END LOOP;
    END IF;
    END;
--
    PROCEDURE VitalOR4   (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL   a, b, c, d :  IN std_ulogic   ;
            CONSTANT    tpd_a_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_b_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_c_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_d_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE a_schd, b_schd, c_schd, d_Schd : SchedType;
        VARIABLE NewValue        : UX01;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE new_schd       : SchedType;
        VARIABLE Dly, Glch      : TIME;
    BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF (     (tpd_a_q = VitalZeroDelay01)
         AND (tpd_b_q = VitalZeroDelay01)
         AND (tpd_c_q = VitalZeroDelay01)
         AND (tpd_d_q = VitalZeroDelay01)) THEN
      LOOP
        q <= VitalOR4 ( a, b, c, d, ResultMap );
        WAIT ON a, b, c, d;
      END LOOP;

    ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        BufPath ( a_schd, InitialEdge(a), tpd_a_q );
        BufPath ( b_schd, InitialEdge(b), tpd_b_q );
        BufPath ( c_schd, InitialEdge(c), tpd_c_q );
        BufPath ( d_Schd, InitialEdge(d), tpd_d_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        BufPath ( a_schd, GetEdge(a), tpd_a_q );
        BufPath ( b_schd, GetEdge(b), tpd_b_q );
        BufPath ( c_schd, GetEdge(c), tpd_c_q );
        BufPath ( d_Schd, GetEdge(d), tpd_d_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue  := a OR b OR c OR d;
        new_schd := a_schd OR b_schd OR c_schd OR d_Schd;

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON a, b, c, d;
      END LOOP;
    END IF;
    END;
--
    PROCEDURE VitalNAND4 (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL   a, b, c, d :  IN std_ulogic   ;
            CONSTANT    tpd_a_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_b_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_c_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_d_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE a_schd, b_schd, c_schd, d_Schd : SchedType;
        VARIABLE NewValue        : UX01;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE new_schd       : SchedType;
        VARIABLE Dly, Glch      : TIME;
    BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF (     (tpd_a_q = VitalZeroDelay01)
         AND (tpd_b_q = VitalZeroDelay01)
         AND (tpd_c_q = VitalZeroDelay01)
         AND (tpd_d_q = VitalZeroDelay01)) THEN
      LOOP
        q <= VitalNAND4 ( a, b, c, d, ResultMap );
        WAIT ON a, b, c, d;
      END LOOP;

    ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        InvPath ( a_schd, InitialEdge(a), tpd_a_q );
        InvPath ( b_schd, InitialEdge(b), tpd_b_q );
        InvPath ( c_schd, InitialEdge(c), tpd_c_q );
        InvPath ( d_Schd, InitialEdge(d), tpd_d_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        InvPath ( a_schd, GetEdge(a), tpd_a_q );
        InvPath ( b_schd, GetEdge(b), tpd_b_q );
        InvPath ( c_schd, GetEdge(c), tpd_c_q );
        InvPath ( d_Schd, GetEdge(d), tpd_d_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue  := (a AND b) NAND (c AND d);
        new_schd := (a_schd AND b_schd) NAND (c_schd AND d_Schd);

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON a, b, c, d;
      END LOOP;
    END IF;
    END;
--
    PROCEDURE VitalNOR4  (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL   a, b, c, d :  IN std_ulogic   ;
            CONSTANT    tpd_a_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_b_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_c_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_d_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE a_schd, b_schd, c_schd, d_Schd : SchedType;
        VARIABLE NewValue        : UX01;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE new_schd       : SchedType;
        VARIABLE Dly, Glch      : TIME;
    BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF (     (tpd_a_q = VitalZeroDelay01)
         AND (tpd_b_q = VitalZeroDelay01)
         AND (tpd_c_q = VitalZeroDelay01)
         AND (tpd_d_q = VitalZeroDelay01)) THEN
      LOOP
        q <= VitalNOR4 ( a, b, c, d, ResultMap );
        WAIT ON a, b, c, d;
      END LOOP;

    ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        InvPath ( a_schd, InitialEdge(a), tpd_a_q );
        InvPath ( b_schd, InitialEdge(b), tpd_b_q );
        InvPath ( c_schd, InitialEdge(c), tpd_c_q );
        InvPath ( d_Schd, InitialEdge(d), tpd_d_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        InvPath ( a_schd, GetEdge(a), tpd_a_q );
        InvPath ( b_schd, GetEdge(b), tpd_b_q );
        InvPath ( c_schd, GetEdge(c), tpd_c_q );
        InvPath ( d_Schd, GetEdge(d), tpd_d_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue  := (a OR b) NOR (c OR d);
        new_schd := (a_schd OR b_schd) NOR (c_schd OR d_Schd);

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON a, b, c, d;
      END LOOP;
    END IF;
    END;
--
    PROCEDURE VitalXOR4  (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL   a, b, c, d :  IN std_ulogic   ;
            CONSTANT    tpd_a_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_b_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_c_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_d_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE ab_schd, bb_schd, cb_schd, DB_Schd : SchedType;
        VARIABLE ai_schd, bi_schd, ci_schd, di_schd : SchedType;
        VARIABLE NewValue        : UX01;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE new_schd       : SchedType;
        VARIABLE Dly, Glch      : TIME;
    BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF (     (tpd_a_q = VitalZeroDelay01)
         AND (tpd_b_q = VitalZeroDelay01)
         AND (tpd_c_q = VitalZeroDelay01)
         AND (tpd_d_q = VitalZeroDelay01)) THEN
      LOOP
        q <= VitalXOR4 ( a, b, c, d, ResultMap );
        WAIT ON a, b, c, d;
      END LOOP;

    ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        BufPath ( ab_schd, InitialEdge(a), tpd_a_q );
        InvPath ( ai_schd, InitialEdge(a), tpd_a_q );

        BufPath ( bb_schd, InitialEdge(b), tpd_b_q );
        InvPath ( bi_schd, InitialEdge(b), tpd_b_q );

        BufPath ( cb_schd, InitialEdge(c), tpd_c_q );
        InvPath ( ci_schd, InitialEdge(c), tpd_c_q );

        BufPath ( DB_Schd, InitialEdge(d), tpd_d_q );
        InvPath ( di_schd, InitialEdge(d), tpd_d_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        BufPath ( ab_schd, GetEdge(a), tpd_a_q );
        InvPath ( ai_schd, GetEdge(a), tpd_a_q );

        BufPath ( bb_schd, GetEdge(b), tpd_b_q );
        InvPath ( bi_schd, GetEdge(b), tpd_b_q );

        BufPath ( cb_schd, GetEdge(c), tpd_c_q );
        InvPath ( ci_schd, GetEdge(c), tpd_c_q );

        BufPath ( DB_Schd, GetEdge(d), tpd_d_q );
        InvPath ( di_schd, GetEdge(d), tpd_d_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue  := a XOR b XOR c XOR d;
        new_schd := VitalXOR4 ( ab_schd,ai_schd, bb_schd,bi_schd,
                                cb_schd,ci_schd, DB_Schd,di_schd );

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON a, b, c, d;
      END LOOP;
    END IF;
    END;
--
    PROCEDURE VitalXNOR4 (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL   a, b, c, d :  IN std_ulogic   ;
            CONSTANT    tpd_a_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_b_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_c_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    tpd_d_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE ab_schd, bb_schd, cb_schd, DB_Schd : SchedType;
        VARIABLE ai_schd, bi_schd, ci_schd, di_schd : SchedType;
        VARIABLE NewValue        : UX01;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE new_schd       : SchedType;
        VARIABLE Dly, Glch      : TIME;
    BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF (     (tpd_a_q = VitalZeroDelay01)
         AND (tpd_b_q = VitalZeroDelay01)
         AND (tpd_c_q = VitalZeroDelay01)
         AND (tpd_d_q = VitalZeroDelay01)) THEN
      LOOP
        q <= VitalXNOR4 ( a, b, c, d, ResultMap );
        WAIT ON a, b, c, d;
      END LOOP;

    ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        BufPath ( ab_schd, InitialEdge(a), tpd_a_q );
        InvPath ( ai_schd, InitialEdge(a), tpd_a_q );

        BufPath ( bb_schd, InitialEdge(b), tpd_b_q );
        InvPath ( bi_schd, InitialEdge(b), tpd_b_q );

        BufPath ( cb_schd, InitialEdge(c), tpd_c_q );
        InvPath ( ci_schd, InitialEdge(c), tpd_c_q );

        BufPath ( DB_Schd, InitialEdge(d), tpd_d_q );
        InvPath ( di_schd, InitialEdge(d), tpd_d_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        BufPath ( ab_schd, GetEdge(a), tpd_a_q );
        InvPath ( ai_schd, GetEdge(a), tpd_a_q );

        BufPath ( bb_schd, GetEdge(b), tpd_b_q );
        InvPath ( bi_schd, GetEdge(b), tpd_b_q );

        BufPath ( cb_schd, GetEdge(c), tpd_c_q );
        InvPath ( ci_schd, GetEdge(c), tpd_c_q );

        BufPath ( DB_Schd, GetEdge(d), tpd_d_q );
        InvPath ( di_schd, GetEdge(d), tpd_d_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue  := NOT (a XOR b XOR c XOR d);
        new_schd := VitalXNOR4 ( ab_schd,ai_schd, bb_schd,bi_schd,
                                 cb_schd,ci_schd, DB_Schd,di_schd );

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON a, b, c, d;
      END LOOP;
    END IF;
    END;

    -- ------------------------------------------------------------------------
    -- Buffers
    --   BUF    ....... standard non-inverting buffer
    --   BUFIF0 ....... non-inverting buffer Data passes thru if (Enable = '0')
    --   BUFIF1 ....... non-inverting buffer Data passes thru if (Enable = '1')
    -- ------------------------------------------------------------------------
    PROCEDURE VitalBUF   (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL            a :  IN std_ulogic   ;
            CONSTANT    tpd_a_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE NewValue      : UX01;
        VARIABLE Glitch_Data  : GlitchDataType;
        VARIABLE Dly, Glch    : TIME;
    BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF (tpd_a_q = VitalZeroDelay01) THEN
      LOOP
        q <= ResultMap(To_UX01(a));
        WAIT ON a;
      END LOOP;

    ELSE
      LOOP
        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue  := To_UX01(a);     -- convert to forcing strengths
        CASE EdgeType'(GetEdge(a)) IS
          WHEN '1'|'/'|'R'|'r' => Dly := tpd_a_q(tr01);
          WHEN '0'|'\'|'F'|'f' => Dly := tpd_a_q(tr10);
          WHEN OTHERS          => Dly := Minimum (tpd_a_q(tr01), tpd_a_q(tr10));
        END CASE;

        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode );

        WAIT ON a;
      END LOOP;
    END IF;
    END;
--
    PROCEDURE VitalBUFIF1 (
            SIGNAL              q : OUT std_ulogic;
            SIGNAL           Data :  IN std_ulogic;
            SIGNAL         Enable :  IN std_ulogic;
            CONSTANT   tpd_data_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT tpd_enable_q :  IN VitalDelayType01Z   := VitalDefDelay01Z;
            CONSTANT    ResultMap :  IN VitalResultZMapType
                                        := VitalDefaultResultZMap
    ) IS
        VARIABLE NewValue        : UX01Z;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE d_Schd, e1_Schd, e0_Schd : SchedType;
        VARIABLE Dly, Glch      : TIME;
    BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF (     (tpd_data_q   = VitalZeroDelay01 )
         AND (tpd_enable_q = VitalZeroDelay01Z)) THEN
      LOOP
        q <= VitalBUFIF1( Data, Enable, ResultMap );
        WAIT ON Data, Enable;
      END LOOP;

    ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        BufPath ( d_Schd, InitialEdge(Data), tpd_data_q );
        BufEnab ( e1_Schd, e0_Schd, InitialEdge(Enable), tpd_enable_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        BufPath ( d_Schd, GetEdge(Data), tpd_data_q );
        BufEnab ( e1_Schd, e0_Schd, GetEdge(Enable), tpd_enable_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue := VitalBUFIF1( Data, Enable );

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data),
                        d_Schd, e1_Schd, e0_Schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON Data, Enable;
      END LOOP;
    END IF;
    END;
--
    PROCEDURE VitalBUFIF0 (
            SIGNAL              q : OUT std_ulogic;
            SIGNAL           Data :  IN std_ulogic;
            SIGNAL         Enable :  IN std_ulogic;
            CONSTANT   tpd_data_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT tpd_enable_q :  IN VitalDelayType01Z   := VitalDefDelay01Z;
            CONSTANT    ResultMap :  IN VitalResultZMapType
                                        := VitalDefaultResultZMap
    ) IS
        VARIABLE NewValue        : UX01Z;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE d_Schd, e1_Schd, e0_Schd : SchedType;
        VARIABLE ne1_schd, ne0_schd : SchedType;
        VARIABLE Dly, Glch      : TIME;
  BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF (     (tpd_data_q   = VitalZeroDelay01 )
         AND (tpd_enable_q = VitalZeroDelay01Z)) THEN
      LOOP
        q <= VitalBUFIF0( Data, Enable, ResultMap );
        WAIT ON Data, Enable;
      END LOOP;

    ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        BufPath ( d_Schd, InitialEdge(Data), tpd_data_q );
        InvEnab ( e1_Schd, e0_Schd, InitialEdge(Enable), tpd_enable_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        BufPath ( d_Schd, GetEdge(Data), tpd_data_q );
        InvEnab ( e1_Schd, e0_Schd, GetEdge(Enable), tpd_enable_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue := VitalBUFIF0( Data, Enable );
        ne1_schd := NOT e1_Schd;
        ne0_schd := NOT e0_Schd;

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data),
                        d_Schd, ne1_schd, ne0_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON Data, Enable;
      END LOOP;
    END IF;
    END;

    PROCEDURE VitalIDENT (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL            a :  IN std_ulogic   ;
            CONSTANT    tpd_a_q :  IN VitalDelayType01Z   := VitalDefDelay01Z;
            CONSTANT  ResultMap :  IN VitalResultZMapType
                                        := VitalDefaultResultZMap
    ) IS
        SUBTYPE v2 IS std_logic_vector(0 TO 1);
        VARIABLE NewValue      : UX01Z;
        VARIABLE Glitch_Data  : GlitchDataType;
        VARIABLE Dly, Glch    : TIME;
    BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF (tpd_a_q = VitalZeroDelay01Z) THEN
      LOOP
        q <= ResultMap(To_UX01Z(a));
        WAIT ON a;
      END LOOP;

    ELSE
      LOOP
        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        CASE v2'(To_X01Z(NewValue) & To_X01Z(a)) IS
          WHEN "00"   => Dly := tpd_a_q(tr10);
          WHEN "01"   => Dly := tpd_a_q(tr01);
          WHEN "0Z"   => Dly := tpd_a_q(tr0z);
          WHEN "0X"   => Dly := tpd_a_q(tr01);
          WHEN "10"   => Dly := tpd_a_q(tr10);
          WHEN "11"   => Dly := tpd_a_q(tr01);
          WHEN "1Z"   => Dly := tpd_a_q(tr1z);
          WHEN "1X"   => Dly := tpd_a_q(tr10);
          WHEN "Z0"   => Dly := tpd_a_q(trz0);
          WHEN "Z1"   => Dly := tpd_a_q(trz1);
          WHEN "ZZ"   => Dly := 0 ns;
          WHEN "ZX"   => Dly := Minimum (tpd_a_q(trz1), tpd_a_q(trz0));
          WHEN "X0"   => Dly := tpd_a_q(tr10);
          WHEN "X1"   => Dly := tpd_a_q(tr01);
          WHEN "XZ"   => Dly := Minimum (tpd_a_q(tr0z), tpd_a_q(tr1z));
          WHEN OTHERS => Dly := Minimum (tpd_a_q(tr01), tpd_a_q(tr10));
        END CASE;
        NewValue  := To_UX01Z(a);

        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode );

        WAIT ON a;
      END LOOP;
    END IF;
    END;

    -- ------------------------------------------------------------------------
    -- Invertors
    --   INV    ......... standard inverting buffer
    --   INVIF0 ......... inverting buffer Data passes thru if (Enable = '0')
    --   INVIF1 ......... inverting buffer Data passes thru if (Enable = '1')
    -- ------------------------------------------------------------------------
    PROCEDURE VitalINV   (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL            a :  IN std_ulogic   ;
            CONSTANT    tpd_a_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE NewValue      : UX01;
        VARIABLE Glitch_Data  : GlitchDataType;
        VARIABLE new_schd     : SchedType;
        VARIABLE Dly, Glch    : TIME;
    BEGIN
    IF (tpd_a_q = VitalZeroDelay01) THEN
      LOOP
        q <= ResultMap(NOT a);
        WAIT ON a;
      END LOOP;

    ELSE
      LOOP
        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue  := NOT a;
        CASE EdgeType'(GetEdge(a)) IS
          WHEN '1'|'/'|'R'|'r' => Dly := tpd_a_q(tr10);
          WHEN '0'|'\'|'F'|'f' => Dly := tpd_a_q(tr01);
          WHEN OTHERS          => Dly := Minimum (tpd_a_q(tr01), tpd_a_q(tr10));
        END CASE;

        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode );

        WAIT ON a;
      END LOOP;
    END IF;
    END;
--
    PROCEDURE VitalINVIF1 (
            SIGNAL              q : OUT std_ulogic;
            SIGNAL           Data :  IN std_ulogic;
            SIGNAL         Enable :  IN std_ulogic;
            CONSTANT   tpd_data_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT tpd_enable_q :  IN VitalDelayType01Z   := VitalDefDelay01Z;
            CONSTANT    ResultMap :  IN VitalResultZMapType
                                        := VitalDefaultResultZMap
    ) IS
        VARIABLE NewValue        : UX01Z;
        VARIABLE new_schd       : SchedType;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE d_Schd, e1_Schd, e0_Schd : SchedType;
        VARIABLE Dly, Glch      : TIME;
  BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF (     (tpd_data_q   = VitalZeroDelay01 )
         AND (tpd_enable_q = VitalZeroDelay01Z)) THEN
      LOOP
        q <= VitalINVIF1( Data, Enable, ResultMap );
        WAIT ON Data, Enable;
      END LOOP;

    ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        InvPath ( d_Schd, InitialEdge(Data), tpd_data_q );
        BufEnab ( e1_Schd, e0_Schd, InitialEdge(Enable), tpd_enable_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        InvPath ( d_Schd, GetEdge(Data), tpd_data_q );
        BufEnab ( e1_Schd, e0_Schd, GetEdge(Enable), tpd_enable_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue := VitalINVIF1( Data, Enable );
        new_schd := NOT d_Schd;

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data),
                        new_schd, e1_Schd, e0_Schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON Data, Enable;
      END LOOP;
    END IF;
    END;
--
    PROCEDURE VitalINVIF0 (
            SIGNAL              q : OUT std_ulogic;
            SIGNAL           Data :  IN std_ulogic;
            SIGNAL         Enable :  IN std_ulogic;
            CONSTANT   tpd_data_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT tpd_enable_q :  IN VitalDelayType01Z   := VitalDefDelay01Z;
            CONSTANT    ResultMap :  IN VitalResultZMapType
                                        := VitalDefaultResultZMap
    ) IS
        VARIABLE NewValue        : UX01Z;
        VARIABLE new_schd       : SchedType;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE d_Schd, e1_Schd, e0_Schd : SchedType;
        VARIABLE ne1_schd, ne0_schd : SchedType := DefSchedType;
        VARIABLE Dly, Glch      : TIME;
  BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF (     (tpd_data_q   = VitalZeroDelay01 )
         AND (tpd_enable_q = VitalZeroDelay01Z)) THEN
      LOOP
        q <= VitalINVIF0( Data, Enable, ResultMap );
        WAIT ON Data, Enable;
      END LOOP;

    ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        InvPath ( d_Schd, InitialEdge(Data), tpd_data_q );
        InvEnab ( e1_Schd, e0_Schd, InitialEdge(Enable), tpd_enable_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        InvPath ( d_Schd, GetEdge(Data), tpd_data_q );
        InvEnab ( e1_Schd, e0_Schd, GetEdge(Enable), tpd_enable_q );

        -- ------------------------------------
        -- Compute function and propation delay
        -- ------------------------------------
        NewValue := VitalINVIF0( Data, Enable );
        ne1_schd := NOT e1_Schd;
        ne0_schd := NOT e0_Schd;
        new_schd := NOT d_Schd;

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data),
                        new_schd, ne1_schd, ne0_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON Data, Enable;
      END LOOP;
    END IF;
    END;

    -- ------------------------------------------------------------------------
    -- Multiplexor
    --   MUX   .......... result := data(dselect)
    --   MUX2  .......... 2-input mux; result := data0 when (dselect = '0'),
    --                                           data1 when (dselect = '1'),
    --                        'X' when (dselect = 'X') and (data0 /= data1)
    --   MUX4  .......... 4-input mux; result := data(dselect)
    --   MUX8  .......... 8-input mux; result := data(dselect)
    -- ------------------------------------------------------------------------
    PROCEDURE VitalMUX2  (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL       d1, d0 :  IN std_ulogic;
            SIGNAL         dSel :  IN std_ulogic;
            CONSTANT   tpd_d1_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT   tpd_d0_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT tpd_dsel_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE NewValue        : UX01;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE new_schd       : SchedType;
        VARIABLE Dly, Glch      : TIME;
        VARIABLE d1_Schd,  d0_Schd  : SchedType;
        VARIABLE dSel_bSchd, dSel_iSchd : SchedType;
        VARIABLE d1_Edge, d0_Edge, dSel_Edge : EdgeType;
    BEGIN

    -- ------------------------------------------------------------------------
    --  For ALL zero delay paths, use simple model
    --   ( No delay selection, glitch detection required )
    -- ------------------------------------------------------------------------
    IF (     (tpd_d1_q   = VitalZeroDelay01)
         AND (tpd_d0_q   = VitalZeroDelay01)
         AND (tpd_dsel_q = VitalZeroDelay01) ) THEN
      LOOP
        q <= VitalMUX2 ( d1, d0, dSel, ResultMap );
        WAIT ON d1, d0, dSel;
      END LOOP;

    ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        BufPath ( d1_Schd, InitialEdge(d1), tpd_d1_q );
        BufPath ( d0_Schd, InitialEdge(d0), tpd_d0_q );
        BufPath ( dSel_bSchd, InitialEdge(dSel), tpd_dsel_q );
        InvPath ( dSel_iSchd, InitialEdge(dSel), tpd_dsel_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        BufPath ( d1_Schd, GetEdge(d1), tpd_d1_q );
        BufPath ( d0_Schd, GetEdge(d0), tpd_d0_q );
        BufPath ( dSel_bSchd, GetEdge(dSel), tpd_dsel_q );
        InvPath ( dSel_iSchd, GetEdge(dSel), tpd_dsel_q );

        -- ------------------------------------
        -- Compute function and propation delaq
        -- ------------------------------------
        NewValue  := VitalMUX2 ( d1, d0, dSel );
        new_schd := VitalMUX2 ( d1_Schd, d0_Schd, dSel_bSchd, dSel_iSchd );

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON d1, d0, dSel;
      END LOOP;
    END IF;
    END;
--
    PROCEDURE VitalMUX4  (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL         Data :  IN std_logic_vector4;
            SIGNAL         dSel :  IN std_logic_vector2;
            CONSTANT tpd_data_q :  IN VitalDelayArrayType01;
            CONSTANT tpd_dsel_q :  IN VitalDelayArrayType01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE LastData  : std_logic_vector(Data'RANGE) := (OTHERS=>'U');
        VARIABLE LastdSel  : std_logic_vector(dSel'RANGE) := (OTHERS=>'U');
        VARIABLE NewValue        : UX01;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE new_schd       : SchedType;
        VARIABLE Dly, Glch      : TIME;
        VARIABLE Data_Schd      : SchedArray4;
        VARIABLE Data_Edge      : EdgeArray4;
        VARIABLE dSel_Edge      : EdgeArray2;
        VARIABLE dSel_bSchd     : SchedArray2;
        VARIABLE dSel_iSchd     : SchedArray2;
        ALIAS Atpd_data_q : VitalDelayArrayType01(Data'RANGE) IS tpd_data_q;
        ALIAS Atpd_dsel_q : VitalDelayArrayType01(dSel'RANGE) IS tpd_dsel_q;
        VARIABLE AllZeroDelay  : BOOLEAN := TRUE; --SN
    BEGIN
      -- ------------------------------------------------------------------------
      --  Check if ALL zero delay paths, use simple model
      --   ( No delay selection, glitch detection required )
      -- ------------------------------------------------------------------------
      FOR i IN dSel'RANGE LOOP
          IF (Atpd_dsel_q(i) /= VitalZeroDelay01) THEN
              AllZeroDelay := FALSE;
              EXIT;
          END IF;
      END LOOP;
      IF (AllZeroDelay) THEN
          FOR i IN Data'RANGE LOOP
              IF (Atpd_data_q(i) /= VitalZeroDelay01) THEN
                  AllZeroDelay := FALSE;
                  EXIT;
              END IF;
          END LOOP;

          IF (AllZeroDelay) THEN LOOP
              q <= VitalMUX(Data, dSel, ResultMap);
              WAIT ON Data, dSel;
          END LOOP;
          END IF;
      ELSE

        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        FOR n IN Data'RANGE LOOP
            BufPath ( Data_Schd(n), InitialEdge(Data(n)), Atpd_data_q(n) );
        END LOOP;
        FOR n IN dSel'RANGE LOOP
            BufPath ( dSel_bSchd(n), InitialEdge(dSel(n)), Atpd_dsel_q(n) );
            InvPath ( dSel_iSchd(n), InitialEdge(dSel(n)), Atpd_dsel_q(n) );
        END LOOP;

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        GetEdge ( Data, LastData, Data_Edge );
        BufPath ( Data_Schd, Data_Edge, Atpd_data_q );

        GetEdge ( dSel, LastdSel, dSel_Edge );
        BufPath ( dSel_bSchd, dSel_Edge, Atpd_dsel_q );
        InvPath ( dSel_iSchd, dSel_Edge, Atpd_dsel_q );

        -- ------------------------------------
        -- Compute function and propation delaq
        -- ------------------------------------
        NewValue  := VitalMUX4 ( Data, dSel );
        new_schd := VitalMUX4 ( Data_Schd, dSel_bSchd, dSel_iSchd );

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON Data, dSel;
      END LOOP;
      END IF; --SN
    END;

    PROCEDURE VitalMUX8  (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL         Data :  IN std_logic_vector8;
            SIGNAL         dSel :  IN std_logic_vector3;
            CONSTANT tpd_data_q :  IN VitalDelayArrayType01;
            CONSTANT tpd_dsel_q :  IN VitalDelayArrayType01;
            CONSTANT  ResultMap :  IN VitalResultMapType  := VitalDefaultResultMap
    ) IS
        VARIABLE LastData  : std_logic_vector(Data'RANGE) := (OTHERS=>'U');
        VARIABLE LastdSel  : std_logic_vector(dSel'RANGE) := (OTHERS=>'U');
        VARIABLE NewValue        : UX01;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE new_schd       : SchedType;
        VARIABLE Dly, Glch      : TIME;
        VARIABLE Data_Schd      : SchedArray8;
        VARIABLE Data_Edge      : EdgeArray8;
        VARIABLE dSel_Edge      : EdgeArray3;
        VARIABLE dSel_bSchd     : SchedArray3;
        VARIABLE dSel_iSchd     : SchedArray3;
        ALIAS Atpd_data_q : VitalDelayArrayType01(Data'RANGE) IS tpd_data_q;
        ALIAS Atpd_dsel_q : VitalDelayArrayType01(dSel'RANGE) IS tpd_dsel_q;
        VARIABLE AllZeroDelay  : BOOLEAN := TRUE; --SN
    BEGIN
      -- ------------------------------------------------------------------------
      --  Check if ALL zero delay paths, use simple model
      --   ( No delay selection, glitch detection required )
      -- ------------------------------------------------------------------------
      FOR i IN dSel'RANGE LOOP
          IF (Atpd_dsel_q(i) /= VitalZeroDelay01) THEN
              AllZeroDelay := FALSE;
              EXIT;
          END IF;
      END LOOP;
      IF (AllZeroDelay) THEN
          FOR i IN Data'RANGE LOOP
              IF (Atpd_data_q(i) /= VitalZeroDelay01) THEN
                  AllZeroDelay := FALSE;
                  EXIT;
              END IF;
          END LOOP;

          IF (AllZeroDelay) THEN LOOP
              q <= VitalMUX(Data, dSel, ResultMap);
              WAIT ON Data, dSel;
          END LOOP;
          END IF;
       ELSE

        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        FOR n IN Data'RANGE LOOP
            BufPath ( Data_Schd(n), InitialEdge(Data(n)), Atpd_data_q(n) );
        END LOOP;
        FOR n IN dSel'RANGE LOOP
            BufPath ( dSel_bSchd(n), InitialEdge(dSel(n)), Atpd_dsel_q(n) );
            InvPath ( dSel_iSchd(n), InitialEdge(dSel(n)), Atpd_dsel_q(n) );
        END LOOP;

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        GetEdge ( Data, LastData, Data_Edge );
        BufPath ( Data_Schd, Data_Edge, Atpd_data_q );

        GetEdge ( dSel, LastdSel, dSel_Edge );
        BufPath ( dSel_bSchd, dSel_Edge, Atpd_dsel_q );
        InvPath ( dSel_iSchd, dSel_Edge, Atpd_dsel_q );

        -- ------------------------------------
        -- Compute function and propation delaq
        -- ------------------------------------
        NewValue  := VitalMUX8 ( Data, dSel );
        new_schd := VitalMUX8 ( Data_Schd, dSel_bSchd, dSel_iSchd );

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON Data, dSel;
      END LOOP;
      END IF;
    END;
--
    PROCEDURE VitalMUX   (
            SIGNAL            q : OUT std_ulogic;
            SIGNAL         Data :  IN std_logic_vector;
            SIGNAL         dSel :  IN std_logic_vector;
            CONSTANT tpd_data_q :  IN VitalDelayArrayType01;
            CONSTANT tpd_dsel_q :  IN VitalDelayArrayType01;
            CONSTANT  ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE LastData  : std_logic_vector(Data'RANGE) := (OTHERS=>'U');
        VARIABLE LastdSel  : std_logic_vector(dSel'RANGE) := (OTHERS=>'U');
        VARIABLE NewValue        : UX01;
        VARIABLE Glitch_Data    : GlitchDataType;
        VARIABLE new_schd       : SchedType;
        VARIABLE Dly, Glch      : TIME;
        VARIABLE Data_Schd      : SchedArray(Data'RANGE);
        VARIABLE Data_Edge      : EdgeArray(Data'RANGE);
        VARIABLE dSel_Edge      : EdgeArray(dSel'RANGE);
        VARIABLE dSel_bSchd     : SchedArray(dSel'RANGE);
        VARIABLE dSel_iSchd     : SchedArray(dSel'RANGE);
        ALIAS Atpd_data_q : VitalDelayArrayType01(Data'RANGE) IS tpd_data_q;
        ALIAS Atpd_dsel_q : VitalDelayArrayType01(dSel'RANGE) IS tpd_dsel_q;
        VARIABLE AllZeroDelay  : BOOLEAN := TRUE; --SN
    BEGIN
      -- ------------------------------------------------------------------------
      --  Check if ALL zero delay paths, use simple model
      --   ( No delay selection, glitch detection required )
      -- ------------------------------------------------------------------------
      FOR i IN dSel'RANGE LOOP
          IF (Atpd_dsel_q(i) /= VitalZeroDelay01) THEN
              AllZeroDelay := FALSE;
              EXIT;
          END IF;
      END LOOP;
      IF (AllZeroDelay) THEN
          FOR i IN Data'RANGE LOOP
              IF (Atpd_data_q(i) /= VitalZeroDelay01) THEN
                  AllZeroDelay := FALSE;
                  EXIT;
              END IF;
          END LOOP;

          IF (AllZeroDelay) THEN LOOP
              q <= VitalMUX(Data, dSel, ResultMap);
              WAIT ON Data, dSel;
          END LOOP;
          END IF;
      ELSE

        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        FOR n IN Data'RANGE LOOP
            BufPath ( Data_Schd(n), InitialEdge(Data(n)), Atpd_data_q(n) );
        END LOOP;
        FOR n IN dSel'RANGE LOOP
            BufPath ( dSel_bSchd(n), InitialEdge(dSel(n)), Atpd_dsel_q(n) );
            InvPath ( dSel_iSchd(n), InitialEdge(dSel(n)), Atpd_dsel_q(n) );
        END LOOP;

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        GetEdge ( Data, LastData, Data_Edge );
        BufPath ( Data_Schd, Data_Edge, Atpd_data_q );

        GetEdge ( dSel, LastdSel, dSel_Edge );
        BufPath ( dSel_bSchd, dSel_Edge, Atpd_dsel_q );
        InvPath ( dSel_iSchd, dSel_Edge, Atpd_dsel_q );

        -- ------------------------------------
        -- Compute function and propation delaq
        -- ------------------------------------
        NewValue  := VitalMUX ( Data, dSel );
        new_schd := VitalMUX ( Data_Schd, dSel_bSchd, dSel_iSchd );

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, ResultMap(NewValue), Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON Data, dSel;
      END LOOP;
     END IF; --SN
    END;

    -- ------------------------------------------------------------------------
    -- Decoder
    --          General Algorithm :
    --              (a) Result(...) := '0' when (enable = '0')
    --              (b) Result(data) := '1'; all other subelements = '0'
    --              ... Result array is decending (n-1 downto 0)
    --
    --          DECODERn  .......... n:2**n decoder
    -- Caution: If 'ResultMap' defines other than strength mapping, the
    --          delay selection is not defined.
    -- ------------------------------------------------------------------------
    PROCEDURE VitalDECODER2  (
            SIGNAL              q : OUT std_logic_vector2;
            SIGNAL           Data :  IN std_ulogic;
            SIGNAL         Enable :  IN std_ulogic;
            CONSTANT   tpd_data_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT tpd_enable_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE NewValue        : std_logic_vector2;
        VARIABLE Glitch_Data    : GlitchArray2;
        VARIABLE new_schd       : SchedArray2;
        VARIABLE Dly, Glch      : TimeArray2;
        VARIABLE Enable_Schd  : SchedType := DefSchedType;
        VARIABLE Data_BSchd, Data_ISchd : SchedType;
    BEGIN
      -- ------------------------------------------------------------------------
      --  Check if ALL zero delay paths, use simple model
      --   ( No delay selection, glitch detection required )
      -- ------------------------------------------------------------------------
      IF (tpd_enable_q = VitalZeroDelay01) AND (tpd_data_q = VitalZeroDelay01) THEN
      LOOP
          q <= VitalDECODER2(Data, Enable, ResultMap);
          WAIT ON Data, Enable;
      END LOOP;
      ELSE

        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        BufPath ( Data_BSchd, InitialEdge(Data), tpd_data_q );
        InvPath ( Data_ISchd, InitialEdge(Data), tpd_data_q );
        BufPath ( Enable_Schd, InitialEdge(Enable), tpd_enable_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        BufPath ( Data_BSchd, GetEdge(Data), tpd_data_q );
        InvPath ( Data_ISchd, GetEdge(Data), tpd_data_q );

        BufPath ( Enable_Schd, GetEdge(Enable), tpd_enable_q );

        -- ------------------------------------
        -- Compute function and propation delaq
        -- ------------------------------------
        NewValue  := VitalDECODER2 ( Data, Enable, ResultMap );
        new_schd := VitalDECODER2 ( Data_BSchd, Data_ISchd, Enable_Schd );

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, NewValue, Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON Data, Enable;
      END LOOP;
      END IF; -- SN
    END;
--
    PROCEDURE VitalDECODER4  (
            SIGNAL              q : OUT std_logic_vector4;
            SIGNAL           Data :  IN std_logic_vector2;
            SIGNAL         Enable :  IN std_ulogic;
            CONSTANT   tpd_data_q :  IN VitalDelayArrayType01;
            CONSTANT tpd_enable_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    ResultMap :  IN VitalResultMapType       := VitalDefaultResultMap
    ) IS
        VARIABLE LastData  : std_logic_vector(Data'RANGE) := (OTHERS=>'U');
        VARIABLE NewValue        : std_logic_vector4;
        VARIABLE Glitch_Data    : GlitchArray4;
        VARIABLE new_schd       : SchedArray4;
        VARIABLE Dly, Glch      : TimeArray4;
        VARIABLE Enable_Schd    : SchedType;
        VARIABLE Enable_Edge    : EdgeType;
        VARIABLE Data_Edge      : EdgeArray2;
        VARIABLE Data_BSchd, Data_ISchd : SchedArray2;
        ALIAS Atpd_data_q : VitalDelayArrayType01(Data'RANGE) IS tpd_data_q;
        VARIABLE AllZeroDelay  : BOOLEAN := TRUE; --SN
    BEGIN
      -- ------------------------------------------------------------------------
      --  Check if ALL zero delay paths, use simple model
      --   ( No delay selection, glitch detection required )
      -- ------------------------------------------------------------------------
      IF (tpd_enable_q /= VitalZeroDelay01) THEN
          AllZeroDelay := FALSE;
      ELSE
          FOR i IN Data'RANGE LOOP
          IF (Atpd_data_q(i) /= VitalZeroDelay01) THEN
              AllZeroDelay := FALSE;
              EXIT;
          END IF;
          END LOOP;
      END IF;
      IF (AllZeroDelay) THEN LOOP
          q <= VitalDECODER4(Data, Enable, ResultMap);
          WAIT ON Data, Enable;
      END LOOP;
      ELSE

        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        FOR n IN Data'RANGE LOOP
            BufPath ( Data_BSchd(n), InitialEdge(Data(n)), Atpd_data_q(n) );
            InvPath ( Data_ISchd(n), InitialEdge(Data(n)), Atpd_data_q(n) );
        END LOOP;
        BufPath ( Enable_Schd, InitialEdge(Enable), tpd_enable_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        GetEdge ( Data, LastData, Data_Edge );
        BufPath ( Data_BSchd, Data_Edge, Atpd_data_q );
        InvPath ( Data_ISchd, Data_Edge, Atpd_data_q );

        BufPath ( Enable_Schd, GetEdge(Enable), tpd_enable_q );

        -- ------------------------------------
        -- Compute function and propation delaq
        -- ------------------------------------
        NewValue  := VitalDECODER4 ( Data, Enable, ResultMap );
        new_schd := VitalDECODER4 ( Data_BSchd, Data_ISchd, Enable_Schd );

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, NewValue, Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON Data, Enable;
      END LOOP;
      END IF;
    END;
--
    PROCEDURE VitalDECODER8  (
            SIGNAL              q : OUT std_logic_vector8;
            SIGNAL           Data :  IN std_logic_vector3;
            SIGNAL         Enable :  IN std_ulogic;
            CONSTANT   tpd_data_q :  IN VitalDelayArrayType01;
            CONSTANT tpd_enable_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE LastData  : std_logic_vector(Data'RANGE) := (OTHERS=>'U');
        VARIABLE NewValue        : std_logic_vector8;
        VARIABLE Glitch_Data    : GlitchArray8;
        VARIABLE new_schd       : SchedArray8;
        VARIABLE Dly, Glch      : TimeArray8;
        VARIABLE Enable_Schd    : SchedType;
        VARIABLE Enable_Edge    : EdgeType;
        VARIABLE Data_Edge      : EdgeArray3;
        VARIABLE Data_BSchd, Data_ISchd : SchedArray3;
        ALIAS Atpd_data_q : VitalDelayArrayType01(Data'RANGE) IS tpd_data_q;
        VARIABLE AllZeroDelay  : BOOLEAN := TRUE; --SN
    BEGIN
      -- ------------------------------------------------------------------------
      --  Check if ALL zero delay paths, use simple model
      --   ( No delay selection, glitch detection required )
      -- ------------------------------------------------------------------------
      IF (tpd_enable_q /= VitalZeroDelay01) THEN
          AllZeroDelay := FALSE;
      ELSE
          FOR i IN Data'RANGE LOOP
          IF (Atpd_data_q(i) /= VitalZeroDelay01) THEN
              AllZeroDelay := FALSE;
              EXIT;
          END IF;
          END LOOP;
      END IF;
      IF (AllZeroDelay) THEN LOOP
          q <= VitalDECODER(Data, Enable, ResultMap);
          WAIT ON Data, Enable;
      END LOOP;
      ELSE

        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        FOR n IN Data'RANGE LOOP
            BufPath ( Data_BSchd(n), InitialEdge(Data(n)), Atpd_data_q(n) );
            InvPath ( Data_ISchd(n), InitialEdge(Data(n)), Atpd_data_q(n) );
        END LOOP;
        BufPath ( Enable_Schd, InitialEdge(Enable), tpd_enable_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        GetEdge ( Data, LastData, Data_Edge );
        BufPath ( Data_BSchd, Data_Edge, Atpd_data_q );
        InvPath ( Data_ISchd, Data_Edge, Atpd_data_q );

        BufPath ( Enable_Schd, GetEdge(Enable), tpd_enable_q );

        -- ------------------------------------
        -- Compute function and propation delaq
        -- ------------------------------------
        NewValue  := VitalDECODER8 ( Data, Enable, ResultMap );
        new_schd := VitalDECODER8 ( Data_BSchd, Data_ISchd, Enable_Schd );

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, NewValue, Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON Data, Enable;
      END LOOP;
      END IF; --SN
    END;
--
    PROCEDURE VitalDECODER   (
            SIGNAL              q : OUT std_logic_vector;
            SIGNAL           Data :  IN std_logic_vector;
            SIGNAL         Enable :  IN std_ulogic;
            CONSTANT   tpd_data_q :  IN VitalDelayArrayType01;
            CONSTANT tpd_enable_q :  IN VitalDelayType01    := VitalDefDelay01;
            CONSTANT    ResultMap :  IN VitalResultMapType
                                        := VitalDefaultResultMap
    ) IS
        VARIABLE LastData  : std_logic_vector(Data'RANGE) := (OTHERS=>'U');
        VARIABLE NewValue        : std_logic_vector(q'RANGE);
        VARIABLE Glitch_Data    : GlitchDataArrayType(q'RANGE);
        VARIABLE new_schd       : SchedArray(q'RANGE);
        VARIABLE Dly, Glch      : VitalTimeArray(q'RANGE);
        VARIABLE Enable_Schd    : SchedType;
        VARIABLE Enable_Edge    : EdgeType;
        VARIABLE Data_Edge      : EdgeArray(Data'RANGE);
        VARIABLE Data_BSchd, Data_ISchd : SchedArray(Data'RANGE);
        ALIAS Atpd_data_q : VitalDelayArrayType01(Data'RANGE) IS tpd_data_q;
        VARIABLE AllZeroDelay  : BOOLEAN := TRUE;
    BEGIN
      -- ------------------------------------------------------------------------
      --  Check if ALL zero delay paths, use simple model
      --   ( No delay selection, glitch detection required )
      -- ------------------------------------------------------------------------
      IF (tpd_enable_q /= VitalZeroDelay01) THEN
          AllZeroDelay := FALSE;
      ELSE
          FOR i IN Data'RANGE LOOP
          IF (Atpd_data_q(i) /= VitalZeroDelay01) THEN
              AllZeroDelay := FALSE;
              EXIT;
          END IF;
          END LOOP;
      END IF;
      IF (AllZeroDelay) THEN LOOP
          q <= VitalDECODER(Data, Enable, ResultMap);
          WAIT ON Data, Enable;
      END LOOP;
      ELSE
        -- --------------------------------------
        -- Initialize delay schedules
        -- --------------------------------------
        FOR n IN Data'RANGE LOOP
            BufPath ( Data_BSchd(n), InitialEdge(Data(n)), Atpd_data_q(n) );
            InvPath ( Data_ISchd(n), InitialEdge(Data(n)), Atpd_data_q(n) );
        END LOOP;
        BufPath ( Enable_Schd, InitialEdge(Enable), tpd_enable_q );

      LOOP
        -- --------------------------------------
        -- Process input signals
        --   get edge values
        --   re-evaluate output schedules
        -- --------------------------------------
        GetEdge ( Data, LastData, Data_Edge );
        BufPath ( Data_BSchd, Data_Edge, Atpd_data_q );
        InvPath ( Data_ISchd, Data_Edge, Atpd_data_q );

        BufPath ( Enable_Schd, GetEdge(Enable), tpd_enable_q );

        -- ------------------------------------
        -- Compute function and propation delaq
        -- ------------------------------------
        NewValue  := VitalDECODER ( Data, Enable, ResultMap );
        new_schd := VitalDECODER ( Data_BSchd, Data_ISchd, Enable_Schd );

        -- ------------------------------------------------------
        -- Assign Outputs
        --  get delays to new value and possable glitch
        --  schedule output change with On Event glitch detection
        -- ------------------------------------------------------
        GetSchedDelay ( Dly, Glch, NewValue, CurValue(Glitch_Data), new_schd );
        VitalGlitchOnEvent ( q, "q", Glitch_Data, NewValue, Dly,
                             PrimGlitchMode, GlitchDelay=>Glch );

        WAIT ON Data, Enable;
      END LOOP;
      END IF;
    END;

    -- ------------------------------------------------------------------------
    FUNCTION VitalTruthTable  (
            CONSTANT TruthTable   : IN VitalTruthTableType;
            CONSTANT DataIn       : IN std_logic_vector
          ) RETURN std_logic_vector IS

        CONSTANT InputSize   : INTEGER := DataIn'LENGTH;
        CONSTANT OutSize     : INTEGER := TruthTable'LENGTH(2) - InputSize;
        VARIABLE ReturnValue : std_logic_vector(OutSize - 1 DOWNTO 0)
                               := (OTHERS => 'X');
        VARIABLE DataInAlias : std_logic_vector(0 TO InputSize - 1)
                               :=  To_X01(DataIn);
        VARIABLE Index       : INTEGER;
        VARIABLE Err         : BOOLEAN := FALSE;

        -- This needs to be done since the TableLookup arrays must be
        -- ascending starting with 0
        VARIABLE TableAlias  : VitalTruthTableType(0 TO (TruthTable'LENGTH(1)-1),
                                                   0 TO (TruthTable'LENGTH(2)-1))
                                := TruthTable;

    BEGIN
        -- search through each row of the truth table
        IF OutSize > 0 THEN
          ColLoop:
            FOR i IN TableAlias'RANGE(1) LOOP

            RowLoop: -- Check each input element of the entry
              FOR j IN 0 TO InputSize LOOP

                IF (j = InputSize) THEN -- This entry matches
                    -- Return the Result
                    Index := 0;
                    FOR k IN TruthTable'LENGTH(2) - 1 DOWNTO InputSize LOOP
                        TruthOutputX01Z ( TableAlias(i,k),
                                          ReturnValue(Index), Err);
                        EXIT WHEN Err;
                        Index := Index + 1;
                    END LOOP;

                    IF Err THEN
                        ReturnValue := (OTHERS => 'X');
                    END IF;
                    RETURN ReturnValue;
                END IF;
                IF NOT ValidTruthTableInput(TableAlias(i,j)) THEN
                    VitalError ( "VitalTruthTable", ErrInpSym,
                                 To_TruthChar(TableAlias(i,j)) );
                    EXIT ColLoop;
                END IF;
                EXIT RowLoop WHEN NOT ( TruthTableMatch( DataInAlias(j),
                                                         TableAlias(i, j)));
              END LOOP RowLoop;
            END LOOP ColLoop;

        ELSE
            VitalError ( "VitalTruthTable", ErrTabWidSml );
        END IF;
        RETURN ReturnValue;
    END VitalTruthTable;

    FUNCTION VitalTruthTable  (
            CONSTANT TruthTable   : IN VitalTruthTableType;
            CONSTANT DataIn       : IN std_logic_vector
          ) RETURN std_logic IS

        CONSTANT InputSize  : INTEGER := DataIn'LENGTH;
        CONSTANT OutSize    : INTEGER := TruthTable'LENGTH(2) - InputSize;
        VARIABLE TempResult : std_logic_vector(OutSize - 1 DOWNTO 0)
                              := (OTHERS => 'X');
    BEGIN
        IF (OutSize > 0) THEN
            TempResult := VitalTruthTable(TruthTable, DataIn);
            IF ( 1 > OutSize) THEN
                VitalError ( "VitalTruthTable", ErrTabResSml );
            ELSIF ( 1 < OutSize) THEN
                VitalError ( "VitalTruthTable", ErrTabResLrg );
            END IF;
            RETURN (TempResult(0));
        ELSE
            VitalError ( "VitalTruthTable", ErrTabWidSml );
            RETURN 'X';
        END IF;
    END VitalTruthTable;

    PROCEDURE VitalTruthTable (
            SIGNAL   Result     : OUT std_logic_vector;
            CONSTANT TruthTable : IN  VitalTruthTableType;
            SIGNAL   DataIn     : IN  std_logic_vector        -- IR#236
    ) IS
        CONSTANT ResLeng     : INTEGER := Result'LENGTH;
        CONSTANT ActResLen   : INTEGER := TruthTable'LENGTH(2) - DataIn'LENGTH;
        CONSTANT FinalResLen : INTEGER := Minimum(ActResLen, ResLeng);
        VARIABLE TempResult  : std_logic_vector(ActResLen - 1 DOWNTO 0)
                                := (OTHERS => 'X');

    BEGIN
        TempResult := VitalTruthTable(TruthTable, DataIn);

        IF (ResLeng > ActResLen) THEN
            VitalError ( "VitalTruthTable", ErrTabResSml );
        ELSIF (ResLeng < ActResLen) THEN
            VitalError ( "VitalTruthTable", ErrTabResLrg );
        END IF;
        TempResult(FinalResLen-1 DOWNTO 0) := TempResult(FinalResLen-1 DOWNTO 0);
        Result <= TempResult;

    END VitalTruthTable;

    PROCEDURE VitalTruthTable (
            SIGNAL   Result     : OUT std_logic;
            CONSTANT TruthTable : IN VitalTruthTableType;
            SIGNAL DataIn       : IN std_logic_vector        -- IR#236
    ) IS

        CONSTANT ActResLen  : INTEGER := TruthTable'LENGTH(2) - DataIn'LENGTH;
        VARIABLE TempResult : std_logic_vector(ActResLen - 1 DOWNTO 0)
                              := (OTHERS => 'X');

    BEGIN
        TempResult := VitalTruthTable(TruthTable, DataIn);

        IF ( 1 > ActResLen) THEN
            VitalError ( "VitalTruthTable", ErrTabResSml );
        ELSIF ( 1 < ActResLen) THEN
            VitalError ( "VitalTruthTable", ErrTabResLrg );
        END IF;
        IF (ActResLen >  0) THEN
            Result <= TempResult(0);
        END IF;

    END VitalTruthTable;

    -- ------------------------------------------------------------------------
    PROCEDURE VitalStateTable (
            VARIABLE Result         : INOUT std_logic_vector;
            VARIABLE PreviousDataIn : INOUT std_logic_vector;
            CONSTANT StateTable     : IN VitalStateTableType;
            CONSTANT DataIn         : IN std_logic_vector;
            CONSTANT NumStates      : IN NATURAL
    ) IS

        CONSTANT InputSize     : INTEGER := DataIn'LENGTH;
        CONSTANT OutSize       : INTEGER
                                 := StateTable'LENGTH(2) - InputSize - NumStates;
        CONSTANT ResLeng       : INTEGER := Result'LENGTH;
        VARIABLE DataInAlias   : std_logic_vector(0 TO DataIn'LENGTH-1)
                                 := To_X01(DataIn);
        VARIABLE PrevDataAlias : std_logic_vector(0 TO PreviousDataIn'LENGTH-1)
                                 := To_X01(PreviousDataIn);
        VARIABLE ResultAlias   : std_logic_vector(0 TO ResLeng-1)
                                 := To_X01(Result);
        VARIABLE ExpResult     : std_logic_vector(0 TO OutSize-1);

    BEGIN
        IF (PreviousDataIn'LENGTH < DataIn'LENGTH) THEN
            VitalError ( "VitalStateTable", ErrVctLng, "PreviousDataIn<DataIn");

            ResultAlias := (OTHERS => 'X');
            Result := ResultAlias;

        ELSIF (OutSize <= 0) THEN
            VitalError ( "VitalStateTable", ErrTabWidSml );

            ResultAlias := (OTHERS => 'X');
            Result := ResultAlias;

        ELSE
            IF (ResLeng > OutSize) THEN
                VitalError ( "VitalStateTable", ErrTabResSml );
            ELSIF (ResLeng < OutSize) THEN
                VitalError ( "VitalStateTable", ErrTabResLrg );
            END IF;

            ExpResult := StateTableLookUp ( StateTable, DataInAlias,
                                            PrevDataAlias, NumStates,
                                            ResultAlias);
            ResultAlias := (OTHERS => 'X');
            ResultAlias ( Maximum(0, ResLeng - OutSize) TO ResLeng - 1)
                   := ExpResult(Maximum(0, OutSize - ResLeng) TO OutSize-1);

            Result := ResultAlias;
            PrevDataAlias(0 TO InputSize - 1) := DataInAlias;
            PreviousDataIn := PrevDataAlias;

        END IF;
    END VitalStateTable;


    PROCEDURE VitalStateTable (
            VARIABLE Result         : INOUT std_logic;        -- states
            VARIABLE PreviousDataIn : INOUT std_logic_vector; -- previous inputs and states
            CONSTANT StateTable     : IN VitalStateTableType; -- User's StateTable data
            CONSTANT DataIn         : IN std_logic_vector     -- Inputs
    ) IS

        VARIABLE ResultAlias : std_logic_vector(0 TO 0);
    BEGIN
        ResultAlias(0) := Result;
        VitalStateTable ( StateTable     => StateTable,
                          DataIn         => DataIn,
                          NumStates      => 1,
                          Result         => ResultAlias,
                          PreviousDataIn => PreviousDataIn
                        );
        Result := ResultAlias(0);

    END VitalStateTable;

    PROCEDURE VitalStateTable (
            SIGNAL   Result     : INOUT std_logic_vector;
            CONSTANT StateTable : IN VitalStateTableType;
            SIGNAL   DataIn     : IN std_logic_vector;
            CONSTANT NumStates  : IN NATURAL
    ) IS

        CONSTANT InputSize   : INTEGER := DataIn'LENGTH;
        CONSTANT OutSize     : INTEGER
                               := StateTable'LENGTH(2) - InputSize - NumStates;
        CONSTANT ResLeng     : INTEGER := Result'LENGTH;

        VARIABLE PrevData    : std_logic_vector(0 TO DataIn'LENGTH-1)
                               := (OTHERS => 'X');
        VARIABLE DataInAlias : std_logic_vector(0 TO DataIn'LENGTH-1);
        VARIABLE ResultAlias : std_logic_vector(0 TO ResLeng-1);
        VARIABLE ExpResult   : std_logic_vector(0 TO OutSize-1);

    BEGIN
        IF (OutSize <= 0) THEN
            VitalError ( "VitalStateTable", ErrTabWidSml );

            ResultAlias  := (OTHERS => 'X');
            Result <= ResultAlias;

        ELSE
            IF (ResLeng > OutSize) THEN
                VitalError ( "VitalStateTable", ErrTabResSml );
            ELSIF (ResLeng < OutSize) THEN
                VitalError ( "VitalStateTable", ErrTabResLrg );
            END IF;

            LOOP
                DataInAlias := To_X01(DataIn);
                ResultAlias := To_X01(Result);
                ExpResult   := StateTableLookUp ( StateTable, DataInAlias,
                                                  PrevData, NumStates,
                                                  ResultAlias);
                ResultAlias := (OTHERS => 'X');
                ResultAlias(Maximum(0, ResLeng - OutSize) TO ResLeng-1)
                       := ExpResult(Maximum(0, OutSize - ResLeng) TO OutSize-1);

                Result   <= ResultAlias;
                PrevData := DataInAlias;

                WAIT ON DataIn;
            END LOOP;

        END IF;

    END VitalStateTable;

    PROCEDURE VitalStateTable (
            SIGNAL   Result     : INOUT std_logic;
            CONSTANT StateTable : IN VitalStateTableType;
            SIGNAL   DataIn     : IN std_logic_vector
    ) IS

        CONSTANT InputSize   : INTEGER := DataIn'LENGTH;
        CONSTANT OutSize     : INTEGER := StateTable'LENGTH(2) - InputSize-1;

        VARIABLE PrevData    : std_logic_vector(0 TO DataIn'LENGTH-1)
                               := (OTHERS => 'X');
        VARIABLE DataInAlias : std_logic_vector(0 TO DataIn'LENGTH-1);
        VARIABLE ResultAlias : std_logic_vector(0 TO 0);
        VARIABLE ExpResult   : std_logic_vector(0 TO OutSize-1);

    BEGIN
        IF (OutSize <= 0) THEN
            VitalError ( "VitalStateTable", ErrTabWidSml );

            Result <= 'X';

        ELSE
            IF ( 1 > OutSize) THEN
                VitalError ( "VitalStateTable", ErrTabResSml );
            ELSIF ( 1 < OutSize) THEN
                VitalError ( "VitalStateTable", ErrTabResLrg );
            END IF;

            LOOP
                ResultAlias(0) := To_X01(Result);
                DataInAlias := To_X01(DataIn);
                ExpResult   := StateTableLookUp ( StateTable, DataInAlias,
                                                  PrevData, 1, ResultAlias);

                Result   <= ExpResult(OutSize-1);
                PrevData := DataInAlias;

                WAIT ON DataIn;
            END LOOP;
        END IF;

    END VitalStateTable;

    -- ------------------------------------------------------------------------
    -- std_logic resolution primitive
    -- ------------------------------------------------------------------------
    PROCEDURE VitalResolve (
            SIGNAL              q : OUT std_ulogic;
            SIGNAL         Data :  IN std_logic_vector  --IR236 4/2/98
    ) IS
        VARIABLE uData : std_ulogic_vector(Data'RANGE);
    BEGIN
        FOR i IN Data'RANGE LOOP
            uData(i) := Data(i);
        END LOOP;
        q <= resolved(uData);
    END;

END VITAL_Primitives;

