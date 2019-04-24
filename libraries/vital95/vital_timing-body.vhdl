-------------------------------------------------------------------------------
-- Title        : Standard VITAL TIMING Package
--              : $Revision$
-- Library      : VITAL
--              :  
-- Developers   : IEEE DASC Timing Working Group (TWG), PAR 1076.4
--              :  
-- Purpose      : This packages defines standard types, attributes, constants,
--              : functions and procedures for use in developing ASIC models.
--              : This file contains the Package Body.
-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------
-- Modification History : 
-- ----------------------------------------------------------------------------
-- Version No:|Auth:| Mod.Date:| Changes Made:
--   v95.0 A  |     | 06/08/95 | Initial ballot draft 1995
--   v95.1    |     | 08/31/95 | #203 - Timing violations at time 0
--                               #204 - Output mapping prior to glitch detection
-- ----------------------------------------------------------------------------

LIBRARY STD;
USE STD.TEXTIO.ALL;

PACKAGE BODY VITAL_Timing IS

    -- --------------------------------------------------------------------
    -- Package Local Declarations
    -- --------------------------------------------------------------------
    TYPE CheckType IS ( SetupCheck, HoldCheck, RecoveryCheck, RemovalCheck,
                        PulseWidCheck, PeriodCheck );

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
    CONSTANT EdgeSymbolMatch : EdgeSymbolMatchType :=  (
      'X'=>('X'=>(                                 OTHERS => FALSE),
            '0'=>('N'|'F'|'v'|'E'|'D'|'*' => TRUE, OTHERS => FALSE ),
            '1'=>('P'|'R'|'^'|'E'|'A'|'*' => TRUE, OTHERS => FALSE ) ),
      '0'=>('X'=>(    'r'|'p'|'R'|'A'|'*' => TRUE, OTHERS => FALSE ),
            '0'=>(                                 OTHERS => FALSE ),
            '1'=>(    '/'|'P'|'p'|'R'|'*' => TRUE, OTHERS => FALSE ) ),
      '1'=>('X'=>(    'f'|'n'|'F'|'D'|'*' => TRUE, OTHERS => FALSE ),
            '0'=>(    '\'|'N'|'n'|'F'|'*' => TRUE, OTHERS => FALSE ),
            '1'=>(                                 OTHERS => FALSE ) ) );

    ---------------------------------------------------------------------------
    ---------------------------------------------------------------------------
    -- Misc Utilities Local Utilities
    ---------------------------------------------------------------------------
    -----------------------------------------------------------------------
    FUNCTION Minimum ( CONSTANT t1,t2 : IN TIME ) RETURN TIME IS
    BEGIN
        IF ( t1 < t2 ) THEN RETURN (t1); ELSE RETURN (t2); END IF;
    END Minimum;
    -----------------------------------------------------------------------
    FUNCTION Maximum ( CONSTANT t1,t2 : IN TIME ) RETURN TIME IS
    BEGIN
        IF ( t1 > t2 ) THEN RETURN (t1); ELSE RETURN (t2); END IF;
    END Maximum;

    --------------------------------------------------------------------
    -- Error Message Types and Tables
    --------------------------------------------------------------------
    TYPE VitalErrorType IS (
        ErrVctLng  ,
        ErrNoPath  ,
        ErrNegPath ,
        ErrNegDel
    );
 
    TYPE VitalErrorSeverityType IS ARRAY (VitalErrorType) OF SEVERITY_LEVEL;
    CONSTANT VitalErrorSeverity : VitalErrorSeverityType := (
        ErrVctLng    => ERROR,
        ErrNoPath    => WARNING,
        ErrNegPath   => WARNING,
        ErrNegDel    => WARNING
    );
 
    CONSTANT MsgNoPath : STRING :=
      "No Delay Path Condition TRUE.  0-delay used. Output signal is: ";
    CONSTANT MsgNegPath : STRING :=
      "Path Delay less than time since input. 0 delay used. Output signal is: ";
    CONSTANT MsgNegDel : STRING :=
      "Negative delay. New output value not scheduled. Output signal is: ";
    CONSTANT MsgVctLng : STRING :=
      "Vector (array) lengths not equal. ";
 
    CONSTANT MsgUnknown : STRING :=
      "Unknown error message.";
 
    FUNCTION VitalMessage (
            CONSTANT ErrorId : IN VitalErrorType
          ) RETURN STRING IS
    BEGIN
        CASE ErrorId IS
            WHEN ErrVctLng    => RETURN MsgVctLng;
            WHEN ErrNoPath    => RETURN MsgNoPath;
            WHEN ErrNegPath   => RETURN MsgNegPath;
            WHEN ErrNegDel    => RETURN MsgNegDel;
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
    -- Time Delay Assignment Subprograms
    ---------------------------------------------------------------------------
    FUNCTION VitalExtendToFillDelay ( 
            CONSTANT Delay : IN VitalDelayType
          ) RETURN VitalDelayType01Z IS
    BEGIN
        RETURN (OTHERS => Delay);
    END VitalExtendToFillDelay;

    FUNCTION VitalExtendToFillDelay ( 
            CONSTANT Delay : IN VitalDelayType01
          ) RETURN VitalDelayType01Z IS
        VARIABLE Delay01Z  : VitalDelayType01Z;
    BEGIN
        Delay01Z(tr01) := Delay(tr01);
        Delay01Z(tr0z) := Delay(tr01);
        Delay01Z(trz1) := Delay(tr01);
        Delay01Z(tr10) := Delay(tr10);
        Delay01Z(tr1z) := Delay(tr10);
        Delay01Z(trz0) := Delay(tr10);
        RETURN (Delay01Z);
    END VitalExtendToFillDelay;

    FUNCTION VitalExtendToFillDelay ( 
            CONSTANT Delay : IN VitalDelayType01Z
          ) RETURN VitalDelayType01Z IS
    BEGIN
        RETURN Delay;
    END VitalExtendToFillDelay;

    ---------------------------------------------------------------------------
    FUNCTION VitalCalcDelay (
            CONSTANT NewVal : IN std_ulogic   := 'X';
            CONSTANT OldVal : IN std_ulogic   := 'X';
            CONSTANT Delay  : IN VitalDelayType
          ) RETURN TIME IS
    BEGIN
          RETURN delay;
    END VitalCalcDelay;

    FUNCTION VitalCalcDelay (
            CONSTANT NewVal : IN std_ulogic   := 'X';
            CONSTANT OldVal : IN std_ulogic   := 'X';
            CONSTANT Delay  : IN VitalDelayType01
          ) RETURN TIME IS
        VARIABLE Result : TIME;
    BEGIN
        CASE Newval IS
          WHEN '0' | 'L' => Result := Delay(tr10);
          WHEN '1' | 'H' => Result := Delay(tr01);
          WHEN 'Z' =>
            CASE Oldval IS
              WHEN '0' | 'L' => Result := Delay(tr01);
              WHEN '1' | 'H' => Result := Delay(tr10);
              WHEN OTHERS    => Result := MAXIMUM(Delay(tr10), Delay(tr01));
            END CASE;
          WHEN OTHERS =>
            CASE Oldval IS
              WHEN '0' | 'L' => Result := Delay(tr01);
              WHEN '1' | 'H' => Result := Delay(tr10);
              WHEN 'Z'       => Result := MINIMUM(Delay(tr10), Delay(tr01));
              WHEN OTHERS    => Result := MAXIMUM(Delay(tr10), Delay(tr01));
            END CASE;
        END CASE;
        RETURN Result;
    END VitalCalcDelay;

    FUNCTION VitalCalcDelay (
            CONSTANT NewVal : IN std_ulogic   := 'X';
            CONSTANT OldVal : IN std_ulogic   := 'X';
            CONSTANT Delay  : IN VitalDelayType01Z
          ) RETURN TIME IS
        VARIABLE Result : TIME;
    BEGIN
        CASE Oldval IS
          WHEN '0' | 'L' =>
            CASE Newval IS
              WHEN '0' | 'L' => Result := Delay(tr10);
              WHEN '1' | 'H' => Result := Delay(tr01);
              WHEN 'Z'       => Result := Delay(tr0z);
              WHEN OTHERS    => Result := MINIMUM(Delay(tr01), Delay(tr0z));
            END CASE;
          WHEN '1' | 'H' =>
            CASE Newval IS
              WHEN '0' | 'L' => Result := Delay(tr10);
              WHEN '1' | 'H' => Result := Delay(tr01);
              WHEN 'Z'       => Result := Delay(tr1z);
              WHEN OTHERS    => Result := MINIMUM(Delay(tr10), Delay(tr1z));
            END CASE;
          WHEN 'Z' =>
            CASE Newval IS
              WHEN '0' | 'L' => Result := Delay(trz0);
              WHEN '1' | 'H' => Result := Delay(trz1);
              WHEN 'Z'       => Result := MAXIMUM (Delay(tr0z), Delay(tr1z));
              WHEN OTHERS    => Result := MINIMUM (Delay(trz1), Delay(trz0));
            END CASE;
          WHEN 'U' | 'X' | 'W' | '-'  =>
            CASE Newval IS
              WHEN '0' | 'L' => Result := MAXIMUM(Delay(tr10), Delay(trz0));
              WHEN '1' | 'H' => Result := MAXIMUM(Delay(tr01), Delay(trz1));
              WHEN 'Z'       => Result := MAXIMUM(Delay(tr1z), Delay(tr0z));
              WHEN OTHERS    => Result := MAXIMUM(Delay(tr10), Delay(tr01));
            END CASE;
        END CASE;
        RETURN Result;
    END VitalCalcDelay;

    ---------------------------------------------------------------------------
    FUNCTION VitalSelectPathDelay (
            CONSTANT NewValue      : IN  std_logic;
            CONSTANT OldValue      : IN  std_logic;
            CONSTANT OutSignalName : IN  string;
            CONSTANT Paths         : IN  VitalPathArrayType;
            CONSTANT DefaultDelay  : IN  VitalDelayType
          ) RETURN TIME IS
 
        VARIABLE TmpDelay  : TIME;
        VARIABLE InputAge  : TIME := TIME'HIGH;
        VARIABLE PropDelay : TIME := TIME'HIGH;
    BEGIN
        -- for each delay path
        FOR i IN Paths'RANGE LOOP
            -- ignore the delay path if it is not enabled
            NEXT WHEN NOT Paths(i).PathCondition;
            -- ignore the delay path if a more recent input event has been seen
            NEXT WHEN Paths(i).InputChangeTime > InputAge;
 
            -- This is the most recent input change (so far)
            -- Get the transition dependent delay
            TmpDelay := VitalCalcDelay(NewValue, OldValue, Paths(i).PathDelay);
 
            -- If other inputs changed at the same time,
            --  then use the minimum of their propagation delays,
            --  else use the propagation delay from this input.
            IF Paths(i).InputChangeTime < InputAge THEN
                PropDelay := TmpDelay;
            ELSE -- Simultaneous inputs change
                IF TmpDelay < PropDelay THEN PropDelay := TmpDelay; END IF;
            end if;
 
            InputAge := Paths(i).InputChangeTime;
        END LOOP;
 
        -- If there were no paths (with an enabled condition),
        --  use the default the delay
        IF (PropDelay  = TIME'HIGH ) THEN
            PropDelay := VitalCalcDelay(NewValue, OldValue, DefaultDelay);
 
        -- If the time since the most recent input event is greater than the
        -- propagation delay from that input then
        --  use the default the delay
        ELSIF (InputAge > PropDelay) THEN
            PropDelay := VitalCalcDelay(NewValue, OldValue, DefaultDelay);
 
        -- Adjust the propagation delay by the time since the
        -- the input event occurred  (Usually 0 ns).
        ELSE
            PropDelay := PropDelay - InputAge;
        END IF;
 
        RETURN PropDelay;
    END;
 
    FUNCTION VitalSelectPathDelay (
            CONSTANT NewValue      : IN  std_logic;
            CONSTANT OldValue      : IN  std_logic;
            CONSTANT OutSignalName : IN  string;
            CONSTANT Paths         : IN  VitalPathArray01Type;
            CONSTANT DefaultDelay  : IN  VitalDelayType01
          ) RETURN TIME IS
 
        VARIABLE TmpDelay  : TIME;
        VARIABLE InputAge  : TIME := TIME'HIGH;
        VARIABLE PropDelay : TIME := TIME'HIGH;
    BEGIN
        -- for each delay path
        FOR i IN Paths'RANGE LOOP
            -- ignore the delay path if it is not enabled
            NEXT WHEN NOT Paths(i).PathCondition;
            -- ignore the delay path if a more recent input event has been seen
            NEXT WHEN Paths(i).InputChangeTime > InputAge;
 
            -- This is the most recent input change (so far)
            -- Get the transition dependent delay
            TmpDelay := VitalCalcDelay(NewValue, OldValue, Paths(i).PathDelay);
 
            -- If other inputs changed at the same time,
            --  then use the minimum of their propagation delays,
            --  else use the propagation delay from this input.
            IF Paths(i).InputChangeTime < InputAge THEN
                PropDelay := TmpDelay;
            ELSE -- Simultaneous inputs change
                IF TmpDelay < PropDelay THEN PropDelay := TmpDelay; END IF;
            end if;
 
            InputAge := Paths(i).InputChangeTime;
        END LOOP;
 
        -- If there were no paths (with an enabled condition),
        --  use the default the delay
        IF (PropDelay  = TIME'HIGH ) THEN
            PropDelay := VitalCalcDelay(NewValue, OldValue, DefaultDelay);
 
        -- If the time since the most recent input event is greater than the
        -- propagation delay from that input then
        --  use the default the delay
        ELSIF (InputAge > PropDelay) THEN
            PropDelay := VitalCalcDelay(NewValue, OldValue, DefaultDelay);
 
        -- Adjust the propagation delay by the time since the
        -- the input event occurred  (Usually 0 ns).
        ELSE
            PropDelay := PropDelay - InputAge;
        END IF;
 
        RETURN PropDelay;
    END;
 
    FUNCTION VitalSelectPathDelay (
            CONSTANT NewValue      : IN  std_logic;
            CONSTANT OldValue      : IN  std_logic;
            CONSTANT OutSignalName : IN  string;
            CONSTANT Paths         : IN  VitalPathArray01ZType;
            CONSTANT DefaultDelay  : IN  VitalDelayType01Z
          ) RETURN TIME IS
 
        VARIABLE TmpDelay  : TIME;
        VARIABLE InputAge  : TIME := TIME'HIGH;
        VARIABLE PropDelay : TIME := TIME'HIGH;
    BEGIN
        -- for each delay path
        FOR i IN Paths'RANGE LOOP
            -- ignore the delay path if it is not enabled
            NEXT WHEN NOT Paths(i).PathCondition;
            -- ignore the delay path if a more recent input event has been seen
            NEXT WHEN Paths(i).InputChangeTime > InputAge;
 
            -- This is the most recent input change (so far)
            -- Get the transition dependent delay
            TmpDelay := VitalCalcDelay(NewValue, OldValue, Paths(i).PathDelay);
 
            -- If other inputs changed at the same time,
            --  then use the minimum of their propagation delays,
            --  else use the propagation delay from this input.
            IF Paths(i).InputChangeTime < InputAge THEN
                PropDelay := TmpDelay;
            ELSE -- Simultaneous inputs change
                IF TmpDelay < PropDelay THEN PropDelay := TmpDelay; END IF;
            end if;
 
            InputAge := Paths(i).InputChangeTime;
        END LOOP;
 
        -- If there were no paths (with an enabled condition),
        --  use the default the delay
        IF (PropDelay  = TIME'HIGH ) THEN
            PropDelay := VitalCalcDelay(NewValue, OldValue, DefaultDelay);
 
        -- If the time since the most recent input event is greater than the
        -- propagation delay from that input then
        --  use the default the delay
        ELSIF (InputAge > PropDelay) THEN
            PropDelay := VitalCalcDelay(NewValue, OldValue, DefaultDelay);
 
        -- Adjust the propagation delay by the time since the
        -- the input event occurred  (Usually 0 ns).
        ELSE
            PropDelay := PropDelay - InputAge;
        END IF;
 
        RETURN PropDelay;
    END;
 
    ---------------------------------------------------------------------------
    ---------------------------------------------------------------------------
    -- Glitch Handlers
    ---------------------------------------------------------------------------
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

        -- Issue Report only if Preempted value has not been
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
    PROCEDURE VitalGlitch (
        SIGNAL   OutSignal     : OUT   std_logic;
        VARIABLE GlitchData    : INOUT VitalGlitchDataType;
        CONSTANT OutSignalName : IN    string;
        CONSTANT NewValue      : IN    std_logic;
        CONSTANT NewDelay      : IN    TIME           := 0 ns;
        CONSTANT Mode          : IN    VitalGlitchKindType := OnEvent;
        CONSTANT XOn           : IN    BOOLEAN        := TRUE;
        CONSTANT MsgOn         : IN    BOOLEAN        := FALSE;
        CONSTANT MsgSeverity   : IN    SEVERITY_LEVEL := WARNING
        ) IS
    ---------------------------------------------------------------------------
        VARIABLE NewGlitch : BOOLEAN := TRUE;
        VARIABLE dly       : TIME    := NewDelay;
 
    BEGIN
        -- If nothing to schedule, just return
        IF NewDelay < 0 ns THEN
            IF (NewValue /= GlitchData.SchedValue) THEN
                VitalError ( "VitalGlitch", ErrNegDel, OutSignalName );
            END IF;
            RETURN;
        END IF;
 
        -- If simple signal assignment
        --   perform the signal assignment
        IF    ( Mode = VitalInertial) THEN
            OutSignal <= NewValue AFTER dly;
        ELSIF ( Mode = VitalTransport ) THEN
            OutSignal <= TRANSPORT NewValue AFTER dly;
        ELSE
            -- Glitch Processing ---
            -- If nothing currently scheduled
            IF GlitchData.SchedTime <= NOW THEN
                -- Note: NewValue is always /= OldValue when called from VPPD
                IF (NewValue = GlitchData.SchedValue) THEN RETURN; END IF;
                -- No new glitch, save time for possable future glitch
                NewGlitch := FALSE;
                GlitchData.GlitchTime := NOW+dly;
     
            -- New value earlier than the earliest previous value scheduled
            ELSIF (NOW+dly <= GlitchData.GlitchTime)
                  AND (NOW+dly <= GlitchData.SchedTime) THEN
                -- No new glitch, save time for possible future glitch
                NewGlitch := FALSE;
                GlitchData.GlitchTime := NOW+dly;
         
            -- Transaction currently scheduled - if glitch already happened
            ELSIF GlitchData.GlitchTime <= NOW THEN
                IF (GlitchData.SchedValue = NewValue) THEN
                    dly := Minimum( GlitchData.SchedTime-NOW, NewDelay );
                END IF;
                NewGlitch := FALSE;
     
            -- Transaction currently scheduled (no glitch if same value)
            ELSIF (GlitchData.SchedValue = NewValue)
                  AND (GlitchData.SchedTime = GlitchData.GlitchTime) THEN
                -- revise scheduled output time if new delay is sooner
                dly := Minimum( GlitchData.SchedTime-NOW, NewDelay );
                -- No new glitch, save time for possable future glitch
                NewGlitch := FALSE;
                GlitchData.GlitchTime := NOW+dly;
     
            -- Transaction currently scheduled represents a glitch
            ELSE
                -- A new glitch has been detected
                NewGlitch := TRUE;
            END IF;
     
            IF NewGlitch THEN
                -- If  messages requested, report the glitch
                IF MsgOn THEN
                    ReportGlitch ("VitalGlitch", OutSignalName,
                                   GlitchData.GlitchTime, GlitchData.SchedValue,
                                   (dly + NOW), NewValue,
                                   MsgSeverity=>MsgSeverity );
                END IF;
     
                -- Force immediate glitch for "OnDetect" mode.
                IF (Mode = OnDetect) THEN
                    GlitchData.GlitchTime := NOW;
                END IF;
     
                -- If 'X' generation is requested, schedule the new value 
                --  preceeded by a glitch pulse.
                -- Otherwise just schedule the new value (inertial mode).
                IF XOn THEN
                    OutSignal <= 'X' AFTER GlitchData.GlitchTime-NOW;
                    OutSignal <= TRANSPORT NewValue AFTER dly;
                ELSE
                    OutSignal <= NewValue AFTER dly;
                END IF;
     
            -- If there no new glitch was detected, just schedule the new value.
            ELSE
                OutSignal <= NewValue AFTER dly;
            END IF;

        END IF;
 
        -- Record the new value and time just scheduled.
        GlitchData.SchedValue := NewValue;
        GlitchData.SchedTime  := NOW+dly;
        RETURN;
    END;
 
    ---------------------------------------------------------------------------
    PROCEDURE VitalPathDelay (
        SIGNAL   OutSignal     : OUT   std_logic;
        VARIABLE GlitchData    : INOUT VitalGlitchDataType;
        CONSTANT OutSignalName : IN    string;
        CONSTANT OutTemp       : IN    std_logic;
        CONSTANT Paths         : IN    VitalPathArrayType;
        CONSTANT DefaultDelay  : IN    VitalDelayType      := VitalZeroDelay;
        CONSTANT Mode          : IN    VitalGlitchKindType := OnEvent;
        CONSTANT XOn           : IN    BOOLEAN             := TRUE;
        CONSTANT MsgOn         : IN    BOOLEAN             := TRUE;
        CONSTANT MsgSeverity   : IN    SEVERITY_LEVEL      := WARNING
    ) IS
 
        VARIABLE PropDelay : TIME;
    BEGIN
        -- Check if the new value to be scheduled is different than the
        -- previously scheduled value
        IF (GlitchData.SchedTime <= NOW) AND
           (GlitchData.SchedValue = OutTemp)
             THEN RETURN;
        END IF;
 
        -- Evaluate propagation delay paths
        PropDelay := VitalSelectPathDelay (OutTemp, GlitchData.LastValue,
                                           OutSignalName, Paths, DefaultDelay);
        GlitchData.LastValue := OutTemp;
 
        -- Schedule the output transactions - including glitch handling
        VitalGlitch (OutSignal, GlitchData, OutSignalName, OutTemp,
                     PropDelay, Mode, XOn, MsgOn, MsgSeverity );
 
    END VitalPathDelay;

    PROCEDURE VitalPathDelay01 (
        SIGNAL   OutSignal     : OUT   std_logic;
        VARIABLE GlitchData    : INOUT VitalGlitchDataType;
        CONSTANT OutSignalName : IN    string;
        CONSTANT OutTemp       : IN    std_logic;
        CONSTANT Paths         : IN    VitalPathArray01Type;
        CONSTANT DefaultDelay  : IN    VitalDelayType01    := VitalZeroDelay01;
        CONSTANT Mode          : IN    VitalGlitchKindType := OnEvent;
        CONSTANT XOn           : IN    BOOLEAN             := TRUE;
        CONSTANT MsgOn         : IN    BOOLEAN             := TRUE;
        CONSTANT MsgSeverity   : IN    SEVERITY_LEVEL      := WARNING
    ) IS
 
        VARIABLE PropDelay : TIME;
    BEGIN
        -- Check if the new value to be scheduled is different than the
        -- previously scheduled value
        IF (GlitchData.SchedTime <= NOW) AND
           (GlitchData.SchedValue = OutTemp)
             THEN RETURN;
        END IF;
 
        -- Evaluate propagation delay paths
        PropDelay := VitalSelectPathDelay (OutTemp, GlitchData.LastValue,
                                           OutSignalName, Paths, DefaultDelay);
        GlitchData.LastValue := OutTemp;
 
        -- Schedule the output transactions - including glitch handling
        VitalGlitch (OutSignal, GlitchData, OutSignalName, OutTemp,
                     PropDelay, Mode, XOn, MsgOn, MsgSeverity );
 
    END VitalPathDelay01;

    PROCEDURE VitalPathDelay01Z (
        SIGNAL   OutSignal     : OUT   std_logic;
        VARIABLE GlitchData    : INOUT VitalGlitchDataType;
        CONSTANT OutSignalName : IN    string;
        CONSTANT OutTemp       : IN    std_logic;
        CONSTANT Paths         : IN    VitalPathArray01ZType;
        CONSTANT DefaultDelay  : IN    VitalDelayType01Z   := VitalZeroDelay01Z;
        CONSTANT Mode          : IN    VitalGlitchKindType := OnEvent;
        CONSTANT XOn           : IN    BOOLEAN             := TRUE;
        CONSTANT MsgOn         : IN    BOOLEAN             := TRUE;
        CONSTANT MsgSeverity   : IN    SEVERITY_LEVEL      := WARNING;
        CONSTANT OutputMap     : IN    VitalOutputMapType 
                                        := VitalDefaultOutputMap
    ) IS
 
        VARIABLE PropDelay : TIME;
    BEGIN
        -- Check if the new value to be scheduled is different than the
        -- previously scheduled value
        IF (GlitchData.SchedTime <= NOW) AND
           (GlitchData.SchedValue = OutputMap(OutTemp) )
             THEN RETURN;
        END IF;
 
        -- Evaluate propagation delay paths
        PropDelay := VitalSelectPathDelay (OutTemp, GlitchData.LastValue,
                                           OutSignalName, Paths, DefaultDelay);
        GlitchData.LastValue := OutTemp;
 
        -- Schedule the output transactions - including glitch handling
        VitalGlitch (OutSignal, GlitchData, OutSignalName, OutputMap(OutTemp),
                     PropDelay, Mode, XOn, MsgOn, MsgSeverity );
 
    END VitalPathDelay01Z;

    ----------------------------------------------------------------------------
    PROCEDURE VitalWireDelay (
            SIGNAL   OutSig  : OUT   std_ulogic;
            SIGNAL   InSig   : IN    std_ulogic;
            CONSTANT twire   : IN    VitalDelayType
    ) IS
    BEGIN
        OutSig <= TRANSPORT InSig AFTER twire;
    END VitalWireDelay;    

    PROCEDURE VitalWireDelay (
            SIGNAL   OutSig  : OUT   std_ulogic;
            SIGNAL   InSig   : IN    std_ulogic;
            CONSTANT twire   : IN    VitalDelayType01
    ) IS
       VARIABLE Delay   : TIME;
    BEGIN
        Delay := VitalCalcDelay( InSig, InSig'LAST_VALUE, twire );
        OutSig <= TRANSPORT InSig AFTER Delay;
    END VitalWireDelay;    

    PROCEDURE VitalWireDelay (
            SIGNAL   OutSig  : OUT   std_ulogic;
            SIGNAL   InSig   : IN    std_ulogic;
            CONSTANT twire   : IN    VitalDelayType01Z
    ) IS
       VARIABLE Delay   : TIME;
    BEGIN
        Delay := VitalCalcDelay( InSig, InSig'LAST_VALUE, twire );
        OutSig <= TRANSPORT InSig AFTER Delay;
    END VitalWireDelay;    

    ----------------------------------------------------------------------------
    PROCEDURE VitalSignalDelay (
            SIGNAL   OutSig     : OUT   std_ulogic;
            SIGNAL   InSig      : IN    std_ulogic;
            CONSTANT dly        : IN   TIME
    ) IS
    BEGIN
        OutSig <= TRANSPORT InSig AFTER dly;
    END;

    ---------------------------------------------------------------------------
    ---------------------------------------------------------------------------
    -- Setup and Hold Time Check Routine 
    ---------------------------------------------------------------------------
    ---------------------------------------------------------------------------
    PROCEDURE ReportViolation (
            CONSTANT TestSignalName : IN STRING := "";
            CONSTANT RefSignalName  : IN STRING := "";
            CONSTANT HeaderMsg      : IN STRING := " ";
            CONSTANT CheckInfo      : IN CheckInfoType;
            CONSTANT MsgSeverity    : IN SEVERITY_LEVEL := WARNING
    ) IS
        VARIABLE Message : LINE;
    BEGIN
        IF NOT CheckInfo.Violation THEN RETURN; END IF;

        Write ( Message, HeaderMsg );
        Case CheckInfo.CheckKind IS
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
    END ReportViolation;

    ---------------------------------------------------------------------------
    -- Procedure  : InternalTimingCheck
    ---------------------------------------------------------------------------
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
        VARIABLE bias, b2 : TIME;
    BEGIN
        -- Check SETUP constraint
      IF RefEdge THEN
          IF SetupEn THEN
            CheckInfo.ObsTime   := RefTime - TestTime;
            CheckInfo.State     := To_X01(TestSignal);
            CASE CheckInfo.State IS
              WHEN '0' => CheckInfo.ExpTime := SetupLow;
              WHEN '1' => CheckInfo.ExpTime := SetupHigh;
              WHEN 'X' => CheckInfo.ExpTime := Maximum(SetupHigh,SetupLow);
            END CASE;
            CheckInfo.Violation := CheckInfo.ObsTime < CheckInfo.ExpTime;
            SetupEn  := FALSE;
          ELSE
            CheckInfo.Violation := FALSE;
          END IF;
  
        -- Check HOLD constraint
      ELSIF TestEvent THEN
          IF HoldEn THEN
            CheckInfo.ObsTime := TestTime - RefTime;
            CheckInfo.State := To_X01(TestSignal);
            CASE CheckInfo.State IS
              WHEN '0' => CheckInfo.ExpTime := HoldHigh;
              WHEN '1' => CheckInfo.ExpTime := HoldLow;
              WHEN 'X' => CheckInfo.ExpTime := Maximum(HoldHigh,HoldLow);
            END CASE;
            CheckInfo.Violation   := CheckInfo.ObsTime < CheckInfo.ExpTime;
            HoldEn := NOT CheckInfo.Violation;
          ELSE
            CheckInfo.Violation := FALSE;
          END IF;
      ELSE
          CheckInfo.Violation := FALSE;
      END IF;

      -- Adjust report values to account for internal model delays
      -- Note: TestDelay, RefDelay, TestTime, RefTime and bias are non-negative
      IF MsgOn AND CheckInfo.Violation THEN
          bias := TestDelay - RefDelay;
          IF TestTime - RefTime <= bias THEN
            CheckInfo.CheckKind := SetupCheck;
            b2 := TIME'HIGH - bias;
            IF (CheckInfo.ObsTime <= b2)
              THEN CheckInfo.ObsTime := CheckInfo.ObsTime + bias;
              ELSE CheckInfo.ObsTime := Time'HIGH;
            END IF;
            IF (CheckInfo.ExpTime <= b2)
              THEN CheckInfo.ExpTime := CheckInfo.ExpTime + bias;
              ELSE CheckInfo.ExpTime := Time'HIGH;
            END IF;
            CheckInfo.DetTime := RefTime - RefDelay;
          ELSE
            CheckInfo.CheckKind :=  HoldCheck;
            CheckInfo.ObsTime := CheckInfo.ObsTime - bias;
            IF (CheckInfo.ExpTime >= 0 ns) THEN
              CheckInfo.ExpTime := CheckInfo.ExpTime - bias;
            END IF;
            CheckInfo.DetTime := TestTime - TestDelay;
          END IF;
      END IF;
    END InternalTimingCheck;

    ---------------------------------------------------------------------------
    ---------------------------------------------------------------------------
    FUNCTION VitalTimingDataInit 
            RETURN VitalTimingDataType IS
    BEGIN
        RETURN (FALSE,'X', 0 ns, FALSE, 'X', 0 ns, FALSE, NULL, NULL, NULL, NULL);
    END;

    ---------------------------------------------------------------------------
    -- Procedure  : VitalSetupHoldCheck
    ---------------------------------------------------------------------------
    PROCEDURE VitalSetupHoldCheck (
            VARIABLE Violation     : OUT    X01;
            VARIABLE TimingData    : INOUT  VitalTimingDataType;
            SIGNAL   TestSignal    : IN     std_ulogic;
            CONSTANT TestSignalName: IN     STRING := "";
            CONSTANT TestDelay     : IN     TIME := 0 ns;
            SIGNAL   RefSignal     : IN     std_ulogic;
            CONSTANT RefSignalName : IN     STRING := "";
            CONSTANT RefDelay      : IN     TIME := 0 ns;
            CONSTANT SetupHigh     : IN     TIME := 0 ns;
            CONSTANT SetupLow      : IN     TIME := 0 ns;
            CONSTANT HoldHigh      : IN     TIME := 0 ns;
            CONSTANT HoldLow       : IN     TIME := 0 ns;
            CONSTANT CheckEnabled  : IN     BOOLEAN := TRUE;
            CONSTANT RefTransition : IN     VitalEdgeSymbolType;
            CONSTANT HeaderMsg     : IN     STRING  := " ";
            CONSTANT XOn           : IN     BOOLEAN := TRUE;
            CONSTANT MsgOn         : IN     BOOLEAN := TRUE;
            CONSTANT MsgSeverity   : IN     SEVERITY_LEVEL := WARNING
    ) IS 

        VARIABLE CheckInfo : CheckInfoType;
        VARIABLE RefEdge, TestEvent : BOOLEAN;
        VARIABLE TestDly : TIME := Maximum(0 ns, TestDelay);
        VARIABLE RefDly  : TIME := Maximum(0 ns, RefDelay);
        VARIABLE bias : TIME;
    BEGIN

      IF (TimingData.NotFirstFlag = FALSE) THEN
         TimingData.TestLast := To_X01(TestSignal);
         TimingData.RefLast := To_X01(RefSignal);
         TimingData.NotFirstFlag := TRUE;
      END IF;

      -- Detect reference edges and record the time of the last edge
      RefEdge := EdgeSymbolMatch(TimingData.RefLast, To_X01(RefSignal),
                                 RefTransition);
      TimingData.RefLast := To_X01(RefSignal);
      IF RefEdge THEN
        TimingData.RefTime := NOW;
        TimingData.HoldEn  := TRUE;
      END IF;

      -- Detect test (data) changes and record the time of the last change
      TestEvent := TimingData.TestLast /= To_X01Z(TestSignal);
      TimingData.TestLast := To_X01Z(TestSignal);
      IF TestEvent THEN
        TimingData.TestTime := NOW;
        TimingData.SetupEn  := TRUE;
      END IF;

      -- Perform timing checks (if enabled)
      Violation := '0';
      IF (CheckEnabled) THEN
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
             MsgOn        => MsgOn );

        -- Report any detected violations and set return violation flag
        IF CheckInfo.Violation THEN
            IF (MsgOn) THEN
                ReportViolation (TestSignalName, RefSignalName,
                                 HeaderMsg, CheckInfo, MsgSeverity );
            END IF;
            IF (XOn) THEN Violation := 'X'; END IF;
        END IF;
      END IF;

    END VitalSetupHoldCheck;

    ---------------------------------------------------------------------------
    PROCEDURE VitalSetupHoldCheck (
            VARIABLE Violation     : OUT    X01;
            VARIABLE TimingData    : INOUT  VitalTimingDataType;
            SIGNAL   TestSignal    : IN     std_logic_vector;
            CONSTANT TestSignalName: IN     STRING := "";
            CONSTANT TestDelay     : IN     TIME := 0 ns;
            SIGNAL   RefSignal     : IN     std_ulogic;
            CONSTANT RefSignalName : IN     STRING := "";
            CONSTANT RefDelay      : IN     TIME := 0 ns;
            CONSTANT SetupHigh     : IN     TIME := 0 ns;
            CONSTANT SetupLow      : IN     TIME := 0 ns;
            CONSTANT HoldHigh      : IN     TIME := 0 ns;
            CONSTANT HoldLow       : IN     TIME := 0 ns;
            CONSTANT CheckEnabled  : IN     BOOLEAN := TRUE;
            CONSTANT RefTransition : IN     VitalEdgeSymbolType;
            CONSTANT HeaderMsg     : IN     STRING := " ";
            CONSTANT XOn           : IN     BOOLEAN := TRUE;
            CONSTANT MsgOn         : IN     BOOLEAN := TRUE;
            CONSTANT MsgSeverity   : IN     SEVERITY_LEVEL := WARNING
    ) IS 

        VARIABLE CheckInfo : CheckInfoType;
        VARIABLE RefEdge : BOOLEAN;
        VARIABLE TestEvent : VitalBoolArrayT(TestSignal'RANGE);
        VARIABLE TestDly : TIME := Maximum(0 ns, TestDelay);
        VARIABLE RefDly  : TIME := Maximum(0 ns, RefDelay);
        VARIABLE bias : TIME;
        VARIABLE ChangedAllAtOnce : BOOLEAN := TRUE;
        VARIABLE StrPtr1 : LINE;

    BEGIN
      -- Initialization of working area. 
      IF (TimingData.NotFirstFlag = FALSE) THEN
        TimingData.TestLastA := NEW std_logic_vector(TestSignal'RANGE);
        TimingData.TestTimeA := NEW  VitalTimeArrayT(TestSignal'RANGE);
        TimingData.HoldEnA   := NEW  VitalBoolArrayT(TestSignal'RANGE);
        TimingData.SetupEnA  := NEW  VitalBoolArrayT(TestSignal'RANGE);
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
        TimingData.HoldEnA.all := (TestSignal'RANGE=>TRUE);
      END IF;
  
      -- Detect test (data) changes and record the time of the last change
      FOR i IN TestSignal'RANGE LOOP
        TestEvent(i) := TimingData.TestLastA(i) /= To_X01Z(TestSignal(i));
        TimingData.TestLastA(i) := To_X01Z(TestSignal(i));
        IF TestEvent(i) THEN
          TimingData.TestTimeA(i) := NOW;
          TimingData.SetupEnA(i)  := TRUE;
          TimingData.TestTime := NOW;
        END IF;
      END LOOP;

      -- Check to see if the Bus subelements changed all at the same time.
      -- If so, then we can reduce the volume of error messages since we no
      -- longer have to report every subelement individually
      FOR i IN TestSignal'RANGE LOOP
        IF TimingData.TestTimeA(i) /= TimingData.TestTime THEN
            ChangedAllAtOnce := FALSE;
            EXIT;
        END IF;
      END LOOP;
  
      -- Perform timing checks (if enabled)
      Violation := '0';
      IF (CheckEnabled) THEN
        FOR i IN TestSignal'RANGE LOOP
          InternalTimingCheck (
              TestSignal   => TestSignal(i),
              RefSignal    => RefSignal,
              TestDelay    => TestDly,
              RefDelay     => RefDly,
              SetupHigh    => SetupHigh,
              SetupLow     => SetupLow,
              HoldHigh     => HoldHigh,
              HoldLow      => HoldLow,
              RefTime      => TimingData.RefTime,
              RefEdge      => RefEdge,
              TestTime     => TimingData.TestTimeA(i),
              TestEvent    => TestEvent(i),
              SetupEn      => TimingData.SetupEnA(i),
              HoldEn       => TimingData.HoldEnA(i),
              CheckInfo    => CheckInfo,
              MsgOn        => MsgOn );

          -- Report any detected violations and set return violation flag
          IF CheckInfo.Violation THEN
            IF (MsgOn) THEN
              IF ( ChangedAllAtOnce AND (i = TestSignal'LEFT) ) THEN
                ReportViolation (TestSignalName&"(...)", RefSignalName,
                                 HeaderMsg, CheckInfo, MsgSeverity );
              ELSIF (NOT ChangedAllAtOnce) THEN
                Write (StrPtr1, i);
                ReportViolation (TestSignalName & "(" & StrPtr1.ALL & ")", 
                                 RefSignalName,
                                 HeaderMsg, CheckInfo, MsgSeverity );
                DEALLOCATE (StrPtr1);
              END IF;
            END IF;
            IF (XOn) THEN
              Violation := 'X';
            END IF;
          END IF;
        END LOOP;
      END IF;

      DEALLOCATE (StrPtr1);

    END VitalSetupHoldCheck;
    
    ---------------------------------------------------------------------------
    -- Function   : VitalRecoveryRemovalCheck
    ---------------------------------------------------------------------------
    PROCEDURE VitalRecoveryRemovalCheck (
            VARIABLE Violation     : OUT    X01;
            VARIABLE TimingData    : INOUT  VitalTimingDataType;
            SIGNAL   TestSignal    : IN     std_ulogic;
            CONSTANT TestSignalName: IN     STRING := "";
            CONSTANT TestDelay     : IN     TIME := 0 ns;
            SIGNAL   RefSignal     : IN     std_ulogic;
            CONSTANT RefSignalName : IN     STRING := "";
            CONSTANT RefDelay      : IN     TIME := 0 ns;
            CONSTANT Recovery      : IN     TIME := 0 ns;
            CONSTANT Removal       : IN     TIME := 0 ns;
            CONSTANT ActiveLow     : IN     BOOLEAN := TRUE;
            CONSTANT CheckEnabled  : IN     BOOLEAN := TRUE;
            CONSTANT RefTransition : IN     VitalEdgeSymbolType;
            CONSTANT HeaderMsg     : IN     STRING := " ";
            CONSTANT XOn           : IN     BOOLEAN := TRUE;
            CONSTANT MsgOn         : IN     BOOLEAN := TRUE;
            CONSTANT MsgSeverity   : IN     SEVERITY_LEVEL := WARNING
    ) IS
        VARIABLE CheckInfo : CheckInfoType;
        VARIABLE RefEdge, TestEvent : BOOLEAN;
        VARIABLE TestDly : TIME := Maximum(0 ns, TestDelay);
        VARIABLE RefDly  : TIME := Maximum(0 ns, RefDelay);
        VARIABLE bias : TIME;
    BEGIN
 
      IF (TimingData.NotFirstFlag = FALSE) THEN
         TimingData.TestLast := To_X01(TestSignal);
         TimingData.RefLast := To_X01(RefSignal);
         TimingData.NotFirstFlag := TRUE;
      END IF;

      -- Detect reference edges and record the time of the last edge
      RefEdge := EdgeSymbolMatch(TimingData.RefLast, To_X01(RefSignal),
                                 RefTransition);
      TimingData.RefLast := To_X01(RefSignal);
      IF RefEdge THEN
        TimingData.RefTime := NOW;
        TimingData.HoldEn  := TRUE;
      END IF;
 
      -- Detect test (data) changes and record the time of the last change
      TestEvent := TimingData.TestLast /= To_X01Z(TestSignal);
      TimingData.TestLast := To_X01Z(TestSignal);
      IF TestEvent THEN
        TimingData.TestTime := NOW;
        TimingData.SetupEn  := TRUE;
      END IF;
 
      -- Perform timing checks (if enabled)
      Violation := '0';
      IF (CheckEnabled) THEN

        IF ActiveLow THEN
         InternalTimingCheck (
             TestSignal, RefSignal, TestDly, RefDly,
             Recovery, 0 ns, 0 ns, Removal,
             TimingData.RefTime, RefEdge,
             TimingData.TestTime, TestEvent,
             TimingData.SetupEn, TimingData.HoldEn,
             CheckInfo, MsgOn );
        ELSE 
         InternalTimingCheck (
             TestSignal, RefSignal, TestDly, RefDly,
             0 ns, Recovery, Removal, 0 ns,
             TimingData.RefTime, RefEdge,
             TimingData.TestTime, TestEvent,
             TimingData.SetupEn, TimingData.HoldEn,
             CheckInfo, MsgOn );
        END IF;

 
        -- Report any detected violations and set return violation flag
        IF CheckInfo.Violation THEN
            IF CheckInfo.CheckKind = SetupCheck THEN
                CheckInfo.CheckKind := RecoveryCheck;
            ELSE
                CheckInfo.CheckKind := RemovalCheck;
            END IF;
            IF (MsgOn) THEN
                ReportViolation (TestSignalName, RefSignalName, 
                                 HeaderMsg, CheckInfo, MsgSeverity );
            END IF;
            IF (XOn) THEN Violation := 'X'; END IF;
        END IF;
      END IF;

    END VitalRecoveryRemovalCheck;

    ---------------------------------------------------------------------------
    PROCEDURE VitalPeriodPulseCheck  (
            VARIABLE Violation      : OUT    X01;
            VARIABLE PeriodData     : INOUT  VitalPeriodDataType;
            SIGNAL   TestSignal     : IN     std_ulogic;
            CONSTANT TestSignalName : IN     STRING := "";
            CONSTANT TestDelay      : IN     TIME := 0 ns;
            CONSTANT Period         : IN     TIME := 0 ns;
            CONSTANT PulseWidthHigh : IN     TIME := 0 ns;
            CONSTANT PulseWidthLow  : IN     TIME := 0 ns;
            CONSTANT CheckEnabled   : IN     BOOLEAN := TRUE;
            CONSTANT HeaderMsg      : IN     STRING := " ";
            CONSTANT XOn            : IN     BOOLEAN := TRUE;
            CONSTANT MsgOn          : IN     BOOLEAN := TRUE;
            CONSTANT MsgSeverity    : IN     SEVERITY_LEVEL := WARNING
    ) IS 

        VARIABLE TestDly : TIME := Maximum(0 ns, TestDelay);
        VARIABLE CheckInfo : CheckInfoType;
        VARIABLE PeriodObs : TIME;
        VARIABLE PulseTest, PeriodTest : BOOLEAN;
        VARIABLE TestValue : X01 := To_X01(TestSignal);
    BEGIN

        IF (PeriodData.NotFirstFlag = FALSE) THEN
           PeriodData.Rise :=
                -maximum(Period, maximum(PulseWidthHigh, PulseWidthLow));
           PeriodData.Fall :=
                -maximum(Period, maximum(PulseWidthHigh, PulseWidthLow));
           PeriodData.Last := To_X01(TestSignal);
           PeriodData.NotFirstFlag := TRUE;
        END IF;

        -- Initialize for no violation
        -- No violation possible if no test signal change
        Violation := '0';
        IF (PeriodData.Last = TestValue) THEN
            RETURN;
        END IF;

        -- record starting pulse times
        IF EdgeSymbolMatch(PeriodData.Last, TestValue, 'P') THEN
            -- Compute period times, then record the High Rise Time
            PeriodObs := NOW - PeriodData.Rise;
            PeriodData.Rise := NOW;
            PeriodTest := TRUE;
        ELSIF EdgeSymbolMatch(PeriodData.Last, TestValue, 'N') THEN
            -- Compute period times, then record the Low Fall Time
            PeriodObs := NOW - PeriodData.Fall;
            PeriodData.Fall := NOW;
            PeriodTest := TRUE;
        ELSE
            PeriodTest := FALSE;
	END IF;

        -- do checks on pulse ends
        IF EdgeSymbolMatch(PeriodData.Last, TestValue, 'p') THEN
            -- Compute pulse times
            CheckInfo.ObsTime := NOW - PeriodData.Fall;
            CheckInfo.ExpTime := PulseWidthLow;
            PulseTest := TRUE;
        ELSIF EdgeSymbolMatch(PeriodData.Last, TestValue, 'n') THEN
            -- Compute pulse times
            CheckInfo.ObsTime := NOW - PeriodData.Rise;
            CheckInfo.ExpTime := PulseWidthHigh;
            PulseTest := TRUE;
        ELSE
            PulseTest := FALSE;
        END IF;

        IF PulseTest AND CheckEnabled THEN
           -- Verify Pulse Width [ignore 1st edge]
            IF ( CheckInfo.ObsTime < CheckInfo.ExpTime ) THEN
                IF (XOn) THEN Violation := 'X'; END IF;
                IF (MsgOn) THEN
                    CheckInfo.Violation := TRUE;
                    CheckInfo.CheckKind := PulseWidCheck;
                    CheckInfo.DetTime   := NOW - TestDly;
                    CheckInfo.State     := PeriodData.Last;
                    ReportViolation (TestSignalName, "", 
                                     HeaderMsg, CheckInfo, MsgSeverity );
                END IF; -- MsgOn
            END IF;
        END IF;

        IF PeriodTest AND CheckEnabled THEN
            -- Verify the Period [ignore 1st edge]
            CheckInfo.ObsTime := PeriodObs;
            CheckInfo.ExpTime := Period;
            IF ( CheckInfo.ObsTime < CheckInfo.ExpTime ) THEN
                IF (XOn) THEN Violation := 'X'; END IF;
                IF (MsgOn) THEN
                    CheckInfo.Violation := TRUE;
                    CheckInfo.CheckKind := PeriodCheck;
                    CheckInfo.DetTime   := NOW - TestDly;
                    CheckInfo.State     := TestValue;
                    ReportViolation (TestSignalName, "",
                                     HeaderMsg, CheckInfo, MsgSeverity );
                END IF; -- MsgOn
            END IF;
        END IF;

        PeriodData.Last := TestValue;

    END VitalPeriodPulseCheck;

END VITAL_Timing;

