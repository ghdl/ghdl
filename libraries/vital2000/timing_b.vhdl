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
--   v95.1    |     | 08/31/95 | #IR203 - Timing violations at time 0
--                               #IR204 - Output mapping prior to glitch detection
--   v98.0    |TAG  | 03/27/98 | Initial ballot draft 1998
--                             | #IR225 - Negative Premptive Glitch
--                                      **Code_effected=ReportGlitch,VitalGlitch,
--		                                VitalPathDelay,VitalPathDelay01,
--                                      VitalPathDelay01z.
--                               #IR105 - Skew timing check needed
--                                      **Code_effected=NONE, New code added!! 
--                               #IR245,IR246,IR251 ITC code to fix false boundry cases
--					                    **Code_effected=InternalTimingCheck.
--				                 #IR248 - Allows VPD to use a default timing delay
--                                      **Code_effected=VitalPathDelay,
--                                        VitalPathDelay01,VitalPathDelay01z,
--                                        VitalSelectPathDelay,VitalSelectPathDelay01,
--                                        VitalSelectPathDelay01z.
--				                 #IR250 - Corrects fastpath condition in VPD
--                                      **Code_effected=VitalPathDelay01,
--                                        VitalPathDelay01z,
--                               #IR252 - Corrects cancelled timing check call if
--                                        condition expires.  
--                                        **Code_effected=VitalSetupHoldCheck,
--                                        VitalRecoveryRemovalCheck.
--   v98.1    | jdc | 03/25/99 | Changed UseDefaultDelay to IgnoreDefaultDelay
--                               and set default to FALSE in VitalPathDelay()
--
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
    -- Tables used to implement 'posedge' and 'negedge' in path delays
    -- These are new tables for Skewcheck routines.  IR105
    ---------------------------------------------------------------------------
 
    TYPE EdgeRable IS ARRAY(std_ulogic, std_ulogic) OF boolean;
 
    CONSTANT Posedge : EdgeRable := (
    -- ------------------------------------------------------------------------
    -- |  U       X     0      1       Z      W      L      H     -
    -- ------------------------------------------------------------------------
       ( FALSE, FALSE, FALSE, TRUE , FALSE, FALSE, FALSE, TRUE , FALSE ), -- U
       ( FALSE, FALSE, FALSE, TRUE , FALSE, FALSE, FALSE, TRUE , FALSE ), -- X
       ( TRUE , TRUE , FALSE, TRUE , TRUE , TRUE , FALSE, TRUE , TRUE  ), -- 0
       ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ), -- 1
       ( FALSE, FALSE, FALSE, TRUE , FALSE, FALSE, FALSE, TRUE , FALSE ), -- Z
       ( FALSE, FALSE, FALSE, TRUE , FALSE, FALSE, FALSE, TRUE , FALSE ), -- W
       ( TRUE , TRUE , FALSE, TRUE , TRUE , TRUE , FALSE, TRUE , TRUE  ), -- L
       ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ), -- H
       ( FALSE, FALSE, FALSE, TRUE , FALSE, FALSE, FALSE, TRUE , FALSE )  -- -
 
    );	 --IR105
 
 
    CONSTANT Negedge : EdgeRable := (
    -- -----------------------------------------------------------------------
    -- |  U       X     0      1       Z      W      L      H     -
    -- -----------------------------------------------------------------------
       ( FALSE, FALSE, TRUE , FALSE, FALSE, FALSE, TRUE , FALSE, FALSE ), -- U
       ( FALSE, FALSE, TRUE , FALSE, FALSE, FALSE, TRUE , FALSE, FALSE ), -- X
       ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ), -- 0
       ( TRUE , TRUE , TRUE , FALSE, TRUE , TRUE , TRUE , FALSE, TRUE  ), -- 1
       ( FALSE, FALSE, TRUE , FALSE, FALSE, FALSE, TRUE , FALSE, FALSE ), -- Z
       ( FALSE, FALSE, TRUE , FALSE, FALSE, FALSE, TRUE , FALSE, FALSE ), -- W
       ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ), -- L
       ( TRUE , TRUE , TRUE , FALSE, TRUE , TRUE , TRUE , FALSE, TRUE  ), -- H
       ( FALSE, FALSE, TRUE , FALSE, FALSE, FALSE, TRUE , FALSE, FALSE ) --  -
 
    );	  --IR105
 
    TYPE SkewType IS (Inphase, Outphase);  --IR105

    CONSTANT noTrigger : TIME := -1 ns;	   --IR105
    ---------------------------------------------------------------------------
	--    End of Skew (IR105 additions)
    ---------------------------------------------------------------------------
   	
 
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
    --
    -- VitalSelectPathDelay returns the path delay selected by the Paths array.
    -- If no paths are selected, it returns either the appropriate default
    -- delay or TIME'HIGH, depending upon the value of IgnoreDefaultDelay.
    --

    FUNCTION VitalSelectPathDelay (
            CONSTANT NewValue           : IN  std_logic;
            CONSTANT OldValue           : IN  std_logic;
            CONSTANT OutSignalName      : IN  string;
            CONSTANT Paths              : IN  VitalPathArrayType;
            CONSTANT DefaultDelay       : IN  VitalDelayType;
            CONSTANT IgnoreDefaultDelay : IN  BOOLEAN
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
            -- then use the minimum of their propagation delays,
            -- else use the propagation delay from this input.
            IF Paths(i).InputChangeTime < InputAge THEN
                PropDelay := TmpDelay;
            ELSE -- Simultaneous inputs change
                IF TmpDelay < PropDelay THEN PropDelay := TmpDelay; END IF;
            end if;
 
            InputAge := Paths(i).InputChangeTime;
        END LOOP;
 
        -- If there were no paths (with an enabled condition),
        -- use the default delay, if so indicated, otherwise return TIME'HIGH
        IF (PropDelay = TIME'HIGH) THEN
	  IF (IgnoreDefaultDelay) THEN
            PropDelay := VitalCalcDelay(NewValue, OldValue, DefaultDelay);
	  END IF;
	  
	-- If the time since the most recent selected input event is
	-- greater than the propagation delay from that input,
	-- then use the default delay (won't happen if no paths are selected)
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
            CONSTANT NewValue           : IN  std_logic;
            CONSTANT OldValue           : IN  std_logic;
            CONSTANT OutSignalName      : IN  string;
            CONSTANT Paths              : IN  VitalPathArray01Type;
            CONSTANT DefaultDelay       : IN  VitalDelayType01;
            CONSTANT IgnoreDefaultDelay : IN  BOOLEAN
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
            -- then use the minimum of their propagation delays,
            -- else use the propagation delay from this input.
            IF Paths(i).InputChangeTime < InputAge THEN
                PropDelay := TmpDelay;
            ELSE -- Simultaneous inputs change
                IF TmpDelay < PropDelay THEN PropDelay := TmpDelay; END IF;
            end if;
 
            InputAge := Paths(i).InputChangeTime;
        END LOOP;
 
        -- If there were no paths (with an enabled condition),
        -- use the default delay, if so indicated, otherwise return TIME'HIGH
        IF (PropDelay = TIME'HIGH) THEN
	  IF (IgnoreDefaultDelay) THEN
            PropDelay := VitalCalcDelay(NewValue, OldValue, DefaultDelay);
	  END IF;
	  
	-- If the time since the most recent selected input event is
	-- greater than the propagation delay from that input,
	-- then use the default delay (won't happen if no paths are selected)
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
            CONSTANT NewValue           : IN  std_logic;
            CONSTANT OldValue           : IN  std_logic;
            CONSTANT OutSignalName      : IN  string;
            CONSTANT Paths              : IN  VitalPathArray01ZType;
            CONSTANT DefaultDelay       : IN  VitalDelayType01Z;
    	    CONSTANT IgnoreDefaultDelay : IN  BOOLEAN
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
            -- then use the minimum of their propagation delays,
            -- else use the propagation delay from this input.
            IF Paths(i).InputChangeTime < InputAge THEN
                PropDelay := TmpDelay;
            ELSE -- Simultaneous inputs change
                IF TmpDelay < PropDelay THEN PropDelay := TmpDelay; END IF;
            end if;
 
            InputAge := Paths(i).InputChangeTime;
        END LOOP;
 
        -- If there were no paths (with an enabled condition),
        -- use the default delay, if so indicated, otherwise return TIME'HIGH
        IF (PropDelay = TIME'HIGH) THEN
	  IF (IgnoreDefaultDelay) THEN
            PropDelay := VitalCalcDelay(NewValue, OldValue, DefaultDelay);
	  END IF;
	  
	-- If the time since the most recent selected input event is
	-- greater than the propagation delay from that input,
	-- then use the default delay (won't happen if no paths are selected)
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
          	

  ASSERT PreemptedTime <= NewTime
          REPORT GlitchRoutine & ": GLITCH Detected on port " & 
                 OutSignalName & StrPtr5.ALL &
                 "; Negative Preempted Value := " & StrPtr3.ALL &
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
        CONSTANT NegPreemptOn  : IN    BOOLEAN        := FALSE;	 --IR225
        CONSTANT MsgOn         : IN    BOOLEAN        := FALSE;
        CONSTANT MsgSeverity   : IN    SEVERITY_LEVEL := WARNING
        ) IS
    ---------------------------------------------------------------------------
        VARIABLE NewGlitch : BOOLEAN := TRUE;
        VARIABLE dly       : TIME    := NewDelay;
        VARIABLE NOW_TIME  : TIME    := NOW;
        VARIABLE NegPreemptGlitch : BOOLEAN := FALSE;
        	
    BEGIN
      NegPreemptGlitch:=FALSE;--reset Preempt-Glitch 
 
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
          IF GlitchData.SchedTime <= NOW THEN		  -- NOW >= last event
            -- Note: NewValue is always /= OldValue when called from VPPD
            IF (NewValue = GlitchData.SchedValue) THEN RETURN; END IF;
  		  NewGlitch := FALSE;
          GlitchData.GlitchTime := NOW+dly;
     
          -- New value earlier than the earliest previous value scheduled
          --  (negative preemptive)
          ELSIF (NOW+dly <= GlitchData.GlitchTime)
             AND (NOW+dly <= GlitchData.SchedTime)  THEN
      
            -- Glitch is negative preemptive - check if same value and
            -- NegPreempt is on IR225 
            IF (GlitchData.SchedValue /= NewValue) AND (NegPreemptOn) AND
               (NOW > 0 NS) THEN
              NewGlitch := TRUE;
              NegPreemptGlitch :=TRUE;	-- Set preempt Glitch condition
            ELSE 
              NewGlitch := FALSE;    	-- No new glitch, save time for
                                        -- possible future glitch
            END IF;
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
              NewGlitch := TRUE; -- A new glitch has been detected
            END IF;
     
            IF NewGlitch THEN
              -- If  messages requested, report the glitch
              IF MsgOn THEN 
                IF NegPreemptGlitch THEN	  --IR225
                  ReportGlitch ("VitalGlitch-Neg", OutSignalName,
                                 GlitchData.GlitchTime, GlitchData.SchedValue,
                                 (dly + NOW), NewValue,
                                 MsgSeverity=>MsgSeverity  );
                ELSE
	              ReportGlitch ("VitalGlitch", OutSignalName,
                                 GlitchData.GlitchTime, GlitchData.SchedValue,
                                 (dly + NOW), NewValue,
                                 MsgSeverity=>MsgSeverity  );
		        END IF;
	        END IF;
              
            -- If 'X' generation is requested, schedule the new value 
            --  preceeded by a glitch pulse.
            -- Otherwise just schedule the new value (inertial mode).
            IF XOn THEN 
              IF (Mode = OnDetect) THEN
                OutSignal <= 'X';
              ELSE
                OutSignal <= 'X' AFTER GlitchData.GlitchTime-NOW;
            END IF;
		                             	
            IF NegPreemptGlitch THEN -- IR225
               OutSignal <= TRANSPORT NewValue AFTER GlitchData.SchedTime-NOW;	
            ELSE
              OutSignal <= TRANSPORT NewValue AFTER dly;    
            END IF; 
            ELSE
              OutSignal <= NewValue AFTER dly; -- no glitch regular prop delay
            END IF;
     
            -- If there no new glitch was detected, just schedule the new value.
            ELSE
              OutSignal <= NewValue AFTER dly;
            END IF;
        END IF;
 
       -- Record the new value and time depending on glitch type just scheduled.
       IF NOT NegPreemptGlitch THEN -- 5/2/96 for "x-pulse" IR225
          GlitchData.SchedValue := NewValue; 
          GlitchData.SchedTime  := NOW+dly;	   -- pulse timing.
	   ELSE
         GlitchData.SchedValue := 'X';
         -- leave GlitchData.SchedTime to old value since glitch is negative
       END IF;
      RETURN;
    END;
 
    ---------------------------------------------------------------------------
   PROCEDURE VitalPathDelay (
        SIGNAL   OutSignal          : OUT   std_logic;
        VARIABLE GlitchData         : INOUT VitalGlitchDataType;
        CONSTANT OutSignalName      : IN    string;
        CONSTANT OutTemp            : IN    std_logic;
        CONSTANT Paths              : IN    VitalPathArrayType;
        CONSTANT DefaultDelay       : IN    VitalDelayType      := VitalZeroDelay;
        CONSTANT Mode               : IN    VitalGlitchKindType := OnEvent;
        CONSTANT XOn                : IN    BOOLEAN             := TRUE;
        CONSTANT MsgOn              : IN    BOOLEAN             := TRUE;
        CONSTANT MsgSeverity        : IN    SEVERITY_LEVEL      := WARNING;
        CONSTANT NegPreemptOn       : IN    BOOLEAN             := FALSE;	--IR225	3/14/98
        CONSTANT IgnoreDefaultDelay : IN    BOOLEAN             := FALSE        --IR248 3/14/98
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
                                           OutSignalName, Paths, DefaultDelay,
	                    				   IgnoreDefaultDelay);

        GlitchData.LastValue := OutTemp;
 
        -- Schedule the output transactions - including glitch handling
        VitalGlitch (OutSignal, GlitchData, OutSignalName, OutTemp,
                     PropDelay, Mode, XOn, NegPreemptOn, MsgOn, MsgSeverity );
 
    END VitalPathDelay;

    ---------------------------------------------------------------------------

    PROCEDURE VitalPathDelay01 (
        SIGNAL   OutSignal          : OUT   std_logic;
        VARIABLE GlitchData         : INOUT VitalGlitchDataType;
        CONSTANT OutSignalName      : IN    string;
        CONSTANT OutTemp            : IN    std_logic;
        CONSTANT Paths              : IN    VitalPathArray01Type;
        CONSTANT DefaultDelay       : IN    VitalDelayType01    := VitalZeroDelay01;
        CONSTANT Mode               : IN    VitalGlitchKindType := OnEvent;
        CONSTANT XOn                : IN    BOOLEAN             := TRUE;
        CONSTANT MsgOn              : IN    BOOLEAN             := TRUE;
        CONSTANT MsgSeverity        : IN    SEVERITY_LEVEL      := WARNING;
        CONSTANT NegPreemptOn       : IN    BOOLEAN             := FALSE;	--IR225	3/14/98
        CONSTANT IgnoreDefaultDelay : IN    BOOLEAN             := FALSE;       --IR248 3/14/98
        CONSTANT RejectFastPath     : IN    BOOLEAN             := FALSE        --IR250
 
 
    ) IS
 
        VARIABLE PropDelay : TIME;
    BEGIN
    	
        -- Check if the new value to be scheduled is different than the
        -- previously scheduled value
        IF (GlitchData.SchedTime <= NOW) AND
           (GlitchData.SchedValue = OutTemp)
             THEN RETURN;
        -- Check if the new value to be Scheduled is the same as the
        -- previously scheduled output transactions.  If this condition
        -- exists and the new scheduled time is < the current GlitchData.
        -- schedTime then a fast path condition exists (IR250).  If the 
        -- modeler wants this condition rejected by setting the 
        -- RejectFastPath actual to true then exit out.  
        ELSIF (GlitchData.SchedValue=OutTemp)  AND (RejectFastPath)
             THEN RETURN;
        END IF;
 
        -- Evaluate propagation delay paths
        PropDelay := VitalSelectPathDelay (OutTemp, GlitchData.LastValue,
                                           OutSignalName, Paths, DefaultDelay,
	                    				   IgnoreDefaultDelay);

        GlitchData.LastValue := OutTemp;


        VitalGlitch (OutSignal, GlitchData, OutSignalName, OutTemp,
                     PropDelay, Mode, XOn, NegPreemptOn, MsgOn, MsgSeverity );
    END VitalPathDelay01;

    ---------------------------------------------------------------------------
    PROCEDURE VitalPathDelay01Z (
        SIGNAL   OutSignal          : OUT   std_logic;
        VARIABLE GlitchData         : INOUT VitalGlitchDataType;
        CONSTANT OutSignalName      : IN    string;
        CONSTANT OutTemp            : IN    std_logic;
        CONSTANT Paths              : IN    VitalPathArray01ZType;
        CONSTANT DefaultDelay       : IN    VitalDelayType01Z   := VitalZeroDelay01Z;
        CONSTANT Mode               : IN    VitalGlitchKindType := OnEvent;
        CONSTANT XOn                : IN    BOOLEAN             := TRUE;
        CONSTANT MsgOn              : IN    BOOLEAN             := TRUE;
        CONSTANT MsgSeverity        : IN    SEVERITY_LEVEL      := WARNING;
        CONSTANT OutputMap          : IN    VitalOutputMapType	:= VitalDefaultOutputMap;
        CONSTANT NegPreemptOn       : IN    BOOLEAN             := FALSE;	--IR225	3/14/98
        CONSTANT IgnoreDefaultDelay : IN    BOOLEAN             := FALSE;       --IR248 3/14/98
        CONSTANT RejectFastPath     : IN    BOOLEAN             := FALSE        --IR250
     ) IS
 
        VARIABLE PropDelay : TIME;
  
    BEGIN
       -- Check if the new value to be scheduled is different than the
       -- previously scheduled value
        IF (GlitchData.SchedTime <= NOW) AND
           (GlitchData.SchedValue = OutTemp)
             THEN RETURN;
       -- Check if the new value to be Scheduled is the same as the
       -- previously scheduled output transactions.  If this condition
       -- exists and the new scheduled time is < the current GlitchData.
       -- schedTime then a fast path condition exists (IR250).  If the 
       -- modeler wants this condition rejected by setting the 
       -- RejectFastPath actual to true then exit out.  
        ELSIF (GlitchData.SchedValue=OutTemp)  AND (RejectFastPath)
             THEN RETURN;
        END IF;
     	
        -- Evaluate propagation delay paths
        PropDelay := VitalSelectPathDelay (OutTemp, GlitchData.LastValue,
                                           OutSignalName, Paths, DefaultDelay,
	                    				   IgnoreDefaultDelay);

        GlitchData.LastValue := OutTemp;
			

        -- Schedule the output transactions - including glitch handling
          VitalGlitch (OutSignal, GlitchData, OutSignalName, OutTemp,
                     PropDelay, Mode, XOn, NegPreemptOn, MsgOn, MsgSeverity );
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
        VARIABLE bias : TIME;
	VARIABLE actualObsTime : TIME;
        VARIABLE BC : TIME;
	VARIABLE Message:LINE;
    BEGIN
        -- Check SETUP constraint
      IF RefEdge THEN
          IF SetupEn THEN
            CheckInfo.ObsTime   := RefTime - TestTime;
            CheckInfo.State     := To_X01(TestSignal);
            CASE CheckInfo.State IS
              WHEN '0' => CheckInfo.ExpTime := SetupLow;
			  -- start of new code IR245-246
				BC := HoldHigh;
			  -- end of new code IR245-246
              WHEN '1' => CheckInfo.ExpTime := SetupHigh;
			  -- start of new code IR245-246
				BC := HoldLow;
			  -- end of new code IR245-246
              WHEN 'X' => CheckInfo.ExpTime := Maximum(SetupHigh,SetupLow);
			  -- start of new code IR245-246
				BC := Maximum(HoldHigh,HoldLow);
			  -- end of new code IR245-246
            END CASE;
			-- added the second condition for IR 245-246
			CheckInfo.Violation := ( (CheckInfo.ObsTime < CheckInfo.ExpTime) 
				AND ( NOT ((CheckInfo.ObsTime = BC) and (BC = 0 ns))) );
			-- start of new code IR245-246
			IF(CheckInfo.ExpTime = 0 ns) THEN
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
      ELSIF TestEvent THEN
          IF HoldEn THEN
            CheckInfo.ObsTime := TestTime - RefTime;
            CheckInfo.State := To_X01(TestSignal);
            CASE CheckInfo.State IS
              WHEN '0' => CheckInfo.ExpTime := HoldHigh;

			  -- new code for unnamed IR 
			     CheckInfo.State := '1';

			  -- start of new code IR245-246
				BC := SetupLow;
			  -- end of new code IR245-246
              WHEN '1' => CheckInfo.ExpTime := HoldLow;

			  -- new code for unnamed IR 
			     CheckInfo.State := '0';

			  -- start of new code IR245-246
				BC := SetupHigh;
			  -- end of new code IR245-246
              WHEN 'X' => CheckInfo.ExpTime := Maximum(HoldHigh,HoldLow);
			  -- start of new code IR245-246
				BC := Maximum(SetupHigh,SetupLow);
			  -- end of new code IR245-246
            END CASE;
			-- added the second condition for IR 245-246
			CheckInfo.Violation := ( (CheckInfo.ObsTime < CheckInfo.ExpTime) 
				AND ( NOT ((CheckInfo.ObsTime = BC) and (BC = 0 ns))) );

			-- start of new code IR245-246
			IF(CheckInfo.ExpTime = 0 ns) THEN
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
           IF ( CheckInfo.CheckKind = HoldCheck) then
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
          IF ( CheckInfo.CheckKind = SetupCheck) then
	    CheckInfo.CheckKind :=  HoldCheck;
	    CASE CheckInfo.State IS
	      WHEN '0' => CheckInfo.ExpTime := HoldHigh;
			  CheckInfo.State   := '1';
	      WHEN '1' => CheckInfo.ExpTime := HoldLow;
			  CheckInfo.State   := '0';
	      WHEN 'X' => CheckInfo.ExpTime := Maximum(HoldHigh,HoldLow);
	    END CASE;
          END IF;

	  CheckInfo.ObsTime := actualObsTime;
	  CheckInfo.ExpTime := CheckInfo.ExpTime - bias;
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
            CONSTANT MsgSeverity   : IN     SEVERITY_LEVEL := WARNING;
            CONSTANT EnableSetupOnTest : IN   BOOLEAN := TRUE;	--IR252 3/23/98
	        CONSTANT EnableSetupOnRef  : IN   BOOLEAN := TRUE;	--IR252 3/23/98
    	    CONSTANT EnableHoldOnRef   : IN   BOOLEAN := TRUE;  --IR252 3/23/98
	        CONSTANT EnableHoldOnTest  : IN   BOOLEAN := TRUE	--IR252 3/23/98
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
    	TimingData.SetupEn := TimingData.SetupEn AND EnableSetupOnRef;  --IR252 3/23/98
        TimingData.HoldEn  := EnableHoldOnRef;						    --IR252 3/23/98
      END IF;

      -- Detect test (data) changes and record the time of the last change
      TestEvent := TimingData.TestLast /= To_X01Z(TestSignal);
      TimingData.TestLast := To_X01Z(TestSignal);
      IF TestEvent THEN
        TimingData.TestTime := NOW;
        TimingData.SetupEn  := EnableSetupOnTest;						--IR252 3/23/98
    	TimingData.HoldEn   := TimingData.HoldEn AND EnableHoldOnTest;	--IR252 3/23/98
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
            CONSTANT MsgSeverity   : IN     SEVERITY_LEVEL := WARNING;
            CONSTANT EnableSetupOnTest : IN   BOOLEAN := TRUE;	--IR252 3/23/98
	        CONSTANT EnableSetupOnRef  : IN   BOOLEAN := TRUE;	--IR252 3/23/98
    	    CONSTANT EnableHoldOnRef   : IN   BOOLEAN := TRUE;  --IR252 3/23/98
	        CONSTANT EnableHoldOnTest  : IN   BOOLEAN := TRUE	--IR252 3/23/98

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
        TimingData.RefTime := NOW;
    	TimingData.SetupEn := TimingData.SetupEn AND EnableSetupOnRef;		--IR252 3/23/98
        TimingData.HoldEnA.all := (TestSignal'RANGE => EnableHoldOnRef);	--IR252 3/23/98
      END IF;
  
      -- Detect test (data) changes and record the time of the last change
      FOR i IN TestSignal'RANGE LOOP
        TestEvent(i) := TimingData.TestLastA(i) /= To_X01Z(TestSignal(i));
        TimingData.TestLastA(i) := To_X01Z(TestSignal(i));
        IF TestEvent(i) THEN 
          TimingData.TestTimeA(i) := NOW;
          TimingData.SetupEnA(i) := EnableSetupOnTest;				--IR252 3/23/98
  	      TimingData.HoldEnA(i) := TimingData.HoldEn AND EnableHoldOnTest;	--IR252 3/23/98
          TimingData.TestTime := NOW;	                                	--IR252 3/23/98	 
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
            CONSTANT MsgSeverity   : IN     SEVERITY_LEVEL := WARNING;
    	    CONSTANT EnableRecOnTest : IN   BOOLEAN := TRUE;	--IR252 3/23/98
	        CONSTANT EnableRecOnRef  : IN   BOOLEAN := TRUE;	--IR252 3/23/98
	        CONSTANT EnableRemOnRef  : IN   BOOLEAN := TRUE;	--IR252 3/23/98
	        CONSTANT EnableRemOnTest : IN   BOOLEAN := TRUE		--IR252 3/23/98
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
    	TimingData.SetupEn := TimingData.SetupEn AND EnableRecOnRef;	  --IR252 3/23/98
        TimingData.HoldEn  := EnableRemOnRef;							  --IR252 3/23/98
      END IF;
 
      -- Detect test (data) changes and record the time of the last change
      TestEvent := TimingData.TestLast /= To_X01Z(TestSignal);
      TimingData.TestLast := To_X01Z(TestSignal);
      IF TestEvent THEN
        TimingData.TestTime := NOW;
        TimingData.SetupEn  := EnableRecOnTest;							  --IR252 3/23/98
    	TimingData.HoldEn   := TimingData.HoldEn AND EnableRemOnTest;	  --IR252 3/23/98
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


 
    PROCEDURE ReportSkewViolation (
        CONSTANT Signal1Name    : IN STRING := "";
        CONSTANT Signal2Name    : IN STRING := "";
        CONSTANT ExpectedTime   : IN TIME;
        CONSTANT OccuranceTime  : IN TIME;
        CONSTANT HeaderMsg      : IN STRING;
        CONSTANT MsgSeverity    : IN SEVERITY_LEVEL := WARNING;
        CONSTANT SkewPhase      : IN SkewType;
        CONSTANT ViolationFlag 	: IN BOOLEAN := TRUE
    ) IS
        VARIABLE Message : LINE;
    BEGIN
        Write ( Message, HeaderMsg );
        IF (ViolationFlag /= TRUE) THEN
          Write ( Message, STRING'(" POSSIBLE") );
        END IF;
        IF (SkewPhase = Inphase) THEN
          Write ( Message, STRING'(" IN PHASE ")      ); 
        ELSE
          Write ( Message, STRING'(" OUT OF PHASE ")      );
        END IF;
        Write ( Message, STRING'("SKEW VIOLATION ON ")      );
        Write ( Message, Signal2Name );
        IF (Signal1Name'LENGTH > 0) THEN
            Write ( Message, STRING'(" WITH RESPECT TO ") );
            Write ( Message, Signal1Name );
        END IF;
        Write ( Message, ';' & LF );
        Write ( Message, STRING'(" At : ")         );
        Write ( Message, OccuranceTime);
        Write ( Message, STRING'("; Skew Limit : ")  );
        Write ( Message, ExpectedTime);
 
        ASSERT FALSE REPORT Message.ALL SEVERITY MsgSeverity;
 
        DEALLOCATE (Message);
    END ReportSkewViolation;
 
 
    PROCEDURE VitalInPhaseSkewCheck (
        VARIABLE Violation         : OUT    X01;
        VARIABLE SkewData          : INOUT  VitalSkewDataType;
        SIGNAL   Signal1           : IN     std_ulogic;
        CONSTANT Signal1Name       : IN     STRING := "";
        CONSTANT Signal1Delay      : IN     TIME := 0 ns;
        SIGNAL   Signal2           : IN     std_ulogic;
        CONSTANT Signal2Name       : IN     STRING := "";
        CONSTANT Signal2Delay      : IN     TIME := 0 ns;
        CONSTANT SkewS1S2RiseRise  : IN     TIME := TIME'HIGH;
        CONSTANT SkewS2S1RiseRise  : IN     TIME := TIME'HIGH;
        CONSTANT SkewS1S2FallFall  : IN     TIME := TIME'HIGH;
        CONSTANT SkewS2S1FallFall  : IN     TIME := TIME'HIGH;
        CONSTANT CheckEnabled      : IN     BOOLEAN := TRUE;
        CONSTANT XOn               : IN     BOOLEAN := TRUE;
        CONSTANT MsgOn             : IN     BOOLEAN := TRUE;
        CONSTANT MsgSeverity       : IN     SEVERITY_LEVEL := WARNING;
        CONSTANT HeaderMsg         : IN     STRING  := "";
        SIGNAL   Trigger           : INOUT  std_ulogic
    ) IS
        VARIABLE ReportType  : VitalSkewExpectedType := none;
        VARIABLE ExpectedType  : VitalSkewExpectedType := none;
        VARIABLE ReportTime  : TIME;
        VARIABLE TriggerDelay  : TIME;
        VARIABLE ViolationCertain  : Boolean := TRUE;
    BEGIN
        Violation := '0';
        ReportType := none;
        TriggerDelay := noTrigger;

        IF (CheckEnabled) THEN
            IF (SkewData.ExpectedType /= none) THEN
                IF (trigger'Event) THEN
                    CASE SkewData.ExpectedType IS
                    WHEN s1r => ReportType := s1r;
                                ReportTime := NOW - Signal1Delay;
                    WHEN s1f => ReportType := s1f;
                                ReportTime := NOW - Signal1Delay;
                    WHEN s2r => ReportType := s2r;
                                ReportTime := NOW - Signal2Delay;
                    WHEN s2f => ReportType := s2f;
                                ReportTime := NOW - Signal2Delay;
                    WHEN OTHERS =>
                    END CASE;
                    SkewData.ExpectedType := none;
                ELSIF ( Signal1'Event OR Signal2'Event ) THEN
                    IF ( Signal1 /= 'X' AND Signal2 /= 'X' ) THEN
                        TriggerDelay := 0 ns;
                        ExpectedType := none;
                    END IF;
                END IF;
            END IF;
    
            IF (Signal1'EVENT and Signal2'EVENT) THEN
                IF (Signal1 = Signal2) THEN
                    IF (Posedge(Signal1'LAST_VALUE, Signal1)) THEN
                        IF ((Signal1Delay - Signal2Delay) >= 
                                SkewS1S2RiseRise) THEN
                            ReportType := s2r;
                            ReportTime := NOW - Signal1Delay +
                                          SkewS1S2RiseRise;
                        ELSIF ((Signal2Delay -Signal1Delay) >= 
                                SkewS2S1RiseRise) THEN
                            ReportType := s1r;
                            ReportTime := NOW - Signal2Delay +
                                          SkewS2S1RiseRise;
                        END IF;
                    ELSIF (Negedge(Signal1'LAST_VALUE, Signal1)) THEN
                        IF ((Signal1Delay - Signal2Delay) >= 
                                SkewS1S2FallFall) THEN 
                            ReportType := s2f;
                            ReportTime := NOW - Signal1Delay +
                                          SkewS1S2FallFall;
                        ELSIF ((Signal2Delay - Signal1Delay) >= 
                                SkewS2S1FallFall) THEN  
                            ReportType := s1f;
                            ReportTime := NOW - Signal2Delay +
                                          SkewS2S1FallFall;
                        END IF;
                    END IF; 
                ELSIF (Posedge(Signal1'LAST_VALUE , Signal1)) THEN
                    IF ((Signal1Delay >= Signal2Delay) and (Signal2Delay > 
                            SkewS2S1FallFall)) THEN 
                        ReportType := s1f;
                        ReportTime := NOW - Signal2Delay + 
                                               SkewS2S1FallFall;
                    ELSIF ((Signal2Delay >= Signal1Delay) and (Signal1Delay > 
                            SkewS1S2RiseRise)) THEN
                        ReportType := s2r;
                        ReportTime := NOW - Signal1Delay + 
                                               SkewS1S2RiseRise;
                    ELSIF (Signal2Delay > Signal1Delay) THEN
                        SkewData.ExpectedType := s2r;
                        TriggerDelay := SkewS1S2RiseRise + 
                                                 Signal2Delay - Signal1Delay;
                    ELSIF (Signal1Delay > Signal2Delay) THEN 
                        SkewData.ExpectedType := s1r;    
                        TriggerDelay := SkewS2S1RiseRise + 
                                                 Signal1Delay - Signal2Delay;
                    ELSIF (SkewS1S2RiseRise < SkewS2S1RiseRise) THEN
                        SkewData.ExpectedType := s2r;
                        TriggerDelay := SkewS1S2RiseRise;
                    ELSE
                        SkewData.ExpectedType := s1r;
                        TriggerDelay := SkewS2S1RiseRise;
                    END IF;
                ELSIF (Negedge(Signal1'LAST_VALUE , Signal1)) THEN 
                    IF ((Signal1Delay >= Signal2Delay) and (Signal2Delay > 
                            SkewS2S1RiseRise)) THEN  
                        ReportType := s1r;
                        ReportTime := NOW - Signal2Delay + 
                                               SkewS2S1RiseRise;
                    ELSIF ((Signal2Delay >= Signal1Delay) and (Signal1Delay > 
                            SkewS1S2FallFall)) THEN 
                        ReportType := s2f;
                        ReportTime := NOW - Signal1Delay + 
                                               SkewS1S2FallFall;
                    ELSIF (Signal2Delay > Signal1Delay) THEN
                        SkewData.ExpectedType := s2f;
                        TriggerDelay := SkewS1S2FallFall + 
                                                 Signal2Delay - Signal1Delay;
                    ELSIF (Signal1Delay > Signal2Delay) THEN
                        SkewData.ExpectedType := s1f;    
                        TriggerDelay := SkewS2S1FallFall + 
                                                 Signal1Delay - Signal2Delay;
                    ELSIF (SkewS1S2FallFall < SkewS2S1FallFall) THEN
                        SkewData.ExpectedType := s2f;
                        TriggerDelay := SkewS1S2FallFall;
                    ELSE
                        SkewData.ExpectedType := s1f;    
                        TriggerDelay := SkewS2S1FallFall;
                    END IF;
                END IF;   
            ELSIF (Signal1'EVENT) THEN
                IF ( Signal1 /= Signal2) THEN
                    IF ( Posedge( Signal1'LAST_VALUE,  Signal1)) THEN
                        IF (SkewS1S2RiseRise > (Signal1Delay - 
                                Signal2Delay)) THEN
                            SkewData.ExpectedType := s2r;
                            TriggerDelay := SkewS1S2RiseRise + 
                                                     Signal2Delay - 
                                                     Signal1Delay;
                        ELSE
                            ReportType := s2r;
                            ReportTime := NOW + SkewS1S2RiseRise - 
                                                   Signal1Delay;
                        END IF; 
                    ELSIF ( Negedge( Signal1'LAST_VALUE,  Signal1)) THEN
                        IF (SkewS1S2FallFall > (Signal1Delay - 
                                Signal2Delay)) THEN    
                            SkewData.ExpectedType := s2f; 
                            TriggerDelay := SkewS1S2FallFall + 
                                                     Signal2Delay - 
                                                     Signal1Delay;
                        ELSE     
                            ReportType := s2f;
                            ReportTime := NOW + SkewS1S2FallFall - 
                                                   Signal1Delay;
                        END IF; 
                    END IF;
                ELSE 
                    IF ( Posedge( Signal1'LAST_VALUE,  Signal1)) THEN
                        IF ((Signal1Delay - SkewS1S2RiseRise) > 
                                (Signal2'LAST_EVENT + Signal2Delay)) THEN
                            IF ((SkewData.Signal2Old2 - Signal2Delay) >
                                    (NOW - Signal1Delay + 
                                    SkewS1S2RiseRise)) THEN
                                ViolationCertain := FALSE;
                                ReportType := s2r;
                                ReportTime := NOW + SkewS1S2RiseRise - 
                                                   Signal1Delay;
                            END IF;
                        END IF;
                    ELSIF ( Negedge( Signal1'LAST_VALUE,  Signal1)) THEN
                        IF ((Signal1Delay - SkewS1S2FallFall) > 
                                (Signal2'LAST_EVENT + Signal2Delay)) THEN
                            IF (( SkewData.Signal2Old2 - Signal2Delay) >
                                    (NOW - Signal1Delay + 
                                    SkewS1S2FallFall )) THEN
                                ViolationCertain := FALSE;
                                ReportType := s2f;
                                ReportTime := NOW + SkewS1S2FallFall - 
                                                   Signal1Delay;
                            END IF;
                        END IF;  
                    END IF;  
                END IF;
            ELSIF (Signal2'EVENT) THEN
                IF (Signal1 /= Signal2) THEN
                    IF (Posedge(Signal2'LAST_VALUE,Signal2)) THEN
                        IF ( SkewS2S1RiseRise > (Signal2Delay - 
                                Signal1Delay)) THEN
                            SkewData.ExpectedType := s1r;
                            TriggerDelay := SkewS2S1RiseRise + 
                                                     Signal1Delay - 
                                                     Signal2Delay;
                        ELSE
                            ReportType := s2r;
                            ReportTime := NOW + SkewS2S1RiseRise - 
                                                   Signal2Delay;
                        END IF;
                    ELSIF (Negedge(Signal2'LAST_VALUE,Signal2)) THEN
                        IF ( SkewS2S1FallFall > (Signal2Delay - 
                                Signal1Delay)) THEN
                            SkewData.ExpectedType := s1f; 
                            TriggerDelay := SkewS2S1FallFall + 
                                                     Signal1Delay - 
                                                     Signal2Delay;
                        ELSE  
                            ReportType := s1f;
                            ReportTime := NOW + SkewS2S1FallFall - 
                                                   Signal2Delay;
                        END IF;
                    END IF; 
                ELSE 
                    IF (Posedge(Signal2'LAST_VALUE, Signal2)) THEN
                        IF ((Signal2Delay - SkewS2S1RiseRise) > 
                                (Signal1'LAST_EVENT + Signal1Delay)) THEN
                            IF (( SkewData.Signal1Old2 - Signal1Delay) >
                                    (NOW - Signal2Delay +
                                    SkewS2S1RiseRise )) THEN
                                ViolationCertain := FALSE;
                                ReportType := s1r;
                                ReportTime := NOW + SkewS2S1RiseRise - 
                                                   Signal2Delay;
                            END IF;
                        END IF;
                    ELSIF (Negedge(Signal2'LAST_VALUE, Signal2)) THEN
                        IF ((Signal2Delay - SkewS2S1FallFall) > 
                                (Signal1'LAST_EVENT + Signal1Delay)) THEN
                            IF (( SkewData.Signal1Old2 - Signal1Delay) >
                                    (NOW - Signal2Delay +
                                    SkewS2S1FallFall )) THEN
                                ViolationCertain := FALSE;
                                ReportType := s1f;
                                ReportTime := NOW + SkewS2S1FallFall - 
                                                   Signal2Delay;
                            END IF;
                        END IF;
                    END IF; 
                END IF; 
            END IF;        

            IF (ReportType /= none) THEN
                IF (MsgOn) THEN
                    CASE ReportType IS
                    WHEN s1r => 
                        ReportSkewViolation(
                            Signal2Name,
                            Signal1Name,
                            SkewS2S1RiseRise,
                            ReportTime,
                            HeaderMsg,
                            MsgSeverity,
                            Inphase,
                            ViolationCertain);
                    WHEN s1f =>
                        ReportSkewViolation(
                            Signal2Name,
                            Signal1Name,
                            SkewS2S1FallFall,
                            ReportTime,
                            HeaderMsg,
                            MsgSeverity,
                            Inphase,
                            ViolationCertain);
                    WHEN s2r =>
                        ReportSkewViolation(
                            Signal1Name,
                            Signal2Name,
                            SkewS1S2RiseRise,
                            ReportTime,
                            HeaderMsg,
                            MsgSeverity,
                            Inphase,
                            ViolationCertain);
                    WHEN s2f =>
                        ReportSkewViolation(
                            Signal1Name,
                            Signal2Name,
                            SkewS1S2FallFall,
                            ReportTime,
                            HeaderMsg,
                            MsgSeverity,
                            Inphase,
                            ViolationCertain);
                    WHEN OTHERS =>
                    END CASE;
                END IF;
                IF (XOn) THEN
                    Violation := 'X';
                END IF;
                SkewData.ExpectedType := none;
            END IF;        
            IF (TriggerDelay /= noTrigger) THEN
                IF (TriggerDelay = 0 ns) THEN
                    trigger <= TRANSPORT trigger AFTER 0 ns;
                ELSE
                    trigger <= TRANSPORT not (trigger) AFTER 
                               TriggerDelay;
                END IF;
            END IF;
        END IF;
        IF (Signal1'EVENT and SkewData.Signal1Old1 /= NOW) THEN
            SkewData.Signal1Old2 := SkewData.Signal1Old1;
            SkewData.Signal1Old1 := NOW;
        END IF;
        IF (Signal2'EVENT and SkewData.Signal2Old1 /= NOW) THEN
            SkewData.Signal2Old2 := SkewData.Signal2Old1;
            SkewData.Signal2Old1 := NOW;
        END IF;
    END VitalInPhaseSkewCheck;

    PROCEDURE VitalOutPhaseSkewCheck (
        VARIABLE Violation         : OUT    X01;
        VARIABLE SkewData          : INOUT  VitalSkewDataType;
        SIGNAL   Signal1           : IN     std_ulogic;
        CONSTANT Signal1Name       : IN     STRING := "";
        CONSTANT Signal1Delay      : IN     TIME := 0 ns;
        SIGNAL   Signal2           : IN     std_ulogic;
        CONSTANT Signal2Name       : IN     STRING := "";
        CONSTANT Signal2Delay      : IN     TIME := 0 ns;
        CONSTANT SkewS1S2RiseFall  : IN     TIME := TIME'HIGH;
        CONSTANT SkewS2S1RiseFall  : IN     TIME := TIME'HIGH;
        CONSTANT SkewS1S2FallRise  : IN     TIME := TIME'HIGH;
        CONSTANT SkewS2S1FallRise  : IN     TIME := TIME'HIGH;
        CONSTANT CheckEnabled      : IN     BOOLEAN := TRUE;
        CONSTANT XOn               : IN     BOOLEAN := TRUE;
        CONSTANT MsgOn             : IN     BOOLEAN := TRUE;
        CONSTANT MsgSeverity       : IN     SEVERITY_LEVEL := WARNING;
        CONSTANT HeaderMsg         : IN     STRING  := "";
        SIGNAL   Trigger           : INOUT  std_ulogic
    ) IS
        VARIABLE ReportType  : VitalSkewExpectedType := none;
        VARIABLE ExpectedType  : VitalSkewExpectedType := none;
        VARIABLE ReportTime  : TIME;
        VARIABLE TriggerDelay  : TIME;
        VARIABLE ViolationCertain  : Boolean := TRUE;
    BEGIN
        Violation := '0';
        TriggerDelay := noTrigger;
        IF (CheckEnabled) THEN
            IF (SkewData.ExpectedType /= none) THEN
                IF (trigger'Event) THEN
                    CASE SkewData.ExpectedType IS 
                    WHEN s1r => ReportType := s1r;
                                ReportTime := NOW - Signal1Delay;
                    WHEN s1f => ReportType := s1f;
                                ReportTime := NOW - Signal1Delay;
                    WHEN s2r => ReportType := s2r;
                                ReportTime := NOW - Signal2Delay;
                    WHEN s2f => ReportType := s2f;
                                ReportTime := NOW - Signal2Delay;
                    WHEN OTHERS =>
                    END CASE;
                    SkewData.ExpectedType := none;
                ELSIF (Signal1'Event OR Signal2'Event ) THEN
                    IF (Signal1 /= 'X' AND Signal2 /= 'X' ) THEN
                        TriggerDelay := 0 ns;
                        SkewData.ExpectedType := none;
                    END IF;
                END IF;
            END IF; 
           
            IF (Signal1'EVENT and Signal2'EVENT) THEN
                IF (Signal1 /= Signal2) THEN
                    IF (Posedge(Signal1'LAST_VALUE, Signal1)) THEN
                        IF ((Signal1Delay - Signal2Delay) >= 
                                        SkewS1S2RiseFall) THEN
                            ReportType := s2f;
                            ReportTime := NOW - Signal1Delay +
                                          SkewS1S2RiseFall;
                        ELSIF ((Signal2Delay - Signal1Delay) >=
                                        SkewS2S1FallRise) THEN
                            ReportType := s1r;
                            ReportTime := NOW - Signal2Delay +
                                          SkewS2S1FallRise;
                        END IF;
                    ELSIF (Negedge(Signal1'LAST_VALUE, Signal1)) THEN
                        IF ((Signal1Delay - Signal2Delay) >= 
                                SkewS1S2FallRise) THEN 
                            ReportType := s2r;
                            ReportTime := NOW - Signal1Delay +
                                          SkewS1S2FallRise;
                        ELSIF ((Signal2Delay - Signal1Delay) >= 
                                SkewS2S1RiseFall) THEN  
                            ReportType := s1f;
                            ReportTime := NOW - Signal2Delay +
                                          SkewS2S1RiseFall;
                        END IF;
                    END IF; 
                ELSIF (Posedge(Signal1'LAST_VALUE, Signal1)) THEN
                    IF ((Signal1Delay >= Signal2Delay) and (Signal2Delay >
                              SkewS2S1RiseFall)) THEN 
                        ReportType := s1f;
                        ReportTime := NOW - Signal2Delay +
                                      SkewS2S1RiseFall;
                    ELSIF ((Signal2Delay >= Signal1Delay) and (Signal1Delay >
                                      SkewS1S2RiseFall)) THEN
                        ReportType := s2f;
                        ReportTime := NOW - Signal1Delay +
                                      SkewS1S2RiseFall;
                    ELSIF (Signal1Delay > Signal2Delay) THEN
                        SkewData.ExpectedType := s1f;
                        TriggerDelay := SkewS2S1RiseFall +
                                        Signal1Delay - Signal2Delay;
                    ELSIF (Signal2Delay > Signal1Delay) THEN 
                        SkewData.ExpectedType := s2f;
                        TriggerDelay := SkewS1S2RiseFall + 
                                        Signal2Delay - Signal1Delay;
                    ELSIF (SkewS2S1RiseFall < SkewS1S2RiseFall) THEN
                        SkewData.ExpectedType := s1f;   
                        TriggerDelay := SkewS2S1RiseFall;
                    ELSE
                        SkewData.ExpectedType := s2f;   
                        TriggerDelay := SkewS1S2RiseFall;
                    END IF; 
                ELSIF (Negedge(Signal1'LAST_VALUE, Signal1)) THEN
                    IF ((Signal1Delay >= Signal2Delay) and (Signal2Delay > 
                                                    SkewS2S1FallRise)) THEN
                        ReportType := s1r;
                        ReportTime := NOW - Signal2Delay +
                                      SkewS2S1FallRise;
                    ELSIF ((Signal2Delay >= Signal1Delay) and (Signal1Delay >
                                                SkewS1S2FallRise)) THEN
                        ReportType := s2r;
                        ReportTime := NOW - Signal1Delay +
                                      SkewS1S2FallRise;
                    ELSIF (Signal1Delay > Signal2Delay) THEN
                        SkewData.ExpectedType := s1r;     
                        TriggerDelay := SkewS2S1FallRise + 
                                        Signal1Delay - Signal2Delay;
                    ELSIF (Signal2Delay > Signal1Delay) THEN
                        SkewData.ExpectedType := s2r;
                        TriggerDelay := SkewS1S2FallRise + 
                                                Signal2Delay - Signal1Delay;
                    ELSIF (SkewS2S1FallRise < SkewS1S2FallRise) THEN
                        SkewData.ExpectedType := s1r;    
                        TriggerDelay := SkewS2S1FallRise;
                    ELSE
                        SkewData.ExpectedType := s2r;
                        TriggerDelay := SkewS1S2FallRise;
                    END IF;
                END IF;
            ELSIF (Signal1'EVENT) THEN
                IF (Signal1 = Signal2) THEN
                    IF (Posedge(Signal1'LAST_VALUE,Signal1)) THEN
                        IF (SkewS1S2RiseFall > (Signal1Delay - 
                                                   Signal2Delay)) THEN
                            SkewData.ExpectedType := s2f;
                            TriggerDelay := SkewS1S2RiseFall +
                                            Signal2Delay - Signal1Delay;
                        ELSE
                            ReportType := s2f;
                            ReportTime := NOW - Signal1Delay +
                                          SkewS1S2RiseFall;
                        END IF;
                    ELSIF ( Negedge(Signal1'LAST_VALUE, Signal1)) THEN
                        IF ( SkewS1S2FallRise > (Signal1Delay - 
                                                   Signal2Delay)) THEN
                            SkewData.ExpectedType := s2r;
                            TriggerDelay := SkewS1S2FallRise +
                                            Signal2Delay - Signal1Delay;
                        ELSE
                            ReportType := s2r;
                            ReportTime := NOW - Signal1Delay +
                                          SkewS1S2FallRise; 
                        END IF;
                    END IF;
                ELSE
                    IF (Posedge( Signal1'LAST_VALUE, Signal1 )) THEN 
                        IF ((Signal1Delay - SkewS1S2RiseFall) >
                            (Signal2'LAST_EVENT + Signal2Delay)) THEN
                            IF (( SkewData.Signal2Old2 - Signal2Delay) >
                                    (NOW - Signal1Delay + 
                                    SkewS1S2RiseFall )) THEN
                                ViolationCertain := FALSE;
                                ReportType := s2f;
                                ReportTime := NOW + SkewS1S2RiseFall - 
                                                   Signal1Delay;
                            END IF;
                        END IF;
                    ELSIF (Negedge(Signal1'LAST_VALUE, Signal1)) THEN
                        IF ((Signal1Delay - SkewS1S2FallRise) > 
                            (Signal2'LAST_EVENT + Signal2Delay)) THEN
                            IF (( SkewData.Signal2Old2 - Signal2Delay) >
                                    (NOW - Signal1Delay + 
                                    SkewS1S2FallRise )) THEN
                                ViolationCertain := FALSE;
                                ReportType := s2r;
                                ReportTime := NOW + SkewS1S2FallRise - 
                                                   Signal1Delay;
                            END IF;
                        END IF;
                    END IF;
                END IF;
            ELSIF (Signal2'EVENT) THEN
                IF (Signal1 = Signal2) THEN
                    IF (Posedge(Signal2'LAST_VALUE,Signal2)) THEN
                        IF (SkewS2S1RiseFall > (Signal2Delay - 
                                                   Signal1Delay)) THEN
                            SkewData.ExpectedType := s1f;
                            TriggerDelay := SkewS2S1RiseFall + Signal1Delay -
                                                  Signal2Delay ;
                        ELSE
                            ReportType := s1f;
                            ReportTime := NOW + SkewS2S1RiseFall -
                                                Signal2Delay;
                        END IF;
                    ELSIF (Negedge(Signal2'LAST_VALUE,Signal2)) THEN
                        IF (SkewS2S1FallRise > (Signal2Delay - 
                                                  Signal1Delay)) THEN
                            SkewData.ExpectedType := s1r;
                            TriggerDelay := SkewS2S1FallRise + Signal1Delay -
                                                  Signal2Delay;
                        ELSE 
                            ReportType := s1r;
                            ReportTime := NOW + SkewS2S1FallRise -
                                                Signal2Delay;
                        END IF;
                    END IF; 
                ELSE
                    IF (Posedge(Signal2'LAST_VALUE,Signal2)) THEN
                        IF ((Signal2Delay - SkewS2S1RiseFall) >
                              (Signal1'LAST_EVENT + Signal1Delay)) THEN
                            IF (( SkewData.Signal1Old2 - Signal1Delay) >
                                    (NOW - Signal2Delay +
                                    SkewS2S1RiseFall )) THEN
                                ViolationCertain := FALSE;
                                ReportType := s1f;
                                ReportTime := NOW + SkewS2S1RiseFall - 
                                                   Signal2Delay;
                            END IF;
                        END IF;
                    ELSIF (Negedge(Signal2'LAST_VALUE,Signal2)) THEN
                        IF ((Signal2Delay - SkewS2S1FallRise) >
                               (Signal1'LAST_EVENT + Signal1Delay)) THEN
                            IF (( SkewData.Signal1Old2 - Signal1Delay) >
                                    (NOW - Signal2Delay +
                                    SkewS2S1FallRise )) THEN
                                ViolationCertain := FALSE;
                                ReportType := s1r;
                                ReportTime := NOW + SkewS2S1FallRise - 
                                                   Signal2Delay;
                            END IF;
                        END IF;
                    END IF;
                END IF;
            END IF;    
                  
            IF (ReportType /= none) THEN
                IF (MsgOn) THEN
                    CASE ReportType IS
                    WHEN s1r =>
                        ReportSkewViolation(
                            Signal2Name,
                            Signal1Name,
                            SkewS2S1FallRise, 
                            ReportTime,
                            HeaderMsg,
                            MsgSeverity,
                            Outphase,
                            ViolationCertain);
                    WHEN s1f =>
                        ReportSkewViolation(
                            Signal2Name,
                            Signal1Name,
                            SkewS2S1RiseFall,
                            ReportTime,
                            HeaderMsg,
                            MsgSeverity,
                            Outphase,
                            ViolationCertain);
                    WHEN s2r =>
                        ReportSkewViolation(
                            Signal1Name,
                            Signal2Name,
                            SkewS1S2FallRise,
                            ReportTime,
                            HeaderMsg,
                            MsgSeverity,
                            Outphase,
                            ViolationCertain);
                    WHEN s2f =>
                        ReportSkewViolation(
                            Signal1Name,
                            Signal2Name,
                            SkewS1S2RiseFall,
                            ReportTime,
                            HeaderMsg,
                            MsgSeverity,
                            Outphase,
                            ViolationCertain);
                    WHEN OTHERS =>
                    END CASE;
                END IF;
                IF (XOn) THEN
                    Violation := 'X';
                END IF;
                ReportType := none;
            END IF;
            IF (TriggerDelay /= noTrigger) THEN
                IF (TriggerDelay = 0 ns) THEN
                    trigger <= TRANSPORT trigger AFTER 0 ns;
                ELSE
                    trigger <= TRANSPORT not (trigger) AFTER
                               TriggerDelay;
                END IF;
            END IF;
        END IF;
        IF (Signal1'EVENT and SkewData.Signal1Old1 /= NOW) THEN
            SkewData.Signal1Old2 := SkewData.Signal1Old1;
            SkewData.Signal1Old1 := NOW;
        END IF;
        IF (Signal2'EVENT and SkewData.Signal2Old1 /= NOW) THEN
            SkewData.Signal2Old2 := SkewData.Signal2Old1;
            SkewData.Signal2Old1 := NOW;
        END IF;
    END VitalOutPhaseSkewCheck;

END VITAL_Timing;
