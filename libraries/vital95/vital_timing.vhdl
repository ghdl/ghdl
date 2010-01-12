-------------------------------------------------------------------------------
-- Title        : Standard VITAL TIMING Package
--              : $Revision$
--              :
-- Library      : This package shall be compiled into a library
--              : symbolically named IEEE.
--              :
-- Developers   : IEEE DASC Timing Working Group (TWG), PAR 1076.4
--              :
-- Purpose      : This packages defines standard types, attributes, constants,
--              : functions and procedures for use in developing ASIC models.
--              :
-- Known Errors : 
--              :
-- Note         : No declarations or definitions shall be included in,
--              : or excluded from this package. The "package declaration"
--              : defines the objects (types, subtypes, constants, functions,
--              : procedures ... etc.) that can be used by a user.  The package
--              : body shall be considered the formal definition of the
--              : semantics of this package.  Tool developers may choose to
--              : implement the package body in the most efficient manner
--              : available to them.
-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------
-- Acknowledgments:
--   This code was originally developed under the "VHDL Initiative Toward ASIC
--   Libraries" (VITAL), an industry sponsored initiative.  Technical
--   Director: William Billowitch, VHDL Technology Group; U.S. Coordinator:
--   Steve Schultz;  Steering Committee Members: Victor Berman, Cadence Design
--   Systems; Oz Levia, Synopsys Inc.; Ray Ryan, Ryan & Ryan; Herman van Beek,
--   Texas Instruments; Victor Martin, Hewlett-Packard Company.
-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------
-- Modification History :
-- ----------------------------------------------------------------------------
-- Version No:|Auth:| Mod.Date:| Changes Made:
--   v95.0 A  |     | 06/02/95 | Initial ballot draft 1995
--   v95.1    |     | 08/31/95 | #203 - Timing violations at time 0
--                               #204 - Output mapping prior to glitch detection 
-- ---------------------------------------------------------------------------- 
LIBRARY IEEE;
USE     IEEE.Std_Logic_1164.ALL;

PACKAGE VITAL_Timing IS
    TYPE VitalTransitionType IS ( tr01, tr10, tr0z, trz1, tr1z, trz0,
                                  tr0X, trx1, tr1x, trx0, trxz, trzx);

    SUBTYPE VitalDelayType     IS TIME;
    TYPE VitalDelayType01   IS ARRAY (VitalTransitionType   RANGE tr01 to tr10)
         OF TIME;
    TYPE VitalDelayType01Z  IS ARRAY (VitalTransitionType   RANGE tr01 to trz0)
         OF TIME;
    TYPE VitalDelayType01ZX IS ARRAY (VitalTransitionType   RANGE tr01 to trzx)
         OF TIME;

    TYPE VitalDelayArrayType     IS ARRAY (NATURAL RANGE <>) OF VitalDelayType;
    TYPE VitalDelayArrayType01   IS ARRAY (NATURAL RANGE <>) OF VitalDelayType01;
    TYPE VitalDelayArrayType01Z  IS ARRAY (NATURAL RANGE <>) OF VitalDelayType01Z;
    TYPE VitalDelayArrayType01ZX IS ARRAY (NATURAL RANGE <>) OF VitalDelayType01ZX;
    -- ----------------------------------------------------------------------
    -- **********************************************************************
    -- ----------------------------------------------------------------------

    CONSTANT VitalZeroDelay     : VitalDelayType     :=   0 ns;
    CONSTANT VitalZeroDelay01   : VitalDelayType01   := ( 0 ns, 0 ns );
    CONSTANT VitalZeroDelay01Z  : VitalDelayType01Z  := ( OTHERS => 0 ns );
    CONSTANT VitalZeroDelay01ZX : VitalDelayType01ZX := ( OTHERS => 0 ns );

    ---------------------------------------------------------------------------
    -- examples of usage:
    ---------------------------------------------------------------------------
    -- tpd_CLK_Q : VitalDelayType  := 5 ns;
    -- tpd_CLK_Q : VitalDelayType01  := (tr01 => 2 ns, tr10 => 3 ns);
    -- tpd_CLK_Q : VitalDelayType01Z := ( 1 ns, 2 ns, 3 ns, 4 ns, 5 ns, 6 ns );
    -- tpd_CLK_Q : VitalDelayArrayType(0 to 1)
    --                          := (0 => 5 ns, 1 => 6 ns);
    -- tpd_CLK_Q : VitalDelayArrayType01(0 to 1)
    --                          := (0 => (tr01 => 2 ns, tr10 => 3 ns),
    --                              1 => (tr01 => 2 ns, tr10 => 3 ns));
    -- tpd_CLK_Q : VitalDelayArrayType01Z(0 to 1)
    --                        := (0 => ( 1 ns, 2 ns, 3 ns, 4 ns, 5 ns, 6 ns ),
    --                            1 => ( 1 ns, 2 ns, 3 ns, 4 ns, 5 ns, 6 ns ));
    ---------------------------------------------------------------------------

    -- TRUE if the model is LEVEL0 | LEVEL1 compliant
    ATTRIBUTE VITAL_Level0 : BOOLEAN;
    ATTRIBUTE VITAL_Level1 : BOOLEAN;

    SUBTYPE std_logic_vector2 IS std_logic_vector(1 DOWNTO 0);
    SUBTYPE std_logic_vector3 IS std_logic_vector(2 DOWNTO 0);
    SUBTYPE std_logic_vector4 IS std_logic_vector(3 DOWNTO 0);
    SUBTYPE std_logic_vector8 IS std_logic_vector(7 DOWNTO 0);

    -- Types for strength mapping of outputs
    TYPE VitalOutputMapType  IS ARRAY ( std_ulogic ) OF std_ulogic;
    TYPE VitalResultMapType  IS ARRAY ( UX01       ) OF std_ulogic;
    TYPE VitalResultZMapType IS ARRAY ( UX01Z      ) OF std_ulogic;
    CONSTANT VitalDefaultOutputMap  : VitalOutputMapType
                                    := "UX01ZWLH-";
    CONSTANT VitalDefaultResultMap  : VitalResultMapType
                                    := ( 'U', 'X', '0', '1' );
    CONSTANT VitalDefaultResultZMap : VitalResultZMapType
                                    := ( 'U', 'X', '0', '1', 'Z' );

    -- Types for fields of VitalTimingDataType
    TYPE VitalTimeArrayT  IS ARRAY (INTEGER RANGE <>) OF TIME;
    TYPE VitalTimeArrayPT IS ACCESS VitalTimeArrayT;
    TYPE VitalBoolArrayT  IS ARRAY (INTEGER RANGE <>) OF BOOLEAN;
    TYPE VitalBoolArrayPT IS ACCESS VitalBoolArrayT;
    TYPE VitalLogicArrayPT IS ACCESS std_logic_vector;

    TYPE VitalTimingDataType IS RECORD
        NotFirstFlag : BOOLEAN;
        RefLast   : X01;
        RefTime   : TIME;
        HoldEn    : BOOLEAN;
        TestLast  : std_ulogic;
        TestTime  : TIME;
        SetupEn   : BOOLEAN;
        TestLastA : VitalLogicArrayPT;
        TestTimeA : VitalTimeArrayPT;
        HoldEnA   : VitalBoolArrayPT;
        SetupEnA  : VitalBoolArrayPT;
    END RECORD;

    FUNCTION VitalTimingDataInit RETURN VitalTimingDataType;

    -- type for internal data of VitalPeriodPulseCheck
    TYPE VitalPeriodDataType IS RECORD
        Last : X01;
        Rise : TIME;
        Fall : TIME;
        NotFirstFlag : BOOLEAN;
    END RECORD;
    CONSTANT VitalPeriodDataInit : VitalPeriodDataType 
                                 := ('X', 0 ns, 0 ns, FALSE );

    -- Type for specifying the kind of Glitch handling to use
    TYPE VitalGlitchKindType IS (OnEvent,
                                 OnDetect,
                                 VitalInertial,
                                 VitalTransport);

    TYPE VitalGlitchDataType IS
      RECORD
        SchedTime    : TIME;
        GlitchTime   : TIME;
        SchedValue   : std_ulogic;
        LastValue    : std_ulogic;
      END RECORD;
    TYPE VitalGlitchDataArrayType IS ARRAY (NATURAL RANGE <>)
         OF VitalGlitchDataType;

    -- PathTypes: for handling simple PathDelay info
    TYPE VitalPathType IS RECORD
        InputChangeTime : TIME;             -- timestamp for path input signal
        PathDelay       : VitalDelayType;   -- delay for this path
        PathCondition   : BOOLEAN;          -- path sensitize condition
    END RECORD;
    TYPE VitalPath01Type IS RECORD
        InputChangeTime : TIME;             -- timestamp for path input signal
        PathDelay       : VitalDelayType01; -- delay for this path
        PathCondition   : BOOLEAN;          -- path sensitize condition
    END RECORD;
    TYPE VitalPath01ZType IS RECORD
        InputChangeTime : TIME;             -- timestamp for path input signal
        PathDelay       : VitalDelayType01Z;-- delay for this path
        PathCondition   : BOOLEAN;          -- path sensitize condition
    END RECORD;

    -- For representing multiple paths to an output
    TYPE VitalPathArrayType    IS ARRAY (NATURAL RANGE <> ) OF VitalPathType;
    TYPE VitalPathArray01Type  IS ARRAY (NATURAL RANGE <> ) OF VitalPath01Type;
    TYPE VitalPathArray01ZType IS ARRAY (NATURAL RANGE <> ) OF VitalPath01ZType;

    TYPE VitalTableSymbolType IS (
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
       'S'       -- steady value                                
    );

    SUBTYPE VitalEdgeSymbolType IS VitalTableSymbolType RANGE '/' TO '*';

    -- ------------------------------------------------------------------------
    --
    -- Function Name:   VitalExtendToFillDelay
    --
    -- Description:     A six element array of delay values of type 
    --                  VitalDelayType01Z is returned when a 1, 2 or 6
    --                  element array is given.  This function will convert
    --                  VitalDelayType and VitalDelayType01 delay values into
    --                  a VitalDelayType01Z type following these rules:
    --
    --                  When a VitalDelayType is passed, all six transition
    --                  values are assigned the input value.  When a 
    --                  VitalDelayType01 is passed, the 01 transitions are
    --                  assigned to the 01, 0Z and Z1 transitions and the 10
    --                  transitions are assigned to 10, 1Z and Z0 transition
    --                  values.  When a VitalDelayType01Z is passed, the values
    --                  are kept as is.
    --
    --                  The function is overloaded based on input type.
    --
    --                  There is no function to fill a 12 value delay
    --                  type.
    --
    -- Arguments:         
    --
    --  IN                 Type           Description
    --                     Delay          A one, two or six delay value Vital-
    --                                    DelayType is passed and a six delay,
    --                                    VitalDelayType01Z, item is returned.        
    --                    
    --  INOUT
    --   none
    --
    --  OUT
    --   none
    --
    --  Returns              
    --   VitalDelayType01Z
    --
    -- -------------------------------------------------------------------------
    FUNCTION VitalExtendToFillDelay (
            CONSTANT Delay : IN VitalDelayType
          ) RETURN VitalDelayType01Z;
    FUNCTION VitalExtendToFillDelay (
            CONSTANT Delay : IN VitalDelayType01
          ) RETURN VitalDelayType01Z;
    FUNCTION VitalExtendToFillDelay (
            CONSTANT Delay : IN VitalDelayType01Z
          ) RETURN VitalDelayType01Z;

    -- ------------------------------------------------------------------------
    --
    -- Function Name:   VitalCalcDelay
    --
    -- Description:     This function accepts a 1, 2 or 6 value delay and 
    --                  chooses the correct delay time to delay the NewVal 
    --                  signal.  This function is overloaded based on the
    --                  delay type passed. The function returns a single value
    --                  of time.
    --
    --                  This function is provided for Level 0 models in order
    --                  to calculate the delay which should be applied 
    --                  for the passed signal. The delay selection is performed
    --                  using the OldVal and the NewVal to determine the 
    --                  transition to select.  The default value of OldVal is X.
    --
    --                  This function cannot be used in a Level 1 model since
    --                  the VitalPathDelay routines perform the delay path
    --                  selection and output driving function.
    --
    -- Arguments:         
    --
    --  IN                 Type           Description
    --                      NewVal         New value of the signal to be
    --                                     assigned   
    --                      OldVal         Previous value of the signal.
    --                                     Default value is 'X'
    --                      Delay          The delay structure from which to
    --                                     select the appropriate delay.  The
    --                                     function overload is based on the
    --                                     type of delay passed.  In the case of
    --                                     the single delay, VitalDelayType, no
    --                                     selection is performed, since there
    --                                     is only one value to choose from.
    --                                     For the other cases, the transition
    --                                     from the old value to the new value
    --                                     decide the value returned.  
    --                    
    --  INOUT
    --   none 
    --
    --  OUT
    --   none
    --
    --  Returns              
    --   Time                              The time value selected from the
    --                                     Delay INPUT is returned.
    --
    -- -------------------------------------------------------------------------
    FUNCTION VitalCalcDelay (
            CONSTANT NewVal : IN std_ulogic   := 'X';
            CONSTANT OldVal : IN std_ulogic   := 'X';
            CONSTANT Delay  : IN VitalDelayType
          ) RETURN TIME;
    FUNCTION VitalCalcDelay (
            CONSTANT NewVal : IN std_ulogic   := 'X';
            CONSTANT OldVal : IN std_ulogic   := 'X';
            CONSTANT Delay  : IN VitalDelayType01
          ) RETURN TIME;
    FUNCTION VitalCalcDelay (
            CONSTANT NewVal : IN std_ulogic   := 'X';
            CONSTANT OldVal : IN std_ulogic   := 'X';
            CONSTANT Delay  : IN VitalDelayType01Z
          ) RETURN TIME;

    -- ------------------------------------------------------------------------
    --
    -- Function Name:   VitalPathDelay
    --
    -- Description:     VitalPathDelay is the Level 1 routine used to select
    --                  the propagation delay path and schedule a new output 
    --                  value.
    --
    --                  For single and dual delay values, VitalDelayType and
    --                  VitalDelayType01 are used.  The output value is
    --                  scheduled with a calculated delay without strength
    --                  modification.  
    --
    --                  For the six delay value, VitalDelayType01Z, the output       
    --                  value is scheduled with a calculated delay.  The drive 
    --                  strength can be modified to handle weak signal strengths
    --                  to model tri-state devices, pull-ups and pull-downs as
    --                  an example.
    --
    --                  The correspondence between the delay type and the
    --                  path delay function is as follows:
    --
    --                  Delay Type                Path Type
    --
    --                  VitalDelayType            VitalPathDelay
    --                  VitalDelayType01          VitalPathDelay01
    --                  VitalDelayType01Z         VitalPathDelay01Z
    --
    --                  For each of these routines, the following capabilities
    --                  is provided:
    --     
    --                  o Transition dependent path delay selection
    --                  o User controlled glitch detection with the ability
    --                    to generate "X" on output and report the violation
    --                  o Control of the severity level for message generation
    --                  o Scheduling of the computed values on the specified
    --                    signal.
    --                  
    --                  Selection of the appropriate path delay begins with the
    --                  candidate paths.  The candidate paths are selected by
    --                  identifying the paths for which the PathCondition is 
    --                  true.  If there is a single candidate path, then that
    --                  delay is selected.  If there is more than one candidate
    --                  path, then the shortest delay is selected using 
    --                  transition dependent delay selection.  If there is no 
    --                  candidate paths, then the delay specified by the 
    --                  DefaultDelay parameter to the path delay is used.
    --                  
    --                  Once the delay is known, the output signal is then
    --                  scheduled with that delay.  In the case of 
    --                  VitalPathDelay01Z, an additional result mapping of
    --                  the output value is performed before scheduling.  The
    --                  result mapping is performed after transition dependent
    --                  delay selection but before scheduling the final output.
    --                      
    --                  In order to perform glitch detection, the user is
    --                  obligated to provide a variable of VitalGlitchDataType
    --                  for the propagation delay functions to use.  The user
    --                  cannot modify or use this information.
    --
    -- Arguments:         
    --
    --  IN             Type                   Description
    --   OutSignalName  string                 The name of the output signal
    --   OutTemp        std_logic              The new output value to be driven
    --   Paths          VitalPathArrayType     A list of paths of VitalPathArray
    --                  VitalPathArrayType01   type.  The VitalPathDelay routine
    --                  VitalPathArrayType01Z  is overloaded based on the type
    --                                         of constant passed in.  With
    --                                         VitalPathArrayType01Z, the
    --                                         resulting output strengths can be
    --                                         mapped.
    --   DefaultDelay   VitalDelayType         The default delay can be changed  
    --                  VitalDelayType01       from zero-delay to another set of 
    --                  VitalDelayType01Z      values.   
    --   Mode           VitalGlitchKindType    The value of this constant
    --                                         selects the type of glitch
    --                                         detection.
    --                       OnEvent           Glitch on transition event
    --                     | OnDetect          Glitch immediate on detection
    --                     | VitalInertial     No glitch, use INERTIAL
    --                                         assignment
    --                     | VitalTransport    No glitch, use TRANSPORT 
    --                                         assignment
    --   XOn            BOOLEAN                Control for generation of 'X' on 
    --                                         glitch.  When TRUE, 'X's are
    --                                         scheduled for glitches, otherwise
    --                                         no are generated.             
    --   MsgOn          BOOLEAN                Control for message generation on
    --                                         glitch detect.  When TRUE,
    --                                         glitches are reported, otherwise
    --                                         they are not reported.      
    --   MsgSeverity    SEVERITY_LEVEL         The level at which the message,
    --                                         or assertion, will be reported. 
    --   OutputMap      VitalOutputMapType     For VitalPathDelay01Z, the output
    --                                         can be mapped to alternate
    --                                         strengths to model tri-state 
    --                                         devices, pull-ups and pull-downs.  
    -- 
    --  INOUT
    --   GlitchData     VitalGlitchDataType    The internal data storage
    --                                         variable required to detect
    --                                         glitches.
    --
    --  OUT
    --   OutSignal      std_logic              The output signal to be driven
    --
    --  Returns              
    --   none 
    --
    -- -------------------------------------------------------------------------
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
    );
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
    );
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
    );

    -- ------------------------------------------------------------------------
    --
    -- Function Name:   VitalWireDelay
    --
    -- Description:     VitalWireDelay is used to delay an input signal.
    --                  The delay is selected from the input parameter passed.
    --                  The function is useful for back annotation of actual
    --                  net delays.  
    --                  
    --                  The function is overloaded to permit passing a delay
    --                  value for twire for VitalDelayType, VitalDelayType01
    --                  and VitalDelayType01Z.  twire is a generic which can
    --                  be back annotated and must be constructed to follow
    --                  the SDF to generic mapping rules.       
    --                  
    -- Arguments:         
    --
    --  IN          Type                  Description         
    --   InSig       std_ulogic            The input signal (port) to be
    --                                     delayed.
    --   twire       VitalDelayType        The delay value for which the input 
    --               VitalDelayType01      signal should be delayed.  For Vital-
    --               VitalDelayType01Z     DelayType, the value is single value 
    --                                     passed.  For VitalDelayType01 and
    --                                     VitalDelayType01Z, the appropriate
    --                                     delay value is selected by VitalCalc-
    --                                     Delay.
    --
    --  INOUT
    --   none 
    --
    --  OUT
    --   OutSig      std_ulogic            The internal delayed signal
    --
    --  Returns              
    --   none 
    --
    -- -------------------------------------------------------------------------
    PROCEDURE VitalWireDelay (
            SIGNAL   OutSig     : OUT   std_ulogic;
            SIGNAL   InSig      : IN    std_ulogic;
            CONSTANT twire      : IN    VitalDelayType
        );

    PROCEDURE VitalWireDelay (
            SIGNAL   OutSig     : OUT   std_ulogic;
            SIGNAL   InSig      : IN    std_ulogic;
            CONSTANT twire      : IN    VitalDelayType01
        );

    PROCEDURE VitalWireDelay (
            SIGNAL   OutSig     : OUT   std_ulogic;
            SIGNAL   InSig      : IN    std_ulogic;
            CONSTANT twire      : IN    VitalDelayType01Z
        );

    -- ------------------------------------------------------------------------
    --
    -- Function Name:   VitalSignalDelay
    --
    -- Description:     The VitalSignalDelay procedure is called in a signal
    --                  delay block in the architecture to delay the 
    --                  appropriate test or reference signal in order to 
    --                  accommodate negative constraint checks.
    --
    --                  The amount of delay is of type TIME and is a constant.
    --
    -- Arguments:         
    --
    --  IN                Type            Description         
    --   InSig             std_ulogic      The signal to be delayed.
    --   dly               TIME            The amount of time the signal is
    --                                     delayed.
    --
    --  INOUT
    --   none
    --
    --  OUT
    --   OutSig            std_ulogic      The delayed signal
    --
    --  Returns              
    --   none
    --
    -- -------------------------------------------------------------------------
    PROCEDURE VitalSignalDelay (
            SIGNAL   OutSig     : OUT   std_ulogic;
            SIGNAL   InSig      : IN    std_ulogic;
            CONSTANT dly        : IN   TIME
    );

    -- ------------------------------------------------------------------------
    --
    -- Function Name:  VitalSetupHoldCheck
    --
    -- Description:    The VitalSetupHoldCheck procedure detects a setup or a 
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
    --                 TestSignal.  A vector and scalar form are provided.
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
    --  IN                 Type           Description    
    --   TestSignal         std_ulogic     Value of test signal
    --                      std_logic_vector
    --   TestSignalName     STRING         Name of test signal
    --   TestDelay          TIME           Model's internal delay associated
    --                                     with TestSignal
    --   RefSignal          std_ulogic     Value of reference signal
    --   RefSignalName      STRING         Name of reference signal
    --   RefDelay           TIME           Model's internal delay associated
    --                                     with RefSignal
    --   SetupHigh          TIME           Absolute minimum time duration before
    --                                     the transition of RefSignal for which
    --                                     transitions of TestSignal are allowed
    --                                     to proceed to the "1" state without
    --                                     causing a setup violation.      
    --   SetupLow           TIME           Absolute minimum time duration before 
    --                                     the transition of RefSignal for which
    --                                     transitions of TestSignal are allowed
    --                                     to proceed to the "0" state without
    --                                     causing a setup violation.      
    --   HoldHigh           TIME           Absolute minimum time duration after
    --                                     the transition of RefSignal for which
    --                                     transitions of TestSignal are allowed
    --                                     to proceed to the "1" state without
    --                                     causing a hold violation.      
    --   HoldLow            TIME           Absolute minimum time duration after 
    --                                     the transition of RefSignal for which
    --                                     transitions of TestSignal are allowed
    --                                     to proceed to the "0" state without
    --                                     causing a hold violation.      
    --   CheckEnabled       BOOLEAN        Check performed if TRUE.
    --   RefTransition      VitalEdgeSymbolType
    --                                     Reference edge specified. Events on
    --                                     the RefSignal which match the edge
    --                                     spec. are used as reference edges.      
    --   HeaderMsg          STRING         String that will accompany any
    --                                     assertion messages produced.      
    --   XOn                BOOLEAN        If TRUE, Violation output parameter
    --                                     is set to "X". Otherwise, Violation
    --                                     is always set to "0."      
    --   MsgOn              BOOLEAN        If TRUE, set and hold violation 
    --                                     message will be generated.
    --                                     Otherwise, no messages are generated,
    --                                     even upon violations.      
    --   MsgSeverity        SEVERITY_LEVEL Severity level for the assertion.      
    --              
    --  INOUT
    --   TimingData         VitalTimingDataType  
    --                                     VitalSetupHoldCheck information
    --                                     storage area.  This is used
    --                                     internally to detect reference edges
    --                                     and record the time of the last edge.
    --
    --  OUT
    --   Violation   X01                   This is the violation flag returned. 
    --
    --  Returns              
    --   none    
    --
    -- -------------------------------------------------------------------------
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
            CONSTANT HeaderMsg     : IN     STRING := " ";
            CONSTANT XOn           : IN     BOOLEAN := TRUE;
            CONSTANT MsgOn         : IN     BOOLEAN := TRUE;
            CONSTANT MsgSeverity   : IN     SEVERITY_LEVEL := WARNING
    );

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
    );

    -- ------------------------------------------------------------------------
    --
    -- Function Name:   VitalRecoveryRemovalCheck
    --
    -- Description:     The VitalRecoveryRemovalCheck detects the presence of
    --                  a recovery or removal violation on the input test 
    --                  signal with respect to the corresponding input reference
    --                  signal.  It assumes non-negative values of setup and
    --                  hold timing constraints.  The timing constraint is 
    --                  specified through parameters representing the recovery
    --                  and removal times associated with a reference edge of 
    --                  the reference signal.  A flag indicates whether a test
    --                  signal is asserted when it is high or when it is low.
    --                  
    --                  It is assumed that negative timing constraints 
    --                  are handled by internally delaying the test or 
    --                  reference signals.  Negative recovery times result in 
    --                  a delayed reference signal.  Negative removal times 
    --                  result in a delayed test signal.  Furthermore, the 
    --                  delays and constraints associated with these and
    --                  other signals may need to be appropriately
    --                  adjusted so that all constraint intervals overlap 
    --                  the delayed reference signals and all constraint
    --                  values (with respect to the delayed signals) are
    --                  non-negative.
    --                  
    -- Arguments:         
    --
    --  IN               Type             Description
    --   TestSignal       std_ulogic       Value of TestSignal.  The routine is
    --   TestSignalName   STRING           Name of TestSignal 
    --   TestDelay        TIME             Model internal delay associated with
    --                                     the TestSignal
    --   RefSignal        std_ulogic       Value of RefSignal
    --   RefSignalName    STRING           Name of RefSignal
    --   RefDelay         TIME             Model internal delay associated with
    --                                     the RefSignal
    --   Recovery         TIME             A change to an unasserted value on
    --                                     the asynchronous TestSignal must
    --                                     precede reference edge (on RefSignal)
    --                                     by at least this time.
    --   Removal          TIME             An asserted condition must be present
    --                                     on the asynchronous TestSignal for at
    --                                     least the removal time following a
    --                                     reference edge on RefSignal.
    --   ActiveLow        BOOLEAN          A flag which indicates if TestSignal
    --                                     is asserted when it is low - "0."
    --                                     FALSE indicate that TestSignal is
    --                                     asserted when it has a value "1."
    --   CheckEnabled     BOOLEAN          The check in enabled when the value
    --                                     is TRUE, otherwise the constraints 
    --                                     are not checked.
    --   RefTransition    VitalEdgeSymbolType  
    --                                     Reference edge specifier.  Events on
    --                                     RefSignal will match the edge
    --                                     specified.
    --   HeaderMsg         STRING          A header message that will accompany
    --                                     any assertion message.
    --   XOn               BOOLEAN         When TRUE, the output Violation is
    --                                     set to "X."  When FALSE, it is always 
    --                                     "0."
    --   MsgOn             BOOLEAN         When TRUE, violation messages are
    --                                     output.  When FALSE, no messages are
    --                                     generated.
    --   MsgSeverity       SEVERITY_LEVEL  Severity level of the asserted
    --                                     message.
    --                             
    --   INOUT
    --    TimingData       VitalTimingDataType 
    --                                     VitalRecoveryRemovalCheck information
    --                                     storage area.  This is used
    --                                     internally to detect reference edges
    --                                     and record the time of the last edge.
    --   OUT
    --    Violation        X01             This is the violation flag returned.
    --
    --   Returns              
    --           none
    --
    -- -------------------------------------------------------------------------
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
    );

    -- ------------------------------------------------------------------------
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
    --  IN                 Type           Description
    --    TestSignal        std_ulogic     Value of test signal  
    --    TestSignalName    STRING         Name of the test signal
    --    TestDelay         TIME           Model's internal delay associated
    --                                     with TestSignal
    --    Period            TIME           Minimum period allowed between
    --                                     consecutive rising ('P') or 
    --                                     falling ('F') transitions.
    --    PulseWidthHigh    TIME           Minimum time allowed for a high
    --                                     pulse ('1' or 'H')
    --    PulseWidthLow     TIME           Minimum time allowed for a low
    --                                     pulse ('0' or 'L')
    --    CheckEnabled      BOOLEAN        Check performed if TRUE.
    --    HeaderMsg         STRING         String that will accompany any
    --                                     assertion messages produced.   
    --    XOn               BOOLEAN        If TRUE, Violation output parameter
    --                                     is set to "X". Otherwise, Violation
    --                                     is always set to "0." 
    --    MsgOn             BOOLEAN        If TRUE, period/pulse violation
    --                                     message will be generated.
    --                                     Otherwise, no messages are generated,
    --                                     even though a violation is detected. 
    --    MsgSeverity       SEVERITY_LEVEL Severity level for the assertion.
    --         
    --   INOUT
    --    PeriodData        VitalPeriodDataType 
    --                                     VitalPeriodPulseCheck information
    --                                     storage area.  This is used
    --                                     internally to detect reference edges
    --                                     and record the pulse and period
    --                                     times.
    --   OUT
    --    Violation         X01            This is the violation flag returned. 
    --
    --   Returns              
    --    none
    --
    -- ------------------------------------------------------------------------
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
        );
 
END VITAL_Timing;
