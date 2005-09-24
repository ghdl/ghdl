--------------------------------------------------------------------------
--
-- Copyright (c) 1990, 1991, 1992 by Synopsys, Inc.  All rights reserved.
-- 
-- This source file may be used and distributed without restriction 
-- provided that this copyright statement is not removed from the file 
-- and that any derivative work contains this copyright notice.
--
--	Package name: std_logic_misc
--
--	Purpose: This package defines supplemental types, subtypes, 
--		 constants, and functions for the Std_logic_1164 Package.
--
--	Author:  GWH
--
--------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;
--library SYNOPSYS;
--use SYNOPSYS.attributes.all;


package std_logic_misc is

    -- output-strength types

    type STRENGTH is (strn_X01, strn_X0H, strn_XL1, strn_X0Z, strn_XZ1, 
		      strn_WLH, strn_WLZ, strn_WZH, strn_W0H, strn_WL1);


--synopsys synthesis_off

    type MINOMAX is array (1 to 3) of TIME;
    

    ---------------------------------------------------------------------
    --
    -- functions for mapping the STD_(U)LOGIC according to STRENGTH
    --
    ---------------------------------------------------------------------

    function strength_map(input: STD_ULOGIC; strn: STRENGTH) return STD_LOGIC;

    function strength_map_z(input:STD_ULOGIC; strn:STRENGTH) return STD_LOGIC;

    ---------------------------------------------------------------------
    --
    -- conversion functions for STD_ULOGIC_VECTOR and STD_LOGIC_VECTOR
    --
    ---------------------------------------------------------------------

--synopsys synthesis_on
    function Drive (V: STD_ULOGIC_VECTOR) return STD_LOGIC_VECTOR;

    function Drive (V: STD_LOGIC_VECTOR) return STD_ULOGIC_VECTOR;
--synopsys synthesis_off

    --attribute CLOSELY_RELATED_TCF of Drive: function is TRUE;

    ---------------------------------------------------------------------
    --
    -- conversion functions for sensing various types
    -- (the second argument allows the user to specify the value to
    --  be returned when the network is undriven)
    --
    ---------------------------------------------------------------------

    function Sense (V: STD_ULOGIC; vZ, vU, vDC: STD_ULOGIC) return STD_LOGIC;

    function Sense (V: STD_ULOGIC_VECTOR; vZ, vU, vDC: STD_ULOGIC) 
    					   return STD_LOGIC_VECTOR;
    function Sense (V: STD_ULOGIC_VECTOR; vZ, vU, vDC: STD_ULOGIC) 
    					   return STD_ULOGIC_VECTOR;

    function Sense (V: STD_LOGIC_VECTOR; vZ, vU, vDC: STD_ULOGIC) 
    					   return STD_LOGIC_VECTOR;
    function Sense (V: STD_LOGIC_VECTOR; vZ, vU, vDC: STD_ULOGIC) 
    					   return STD_ULOGIC_VECTOR;

--synopsys synthesis_on


    ---------------------------------------------------------------------
    --
    --	Function: STD_LOGIC_VECTORtoBIT_VECTOR STD_ULOGIC_VECTORtoBIT_VECTOR
    --
    --	Purpose: Conversion fun. from STD_(U)LOGIC_VECTOR to BIT_VECTOR
    --
    --	Mapping:	0, L --> 0
    --			1, H --> 1
    --			X, W --> vX if Xflag is TRUE
    --			X, W --> 0  if Xflag is FALSE
    --			Z --> vZ if Zflag is TRUE
    --			Z --> 0  if Zflag is FALSE
    --			U --> vU if Uflag is TRUE
    --			U --> 0  if Uflag is FALSE
    --			- --> vDC if DCflag is TRUE
    --			- --> 0  if DCflag is FALSE
    --
    ---------------------------------------------------------------------

    function STD_LOGIC_VECTORtoBIT_VECTOR (V: STD_LOGIC_VECTOR
--synopsys synthesis_off
    	; vX, vZ, vU, vDC: BIT := '0'; 
    	  Xflag, Zflag, Uflag, DCflag: BOOLEAN := FALSE
--synopsys synthesis_on
    	) return BIT_VECTOR;

    function STD_ULOGIC_VECTORtoBIT_VECTOR (V: STD_ULOGIC_VECTOR
--synopsys synthesis_off
    	; vX, vZ, vU, vDC: BIT := '0'; 
    	  Xflag, Zflag, Uflag, DCflag: BOOLEAN := FALSE
--synopsys synthesis_on
    	) return BIT_VECTOR;
    

    ---------------------------------------------------------------------
    --
    --	Function: STD_ULOGICtoBIT
    --
    --	Purpose: Conversion function from STD_(U)LOGIC to BIT
    --
    --	Mapping:	0, L --> 0
    --			1, H --> 1
    --			X, W --> vX if Xflag is TRUE
    --			X, W --> 0  if Xflag is FALSE
    --			Z --> vZ if Zflag is TRUE
    --			Z --> 0  if Zflag is FALSE
    --			U --> vU if Uflag is TRUE
    --			U --> 0  if Uflag is FALSE
    --			- --> vDC if DCflag is TRUE
    --			- --> 0  if DCflag is FALSE
    --
    ---------------------------------------------------------------------

    function STD_ULOGICtoBIT (V: STD_ULOGIC
--synopsys synthesis_off
    	; vX, vZ, vU, vDC: BIT := '0'; 
    	  Xflag, Zflag, Uflag, DCflag: BOOLEAN := FALSE
--synopsys synthesis_on
    	) return BIT;

        --------------------------------------------------------------------
        function AND_REDUCE(ARG: STD_LOGIC_VECTOR) return UX01;
        function NAND_REDUCE(ARG: STD_LOGIC_VECTOR) return UX01;
        function OR_REDUCE(ARG: STD_LOGIC_VECTOR) return UX01;
        function NOR_REDUCE(ARG: STD_LOGIC_VECTOR) return UX01;
        function XOR_REDUCE(ARG: STD_LOGIC_VECTOR) return UX01;
        function XNOR_REDUCE(ARG: STD_LOGIC_VECTOR) return UX01;
    
        function AND_REDUCE(ARG: STD_ULOGIC_VECTOR) return UX01;
        function NAND_REDUCE(ARG: STD_ULOGIC_VECTOR) return UX01;
        function OR_REDUCE(ARG: STD_ULOGIC_VECTOR) return UX01;
        function NOR_REDUCE(ARG: STD_ULOGIC_VECTOR) return UX01;
        function XOR_REDUCE(ARG: STD_ULOGIC_VECTOR) return UX01;
        function XNOR_REDUCE(ARG: STD_ULOGIC_VECTOR) return UX01;
    
--synopsys synthesis_off
	
        function fun_BUF3S(Input, Enable: UX01; Strn: STRENGTH) return STD_LOGIC;
        function fun_BUF3SL(Input, Enable: UX01; Strn: STRENGTH) return STD_LOGIC;
        function fun_MUX2x1(Input0, Input1, Sel: UX01) return UX01;

        function fun_MAJ23(Input0, Input1, Input2: UX01) return UX01;
        function fun_WiredX(Input0, Input1: std_ulogic) return STD_LOGIC;

--synopsys synthesis_on
	
end;
