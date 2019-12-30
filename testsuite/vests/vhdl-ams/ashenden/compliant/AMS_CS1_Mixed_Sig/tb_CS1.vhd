
-- Copyright (C) 2002 Morgan Kaufmann Publishers, Inc

-- This file is part of VESTs (Vhdl tESTs).

-- VESTs is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at
-- your option) any later version. 

-- VESTs is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
-- for more details. 

-- You should have received a copy of the GNU General Public License
-- along with VESTs; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA 

-- Simple Digital-Controlled Two-position Switch Model
-- Switch position 1 ('0') or switch position 2 ('1')

LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
use IEEE.std_logic_arith.all;
use IEEE.math_real.all;

-- Use proposed IEEE natures and packages
LIBRARY IEEE_proposed;
USE IEEE_proposed.electrical_systems.ALL;

ENTITY switch_dig_2in is
	GENERIC (r_open : RESISTANCE := 1.0e6; -- Open switch resistance
		 	r_closed : RESISTANCE := 0.001; -- Closed switch resistance
		 	trans_time : real := 0.00001); -- Transition time to each position

	PORT (sw_state : in std_logic; -- Digital control input 
    	      TERMINAL p_in1, p_in2, p_out : ELECTRICAL); -- Analog output

END ENTITY switch_dig_2in;


ARCHITECTURE ideal OF switch_dig_2in IS

	SIGNAL r_sig1 : RESISTANCE := r_closed; -- Variable to accept switch resistance
	SIGNAL r_sig2 : RESISTANCE := r_open; -- Variable to accept switch resistance
	QUANTITY v1 ACROSS i1 THROUGH p_in1 TO p_out; -- V & I for in1 to out
	QUANTITY v2 ACROSS i2 THROUGH p_in2 TO p_out; -- V & I for in2 to out
	QUANTITY r1 : RESISTANCE; -- Time-varying resistance for in1 to out
	QUANTITY r2 : RESISTANCE; -- Time-varying resistance for in2 to out

BEGIN
 
 PROCESS (sw_state) -- Sensitivity to digital control input
    BEGIN
	  IF (sw_state = '0') THEN -- Close sig1, open sig2
	    r_sig1 <= r_closed;
	    r_sig2 <= r_open;
	  ELSIF (sw_state = '1') THEN -- Open sig1, close sig2
	    r_sig1 <= r_open;
	    r_sig2 <= r_closed;
	  END IF;
    END PROCESS;

	r1 == r_sig1'ramp(trans_time, trans_time); -- Ensure resistance continuity
	r2 == r_sig2'ramp(trans_time, trans_time); -- Ensure resistance continuity
	v1 == r1*i1; -- Apply Ohm's law to in1
	v2 == r2*i2; -- Apply Ohm's law to in2

END ARCHITECTURE ideal;
--

-- Digital clock with 50% duty cycle
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY clock IS
  GENERIC (
    period : time);              -- Clock period
  
  PORT (
    clk_out    : OUT std_logic);
  
END ENTITY clock;

ARCHITECTURE ideal OF clock IS

BEGIN

-- clock process
    process
    begin
        clk_out <= '0';
        wait for period/2;
        clk_out <= '1';
        wait for period/2;
    end process;

END ARCHITECTURE ideal;
--

-- This digital clock allows user to specify the duty cycle using
-- the parameters "on_time" and "off_time"

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

ENTITY clock_duty IS

  GENERIC (
    on_time : time := 20 us;
	off_time : time := 19.98 ms
 	); 
  
  PORT (
    clock_out    : OUT std_logic := '0');
  
END ENTITY clock_duty;

ARCHITECTURE ideal OF clock_duty IS

BEGIN

-- clock process
    process
    begin
        clock_out <= '1';
        wait for on_time;
        clock_out <= '0';
        wait for off_time;
    end process;

END ARCHITECTURE ideal;
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity rc_clk is
    port(
        clk_100k : out std_logic;
        clk_6K : out std_logic;
        clk_50 : out std_logic
    );
end rc_clk;

architecture rc_clk of rc_clk is
    -- Component declarations
    -- Signal declarations
begin
    -- Signal assignments
    -- Component instances
    XCMP1 : entity work.clock(ideal)
        generic map(
            period => 10 us
        )
        port map(
            CLK_OUT => clk_100k
        );
    XCMP2 : entity work.clock(ideal)
        generic map(
            period => 150 us
        )
        port map(
            CLK_OUT => clk_6K
        );
    clk_50Hz : entity work.clock_duty(ideal)
        generic map(
            on_time => 20 us,
            off_time => 19.98 ms
        )
        port map(
            CLOCK_OUT => clk_50
        );
end rc_clk;
--

-- This model counts the number of input clock transitions and outputs
-- a '1' when this number equals the value of the user-defined constant 'count'

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity bit_cnt is
  generic (
	count : integer -- User-defined value to count up to
	);             
port
(
      bit_in : in std_logic ;
      clk : in std_logic ;
      dly_out : out std_logic 
);
end bit_cnt;

architecture behavioral of bit_cnt is
begin
	serial_clock : process is
	begin
		wait until bit_in'event AND (bit_in = '1' OR bit_in = 'H');
	  	FOR i IN 0 to count LOOP -- Loop for 'count' clock transitions
	  		wait until clk'event AND (clk = '1' OR clk = 'H');
     	END LOOP ;
		dly_out <= '1'; -- After count is reached, set output high
		wait until bit_in'event AND (bit_in = '0' OR bit_in = 'L');
		dly_out <= '0'; -- Reset output to '0' on next clock input
	end process serial_clock;
end;
--

LIBRARY IEEE;
USE IEEE.std_logic_1164.all;
USE IEEE.std_logic_arith.all;
LIBRARY IEEE_proposed;
USE IEEE_proposed.electrical_systems.all;
USE IEEE_proposed.mechanical_systems.all;

ENTITY state_mach1 IS
    PORT (
          a2d_eoc : IN std_logic;
          clk_50 : IN std_logic;
          clk_100k : IN std_logic;
          clk_6k : IN std_logic;
          ser_done : IN std_logic;
          ch_sel : OUT std_logic;
          frm_gen : OUT std_logic;
          a2d_oe : OUT std_logic;
          a2d_start : OUT std_logic;
          p2s_oe : OUT std_logic;
          p2s_load : OUT std_logic;
          parity_oe : OUT std_logic;
          ser_cnt : OUT std_logic;
          p2s_clr : OUT std_logic);

END state_mach1;

ARCHITECTURE state_diagram OF state_mach1 IS

    ATTRIBUTE ENUM_TYPE_ENCODING: STRING;

    TYPE TYP_state_mach1_sm1 IS (V_begin, frm_rd, ser_oe, ch1, data_en, tdm_oe, ch2
           , load, ad_ch2, delay);
    SIGNAL CS_state_mach1_sm1, NS_state_mach1_sm1 : TYP_state_mach1_sm1;

    SIGNAL FB_frm_gen : std_logic;
    SIGNAL FB_p2s_load : std_logic;
    SIGNAL FB_ch_sel : std_logic;

BEGIN 
    frm_gen <= FB_frm_gen ;
    p2s_load <= FB_p2s_load ;
    ch_sel <= FB_ch_sel ;

sm1: 
  PROCESS (CS_state_mach1_sm1, clk_50, FB_frm_gen, FB_p2s_load, ser_done, a2d_eoc, FB_ch_sel) 
  BEGIN

    CASE CS_state_mach1_sm1 IS
      WHEN V_begin =>
        FB_frm_gen <= ('1');
        a2d_start <= ('0');
        a2d_oe <= ('0');
        FB_p2s_load <= ('0');
        p2s_clr <= ('0');
        p2s_oe <= ('0');
        FB_ch_sel <= ('0');
        parity_oe <= ('0');
        ser_cnt <= ('0');

        IF ((FB_frm_gen = '1')) THEN
          NS_state_mach1_sm1 <= frm_rd;
        ELSE
          NS_state_mach1_sm1 <= V_begin;
        END IF;

      WHEN frm_rd =>
        FB_p2s_load <= ('1');

        IF ((FB_p2s_load = '1')) THEN
          NS_state_mach1_sm1 <= ser_oe;
        ELSE
          NS_state_mach1_sm1 <= frm_rd;
        END IF;

      WHEN ser_oe =>
        p2s_oe <= ('1');
        FB_frm_gen <= ('0');
        FB_p2s_load <= ('0');
        ser_cnt <= ('1');

        IF ((ser_done = '1')) THEN
          NS_state_mach1_sm1 <= ch1;
        ELSE
          NS_state_mach1_sm1 <= ser_oe;
        END IF;

      WHEN ch1 =>
        p2s_oe <= ('0');
        FB_ch_sel <= ('0');
        a2d_start <= ('1');
        ser_cnt <= ('0');

        IF ((a2d_eoc = '1')) THEN
          NS_state_mach1_sm1 <= data_en;
        ELSE
          NS_state_mach1_sm1 <= ch1;
        END IF;

      WHEN data_en =>
        a2d_start <= ('0');
        a2d_oe <= ('1');
        parity_oe <= ('1');
          NS_state_mach1_sm1 <= load;

      WHEN tdm_oe =>
        a2d_oe <= ('0');
        parity_oe <= ('0');
        p2s_oe <= ('1');
        FB_p2s_load <= ('0');
        ser_cnt <= ('1');

        IF (((ser_done = '1') AND (FB_ch_sel = '0'))) THEN
          NS_state_mach1_sm1 <= ch2;
        ELSE
          NS_state_mach1_sm1 <= tdm_oe;
        END IF;

      WHEN ch2 =>
        p2s_oe <= ('0');
        ser_cnt <= ('0');
        FB_ch_sel <= ('1');
          NS_state_mach1_sm1 <= delay;

      WHEN load =>
        FB_p2s_load <= ('1');
          NS_state_mach1_sm1 <= tdm_oe;

      WHEN ad_ch2 =>
        a2d_start <= ('1');

        IF ((a2d_eoc = '1')) THEN
          NS_state_mach1_sm1 <= data_en;
        ELSE
          NS_state_mach1_sm1 <= ad_ch2;
        END IF;

      WHEN delay =>
          NS_state_mach1_sm1 <= ad_ch2;

    END CASE;

  END PROCESS; 

sm1_CTL:
  PROCESS (clk_100k, clk_50)
  BEGIN

    IF (clk_100k'event AND clk_100k='1')  THEN 
        IF (clk_50= '1' ) THEN 
            CS_state_mach1_sm1 <= V_begin;
        ELSE
            CS_state_mach1_sm1 <= NS_state_mach1_sm1;
        END IF;
    END IF;

  END PROCESS;


END state_diagram;
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity sm_cnt is
    port(
        a2d_eoc : in std_logic;
        clk_50 : in std_logic;
        clk_100k : in std_logic;
        clk_6k : in std_logic;
        p2s_load : out std_logic;
        p2s_oe : out std_logic;
        parity_oe : out std_logic;
        a2d_start : out std_logic;
        a2d_oe : out std_logic;
        frm_gen : out std_logic;
        ch_sel : out std_logic;
        p2s_clr : out std_logic
    );
end sm_cnt;

architecture sm_cnt of sm_cnt is
    -- Component declarations
    -- Signal declarations
    signal serial_cnt : std_logic;
    signal XSIG010022 : std_logic;
begin
    -- Signal assignments
    -- Component instances
    bit_cnt1 : entity work.bit_cnt(behavioral)
        generic map(
            count => 15
        )
        port map(
            bit_in => serial_cnt,
            clk => clk_6k,
            dly_out => XSIG010022
        );
    state_mach16 : entity work.state_mach1
        port map(
            ser_cnt => serial_cnt,
            ch_sel => ch_sel,
            frm_gen => frm_gen,
            a2d_oe => a2d_oe,
            a2d_start => a2d_start,
            parity_oe => parity_oe,
            p2s_oe => p2s_oe,
            p2s_load => p2s_load,
            p2s_clr => p2s_clr,
            clk_6k => clk_6k,
            clk_100k => clk_100k,
            clk_50 => clk_50,
            a2d_eoc => a2d_eoc,
            ser_done => XSIG010022
        );
end sm_cnt;
--

--This is a VHDL-AMS model of a simple analog to digital converter. The model
--describes the general behavior of A/D converters for system level design and
--verification.
--The format of the digital output is binary coding.
--
--N.B, dout(n-1) is the MSB while dout(0) is the LSB.
--

-- Use IEEE natures and packages
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;

entity a2d_nbit is
 generic (
  Vmax: REAL := 5.0 ;   -- ADC's maximum range
  Nbits: INTEGER := 10 ;  -- number bits in ADC's output
  delay: TIME := 10 us    -- ADC's conversion time
 );
 
port (
  signal start: in std_logic ; -- Start signal
  signal clk: in std_logic ;   -- Strobe clock
  signal oe: in std_logic ; -- Output enable
  terminal ain: ELECTRICAL ;  -- ADC's analog input terminal
  signal eoc: out std_logic := '0' ; -- End Of Conversion pin
  signal dout: out std_logic_vector(0 to (Nbits-1))); -- ADC's digital output signal
end entity a2d_nbit;

architecture sar of a2d_nbit is

  type states is (input, convert, output) ; -- Three states of A2D Conversion
  constant bit_range : INTEGER := Nbits-1 ; -- Bit range for dtmp and dout
  quantity Vin across Iin through ain to electrical_ref;     -- ADC's input branch

begin

 sa_adc: process

  variable thresh: REAL := Vmax ; -- Threshold to test input voltage against
  variable Vtmp: REAL := Vin ; -- Snapshot of input voltage when conversion starts
  variable dtmp: std_logic_vector(0 to (Nbits-1)); -- Temp. output data
  variable status: states := input ; -- Begin with "input" CASE 
  variable bit_cnt: integer := Nbits -1 ;

 begin
 CASE status is
	when input => -- Read input voltages when start goes high
		wait on start until start = '1' or start = 'H' ;
  		thresh := Vmax ;
  		Vtmp := Vin ;
		eoc <= '0' ;
		status := convert ; -- Go to convert state
	when convert => -- Begin successive approximation conversion
		thresh := thresh / 2.0 ; -- Get value of MSB
		wait on clk until clk = '1' OR clk = 'H';
		if Vtmp > thresh then
			dtmp(bit_cnt) := '1' ;
			Vtmp := Vtmp - thresh ;
		else
			dtmp(bit_cnt) := '0' ;
		end if ;
		if bit_cnt < 1 then
			status := output ; -- Go to output state
		end if;
		bit_cnt := bit_cnt - 1 ;
	when output => -- Wait for output enable, then put data on output pins
		eoc <= '1' after delay ;
		wait on oe until oe = '1' OR oe = 'H' ;
				dout <= dtmp ;
		wait on oe until oe = '0' OR oe = 'L' ; -- Hi Z when OE is low
				dout <= (others => 'Z') ;
		bit_cnt := bit_range ;
		status := input ; -- Set up for next conversion
	END CASE ;
 end process sa_adc ;

 Iin == 0.0 ; -- Ideal input draws no current

end architecture sar ;
--

-- Parallel input/serial output shift register
-- With 4 trailing zeros

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity shift_reg is
generic ( td : time := 0 ns); 

port
(
      bus_in : in std_logic_vector ; -- Input bus
      clk : in std_logic ; -- Shift clock
      oe : in std_logic ; -- Output enable
      ser_out : out std_logic := '0'; -- Output port
      load : in std_logic ; -- Parallel input load
	clr : in std_logic -- Clear register
);

end entity shift_reg;

architecture behavioral of shift_reg is
begin

control_proc : process
  VARIABLE bit_val : std_logic_vector(11 downto 0);  -- Default 12-bit input
  begin

	IF (clr = '1' OR clr = 'H') then
    		bit_val := "000000000000"; -- Set all input bits to zero
	ELSE
  		wait until load'event AND (load = '1' OR load = 'H');
		FOR i IN bus_in'high DOWNTO bus_in'low LOOP
        		bit_val(i) := bus_in(i) ; -- Transfer input data to variable
     		END LOOP ;
	END IF;

	wait until oe'event AND (oe = '1' OR oe = 'H'); -- Shift if output enabled
	FOR i IN bit_val'high DOWNTO bit_val'low LOOP
		wait until clk'event AND (clk = '1' OR clk = 'H');
      	ser_out <= bit_val(i) ;
	END LOOP ;

	  FOR i IN 1 TO 4 LOOP -- This loop pads the serial output with 4 zeros	
		wait until clk'event AND (clk = '1' OR clk = 'H');
		ser_out <= '0';
	  END LOOP;

END process;

end architecture behavioral;
--

-- This model generates a 12-bit data frame synchronization code

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity frame_gen is
port
(
      oe : in std_logic := '0';
	sync_out : out std_logic_vector (11 downto 0) := "ZZZZZZZZZZZZ");

end entity frame_gen;

architecture simple of frame_gen is
begin
	enbl: PROCESS
  		BEGIN 
  			WAIT ON OE;
			IF OE = '1' THEN
				sync_out <= "010101010101"; -- Sync code
			ELSE
				sync_out <= "ZZZZZZZZZZZZ";
			END IF;
  	END PROCESS;
end architecture simple;
--

-- Two input XOR gate
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY xor2 IS
  GENERIC (
    delay : time := 0 ns);              -- Delay time
  
  PORT (
    in1, in2 : IN  std_logic;
    output    : OUT std_logic);
  
END ENTITY xor2;

ARCHITECTURE ideal OF xor2 IS
BEGIN
   output <= in1 XOR in2 AFTER delay;
END ARCHITECTURE ideal;
--

-- level_set_tri.vhd 
-- If OE = '1' set digital output "level" with parameter "logic_val" (default is 'Z')
-- If OE = '0' set output to high impedance

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY level_set_tri IS

  GENERIC (
    logic_val : std_logic := 'Z');     

  PORT (
  	OE : IN std_logic;
    level : OUT std_logic := 'Z');

END ENTITY level_set_tri;

-- Simple architecture

ARCHITECTURE ideal OF level_set_tri IS
BEGIN
  oe_ctl: PROCESS
  BEGIN
	WAIT ON OE;
 	IF OE = '1' THEN 
   		level <= logic_val;
	ELSE
		level <= 'Z';
	END IF;
  END PROCESS;
  
END ARCHITECTURE ideal;

--

-- Simple Tri-state Buffer with delay time
-- If OE = 1, output = input after delay
-- If OE /= 1, output = Z after delay

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY buffer_tri IS
  GENERIC (
    delay : time := 0 ns);  -- Delay time
  
  PORT (
    input : IN  std_logic;
	OE : IN std_logic;
    output : OUT std_logic);
  
END ENTITY buffer_tri;

ARCHITECTURE ideal OF buffer_tri IS
BEGIN
  oe_ctl: PROCESS
  BEGIN
	WAIT ON OE, input;
 	IF OE = '1' THEN 
   		output <= input AFTER delay;
	ELSE
		output <= 'Z' AFTER delay;
	END IF;
  END PROCESS;
END ARCHITECTURE ideal;
--

-- ideal one bit D/A converter

LIBRARY IEEE_proposed;
USE IEEE_proposed.electrical_systems.ALL;

LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;

ENTITY d2a_bit IS
  GENERIC (vlow : real :=0.0;           -- output high voltage
           vhigh : real :=5.0);         -- output low voltage
  PORT	  (D : IN std_logic;            -- digital (std_logic) intout
           TERMINAL A : electrical);    -- analog (electrical) output
END ENTITY d2a_bit;

ARCHITECTURE ideal OF d2a_bit IS
  QUANTITY vout ACROSS iout THROUGH A TO ELECTRICAL_REF;
  SIGNAL vin : real := 0.0;

  BEGIN
    vin <= vhigh WHEN D = '1' ELSE vlow;
    -- Use 'RAMP for discontinuous signal
    vout == vin'RAMP(1.0e-9);      

END ARCHITECTURE ideal;
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity parity_gen is
    port(
        parity : in std_logic_vector(1 to 10);
        oe : in std_logic;
        parity_out : out std_logic_vector(0 to 11)
    );
end parity_gen;

architecture parity_gen of parity_gen is
    -- Component declarations
    -- Signal declarations
    terminal par_bit_gen_a : electrical;
    signal XSIG010002 : std_logic;
    signal XSIG010003 : std_logic;
    signal XSIG010004 : std_logic;
    signal XSIG010005 : std_logic;
    signal XSIG010006 : std_logic;
    signal XSIG010007 : std_logic;
    signal XSIG010008 : std_logic;
    signal XSIG010009 : std_logic;
    signal XSIG010098 : std_logic;
begin
    -- Signal assignments
    -- Component instances
    XCMP1 : entity work.xor2(ideal)
        port map(
            in1 => parity(1),
            in2 => parity(2),
            output => XSIG010002
        );
    XCMP2 : entity work.xor2(ideal)
        port map(
            in1 => parity(3),
            in2 => parity(4),
            output => XSIG010003
        );
    XCMP3 : entity work.xor2(ideal)
        port map(
            in1 => parity(5),
            in2 => parity(6),
            output => XSIG010004
        );
    XCMP4 : entity work.xor2(ideal)
        port map(
            in1 => parity(7),
            in2 => parity(8),
            output => XSIG010005
        );
    XCMP5 : entity work.xor2(ideal)
        port map(
            in1 => parity(9),
            in2 => parity(10),
            output => XSIG010008
        );
    XCMP6 : entity work.xor2(ideal)
        port map(
            in1 => XSIG010002,
            in2 => XSIG010003,
            output => XSIG010006
        );
    XCMP7 : entity work.xor2(ideal)
        port map(
            in1 => XSIG010004,
            in2 => XSIG010005,
            output => XSIG010007
        );
    XCMP8 : entity work.xor2(ideal)
        port map(
            in1 => XSIG010006,
            in2 => XSIG010007,
            output => XSIG010009
        );
    XCMP9 : entity work.xor2(ideal)
        port map(
            in1 => XSIG010009,
            in2 => XSIG010008,
            output => XSIG010098
        );
    XCMP18 : entity work.level_set_tri(ideal)
        generic map(
            logic_val => '1'
        )
        port map(
            level => parity_out(11),
            oe => oe
        );
    XCMP19 : entity work.buffer_tri(ideal)
        port map(
            input => parity(1),
            output => parity_out(1),
            oe => oe
        );
    XCMP20 : entity work.buffer_tri(ideal)
        port map(
            input => parity(2),
            output => parity_out(2),
            oe => oe
        );
    XCMP21 : entity work.buffer_tri(ideal)
        port map(
            input => parity(3),
            output => parity_out(3),
            oe => oe
        );
    XCMP22 : entity work.buffer_tri(ideal)
        port map(
            input => parity(4),
            output => parity_out(4),
            oe => oe
        );
    XCMP23 : entity work.buffer_tri(ideal)
        port map(
            input => parity(5),
            output => parity_out(5),
            oe => oe
        );
    XCMP24 : entity work.buffer_tri(ideal)
        port map(
            input => parity(6),
            output => parity_out(6),
            oe => oe
        );
    XCMP25 : entity work.buffer_tri(ideal)
        port map(
            input => parity(7),
            output => parity_out(7),
            oe => oe
        );
    XCMP26 : entity work.buffer_tri(ideal)
        port map(
            input => parity(8),
            output => parity_out(8),
            oe => oe
        );
    XCMP27 : entity work.buffer_tri(ideal)
        port map(
            input => parity(9),
            output => parity_out(9),
            oe => oe
        );
    XCMP28 : entity work.buffer_tri(ideal)
        port map(
            input => parity(10),
            output => parity_out(10),
            oe => oe
        );
    XCMP29 : entity work.buffer_tri(ideal)
        port map(
            input => XSIG010098,
            output => parity_out(0),
            oe => oe
        );
    XCMP30 : entity work.d2a_bit(ideal)
        port map(
            D => XSIG010098,
            A => par_bit_gen_a
        );
end parity_gen;
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity tdm_encoder is
    port(
        clk : in std_logic;
        p2s_oe : in std_logic;
        p2s_load : in std_logic;
        frm_gen : in std_logic;
        parity_oe : in std_logic;
        tdm_out : out std_logic;
        p2s_clr : in std_logic;
        a2d_data : in std_logic_vector(1 to 10)
    );
end tdm_encoder;

architecture tdm_encoder of tdm_encoder is
    -- Component declarations
    -- Signal declarations
    signal sync_par : std_logic_vector(0 to 11);
begin
    -- Signal assignments
    -- Component instances
    p2s1 : entity work.shift_reg(behavioral)
        port map(
            bus_in => sync_par,
            clk => clk,
            oe => p2s_oe,
            ser_out => tdm_out,
            load => p2s_load,
            clr => p2s_clr
        );
    sync_gen1 : entity work.frame_gen(simple)
        port map(
            oe => frm_gen,
            sync_out => sync_par
        );
    par_gen1 : entity work.parity_gen
        port map(
            parity => a2d_data,
            parity_out => sync_par,
            oe => parity_oe
        );
end tdm_encoder;
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity Digitize_Encode is
    port(
        tdm_out : out std_logic;
        terminal ch1_in : electrical;
        terminal ch2_in : electrical
    );
end Digitize_Encode;

architecture Digitize_Encode of Digitize_Encode is
    -- Component declarations
    -- Signal declarations
    terminal a2d_ana_in : electrical;
    signal a2d_oe : std_logic;
    signal ch_bus : std_logic_vector(1 to 10);
    signal frm_gen_ctl : std_logic;
    signal p2s_clr : std_logic;
    signal p2s_load : std_logic;
    signal p2s_oe : std_logic;
    signal par_oe : std_logic;
    signal start_a2d1 : std_logic;
    signal sw_ctl : std_logic;
    signal XSIG010091 : std_logic;
    signal XSIG010173 : std_logic;
    signal XSIG010180 : std_logic;
    signal XSIG010181 : std_logic;
begin
    -- Signal assignments
    -- Component instances
    A_SWITCH1 : entity work.switch_dig_2in(ideal)
        port map(
            p_in1 => ch1_in,
            p_out => a2d_ana_in,
            sw_state => sw_ctl,
            p_in2 => ch2_in
        );
    rc_clk2 : entity work.rc_clk
        port map(
            clk_50 => XSIG010180,
            clk_6K => XSIG010173,
            clk_100k => XSIG010181
        );
    sm_xmtr1 : entity work.sm_cnt
        port map(
            clk_100k => XSIG010181,
            a2d_start => start_a2d1,
            a2d_eoc => XSIG010091,
            p2s_oe => p2s_oe,
            p2s_load => p2s_load,
            ch_sel => sw_ctl,
            frm_gen => frm_gen_ctl,
            parity_oe => par_oe,
            a2d_oe => a2d_oe,
            clk_50 => XSIG010180,
            clk_6k => XSIG010173,
            p2s_clr => p2s_clr
        );
    a2d1 : entity work.a2d_nbit(sar)
        generic map(
            Vmax => 4.8
        )
        port map(
            dout => ch_bus,
            ain => a2d_ana_in,
            clk => XSIG010181,
            start => start_a2d1,
            eoc => XSIG010091,
            oe => a2d_oe
        );
    tdm_enc1 : entity work.tdm_encoder
        port map(
            clk => XSIG010173,
            p2s_oe => p2s_oe,
            tdm_out => tdm_out,
            p2s_load => p2s_load,
            a2d_data => ch_bus,
            frm_gen => frm_gen_ctl,
            parity_oe => par_oe,
            p2s_clr => p2s_clr
        );
end Digitize_Encode;
--

-- Electrical sinusoidal voltage source (stick.vhd)

LIBRARY IEEE;
USE IEEE.MATH_REAL.ALL;
-- Use proposed IEEE natures and packages
LIBRARY IEEE_proposed;
USE IEEE_proposed.ELECTRICAL_SYSTEMS.ALL;


ENTITY stick IS
  
-- Initialize parameters
  GENERIC (
    freq      : real;                     -- frequency,       [Hertz]
    amplitude : real;                     -- amplitude,       [Volt]
    phase     : real := 0.0;              -- initial phase,   [Degree]
    offset    : real := 0.0;              -- DC value,        [Volt]
    df        : real := 0.0;              -- damping factor,  [1/second]
    ac_mag    : real := 1.0;              -- AC magnitude,    [Volt]
    ac_phase  : real := 0.0);             -- AC phase,        [Degree]

-- Define ports as electrical terminals
  PORT (
    TERMINAL v_out : ELECTRICAL);

END ENTITY stick;

-- Ideal Architecture
ARCHITECTURE ideal OF stick IS
-- Declare Branch Quantities
  QUANTITY v ACROSS i THROUGH v_out TO electrical_ref;
-- Declare Quantity for Phase in radians (calculated below)
  QUANTITY phase_rad : real;          
-- Declare Quantity in frequency domain for AC analysis
  QUANTITY ac_spec : real SPECTRUM ac_mag, math_2_pi*ac_phase/360.0;

BEGIN  
-- Convert phase to radians
  phase_rad == math_2_pi *(freq * NOW + phase / 360.0);
  
  IF DOMAIN = QUIESCENT_DOMAIN OR DOMAIN = TIME_DOMAIN USE
    v == offset + amplitude * sin(phase_rad) * EXP(-NOW * df);
  ELSE 
    v == ac_spec;    -- used for Frequency (AC) analysis
  END USE;

END ARCHITECTURE ideal;
--

-- Two input AND gate
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY and2 IS
  GENERIC (
    delay : time := 0 ns);              -- Delay time
  
  PORT (
    in1, in2 : IN  std_logic;
    output    : OUT std_logic);
  
END ENTITY and2;

ARCHITECTURE ideal OF and2 IS
BEGIN
   output <= in1 AND in2 AFTER delay;
END ARCHITECTURE ideal;
--

-- D Flip Flop with reset (negative edge triggered)

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY d_latch_n_edge_rst IS
	GENERIC (
		delay : time := 0 ns);  -- Delay time
  
	PORT (
		data, clk 	: IN std_logic;
		q 	: OUT std_logic := '0';
		qn	: OUT std_logic := '1';
		rst	: IN std_logic := '0');  -- reset 
	   
END ENTITY d_latch_n_edge_rst ;

ARCHITECTURE behav OF d_latch_n_edge_rst IS
BEGIN
 
 data_in : PROCESS(clk, rst) IS  

   BEGIN
     IF clk = '0' AND clk'event AND rst /= '1' THEN
	     q  <= data     	AFTER delay;
         qn <= NOT data 	AFTER delay;
     ELSIF rst = '1' THEN
         q <= '0';
         qn <= '1';
     END IF;
     
   END PROCESS data_in;  -- End of process data_in

END ARCHITECTURE behav;
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity counter_12 is
    port(
        cnt : out std_logic_vector(0 to 11);
        reset : in std_logic;
        enable : in std_logic;
        clk : in std_logic
    );
end counter_12;

architecture counter_12 of counter_12 is
    -- Component declarations
    -- Signal declarations
    signal cdb2vhdl_tmp_1 : std_logic_vector(0 to 11);
    signal XSIG010078 : std_logic;
    signal XSIG010081 : std_logic;
    signal XSIG010083 : std_logic;
    signal XSIG010085 : std_logic;
    signal XSIG010087 : std_logic;
    signal XSIG010101 : std_logic;
    signal XSIG010102 : std_logic;
    signal XSIG010103 : std_logic;
    signal XSIG010104 : std_logic;
    signal XSIG010115 : std_logic;
    signal XSIG010116 : std_logic;
    signal XSIG010117 : std_logic;
    signal XSIG010132 : std_logic;
begin
    -- Signal assignments
    cnt(0) <= cdb2vhdl_tmp_1(0);
    cnt(1) <= cdb2vhdl_tmp_1(1);
    cnt(2) <= cdb2vhdl_tmp_1(2);
    cnt(3) <= cdb2vhdl_tmp_1(3);
    cnt(4) <= cdb2vhdl_tmp_1(4);
    cnt(5) <= cdb2vhdl_tmp_1(5);
    cnt(6) <= cdb2vhdl_tmp_1(6);
    cnt(7) <= cdb2vhdl_tmp_1(7);
    cnt(8) <= cdb2vhdl_tmp_1(8);
    cnt(9) <= cdb2vhdl_tmp_1(9);
    cnt(10) <= cdb2vhdl_tmp_1(10);
    cnt(11) <= cdb2vhdl_tmp_1(11);
    -- Component instances
    XCMP92 : entity work.and2(ideal)
        port map(
            in1 => clk,
            in2 => enable,
            output => XSIG010132
        );
    XCMP93 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => XSIG010132,
            DATA => XSIG010078,
            QN => XSIG010078,
            Q => cdb2vhdl_tmp_1(0),
            RST => reset
        );
    XCMP94 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => cdb2vhdl_tmp_1(0),
            DATA => XSIG010081,
            QN => XSIG010081,
            Q => cdb2vhdl_tmp_1(1),
            RST => reset
        );
    XCMP95 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => cdb2vhdl_tmp_1(1),
            DATA => XSIG010083,
            QN => XSIG010083,
            Q => cdb2vhdl_tmp_1(2),
            RST => reset
        );
    XCMP96 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => cdb2vhdl_tmp_1(2),
            DATA => XSIG010085,
            QN => XSIG010085,
            Q => cdb2vhdl_tmp_1(3),
            RST => reset
        );
    XCMP97 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => cdb2vhdl_tmp_1(3),
            DATA => XSIG010087,
            QN => XSIG010087,
            Q => cdb2vhdl_tmp_1(4),
            RST => reset
        );
    XCMP98 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => cdb2vhdl_tmp_1(4),
            DATA => XSIG010101,
            QN => XSIG010101,
            Q => cdb2vhdl_tmp_1(5),
            RST => reset
        );
    XCMP99 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => cdb2vhdl_tmp_1(5),
            DATA => XSIG010102,
            QN => XSIG010102,
            Q => cdb2vhdl_tmp_1(6),
            RST => reset
        );
    XCMP100 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => cdb2vhdl_tmp_1(6),
            DATA => XSIG010103,
            QN => XSIG010103,
            Q => cdb2vhdl_tmp_1(7),
            RST => reset
        );
    XCMP101 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => cdb2vhdl_tmp_1(7),
            DATA => XSIG010104,
            QN => XSIG010104,
            Q => cdb2vhdl_tmp_1(8),
            RST => reset
        );
    XCMP102 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => cdb2vhdl_tmp_1(8),
            DATA => XSIG010115,
            QN => XSIG010115,
            Q => cdb2vhdl_tmp_1(9),
            RST => reset
        );
    XCMP103 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => cdb2vhdl_tmp_1(9),
            DATA => XSIG010116,
            QN => XSIG010116,
            Q => cdb2vhdl_tmp_1(10),
            RST => reset
        );
    XCMP104 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => cdb2vhdl_tmp_1(10),
            DATA => XSIG010117,
            QN => XSIG010117,
            Q => cdb2vhdl_tmp_1(11),
            RST => reset
        );
end counter_12;
--

-- ideal one bit A/D converter

LIBRARY IEEE;
USE IEEE.math_real.ALL;
USE IEEE.std_logic_1164.ALL;

LIBRARY IEEE_proposed;
USE IEEE_proposed.electrical_systems.ALL;

ENTITY a2d_bit IS
  
  GENERIC (
    thres : real := 2.5);    -- Threshold to determine logic output

  PORT (
    TERMINAL a :     electrical;   -- analog input
    SIGNAL   d : OUT std_logic);   -- digital (std_logic) output
  
END ENTITY a2d_bit;


ARCHITECTURE ideal OF a2d_bit IS

  QUANTITY vin ACROSS a;
  
  BEGIN  -- threshold
-- Process needed to detect threshold crossing and assign output (d)
    PROCESS (vin'ABOVE(thres)) IS
      BEGIN  -- PROCESS
      IF vin'ABOVE(thres) THEN
         d <= '1';
      ELSE
         d <= '0';  
      END IF;
    END PROCESS;

END ideal;


-- Digital clock with 50% duty cycle and enable pin
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY clock_en IS
  GENERIC (
    pw : time);              -- Clock pulse width
  
  PORT (
	enable : IN std_logic ;
	clock_out : INOUT std_logic := '0');
  
END ENTITY clock_en;

ARCHITECTURE ideal OF clock_en IS

BEGIN

-- clock process
    process (clock_out, enable) is
    begin
   	if clock_out = '0' AND enable = '1' THEN
        clock_out <= '1' after pw, '0' after 2*pw;
	end if;
    end process;

END ARCHITECTURE ideal;
--

-- Inverter
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY inverter IS
  GENERIC (
    delay : time := 0 ns);              -- Delay time
  
  PORT (
    input : IN  std_logic;
    output : OUT std_logic);
  
END ENTITY inverter;

ARCHITECTURE ideal OF inverter IS
BEGIN
   output <= NOT input AFTER delay;
END ARCHITECTURE ideal;
--

-- Two input OR gate
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY or2 IS
  GENERIC (
    delay : time := 0 ns);              -- Delay time
  
  PORT (
    in1, in2 : IN  std_logic;
    output    : OUT std_logic);
  
END ENTITY or2;

ARCHITECTURE ideal OF or2 IS
BEGIN
   output <= in1 OR in2 AFTER delay;
END ARCHITECTURE ideal;
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

ENTITY d2a_nbit IS

  GENERIC (
    vmax : real := 5.0;	-- High output
    vmin : real := 0.0;	-- Low output
    high_bit : integer := 9; -- High end of bit range for D/A
	low_bit : integer := 0); -- Low end of bit range for D/A

  PORT (
    SIGNAL   bus_in : IN STD_LOGIC_VECTOR;  -- variable width vector input
    SIGNAL latch : IN STD_LOGIC;
    TERMINAL ana_out  :    electrical);      -- analog output

END ENTITY d2a_nbit ;

ARCHITECTURE behavioral OF d2a_nbit IS

  SIGNAL   sout : real := 0.0;
  QUANTITY vout across iout through ana_out TO electrical_ref;

BEGIN  -- ARCHITECTURE behavioral

  proc : PROCESS

    VARIABLE v_sum  : real; -- Sum of voltage contribution from each bit
    VARIABLE delt_v : real; -- Represents the voltage value of each bit

  BEGIN
	WAIT UNTIL (latch'event and latch = '1'); -- Begin when latch goes high
  	v_sum     := vmin;
    	delt_v    := vmax - vmin;

    FOR i IN high_bit DOWNTO low_bit LOOP -- Perform the conversions
      delt_v  := delt_v / 2.0;
      IF bus_in(i) = '1' OR bus_in(i) = 'H' THEN
        v_sum := v_sum + delt_v;
      END IF;
    END LOOP;

    sout <= v_sum;
  END PROCESS;

  vout == sout'ramp(100.0E-9); -- Ensure continuous transition between levels

END ARCHITECTURE behavioral;


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity pw2ana is
    port(
        terminal ana_out : electrical;
        terminal pw_in : electrical
    );
end pw2ana;

architecture pw2ana of pw2ana is
    -- Component declarations
    -- Signal declarations
    signal bus_servo : std_logic_vector(0 to 11);
    signal XSIG010008 : std_logic;
    signal XSIG010013 : std_logic;
    signal XSIG010019 : std_logic;
    signal XSIG010020 : std_logic;
    signal XSIG010021 : std_logic;
    signal XSIG010022 : std_logic;
begin
    -- Signal assignments
    -- Component instances
    counter_rudder : entity work.counter_12
        port map(
            enable => XSIG010022,
            cnt => bus_servo,
            reset => XSIG010021,
            clk => XSIG010008
        );
    XCMP3 : entity work.a2d_bit(ideal)
        port map(
            D => XSIG010022,
            A => pw_in
        );
    clk_en_rudder : entity work.clock_en(ideal)
        generic map(
            pw => 500 ns
        )
        port map(
            CLOCK_OUT => XSIG010008,
            enable => XSIG010022
        );
    XCMP5 : entity work.inverter(ideal)
        generic map(
            delay => 2 us
        )
        port map(
            input => XSIG010022,
            output => XSIG010013
        );
    XCMP8 : entity work.inverter(ideal)
        generic map(
            delay => 2 us
        )
        port map(
            input => XSIG010020,
            output => XSIG010021
        );
    XCMP9 : entity work.inverter(ideal)
        generic map(
            delay => 2 us
        )
        port map(
            input => XSIG010022,
            output => XSIG010019
        );
    or_rudder : entity work.or2(ideal)
        port map(
            in1 => XSIG010022,
            in2 => XSIG010019,
            output => XSIG010020
        );
    DA1 : entity work.d2a_nbit(behavioral)
        generic map(
            vmax => 4.8,
            high_bit => 9,
            low_bit => 0
        )
        port map(
            bus_in => bus_servo,
            ana_out => ana_out,
            latch => XSIG010013
        );
end pw2ana;
--

-- 12-bit digital comparator model
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

entity dig_cmp is
port
(
      eq : out std_logic := '0';
	  in1 : in std_logic_vector (0 to 11);
	  in2 : in std_logic_vector (0 to 11);
	  latch_in1 : in std_logic := '0'; -- Currently unused
	  latch_in2 : in std_logic := '0';
	  cmp : in std_logic := '0';
	  clk : in std_logic
	  );

end entity dig_cmp ;

architecture simple of dig_cmp is

begin

	compare: PROCESS (latch_in2, cmp, clk) -- Sensitivity list
	variable in2_hold : std_logic_vector (0 to 11) := "000000000000";
  		BEGIN
		if latch_in2 = '1' then -- in2 data is latched and stored
			in2_hold := in2;
		end if;
		if cmp = '1' then
			if in1 = in2_hold then -- latched in2 checked against current in1
				eq <= '0';
			else eq <= '1';
			end if;
		end if;
  	END PROCESS;
end architecture simple;

-- Set/reset flip flop
-- When S goes high, Q is set high until reset
-- When R goes high, Q is set low until set
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity sr_ff is
port
(
      S : in std_logic ;
      R : in std_logic ;
	Q : out std_logic 
);

end sr_ff ;

architecture simple of sr_ff is
begin

 set_reset: PROCESS(S, R) IS  

   BEGIN
--	assert S='1' nand R='1' -- Warning if both inputs are high
--		report "S and R are both active. Use with caution"
--		severity warning;
	if S'event AND S = '1' then
		Q <= '1';
	end if;
	if R'event AND R = '1' then
		Q <= '0';
	end if;     
   END PROCESS set_reset; 

end;
--

LIBRARY IEEE;
USE IEEE.std_logic_1164.all;
USE IEEE.std_logic_arith.all;
LIBRARY IEEE_proposed;
USE IEEE_proposed.electrical_systems.all;
USE IEEE_proposed.mechanical_systems.all;

ENTITY state_mach_rcvr IS
    PORT (
          clk_50 : IN std_logic;
          clk_100k : IN std_logic;
          ser_done : IN std_logic;
          par_det : IN std_logic;
          frm_det : IN std_logic;
          clk_6k : IN std_logic;
          start_pulse : IN std_logic;
          dly_done : IN std_logic;
          s2p_rst : OUT std_logic;
          s2p_en : OUT std_logic;
          cnt1_en : OUT std_logic;
          cnt1_rst : OUT std_logic;
          cmp1_ltch1 : OUT std_logic;
          cmp1_ltch2 : OUT std_logic;
          cnt2_en : OUT std_logic;
          cnt2_rst : OUT std_logic;
          cmp2_ltch1 : OUT std_logic;
          cmp2_ltch2 : OUT std_logic;
          da_latch : OUT std_logic;
          ser_cnt : OUT std_logic;
          dly_cnt : OUT std_logic;
          par_oe : OUT std_logic);

END state_mach_rcvr;

ARCHITECTURE state_diagram OF state_mach_rcvr IS

    ATTRIBUTE ENUM_TYPE_ENCODING: STRING;

    TYPE TYP_state_mach_rcvr_sm1 IS (V_begin, cnt, ch1, rst1, ch2, rst2, cnt_cmp, rst_cnt
           , s_bit, par1, par2);
    SIGNAL CS_state_mach_rcvr_sm1, NS_state_mach_rcvr_sm1 : TYP_state_mach_rcvr_sm1;


BEGIN 

sm1: 
  PROCESS (CS_state_mach_rcvr_sm1, clk_50, frm_det, ser_done, start_pulse, dly_done, par_det) 
  BEGIN

    CASE CS_state_mach_rcvr_sm1 IS
      WHEN V_begin =>
        cnt1_en <= ('0');
        cnt1_rst <= ('1');
        cmp1_ltch1 <= ('0');
        cmp1_ltch2 <= ('0');
        cnt2_en <= ('0');
        cnt2_rst <= ('1');
        cmp2_ltch1 <= ('0');
        cmp2_ltch2 <= ('0');
        s2p_en <= ('1');
        s2p_rst <= ('0');
        da_latch <= ('0');
        ser_cnt <= ('0');
        dly_cnt <= ('0');
        par_oe <= ('0');

        IF ((frm_det = '1')) THEN
          NS_state_mach_rcvr_sm1 <= s_bit;
        ELSE
          NS_state_mach_rcvr_sm1 <= V_begin;
        END IF;

      WHEN cnt =>
        ser_cnt <= ('1');
        cnt1_rst <= ('0');
        cnt2_rst <= ('0');

        IF ((ser_done = '1')) THEN
          NS_state_mach_rcvr_sm1 <= par1;
        ELSE
          NS_state_mach_rcvr_sm1 <= cnt;
        END IF;

      WHEN ch1 =>
        cmp1_ltch2 <= ('1');
        ser_cnt <= ('0');
        dly_cnt <= ('1');

        IF (((start_pulse = '1') AND (dly_done = '1'))) THEN
          NS_state_mach_rcvr_sm1 <= rst1;
        ELSE
          NS_state_mach_rcvr_sm1 <= ch1;
        END IF;

      WHEN rst1 =>
        cmp1_ltch2 <= ('0');
        ser_cnt <= ('1');
        dly_cnt <= ('0');
        par_oe <= ('0');

        IF ((ser_done = '1')) THEN
          NS_state_mach_rcvr_sm1 <= par2;
        ELSE
          NS_state_mach_rcvr_sm1 <= rst1;
        END IF;

      WHEN ch2 =>
        cmp2_ltch2 <= ('1');
        ser_cnt <= ('0');
        da_latch <= ('1');
          NS_state_mach_rcvr_sm1 <= rst2;

      WHEN rst2 =>
        cmp2_ltch2 <= ('0');
        s2p_en <= ('0');
        par_oe <= ('0');
        da_latch <= ('0');
          NS_state_mach_rcvr_sm1 <= cnt_cmp;

      WHEN cnt_cmp =>
        cnt1_en <= ('1');
        cmp1_ltch1 <= ('1');
        cnt2_en <= ('1');
        cmp2_ltch1 <= ('1');
          NS_state_mach_rcvr_sm1 <= rst_cnt;

      WHEN rst_cnt =>
        cnt1_en <= ('0');
        cmp1_ltch1 <= ('0');
        cnt2_en <= ('0');
        cmp2_ltch1 <= ('0');
          NS_state_mach_rcvr_sm1 <= rst_cnt;

      WHEN s_bit =>

        IF ((start_pulse = '1')) THEN
          NS_state_mach_rcvr_sm1 <= cnt;
        ELSE
          NS_state_mach_rcvr_sm1 <= s_bit;
        END IF;

      WHEN par1 =>
        par_oe <= ('1');

        IF ((par_det = '0')) THEN
          NS_state_mach_rcvr_sm1 <= ch1;
        ELSIF ((par_det = '1')) THEN
          NS_state_mach_rcvr_sm1 <= rst1;
        ELSE
          NS_state_mach_rcvr_sm1 <= par1;
        END IF;

      WHEN par2 =>
        par_oe <= ('1');

        IF ((par_det = '0')) THEN
          NS_state_mach_rcvr_sm1 <= ch2;
        ELSIF ((par_det = '1')) THEN
          NS_state_mach_rcvr_sm1 <= rst2;
        ELSE
          NS_state_mach_rcvr_sm1 <= par2;
        END IF;

    END CASE;

  END PROCESS; 

sm1_CTL:
  PROCESS (clk_100k, clk_50)
  BEGIN

    IF (clk_100k'event AND clk_100k='1')  THEN 
        IF (clk_50= '1' ) THEN 
            CS_state_mach_rcvr_sm1 <= V_begin;
        ELSE
            CS_state_mach_rcvr_sm1 <= NS_state_mach_rcvr_sm1;
        END IF;
    END IF;

  END PROCESS;


END state_diagram;
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity sm_cnt_rcvr is
    port(
        cmp1_ltch1 : out std_logic;
        cmp2_ltch1 : out std_logic;
        s2p_en : out std_logic;
        s2p_rst : out std_logic;
        frm_det : in std_logic;
        par_det : in std_logic;
        clk_100k : in std_logic;
        clk_6k : in std_logic;
        clk_50 : in std_logic;
        start_pulse : in std_logic;
        cnt1_en : out std_logic;
        cnt1_rst : out std_logic;
        cmp1_ltch2 : out std_logic;
        cnt2_en : out std_logic;
        cnt2_rst : out std_logic;
        cmp2_ltch2 : out std_logic;
        da_latch : out std_logic;
        par_oe : out std_logic
    );
end sm_cnt_rcvr;

architecture sm_cnt_rcvr of sm_cnt_rcvr is
    -- Component declarations
    -- Signal declarations
    signal ser_cnt : std_logic;
    signal XSIG010002 : std_logic;
    signal XSIG010145 : std_logic;
    signal XSIG010146 : std_logic;
begin
    -- Signal assignments
    -- Component instances
    bit_cnt3 : entity work.bit_cnt(behavioral)
        generic map(
            count => 2
        )
        port map(
            bit_in => XSIG010145,
            clk => clk_6k,
            dly_out => XSIG010146
        );
    bit_cnt4 : entity work.bit_cnt(behavioral)
        generic map(
            count => 10
        )
        port map(
            bit_in => ser_cnt,
            clk => clk_6k,
            dly_out => XSIG010002
        );
    state_mach_rcvr8 : entity work.state_mach_rcvr
        port map(
            clk_100k => clk_100k,
            clk_50 => clk_50,
            s2p_rst => s2p_rst,
            s2p_en => s2p_en,
            cnt1_en => cnt1_en,
            cnt1_rst => cnt1_rst,
            cmp1_ltch1 => cmp1_ltch1,
            cmp1_ltch2 => cmp1_ltch2,
            cnt2_en => cnt2_en,
            cnt2_rst => cnt2_rst,
            cmp2_ltch1 => cmp2_ltch1,
            cmp2_ltch2 => cmp2_ltch2,
            da_latch => da_latch,
            ser_cnt => ser_cnt,
            ser_done => XSIG010002,
            par_det => par_det,
            frm_det => frm_det,
            clk_6k => clk_6k,
            start_pulse => start_pulse,
            dly_done => XSIG010146,
            dly_cnt => XSIG010145,
            par_oe => par_oe
        );
end sm_cnt_rcvr;
--
-- level_set.vhd 
-- Set digital output "level" with parameter "logic_val" (default is '1')

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY level_set IS

  GENERIC (
    logic_val : std_logic := '1');     

  PORT (
    level : OUT std_logic);

END ENTITY level_set;

-- Simple architecture

ARCHITECTURE ideal OF level_set IS

BEGIN

  level <= logic_val;
  
END ARCHITECTURE ideal;

--
-- Serial to parallel data converter

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity ser2par is
port
(
      par_out : inout std_logic_vector(0 to 11) := "ZZZZZZZZZZZZ";
      clk : in std_logic ;
      load_en : in std_logic ;
      ser_in : in std_logic ;
      reset : in std_logic 
);

begin

end ser2par;

architecture a1 of ser2par is
BEGIN 
	sr_sm: PROCESS (load_en, clk, reset, ser_in)	
	BEGIN
		if (reset = '1' and load_en = '1') then
			par_out <= "000000000000";  -- Reset the parallel data out
			
		elsif  (reset = '0' and load_en = '1') then
			if (clk'event and clk = '1') then 
				
				-- The register will shift when load is enabled 
				-- and will shift at rising edge of clock
				
				par_out(0) <= ser_in; -- Input data shifts into bit 0
				par_out(1) <= par_out(0);
				par_out(2) <= par_out(1);
				par_out(3) <= par_out(2);
				par_out(4) <= par_out(3);
				par_out(5) <= par_out(4);
				par_out(6) <= par_out(5);
				par_out(7) <= par_out(6);
				par_out(8) <= par_out(7);
				par_out(9) <= par_out(8);
				par_out(10) <= par_out(9);
				par_out(11) <= par_out(10); 

			end if;

		else
			par_out <= "ZZZZZZZZZZZZ"; -- No change in output. Tri-state if load_en = 0.		
		end if;			
	END PROCESS;
end;

--
-- This model ouputs a '1' when a specific bit pattern is encountered
-- Otherwise, it outputs a zero

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity frame_det is
port
(
	  bus_in : in std_logic_vector (0 to 11);
	  clk : in std_logic;
	  frm_bit : out std_logic := '0' -- Initialize output to zero
	  );

end entity frame_det;

architecture simple of frame_det is
begin
	enbl: PROCESS (bus_in, clk) -- Sensitivity list
  		BEGIN 
		if bus_in = "010101010101" then -- This is the pre-defined bit pattern
			if clk'event AND clk = '0' then -- Output updated synchronously
				frm_bit <= '1';
			end if;
		else frm_bit <= '0';
		end if;
  	END PROCESS;
end architecture simple;

--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity parity_det is
    port(
        bus_in : in std_logic_vector(0 to 11);
        par_bit : out std_logic;
        oe : in std_logic
    );
end parity_det;

architecture parity_det of parity_det is
    -- Component declarations
    -- Signal declarations
    signal XSIG010010 : std_logic;
    signal XSIG010011 : std_logic;
    signal XSIG010012 : std_logic;
    signal XSIG010013 : std_logic;
    signal XSIG010014 : std_logic;
    signal XSIG010015 : std_logic;
    signal XSIG010016 : std_logic;
    signal XSIG010017 : std_logic;
    signal XSIG010019 : std_logic;
    signal XSIG010057 : std_logic;
begin
    -- Signal assignments
    -- Component instances
    XCMP1 : entity work.xor2(ideal)
        port map(
            in1 => bus_in(1),
            in2 => bus_in(2),
            output => XSIG010010
        );
    XCMP2 : entity work.xor2(ideal)
        port map(
            in1 => bus_in(3),
            in2 => bus_in(4),
            output => XSIG010011
        );
    XCMP3 : entity work.xor2(ideal)
        port map(
            in1 => bus_in(5),
            in2 => bus_in(6),
            output => XSIG010012
        );
    XCMP4 : entity work.xor2(ideal)
        port map(
            in1 => bus_in(7),
            in2 => bus_in(8),
            output => XSIG010013
        );
    XCMP5 : entity work.xor2(ideal)
        port map(
            in1 => bus_in(9),
            in2 => bus_in(10),
            output => XSIG010016
        );
    XCMP6 : entity work.xor2(ideal)
        port map(
            in1 => XSIG010010,
            in2 => XSIG010011,
            output => XSIG010014
        );
    XCMP7 : entity work.xor2(ideal)
        port map(
            in1 => XSIG010012,
            in2 => XSIG010013,
            output => XSIG010015
        );
    XCMP8 : entity work.xor2(ideal)
        port map(
            in1 => XSIG010014,
            in2 => XSIG010015,
            output => XSIG010017
        );
    XCMP9 : entity work.xor2(ideal)
        port map(
            in1 => XSIG010017,
            in2 => XSIG010016,
            output => XSIG010019
        );
    XCMP10 : entity work.xor2(ideal)
        port map(
            in1 => XSIG010019,
            in2 => bus_in(0),
            output => XSIG010057
        );
    XCMP12 : entity work.and2(ideal)
        port map(
            in1 => oe,
            in2 => XSIG010057,
            output => par_bit
        );
end parity_det;
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity TDM_Demux_dbg is
    port(
        s2p_en : in std_logic;
        tdm_in : in std_logic;
        clk_6k : in std_logic;
        s2p_rst : in std_logic;
        par_det : out std_logic;
        frm_det : out std_logic;
        da_latch : in std_logic;
        par_oe : in std_logic;
        data_bus : out std_logic_vector(1 to 10);
        start_bit : out std_logic
    );
end TDM_Demux_dbg;

architecture TDM_Demux_dbg of TDM_Demux_dbg is
    -- Component declarations
    -- Signal declarations
    terminal d2a_out : electrical;
    signal rcvr_bus : std_logic_vector(0 to 11);
begin
    -- Signal assignments
    data_bus(1) <= rcvr_bus(1);
    data_bus(2) <= rcvr_bus(2);
    data_bus(3) <= rcvr_bus(3);
    data_bus(4) <= rcvr_bus(4);
    data_bus(5) <= rcvr_bus(5);
    data_bus(6) <= rcvr_bus(6);
    data_bus(7) <= rcvr_bus(7);
    data_bus(8) <= rcvr_bus(8);
    data_bus(9) <= rcvr_bus(9);
    data_bus(10) <= rcvr_bus(10);
    start_bit <= rcvr_bus(0);
    -- Component instances
    s2p1 : entity work.ser2par(a1)
        port map(
            par_out => rcvr_bus,
            clk => clk_6k,
            load_en => s2p_en,
            ser_in => tdm_in,
            reset => s2p_rst
        );
    frm_det1 : entity work.frame_det(simple)
        port map(
            bus_in => rcvr_bus,
            frm_bit => frm_det,
            clk => clk_6k
        );
    par_det1 : entity work.parity_det
        port map(
            bus_in => rcvr_bus,
            par_bit => par_det,
            oe => par_oe
        );
    XCMP113 : entity work.d2a_nbit(behavioral)
        generic map(
            low_bit => 1,
            high_bit => 10,
            vmax => 4.8
        )
        port map(
            bus_in => rcvr_bus(1 to 10),
            ana_out => d2a_out,
            latch => da_latch
        );
end TDM_Demux_dbg;
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity Decode_PW is
    port(
        bit_stream_in : in std_logic;
        terminal ch1_pw : electrical;
        terminal ch2_pw : electrical
    );
end Decode_PW;

architecture Decode_PW of Decode_PW is
    -- Component declarations
    -- Signal declarations
    signal cmp_bus : std_logic_vector(0 to 11);
    signal cnt1 : std_logic_vector(0 to 11);
    signal cnt2 : std_logic_vector(0 to 11);
    signal rud_clk : std_logic;
    signal rud_cmp : std_logic;
    signal rud_eq : std_logic;
    signal rud_ff_rst : std_logic;
    signal rud_ff_set : std_logic;
    signal rud_ltch1 : std_logic;
    signal rud_ltch2 : std_logic;
    signal XSIG010225 : std_logic;
    signal XSIG010228 : std_logic;
    signal XSIG010229 : std_logic;
    signal XSIG010256 : std_logic;
    signal XSIG010266 : std_logic;
    signal XSIG010267 : std_logic;
    signal XSIG010268 : std_logic;
    signal XSIG010289 : std_logic;
    signal XSIG010315 : std_logic;
    signal XSIG010339 : std_logic;
    signal XSIG010357 : std_logic;
    signal XSIG010371 : std_logic;
    signal XSIG010373 : std_logic;
    signal XSIG010383 : std_logic;
    signal XSIG010384 : std_logic;
    signal XSIG010385 : std_logic;
    signal XSIG010386 : std_logic;
    signal XSIG010390 : std_logic;
    signal XSIG010433 : std_logic;
begin
    -- Signal assignments
    -- Component instances
    cntr1 : entity work.counter_12
        port map(
            enable => XSIG010384,
            cnt => cnt1,
            reset => XSIG010357,
            clk => XSIG010433
        );
    cntr2 : entity work.counter_12
        port map(
            enable => rud_cmp,
            cnt => cnt2,
            reset => XSIG010385,
            clk => rud_clk
        );
    cmp1 : entity work.dig_cmp(simple)
        port map(
            in1 => cnt1,
            eq => XSIG010371,
            clk => XSIG010433,
            in2 => cmp_bus,
            cmp => XSIG010384,
            latch_in1 => XSIG010256,
            latch_in2 => XSIG010383
        );
    cmp2 : entity work.dig_cmp(simple)
        port map(
            in1 => cnt2,
            eq => rud_eq,
            clk => rud_clk,
            in2 => cmp_bus,
            cmp => rud_cmp,
            latch_in1 => rud_ltch1,
            latch_in2 => rud_ltch2
        );
    clk_1M2 : entity work.clock_en(ideal)
        generic map(
            pw => 500 ns
        )
        port map(
            CLOCK_OUT => rud_clk,
            enable => rud_cmp
        );
    clk_1M1 : entity work.clock_en(ideal)
        generic map(
            pw => 500 ns
        )
        port map(
            CLOCK_OUT => XSIG010433,
            enable => XSIG010384
        );
    XCMP134 : entity work.d2a_bit(ideal)
        port map(
            D => XSIG010371,
            A => ch1_pw
        );
    XCMP135 : entity work.d2a_bit(ideal)
        port map(
            D => rud_eq,
            A => ch2_pw
        );
    XCMP137 : entity work.SR_FF(simple)
        port map(
            S => rud_ff_set,
            R => rud_ff_rst,
            Q => rud_cmp
        );
    XCMP138 : entity work.inverter(ideal)
        port map(
            input => rud_eq,
            output => rud_ff_rst
        );
    XCMP139 : entity work.SR_FF(simple)
        port map(
            S => XSIG010373,
            R => XSIG010339,
            Q => XSIG010384
        );
    XCMP140 : entity work.inverter(ideal)
        port map(
            input => XSIG010371,
            output => XSIG010339
        );
    rc_clk2 : entity work.rc_clk
        port map(
            clk_50 => XSIG010289,
            clk_6K => XSIG010225,
            clk_100k => XSIG010315
        );
    sm_rcvr1 : entity work.sm_cnt_rcvr
        port map(
            cnt1_en => XSIG010373,
            cmp1_ltch1 => XSIG010256,
            cnt2_rst => XSIG010385,
            clk_100k => XSIG010315,
            cnt1_rst => XSIG010357,
            cnt2_en => rud_ff_set,
            cmp2_ltch1 => rud_ltch1,
            frm_det => XSIG010229,
            par_det => XSIG010228,
            s2p_en => XSIG010266,
            s2p_rst => XSIG010267,
            clk_6k => XSIG010225,
            clk_50 => XSIG010289,
            da_latch => XSIG010268,
            cmp1_ltch2 => XSIG010383,
            cmp2_ltch2 => rud_ltch2,
            start_pulse => XSIG010390,
            par_oe => XSIG010386
        );
    XCMP155 : entity work.level_set(ideal)
        generic map(
            logic_val => '0'
        )
        port map(
            level => cmp_bus(11)
        );
    XCMP157 : entity work.TDM_Demux_dbg
        port map(
            data_bus => cmp_bus(0 to 9),
            tdm_in => bit_stream_in,
            clk_6k => XSIG010225,
            s2p_en => XSIG010266,
            s2p_rst => XSIG010267,
            da_latch => XSIG010268,
            frm_det => XSIG010229,
            par_det => XSIG010228,
            par_oe => XSIG010386,
            start_bit => XSIG010390
        );
    XCMP172 : entity work.level_set(ideal)
        generic map(
            logic_val => '1'
        )
        port map(
            level => cmp_bus(10)
        );
end Decode_PW;
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity tb_CS1 is
end tb_CS1;

architecture TB_CS1 of tb_CS1 is
    -- Component declarations
    -- Signal declarations
    terminal rudder : electrical;
    terminal rudder_out : electrical;
    terminal rudder_servo : electrical;
    signal tdm_stream2 : std_logic;
    terminal throttle : electrical;
    terminal throttle_out : electrical;
    terminal throttle_servo : electrical;
begin
    -- Signal assignments
    -- Component instances
    Digitize_Encode1 : entity work.Digitize_Encode
        port map(
            ch2_in => rudder,
            ch1_in => throttle,
            tdm_out => tdm_stream2
        );
    throttle_1 : entity work.stick(ideal)
        generic map(
            freq => 1.0,
            amplitude => 2.397,
            phase => 0.0,
            offset => 2.397
        )
        port map(
            v_out => throttle
        );
    rudder_1 : entity work.stick(ideal)
        generic map(
            offset => 2.397,
            phase => 90.0,
            amplitude => 2.397,
            freq => 1.0
        )
        port map(
            v_out => rudder
        );
    pw2ana1 : entity work.pw2ana
        port map(
            ana_out => throttle_out,
            pw_in => throttle_servo
        );
    pw2ana2 : entity work.pw2ana
        port map(
            ana_out => rudder_out,
            pw_in => rudder_servo
        );
    Decode_PW10 : entity work.Decode_PW
        port map(
            bit_stream_in => tdm_stream2,
            ch2_pw => rudder_servo,
            ch1_pw => throttle_servo
        );
end TB_CS1;
--
