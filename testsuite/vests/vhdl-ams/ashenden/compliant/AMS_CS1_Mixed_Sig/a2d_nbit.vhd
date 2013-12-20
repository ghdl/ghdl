
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

library ieee;  use ieee.std_logic_1164.all;
library ieee_proposed;  use ieee_proposed.electrical_systems.all;

entity a2d_nbit is
  port ( signal start : in std_ulogic; 	                     -- Start signal
         signal clk : in std_ulogic;   	                     -- Strobe clock
         terminal ain : electrical;  	                     -- Analog input terminal
         signal eoc : out std_ulogic := '0';                 -- End of conversion pin
         signal dout : out std_ulogic_vector(9 downto 0) );  -- Digital output signal
end entity a2d_nbit;

----------------------------------------------------------------

architecture sar of a2d_nbit is
  
  constant Vmax : real := 5.0;   	-- ADC's maximum range
  constant delay : time := 10 us;	-- ADC's conversion time

  type states is (input, convert); 	-- Two states of A2D Conversion
  constant bit_range : integer := 9; 	-- Bit range for dtmp and dout
  
  quantity Vin across Iin through ain to electrical_ref;     -- ADC's input branch

begin

  sa_adc: process is

    variable thresh : real := Vmax;  -- Threshold to test input voltage against
    variable Vtmp : real := Vin;     -- Snapshot of input voltage
                                     -- when conversion starts
    variable dtmp : std_ulogic_vector(bit_range downto 0);  -- Temp. output data
    variable status : states := input;                      -- Begin with "input" case 
    variable bit_cnt : integer := bit_range;

  begin
    case status is
      when input => 			-- Read input voltages when start goes high
        wait on start until start = '1' or start = 'H';
        bit_cnt := bit_range;           -- Reset bit_cnt for conversion
        thresh := Vmax;
        Vtmp := Vin;                    -- Variable to hold input comparison voltage
        eoc <= '0';                     -- Reset end of conversion
        status := convert;              -- Go to convert state
      when convert => 			-- Begin successive approximation conversion
        wait on clk until clk = '1' or clk = 'H';
        thresh := thresh / 2.0;         -- Get value of MSB
        if Vtmp > thresh then
          dtmp(bit_cnt) := '1';         -- Store '1' in dtmp variable vector
          Vtmp := Vtmp - thresh;	-- Prepare for next comparison
        else
          dtmp(bit_cnt) := '0';         -- Store '0' in dtmp variable vector
        end if;
        if bit_cnt > 0 then
          bit_cnt := bit_cnt - 1;       -- Decrement the bit count
        else
          dout <= dtmp;	                -- Put contents of dtmp on output pins
          eoc <= '1' after delay;       -- Signal end of conversion
          status := input; 		-- Go to input state
        end if;
    end case;
  end process sa_adc;

  Iin == 0.0;  -- Ideal input draws no current

end architecture sar;
