library IEEE;
use			IEEE.STD_LOGIC_1164.all;
package sortnet_tb is
  generic (
    DATA_BITS			: positive;
    INPUTS				: positive
    );

  subtype T_DATA	is std_logic_vector(DATA_BITS - 1 downto 0);
  type T_DATA_VECTOR	is array(natural range <>) of T_DATA;

  type T_SCOREBOARD_DATA is record
    Data  : T_DATA_VECTOR(INPUTS - 1 downto 0);
  end record;
end sortnet_tb;

entity repro1 is
end repro1;

architecture behav of repro1 is
  package tb is new work.sortnet_tb generic map (3, 4);
begin
end behav;
