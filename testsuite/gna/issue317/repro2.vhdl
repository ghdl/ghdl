package repro2_scbd is
  generic (type t);
end repro2_scbd;

library IEEE;
use			IEEE.STD_LOGIC_1164.all;
package repro2_sortnet_tb is
  generic (
    DATA_BITS			: positive;
    INPUTS				: positive
    );

  subtype T_DATA	is std_logic_vector(DATA_BITS - 1 downto 0);
  type T_DATA_VECTOR	is array(natural range <>) of T_DATA;

  type T_SCOREBOARD_DATA is record
    Data  : T_DATA_VECTOR(INPUTS - 1 downto 0);
  end record;

  package scbd is new work.repro2_scbd generic map (t => t_scoreboard_data);
end repro2_sortnet_tb;

entity repro2 is
end repro2;

architecture behav of repro2 is
  package tb is new work.repro2_sortnet_tb generic map (3, 4);
begin
end behav;
