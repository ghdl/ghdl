library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.protocol_pkg.T_ARRAY;

entity Node is
  generic (
    COUNTER_BITS : NATURAL;
    N_NODES : NATURAL );
  port (
    other_ctrs : in T_ARRAY(0 to N_NODES-2)(COUNTER_BITS-1 downto 0);
    count : out STD_LOGIC_VECTOR(COUNTER_BITS-1 downto 0));
end entity;

architecture impl of Node is
begin
end architecture;
