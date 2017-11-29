package DummyPackage is
  constant FISH: integer := 4;
  function STICKS return natural;
  constant IM_TOTALLY_NOT_NEGATIVE: natural := FISH - STICKS;
end package;

package body DummyPackage is
  function STICKS return natural is
  begin
    return 5;
  end function;
end package body;

library ieee;
use ieee.std_logic_1164.all;
use work.DummyPackage.all;

entity DummyEntity is
port (
  signal i_data: in std_logic_vector(IM_TOTALLY_NOT_NEGATIVE-1 downto 0);                                              
  signal o_data: out std_logic_vector(IM_TOTALLY_NOT_NEGATIVE-1 downto 0)                                              
);
end entity;

architecture arch of DummyEntity is
begin
  o_data <= i_data;
end architecture;
