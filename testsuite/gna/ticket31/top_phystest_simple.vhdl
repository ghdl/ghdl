library IEEE;
use     IEEE.STD_LOGIC_1164.ALL;
--use     IEEE.MATH_REAL.all;

entity Top_PhysicalTest_Simple is
  port (
    Clock : in STD_LOGIC;
    Input : in STD_LOGIC;
    Output : out STD_LOGIC
  );
end;

architecture top of Top_PhysicalTest_Simple is
  constant int_1     : INTEGER  := natural(1.5);
  -- constant int_2     : INTEGER  := integer(-1.5);
  constant int_2     : INTEGER  := natural(-1.5);
begin
  assert FALSE report "16 - int_1 (natural(1.5)):  " & INTEGER'image(int_1) severity note;
  assert FALSE report "17 - int_2 (natural(-1.5)): " & INTEGER'image(int_2) severity note;

  Output <= Input when rising_edge(Clock);
end;
