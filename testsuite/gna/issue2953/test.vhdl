library ieee;
use ieee.std_logic_1164.all;

entity test is
  port (
    x: std_logic
  );
end;

architecture rtl of test is
begin
  process (all)
    variable y: std_logic;
  begin
    with x select y :=
      '-' when others;
  end process;
end;
