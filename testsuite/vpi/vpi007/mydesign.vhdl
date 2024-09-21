library ieee ;
use ieee.std_logic_1164.all;

entity myentity is
  port (
    out1 : out integer := 0
    );
end myentity;

architecture arch of myentity is
begin
  process
  begin
    wait for 1 ms;
    out1 <= 17;
  end process;
end arch;
