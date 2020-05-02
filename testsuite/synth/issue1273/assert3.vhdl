library ieee;
use ieee.std_logic_1164.all;

entity assert3 is
  port (v : std_logic_Vector (7 downto 0);
        en : std_logic;
        res : out natural);
end;

architecture behav of assert3 is
begin
  process (v, en)
  begin
    res <= 0;
    if en = '1' then
      for i in v'range loop
        if v (i) = '1' then
          res <= i;
          exit;
        end if;
        assert i > 3 report "bad v value";
      end loop;
    end if;
  end process;
end behav;

