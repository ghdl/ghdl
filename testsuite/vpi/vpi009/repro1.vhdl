library ieee;
use ieee.std_logic_1164.all;

entity repro1 is
end;

architecture rtl of repro1 is
  signal q : std_logic;
begin
  -- process
  -- begin
  --   q <= '0';
  --   wait for 40 ns;
  --   q <= '1';
  --   wait for 40 ns;
  --   q <= '0';
  --   wait;
  -- end process;
  q <= '0', '1' after 40 ns, '0' after 80 ns;

  process
  begin
    for i in 0 to 10 loop
      report "q=" & std_logic'image(q);
      wait for 10 ns;
    end loop;
    wait;
  end process;
end rtl;
