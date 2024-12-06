library ieee;
use ieee.std_logic_1164.all;

entity tb_test is
end entity;

architecture arch of tb_test is

  signal x : integer range 0 to 9;
  signal y : integer range 0 to 10;
  signal r, s : std_ulogic;

begin

  process
  begin
    for ix in 0 to 9 loop
      x <= ix;
      for iy in 0 to 10 loop
        y <= iy;
        wait for 10 ns;
        if x = y then
          assert r = '1' severity failure;
        else
          assert r = '0' severity failure;
        end if;
      end loop;
    end loop;
    wait;
  end process;

  inst: entity work.test
  port map (
    x => x,
    y => y,
    o => r
  );

end architecture;
