library ieee;
use ieee.std_logic_1164.all;
use work.view03_pkg.all;

entity view03 is
  port (
    clk : std_logic;
    rst : std_logic;
    b : view bidir_master_view);
end;

architecture rtl of view03 is
begin
  process (clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        b.tx.valid <= '0';
        b.rx.ready <= '1';
      else
        if b.rx.ready = '1' and b.rx.valid = '1' then
          b.rx.ready <= '0';
          b.tx.data <= not b.rx.data;
          b.tx.valid <= '1';
        end if;
        if b.tx.valid = '1' and b.tx.ready = '1' then
          b.tx.valid <= '0';
          b.rx.ready <= '1';
        end if;
      end if;
    end if;
  end process;
end architecture rtl;
