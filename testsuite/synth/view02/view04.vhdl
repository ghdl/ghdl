library ieee;
use ieee.std_logic_1164.all;
use work.view04_pkg.all;

entity view04 is
  port (
    clk : std_logic;
    rst : std_logic;
    b : view bus_slave_view);
end;

architecture rtl of view04 is
begin
  process (clk)
    variable data : std_logic_vector(7 downto 0);
  begin
    if rising_edge(clk) then
      if rst = '1' then
        b.tx(1).ready <= '1';
        b.tx(2).ready <= '1';
        b.rx.valid <= '0';
        data := (others => '0');
      else
        if b.tx(1).valid = '1' and b.tx(1).ready = '1' then
          data := data xor b.tx(1).data;
          b.tx(1).ready <= '0';
        end if;
        if b.tx(2).valid = '1' and b.tx(2).ready = '1' then
          data := data xor b.tx(2).data;
          b.tx(2).ready <= '0';
        end if;
        if b.tx(1).ready = '0' and b.tx(2).ready = '0' then
          b.rx.data <= data;
          b.rx.valid <= '1';
        end if;
        if b.rx.ready = '1' and b.rx.valid = '1' then
          data := (others => '0');
          b.rx.valid <= '0';
          b.tx(1).ready <= '1';
          b.tx(2).ready <= '1';
        end if;
      end if;
    end if;
  end process;
end architecture rtl;
