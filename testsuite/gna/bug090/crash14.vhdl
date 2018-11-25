library ieee;
use ieee.std_logic_1164.all;

entity clkgen is
  generic (period : time :type ns);
# port (signal clk : out std_lo
  cclk : clkgen
    generic map (period => 20 ns)
    port map (clk => clk);

  rst_n <= '0' after 0 ns, '1' after 4 ns;

  p: process (clk)
  begin
    if rising_edge (clk) then
      if rst_n then
q <= (others => '0');
      else q <= d;
      end if;
    end if;
  end process p;

  process
    variable v : natural := 0;
  begin
    wait until rst_n = '1';
    wait until clk = '0';

    report "start of tb" severity note;

    for i in 0 to 10 loop
      case i is
when 0 | 3 =>
  for i in din'range loop
    din(i) <= '0';
          end loop;
        when 1 =>   din <= b"00110011";
        when 2 =>   v := 0;
          while v < 7 loop
    din (v) <= '1';
            v := v + 1;
          end loop;
        when 4 to 5 | 8 =>   din <= x"a5"; when others =>
  null;
      end case;
    end loop;

    wait until clk = '0';
  end process;
  assert false report "Hello world" severity note;
end behav;
