library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity tb is
  -- empty
end entity;

architecture test_logic of tb is
  constant COUNTER_BITS : NATURAL := 8;
  signal res_n, start, stop : STD_LOGIC;
begin

  process
  begin
    res_n <= '0';
    stop <= '0';

    wait for 20 ns;

    res_n <= '1';
    wait for 10 us;

    stop <= '1';
    res_n <= '0';
    wait;
  end process;

  uut: entity work.top
    generic map ( COUNTER_BITS => COUNTER_BITS );

end architecture;
