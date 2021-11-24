library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_delayline is
    generic (
       delay : natural := 10
    );
end entity;

architecture rtl of tb_delayline is
    signal clk : std_logic := '0';
    signal i, o : std_logic_vector(7 downto 0);
begin

  -- 100 Mhz clock
  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 5 ns;
      clk <= '1';
      wait for 5 ns;
    end pulse;
  begin
    for j in 1 to 9 loop
      i <= std_logic_vector(to_unsigned (j + 16 * (15 - j), 8));
      pulse;
    end loop;

    i <= x"f0";
    pulse;
    i <= x"1e";
    for j in 1 to 9 loop
      pulse;
      assert o = std_logic_vector(to_unsigned (j + 16 * (15 - j), 8))
        severity failure;
    end loop;
    pulse;
    assert o = x"f0" severity failure;
    report "Test is ok";
    wait;
  end process;

    -- Device Under Test
    c0: entity work.delayline(rtl)
        generic map (
            delay => delay
        )
        port map (
            clk => clk,
            i => i,
            o => o
        );
end architecture;

