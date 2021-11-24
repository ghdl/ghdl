library IEEE;
use IEEE.std_logic_1164.all;

entity tb_delayline1d is
    generic (
       delay : natural := 10
    );
end entity;

architecture rtl of tb_delayline1d is
    signal clk : std_logic := '0';
    signal i, o : std_logic;
begin

  -- 100 Mhz clock
  process
  begin
    for k in 1 to 5 loop
      clk <= not clk;
      wait for 5 ns;
    end loop;
    wait;
  end process;

    -- Device Under Test
    c0: entity work.delayline1d(rtl)
        generic map (
            delay => delay
        )
        port map (
            clk => clk,
            i => i,
            o => o
        );
end architecture;

