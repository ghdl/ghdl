library ieee;
use ieee.std_logic_1164.all;

entity A is
  port (
    clk : in std_logic;
    rst : in std_logic;
    b : inout std_logic := 'Z'
  );
end A;

architecture rtl of A is
  signal c : std_logic := '1';
begin
  PROC : process(clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        c <= '1';
      else
        c <= not c;

        if c = '0' then
          b <= '0';
        else
          b <= 'Z';
        end if;
      end if;
    end if;
  end process;
end architecture;
