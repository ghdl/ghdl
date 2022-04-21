library ieee;
use ieee.std_logic_1164.all;

entity mymodule is
  port (
    clk: in std_logic;
    reset: in std_logic;
    i: in std_logic_vector(1 downto 0);
    o: out std_logic
  );
end mymodule;

architecture rtl of mymodule is
signal o_next: std_logic;
begin
process(i) begin
  o_next <= '0';
  case i is
    when "00" => o_next <= '0';
    when others => o_next <= '1';
  end case;
end process;

process(clk, reset) begin
  if rising_edge(clk) then
    if reset = '1' then
      o <= '0';
    else
      o <= o_next;
    end if;
  end if;
end process;
end rtl ;
