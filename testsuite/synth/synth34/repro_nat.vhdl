library ieee;
use ieee.std_logic_1164.all;

entity sub_nat is
  port (
    clk : in std_logic;
    a : in natural;
    b : out natural
  );
end sub_nat;

architecture rtl of sub_nat is
begin
  process(clk)
  begin
    if rising_edge(clk) then
      b <= a;
    end if;
  end process;
end rtl;


library ieee;
use ieee.std_logic_1164.all;

entity repro_nat is
  port (
    clk : in std_logic;
    a : in natural;
    b : out natural
  );
end repro_nat;

architecture rtl of repro_nat is
begin
  i_sub_nat : entity work.sub_nat
  port map (
    clk => clk,
    a => a,
    b => b
  );
end rtl;
