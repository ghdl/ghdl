library ieee;
use ieee.std_logic_1164.all;

entity slice07 is
    port (clk : std_ulogic);
end;

architecture rtl of slice07 is
  signal sidx : natural range 0 to 0 := 0;
begin
  process(clk)
    variable vmem : std_ulogic_vector(7 downto 0);
    variable j : integer;
  begin
    if rising_edge(clk) then
      j := sidx * 8;
      vmem(j + 7 downto j) := x"ba";
    end if;
  end process;
end;
