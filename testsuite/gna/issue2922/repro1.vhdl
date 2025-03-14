library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro1 is
end;

architecture behaviour of repro1
is
  constant ADDR_PHY_RD  : std_logic_vector(15 downto 0) := x"C028";
  subtype AR_CSR is natural range 9 downto 2;
begin
  bit_extract_process : process
  begin
    report "(ADDR_PHY_RD(AR_CSR)) = " & integer'image(to_integer(unsigned(std_logic_vector(ADDR_PHY_RD(AR_CSR))))); -- THIS LINE CRASHES GHDL
    wait;
  end process;

end behaviour;
