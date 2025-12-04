library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;

entity reduce01 is
end;

architecture arch of reduce01 is
begin
  process
    variable v0 : std_logic_vector (3 downto 0) := x"5";
  begin
    assert and_reduce(v0) = '0' severity failure;
    assert nand_reduce(v0) = '1' severity failure;

    assert or_reduce(v0) = '1' severity failure;
    assert nor_reduce(v0) = '0' severity failure;

    assert xor_reduce(v0) = '0' severity failure;
    assert xnor_reduce(v0) = '1' severity failure;

    wait;
  end process;
end;
