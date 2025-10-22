library ieee;
use ieee.std_logic_1164.all;

entity A is
end A;

architecture sim of A is
  type b is array (natural range <>) of std_logic_vector(20 downto 0);
begin

  process
    procedure d(c : b) is
    begin
    end procedure;
  begin
    wait for 10 ns;
    d(("0" & x"00000"));
    -- d((0 => ("0" & x"00000")));
    wait for 10 ns;
    wait;
  end process;
end architecture;
