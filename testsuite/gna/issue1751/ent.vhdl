library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent is
end entity ent;

architecture beh of ent is
  signal sig_1 : std_logic;
  alias a_sig_1 is sig_1;
begin
  process
  begin
    a_sig_1 <= force '1';
    a_sig_1 <= release;
    wait;
  end process;
end architecture beh;
