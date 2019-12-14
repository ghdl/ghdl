library ieee;
use ieee.std_logic_1164.all;

use work.test_pkg.all;

entity test is
end entity;

architecture a of test is
begin

  process
    variable rec : record_t(data(7 downto 0));
  begin
    test_procedure(rec);
    report to_string(rec.data);
    wait;
  end process;

end architecture;
