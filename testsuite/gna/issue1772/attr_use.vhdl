entity use1772 is end entity;

use work.attr_pkg.all;

architecture a of use1772 is
begin
  process
    constant info : pin_bit_info := tconv'pin_bit_information;
  begin
    report "elements: " & integer'image(info'length);
    wait;
  end process;
end architecture;
