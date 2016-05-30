entity test is
end entity test;

library ieee;
use ieee.std_logic_1164.all;

architecture test of test is

begin

  p: process is

    subtype xa_t is std_ulogic_vector(3 downto 0);
    subtype ya_t is std_ulogic_vector(1 downto 0);

    procedure x(constant a : xa_t) is
    begin
      for i in a'range loop
        report "x.a(" & integer'image(i) & "): " & std_ulogic'image(a(i)) severity note;
      end loop;
    end procedure x;

    procedure y(constant a : in ya_t) is
    begin
      x(a(1 downto 0) => a,
        a(3 downto 2) => (others => '0'));
    end procedure y;

  begin
    y(a => "11");
    wait;
  end process p;

end architecture test;
