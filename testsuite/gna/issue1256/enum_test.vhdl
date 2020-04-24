library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package pkg_enum is
  type State is (A,B,C,D,E);
end pkg_enum;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.all;
use work.pkg_enum.all;

entity enum_test is
    port(enum_in : in State;
         enum_out : out State;
         enum_decoded: out unsigned(2 downto 0)
     );
end enum_test;

architecture arch of enum_test is
begin
    with enum_in select
        enum_decoded <= "000" when A,
                        "001" when B,
                        "010" when C,
                        "011" when D,
                        "100" when others;
    
    enum_out <= enum_in;
end arch;
