library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

package types is
    subtype test_sub_t is std_logic_vector(3 downto 0);
    type test_array_t is array (integer range <>) of test_sub_t;
end package types;

------------------------------------------------------------------------------

library work;
use work.types.all;

entity ent is
    port (
        test_in: in test_array_t(0 to 0)
    );
end entity ent;

architecture rtl of ent is
begin
end architecture rtl;

--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.types.all;
use work.ent;

entity top is
end entity top;

architecture rtl of top is
    signal a: signed(1 downto 0) := "00";
begin

    ent_0: entity ent
        port map (
            test_in(0) => test_sub_t(a & "00")
        );

end architecture rtl;
