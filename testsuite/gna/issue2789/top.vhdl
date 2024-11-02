library IEEE;
    use IEEE.std_logic_1164.all;

use work.pkg;

package top_pkg is
    -- reduced to a single std_logic for exposition
    type my_type is record
        sig: std_logic;
    end record;

    -- instantiating the generic package
    package my_pkg is new pkg generic map(my_type, 16);
end package;

library IEEE;
    use IEEE.std_logic_1164.all;

use work.top_pkg.all;

entity top is end;

architecture tb of top is
    -- trying to create a signal here
    signal bitpacked: my_pkg.type_with_bits;
begin
end;
