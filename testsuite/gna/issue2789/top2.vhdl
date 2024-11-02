use work.pkg2;

package top2_pkg is
    -- reduced to a single std_logic for exposition
    type my_type is record
        sig: bit;
    end record;

    -- instantiating the generic package
    package my_pkg is new pkg2 generic map(my_type, 16);
end package;

use work.top2_pkg.all;

entity top2 is end;

architecture tb of top2 is
    -- trying to create a signal here
    signal bitpacked: my_pkg.type_with_bits;
begin
end;
