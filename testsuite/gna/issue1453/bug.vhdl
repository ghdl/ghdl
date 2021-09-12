library ieee;
use ieee.numeric_std.all;
package bug is
    subtype byte is unsigned (7 downto 0);
    subtype word is unsigned (15 downto 0);
    function high_byte(w: word) return byte;
end bug;
package body bug is
    function high_byte(w: word) return byte is
    begin
        return (7 downto 0 => w(15 downto 8));
    end;
end bug;
