library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;

use std.textio.all;

package sim_ram_pkg is

    subtype lol is text;
    
    type sim_ram is record
        fid: lol;
    end record sim_ram;

end sim_ram_pkg;

package body sim_ram_pkg is

end package body sim_ram_pkg;
