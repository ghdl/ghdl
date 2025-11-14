
library ieee;
use ieee.std_logic_1164.all;

package reprod_pkg is
  constant TRACE_WIDTH : integer := 512;
  type itracebuf_in_type5 is record
    addr0            : std_logic_vector(11 downto 0);
    addr1            : std_logic_vector(11 downto 0);
    data0            : std_logic_vector(TRACE_WIDTH/2-1 downto 0);
    data1            : std_logic_vector(TRACE_WIDTH/2-1 downto 0);
    enable           : std_logic_vector(1 downto 0);
    write            : std_logic_vector(1 downto 0);
  end record;
  type itracebuf_out_type5 is record
    data            : std_logic_vector(TRACE_WIDTH-1 downto 0);
  end record;
end package;
