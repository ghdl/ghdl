--  Shaped like the Quartus / MAX II simulation netlist in issue #2133: a flat
--  entity with an INOUT std_logic_vector driven by per-bit tristate buffers,
--  an enable steering the direction and an output mirroring the bus.  The
--  vendor primitives are replaced by an explicit tristate buffer, so no
--  proprietary library is needed.

library ieee;
use ieee.std_logic_1164.all;

entity tri_buf is
  port (datain  : in    std_logic;
        oe      : in    std_logic;
        padio   : inout std_logic;
        dataout : out   std_logic);
end entity;

architecture structure of tri_buf is
begin
  padio   <= datain when oe = '1' else 'Z';
  dataout <= padio;
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity min_bidi is
  port (ad_out : out   std_logic_vector (15 downto 0);
        oe_ad  : in    std_logic;
        mc_ad  : inout std_logic_vector (15 downto 0));
end entity;

architecture structure of min_bidi is
  signal din : std_logic_vector (15 downto 0);
begin
  din <= x"BEEF";

  gen : for i in mc_ad'range generate
    buf : entity work.tri_buf
      port map (datain  => din (i),
                oe      => oe_ad,
                padio   => mc_ad (i),
                dataout => ad_out (i));
  end generate;
end architecture;
