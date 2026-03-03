library ieee;
use ieee.std_logic_1164.all;

package repro4_pkg is
  constant AHBDW        : integer := 128;
  constant NAHBMST   : integer := 16;  -- maximum AHB masters
  constant NAHBIRQ : integer := 32;
  constant NAHBSLV : integer := 16;
  constant NAHBCFG : integer := 8;

  subtype amba_config_word is std_logic_vector(31 downto 0);
  type ahb_config_type is array (0 to NAHBCFG-1) of amba_config_word;

  type ahb_slv_out_type is record
    hready      : std_ulogic;                           -- transfer done
    hresp       : std_logic_vector(1 downto 0);         -- response type
    hrdata      : std_logic_vector(AHBDW-1 downto 0);   -- read data bus
    hsplit      : std_logic_vector(NAHBMST-1 downto 0); -- split completion
    hirq        : std_logic_vector(NAHBIRQ-1 downto 0); -- interrupt bus
    hconfig     : ahb_config_type;                      -- memory access reg.
    hindex      : integer range 0 to NAHBSLV-1;         -- diagnostic use only
  end record;

  type ahb_slv_out_vector_type is array (natural range <>) of ahb_slv_out_type;

  subtype ahb_slv_out_vector is ahb_slv_out_vector_type(NAHBSLV-1 downto 0);

end;
