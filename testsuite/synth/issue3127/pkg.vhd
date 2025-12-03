library ieee;
use ieee.std_logic_1164.all;

package pkg is

constant AHBDW : integer := 128;

type ahb_partial_t is record
  hwdata : std_logic_vector(AHBDW-1 downto 0);
  hready : std_logic;
end record;

type ahb_partial_vec_t is array (0 to 1) of ahb_partial_t;

component Design_Entity is
    port (
        do : in  std_logic := '0';
        b  : out std_logic
    );
end component Design_Entity;
end package pkg;
