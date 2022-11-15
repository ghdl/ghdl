library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
-- use ieee.math_real.all;

LIBRARY altera_mf;
-- USE altera_mf.altera_mf_components.all;

entity test is
  port (
    clk     : in std_logic;
    rst     : in std_logic;
    re     : in std_logic;
    we     : in std_logic;
    src_in  : in std_logic_vector(255 downto 0);
    src_out : out std_logic_vector(255 downto 0)
  );
end entity;

architecture rtl of test is

begin

  scfifo_rx : entity altera_mf.scfifo
  generic map (
    lpm_width => 256,
    lpm_widthu => 2,
    lpm_numwords => 2,
    lpm_showahead => "OFF",
    lpm_hint => "USE_EAB=ON",
    ram_block_type => "AUTO",
    intended_device_family => "Arria 10",
    almost_full_value => 0,
    almost_empty_value => 0,
    overflow_checking => "ON",
    underflow_checking => "ON",
    allow_rwcycle_when_full => "OFF",
    add_ram_output_register => "OFF",
    use_eab => "ON",
    lpm_type => "scfifo",
    enable_ecc => "false",
    maximum_depth => 0
  )
  port map (
    aclr => rst,
    clock => clk,
    wrreq => we,
    data => src_in,
    full => open,
    rdreq => re,
    q => src_out,
    empty => open,
    usedw => open
  );

end architecture;
