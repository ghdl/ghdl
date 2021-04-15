
library ieee ;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity bidir_io is
    generic (
    EN_PULLDN   : boolean := false; 
    EN_PULLUP   : boolean := false);    
    port (
    io_pin    : inout std_logic; 
    d_in      : out   std_logic;
    d_out     : in    std_logic;
    out_en    : in    std_logic);
end bidir_io;

architecture ice40 of bidir_io is

    signal use_pullup : std_logic;

    component SB_IO
    generic(
        PIN_TYPE    : std_logic_vector;
        PULLUP      : std_logic;
        NEG_TRIGGER : std_logic);
    port(
        PACKAGE_PIN       : inout std_logic;
        LATCH_INPUT_VALUE : in  std_logic;
        CLOCK_ENABLE      : in  std_logic;
        INPUT_CLK         : in  std_logic;
        OUTPUT_CLK        : in  std_logic;
        OUTPUT_ENABLE     : in  std_logic;
        D_OUT_0           : in std_logic;
        D_OUT_1           : in std_logic;
        D_IN_0            : out std_logic;
        D_IN_1            : out std_logic);
end component;

begin


gen_pd : if EN_PULLDN generate
    assert false report "not implemented" severity error;
end generate;

use_pullup <= '1' when EN_PULLUP else '0';

u_iobuf : SB_IO
    generic map(
        PIN_TYPE => b"101001",
        PULLUP => use_pullup,
        NEG_TRIGGER => '0')
    port map(
        PACKAGE_PIN => io_pin,
        LATCH_INPUT_VALUE => '0',
        CLOCK_ENABLE => '0',
        INPUT_CLK => '0',
        OUTPUT_CLK => '0',
        OUTPUT_ENABLE => out_en,
        D_OUT_0 => d_out,
        D_OUT_1 => '0',
        D_IN_0 => d_in,
        D_IN_1 => open);

end ice40;
