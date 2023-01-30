library ieee;
use ieee.std_logic_1164.all;

entity ecp5pll is
  generic (dynamic_en   : natural := 0);
  port (
    clk_i        : in  std_logic;
    clk_o        : out std_logic;
    phasesel     : in  std_logic_vector(1 downto 0) := "00");
end;

architecture mix of ecp5pll is
  type T_secondary is record
    div            : natural;
    freq_string    : string(0 to 9);
  end record T_secondary;

  signal CLKOP_t  : std_logic;
  signal PHASESEL_HW : std_logic_vector(1 downto 0);
begin
  G_dynamic: if dynamic_en /= 0 generate
    PHASESEL_HW <= phasesel;
  end generate;

  clk_o <= CLKOP_t;
end mix;
