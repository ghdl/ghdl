

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity blinky_synth is
  port (
    clk : in std_logic;
    led : out std_logic
  );
end entity;

architecture rtl of blinky_synth is
  signal pll_clk : std_logic;
  signal feedback : std_logic;
  signal r_count : unsigned(24 downto 0) := (others => '0');

  component MMCME2_ADV is
    generic (
      BANDWIDTH : string := "OPTIMIZED";
      CLKFBOUT_MULT_F : real := 5.000;
      CLKFBOUT_PHASE : real := 0.000;
      CLKIN1_PERIOD : real := 0.000;
      CLKOUT0_DIVIDE_F : real := 1.000;
      CLKOUT0_PHASE : real := 0.000;
      COMPENSATION : string := "ZHOLD";
      DIVCLK_DIVIDE : integer := 1;
      REF_JITTER1 : real := 0.0
    );
    port (
      CLKFBOUT : out std_ulogic := '0';
      CLKOUT0 : out std_ulogic := '0';
      LOCKED : out std_ulogic := '0';
      CLKFBIN : in std_ulogic;
      CLKIN1 : in std_ulogic;
      PSINCDEC : in std_ulogic;
      PWRDWN : in std_ulogic;
      RST : in std_ulogic
    );
  end component MMCME2_ADV;

begin
  mmcm_adv_inst : MMCME2_ADV
  generic map(
    BANDWIDTH => "OPTIMIZED",
    COMPENSATION => "ZHOLD",
    CLKFBOUT_MULT_F => 10.625,
    CLKIN1_PERIOD => 10.000,
    CLKOUT0_DIVIDE_F => 20.875,
    CLKOUT0_PHASE => 0.000,
    DIVCLK_DIVIDE => 1,
    REF_JITTER1 => 0.01
  )
  port map(
    PSINCDEC => '0',
    CLKFBIN => feedback,
    CLKIN1 => clk,
    PWRDWN => '0',
    RST => '0',
    CLKFBOUT => feedback,
    CLKOUT0 => pll_clk,
    LOCKED => open
  );

  led <= r_count(24);

  process (pll_clk) is
  begin
    if rising_edge(pll_clk) then
      r_count <= r_count + 1;
    end if;
  end process;
end architecture;

