library ieee;
use ieee.std_logic_1164.all,
  ieee.numeric_std.all;

package DAC_package_t is
  generic (
    mode_totempole             : boolean;
    channels_number            : integer range 2 to 300;
    data_size                  : integer range 4 to 64;
    DAC_data_size              : integer range 4 to 50;
    nbre_DACs_used             : integer range 1 to 64;
    MasterCLK_SampleCLK_ratio  : integer range 14 to 100 := 22;
    MasterCLK_DACCLK_ratio     : integer range 2 to 2    := 2;
    Negation_fast_not_accurate : boolean                 := false);
  constant nbre_outputs_per_DAC : natural := channels_number / nbre_DACs_used;

  component DAC_bundle_dummy is
    port(data_in : in  std_logic_vector(data_size - 1 downto 0));
  end component DAC_bundle_dummy;

  subtype registers_control_st is std_logic_vector(2 downto 0);
end package DAC_package_t;

library ieee;
use ieee.std_logic_1164.all,
  ieee.numeric_std.all;

package DAC_package is
  new work.DAC_package_t generic map (
    mode_totempole             => false,
    channels_number            => 4,
    data_size                  => 12,
    DAC_data_size              => 8,
    nbre_DACS_used             => 1,
    MasterCLK_SampleCLK_ratio  => 22,
    MasterCLK_DACCLK_ratio     => 2,
    Negation_fast_not_accurate => true);

