library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity name01_adder is
  generic (ga_000 : natural := 1_000_000_000;
           ga_001 : natural := 1_010_000_000;
           ga_002 : natural := 1_020_000_000;
           ga_003 : natural := 1_030_000_000;
           ga_004 : natural := 1_040_000_000;
           ga_005 : natural := 1_050_000_000;
           ga_006 : natural := 1_060_000_000;
           ga_007 : natural := 1_070_000_000;
           ga_008 : natural := 1_080_000_000;
           ga_009 : natural := 1_090_000_000;
           ga_010 : natural := 1_100_000_000;
           ga_011 : natural := 1_110_000_000;
           ga_012 : natural := 1_120_000_000;
           ga_013 : natural := 1_130_000_000;
           ga_014 : natural := 1_140_000_000;
           ga_015 : natural := 1_150_000_000;
           ga_016 : natural := 1_160_000_000;
           ga_017 : natural := 1_170_000_000;
           ga_018 : natural := 1_180_000_000;
           ga_019 : natural := 1_190_000_000;
           ga_020 : natural := 1_200_000_000;
           ga_021 : natural := 1_210_000_000;
           ga_022 : natural := 1_220_000_000;
           ga_023 : natural := 1_230_000_000;
           ga_024 : natural := 1_240_000_000;
           ga_025 : natural := 1_250_000_000;
           ga_026 : natural := 1_260_000_000;
           ga_027 : natural := 1_270_000_000;
           ga_028 : natural := 1_280_000_000;
           ga_029 : natural := 1_290_000_000;
           ga_030 : natural := 1_200_000_000;
           ga_031 : natural := 1_210_000_000;
           ga_032 : natural := 1_220_000_000;
           ga_033 : natural := 1_230_000_000;
           ga_034 : natural := 1_240_000_000;
           ga_035 : natural := 1_250_000_000;
           ga_036 : natural := 1_260_000_000;
           ga_037 : natural := 1_270_000_000;
           ga_038 : natural := 1_280_000_000;
           ga_039 : natural := 1_290_000_000;
           ga_040 : natural := 1_200_000_000;
           ga_041 : natural := 1_210_000_000;
           ga_042 : natural := 1_220_000_000;
           ga_043 : natural := 1_230_000_000;
           ga_044 : natural := 1_240_000_000;
           ga_045 : natural := 1_250_000_000;
           ga_046 : natural := 1_260_000_000;
           ga_047 : natural := 1_270_000_000;
           ga_048 : natural := 1_280_000_000;
           ga_049 : natural := 1_290_000_000;
           ga_050 : natural := 1_300_000_000
           );
  port (a, b : in std_logic_vector(7 downto 0);
        r : out std_logic_vector(7 downto 0));
end name01_adder;

architecture behav of name01_adder is
begin
  r <= std_logic_vector(unsigned(a) + unsigned(b));
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity name01 is
  port (a, b : in std_logic_vector(7 downto 0);
        r : out std_logic_vector(7 downto 0));
end;

architecture behav of name01 is
  signal t : std_logic_vector(7 downto 0);
begin
  adder: entity work.name01_adder
    port map (a, b, t);

  r <= t and x"ee";
end behav;
