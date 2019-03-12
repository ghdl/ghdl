library ieee;
use ieee.std_logic_1164.all;
entity HA_Entity is
  port (
    i_bit1  : in std_logic;
    i_bit2  : in std_logic;
    o_sum   : out std_logic;
    o_carry : out std_logic
    );
end HA_Entity;
architecture HA_Arch of HA_Entity is
  component HA_Comp is
    port (
      i_bit1  : in  std_logic;
      i_bit2  : in  std_logic;
      o_sum   : out std_logic;
      o_carry : out std_logic
      );
  end component HA_Comp;
begin
  HA_Inst : HA_Comp
    port map (
      i_bit1  => i_bit1,
      i_bit2  => i_bit2,
      o_sum   => o_sum,
      o_carry => o_carry);
end HA_Arch;
library ieee;
use ieee.std_logic_1164.all;
entity HA_Comp_Entity is
  port (
    i_bit1  : in std_logic;
    i_bit2  : in std_logic;
    o_sum   : out std_logic;
    o_carry : out std_logic
    );
end HA_Comp_Entity;
architecture HA_Comp_Arch_1 of HA_Comp_Entity is
begin
  o_sum   <= i_bit1 xor i_bit2;
  o_carry <= i_bit1 and i_bit2;
end HA_Comp_Arch_1;
use work.all;
configuration HA_Config of HA_Entity is
  for HA_Arch
    for HA_Inst : HA_Comp
      use entity HA_Comp_Entity(HA_Comp_Arch_1);
    end for;
  end for;
end HA_Config;
