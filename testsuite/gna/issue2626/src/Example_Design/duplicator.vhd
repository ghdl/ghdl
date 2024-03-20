library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity duplicator is
   port (
      -- Input
      s_valid_i    : in  std_logic;
      s_ready_o    : out std_logic;

      -- Output 1
      m1_valid_o   : out std_logic;
      m1_ready_i   : in  std_logic;

      -- Output 2
      m2_valid_o   : out std_logic;
      m2_ready_i   : in  std_logic
   );
end entity duplicator;

architecture synthesis of duplicator is

begin

   s_ready_o <= m1_ready_i and m2_ready_i;
   m1_valid_o <= s_valid_i and m2_ready_i;
   m2_valid_o <= s_valid_i and m1_ready_i;

end architecture synthesis;

