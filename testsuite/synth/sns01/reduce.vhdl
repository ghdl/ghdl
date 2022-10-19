library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;

entity reduce is
  port (vec : std_logic_vector(7 downto 0);
        res_and : out std_logic;
        res_nand : out std_logic;
        res_or : out std_logic;
        res_nor : out std_logic;
        res_xor : out std_logic;
        res_xnor : out std_logic);
end reduce;

architecture behav of reduce is
begin
  res_and <= and_reduce(vec);
  res_nand <= nand_reduce(vec);
  res_or <= or_reduce(vec);
  res_nor <= nor_reduce(vec);
  res_xor <= xor_reduce(vec);
  res_xnor <= xnor_reduce(vec);
end behav;
