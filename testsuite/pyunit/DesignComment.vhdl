--  No copyright for :accum: design.

library ieee;
use ieee.std_logic_1164.all;

entity accum is
  port (
    --  :a: and :b: are the inputs of the adder.
    a, b : in std_logic_vector (31 downto 0);
    --  :res: is the result of the adder.
    res : out std_logic_vector (31 downto 0)
    );
end accum;

