library ieee;
use ieee.std_logic_1164.all;

entity mcve2 is
end;

architecture sim of mcve2 is
  constant width_in     : natural := 11;        -- 12 bits
  constant last_stage   : natural := 5;

  signal v_clk          : std_logic := '0';
  signal v_rst          : std_logic;

  subtype stage_reg_t is std_logic_vector (width_in downto 0);
  type stage_regs_t is array (0 to last_stage) of stage_reg_t;
  signal stage_reg      : stage_regs_t;

begin

end sim;
