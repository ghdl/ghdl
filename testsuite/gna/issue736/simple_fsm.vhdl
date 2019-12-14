--Standard Library
library ieee;
--Standard Packages
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity simple_fsm is
  port (
    clk : in  std_logic;
    rst : in  std_logic;

    valid : in std_logic;
    invalid : in std_logic
    );
end simple_fsm;

architecture rtl of simple_fsm is

  type t_states is (e_IDLE, e_S1);
  signal p_state : t_states := e_IDLE;
  signal n_state : t_states;
  
begin
  
  p_sync_fsm : process(clk)
  begin
    if rising_edge(clk) then
      if (rst = '1') then
        p_state <= e_IDLE;
      else
        p_state <= n_state;
      end if;
    end if;
  end process;

  p_comb_fsm : process (all)
  begin
    case p_state is
      when e_IDLE =>
        n_state <= e_S1 when valid = '1' else e_IDLE;
      when e_S1 =>
        n_state <= e_IDLE when (valid = '0' and invalid = '1') else e_S1;
    end case;
  end process;

end rtl;
