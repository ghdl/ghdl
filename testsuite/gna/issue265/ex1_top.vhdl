library IEEE;
use     IEEE.std_logic_1164.all;

entity ex1_top is
end entity;

architecture a of ex1_top is
  signal A : boolean;
  
  function conv_inst_top(p : std_logic) return boolean is
  begin
    return (p = '0'); -- inverter
  end function;
  
  function conv_top_inst(p : boolean) return std_logic is
  begin
    if (p = FALSE) then
      return '0'; -- identity
    elsif (p = TRUE) then
      return '1'; -- identity
    else
      return 'X'; -- error
    end if;
  end function;
begin
  A <= TRUE;

  inst : entity work.ex1_entity
    port map (
      conv_inst_top(X) => conv_top_inst(A)
    );
end architecture;
