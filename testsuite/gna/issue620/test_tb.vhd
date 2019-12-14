library ieee;
use ieee.std_logic_1164.all;

entity test_tb is
  package inst is new work.type_declaration_pkg generic map(2);
                  use inst.all;

  package userInst is new work.type_user_pkg generic map(inst.myType, inst.unity);
  use userInst.all;                                    
end entity test_tb;

architecture behavioral of test_tb is
  signal thisThing : inst.myType := (others => 0);
  signal thatThing : inst.myType;
  signal clk : std_ulogic := '0';
begin
  process
  begin
    for i in 1 to 5 loop
      wait for 5 ns;
      clk <= '0';
      wait for 5 ns;
      clk <= '1';
    end loop;
    wait;
  end process;
  process
  begin
    unity_proc(clk, thisThing, thatThing);
  end process;
end architecture behavioral;
