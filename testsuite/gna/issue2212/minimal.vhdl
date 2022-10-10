library IEEE;
  use IEEE.std_logic_1164.all;
  use IEEE.numeric_std.all;
 
entity minimal is
  port(
      duty_cycle: in std_logic_vector(7 downto 0)
    );
end entity minimal;

architecture rtl of minimal is

begin
  
 DECIDE_STATE: process(all) is
--DECIDE_STATE: process(duty_cycle) is -- uncomment to run
    variable duty : boolean;
  begin
    duty := true when signed(duty_cycle) > 0 else false;
  end process;
  
end architecture rtl;
