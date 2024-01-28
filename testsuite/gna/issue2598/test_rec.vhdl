library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_rec is
end entity;

architecture rtl of test_rec is
 type t_record is record
   test : std_logic;
 end record;
 signal test1 : t_record;
begin
  p_proc_1 : process(test1.test)
  begin
  if rising_edge(test1.test) then
    null;
  end if;
  end process;
end architecture;
