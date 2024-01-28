library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity;

architecture rtl of test is
 type t_record is record
   test : std_logic;
 end record;
 signal test1 : t_record;
 signal test2 : std_logic_vector(1 downto 0);
 signal test3_0 : std_logic;
 signal test3_1 : std_logic;
 signal test3_2 : natural;
 signal test3_3 : natural;
 signal test3_4 : std_logic;
 signal test3_5 : time;
 signal test4_0 : std_logic;
 signal test4_1 : std_logic;
begin
  p_proc_1 : process(test1.test)
  begin
  if rising_edge(test1.test) then
    null;
  end if;
  end process;

  p_proc_2 : process(test2(1))
  begin
  if rising_edge(test2(1)) then
    null;
  end if;
  end process;

  p_proc_3 : process(test3_0, test3_1, test3_4)
    variable v_test3_2 : natural;
  begin
    if test3_0 then
      if rising_edge(test3_1) then
        if test3_2 /= v_test3_2 then
          v_test3_2 := v_test3_2 + 1;
        end if;
        if test3_5 < 0 ns then
          null;
        end if;
        test3_3 <= test3_3 + 1;
      end if;
    elsif test3_4'event then
      null;
    end if;
  end process;

  p_proc_4_0 : process(test4_0, test4_1)
    variable v_test3_2 : natural;
  begin
    if rising_edge(test4_0) then
      null;
    elsif test4_1'event then
      null;
    end if;
  end process;

  p_proc_4_1 : process(test4_0, test4_1)
    variable v_test3_2 : natural;
  begin
    if test4_1'event then
      null;
    elsif rising_edge(test4_0) then
      null;
    end if;
  end process;
end architecture;
