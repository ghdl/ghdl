entity case1 is
end;

architecture behav of case1 is
begin
  process
  begin
    for i in 1 to 10 loop
      case i is
        when 1 =>
          report "one";
          wait for 1 ns;
        when 2 =>
          report "two";
          wait for 2 ns;
        when 3 =>
          report "three";
          wait for 3 ns;
        when 4 to 9 =>
          report "a big digit";
          wait for 5 ns;
        when others =>
          report "a number";            -- including 0.
          wait for 10 ns;
      end case;
    end loop;
    report "SUCCESS";
    wait;
  end process;
 
end behav;
