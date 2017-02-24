entity tb_test4 is
end tb_test4;

architecture tb of tb_test4 is
begin
    test_runner : process
    begin
      assert 2.0**(-1023) > 0.0 report "This passes";
      assert 2.0**(-1024) > 0.0 report "This fails" severity note;
      assert 2.0**(-1024) = 0.0 report "This passes" severity failure;

      assert (2.0**(-1023))/2 > 0.0 report "This fails" severity error;

      wait;
    end process test_runner;
end;
