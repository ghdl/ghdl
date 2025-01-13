entity mwe2 is
end entity;

architecture test of mwe2 is
  type arr_t is array (0 to 2) of integer;
begin
    process
        variable v : arr_t;
    begin
      v := (character'pos('('), -3, -2);

      report integer'image(v(0));
      report integer'image(v(1));
      report integer'image(v(2));

      assert v(0) = character'pos('(');
      assert v(1) = -3;
      assert v(2) = -2;
      wait;
    end process;
end architecture;
