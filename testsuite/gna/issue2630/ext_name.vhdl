use std.textio.all;

entity ext_name is
end ext_name;

architecture behaviour of ext_name is
begin
  a : block is
    constant A_C : natural := 42;
  begin
  end block;

  b : block is
    alias A_C is << constant .ext_name.a.A_C : natural >>;
  begin
    b_proc : process
      variable l : line;
    begin
      write (l, to_string(A_C));
      writeline (output, l);
      assert A_C = 42 severity failure;
      wait;
    end process;
  end block;
end behaviour;
