use work.pkg_c.all;

entity test is
end entity;

architecture tb of test is
  constant block_len : natural := 3;
begin
  main: process
    variable val: integer;
  begin
    report "HELLO" severity note;
	for x in 0 to block_len-1 loop
      val := get(x);
      set(block_len+x, val+1);
    end loop;
    wait;
  end process;
end architecture;
