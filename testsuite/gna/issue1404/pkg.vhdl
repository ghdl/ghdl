package mylib_pkg is

  pure function ceil_log2(constant v : natural) return natural;

end package mylib_pkg;

package body mylib_pkg is

  pure function ceil_log2(constant v : natural) return natural is
    variable res : natural;
  begin
    res := 31;
    for i in 30 downto 0 loop
      if (2**i >= v) then
        res := i;
      end if;
    end loop;
    return res;
  end function ceil_log2;

end package body mylib_pkg;
