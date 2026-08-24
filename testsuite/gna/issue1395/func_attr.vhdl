--  The original reproducer from the issue: the range of the local string
--  comes from the attributes of an unconstrained parameter, so it is not
--  known until the call.  With a BIT_VECTOR(4 downto 0) actual it is
--  string(4 downto 0), which is outside POSITIVE.
entity func_attr is
end;

architecture arch of func_attr is
begin
  process
    function bv2str (bv : bit_vector) return string is
      variable st_out : string (bv'high downto bv'low);
    begin
      for i in bv'range loop
        if bv (i) = '0' then
          st_out (i) := '0';
        else
          st_out (i) := '1';
        end if;
      end loop;
      return st_out;
    end function;

    variable bitv : bit_vector (4 downto 0);
  begin
    report bv2str (bitv);
    wait;
  end process;
end;
