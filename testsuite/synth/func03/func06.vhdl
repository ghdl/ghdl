entity func06 is
  port (a: bit_vector(3 downto 0);
        o : out bit);
end;

architecture behav of func06 is
  type my_rec is record
    a : bit;
    w : bit_vector;
  end record;

  function is_0 (v : my_rec) return bit is
  begin
    if v.w = "0011" then
      return v.a;
    else
      return '0';
    end if;
  end is_0;

begin
  o <= is_0(v.a => '1', v.w => a);
end;
