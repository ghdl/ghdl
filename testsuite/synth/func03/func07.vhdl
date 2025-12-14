entity func07 is
  port (a, b: bit_vector(3 downto 0);
        o : out bit);
end;

architecture behav of func07 is
  type my_arr is array (1 to 2) of bit_vector;

  function is_0 (v : my_arr) return bit is
  begin
    if (not v(1) and v(2)) /= (v(1)'range => '0') then
      return '1';
    else
      return '0';
    end if;
  end is_0;

begin
  o <= is_0(v(2) => a, v(1) => b);
end;
