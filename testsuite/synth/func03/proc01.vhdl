entity proc01 is
  port (a: bit_vector(3 downto 0);
        r : out bit_vector(3 downto 0);
        rx : out bit);
end;

architecture behav of proc01 is
  procedure inc (inp : bit_vector; signal res: out bit_vector)
  is
    alias ainp : bit_vector(inp'length -1 downto 0) is inp;
    alias ares : bit_vector(res'length -1 downto 0) is res;
    variable carry : bit;
  begin
    carry := '1';
    for i in ares'reverse_range loop
      if i <= ainp'left then
        res (i) <= inp (i) xor carry;
        carry := inp(i) and carry;
      else
        res (i) <= carry;
        carry := '0';
      end if;
    end loop;
  end inc;

begin
  inc (a, res(3 downto 0) => r, res(4) => rx);
end;
