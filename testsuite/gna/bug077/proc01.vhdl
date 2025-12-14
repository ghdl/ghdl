entity proc01 is
end;

architecture behav of proc01 is
  procedure inc (inp : bit_vector; res: out bit_vector)
  is
    alias ainp : bit_vector(inp'length -1 downto 0) is inp;
    alias ares : bit_vector(res'length -1 downto 0) is res;
    variable carry : bit;
  begin
    carry := '1';
    for i in ares'reverse_range loop
      if i <= ainp'left then
        ares (i) := ainp (i) xor carry;
        carry := ainp(i) and carry;
      else
        ares (i) := carry;
        carry := '0';
      end if;
    end loop;
  end inc;


begin
  process
    variable a: bit_vector(3 downto 0);
    variable r :  bit_vector(3 downto 0);
    variable rx : bit;
  begin
    for i in 1 to 4 loop
      case i is
        when 1 =>
          a := "0000";
        when 2 =>
          assert r = "0001" severity failure;
          assert rx = '0' severity failure;

          a := "1110";

        when 3 =>
          assert r = "1111" severity failure;
          assert rx = '0' severity failure;

          a := "1111";
        when 4 =>
          assert r = "0000" severity failure;
          assert rx = '1' severity failure;
        when others =>
          wait;
      end case;
      inc (a, res(1 to 4) => r, res(0) => rx);
    end loop;
  end process;
end;
