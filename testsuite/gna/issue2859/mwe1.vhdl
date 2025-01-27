entity mwe1 is
  generic (
    VAL : natural);
end entity;

architecture test of mwe1 is
  function to_bv (v : natural) return bit_vector
  is
    variable v1 : natural;
    variable res : bit_vector(1 downto 0);
  begin
    v1 := v;
    for i in res'reverse_range loop
      if v1 mod 2 = 1 then
        res(i) := '1';
      else
        res(i) := '0';
      end if;
      v1 := v1 / 2;
    end loop;
    return res;
  end to_bv;
  constant BVAL : bit_vector(1 downto 0) := to_bv(VAL);
begin
    GenCase : case BVAL generate
        when "00" =>
          assert false report "zero" severity failure;
        when "10" =>
          assert false report "10";
        when others =>
          assert false report "others" severity failure;
    end generate;
end architecture;

