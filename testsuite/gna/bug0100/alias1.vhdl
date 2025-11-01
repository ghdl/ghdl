entity alias1 is
end entity;

architecture rtl of alias1 is
  function to_01 (s : string) return bit_vector is
    alias
    variable res : bit_vector(s'length - 1 downto 0);
  begin
    return res;
  end to_01;
begin
end architecture;
