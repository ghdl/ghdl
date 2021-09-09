entity ent is
  generic(
    BITS : positive);
  port(
    input : in bit_vector(BITS - 1 downto 0));
end entity;

architecture rtl of ent is
begin
end architecture;



entity test is
end entity;

architecture rtl of test is
  constant MAX : positive := 7;
  signal input : natural;

  function to_bv (l : natural; v : natural) return bit_vector
  is
    variable res : bit_vector (l - 1 downto 0);
  begin
    if v /= 0 then
      res (0) := '1';
    end if;
    return res;
  end to_bv;
begin
  ent : entity work.ent
    generic map(
      BITS => MAX)
    port map(
      input => to_bv(MAX, input));
end architecture;
