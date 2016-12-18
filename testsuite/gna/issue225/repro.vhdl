entity foo is
  port (
    a0 : in bit_vector(1 downto 0)
    );
end entity;

architecture bar of foo is
begin
  assert a0(0) = '0';
end architecture;

entity foo_tb is
  generic
  ( DEFAULT_X : bit_vector(1 downto 0) := (others => '0')
  );
end entity;

architecture tb of foo_tb is

  function compute_stuff_with_x(x : bit_vector) return bit_vector is
  begin
    return x;
  end compute_stuff_with_x;

begin

  foo_inst:
    entity work.foo
    port map
    ( a0 => compute_stuff_with_x(DEFAULT_X)
    );

end architecture;
