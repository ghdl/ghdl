entity visibility8f3 is
end entity;

architecture test of visibility8f3 is
  function f (a : integer) return integer is
  begin
    return 0;
  end f;

  function g (a : bit) return integer is
  begin
    return 0;
  end g;
  alias g is f [integer return integer];
begin

end architecture;
