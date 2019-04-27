entity tb is
end tb;

architecture behav of tb is
  function get_rand return integer;
  attribute foreign of get_rand: function is "VHPIDIRECT ./getrand.so get_rand";

  function get_rand return integer is
  begin
    assert false severity failure;
  end get_rand;
begin
  assert get_rand >= 0 severity note;
end behav;
