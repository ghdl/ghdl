entity repro is
end;

architecture behav of repro is
  type my_rec is record
    bv : bit_vector;
  end record;
  
  function get_bv (n : natural) return my_rec is
  begin
    return (bv => (1 to n => '0'));
  end get_bv;

  constant l : natural := get_bv (5).bv'length;
begin
  assert l = 5;
end;
