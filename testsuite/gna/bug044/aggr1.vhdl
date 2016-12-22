entity aggr1 is
end aggr1;

architecture behav of aggr1 is
  procedure proc (b, c : out bit_vector) is
  begin
    b := (others => '0');
    c := ('1', others => '0');
  end proc;
begin
end behav;
