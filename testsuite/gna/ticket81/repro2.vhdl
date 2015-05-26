entity repro2 is
end;

architecture behav of repro2 is
  function zeros (a, b : bit_vector) return bit_vector is
  begin
    if a'length = 1 then
      return "0";
    end if;
  end;
begin
end behav;
