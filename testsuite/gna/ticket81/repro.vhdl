entity repro is
end;

architecture behav of repro is
  function zeros (a, b : bit_vector) return bit_vector is
    constant res : bit_vector (a'length + b'length - 1 downto 0) := (others => '0');
  begin
    return res;
  end;
begin
end behav;
