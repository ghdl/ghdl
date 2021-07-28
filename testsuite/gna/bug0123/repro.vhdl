entity repro is
end;

architecture behav of repro is
  type mem_t is array (natural range <>) of bit_vector (3 downto 0);
  constant cst : mem_t := (1 => "0000",
                           2 => "0001",
                           3 => "00" & "01");
begin
end behav;
