entity repro7 is
end repro7;

architecture behav of repro7 is
  type my_rec is record
    addr : bit_vector;
    wr : boolean;
    desc : string;
  end record;

  constant v : my_rec (addr(0 downto 0)) := (
    addr => (others => '0'),
    wr   => true,
    desc => "none");
begin
  assert v.wr;
  assert v.desc = "none";
end behav;
