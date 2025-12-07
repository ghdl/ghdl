entity convarr05 is
  generic (w : natural := 4);
  port (a : bit_vector(w - 1 downto 0);
        q : out bit_vector(w -1 downto 0));
end;

architecture behav of convarr05 is
  subtype bv5 is bit_vector(0 to 5);
begin
  q <= bv5(a);
end;
