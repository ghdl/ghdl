entity vu is
  port (clk : bit; a : bit);
end entity;

architecture a of vu is
begin
end architecture;

vunit v1 (vu(a))
{
}

vunit v2 (vu(a))
{
  inherit v1;
  inherit v1;
}
