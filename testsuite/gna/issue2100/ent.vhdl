library ieee;
context ieee.ieee_std_context;

entity ent is
  port (
    din  : in unsigned(15 downto 0);
    dout : out unsigned(31 downto 0)
  );
end ent;

architecture arch of ent is

begin

  dout <= resize(din, dout'subtype);

end architecture;
