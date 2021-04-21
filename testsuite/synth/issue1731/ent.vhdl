library ieee;
context ieee.ieee_std_context;

entity ent is
  port (
    dsin  : in signed(15 downto 0);
    dsout : out signed(31 downto 0);
    duin  : in unsigned(15 downto 0);
    duout : out unsigned(31 downto 0)
  );
end;

architecture arch of ent is

begin

  dsout <= resize(signed(dsin), dsout);
  duout <= resize(unsigned(duin), duout);

end architecture;
