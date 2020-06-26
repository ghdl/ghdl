entity foo4 is
  port (
    input : in bit_vector(1 downto 0)
    );
end foo4;

architecture behave of foo4 is
begin
end behave;

entity bar4 is
end entity bar4;

architecture behave of bar4 is
  component foo4 is
  port (
    input : in natural
    );
  end component;

begin

  my_foo : foo4
    port map (
      input => 0
      );
end behave;

configuration cfg of bar4 is
  for behave
    for my_foo : foo4
      use open;
    end for;
  end for;
end cfg;
