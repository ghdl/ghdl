library ieee;
use IEEE.fixed_pkg.all;
entity mre is
  port (
    x: out real
  );
end entity;
architecture a of mre
is
begin
  x <= 1.1;
end architecture;

