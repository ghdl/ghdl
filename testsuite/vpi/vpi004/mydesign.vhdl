library ieee ;
use ieee.std_logic_1164.all;

entity myentity is
  port (
    iportbool: in boolean;
    iportint: in integer;
    iportsl: in std_logic;
    oportbool: out boolean;
    oportint: out integer;
    oportsl: out std_logic
    );
end myentity;

architecture arch of myentity is
  constant constsl: std_logic := '0';
  signal sigsl: std_logic;
  constant constint: integer := 42;
  signal sigint: integer;
  constant constbool: boolean := True;
  signal sigbool: boolean;
  constant conststring: string := "fish";
begin
  sigsl <= iportsl;
  sigbool <= iportbool;
  sigint <= iportint;

  oportbool <= constbool;
  oportint <= constint;
  oportsl <= constsl;

end arch;
