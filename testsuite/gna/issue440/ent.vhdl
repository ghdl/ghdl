package gpkg is
  generic (const : natural);
end package;

package ipkg is new work.gpkg generic map (const => 1);

entity ent is
end entity;

architecture a of ent is
begin
  main : process
  begin
     -- Case 1
     assert work.ipkg.const = 1; -- Should this result in a 'no declaration of const' error?
     -- case 2     
--     assert << constant @work.ipkg.const : natural>> = 1; -- Should this be visible?
  end process;
end architecture;
