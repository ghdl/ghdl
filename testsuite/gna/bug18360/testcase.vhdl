entity TESTCASE is
end entity TESTCASE;

architecture PROBLEM of TESTCASE is
  type ENUMERATION_TYPE is (VALUE1, VALUE2);
  constant SOME_VAR : integer := ENUMERATION_TYPE'length;
begin
end architecture PROBLEM;
