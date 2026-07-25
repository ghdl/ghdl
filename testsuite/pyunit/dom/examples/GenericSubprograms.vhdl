-- Author: Patrick Lehmann
--
-- A package with generic subprogram interface items (VHDL-2008), exercising
-- GenericFunctionInterfaceItem/GenericProcedureInterfaceItem translation.
package GenericSubprograms is
	generic (
		function compare(a, b : integer) return boolean;
		procedure log(msg : string)
	);
end package GenericSubprograms;
