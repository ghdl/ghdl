--  Package (and subprograms) instantiations

--  When a package is instantiated, we need to 'duplicate' its declaration.
--  This looks useless for analysis but it isn't: a type from a package
--  instantiated twice declares two different types.  Without duplication, we
--  need to attach to each declaration its instance, which looks more expansive
--  that duplicating the declaration.
--
--  Furthermore, for generic type interface, it looks a good idea to duplicate
--  the body (macro expansion).
--
--  Duplicating is not trivial: internal links must be kept and external
--  links preserved.  A table is used to map nodes from the uninstantiated
--  package to its duplicated node.  Links from instantiated declaration to
--  the original declaration are also stored in that table.

with Vhdl.Nodes; use Vhdl.Nodes;

package Vhdl.Sem_Inst is
   --  Return the origin of node N, the node from which N was instantiated.
   --  If N is not an instance, this function returns Null_Iir.
   function Get_Origin (N : Iir) return Iir;

   --  Create declaration chain and generic declarations for INTER from PKG,
   --  so the the parent of INTER can be analyzed and references to PKG
   --  points to an instance.
   --  It is necessary to create an instance because two interface packages
   --  are different even if they have the same uninstantiated package.
   procedure Instantiate_Interface_Package_Declaration
     (Inter : Iir; Pkg : Iir);

   --  Instantiate each node of PKG to INST.
   --  A generic interface package points to the actual.
   procedure Instantiate_Package_Declaration (Inst : Iir; Pkg : Iir);

   --  Create interface parameters of INST from SUBPRG.
   procedure Instantiate_Subprogram_Declaration (Inst : Iir; Subprg : Iir);

   --  Return the instantiation of the body for INST, ie macro-expand the
   --  body.  INST has the form of a generic-mapped package.
   function Instantiate_Package_Body (Inst : Iir) return Iir;

   function Instantiate_Entity_Declaration (Ent : Iir; Map_Parent : Iir)
                                           return Iir;

   function Instantiate_Component_Declaration (Comp : Iir; Map_Parent : Iir)
                                              return Iir;

   --  Instantiate architecture ARCH for *instantiated* entity ENT.
   --  STMT is the statement that instantiated ENT.
   function Instantiate_Architecture
     (Arch : Iir; Ent : Iir; Stmt : Iir; Map_Parent : Iir) return Iir;

   --  In CHAIN, substitute all references to E by REP.
   procedure Substitute_On_Chain (Chain : Iir; E : Iir; Rep : Iir);

   --  Deep copy of ORIG.  Doesn't change location.
   function Copy_Tree (Orig : Iir) return Iir;

   --  Like Get_Subprogram_Body, but also works for instances, where
   --  instantiated nodes have no bodies.
   --  See comment about Get/Set_Subprogram_body in vhdl-nodes.
   function Get_Subprogram_Body_Origin (Spec : Iir) return Iir;

   --  Likewise for protected type bodies.
   --  ATYPE is the protected type definition.
   function Get_Protected_Type_Body_Origin (Atype : Iir) return Iir;
end Vhdl.Sem_Inst;
