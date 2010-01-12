with Types; use Types;
with PSL.Nodes; use PSL.Nodes;

package PSL.Hash is
   --  Initialize the package.
   procedure Init;

   --  Get the PSL node for node HDL.
   --  Only one PSL node is created for an HDL node.
   function Get_PSL_Node (Hdl : Int32) return Node;
end PSL.Hash;
