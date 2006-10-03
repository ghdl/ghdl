with Iirs; use Iirs;

package Iirs_Walk is
   type Walk_Status is
     (
      --  Continue to walk.
      Walk_Continue,

      --  Stop walking in the subtree, continue in the parent tree.
      Walk_Up,

      --  Abort the walk.
      Walk_Abort);

   type Walk_Cb is access function (El : Iir) return Walk_Status;

   --  Walk on all elements of CHAIN.
   function Walk_Chain (Chain : Iir; Cb : Walk_Cb) return Walk_Status;


   function Walk_Assignment_Target (Target : Iir; Cb : Walk_Cb)
                                   return Walk_Status;

   --  Walk on all stmts and sub-stmts of CHAIN.
   function Walk_Sequential_Stmt_Chain (Chain : Iir; Cb : Walk_Cb)
                                       return Walk_Status;
end Iirs_Walk;
