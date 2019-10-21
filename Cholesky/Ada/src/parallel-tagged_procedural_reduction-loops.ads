generic

   type Iteration_Index_Type is (<>);
   --  Loop Index type

package Parallel.Tagged_Procedural_Reduction.Loops is
   type Parallel_Dispatcher is limited interface;

   procedure Execute_Parallel_Loop
     (Dispatcher : Parallel_Dispatcher;

      From : Iteration_Index_Type := Iteration_Index_Type'First;
      To : Iteration_Index_Type := Iteration_Index_Type'Last;

      Process   : not null access procedure
        (Start, Finish : Iteration_Index_Type;
         Item : in out Procedural_Result'Class))
   is abstract;

end Parallel.Tagged_Procedural_Reduction.Loops;
