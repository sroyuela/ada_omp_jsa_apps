pragma Warnings (Off, "*Reducer* is not referenced");
pragma Warnings (Off, "*Identity_Value* is not referenced");

generic

   type Result_Type is private;
   --  Final Result type

   with procedure Reducer (Left, Right : in out Result_Type);
   --  Reducing operation used to compute final result. The operation
   --  needs to take two values and reduce into a single value (the Left)
   --  parameter. The Right parameter is in out to allow for finalization
   --  if necessary.

   Identity_Value : Result_Type;
   --  A special value that when applied as the right operand of the
   --  Reducing function, does not change the value of the left operand.

   type Iteration_Index_Type is (<>);
   --  Loop Index type

package Parallel.Procedural_Reducing_Loops is

   type Parallelism_Manager is limited interface;

   procedure Execute_Parallel_Loop
     (Manager : Parallelism_Manager;

      From : Iteration_Index_Type := Iteration_Index_Type'First;
      To : Iteration_Index_Type := Iteration_Index_Type'Last;
      Worker_Count : Worker_Count_Type := Default_Worker_Count;
      Process   : not null access procedure
        (Start, Finish : Iteration_Index_Type;
         Item : in out Result_Type);
      Result : aliased in out Result_Type)
   is abstract;

end Parallel.Procedural_Reducing_Loops;

pragma Warnings (On, "*Reducer* is not referenced");
pragma Warnings (On, "*Identity_Value* is not referenced");
