with Parallel.Task_Pools.Bounded;

package body Parallel.Task_Pools is

   -----------------------
   -- Default_Task_Pool --
   -----------------------

   Default_Pool : aliased Bounded.Task_Pool
     (Number_Of_Workers => 100,
      Storage_Size => Default_Worker_Storage_Size,
      Ceiling_Priority => System.Max_Priority);
--     := Bounded.Create (Number_Of_Workers => 100);

   Default_Pool_Access : access Task_Pool_Interface'Class
     := Default_Pool'Access;

   -----------------------
   -- Default_Task_Pool --
   -----------------------

   function Default_Task_Pool return not null access Task_Pool_Interface'Class
   is
   begin
      return Default_Pool_Access;
   end Default_Task_Pool;

   ---------------------------
   -- Set_Default_Task_Pool --
   ---------------------------

   procedure Set_Default_Task_Pool
     (Pool : aliased in out Task_Pool_Interface'Class)
   is
   begin
      Default_Pool_Access := Pool'Unchecked_Access;
   end Set_Default_Task_Pool;

end Parallel.Task_Pools;
