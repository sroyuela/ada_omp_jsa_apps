pragma Profile (Ravenscar);

with System.Storage_Elements;
with System.Multiprocessors; use System;

generic

   Storage_Size : System.Storage_Elements.Storage_Count
     := Default_Worker_Storage_Size;
   Worker_Priority : System.Priority := System.Default_Priority;
   Number_Of_Workers : Pool_Worker_Count := 100;

package Parallel.Ravenscar_Task_Pools.Implementation is

   type Worker (Core : Multiprocessors.CPU_Range) is limited private;

   type Worker_Array is array (1 .. Number_Of_Workers) of access Worker;
   --  The Ada tasks in the task pool

   type Task_Pool (Workers : access Worker_Array) is limited new
     Task_Pool_Interface with private;
   --  task pool object type that has a pool of real Ada tasks to
   --  process virtual thread fibers that are submitted to the pool for
   --  processing. Each Ada task in the task pool corresponds to an OS threads
   --  in GNAT

private

   type Work_Plan_Access is access all Work_Plan'Class;

   task type Worker_Task
     (Core : Multiprocessors.CPU_Range;
      Static_Worker : not null access Worker)
   with CPU => Core, Priority => Worker_Priority
   is
      pragma Storage_Size (Storage_Size);
   end Worker_Task;

   protected type Worker_Manager
   is
      pragma Priority (Worker_Priority);

      entry Wait_For_Work
        (Worker : out Pool_Index;
         Plan   : out Work_Plan_Access;
         Item   : out Plan_Index);

      procedure Offer_Work
        (Worker : Pool_Index;
         Plan   : aliased in out Work_Plan'Class;
         Item   : Plan_Index);

   private
      Worker_Index    : Pool_Index;
      Assignment_Plan : Work_Plan_Access;
      Assignment_Id   : Plan_Index;
      Offered_Work    : Boolean := False;
   end Worker_Manager;

   type Worker (Core : Multiprocessors.CPU_Range) is limited
      record
         Manager : Worker_Manager;
         Static_Task : Worker_Task
           (Core => Core,
            Static_Worker => Worker'Access);
      end record;

   type Idle_List is
     array (Pool_Index range 1 .. Number_Of_Workers) of Pool_Index;

   function Create_Idle_List return Idle_List;

   protected type Task_Manager
     (Pool : not null access Task_Pool) is

      procedure Offer_Work
        (Plan          : aliased in out Work_Plan'Class;
         Item          : Plan_Index);

      procedure Offer_Work_To_Group
        (Plan          : aliased in out Work_Plan'Class;
         Worker_Count  : Positive_Worker_Count);

      procedure Set_Worker_Idle
        (Worker : Pool_Index;
         Plan : aliased in out Work_Plan'Class;
         Item : Plan_Index);

      procedure Next_Worker_Id
        (Plan : aliased in out Work_Plan'Class;
         Requester : Plan_Index;
         Item : out Plan_Index);

      function Active_Workers return Pool_Worker_Count;

   private

      Outstanding_Workers : Pool_Worker_Count := 0;
      Idle_Workers     : Idle_List := Create_Idle_List;

      pragma Inline (Set_Worker_Idle);

   end Task_Manager;

   type Task_Pool (Workers : access Worker_Array) is limited new
     Task_Pool_Interface with
      record
         Manager : Task_Manager (Pool => Task_Pool'Access);
      end record;

   overriding
   procedure Offer_Work
     (Pool : in out Task_Pool;
      Plan : aliased in out Work_Plan'Class;
      Item : Plan_Index);

   overriding
   procedure Offer_Work_To_Group
     (Pool : in out Task_Pool;
      Plan : aliased in out Work_Plan'Class;
      Worker_Count : Positive_Worker_Count);

   overriding
   function Priority
     (Pool : Task_Pool) return System.Priority is (Worker_Priority);

   overriding
   procedure Next_Worker_Id
     (Pool : in out Task_Pool;
      Plan : aliased in out Work_Plan'Class;
      Requester : Plan_Index;
      Item : out Plan_Index);

   overriding
   procedure Finished_Work
     (Pool : in out Task_Pool;
      Worker : Pool_Index;
      Plan : aliased in out Work_Plan'Class;
      Item : Plan_Index);

   pragma Inline (Offer_Work, Offer_Work_To_Group);
   pragma Inline (Next_Worker_Id, Finished_Work, Priority);

   overriding function Available_Workers
     (Pool : Task_Pool) return Worker_Count_Type is
      (Pool.Workers'Length -
        Worker_Count_Type (Pool.Manager.Active_Workers));

   overriding function Total_Workers
     (Pool : Task_Pool) return Positive_Worker_Count is
      (Pool.Workers'Length);

end Parallel.Ravenscar_Task_Pools.Implementation;
