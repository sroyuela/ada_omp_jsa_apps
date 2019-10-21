with System.Storage_Elements;
with System.Multiprocessors.Dispatching_Domains;
use System.Multiprocessors;

private with Ada.Dynamic_Priorities;

package Parallel.Task_Pools.Bounded is

   --  pragma Preelaborate;

   type Task_Pool
     (Number_Of_Workers : Positive_Worker_Count;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Ceiling_Priority : System.Priority)
   is limited new Task_Pool_Interface with private;
   --  task pool object type that has a pool of real Ada tasks to
   --  process virtual thread fibers that are submitted to the pool for
   --  processing. Each Ada task in the task pool corresponds to an OS threads
   --  in GNAT

   function Create
     (Number_Of_Workers : Positive_Worker_Count;
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Default_Worker_Storage_Size;
      Ceiling_Priority : System.Priority :=
        System.Max_Priority)
      return Task_Pool;
   --  This call provides a means to create a task pool without having to
   --  specify all the discriminants. Alternatively, a Task_Pool object can
   --  just be declared without calling Create.

   function Create
     (Number_Of_Workers : Positive_Worker_Count;
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Default_Worker_Storage_Size;
      Ceiling_Priority : System.Priority :=
        System.Max_Priority;
      Domain : in out Dispatching_Domains.Dispatching_Domain)
      return Task_Pool;
   --  This Create call is identical to the first except that it also
   --  assigns all the tasks in the task pool to the specified
   --  dispatching domain. This call can only be made during elaboration
   --  before the main program has started, and thus can only assign
   --  workers from the system dispatching domain to a specific domain.

private

   use Ada;

   task type Worker
     (Pool : access Task_Pool'Class := null;
      Id : Worker_Id := Worker_Id'Last;
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Default_Worker_Storage_Size)
   is
      pragma Storage_Size (Storage_Size);

      entry Work_Offered (Item : aliased in out Work_Plan'Class;
                          Priority : System.Priority);

   end Worker;

   type Worker_Array is
     array (Worker_Id range <>) of Worker;
   --  The Ada tasks in the task pool

   type Task_Manager;

   function Create_Worker
     (Pool : access Task_Pool'Class;
      Storage_Size : System.Storage_Elements.Storage_Count
     ) return Worker;
   --  Creates a task in the task pool

   type Idle_List is
     array (Worker_Id range <>) of Worker_Id;

   function Create_Idle_List
     (Number_Of_Workers : Positive_Worker_Count) return Idle_List;

   protected type Task_Manager
     (Pool : access Task_Pool'Class;
      Number_Of_Workers : Positive_Worker_Count;
      Priority : System.Priority;
      Storage_Size : System.Storage_Elements.Storage_Count)
   with Priority => Priority is

      entry Request_Workers
        (Worker_Count : Positive_Worker_Count;
         First_Worker : out Worker_Id);

      entry Offer_Work
        (Item : aliased in out Work_Plan'Class;
         Priority : System.Priority);

      procedure Finished_Offer;

      entry Completed_Work (Worker : Worker_Id);

      procedure Assign
        (Domain : in out Dispatching_Domains.Dispatching_Domain);

      function Active_Workers return Worker_Count_Type;

   private

      entry Allocate_Workers
        (Worker_Count : Positive_Worker_Count;
         First_Worker : out Worker_Id);

      Outstanding_Workers : Worker_Count_Type := 0;
      Idle_Workers     : Idle_List (1 .. Number_Of_Workers)
        := Create_Idle_List (Number_Of_Workers);
      Workers : Worker_Array (1 .. Number_Of_Workers)
        := (others => Create_Worker (Pool         => Pool,
                                     Storage_Size => Storage_Size));
      Busy : Boolean := False;
      Requested_Workers : Positive_Worker_Count;
   end Task_Manager;

   type Task_Pool
     (Number_Of_Workers : Positive_Worker_Count;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Ceiling_Priority : System.Priority) is
   limited new Task_Pool_Interface with
      record
         --  Temporary used for initializing worker ids
         Next_Id : Worker_Id := 1;

         Manager : Task_Manager
           (Task_Pool'Access,
            Number_Of_Workers,
            Ceiling_Priority,
            Storage_Size);

      end record;

   overriding
   procedure Offer_Work
     (Pool : in out Task_Pool;
      Item : aliased in out Work_Plan'Class;
      Worker_Count : Positive_Worker_Count)
   with Pre'Class => Dynamic_Priorities.Get_Priority <= Pool.Ceiling_Priority;
   --  Allows a work plan to request workers from the task pool. The Work
   --  plan is offered to the task pool, which is then engaged by each
   --  worker up to the requested Worker_Count

   overriding function Available_Workers
     (Pool : Task_Pool) return Worker_Count_Type is
      (Pool.Number_Of_Workers - Pool.Manager.Active_Workers);

   overriding function Total_Workers
     (Pool : Task_Pool) return Positive_Worker_Count is
     (Pool.Number_Of_Workers);

   overriding function Worker_Stack_Size
     (Pool : Task_Pool) return System.Storage_Elements.Storage_Count is
      (Pool.Storage_Size);

   pragma Inline (Offer_Work);

end Parallel.Task_Pools.Bounded;
