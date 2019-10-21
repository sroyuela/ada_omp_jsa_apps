with System.Storage_Elements;

package Parallel.Task_Pools is

   --  pragma Preelaborate;

   --  A Work Plan gives the task pool client the full control on how the
   --  worker manages and approaches its work. The task pool only provides
   --  the worker, the work plan defines the work to be done.
   --
   type Work_Plan is limited interface;

   procedure Engage
     (Plan : Work_Plan) is abstract;
   --  When a worker starts executing, it engages the work plan. The
   --  parallelism manager client defines the work. The Engage is called
   --  once and executes the plan. Upon returning, the Worker is once again
   --  idle and returns to the task pool

   type Task_Pool_Interface is limited interface;

   procedure Offer_Work
     (Pool : in out Task_Pool_Interface;
      Item : aliased in out Work_Plan'Class;
      Worker_Count : Positive_Worker_Count) is abstract
   with Pre'Class => Pool.Available_Workers >= Worker_Count;
   --  Allows a work plan to request workers from the task pool. The Work
   --  plan is offered to the task pool, which is then engaged by each
   --  worker up to the requested Worker_Count
   --  Note: This routine is intended to be called by the parallelism
   --  manager, and not exposed to the user client code

   function Available_Workers
     (Pool : Task_Pool_Interface) return Worker_Count_Type is abstract;

   function Total_Workers
     (Pool : Task_Pool_Interface) return Positive_Worker_Count is abstract;

   function Worker_Stack_Size
     (Pool : Task_Pool_Interface) return System.Storage_Elements.Storage_Count
      is abstract;

   function Default_Task_Pool return not null access Task_Pool_Interface'Class;

   procedure Set_Default_Task_Pool
     (Pool : aliased in out Task_Pool_Interface'Class);
   --  Note: This call is not task safe

   pragma Inline (Default_Task_Pool, Set_Default_Task_Pool);

end Parallel.Task_Pools;
