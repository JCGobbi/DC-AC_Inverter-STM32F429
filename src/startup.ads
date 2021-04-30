
--  Initialization is executed only once at power-on and executes
--  routines that set-up peripherals.
package StartUp is

   procedure Initialize;
   --  Initializes peripherals and configures them into a known state.

   function Is_Initialized return Boolean;
   --  Return True when initialized.

   procedure Wait_Until_Initialized;

private

   Initialized : Boolean := False;

end StartUp;