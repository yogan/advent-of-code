program TestRunner;

uses
  fpcunit, testregistry, MyTests;

begin
  Application.Initialize;
  Application.CreateForm(TTestForm, TestForm);
  Application.Run;
end.

