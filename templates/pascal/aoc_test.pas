program Tests;

uses
  fpcunit, testregistry;

type
  TMyTest = class(TTestCase)
  published
    procedure TestAddition;
  end;

implementation

procedure TMyTest.TestAddition;
begin
  AssertEquals(4, 2 + 2);
end;

var
  TestForm: TTestForm;

begin
  Application.Initialize;
  Application.CreateForm(TTestForm, TestForm);
  Application.Run;
end.
