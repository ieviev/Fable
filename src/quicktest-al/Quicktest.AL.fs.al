// fbl
codeunit 50001 Codeunit1
{
    SingleInstance=true;

    procedure test1() : Integer
    var
        a: Integer;
        b: Integer;
        c: Integer;
    begin
        a := 4;
        b := 2;
        c := a + b;
        exit(c);
    end;
}

