unit unix_init_xlib;
{$mode objfpc}{$H+}
interface 
implementation
uses 
 xlib;
Var
 AStatus : TStatus; 
initialization 
AStatus := XInitThreads; 
AStatus := AStatus; 
end.
