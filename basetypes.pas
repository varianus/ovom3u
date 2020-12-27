unit BaseTypes;

interface
type
  TEngineState = (
    ENGINE_STOP,
    ENGINE_PLAY,
    ENGINE_PAUSE,
    ENGINE_IDLE);

  TEngineCommand = (ecEvent,ecPaint);

  REpgInfo = record
    Title: string;
    Plot: string;
    StartTime: TDateTime;
    EndTime: TDateTime;
  end;

implementation

end.

