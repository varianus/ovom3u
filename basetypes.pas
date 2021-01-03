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
    Channel: string;
    Title: string;
    Plot: string;
    StartTime: TDateTime;
    EndTime: TDateTime;
  end;

  AREpgInfo = array of REpgInfo;

implementation

end.

